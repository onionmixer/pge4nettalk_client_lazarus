unit TwistedKnot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, syncobjs;

const
  TwistedKnotHeaderLength = 32;
  TwistedKnotFrameLength = 1024;
  TwistedKnotPacketLength =
    (TwistedKnotHeaderLength + TwistedKnotFrameLength);

type

  TTwistedKnotStatus = (
    TwistedKnotStatusConnect,
    TwistedKnotStatusContinue,
    TwistedKnotStatusDisconnect
  );

  TTwistedKnotStreamStatus = (
    TwistedKnotStreamReady,
    TwistedKnotStreamTransfer,
    TwistedKnotStreamSuspend,
    TwistedKnotStreamReset,
    TwistedKnotStreamComplete
  );

  TTwistedKnotSender = class;
  TTwistedKnotReceiver = class;

  { TTwistedKnot }

  TTwistedKnot = class
  public
    procedure notify(Status: TTwistedKnotStatus); virtual;
    procedure sendStart(Sender: TTwistedKnotSender); virtual;
    procedure sendCompleted(Sender: TTwistedKnotSender); virtual;
    function canReceive(UniqueID, From, Length: Cardinal): Boolean; virtual;
    procedure receiveStart(Receiver: TTwistedKnotReceiver); virtual;
    procedure receiveCompleted(Receiver: TTwistedKnotReceiver); virtual;
  end;

  { TTwistedKnotConnection }

  TTwistedKnotConnection = class(TThread)
  private
    fPublicKey: String;
    fAddress: String;
    fPort: Integer;
    fHandler: TTwistedKnot;

    fConnected: Boolean;
    fHandshaked: Boolean;

    fConnectionID: Integer;
    fEncryptSalt: array[0..15] of Byte;
    fEncryptKey: Pointer;
    fDecryptKey: Pointer;

    fReceivers: TFPObjectHashTable;
    fReadBuffer: array[0..TwistedKnotPacketLength - 1] of Byte;
    fReadPosition: Integer;
    fPacketLength: Integer;
    fLastReceived: TDateTime;

    fSenders: TFPObjectHashTable;
    fSenderQueue: TObjectList;
    fControlPacket: TList;
    fWriteBuffer: array[0..TwistedKnotPacketLength - 1] of Byte;
    fWritePosition: Integer;
    fNextStreamID: DWord;

    fEvent: TEvent;
    fSection: TCriticalSection;

    fStop, fRetry: Boolean;
    fSocket, fDummy: Integer;
    {$IFNDEF WINDOWS}
    fWrite: Integer;
    {$ENDIF}

    function ExtractRSAKey(Output: Pointer; Size: Integer): Integer;
    function getHandshakeData: Pointer;

    procedure HandleRead;
    procedure HandleWrite;
    procedure sendControlPacket(packetType: Word; toAddress, streamID, value1,
      value2: DWord);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ping;
    procedure Send(Data: Pointer; Len: Integer; UniqueID, toAddress:DWord);

    procedure Connect;
    procedure Close;

    property PublicKey: String read fPublicKey write fPublicKey;
    property Address: String read fAddress write fAddress;
    property Port: Integer read fPort write fPort;
    property Connected: Boolean read fConnected;
    property Handshaked: Boolean read fHandshaked;
    property Handler: TTwistedKnot read fHandler write fHandler;
    property LastReceived: TDateTime read fLastReceived;
  end;
  
  { TTwistedKnotSender }

  TTwistedKnotSender = class
  private
    fStreamID: Cardinal;
    fStatus: TTwistedKnotStreamStatus;
    fUniqueID: Cardinal;
    fToAddress: Cardinal;
    fLength: Integer;
    fSent: Integer;
    fBuffer: Pointer;
    fFrameFlags: Array of Byte;
    fFrameCount: Integer;
    fNextFrameNo: Integer;
    fSection: TCriticalSection;
  public
    constructor Create(buffer: Pointer; length, streamID, uniqueID, toAddress: Cardinal);
    destructor Destroy; override;

    procedure rewindFrameNo;
    function getNextFrameNo: Integer;
    function getFrame(frame: Pointer; frameNo: Integer): Integer;
    procedure markFrame(frameNo: Integer);

    property StreamID: Cardinal read fStreamID;
    property Status: TTwistedKnotStreamStatus read fStatus write fStatus;
    property UniqueID: Cardinal read fUniqueID;
    property ToAddress: Cardinal read fToAddress;
    property Length: Integer read fLength;
    property Sent: Integer read fSent;
  end;

  { TTwistedKnotReceiver }

  TTwistedKnotReceiver = class
  private
    fStreamID: Cardinal;
    fStatus: TTwistedKnotStreamStatus;
    fUniqueID: Cardinal;
    fFromAddress: Cardinal;
    fLength: Integer;
    fReceived: Integer;
    fBuffer: Pointer;
    fFrameFlags: Array of Byte;
    fFrameCount: Integer;
    fSection: TCriticalSection;

    function getBuffer: Pointer;
  public
    constructor Create(length, streamID, uniqueID, fromAddress: Cardinal);
    destructor Destroy; override;

    procedure fillFrame(frame: Pointer; frameNo: Integer);
    function getBytes(bytes: Pointer; pos, len: Cardinal): Boolean;

    property StreamID: Cardinal read fStreamID;
    property Status: TTwistedKnotStreamStatus read fStatus write fStatus;
    property UniqueID: Cardinal read fUniqueID;
    property FromAddress: Cardinal read fFromAddress;
    property Length: Integer read fLength;
    property Received: Integer read fReceived;
    property Buffer: Pointer read getBuffer;
  end;

  { TTwistedKnotPacket }

  TTwistedKnotPacket = class
  private
    fPacket: PByte;
    fCapacity, fSize: Cardinal;
    fPosition: Cardinal;

    procedure Realloc(minimum: Cardinal = 0);
    procedure setPosition(AValue: Cardinal);
  public
    constructor Create(Capacity: Cardinal = 1024);
    constructor Create(Packet: Pointer; Size: Cardinal);
    destructor Destroy; override;

    function getInt8: ShortInt;
    function getInt16: SmallInt;
    function getInt32: LongInt;

    function getUInt8: Byte;
    function getUInt16: Word;
    function getUInt32: DWord;

    function getNumber: DWord;
    procedure getBytes(Buffer: Pointer; Size: Cardinal);
    function getString: UTF8String;

    procedure putInt8(AValue: ShortInt);
    procedure putInt16(AValue: SmallInt);
    procedure putInt32(AValue: LongInt);

    procedure putUInt8(AValue: Byte);
    procedure putUInt16(AValue: Word);
    procedure putUInt32(AValue: DWord);

    procedure putNumber(AValue: DWord);
    procedure putBytes(Buffer: Pointer; Size: Cardinal);
    procedure putString(AValue: UTF8String);

    property Packet: PByte read fPacket;
    property Capacity: Cardinal read fCapacity;
    property Size: Cardinal read fSize;
    property Position: Cardinal read fPosition write setPosition;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows, WinSock2,
{$ENDIF}
{$IFDEF UNIX}
  Unix, BaseUnix, UnixUtil, NetDB,
{$ENDIF}
  Sockets, DateUtils, OpenSSLWrapper, base64, crc;

type
  TPacketType = (
    PacketTypeError,
    PacketTypeStreamInfo,
    PacketTypeFrame,
    PacketTypeReceived,
    PacketTypeContinue,
    PacketTypePause,
    PacketTypeReset,
    PacketTypeRetry,
    PacketTypePing,
    PacketTypeConnect
  );

{ TTwistedKnot }

procedure TTwistedKnot.notify(Status: TTwistedKnotStatus);
begin

end;

procedure TTwistedKnot.sendStart(Sender: TTwistedKnotSender);
begin

end;

procedure TTwistedKnot.sendCompleted(Sender: TTwistedKnotSender);
begin
  Sender.Free;
end;

function TTwistedKnot.canReceive(UniqueID, From, Length: Cardinal): Boolean;
begin
  result := True;
end;

procedure TTwistedKnot.receiveStart(Receiver: TTwistedKnotReceiver);
begin

end;

procedure TTwistedKnot.receiveCompleted(Receiver: TTwistedKnotReceiver);
begin
  Receiver.Free;
end;

{ TTwistedKnotConnection }

function TTwistedKnotConnection.ExtractRSAKey(Output: Pointer; Size: Integer): Integer;

function decodeBase64(Input: String; var Output; Size: Integer): Integer;
const
  decodeTable:array[0..95] of Byte =
  (
    99,99,99,99,99,99,99,99,99,99,99,62,99,99,99,63,
    52,53,54,55,56,57,58,59,60,61,99,99,99,99,99,99,
    99, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
    15,16,17,18,19,20,21,22,23,24,25,99,99,99,99,99,
    99,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
    41,42,43,44,45,46,47,48,49,50,51,99,99,99,99,99
  );

var
  len, pos, count: Integer;
  resultLength, resultPosition: Integer;
  dec: PByte;
  base64: Integer;
  values: array[0..3] of Byte;
  decode: array[0..2] of Byte;
begin
  result := 0;
  len := Length(Input);
  if len = 0 then
    exit;

  resultPosition := 0;
  dec := @Output;

  pos := 1;
  count := 0;

  while pos <= len do
  begin
    while pos <= len do
    begin
      base64 := Ord(Input[pos]);
      inc(pos);

      if (base64 < 32) or (base64 > 127) or (base64 = 61) then
        continue;

      values[count] := decodeTable[base64 - 32];
      if values[count] <> 99 then
      begin
        inc(count);
        if count = 4 then
          break;
      end;
    end;

    if count >= 2 then
      decode[0] := (values[0] shl 2) or (values[1] shr 4);
    if count >= 3 then
      decode[1] := (values[1] shl 4) or (values[2] shr 2);
    if count >= 4 then
      decode[2] := (values[2] shl 6) or values[3];

    if count >= 2 then
    begin
      if (resultPosition + count - 1) > Size then
        exit;
      Move(decode, (dec + resultPosition)^, count - 1);
      inc(resultPosition, count - 1);
    end;

    count := 0;
  end;

  result := resultPosition;
end;

var
  key: array[0..199] of Byte;
  len, pos: Integer;

begin
  result := 0;

  len := decodeBase64(fPublicKey, key, Length(key));

  if len = 0 then
    exit;

  pos := 0;

  if key[pos] <> $30 then exit;
  inc(pos);
  if key[pos] > $80 then inc(pos, key[pos] - $80);
  inc(pos);
  if pos >= len then exit;
  // OID
  if key[pos] <> $30 then exit;
  inc(pos, 15);
  if pos >= (len - 2) then exit;
  // RSA
  if key[pos] <> $3 then exit;
  inc(pos);
  if key[pos] > $80 then inc(pos, key[pos] - $80);
  inc(pos);
  if pos >= (len - 1) then exit;
  // NULL
  if key[pos] <> $00 then exit;
  inc(pos);

  if Size > (len - pos) then
  begin
    Result := len - pos;
    Move(key[pos], Output^, Result);
  end;
end;

function TTwistedKnotConnection.getHandshakeData: Pointer;
var
  i: Integer;
  key_arr: array[0..199] of Byte;
  key_len: Integer;
  key_ptr: Pointer;
  rsa_key: PRSA;
  handshake: array[0..24] of DWord;
  TZBias: Integer;

{$IFDEF WINDOWS}
  TimeZone: TTimeZoneInformation;
{$ENDIF}

begin
  result := GetMem(128);
  if result = nil then exit;

  {$IFDEF WINDOWS}
  case GetTimeZoneInformation(TimeZone) of
    TIME_ZONE_ID_STANDARD: TZBias := TimeZone.Bias + TimeZone.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: TZBias := TimeZone.Bias + TImeZone.DaylightBias;
  else
    TZBias := TimeZone.Bias;
  end;
  {$ELSE}
  TZBias := UnixUtil.TZSeconds div -60;
  {$ENDIF}

  Randomize;

  for i := 0 to 24 do
    handshake[i] := Random($100000000);

  handshake[0] := DateTimeToUnix(IncMinute(Now, TZBias));
  handshake[1] := fConnectionID;
  handshake[2] := 0;
  Move(fEncryptSalt, handshake[3], sizeof(fEncryptSalt));

  key_ptr := @key_arr;
  key_len := ExtractRSAKey(key_ptr, Length(key_arr));
  rsa_key := nil;
  rsa_key := d2i_RSAPublicKey(rsa_key, @key_ptr, key_len);

  i := RSA_public_encrypt(100, @handshake, result, rsa_key, RSA_PKCS1_PADDING);

  RSA_free(rsa_key);

  if i <> 128 then
  begin
    FreeMem(result);
    result := nil;
    exit;
  end;

  AES_set_decrypt_key(@handshake[6], 128, fDecryptKey);
end;

procedure TTwistedKnotConnection.HandleRead;

function getShort(const Input): Word;
begin
  Move(Input, result, 2);
  result := ntohs(result);
end;

function getLong(const Input): DWord;
begin
  Move(Input, result, 4);
  result := ntohl(result);
end;

var
  i: Integer;
  decrypt: array[0..TwistedKnotPacketLength - 1] of Byte;
  text: array[0..15] of Byte;
  packetLength: Word;
  packetType: TPacketType;
  checksum, crc: DWord;
  key: String;
  receiver: TTwistedKnotReceiver;
  sender: TTwistedKnotSender;

  from, streamID, streamLength, uniqueID, frameNo: DWord;
  query: Boolean;
  connectionID, flag: DWord;
  status: TTwistedKnotStatus;

begin
  fLastReceived := Now;

  while not Terminated and not fStop and not fRetry do
  begin
    i := Sockets.fpRecv(fSocket, @fReadBuffer[fReadPosition],
      sizeof(fReadBuffer) - fReadPosition, 0);

    if (i < 0) and (SocketError = EsockEWOULDBLOCK) then
      break;

    if (i <= 0) then
    begin
      fRetry := True;
      break;
    end;

    inc(fReadPosition, i);

    while fReadPosition >= fPacketLength do
    begin
      for i := 0 to (fPacketLength div 16) - 1 do
        AES_decrypt(@fReadBuffer[i * 16], @decrypt[i * 16], fDecryptKey);

      packetLength := getShort(decrypt[2]);
      if packetLength <> fPacketLength then
      begin
        fPacketLength := packetLength;
        continue;
      end;

      dec(fReadPosition, fPacketLength);
      Move(fReadBuffer[fPacketLength], fReadBuffer, fReadPosition);
      fPacketLength := 16;

      i := getShort(decrypt[0]);
      if (i < 0) or (i > Ord(High(TPacketType))) then
      begin
        fRetry := True;
        break;
      end;

      packetType := TPacketType(i);
      checksum := getLong(decrypt[4]);
      crc := 0;

      if packetLength > 16 then
      begin
        crc := crc32(0, @decrypt[8], packetLength - 8);
      end;

      if checksum <> crc then
      begin
        fRetry := True;
        break;
      end;

      if packetType = PacketTypeStreamInfo then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);
        streamLength := getLong(decrypt[20]);
        uniqueID := getLong(decrypt[24]);

        if Assigned(fHandler) then
          query := fHandler.canReceive(uniqueID, from, streamLength)
        else
          query := True;

        if query then
        begin
          receiver := TTwistedKnotReceiver.Create(streamLength, streamID,
            uniqueID, from);

          if Assigned(fHandler) then
            fHandler.receiveStart(receiver);

          key := IntToStr(from) + ':' + IntToStr(streamID);
          fReceivers.Add(key, receiver);

          sendControlPacket(Ord(PacketTypeContinue), from, streamID, 0, 0);
        end else begin
          sendControlPacket(Ord(PacketTypeReset), from, streamID, 0, 0);
        end;
      end
      else if packetType = PacketTypeFrame then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);
        frameNo := getLong(decrypt[20]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        receiver := TTwistedKnotReceiver(fReceivers.Items[key]);
        if receiver = nil then continue;

        receiver.fillFrame(@decrypt[TwistedKnotHeaderLength], frameNo);

        sendControlPacket(Ord(PacketTypeReceived), from, streamID, frameNo, 0);

        if receiver.Status = TwistedKnotStreamComplete then
        begin
          fReceivers.Delete(key);

          if Assigned(fHandler) then
            fHandler.receiveCompleted(receiver)
          else
            receiver.Free;
        end;
      end
      else if packetType = PacketTypeReceived then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);
        frameNo := getLong(decrypt[20]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TTwistedKnotSender(fSenders.Items[key]);
        if sender = nil then continue;

        sender.markFrame(frameNo);

        if sender.Status = TwistedKnotStreamComplete then
        begin
          fSenders.Delete(key);

          if Assigned(fHandler) then
            fHandler.sendCompleted(sender)
          else
            sender.Free;
        end;
      end
      else if packetType = PacketTypeContinue then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TTwistedKnotSender(fSenders.Items[key]);
        if sender = nil then continue;

        fSection.Acquire;
        if fSenderQueue.IndexOf(sender) = -1 then
        begin
          fSenderQueue.Add(sender);
        end;
        fSection.Release;

        if sender.Status = TwistedKnotStreamReady then
        begin
          if Assigned(fHandler) then
            fHandler.sendStart(sender);
        end;
      end
      else if packetType = PacketTypePause then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TTwistedKnotSender(fSenders.Items[key]);
        if sender = nil then continue;

        sender.Status := TwistedKnotStreamSuspend;

        fSection.Acquire;
        fSenderQueue.Remove(sender);
        fSection.Release;
      end
      else if packetType = PacketTypeReset then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TTwistedKnotSender(fSenders.Items[key]);
        if sender = nil then continue;

        sender.Status := TwistedKnotStreamReset;

        fSenders.Delete(key);

        if Assigned(fHandler) then
          fHandler.sendCompleted(sender)
        else
          sender.Free;

        fSection.Acquire;
        fSenderQueue.Remove(sender);
        fSection.Release;
      end
      else if packetType = PacketTypeRetry then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TTwistedKnotSender(fSenders.Items[key]);
        if sender = nil then continue;

        sender.rewindFrameNo;
      end
      else if packetType = PacketTypeConnect then
      begin
        connectionID := getLong(decrypt[8]);
        flag := getLong(decrypt[12]);
        Move(decrypt[16], fEncryptSalt, sizeof(fEncryptSalt));

        AES_set_encrypt_key(@fEncryptSalt, 128, fEncryptKey);
        fHandshaked := True;

        if flag = 0 then
        begin
          fReceivers.Clear;
          fSenders.Clear;
          fSenderQueue.Clear;
          fControlPacket.Clear;

          status := TwistedKnotStatusConnect;
        end else begin
          status := TwistedKnotStatusContinue;
        end;

        if Assigned(fHandler) then
          fHandler.notify(status);
      end;
    end;
  end;
end;

procedure TTwistedKnotConnection.HandleWrite;

function getShort(const Input: Pointer): Word;
begin
  Move(Input^, result, 2);
  result := ntohs(result);
end;

procedure putShort(Dest: Pointer; Value: Word);
begin
  Value := htons(Value);
  Move(Value, Dest^, 2);
end;

procedure putLong(Dest: Pointer; Value: DWord);
begin
  Value := htonl(Value);
  Move(Value, Dest^, 4);
end;

var
  i: Integer;
  packet: PByte;
  packetLength: Word;
  sender: TTwistedKnotSender;
  frame: array[0..TwistedKnotFrameLength - 1] of Byte;
  frameNo, gap: Integer;
  frameLength, checksum: DWord;
begin
  while not Terminated and not fStop and not fRetry do
  begin
    if fWritePosition > 0 then
    begin
      i := Sockets.fpsend(fSocket, @fWriteBuffer, fWritePosition, 0);

      if ((i < 0) and (SocketError = EsockEWOULDBLOCK)) or (i = 0) then
        break;

      if (i <= 0) then
      begin
        fRetry := True;
        break;
      end;

      dec(fWritePosition, i);
      Move(fWriteBuffer[i], fWriteBuffer, fWritePosition);
      continue;
    end;

    if not fHandshaked then break;

    if fControlPacket.Count > 0 then
    begin
      fSection.Acquire;
      packet := fControlPacket.Items[0];
      fControlPacket.Delete(0);
      fSection.Release;

      packetLength := GetShort(packet + 2);
    end
    else if fSenderQueue.Count > 0 then
    begin
      fSection.Acquire;
      i := Random(fSenderQueue.Count);
      sender := TTwistedKnotSender(fSenderQueue.Items[i]);
      fSection.Release;

      frameNo := sender.getNextFrameNo;
      if frameNo = -1 then
      begin
        fSection.Acquire;
        fSenderQueue.Remove(sender);
        fSection.Release;

        continue;
      end;

      frameLength := sender.getFrame(@frame, frameNo);
      gap := ($10 - (frameLength and $0f)) and $0f;

      packetLength := TwistedKnotHeaderLength + frameLength + gap;
      packet := GetMem(packetLength);
      FillChar(packet^, packetLength, 0);

      putShort(packet + 0, Ord(PacketTypeFrame));
      putShort(packet + 2, packetLength);
      putLong(packet + 4, 0); // checksum
      putLong(packet + 8, 0); // from
      putLong(packet + 12, sender.ToAddress);
      putLong(packet + 16, sender.StreamID);
      putLong(packet + 20, frameNo);
      Move(frame, Pointer(packet + TwistedKnotHeaderLength)^, frameLength);
    end
    else
    begin
      packet := nil;
    end;

    if packet = nil then break;

    checksum := (packet + 2)^;
    checksum := (checksum shl 8) or (packet + 3)^;
    if (packetLength <> checksum) then
    begin
      FreeMem(packet);
      continue;
    end;

    if packetLength > 16 then
    begin
      checksum := crc32(0, packet + 8, packetLength - 8);
      putLong(packet + 4, checksum);
    end;

    for i := 0 to (packetLength div 16) - 1 do
      AES_encrypt(Pointer(packet + i * 16), @fWriteBuffer[i * 16], fEncryptKey);
    fWritePosition := packetLength;

    FreeMem(packet);
  end;
end;

procedure TTwistedKnotConnection.sendControlPacket(packetType: Word; toAddress,
  streamID, value1, value2: DWord);

procedure putShort(Dest: Pointer; Value: Word);
begin
  Value := htons(Value);
  Move(Value, Dest^, 2);
end;

procedure putLong(Dest: Pointer; Value: DWord);
begin
  Value := htonl(Value);
  Move(Value, Dest^, 4);
end;

{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
var
  packet: PByte;
begin
  packet := GetMem(32);

  putShort(packet + 0, packetType);
  putShort(packet + 2, 32); // length
  putLong(packet + 4, 0); // checksum
  putLong(packet + 8, 0); // from
  putLong(packet + 12, toAddress);
  putLong(packet + 16, streamID);
  putLong(packet + 20, value1);
  putLong(packet + 24, value2);
  putLong(packet + 28, 0);

  fSection.Acquire;
  fControlPacket.Add(packet);
  fSection.Release;

  if fConnected then
  begin
    {$IFDEF WINDOWS}
    if (fDummy > 0) then
      CloseSocket(fDummy);
    fDummy := -1;
    {$ENDIF}
    {$IFDEF UNIX}
    fpWrite(fWrite, dummy, 1);
    {$ENDIF}
  end;
end;

{$IFDEF WINDOWS}
function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
begin
  Result := Select(nfds, readfds, writefds, exceptfds, timeout);
end;

function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
begin
  Result := 0;
  if FD_ISSET(Socket, FDSet) then
    Result := 1;
end;

procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
begin
  FD_SET(Socket, FDSet);
end;

procedure fpFD_ZERO(var FDSet: TFDSet); inline;
begin
  FD_ZERO(FDSet);
end;
{$ENDIF}

procedure TTwistedKnotConnection.Execute;
var
  data: array[0..24] of Cardinal;
  ptr: PAnsiChar;
  delay: Integer;
  SAddr: SockAddr;
  opt: DWord;
  readfds: TFDSet;
  writefds: TFDSet;
  pwritefds: Pointer;
  i, nfds: Integer;

{$IFDEF WINDOWS}
  host: PHostEnt;
{$ENDIF}
{$IFDEF UNIX}
  host: THostEntry;
{$ENDIF}

begin
  Randomize;

  while not Terminated do
  begin
    if fEvent.WaitFor(5000) <> wrSignaled then
      continue;
    fEvent.ResetEvent;

    delay := 0;
    fStop := False;
    fConnected := False;
    fLastReceived := Now;

    while not Terminated and not fStop do
    begin
      if (delay > 0) then
        Sleep(delay);
      delay := 5000;

      if (fAddress = '') then
      begin
        delay := 300;
        continue;
      end;

      fSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
      if (fSocket < 0) then
        continue;

      FillByte(SAddr, SizeOf(SAddr), 0);
      SAddr.sin_family := AF_INET;
      SAddr.sin_port := htons(fPort);
      SAddr.sin_addr := StrToNetAddr(fAddress);
      if SAddr.sin_addr.s_addr = 0 then
      begin
        {$IFDEF WINDOWS}
        host := GetHostByName(PChar(fAddress));
        SAddr.sin_addr := pin_addr(host^.h_addr_list[0])^;
        {$ENDIF}
        {$IFDEF UNIX}
        if GetHostByName(PChar(fAddress), host) then
          SAddr.sin_addr.s_addr := htonl(host.Addr.s_addr)
        else if ResolveHostByName(PChar(fAddress), host) then
          SAddr.sin_addr := host.Addr;
        {$ENDIF}
      end;

      if fpConnect(fSocket, @SAddr, SizeOf(Saddr)) < 0 then
      begin
        CloseSocket(fSocket);
        continue;
      end;

      {$IFDEF WINDOWS}
      opt := 1;
      ioctlsocket(fSocket, FIONBIO, @opt);
      fDummy := -1;
      nfds := 1;
      {$ENDIF}
      {$IFDEF UNIX}
      opt := FpFcntl(fSocket, F_GetFl);
      opt := opt or O_NONBLOCK;
      FpFcntl(fSocket, F_SetFl, opt);

      if AssignPipe(fDummy, fWrite) < 0 then
      begin
        Terminate;
        fDummy := -1;
        fWrite := -1;
        break;
      end;

      if fDummy > fSocket then
        nfds := fDummy + 1
      else
        nfds := fSocket + 1;
      {$ENDIF}

      fConnected := True;
      fRetry := False;

      Move(getHandshakeData^, fWriteBuffer[0], 128);
      fWritePosition := 128;

      while not Terminated and not fStop and not fRetry do
      begin
        {$IFDEF WINDOWS}
        if fDummy < 0 then
        begin
          fDummy := fpSocket(AF_INET, SOCK_DGRAM, 0);
        end;
        {$ENDIF}

        fpFD_Zero(readfds);
        fpFD_Zero(writefds);

        fpFD_Set(fSocket, readfds);
        fpFD_Set(fDummy, readfds);

        fSection.Acquire;
        if (fWritePosition > 0) or (fSenderQueue.Count > 0) or
          (fControlPacket.Count > 0) then
        begin
          fpFD_Set(fSocket, writefds);
          pwritefds := @writefds;
        end else begin
          pwritefds := nil;
        end;
        fSection.Release;

        if (fpSelect(nfds, @readfds, pwritefds, nil, nil) < 0) then
          continue;

        if fpFD_ISSET(fSocket, readfds) = 1 then
          HandleRead;

        if fpFD_ISSET(fSocket, writefds) = 1 then
          HandleWrite;

        {$IFDEF UNIX}
        if fpFD_ISSET(fDummy, readfds) = 1 then
          opt := fpRead(fDummy, opt, SizeOf(opt));
        {$ENDIF}
      end;

      fConnected := False;
      CloseSocket(fSocket);
      {$IFDEF WINDOWS}
      if fDummy >= 0 then
        CloseSocket(fDummy);
      {$ENDIF}
      {$IFDEF UNIX}
      if fDummy >= 0 then
        fpClose(fDummy);
      if fWrite >= 0 then
        fpClose(fWrite);
      fWrite := -1;
      {$ENDIF}
      fDummy := -1;

      fReadPosition := 0;
      fWritePosition := 0;

      if Assigned(fHandler) then
        fHandler.notify(TwistedKnotStatusDisconnect);

      delay := 5000;
    end;

    fEvent.ResetEvent;
  end;
end;

constructor TTwistedKnotConnection.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  fHandler := nil;

  fConnected := False;
  fHandshaked := False;

  fAddress := '';

  fEncryptKey := GetMem(SizeOf(AES_KEY));
  fDecryptKey := GetMem(SizeOf(AES_KEY));

  fReceivers := TFPObjectHashTable.Create(False);
  fPacketLength := 16;
  fLastReceived := Now;

  fSenders := TFPObjectHashTable.Create(False);
  fSenderQueue := TObjectList.Create(False);
  fControlPacket := TList.Create;
  fNextStreamID := 0;

  fEvent := TSimpleEvent.Create;
  fSection := syncobjs.TCriticalSection.Create;

  fSocket := -1;
end;

destructor TTwistedKnotConnection.Destroy;
begin
  Close;
  while fConnected do Sleep(1);

  fReceivers.Free;
  fSenders.Free;
  fSenderQueue.Free;
  fControlPacket.Free;
  fEvent.Free;
  fSection.Free;

  inherited Destroy;
end;

procedure TTwistedKnotConnection.Ping;

procedure putShort(Dest: Pointer; Value: Word);
begin
  Value := htons(Value);
  Move(Value, Dest^, 2);
end;

procedure putLong(Dest: Pointer; Value: DWord);
begin
  Value := htonl(Value);
  Move(Value, Dest^, 4);
end;

{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
var
  packet: PByte;
begin
  packet := GetMem(16);

  putShort(packet + 0, Ord(PacketTypePing));
  putShort(packet + 2, 16); // length
  putLong(packet + 4, 0);
  putLong(packet + 8, 0);
  putLong(packet + 12, 0);

  fSection.Acquire;
  fControlPacket.Add(packet);
  fSection.Release;

  if fConnected then
  begin
    {$IFDEF WINDOWS}
    if (fDummy > 0) then
      CloseSocket(fDummy);
    fDummy := -1;
    {$ENDIF}
    {$IFDEF UNIX}
    fpWrite(fWrite, dummy, 1);
    {$ENDIF}
  end;
end;

procedure TTwistedKnotConnection.Send(Data: Pointer; Len: Integer;
  UniqueID, toAddress:DWord);
{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
var
  StreamID: DWord;
  sender: TTwistedKnotSender;
  key: AnsiString;
begin
  fSection.Acquire;
  StreamID := fNextStreamID;
  fNextStreamID := (fNextStreamID + 1) and $FFFFFFFF;
  fSection.Release;

  sender := TTwistedKnotSender.Create(Data, Len, StreamID, uniqueID,
    toAddress);

  fSection.Acquire;
  key := IntToStr(toAddress) + ':' + IntToStr(StreamID);
  fSenders.Add(key, sender);
  fSection.Release;

  sendControlPacket(Ord(PacketTypeStreamInfo), toAddress, streamID, Len,
    uniqueID);

  if fConnected then
  begin
    {$IFDEF WINDOWS}
    if (fDummy > 0) then
      CloseSocket(fDummy);
    fDummy := -1;
    {$ENDIF}
    {$IFDEF UNIX}
    fpWrite(fWrite, dummy, 1);
    {$ENDIF}
  end;
end;

procedure TTwistedKnotConnection.Connect;
{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
begin
  if fConnected then
  begin
    fRetry := True;
    {$IFDEF WINDOWS}
    if (fDummy > 0) then
      CloseSocket(fDummy);
    fDummy := -1;
    {$ENDIF}
    {$IFDEF UNIX}
    fpWrite(fWrite, dummy, 1);
    {$ENDIF}
  end else begin
    fEvent.SetEvent;
  end;
end;

procedure TTwistedKnotConnection.Close;
{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
begin
  fStop := True;
  {$IFDEF WINDOWS}
  if (fDummy > 0) then
    CloseSocket(fDummy);
  fDummy := -1;
  {$ENDIF}
  {$IFDEF UNIX}
  fpWrite(fWrite, dummy, 1);
  {$ENDIF}
end;

{ TTwistedKnotSender }

constructor TTwistedKnotSender.Create(buffer: Pointer; length, streamID,
  uniqueID, toAddress: Cardinal);
begin
  fStreamID := streamID;
  fUniqueID := uniqueID;
  fToAddress := toAddress;

  fSent := 0;
  fNextFrameNo := 0;
  fStatus := TwistedKnotStreamReady;

  if (buffer = nil) or (length = 0) then
  begin
    fBuffer := nil;
    fLength := 0;
  end else begin
    fBuffer := GetMem(length);
    Move(buffer^, fBuffer^, length);
    fLength := length;
  end;

  fFrameCount := (fLength + TwistedKnotFrameLength - 1) div TwistedKnotFrameLength;
  SetLength(fFrameFlags, fFrameCount);
  FillChar(fFrameFlags[0], fFrameCount, #0);
  fSection := syncobjs.TCriticalSection.Create;
end;

destructor TTwistedKnotSender.Destroy;
begin
  if Assigned(fBuffer) then
    FreeMem(fBuffer);
  SetLength(fFrameFlags, 0);
  fSection.Free;
  inherited Destroy;
end;

procedure TTwistedKnotSender.rewindFrameNo;
begin
  fNextFrameNo := 0;
end;

function TTwistedKnotSender.getNextFrameNo: Integer;
begin
  result := -1;
  if (fNextFrameNo < 0) or (fNextFrameNo >= fFrameCount) then
    exit;

  while (fNextFrameNo < fFrameCount) do
  begin
    if (fFrameFlags[fNextFrameNo] = 0) then
    begin
      result := fNextFrameNo;
      inc(fNextFrameNo);
      exit;
    end;

    inc(fNextFrameNo);
  end;
end;

function TTwistedKnotSender.getFrame(frame: Pointer; frameNo: Integer): Integer;
var
  frameLength: Integer;
  byteBuffer: PByte;
begin
  Result := 0;
  if (frameNo < 0) or (frameNo >= fFrameCount) then
    exit;

  frameLength := 0;
  if frameNo = (fFrameCount - 1) then
    frameLength := fLength mod TwistedKnotFrameLength;
  if frameLength = 0 then
    frameLength := TwistedKnotFrameLength;

  byteBuffer := PByte(fBuffer) + (frameNo * TwistedKnotFrameLength);
  Move(byteBuffer^, frame^, frameLength);
  fStatus := TwistedKnotStreamTransfer;
  result := frameLength;
end;

procedure TTwistedKnotSender.markFrame(frameNo: Integer);
var
  frameLength: Integer;
begin
  if (frameNo < 0) or (frameNo >= fFrameCount) then
    exit;

  fSection.Acquire;
  if fFrameFlags[frameNo] = 0 then
  begin
    frameLength := 0;
    if frameNo = (fFrameCount - 1) then
      frameLength := fLength mod TwistedKnotFrameLength;
    if frameLength = 0 then
      frameLength := TwistedKnotFrameLength;

    fFrameFlags[frameNo] := 1;
    inc(fSent, frameLength);

    if fLength = fSent then
      fStatus := TwistedKnotStreamComplete;
  end;
  fSection.Release;
end;

{ TTwistedKnotReceiver }

function TTwistedKnotReceiver.getBuffer: Pointer;
begin
  if fLength = fReceived then
    result := fBuffer
  else
    result := nil;
end;

constructor TTwistedKnotReceiver.Create(length, streamID, uniqueID,
  fromAddress: Cardinal);
begin
  fLength := length;
  fStreamID := streamID;
  fUniqueID := uniqueID;
  fFromAddress := fromAddress;

  fReceived := 0;
  fStatus := TwistedKnotStreamReady;

  if length = 0 then
  begin
    fFrameCount := 0;
    fBuffer := nil;
  end else begin
    fFrameCount := (fLength + TwistedKnotFrameLength - 1) div TwistedKnotFrameLength;
    fBuffer := GetMem(fLength);
  end;

  SetLength(fFrameFlags, fFrameCount);
  FillChar(fFrameFlags[0], fFrameCount, #0);
  fSection := syncobjs.TCriticalSection.Create;
end;

destructor TTwistedKnotReceiver.Destroy;
begin
  if Assigned(fBuffer) then
    FreeMem(fBuffer);
  SetLength(fFrameFlags, 0);
  fSection.Free;
  inherited Destroy;
end;

procedure TTwistedKnotReceiver.fillFrame(frame: Pointer; frameNo: Integer);
var
  frameLength: Integer;
  byteBuffer: PByte;
begin
  if (frameNo < 0) or (frameNo >= fFrameCount) then
    exit;

  fSection.Acquire;
  if fFrameFlags[frameNo] = 0 then
  begin
    frameLength := 0;
    if frameNo = (fFrameCount - 1) then
      frameLength := fLength mod TwistedKnotFrameLength;
    if frameLength = 0 then
      frameLength := TwistedKnotFrameLength;

    byteBuffer := PByte(fBuffer) + (frameNo * TwistedKnotFrameLength);
    Move(frame^, byteBuffer^, frameLength);
    fFrameFlags[frameNo] := 1;
    inc(fReceived, frameLength);

    if fLength = fReceived then
      fStatus := TwistedKnotStreamComplete
    else
      fStatus := TwistedKnotStreamTransfer;
  end;
  fSection.Release;
end;

function TTwistedKnotReceiver.getBytes(bytes: Pointer; pos,
  len: Cardinal): Boolean;
var
  front, rear: Cardinal;
  byteBuffer: PByte;
begin
  result := False;
  if (pos < 0) or (len <= 0) or ((pos + len) >= fLength) then
    exit;

  front := (pos + TwistedKnotFrameLength - 1) div TwistedKnotFrameLength;
  rear := (pos + len + TwistedKnotFrameLength - 1) div TwistedKnotFrameLength;

  while (front <= rear) do
  begin
    if fFrameFlags[front] = 0 then
      exit;
    inc(front);
  end;

  byteBuffer := PByte(fBuffer) + pos;
  Move(byteBuffer^, bytes^, len);
  Result := True;
end;

{ TTwistedKnotPacket }

procedure TTwistedKnotPacket.Realloc(minimum: Cardinal);
var
  Growth: Cardinal;
begin
  if minimum > 65536 then
    Growth := (((minimum - 1) div 65536) + 1) * 65536
  else if fCapacity > 65536 then
    Growth := 65536
  else if fCapacity > minimum then
    Growth := fCapacity
  else
    Growth := minimum;

  inc(fCapacity, Growth);
  fPacket := ReallocMem(fPacket, fCapacity);
end;

procedure TTwistedKnotPacket.setPosition(AValue: Cardinal);
begin
  if fPosition = AValue then Exit;
  if AValue < fSize then
    fPosition := AValue
  else
    fPosition := fSize;
end;

constructor TTwistedKnotPacket.Create(Capacity: Cardinal);
begin
  if fCapacity = 0 then
    fCapacity := 1024;
  fPacket := GetMem(fCapacity);
  fCapacity := Capacity;
  fSize := 0;
  fPosition := 0;
end;

constructor TTwistedKnotPacket.Create(Packet: Pointer; Size: Cardinal);
begin
  if Size = 0 then
    fCapacity := 1024
  else
    fCapacity := Size;
  fPacket := GetMem(fCapacity);
  if Size > 0 then
    Move(Packet^, fPacket^, Size);
  fSize := Size;
  fPosition := 0;
end;

destructor TTwistedKnotPacket.Destroy;
begin
  inherited Destroy;

  if Assigned(fPacket) then
    FreeMem(fPacket);
end;

function TTwistedKnotPacket.getInt8: ShortInt;
begin
  Result := PShortInt(fPacket + fPosition)^;
  inc(fPosition);
end;

function TTwistedKnotPacket.getInt16: SmallInt;
begin
  Result := BEtoN(PSmallInt(fPacket + fPosition)^);
  inc(fPosition, 2);
end;

function TTwistedKnotPacket.getInt32: LongInt;
begin
  Result := BEtoN(PLongInt(fPacket + fPosition)^);
  inc(fPosition, 4);
end;

function TTwistedKnotPacket.getUInt8: Byte;
begin
  Result := PByte(fPacket + fPosition)^;
  inc(fPosition);
end;

function TTwistedKnotPacket.getUInt16: Word;
begin
  Result := BEtoN(PWord(fPacket + fPosition)^);
  inc(fPosition, 2);
end;

function TTwistedKnotPacket.getUInt32: DWord;
begin
  Result := BEtoN(PDWord(fPacket + fPosition)^);
  inc(fPosition, 4);
end;

function TTwistedKnotPacket.getNumber: DWord;
var
  Value: Byte;
  Shift: Cardinal;
begin
  Result := 0;
  Shift := 0;
  while (True) do
  begin
    Value := getUInt8;
    Result := Result or ((Value and $7F) shl Shift);
    if (Value and $80) = 0 then
      break;
    inc(Shift, 7);
  end;
end;

procedure TTwistedKnotPacket.getBytes(Buffer: Pointer; Size: Cardinal);
begin
  if Size = 0 then
    exit;
  Move((fPacket + fPosition)^, Buffer^, Size);
  inc(fPosition, Size);
end;

function TTwistedKnotPacket.getString: UTF8String;
var
  Len: Cardinal;
begin
  Len := getNumber;
  SetLength(Result, Len);
  getBytes(PChar(Result), Len);
end;

procedure TTwistedKnotPacket.putInt8(AValue: ShortInt);
begin
  if (fCapacity - fPosition) < 1 then
    Realloc(1);
  PShortInt(fPacket + fPosition)^ := AValue;
  inc(fPosition);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putInt16(AValue: SmallInt);
begin
  if (fCapacity - fPosition) < 2 then
    Realloc(2);
  PSmallInt(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 2);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putInt32(AValue: LongInt);
begin
  if (fCapacity - fPosition) < 4 then
    Realloc(4);
  PLongInt(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 4);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putUInt8(AValue: Byte);
begin
  if (fCapacity - fPosition) < 1 then
    Realloc(1);
  PByte(fPacket + fPosition)^ := AValue;
  inc(fPosition);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putUInt16(AValue: Word);
begin
  if (fCapacity - fPosition) < 2 then
    Realloc(2);
  PWord(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 2);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putUInt32(AValue: DWord);
begin
  if (fCapacity - fPosition) < 4 then
    Realloc(4);
  PDWord(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 4);
  if fPosition > fSize then
    fSize := fPosition;
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putNumber(AValue: DWord);
var
  Value: Byte;
begin
  if AValue = 0 then
  begin
    putUInt8(0);
    exit;
  end;
  while AValue > 0 do
  begin
    Value := AValue and $7F;
    AValue := AValue shr 7;
    if AValue > 0 then
      Value := Value or $80;
    putUInt8(Value);
  end;
end;

procedure TTwistedKnotPacket.putBytes(Buffer: Pointer; Size: Cardinal);
begin
  if Size = 0 then
    exit;
  if (fCapacity - fPosition) < Size then
    Realloc(Size);
  Move(Buffer^, (fPacket + fPosition)^, Size);
  inc(fPosition, Size);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TTwistedKnotPacket.putString(AValue: UTF8String);
var
  Len: Cardinal;
begin
  Len := Length(AValue);
  putNumber(Len);
  putBytes(PChar(AValue), Len);
end;

end.