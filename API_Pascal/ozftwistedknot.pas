unit OZFTwistedKnot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, syncobjs,
  OZFTwistedKnotStream, OZFTwistedKnotPacket;

type

  TOZFTwistedKnotStatus = (
    TwistedKnotStatusConnect,
    TwistedKnotStatusContinue,
    TwistedKnotStatusDisconnect
  );

  { TOZFTwistedKnotDelegate }

  TOZFTwistedKnotDelegate = class
  public
    procedure notify(Status: TOZFTwistedKnotStatus); virtual;
    procedure sendStart(Address, UniqueID, StreamID: DWord); virtual;
    procedure sendCompleted(Sender: TOZFTwistedKnotStream); virtual;
    function canReceive(Address, UniqueID, Length: DWord): Boolean; virtual;
    procedure receiveStart(Address, UniqueID, StreamID: DWord); virtual;
    procedure receiveCompleted(Receiver: TOZFTwistedKnotStream); virtual;
  end;

  { TOZFTwistedKnot }

  TOZFTwistedKnot = class(TThread)
  private
    fPublicKey: String;
    fAddress: String;
    fPort: Integer;
    fHandler: TOZFTwistedKnotDelegate;

    fConnected: Boolean;
    fHandshaked: Boolean;

    fConnectionID: Integer;
    fEncryptSalt: array[0..15] of Byte;
    fEncryptKey: Pointer;
    fDecryptKey: Pointer;

    fReceivers: TFPObjectHashTable;
    fReceiverQueue: TObjectList;
    fReadBuffer: array[0..OZFTwistedKnotPacketLength - 1] of Byte;
    fReadPosition: Integer;
    fPacketLength: Integer;
    fLastReceived: TDateTime;

    fSenders: TFPObjectHashTable;
    fSenderQueue: TObjectList;
    fControlPacket: TList;
    fWriteBuffer: array[0..OZFTwistedKnotPacketLength - 1] of Byte;
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
    procedure RetryDelayed;
    procedure Send(Data: Pointer; Len: Integer; UniqueID, toAddress:DWord);

    procedure Connect;
    procedure Close;

    function querySendProgress(Address, StreamID: DWord): DWord;
    function querySendLength(Address, StreamID: DWord): DWord;
    function cancelSend(Address, StreamID: DWord): Boolean;

    function queryReceiveProgress(Address, StreamID: DWord): DWord;
    function queryReceiveLength(Address, StreamID: DWord): DWord;
    function cancelReceive(Address, StreamID: DWord): Boolean;

    property PublicKey: String read fPublicKey write fPublicKey;
    property Address: String read fAddress write fAddress;
    property Port: Integer read fPort write fPort;
    property Connected: Boolean read fConnected;
    property Handshaked: Boolean read fHandshaked;
    property Handler: TOZFTwistedKnotDelegate read fHandler write fHandler;
    property LastReceived: TDateTime read fLastReceived;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows, WinSock2,
{$ENDIF}
{$IFDEF UNIX}
  Unix, BaseUnix, UnixUtil, NetDB,
{$ENDIF}
  Sockets, DateUtils, OpenSSLWrapper, crc;

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

{ TOZFTwistedKnotDelegate }

procedure TOZFTwistedKnotDelegate.notify(Status: TOZFTwistedKnotStatus);
begin

end;

procedure TOZFTwistedKnotDelegate.sendStart(Address, UniqueID, StreamID: DWord);
begin

end;

procedure TOZFTwistedKnotDelegate.sendCompleted(Sender: TOZFTwistedKnotStream);
begin
  Sender.Free;
end;

function TOZFTwistedKnotDelegate.canReceive(Address, UniqueID, Length: DWord): Boolean;
begin
  result := True;
end;

procedure TOZFTwistedKnotDelegate.receiveStart(Address, UniqueID, StreamID: DWord);
begin

end;

procedure TOZFTwistedKnotDelegate.receiveCompleted(Receiver: TOZFTwistedKnotStream);
begin
  Receiver.Free;
end;

{ TOZFTwistedKnot }

function TOZFTwistedKnot.ExtractRSAKey(Output: Pointer; Size: Integer): Integer;

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
  resultPosition: Integer;
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

function TOZFTwistedKnot.getHandshakeData: Pointer;
var
  i: Integer;
  key_arr: array[0..199] of Byte;
  key_len: Integer;
  key_ptr: Pointer;
  rsa_key: PRSA;
  handshake: array[0..24] of DWord;
  TZBias: Integer;
  timestamp: Int64;

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

  handshake[0] := NToBE(Cardinal(1));
  timestamp := DateTimeToUnix(IncMinute(Now, TZBias));
  timestamp := NToBE(timestamp);
  Move(timestamp, handshake[1], 8);
  handshake[3] := NToBE(fConnectionID);
  Move(fEncryptSalt, handshake[4], sizeof(fEncryptSalt));

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

  AES_set_decrypt_key(@handshake[8], 128, fDecryptKey);
end;

procedure TOZFTwistedKnot.HandleRead;

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
  decrypt: array[0..OZFTwistedKnotPacketLength - 1] of Byte;
  packetLength: Word;
  packetType: TPacketType;
  checksum, crc: DWord;
  key: String;
  receiver: TOZFTwistedKnotStream;
  sender: TOZFTwistedKnotStream;

  from, streamID, streamLength, uniqueID, frameNo: DWord;
  query: Boolean;
  connectionID, flag: DWord;
  status: TOZFTwistedKnotStatus;

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
          query := fHandler.canReceive(from, uniqueID, streamLength)
        else
          query := True;

        if query then
        begin
          receiver := TOZFTwistedKnotStream.Create(streamLength, streamID,
            uniqueID, from);

          if Assigned(fHandler) then
            fHandler.receiveStart(receiver.Address, receiver.UniqueID, receiver.StreamID);

          key := IntToStr(from) + ':' + IntToStr(streamID);
          fReceivers.Add(key, receiver);

          fSection.Acquire;
          fReceiverQueue.Add(receiver);
          fSection.Release;

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
        receiver := TOZFTwistedKnotStream(fReceivers.Items[key]);
        if receiver = nil then continue;

        receiver.fillFrame(@decrypt[OZFTwistedKnotHeaderLength], frameNo);

        sendControlPacket(Ord(PacketTypeReceived), from, streamID, frameNo, 0);

        fSection.Acquire;
        fReceiverQueue.Remove(receiver);
        if receiver.Status <> TwistedKnotStreamComplete then
          fReceiverQueue.Add(receiver);
        fSection.Release;

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
        sender := TOZFTwistedKnotStream(fSenders.Items[key]);
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
        sender := TOZFTwistedKnotStream(fSenders.Items[key]);
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
            fHandler.sendStart(sender.Address, sender.UniqueID, sender.StreamID);
        end;
      end
      else if packetType = PacketTypePause then
      begin
        from := getLong(decrypt[8]);
        streamID := getLong(decrypt[16]);

        key := IntToStr(from) + ':' + IntToStr(streamID);
        sender := TOZFTwistedKnotStream(fSenders.Items[key]);
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
        sender := TOZFTwistedKnotStream(fSenders.Items[key]);
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
        sender := TOZFTwistedKnotStream(fSenders.Items[key]);
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

procedure TOZFTwistedKnot.HandleWrite;

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
  sender: TOZFTwistedKnotStream;
  frame: array[0..OZFTwistedKnotFrameLength - 1] of Byte;
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
      sender := TOZFTwistedKnotStream(fSenderQueue.Items[i]);
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

      packetLength := OZFTwistedKnotHeaderLength + frameLength + gap;
      packet := GetMem(packetLength);
      FillChar(packet^, packetLength, 0);

      putShort(packet + 0, Ord(PacketTypeFrame));
      putShort(packet + 2, packetLength);
      putLong(packet + 4, 0); // checksum
      putLong(packet + 8, 0); // from
      putLong(packet + 12, sender.Address);
      putLong(packet + 16, sender.StreamID);
      putLong(packet + 20, frameNo);
      Move(frame, Pointer(packet + OZFTwistedKnotHeaderLength)^, frameLength);
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

procedure TOZFTwistedKnot.sendControlPacket(packetType: Word; toAddress,
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

procedure TOZFTwistedKnot.Execute;
var
  delay: Integer;
  SAddr: SockAddr;
  opt: DWord;
  readfds: TFDSet;
  writefds: TFDSet;
  pwritefds: Pointer;
  nfds: Integer;

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

constructor TOZFTwistedKnot.Create;
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
  fReceiverQueue := TObjectList.create(False);
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

destructor TOZFTwistedKnot.Destroy;
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

procedure TOZFTwistedKnot.Ping;

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

procedure TOZFTwistedKnot.RetryDelayed;
var
  limit: TDateTime;
  receiver: TOZFTwistedKnotStream;
  i: Integer;
begin
  if fReceiverQueue.Count = 0 then
    exit;

  fSection.Acquire;
  limit := IncSecond(Now, -5);
  for i := 0 to fReceiverQueue.Count - 1 do
  begin
    receiver := TOZFTwistedKnotStream(fReceiverQueue.Items[i]);
    if receiver.LastAccess > limit then
      break;
    fSection.Release;
    SendControlPacket(Ord(PacketTypeRetry), receiver.Address,
      receiver.StreamID, 0, 0);
    fSection.Acquire;
  end;
  fSection.Release;
end;

procedure TOZFTwistedKnot.Send(Data: Pointer; Len: Integer;
  UniqueID, toAddress:DWord);
{$IFDEF UNIX}
const
  dummy: Char = 'x';
{$ENDIF}
var
  StreamID: DWord;
  sender: TOZFTwistedKnotStream;
  key: AnsiString;
begin
  if (Data = nil) or (Len = 0) then
    exit;

  fSection.Acquire;
  StreamID := fNextStreamID;
  fNextStreamID := (fNextStreamID + 1) and $FFFFFFFF;
  fSection.Release;

  sender := TOZFTwistedKnotStream.Create(Data, Len, StreamID, uniqueID,
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

procedure TOZFTwistedKnot.Connect;
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

procedure TOZFTwistedKnot.Close;
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

function TOZFTwistedKnot.querySendProgress(Address, StreamID: DWord
  ): DWord;
var
  key: String;
  sender: TOZFTwistedKnotStream;
begin
  key := IntToStr(Address) + ':' + IntToStr(StreamID);
  sender := TOZFTwistedKnotStream(fSenders.Items[key]);

  if sender = nil then
    Result := 0
  else
    Result := sender.Progress;
end;

function TOZFTwistedKnot.querySendLength(Address, StreamID: DWord
  ): DWord;
var
  key: String;
  sender: TOZFTwistedKnotStream;
begin
  key := IntToStr(Address) + ':' + IntToStr(StreamID);
  sender := TOZFTwistedKnotStream(fSenders.Items[key]);

  if sender = nil then
    Result := 0
  else
    Result := sender.Length;
end;

function TOZFTwistedKnot.cancelSend(Address, StreamID: DWord): Boolean;
begin
  result := False;
end;

function TOZFTwistedKnot.queryReceiveProgress(Address, StreamID: DWord
  ): DWord;
var
  key: String;
  receiver: TOZFTwistedKnotStream;
begin
  key := IntToStr(Address) + ':' + IntToStr(StreamID);
  receiver := TOZFTwistedKnotStream(fReceivers.Items[key]);

  if receiver = nil then
    Result := 0
  else
    Result := receiver.Progress;
end;

function TOZFTwistedKnot.queryReceiveLength(Address, StreamID: DWord
  ): DWord;
var
  key: String;
  receiver: TOZFTwistedKnotStream;
begin
  key := IntToStr(Address) + ':' + IntToStr(StreamID);
  receiver := TOZFTwistedKnotStream(fReceivers.Items[key]);

  if receiver = nil then
    Result := 0
  else
    Result := receiver.Length;
end;

function TOZFTwistedKnot.cancelReceive(Address, StreamID: DWord
  ): Boolean;
begin

end;

end.
