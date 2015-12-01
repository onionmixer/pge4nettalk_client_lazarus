unit OZFTwistedKnotStream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs;

const
  OZFTwistedKnotHeaderLength = 32;
  OZFTwistedKnotFrameLength = 1024;
  OZFTwistedKnotPacketLength =
    (OZFTwistedKnotHeaderLength + OZFTwistedKnotFrameLength);

type
  TOZFTwistedKnotStreamStatus = (
    TwistedKnotStreamReady,
    TwistedKnotStreamTransfer,
    TwistedKnotStreamSuspend,
    TwistedKnotStreamReset,
    TwistedKnotStreamComplete
  );

  { TOZFTwistedKnotStream }

  TOZFTwistedKnotStream = class
  protected
    fStreamID: Cardinal;
    fStatus: TOZFTwistedKnotStreamStatus;
    fUniqueID: Cardinal;
    fAddress: Cardinal;
    fLength: Integer;
    fProgress: Integer;

    fBuffer: Pointer;
    fFrameFlags: Array of Byte;
    fFrameCount: Integer;
    fSection: TCriticalSection;

    fLastAccess: TDateTime;
    fNextFrameNo: Integer;

    function getBuffer: Pointer;
  public
    constructor Create(ReceiveLength, ReceiveStreamID, ReceiveUniqueID, ReceiveAddress: Cardinal);
    constructor Create(SendBuffer: Pointer; SendLength, SendStreamID, SendUniqueID, SendAddress: Cardinal);
    destructor Destroy; override;

    procedure fillFrame(frame: Pointer; frameNo: Integer);
    function getBytes(bytes: Pointer; pos, len: Cardinal): Boolean;

    procedure rewindFrameNo;
    function getNextFrameNo: Integer;
    function getFrame(frame: Pointer; frameNo: Integer): Integer;
    procedure markFrame(frameNo: Integer);

    property StreamID: Cardinal read fStreamID;
    property Status: TOZFTwistedKnotStreamStatus read fStatus write fStatus;
    property UniqueID: Cardinal read fUniqueID;
    property Address: Cardinal read fAddress;
    property Length: Integer read fLength;
    property Progress: Integer read fProgress;
    property Buffer: Pointer read getBuffer;
    property LastAccess: TDateTime read fLastAccess;
  end;

implementation

{ TOZFTwistedKnotStream }

function TOZFTwistedKnotStream.getBuffer: Pointer;
begin
  if fLength = fProgress then
    result := fBuffer
  else
    result := nil;
end;

constructor TOZFTwistedKnotStream.Create(ReceiveLength, ReceiveStreamID,
  ReceiveUniqueID, ReceiveAddress: Cardinal);
begin
  inherited Create;

  fLength := ReceiveLength;
  fStreamID := ReceiveStreamID;
  fUniqueID := ReceiveUniqueID;
  fAddress := ReceiveAddress;

  fProgress := 0;
  fStatus := TwistedKnotStreamReady;
  fLastAccess := Now;

  if fLength = 0 then
  begin
    fFrameCount := 0;
    fBuffer := nil;
  end else begin
    fFrameCount := (fLength + OZFTwistedKnotFrameLength - 1) div OZFTwistedKnotFrameLength;
    fBuffer := GetMem(fLength);
  end;

  SetLength(fFrameFlags, fFrameCount);
  FillChar(fFrameFlags[0], fFrameCount, #0);

  fSection := syncobjs.TCriticalSection.Create;
end;

constructor TOZFTwistedKnotStream.Create(SendBuffer: Pointer; SendLength,
  SendStreamID, SendUniqueID, SendAddress: Cardinal);
begin
  inherited Create;

  fStreamID := SendStreamID;
  fUniqueID := SendUniqueID;
  fAddress := SendAddress;

  fProgress := 0;
  fStatus := TwistedKnotStreamReady;
  fLastAccess := Now;

  fNextFrameNo := 0;

  if (SendBuffer = nil) or (SendLength = 0) then
  begin
    fBuffer := nil;
    fLength := 0;
  end else begin
    fBuffer := GetMem(SendLength);
    Move(SendBuffer^, fBuffer^, SendLength);
    fLength := SendLength;
  end;

  fFrameCount := (fLength + OZFTwistedKnotFrameLength - 1) div OZFTwistedKnotFrameLength;
  SetLength(fFrameFlags, fFrameCount);
  FillChar(fFrameFlags[0], fFrameCount, #0);

  fSection := syncobjs.TCriticalSection.Create;
end;

destructor TOZFTwistedKnotStream.Destroy;
begin
  inherited Destroy;

  FreeAndNil(fSection);

  if Assigned(fBuffer) then
  begin
    FreeMem(fBuffer);
    fBuffer := nil;
  end;

  SetLength(fFrameFlags, 0);
end;

procedure TOZFTwistedKnotStream.fillFrame(frame: Pointer; frameNo: Integer);
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
      frameLength := fLength mod OZFTwistedKnotFrameLength;
    if frameLength = 0 then
      frameLength := OZFTwistedKnotFrameLength;

    byteBuffer := PByte(fBuffer) + (frameNo * OZFTwistedKnotFrameLength);
    Move(frame^, byteBuffer^, frameLength);
    fFrameFlags[frameNo] := 1;
    inc(fProgress, frameLength);

    fLastAccess := Now;

    if fLength = fProgress then
      fStatus := TwistedKnotStreamComplete
    else
      fStatus := TwistedKnotStreamTransfer;
  end;
  fSection.Release;
end;

function TOZFTwistedKnotStream.getBytes(bytes: Pointer; pos,
  len: Cardinal): Boolean;
var
  front, rear: Cardinal;
  byteBuffer: PByte;
begin
  result := False;
  if (pos < 0) or (len <= 0) or ((pos + len) >= fLength) then
    exit;

  front := (pos + OZFTwistedKnotFrameLength - 1) div OZFTwistedKnotFrameLength;
  rear := (pos + len + OZFTwistedKnotFrameLength - 1) div OZFTwistedKnotFrameLength;

  while (front <= rear) do
  begin
    if fFrameFlags[front] = 0 then
      exit;
    inc(front);
  end;

  byteBuffer := PByte(fBuffer) + pos;
  Move(byteBuffer^, bytes^, len);
  fLastAccess := Now;
  Result := True;
end;

procedure TOZFTwistedKnotStream.rewindFrameNo;
begin
  fNextFrameNo := 0;
end;

function TOZFTwistedKnotStream.getNextFrameNo: Integer;
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

function TOZFTwistedKnotStream.getFrame(frame: Pointer; frameNo: Integer): Integer;
var
  frameLength: Integer;
  byteBuffer: PByte;
begin
  Result := 0;
  if (frameNo < 0) or (frameNo >= fFrameCount) then
    exit;

  frameLength := 0;
  if frameNo = (fFrameCount - 1) then
    frameLength := fLength mod OZFTwistedKnotFrameLength;
  if frameLength = 0 then
    frameLength := OZFTwistedKnotFrameLength;

  byteBuffer := PByte(fBuffer) + (frameNo * OZFTwistedKnotFrameLength);
  Move(byteBuffer^, frame^, frameLength);
  fStatus := TwistedKnotStreamTransfer;
  result := frameLength;
end;

procedure TOZFTwistedKnotStream.markFrame(frameNo: Integer);
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
      frameLength := fLength mod OZFTwistedKnotFrameLength;
    if frameLength = 0 then
      frameLength := OZFTwistedKnotFrameLength;

    fFrameFlags[frameNo] := 1;
    inc(fProgress, frameLength);

    if fLength = fProgress then
      fStatus := TwistedKnotStreamComplete;
  end;
  fSection.Release;
end;

end.

