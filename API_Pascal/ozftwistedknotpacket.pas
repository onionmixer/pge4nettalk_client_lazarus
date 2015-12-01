unit OZFTwistedKnotPacket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TOZFTwistedKnotPacket }

  TOZFTwistedKnotPacket = class
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

    function getNumber: Int64;
    procedure getBytes(Buffer: Pointer; Size: Cardinal);
    function getString: UTF8String;

    procedure putInt8(AValue: ShortInt);
    procedure putInt16(AValue: SmallInt);
    procedure putInt32(AValue: LongInt);

    procedure putUInt8(AValue: Byte);
    procedure putUInt16(AValue: Word);
    procedure putUInt32(AValue: DWord);

    procedure putNumber(AValue: Int64);
    procedure putBytes(Buffer: Pointer; Size: Cardinal);
    procedure putString(AValue: UTF8String);

    function setup:Boolean; virtual; abstract;

    property Packet: PByte read fPacket;
    property PacketSize: Cardinal read fSize;
  end;

implementation

{ TOZFTwistedKnotPacket }

procedure TOZFTwistedKnotPacket.Realloc(minimum: Cardinal);
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

procedure TOZFTwistedKnotPacket.setPosition(AValue: Cardinal);
begin
  if fPosition = AValue then Exit;
  if AValue < fSize then
    fPosition := AValue
  else
    fPosition := fSize;
end;

constructor TOZFTwistedKnotPacket.Create(Capacity: Cardinal);
begin
  if fCapacity = 0 then
    fCapacity := 1024;
  fPacket := GetMem(fCapacity);
  fCapacity := Capacity;
  fSize := 0;
  fPosition := 0;
end;

constructor TOZFTwistedKnotPacket.Create(Packet: Pointer; Size: Cardinal);
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

destructor TOZFTwistedKnotPacket.Destroy;
begin
  inherited Destroy;

  if Assigned(fPacket) then
    FreeMem(fPacket);
end;

function TOZFTwistedKnotPacket.getInt8: ShortInt;
begin
  Result := PShortInt(fPacket + fPosition)^;
  inc(fPosition);
end;

function TOZFTwistedKnotPacket.getInt16: SmallInt;
begin
  Result := BEtoN(PSmallInt(fPacket + fPosition)^);
  inc(fPosition, 2);
end;

function TOZFTwistedKnotPacket.getInt32: LongInt;
begin
  Result := BEtoN(PLongInt(fPacket + fPosition)^);
  inc(fPosition, 4);
end;

function TOZFTwistedKnotPacket.getUInt8: Byte;
begin
  Result := PByte(fPacket + fPosition)^;
  inc(fPosition);
end;

function TOZFTwistedKnotPacket.getUInt16: Word;
begin
  Result := BEtoN(PWord(fPacket + fPosition)^);
  inc(fPosition, 2);
end;

function TOZFTwistedKnotPacket.getUInt32: DWord;
begin
  Result := BEtoN(PDWord(fPacket + fPosition)^);
  inc(fPosition, 4);
end;

function TOZFTwistedKnotPacket.getNumber: Int64;
var
  Value: Byte;
  isMinus: Boolean;
  Shift: Cardinal;
begin
  Result := 0;
  Value := getUInt8;
  isMinus := ((Value and $40) <> 0);

  if (Value and $80) <> 0 then
  begin
    Result := Value and $3F;
    Shift := 6;

    while (True) do
    begin
      Value := getUInt8;
      Result := Result or ((Value and $7F) shl Shift);
      if (Value and $80) = 0 then
        break;
      inc(Shift, 7);
    end;
  end
  else
  begin
    Result := Value and $3F;
  end;

  if isMinus then
    Result := Result * -1;
end;

procedure TOZFTwistedKnotPacket.getBytes(Buffer: Pointer; Size: Cardinal);
begin
  if Size = 0 then
    exit;
  Move((fPacket + fPosition)^, Buffer^, Size);
  inc(fPosition, Size);
end;

function TOZFTwistedKnotPacket.getString: UTF8String;
var
  Len: Int64;
begin
  Len := getNumber;
  SetLength(Result, Len);
  getBytes(PChar(Result), Len);
end;

procedure TOZFTwistedKnotPacket.putInt8(AValue: ShortInt);
begin
  if (fCapacity - fPosition) < 1 then
    Realloc(1);
  PShortInt(fPacket + fPosition)^ := AValue;
  inc(fPosition);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TOZFTwistedKnotPacket.putInt16(AValue: SmallInt);
begin
  if (fCapacity - fPosition) < 2 then
    Realloc(2);
  PSmallInt(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 2);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TOZFTwistedKnotPacket.putInt32(AValue: LongInt);
begin
  if (fCapacity - fPosition) < 4 then
    Realloc(4);
  PLongInt(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 4);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TOZFTwistedKnotPacket.putUInt8(AValue: Byte);
begin
  if (fCapacity - fPosition) < 1 then
    Realloc(1);
  PByte(fPacket + fPosition)^ := AValue;
  inc(fPosition);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TOZFTwistedKnotPacket.putUInt16(AValue: Word);
begin
  if (fCapacity - fPosition) < 2 then
    Realloc(2);
  PWord(fPacket + fPosition)^ := NtoBE(AValue);
  inc(fPosition, 2);
  if fPosition > fSize then
    fSize := fPosition;
end;

procedure TOZFTwistedKnotPacket.putUInt32(AValue: DWord);
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

procedure TOZFTwistedKnotPacket.putNumber(AValue: Int64);
var
  Value: Byte;
  isMinus: Boolean;
begin
  if AValue = 0 then
  begin
    putUInt8(0);
    exit;
  end;

  isMinus := AValue < 0;
  if isMinus then
    AValue := AValue * -1;

  Value := AValue and $3F;
  if isMinus then
    Value := Value or $40;
  AValue := AValue shr 6;

  if AValue > 0 then
    Value := Value or $80;
  putUInt8(Value);

  while AValue > 0 do
  begin
    Value := AValue and $7F;
    AValue := AValue shr 7;
    if AValue > 0 then
      Value := Value or $80;
    putUInt8(Value);
  end;
end;

procedure TOZFTwistedKnotPacket.putBytes(Buffer: Pointer; Size: Cardinal);
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

procedure TOZFTwistedKnotPacket.putString(AValue: UTF8String);
var
  Len: Cardinal;
begin
  Len := Length(AValue);
  putNumber(Len);
  putBytes(PChar(AValue), Len);
end;

end.

