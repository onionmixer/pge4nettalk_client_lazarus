unit OZFTalkTo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  TalkToID:Word = $5454;

  TalkToFunctionIDAuth = 1;
  TalkToFunctionIDQuit = 2;
  TalkToFunctionIDStat = 3;
  TalkToFunctionIDMessage = 4;
  TalkToFunctionIDGroupJoin = 5;
  TalkToFunctionIDGroupInvite = 6;
  TalkToFunctionIDGroupExit = 7;
  TalkToFunctionIDFileShare = 8;

  TalkToErrorCodeNone = 0;
  TalkToErrorCodeError = 1;

type

  { TTalkTo }

  TTalkTo = class
  private
    fFunctionID: Word;
    fErrorCode: Word;
    fArgs: TStrings;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);
    destructor Destroy; override;

    function getData(var size:DWord): Pointer;

    property functionID: Word read fFunctionID write fFunctionID;
    property errorCode: Word read fErrorCode write fErrorCode;
    property args: TStrings read fArgs write fArgs;
  end;

implementation

uses TwistedKnot;

{ TTalkTo }

constructor TTalkTo.Create;
begin
  fFunctionID := 0;
  fErrorCode := 0;
  fArgs := nil;
end;

constructor TTalkTo.Create(data: Pointer; size: DWord);
var
  packet: TTwistedKnotPacket;
  serviceID: Word;
  count, i: DWord;
  arg: UTF8String;
begin
  fFunctionID := 0;
  fErrorCode := 0;
  fArgs := nil;

  packet := TTwistedKnotPacket.Create(data, size);

  serviceID := packet.getUInt16;
  if serviceID <> TalkToID then
  begin
    packet.Free;
    exit;
  end;

  fFunctionID := packet.getUInt16;
  fErrorCode := packet.getUInt16;
  count := packet.getNumber;

  if count > 0 then
  begin
    fArgs := TStringList.Create;

    for i := 1 to count do
    begin
      arg := packet.getString;
      fArgs.Add(arg);
    end;
  end;
end;

destructor TTalkTo.Destroy;
begin
  if Assigned(fArgs) then
    FreeAndNil(fArgs);
  inherited Destroy;
end;

function TTalkTo.getData(var size: DWord): Pointer;
var
  packet: TTwistedKnotPacket;
  i: Integer;
  arg: String;
  ptr: Pointer;
begin
  packet := TTwistedKnotPacket.Create;

  packet.putUInt16(TalkToID);
  packet.putUInt16(fFunctionID);
  packet.putUInt16(fErrorCode);

  if Assigned(fArgs) and (fArgs.Count > 0) then
  begin
    packet.putNumber(fArgs.Count);
    for i := 0 to fArgs.Count - 1 do
    begin
      arg := fArgs.Strings[i];
      packet.putString(arg);
    end;
  end else begin
    packet.putNumber(0);
  end;

  size := packet.Size;
  ptr := GetMem(size);
  Move(packet.Packet^, ptr^, size);
  packet.Free;

  Result := ptr;
end;

end.

