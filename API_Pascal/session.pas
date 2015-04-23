unit Session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  SessionID:Word = $5353;
  SessionFunctionIDSignIn = 1;
  SessionFunctionIDSignOut = 2;
  SessionFunctionIDUserInfo = 3;
  SessionErrorCodeNone = 0;
  SessionErrorCodeError = 0;

type

  { TSession }

  TSession = class
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

{ TSession }

constructor TSession.Create;
begin
  fFunctionID := 0;
  fErrorCode := 0;
  fArgs := nil;
end;

constructor TSession.Create(data: Pointer; size: DWord);
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
  if serviceID <> SessionID then
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

destructor TSession.Destroy;
begin
  if Assigned(fArgs) then
    FreeAndNil(fArgs);
  inherited Destroy;
end;

function TSession.getData(var size: DWord): Pointer;
var
  packet: TTwistedKnotPacket;
  i: Integer;
  arg: String;
  ptr: Pointer;
begin
  packet := TTwistedKnotPacket.Create;

  packet.putUInt16(SessionID);
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

