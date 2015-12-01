unit OZFBlahBlah;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OZFTwistedKnotPacket;

const
  OZFBlahBlahID:Word = $5454;

  OZFBlahBlahFunctionIDAuth = 1;
  OZFBlahBlahFunctionIDQuit = 2;
  OZFBlahBlahFunctionIDStat = 3;
  OZFBlahBlahFunctionIDMessage = 4;
  OZFBlahBlahFunctionIDGroupJoin = 5;
  OZFBlahBlahFunctionIDGroupInvite = 6;
  OZFBlahBlahFunctionIDGroupExit = 7;
  OZFBlahBlahFunctionIDFileShare = 8;

  OZFBlahBlahErrorCodeNone = 0;
  OZFBlahBlahErrorCodeError = 1;

type

  { TOZFBlahBlah }

  TOZFBlahBlah = class(TOZFTwistedKnotPacket)
  private
    fFunctionID: Word;
    fErrorCode: Word;
    fArgs: TStrings;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);
    destructor Destroy; override;

    function setup: Boolean; override;

    property functionID: Word read fFunctionID write fFunctionID;
    property errorCode: Word read fErrorCode write fErrorCode;
    property args: TStrings read fArgs write fArgs;
  end;

implementation

{ TOZFBlahBlah }

constructor TOZFBlahBlah.Create;
begin
  inherited Create;

  fFunctionID := 0;
  fErrorCode := 0;
  fArgs := nil;
end;

constructor TOZFBlahBlah.Create(data: Pointer; size: DWord);
var
  serviceID: Word;
  count, i: DWord;
  arg: UTF8String;
begin
  inherited Create(data, size);

  fFunctionID := 0;
  fErrorCode := 0;
  fArgs := nil;

  serviceID := getUInt16;
  if serviceID <> OZFBlahBlahID then
  begin
    exit;
  end;

  fFunctionID := getUInt16;
  fErrorCode := getUInt16;
  count := getNumber;

  if count > 0 then
  begin
    fArgs := TStringList.Create;

    for i := 1 to count do
    begin
      arg := getString;
      fArgs.Add(arg);
    end;
  end;
end;

destructor TOZFBlahBlah.Destroy;
begin
  if Assigned(fArgs) then
    FreeAndNil(fArgs);
  inherited Destroy;
end;

function TOZFBlahBlah.setup: Boolean;
var
  i: Integer;
  arg: String;
begin
  putUInt16(OZFBlahBlahID);
  putUInt16(fFunctionID);
  putUInt16(fErrorCode);

  if Assigned(fArgs) and (fArgs.Count > 0) then
  begin
    putNumber(fArgs.Count);
    for i := 0 to fArgs.Count - 1 do
    begin
      arg := fArgs.Strings[i];
      putString(arg);
    end;
  end else begin
    putNumber(0);
  end;

  Result := True;
end;

end.

