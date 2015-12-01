unit OZFMinions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OZFTwistedKnotPacket;

const
  OZFMinionsID:Word = $5353;
  OZFMinionsFunctionIDSignIn = 1;
  OZFMinionsFunctionIDSignOut = 2;
  OZFMinionsFunctionIDRequestUserInfo = 3;
  OZFMinionsFunctionIDResponse = 4;
  OZFMinionsFunctionIDUserInfo = 5;
  OZFMinionsErrorCodeNone = 0;
  OZFMinionsErrorCodeError = 1;

type

  { TOZFMinions }

  TOZFMinions = class(TOZFTwistedKnotPacket)
  private
    fFunctionID: Word;
    fErrorCode: Word;
    fIdentifier: String;
    fPassword: String;
    fUseNickName: Boolean;
    fClientID: DWord;
    fUserSeq: DWord;
    fUserNickname: String;
    fUserName: String;
    fMobileOnly: Boolean;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);

    function setup: Boolean; override;

    property functionID: Word read fFunctionID write fFunctionID;
    property errorCode: Word read fErrorCode write fErrorCode;
    property identifier: String read fIdentifier write fIdentifier;
    property password: String read fPassword write fPassword;
    property useNickname: Boolean read fUseNickname write fUseNickname;
    property clientID: DWord read fClientID write fClientID;
    property userSeq: DWord read fUserSeq write fUserSeq;
    property userNickname: String read fUserNickname write fUserNickname;
    property userName: String read fUserName write fUserName;
    property mobileOnly: Boolean read fMobileOnly write fMobileOnly;
  end;

implementation

{ TOZFMinions }

constructor TOZFMinions.Create;
begin
  inherited Create;

  fFunctionID := 0;
  fErrorCode := 0;
end;

constructor TOZFMinions.Create(data: Pointer; size: DWord);
var
  serviceID: Word;
begin
  inherited Create(data, size);

  fFunctionID := 0;
  fErrorCode := 0;

  serviceID := getUInt16;
  if serviceID <> OZFMinionsID then
  begin
    exit;
  end;

  fFunctionID := getUInt16;
  fErrorCode := getUInt16;

  case fFunctionID of
    OZFMinionsFunctionIDSignIn:
    begin
        fIdentifier := getString;
        fPassword := getString;
        fUseNickName := (getUInt8 <> 0);
    end;
    OZFMinionsFunctionIDRequestUserInfo:
    begin
        fClientID := getUInt32;
    end;
    OZFMinionsFunctionIDUserInfo:
    begin
        fClientID := getUInt32;
        fUserSeq := getNumber;
        fIdentifier := getString;
        fUserNickname := getString;
        fUserName := getString;
        fMobileOnly := (getUInt8 <> 0);
    end;
  end;
end;

function TOZFMinions.setup: Boolean;
begin
  Result := False;

  putUInt16(OZFMinionsID);
  putUInt16(fFunctionID);
  putUInt16(fErrorCode);

  case fFunctionID of
    OZFMinionsFunctionIDSignIn:
    begin
      if (fIdentifier = '') or (fPassword = '') then
        exit;

      putString(fIdentifier);
      putString(fPassword);
      if fUseNickname then
        putUInt8(1)
      else
        putUInt8(0);
    end;
    OZFMinionsFunctionIDRequestUserInfo:
    begin
      putUint32(fClientID);
    end;
    OZFMinionsFunctionIDUserInfo:
    begin
      putUInt32(fClientID);
      putNumber(fUserSeq);
      putString(fIdentifier);
      putString(fUserNickname);
      putString(fUserName);
      if fMobileOnly then
        putUInt8(1)
      else
        putUInt8(0);
    end;
  end;

  Result := True;
end;

end.

