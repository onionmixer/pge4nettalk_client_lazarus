unit OZFCargoCompany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OZFTwistedKnotPacket;

const
  /// Cargo Company ID
  OZFCargoCompanyID:Word = $464D;

  /// UR: 파일 업로드 요청
  OZFCargoCompanyFunctionIDUploadRequest:Word = $5552;
  /// UP: 파일 업로드
  OZFCargoCompanyFunctionIDUpload:Word = $5550;
  /// DN: 파일 다운로드
  OZFCargoCompanyFunctionIDDownload:Word = $444E;
  /// TR: 파일 조각 전송
  OZFCargoCompanyFunctionIDTransfer:Word = $5452;
  /// CX: 파일 전송 취소
  OZFCargoCompanyFunctionIDCancel:Word = $4358;
  /// RE: 요청 결과
  OZFCargoCompanyFunctionIDResult:Word = $5245;
  /// RM: 파일 삭제
  OZFCargoCompanyFunctionIDRemove:Word = $524D;
  /// SH: 파일 공유
  OZFCargoCompanyFunctionIDShare:Word = $5348;
  /// LS: 파일 목록
  OZFCargoCompanyFunctionIDList:Word = $4C53;
  /// ST: 파일 목록
  OZFCargoCompanyFunctionIDStat:Word = $5354;

type

  { TCargoCompany }

  TCargoCompany = class(TOZFTwistedKnotPacket)
  private
    fType: Word;
    fErrorCode: Word;
    fResultType: Word;
    fSeq: DWord;
    fName: UTF8String;
    fSize: DWord;
    fMime: UTF8String;
    fExpire: DWord;
    fMD5: UTF8String;
    fSHA256: UTF8String;
    fPosition: DWord;
    fContent: Pointer;
    fContentSize: DWord;
    fUser: DWord;
    fArgs: TStrings;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);
    destructor Destroy; override;

    function setup: Boolean; override;

    property Command: Word read fType write fType;
    property ErrorCode: Word read fErrorCode write fErrorCode;
    property ResultType: Word read fResultType write fResultType;
    property Seq: DWord read fSeq write fSeq;
    property Name: UTF8String read fName write fName;
    property Size: DWord read fSize write fSize;
    property Mime: UTF8String read fMime;
    property Expire: DWord read fExpire;
    property MD5: UTF8String read fMD5 write fMD5;
    property SHA256: UTF8String read fSHA256 write fSHA256;
    property Position: DWord read fPosition write fPosition;
    property Content: Pointer read fContent write fContent;
    property ContentSize: DWord read fContentSize write fContentSize;
    property User: DWord read fUser write fUser;
    property Args: TStrings read fArgs write fArgs;
  end;

implementation

{ TCargoCompany }

constructor TCargoCompany.Create;
begin
  inherited Create;

  fType := 0;
  fErrorCode := 0;
  fName := '';
  fSize := 0;
  fMime := '';
  fExpire := 0;
  fMD5 := '';
  fSHA256 := '';
  fPosition := 0;
  fContent := nil;
  fContentSize := 0;
  fUser := 0;
  fArgs := nil;
end;

constructor TCargoCompany.Create(data: Pointer; size: DWord);
var
  serviceID: Word;
  count, i: DWord;
begin
  inherited Create(data, size);

  fType := 0;
  fErrorCode := 0;
  fName := '';
  fContent := nil;
  fContentSize := 0;
  fArgs := nil;

  serviceID := getUInt16;
  if serviceID <> OZFCargoCompanyID then
  begin
    exit;
  end;

  fType := getUInt16;
  fErrorCode := getUInt16;

  if fType = OZFCargoCompanyFunctionIDUploadRequest then
  begin
    fName := getString;
    fSize := getNumber;
    fMD5 := getString;
    fSHA256 := getString;
  end
  else if fType = OZFCargoCompanyFunctionIDUpload then
  begin
    fSeq := getNumber;
    fPosition := getNumber;
  end
  else if fType = OZFCargoCompanyFunctionIDDownload then
  begin
    fSeq := getNumber;
    fPosition := getNumber;
  end
  else if fType = OZFCargoCompanyFunctionIDTransfer then
  begin
    fSeq := getNumber;
    fPosition := getNumber;
    fContentSize := getNumber;
    fContent := GetMem(fContentSize);
    getBytes(fContent, fContentSize);
  end
  else if fType = OZFCargoCompanyFunctionIDResult then
  begin
    fResultType := getUInt16;
    fSeq := getNumber;
    fName := getString;
    fSize := getNumber;
    fMime := getString;
    fExpire := getNumber;
  end
  else if fType = OZFCargoCompanyFunctionIDRemove then
  begin
    fSeq := getNumber;
  end
  else if fType = OZFCargoCompanyFunctionIDShare then
  begin
    fSeq := getNumber;
    count := getNumber;
    fArgs := TStringList.Create;
    for i := 1 to count do begin
      fArgs.Add(IntToStr(getNumber));
    end;
  end
  else if fType = OZFCargoCompanyFunctionIDList then
  begin
    count := getNumber;
    fArgs := TStringList.Create;
    for i := 1 to count do begin
      fArgs.Add(IntToStr(getNumber));
      fArgs.Add(getString);
      fArgs.Add(IntToStr(getNumber));
      fArgs.Add(getString);
      fArgs.Add(IntToStr(getNumber));
    end;
  end
  else if fType = OZFCargoCompanyFunctionIDStat then
  begin
    fSeq := getNumber;
  end
  else
  begin
    fType := 0;
  end;
end;

destructor TCargoCompany.Destroy;
begin
  if Assigned(fContent) then
  begin
    FreeMem(fContent);
    fContent := nil;
    fContentSize := 0;
  end;

  if Assigned(fArgs) then
  begin
    fArgs.Free;
    fArgs := nil;
  end;
end;

function TCargoCompany.setup: Boolean;
var
  i: Integer;
begin
  Result := False;

  putUInt16(OZFCargoCompanyID);
  putUInt16(fType);
  putUInt16(fErrorCode);

  if fType = OZFCargoCompanyFunctionIDUploadRequest then
  begin
    if (fName = '') or (fMD5 = '') or (fSHA256 = '') then
    begin
      exit;
    end;

    putString(fName);
    putNumber(fSize);
    putString(fMD5);
    putString(fSHA256);
  end
  else if fType = OZFCargoCompanyFunctionIDUpload then
  begin
    putNumber(fSeq);
    putNumber(fPosition);
  end
  else if fType = OZFCargoCompanyFunctionIDDownload then
  begin
    putNumber(fSeq);
    putNumber(fPosition);
  end
  else if fType = OZFCargoCompanyFunctionIDTransfer then
  begin
    if (fContent = nil) then
    begin
      exit;
    end;

    putNumber(fSeq);
    putNumber(fPosition);
    putNumber(fContentSize);
    putBytes(fContent, fContentSize);
  end
  else if fType = OZFCargoCompanyFunctionIDRemove then
  begin
    putNumber(fSeq);
  end
  else if fType = OZFCargoCompanyFunctionIDShare then
  begin
    putNumber(fSeq);
    if (fArgs = nil) or (fArgs.Count = 0) then
    begin
      putNumber(0);
    end
    else
    begin
      putNumber(fArgs.Count);
      for i := 0 to fArgs.Count - 1 do
        putNumber(StrToInt(fArgs[i]));
    end;
  end
  else if fType = OZFCargoCompanyFunctionIDList then
  begin
    putNumber(0);
  end
  else if fType = OZFCargoCompanyFunctionIDStat then
  begin
    putNumber(fSeq);
  end
  else
  begin
    exit;
  end;

  Result := True;
end;

end.

