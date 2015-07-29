unit CargoCompany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  /// Cargo Company ID
  CargoCompanyID:Word = $464D;

  /// UR: 파일 업로드 요청
  CargoCompanyTypeUploadRequest:Word = $5552;
  /// UP: 파일 업로드
  CargoCompanyTypeUpload:Word = $5550;
  /// DN: 파일 다운로드
  CargoCompanyTypeDownload:Word = $444E;
  /// TR: 파일 조각 전송
  CargoCompanyTypeTransfer:Word = $5452;
  /// RE: 요청 결과
  CargoCompanyTypeResult:Word = $5245;
  /// RM: 파일 삭제
  CargoCompanyTypeRemove:Word = $524D;
  /// SH: 파일 공유
  CargoCompanyTypeShare:Word = $5348;
  /// LS: 파일 목록
  CargoCompanyTypeList:Word = $4C53;
  /// ST: 파일 목록
  CargoCompanyTypeStat:Word = $5354;

type

  { TCargoCompany }

  TCargoCompany = class
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

    function getData(var size:DWord): Pointer;

    property Command: Word read fType write fType;
    property ErrorCode: Word read fErrorCode write fErrorCode;
    property ResultType: Word read fResultType write fResultType;
    property Seq: DWord read fSeq write fSeq;
    property Name: UTF8String read fName write fName;
    property Size: DWord read fSize write fSize;
    property Mime: UTF8String read fMime write fMime;
    property Expire: DWord read fExpire write fExpire;
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

uses TwistedKnot;

constructor TCargoCompany.Create;
begin
  fType := 0;
  fErrorCode := 0;
  fName := '';
  fContent := nil;
  fContentSize := 0;
  fArgs := nil;
end;

constructor TCargoCompany.Create(data: Pointer; size: DWord);
var
  packet: TTwistedKnotPacket;
  serviceID: Word;
  count, i: DWord;
begin
  fType := 0;
  fErrorCode := 0;
  fName := '';
  fContent := nil;
  fContentSize := 0;
  fArgs := nil;

  packet := TTwistedKnotPacket.Create(data, size);

  serviceID := packet.getUInt16;
  if serviceID <> CargoCompanyID then
  begin
    packet.Free;
    exit;
  end;

  fType := packet.getUInt16;
  fErrorCode := packet.getUInt16;

  if fType = CargoCompanyTypeUploadRequest then
  begin
    fName := packet.getString;
    fSize := packet.getNumber;
    fMD5 := packet.getString;
    fSHA256 := packet.getString;
  end
  else if fType = CargoCompanyTypeUpload then
  begin
    fSeq := packet.getNumber;
    fPosition := packet.getNumber;
  end
  else if fType = CargoCompanyTypeDownload then
  begin
    fSeq := packet.getNumber;
    fPosition := packet.getNumber;
  end
  else if fType = CargoCompanyTypeTransfer then
  begin
    fSeq := packet.getNumber;
    fPosition := packet.getNumber;
    fContentSize := packet.getNumber;
    fContent := GetMem(fContentSize);
    packet.getBytes(fContent, fContentSize);
  end
  else if fType = CargoCompanyTypeResult then
  begin
    fResultType := packet.getUInt16;
    fSeq := packet.getNumber;
    fName := packet.getString;
    fSize := packet.getNumber;
    fMime := packet.getString;
    fExpire := packet.getNumber;
  end
  else if fType = CargoCompanyTypeRemove then
  begin
    fSeq := packet.getNumber;
  end
  else if fType = CargoCompanyTypeShare then
  begin
    fSeq := packet.getNumber;
    count := packet.getNumber;
    fArgs := TStringList.Create;
    for i := 1 to count do begin
      fArgs.Add(IntToStr(packet.getNumber));
    end;
  end
  else if fType = CargoCompanyTypeList then
  begin
    count := packet.getNumber;
    fArgs := TStringList.Create;
    for i := 1 to count do begin
      fArgs.Add(IntToStr(packet.getNumber));
      fArgs.Add(packet.getString);
      fArgs.Add(IntToStr(packet.getNumber));
      fArgs.Add(packet.getString);
      fArgs.Add(IntToStr(packet.getNumber));
    end;
  end
  else if fType = CargoCompanyTypeStat then
  begin
    fSeq := packet.getNumber;
  end
  else
  begin
    fType := 0;
  end;

  packet.Free;
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

function TCargoCompany.getData(var size: DWord): Pointer;
var
  packet: TTwistedKnotPacket;
  ptr: Pointer;
  i: Integer;
begin
  packet := TTwistedKnotPacket.Create;

  packet.putUInt16(CargoCompanyID);
  packet.putUInt16(fType);
  packet.putUInt16(fErrorCode);

  size := 0;
  Result := nil;

  if fType = CargoCompanyTypeUploadRequest then
  begin
    if (fName = '') or (fMD5 = '') or (fSHA256 = '') then
    begin
      packet.Free;
      exit;
    end;

    packet.putString(fName);
    packet.putNumber(fSize);
    packet.putString(fMD5);
    packet.putString(fSHA256);
  end
  else if fType = CargoCompanyTypeUpload then
  begin
    packet.putNumber(fSeq);
    packet.putNumber(fPosition);
  end
  else if fType = CargoCompanyTypeDownload then
  begin
    packet.putNumber(fSeq);
    packet.putNumber(fPosition);
  end
  else if fType = CargoCompanyTypeTransfer then
  begin
    if (fContent = nil) then
    begin
      packet.Free;
      exit;
    end;

    packet.putNumber(fSeq);
    packet.putNumber(fPosition);
    packet.putNumber(fContentSize);
    packet.putBytes(fContent, fContentSize);
  end
  else if fType = CargoCompanyTypeRemove then
  begin
    packet.putNumber(fSeq);
  end
  else if fType = CargoCompanyTypeShare then
  begin
    packet.putNumber(fSeq);
    if (fArgs = nil) or (fArgs.Count = 0) then
    begin
      packet.putNumber(0);
    end
    else
    begin
      packet.putNumber(fArgs.Count);
      for i := 0 to fArgs.Count - 1 do
        packet.putNumber(StrToInt(fArgs[i]));
    end;
  end
  else if fType = CargoCompanyTypeList then
  begin
    packet.putNumber(0);
  end
  else if fType = CargoCompanyTypeStat then
  begin
    packet.putNumber(fSeq);
  end
  else
  begin
    packet.Free;
    exit;
  end;

  size := packet.Size;
  ptr := GetMem(size);
  Move(packet.Packet^, ptr^, size);
  packet.Free;

  Result := ptr;
end;

end.

