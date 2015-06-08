unit CargoCompany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  /// Cargo Company ID
  CargoCompanyID:Word = $464D;

  /// DN: 파일 다운로드
  CargoCompanyTypeDownload:Word = $444E;
  /// UP: 파일 업로드
  CargoCompanyTypeUpload:Word = $5550;
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
    fContent: Pointer;
    fContentSize: DWord;
    fUser: DWord;
    fFiles: TStrings;
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
    property Content: Pointer read fContent write fContent;
    property ContentSize: DWord read fContentSize write fContentSize;
    property User: DWord read fUser write fUser;
    property Files: TStrings read fFiles write fFiles;
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
  fFiles := nil;
end;

constructor TCargoCompany.Create(data: Pointer; size: DWord);
var
  packet: TTwistedKnotPacket;
  serviceID: Word;
  pos, count, i: DWord;
  filename: UTF8String;
begin
  fType := 0;
  fErrorCode := 0;
  fName := '';
  fContent := nil;
  fContentSize := 0;
  fFiles := nil;

  packet := TTwistedKnotPacket.Create(data, size);

  serviceID := packet.getUInt16;
  if serviceID <> CargoCompanyID then
  begin
    packet.Free;
    exit;
  end;

  fType := packet.getUInt16;
  fErrorCode := packet.getUInt16;

  if fType = CargoCompanyTypeUpload then
  begin
    fName := packet.getString;
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
    fUser := packet.getNumber;
  end
  else if fType = CargoCompanyTypeList then
  begin
    count := packet.getNumber;
    fFiles := TStringList.Create;
    for i := 1 to count do begin
      fFiles.Add(IntToStr(packet.getNumber));
      fFiles.Add(packet.getString);
      fFiles.Add(IntToStr(packet.getNumber));
      fFiles.Add(packet.getString);
      fFiles.Add(IntToStr(packet.getNumber));
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

  if Assigned(fFiles) then
  begin
    fFiles.Free;
    fFiles := nil;
  end;
end;

function TCargoCompany.getData(var size: DWord): Pointer;
var
  packet: TTwistedKnotPacket;
  ptr: Pointer;
begin
  packet := TTwistedKnotPacket.Create;

  packet.putUInt16(CargoCompanyID);
  packet.putUInt16(fType);
  packet.putUInt16(fErrorCode);

  size := 0;
  Result := nil;

  if fType = CargoCompanyTypeUpload then
  begin
    if (fName = '') or (fContent = nil) then
    begin
      packet.Free;
      exit;
    end;

    packet.putString(fName);
    packet.putNumber(fContentSize);
    packet.putBytes(fContent, fContentSize);
  end
  else if fType = CargoCompanyTypeDownload then
  begin
    packet.putNumber(fSeq);
  end
  else if fType = CargoCompanyTypeRemove then
  begin
    packet.putNumber(fSeq);
  end
  else if fType = CargoCompanyTypeShare then
  begin
    packet.putNumber(fSeq);
    packet.putNumber(fUser);
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

