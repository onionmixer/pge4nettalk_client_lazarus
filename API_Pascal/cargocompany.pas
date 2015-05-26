unit CargoCompany;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  CargoCompanyID:Word = $464D;
  /// UP: 파일 업로드 요청
  CargoCompanyTypeUpload = $5550;
  /// RM: 파일 삭제 요청
  CargoCompanyTypeRemove = $524D;
  /// DN: 파일 다운로드 요청
  CargoCompanyTypeDownload = $444E;
  /// LS: 파일 목록 요청
  CargoCompanyTypeList = $4C53;
  /// RE: 요청에 대한 결과값
  CargoCompanyTypeResult = $5245;

type

  { TCargoCompany }

  TCargoCompany = class
  private
    fCommand: Word;
    fErrorCode: Word;
    fName: UTF8String;
    fContent: Pointer;
    fContentSize: DWord;
    fOwner: UTF8String;
    fFiles: TStrings;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);
    destructor Destroy; override;

    function getData(var size:DWord): Pointer;

    property Command: Word read FCommand write fCommand;
    property ErrorCode: Word read fErrorCode write fErrorCode;
    property Name: UTF8String read fName write fName;
    property Content: Pointer read fContent write fContent;
    property ContentSize: DWord read fContentSize write fContentSize;
    property Owner: UTF8String read fOwner write fOwner;
    property Files: TStrings read fFiles write fFiles;
  end;

implementation

{ TCargoCompany }

uses TwistedKnot;

constructor TCargoCompany.Create;
begin
  fCommand := 0;
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
  fCommand := 0;
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

  fCommand := packet.getUInt16;

  if fCommand = CargoCompanyTypeUpload then
  begin
    fName := packet.getString;
    fContentSize := packet.getNumber;
    fContent := GetMem(fContentSize);
    packet.getBytes(fContent, fContentSize);
  end
  else if fCommand = CargoCompanyTypeList then
  begin
    count := packet.getNumber;
    fFiles := TStringList.Create;
    for i := 1 to count do begin
      fFiles.Add(IntToStr(packet.getNumber));
      fFiles.Add(packet.getString);
      fFiles.Add(IntToStr(packet.getNumber));
      fFiles.Add(IntToStr(packet.getNumber));
    end;
  end
  else if fCommand = CargoCompanyTypeResult then
  begin
    fName := packet.getString;
    fErrorCode := packet.getUInt16;
  end
  else
  begin
    fCommand := 0;
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
  packet.putUInt16(fCommand);

  size := 0;
  Result := nil;

  if fCommand = CargoCompanyTypeUpload then
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
  else if fCommand = CargoCompanyTypeRemove then
  begin
    if (fName = '') then
    begin
      packet.Free;
      exit;
    end;

    packet.putString(fName);
  end
  else if fCommand = CargoCompanyTypeDownload then
  begin
    if (fName = '') or (fOwner = '') then
    begin
      packet.Free;
      exit;
    end;

    packet.putString(fName);
    packet.putString(fOwner);
  end
  else if fCommand = CargoCompanyTypeList then
  begin
    packet.putNumber(0);
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

