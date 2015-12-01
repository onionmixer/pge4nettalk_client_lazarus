unit OZFDimension;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OZFTwistedKnotPacket;

const
  /// Dimension ID
  OZFDimensionID:Word = $0404;

type
  TOZFDimensionTable = Array Of Array Of String;

  { TOZFDimension }

  TOZFDimension = class(TOZFTwistedKnotPacket)
  private
    fErrorCode: Word;
    fFlags: Word;
    fQueryID: String;
    fTable: TOZFDimensionTable;
  public
    constructor Create;
    constructor Create(data: Pointer; size: DWord);

    function Setup: Boolean; override;

    property ErrorCode: Word read fErrorCode write fErrorCode;
    property Flags: Word read fFlags write fFlags;
    property QueryID: String read fQueryID write fQueryID;
    property Table: TOZFDimensionTable read fTable write fTable;
  end;

implementation

{ TOZFDimension }

constructor TOZFDimension.Create;
begin
  inherited Create;

  fErrorCode := 0;
  fFlags := 0;
  fQueryID := '';
  fTable := nil;
end;

constructor TOZFDimension.Create(data: Pointer; size: DWord);
var
  serviceID: Word;
  rows, cols, i, j: Integer;
begin
  inherited Create(data, size);

  fErrorCode := 0;
  fFlags := 0;
  fQueryID := '';
  fTable := nil;

  serviceID := getUInt16;
  if serviceID <> OZFDimensionID then
  begin
    exit;
  end;

  fQueryID := getString;
  if Length(fQueryID) <> 14 then
  begin
    fQueryID := '';
    exit;
  end;

  fErrorCode := getUInt16;
  fFlags := getUInt16;

  rows := getNumber;
  cols := getNumber;

  if (rows > 0) and (cols > 0) then
  begin
    SetLength(fTable, rows, cols);
    for i := 0 to rows - 1 do
    begin
      for j := 0 to cols - 1 do
      begin
        fTable[i, j] := getString;
      end;
    end;
  end;
end;

function TOZFDimension.Setup: Boolean;
var
  i, j: Integer;
begin
  Result := False;

  if Length(fQueryID) <> 14 then
    exit;

  putUInt16(OZFDimensionID);
  putString(fQueryID);
  putUInt16(fErrorCode);
  putUInt16(fFlags);

  if (fTable <> nil) And (Length(fTable) > 0) then
  begin
    putNumber(Length(fTable));
    putNumber(Length(fTable[0]));

    for i := Low(fTable) to High(fTable) do
    begin
      for j := Low(fTable[i]) to High(fTable[i]) do
      begin
        putString(fTable[i, j]);
      end;
    end;
  end
  else
  begin
    putNumber(0);
    putNumber(0);
  end;

  Result := True;
end;

end.

