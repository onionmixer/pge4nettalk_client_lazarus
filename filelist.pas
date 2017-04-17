unit filelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls;

type

  { TFormFileList }

  TFormFileList = class(TForm)
    ButtonShare: TButton;
    ButtonUpload: TButton;
    ButtonDelete: TButton;
    ButtonRefresh: TButton;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonShareClick(Sender: TObject);
    procedure ButtonUploadClick(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
    fShare: String;
    procedure SetShare(AValue: String);
  public
    { public declarations }
    property Share: String read fShare write SetShare;
  end;

var
  FormFileList: TFormFileList;

implementation

{$R *.lfm}

uses main,
  LCLType, LazFileUtils,
  OZFTwistedKnotPacket, OZFCargoCompany;

{ TFormFileList }

procedure TFormFileList.ButtonRefreshClick(Sender: TObject);
var
  cargo: TOZFCargoCompany;
begin
  cargo := TOZFCargoCompany.Create;
  cargo.Command := OZFCargoCompanyFunctionIDList;
  FormMain.SendData(cargo, OZFCargoCompanyID);
  cargo.Free;
end;

procedure TFormFileList.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
const
  units: array[0..4] of String = ('B', 'KB', 'MB', 'GB', 'TB');
var
  FileName: String;
  FileSize: Double;
  FileList: TStringList;
  msg: String;
  doUpload: Boolean;
  i: Integer;
begin
  FileList := TStringList.Create;
  FileSize := 0;
  for FileName in FileNames do
  begin
    if not DirectoryExistsUTF8(FileName) then
    begin
      FileList.Add(FileName);
      FileSize := FileSize + FileSizeUTF8(FileName);
    end;
  end;

  msg := '';
  if FileList.Count = 1 then
    msg := 'Upload 1 file'
  else if FileList.Count > 1 then
    msg := 'Upload ' + IntToStr(FileList.Count) + ' files';

  if msg = '' then
    doUpload := False
  else
  begin
    for i := 0 to 4 do
    begin
      if FileSize < 1024 then
        break;
      FileSize := FileSize / 1024;
    end;

    if i = 0 then
      msg := msg + '(' + IntToStr(Trunc(FileSize)) + ' ' + units[i] + ') ?'
    else
      msg := msg + ' (' + Format('%.2f', [FileSize]) + ' ' + units[i] + ') ?';

    doUpload := (Application.MessageBox(PChar(msg), 'Upload', MB_YESNO) = IDYES);
  end;

  if doUpload then
  begin
    for i := 0 to FileList.Count - 1 do
      FormMain.AddUpload('', FileList[i]);
  end;

  FileList.Free;
end;

procedure TFormFileList.SetShare(AValue: String);
begin
  if (ButtonShare.Visible = (AValue <> '')) and (fShare = AValue) then Exit;
  fShare := AValue;
  if fShare = '' then
  begin
    Caption := 'File List';
    ButtonShare.Visible := False;
  end
  else
  begin
    Caption := 'File Share - ' + FormMain.GetNick(fShare);
    ButtonShare.Visible := True;
  end;
end;

procedure TFormFileList.ButtonUploadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FormMain.AddUpload(fShare, OpenDialog1.FileName);
  end;
end;

procedure TFormFileList.ButtonDeleteClick(Sender: TObject);
var
  cargo: TOZFCargoCompany;
begin
  if ListView1.SelCount <> 1 then
  begin
    Application.MessageBox('Select file in List', 'Confirm', MB_ICONWARNING);
    exit;
  end;

  cargo := TOZFCargoCompany.Create;
  cargo.Command := OZFCargoCompanyFunctionIDRemove;
  cargo.FileSeq := StrToInt(ListView1.Selected.SubItems[2]);
  FormMain.SendData(cargo, OZFCargoCompanyID);
  cargo.Free;
end;

procedure TFormFileList.ButtonShareClick(Sender: TObject);
begin
  if fShare = '' then
    exit;

  if ListView1.SelCount <> 1 then
  begin
    Application.MessageBox('Select file in List', 'Confirm', MB_ICONWARNING);
    exit;
  end;

  FormMain.FileShare(fShare, ListView1.Selected.SubItems[2]);
  Hide;
end;

end.

