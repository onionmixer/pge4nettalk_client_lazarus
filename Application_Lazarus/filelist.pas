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

uses main, chat, LCLType, TwistedKnot, CargoCompany;

{ TFormFileList }

procedure TFormFileList.ButtonRefreshClick(Sender: TObject);
var
  cargo: TCargoCompany;
  data: Pointer;
  size: DWord;
begin
  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeList;
  data := cargo.getData(size);
  FormMain.SendData(data, size, CargoCompanyID);
  cargo.Free;
end;

procedure TFormFileList.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  FileName: String;
begin
  for FileName in FileNames do
  begin
    if not DirectoryExistsUTF8(FileName) then
      FormMain.AddUpload('', FileName);
  end;
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
  cargo: TCargoCompany;
  data: Pointer;
  size: DWord;
begin
  if ListView1.SelCount <> 1 then
  begin
    Application.MessageBox('Select file in List', 'Confirm', MB_ICONWARNING);
    exit;
  end;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeRemove;
  cargo.Seq := StrToInt(ListView1.Selected.SubItems[2]);

  data := cargo.getData(size);
  FormMain.SendData(data, size, CargoCompanyID);
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

