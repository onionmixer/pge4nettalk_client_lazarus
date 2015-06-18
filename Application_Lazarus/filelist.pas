unit filelist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, cDataStructs;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    fShare: String;
    fShareList: TSparseStringArray;
    procedure SetShare(AValue: String);
  public
    { public declarations }
    function GetSharedTarget(UniqueID: DWord):String;

    property Share: String read fShare write SetShare;
  end;

var
  FormFileList: TFormFileList;

implementation

{$R *.lfm}

uses main, chat, LazUTF8Classes, TwistedKnot, CargoCompany;

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
  FormMain.SendData(data, size, $5454);
  cargo.Free;
end;

procedure TFormFileList.FormCreate(Sender: TObject);
begin
  fShareList := TSparseStringArray.Create;
end;

procedure TFormFileList.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fShareList);
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

function TFormFileList.GetSharedTarget(UniqueID: DWord): String;
begin
  if fShareList.HasItem(UniqueID) then
  begin
    Result := fShareList[UniqueID];
    fShareList.Delete(UniqueID);
  end
  else
    Result := '';
end;

procedure TFormFileList.ButtonUploadClick(Sender: TObject);
var
  cargo: TCargoCompany;
  fsIn: TFileStreamUTF8;
  size, read: DWord;
  data: Pointer;
begin
  if OpenDialog1.Execute then
  begin
    data := nil;

    try
      fsIn := TFileStreamUTF8.Create(OpenDialog1.FileName, fmOpenRead);
      size := fsIn.Size;
      data := GetMem(size);
      if data = nil then
      begin
        FreeAndNil(fsIn);
        Application.MessageBox('Not enough memory', 'Confirm', 0);
        exit;
      end;
      read := fsIn.Read(data^, size);
      FreeAndNil(fsIn);
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar(E.Message), 'File I/O Error', 0);
        if data <> nil then
          FreeMem(data);
        exit;
      end;
    end;

    if size <> read then
    begin
      Application.MessageBox('Error on file reading', 'Confirm', 0);
      FreeMem(data);
      exit;
    end;

    cargo := TCargoCompany.Create;
    cargo.Command := CargoCompanyTypeUpload;
    cargo.Name := ExtractFileName(OpenDialog1.FileName);
    cargo.ContentSize := size;
    cargo.Content := data;

    data := cargo.getData(size);
    FormMain.SendData(data, size, $5454);
    cargo.Free;
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
    Application.MessageBox('Select file in List', 'Confirm', 0);
    exit;
  end;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeRemove;
  cargo.Seq := StrToInt(ListView1.Selected.SubItems[2]);

  data := cargo.getData(size);
  FormMain.SendData(data, size, $5454);
  cargo.Free;
end;

procedure TFormFileList.ButtonShareClick(Sender: TObject);
var
  cargo: TCargoCompany;
  data: Pointer;
  size, uniqueID: DWord;
  form: TFormChat;
  i: Integer;
begin
  if ListView1.SelCount <> 1 then
  begin
    Application.MessageBox('Select file in List', 'Confirm', 0);
    exit;
  end;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeShare;
  cargo.Seq := StrToInt(ListView1.Selected.SubItems[2]);
  if fShare[1] = '#' then
  begin
    form := FormMain.ChatForm(fShare, False);
    if Assigned(form) then
    begin
      cargo.Args := TStringList.Create;
      cargo.Args.AddStrings(form.Users);
    end;
  end
  else
  begin
    cargo.Args := TStringList.Create;
    cargo.Args.Add(fShare);
  end;

  data := cargo.getData(size);
  cargo.Free;

  uniqueID := FormMain.SendData(data, size, $5454);
  fShareList[uniqueID] := fShare;

  Hide;
end;

end.

