unit transfer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, udptransfer;

type

  { TFormTransfer }

  TFormTransfer = class(TForm)
    ButtonStop: TButton;
    ButtonStart: TButton;
    ButtonDel: TButton;
    ButtonAdd: TButton;
    LabelName: TLabel;
    LabelSize: TLabel;
    ListViewFiles: TListView;
    OpenDialogSend: TOpenDialog;
    ProgressBarSize: TProgressBar;
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDelClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
    fIsSend: Boolean;
    fUser: String;
    fID: String;
    fUDPTransfer: TUDPTransfer;
    fCurrent: Integer;
    fRecvSize: Int64;
    fStart: Boolean;
    procedure AddFile(FileName: String);
  public
    { public declarations }
    procedure SetUser(User: String; ID: String; IsSend: Boolean);
    procedure Connect(Conn: String);
    procedure Recv(FileInfo: String);
    procedure OnProgress(Sender: TObject; Progress: Int64);
    procedure GetDropFilesFromChat;
    property TransferID: String read fID;
  end;

//var
//  FormTransfer: TFormTransfer;

implementation

{$R *.lfm}

uses main, chat;

function FileSizeUTF8(FileName: String): Int64;
{$IFNDEF UNIX}
var
  sr: TSearchRec;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result:=filesize(FileName);
{$ELSE}
  if FindFirstUTF8(FileName, faAnyFile, sr ) = 0 then
  begin
     Result := Int64(sr.FindData.nFileSizeHigh);
     Result := (Result shl 32) + Int64(sr.FindData.nFileSizeLow)
  end
  else
     Result := 0;
  FindCloseUTF8(sr);
{$ENDIF}
end;

{ TFormTransfer }

procedure TFormTransfer.ButtonAddClick(Sender: TObject);
var
  i: Integer;
begin
  if OpenDialogSend.Execute then
  begin
    for i := 0 to OpenDialogSend.Files.Count-1 do
      AddFile(OpenDialogSend.Files[i]);
  end;
end;

procedure TFormTransfer.ButtonDelClick(Sender: TObject);
var
  i: Integer;
  enable: Boolean;
begin
  enable := False;
  for i := ListViewFiles.Items.Count - 1 downto 0 do
  begin
    if not ListViewFiles.Items[i].Selected then
    begin
      if not fStart and not enable and
        (ListViewFiles.Items[i].SubItems[1] = 'Ready') then
        enable := True;
      continue;
    end;
    if ListViewFiles.Items[i].SubItems[1] <> 'Ready' then continue;
    ListViewFiles.Items[i].Delete;
  end;
  ButtonStart.Enabled := enable;
end;

procedure TFormTransfer.ButtonStartClick(Sender: TObject);
begin
  if fIsSend then
  begin
    fCurrent := 0;
    FormMain.SendMsg('FILEreq|' + fUser + '|' + fID);
    LabelName.Caption := 'Ready to accept transfer from ' + fUser;
  end else begin
    FormMain.SendMsg('FILEres|' + fUser + '|' + fID);
    LabelName.Caption := 'File trasfer from ' + fUser;
  end;
  ButtonStart.Enabled := False;
  fStart := True;
end;

procedure TFormTransfer.ButtonStopClick(Sender: TObject);
begin
  FormMain.SendMsg('FILErej|' + fUser + '|' + fID);
end;

procedure TFormTransfer.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  FormMain.SendMsg('FILErej|' + fUser + '|' + fID);
end;

procedure TFormTransfer.FormCreate(Sender: TObject);
begin
  fUDPTransfer := TUDPTransfer.Create;
  fUDPTransfer.OnDebug := @FormMain.OnDebug;
  fUDPTransfer.OnProgress := @OnProgress;
  fUDPTransfer.Start;
  FormMain.OnDebug(Self, 'transfer created');
  fStart := False;
end;

procedure TFormTransfer.FormDestroy(Sender: TObject);
begin
  fUDPTransfer.Stop;
end;

procedure TFormTransfer.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
begin
  for i := 0 to High(FileNames) do
  begin
    AddFile(FileNames[i]);
  end;
end;

procedure TFormTransfer.AddFile(FileName: String);
var
  i: Integer;
  item: TListItem;
  size: Int64;
  realsize: String;
begin
  if (FileName = '') or not FileExistsUTF8(FileName) then exit;
  for i := 0 to ListViewFiles.Items.Count - 1 do
  begin
    if (ListViewFiles.Items[i].SubItems[2] = FileName) then
      exit;
  end;

  item := ListViewFiles.Items.Add;
  item.Caption := ExtractFileName(FileName);
  size := FileSizeUTF8(FileName);
  realsize := IntToStr(size);
  if size < 1024 then
    item.SubItems.Add(IntToStr(size) + ' B')
  else if size < (1024 * 1024) then
    item.SubItems.Add(IntToStr(size div 1024) + ' KB')
  else
    item.SubItems.Add(IntToStr(size div (1024 * 1024)) + ' MB');
  item.SubItems.Add('Ready'); // Status
  item.SubItems.Add(filename); // Full Path
  item.SubItems.Add(realsize); // Real Size

  if not fStart then
    ButtonStart.Enabled := True;
end;

procedure TFormTransfer.GetDropFilesFromChat;
var
  form: TFormChat;
  filename: String;
begin
  form := FormMain.ChatForm(fUser, False);
  if form = nil then exit;
  while true do
  begin
    filename := ''; //form.GetDropFile;
    if filename = '' then break;
    AddFile(filename);
  end;
end;

procedure TFormTransfer.SetUser(User: String; ID: String; IsSend: Boolean);
begin
  fUser := User;
  fID := ID;
  fIsSend := IsSend;
  if fIsSend then
  begin
    Caption := 'File Send - ' + fUser;
    ButtonAdd.Visible := True;
    ButtonDel.Visible := True;
    ButtonStart.Enabled := False;
    GetDropFilesFromChat;
  end else begin
    Caption := 'File Receive - ' + fUser;
    ButtonAdd.Visible := False;
    ButtonDel.Visible := False;
    ButtonStart.Enabled := True;
  end;
end;

procedure TFormTransfer.Connect(Conn: String);
var
  addr: String;
  port: Integer;
  cut: Integer;
begin
  cut := Pos('|', Conn);
  addr := Copy(Conn, 1, cut - 1);
  Delete(conn, 1, cut);
  port := StrToInt(conn);
  fUDPTransfer.SendTo(addr, port, fID + '|' + fUser);
end;

procedure TFormTransfer.Recv(FileInfo: String);
var
  filename: String;
  cut: Integer;
  item: TListItem;
  size: Int64;
begin
  cut := Pos('|', FileInfo);
  filename := Copy(FileInfo, 1, cut - 1);
  Delete(FileInfo, 1, cut);
  fRecvSize := StrToInt(FileInfo);
  size := fRecvSize;

  fCurrent := ListViewFiles.Items.Count;
  item := ListViewFiles.Items.Add;
  item.Caption := filename;
  if size < 1024 then
    item.SubItems.Add(IntToStr(size) + ' B')
  else if size < (1024 * 1024) then
    item.SubItems.Add(IntToStr(size div 1024) + ' KB')
  else
    item.SubItems.Add(IntToStr(size div (1024 * 1024)) + ' MB');

  item.SubItems.Add('Receiving');

  filename := ExtractFilePath(Application.ExeName) + filename;
  item.SubItems.Add(filename);
  item.SubItems.Add(FileInfo);

  LabelName.Caption := 'File: ' + filename;
  LabelSize.Caption := 'Size: 0 B / ' + IntToStr(fRecvSize) + ' B';

  fUDPTransfer.Recv(filename);
end;

procedure TFormTransfer.OnProgress(Sender: TObject; Progress: Int64);
var
  i: Integer;
  size: String;
  permil: Int64;
begin
  if Progress = -1 then
  begin
    FormMain.SendMsg('FILErej|' + fUser + '|' + fID);
    exit;
  end
  else if Progress = 0 then
  begin
    if fIsSend then
    begin
      for i := fCurrent to ListViewFiles.Items.Count - 1 do
      begin
        if ListViewFiles.Items[i].SubItems[1] = 'Sending' then
        begin
          ListViewFiles.Items[i].SubItems[1] := 'Sent';
        end
        else if ListViewFiles.Items[i].SubItems[1] = 'Ready' then
        begin
          FormMain.SendMsg('FILErcv|' + fUser + '|' + fID + '|' +
            ListViewFiles.Items[i].Caption + '|' +
            ListViewFiles.Items[i].SubItems[3]);
          ListViewFiles.Items[i].SubItems[1] := 'Sending';
          LabelName.Caption := 'File: ' + ListViewFiles.Items[i].Caption;
          LabelSize.Caption := 'Size 0 B / ' + ListViewFiles.Items[i].SubItems[0];
          fUDPTransfer.Send(ListViewFiles.Items[i].SubItems[2]);
          fCurrent := i;
          break;
        end;
      end;
      if (ListViewFiles.Items[fCurrent].SubItems[1] <> 'Sending') then
      begin
        FormMain.SendMsg('FILErej|' + fUser + '|' + fID);
        exit;
      end;
    end else begin
      if (fCurrent < 0) or (fCurrent >= ListViewFiles.Items.Count) then exit;
      if (ListViewFiles.Items[fCurrent].SubItems[1] = 'Receiving') then
        ListViewFiles.Items[fCurrent].SubItems[1] := 'Received';
    end;
    ProgressBarSize.Position := 0;
  end else begin
    if (fCurrent < 0) or (fCurrent >= ListViewFiles.Items.Count) then exit;
    permil := StrToInt(ListViewFiles.Items[fCurrent].SubItems[3]);
    permil := Progress * 1000 div permil;
    ProgressBarSize.Position := permil;

    if Progress < 1024 then
      size := IntToStr(Progress) + ' B'
    else if Progress < (1024 * 1024) then
      size := IntToStr(Progress div 1024) + ' KB'
    else
      size := IntToStr(Progress div (1024 * 1024)) + ' MB';
    LabelSize.Caption := 'Size ' + size + ' / ' + ListViewFiles.Items[fCurrent].SubItems[0];
  end;
end;

end.
