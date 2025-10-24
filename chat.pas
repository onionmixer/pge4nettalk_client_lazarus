unit chat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichView, RVStyle, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus, RegExpr, SyncObjs, OZFBlahBlah;

type

  { TFormChat }

  TFormChat = class(TForm)
    ButtonSend: TButton;
    Image1: TImage;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MemoMessage: TMemo;
    MenuItem1: TMenuItem;
    MenuClose: TMenuItem;
    MenuInvite: TMenuItem;
    MenuShare: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    procedure ButtonSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MemoMessageKeyPress(Sender: TObject; var Key: char);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuInviteClick(Sender: TObject);
    procedure MenuShareClick(Sender: TObject);
    procedure RichView1Click(Sender: TObject);
    procedure RichView1Jump(Sender: TObject; id: Integer);
    procedure RichView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    fFromID: String;
    fTargetID: String;
    fUsers: TStringList;
    fJumpSeq: TStringList;
    fRichView: TRichView;
    fRVStyle: TRVStyle;
    fPopupRegExpr: TRegExpr;
    fPopupList: TStringList;
    fPopupLock: SyncObjs.TCriticalSection;

    procedure showPopup(filename: String);
  public
    { public declarations }
    procedure SetUser(from, target: String);
    procedure RecvMsg(tt: TOZFBlahBlah);
    procedure RecvFile(from, filename, mime: String; seq, size, expire: DWord);
    procedure Notice(message: String);

    property Users: TStringList read fUsers;
  end;

//var
//  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  main, invite, filelist,
  LazFileUtils, LCLType, DateUtils,
  ChatLabel, logger;

{ TFormChat }

procedure TFormChat.ButtonSendClick(Sender: TObject);
var
  message: String;
  tt: TOZFBlahBlah;
begin
  message := TrimRight(MemoMessage.Text);
  if message <> '' then
  begin
    tt := TOZFBlahBlah.Create;
    tt.functionID := OZFBlahBlahFunctionIDMessage;
    tt.args := TStringList.Create;
    tt.args.add(fTargetID);
    tt.args.add(message);

    FormMain.SendData(tt, OZFBlahBlahID);
    FreeAndNil(tt);

    MemoMessage.Clear;
  end;
  MemoMessage.SetFocus;
end;

procedure TFormChat.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  fRVStyle := TRVStyle.Create(Self);
  fRichView := TRichView.Create(Self);
  fRichView.Style := fRVStyle;

  fRichView.Left := 0;
  fRichView.Top := 0;
  fRichView.Align := alClient;

  fRichView.OnClick := @RichView1Click;
  fRichView.OnDragDrop := @FormDragDrop;
  fRichView.OnDragOver := @FormDragOver;
  fRichView.OnJump := @RichView1Jump;
  fRichView.OnKeyDown := @RichView1KeyDown;

  fRichView.Parent := Self;

  fFromID := '';
  fTargetID := '';
  fJumpSeq := TStringList.Create;
  fUsers := nil;
  fPopupRegExpr := TRegExpr.Create('(apple|banana)');
  fPopupList := TStringList.Create;
  fPopupLock := SyncObjs.TCriticalSection.Create;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  if Assigned(fJumpSeq) then
    FreeAndNil(fJumpSeq);
  if Assigned(fUsers) then
    FreeAndNil(fUsers);
  if Assigned(fPopupRegExpr) then
    FreeAndNil(fPopupRegExpr);
  if Assigned(fPopupList) then
    FreeAndNil(fPopupList);
  if Assigned(fPopupLock) then
    FreeAndNil(fPopupLock);
  FormMain.ChatClose(fTargetID);
end;

procedure TFormChat.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  TreeView: TTreeView;
  User: String;
  tt: TOZFBlahBlah;
begin
  if not (Source is TTreeView) then exit;
  TreeView := Source as TTreeView;
  if TreeView.Selected = nil then exit;
  User := IntToStr(PtrInt(TreeView.Selected.Data));

  if not Assigned(fUsers) then
  begin
    fUsers := TStringList.Create;
    fUsers.Sorted := True;
    fUsers.Duplicates := dupIgnore;
    if fFromID <> '' then
      fUsers.Add(fFromID);
    if fTargetID <> '' then
      fUsers.Add(fTargetID);
  end;

  if fUsers.IndexOf(User) >= 0 then exit;

  tt := TOZFBlahBlah.Create;
  tt.functionID := OZFBlahBlahFunctionIDGroupInvite;
  tt.args := TStringList.Create;
  tt.args.add(fTargetID);
  tt.args.add(User);

  FormMain.SendData(tt, OZFBlahBlahID);
  FreeAndNil(tt);
end;

procedure TFormChat.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TTreeView);
end;

procedure TFormChat.FormDropFiles(Sender: TObject;
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
      FormMain.AddUpload(fTargetID, FileList[i]);
  end;

  FileList.Free;
end;

procedure TFormChat.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl, ssShift] then
  begin
    if Key = VK_F then
      MenuShareClick(Sender);
    if Key = VK_I then
      MenuInviteClick(Sender);
  end;
end;

procedure TFormChat.FormShow(Sender: TObject);
begin
  MemoMessage.SetFocus;
end;

procedure TFormChat.MemoMessageKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ButtonSendClick(Sender);
  end;
end;

procedure TFormChat.MenuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormChat.MenuInviteClick(Sender: TObject);
begin
  if not Assigned(fUsers) then
  begin
    fUsers := TStringList.Create;
    fUsers.Sorted := True;
    fUsers.Duplicates := dupIgnore;
    if fFromID <> '' then
      fUsers.Add(fFromID);
    if fTargetID <> '' then
      fUsers.Add(fTargetID);
  end;

  with TFormInvite.Create(self) do
  begin
    TargetID := fTargetID;
    Setup(fUsers);
    Show;
  end;
end;

procedure TFormChat.MenuShareClick(Sender: TObject);
begin
  FormFileList.Share := fTargetID;
  FormFileList.Show;
end;

procedure TFormChat.RichView1Click(Sender: TObject);
begin

end;

procedure TFormChat.RichView1Jump(Sender: TObject; id: Integer);
begin
  FormMain.FileDownload(fTargetID, StrToInt(fJumpSeq[id]));
end;

procedure TFormChat.RichView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
  begin
    fRichView.CopyText;
  end else
    FormKeyDown(Sender, Key, Shift);
end;

procedure TFormChat.Timer1Timer(Sender: TObject);
var
  filename: String;
begin
  if Panel2.Visible then
  begin
    Panel2.Visible := False;
    Timer1.Interval := 100;
    exit;
  end;

  Timer1.Enabled := False;

  fPopupLock.Enter;
  if fPopupList.Count = 0 then
  begin
    fPopupLock.Leave;
    exit;
  end;
  filename := fPopupList[0];
  fPopupList.Delete(0);
  fPopupLock.Leave;

  Image1.Picture.LoadFromFile(filename);
  Panel2.Visible := True;
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

procedure TFormChat.showPopup(filename: String);
begin
  if not FileExists(filename) then
    exit;

  if Timer1.Enabled then
  begin
    fPopupLock.Enter;
    fPopupList.Add(filename);
    fPopupLock.Leave;
  end
  else
  begin
    Image1.Picture.LoadFromFile(filename);
    Panel2.Visible := True;
    Timer1.Interval := 1000;
    Timer1.Enabled := True;
  end;
end;

procedure TFormChat.SetUser(from, target: String);
begin
  if (target[1] = '#') and not Assigned(fUsers) then
  begin
    fUsers := TStringList.Create;
    fUsers.Sorted := True;
    fUsers.Duplicates := dupIgnore;
    if fFromID <> '' then
      fUsers.Add(fFromID);
    if fTargetID <> '' then
      fUsers.Add(fTargetID);
  end;
  fFromID := from;
  fTargetID := target;
  Caption := 'BlahBlah - ' + FormMain.GetNick(target);
end;

procedure TFormChat.RecvMsg(tt: TOZFBlahBlah);
var
  from, target, message: String;
  i: Integer;
  textAlign: TRVAlign;
  Chat: TChatLabel;
  match: String;
begin
  if tt.functionID = OZFBlahBlahFunctionIDMessage then
  begin
    target := tt.args[0];
    from := tt.args[1];
    message := tt.args[2];

    {$IFDEF DEBUG}
    LogDebug('MESSAGE received: target="%s", from="%s", message="%s"', [target, from, message]);
    {$ENDIF}

    if message = '' then
      exit;

    Chat := TChatLabel.Create(fRichView);

    if (from = fFromID) then
    begin
      textAlign := rvalRight;
      Chat.Skin := FormMain.RightSkin;
      Chat.LeftSide := False;
    end
    else
    begin
      textAlign := rvalLeft;
      Chat.Skin := FormMain.LeftSkin;
      Chat.LeftSide := True;
    end;
    Chat.SelectSkin := FormMain.SelectSkin;
    Chat.SelectColor := $7ED3FF;
    Chat.Caption := message;

    //fRichView.AddTextFromNewLine(from + ': ', rvsSubHeading, textAlign);
    //fRichView.AddHotSpot(0, ImageList1, False);
    //fRichView.AddHotSpot(1, ImageList1, False);
    //fRichView.AddTextFromNewLine(aMsg, rvsJump1, textAlign);

    from := FormMain.GetNick(from);
    fRichView.AddTextFromNewLine(from, 0, textAlign);
    fRichView.AddControl(Chat, textAlign);

    fRichView.FormatTail;
    fRichView.Invalidate;
    if not Showing then Show;

    if fPopupRegExpr.Exec(message) then
    begin
      match := fPopupRegExpr.Match[0];
      if match = 'apple' then showPopup('sample_image_apple.jpg');
      if match = 'banana' then showPopup('sample_image_banana.jpg');
    end;
  end
  else if tt.functionID = OZFBlahBlahFunctionIDGroupInvite then
  begin
    target := tt.args[0];
    from := tt.args[1];
    if tt.args.Count > 2 then
    begin
      for i := 2 to tt.args.Count - 1 do
        fUsers.Add(tt.args[i]);
    end
    else
    begin
      fUsers.Add(from);
    end;
    from := FormMain.GetNick(from);
    message := 'invite user ' + from;

    fRichView.AddTextFromNewLine(message, 0, rvalCenter);

    fRichView.FormatTail;
    fRichView.Invalidate;
    if not Showing then Show;
  end
  else if tt.functionID = OZFBlahBlahFunctionIDGroupExit then
  begin
    target := tt.args[0];
    from := tt.args[1];
    i := fUsers.IndexOf(from);
    if i >= 0 then
      fUsers.Delete(i);
    message := 'exit user ' + from;

    fRichView.AddTextFromNewLine(message, 0, rvalCenter);

    fRichView.FormatTail;
    fRichView.Invalidate;
    if not Showing then Show;
  end;
end;

procedure TFormChat.RecvFile(from, filename, mime: String; seq, size, expire: DWord);
var
  msg: String;
  textAlign: TRVAlign;
  date: TDateTime;
begin
  if (from = fFromID) then
  begin
    textAlign := rvalRight;
  end
  else
  begin
    textAlign := rvalLeft;
  end;

  date := UnixToDateTime(expire);
  date := UniversalTimeToLocal(date);
  msg := FormatDateTime('yy-mm-dd hh:nn', date);

  from := FormMain.GetNick(from);
  fRichView.AddTextFromNewLine(from, 0, textAlign);
  fRichView.AddTextFromNewLine('Download: ' + filename, rvsJump1, textAlign);
  fRichView.AddTextFromNewLine('Size: ' + IntToStr(size), 0, textAlign);
  fRichView.AddTextFromNewLine('Expire: ' + msg, 0, textAlign);

  fRichView.FormatTail;
  fRichView.Invalidate;
  if not Showing then Show;

  fJumpSeq.Add(IntToStr(seq));
end;

procedure TFormChat.Notice(message: String);
begin
  fRichView.AddTextFromNewLine(message, 0, rvalCenter);
  fRichView.FormatTail;
  fRichView.Invalidate;
  if not Showing then Show;
end;

end.

