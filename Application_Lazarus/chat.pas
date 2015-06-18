unit chat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichView, RVStyle, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus, OZFTalkTo;

type

  { TFormChat }

  TFormChat = class(TForm)
    ButtonSend: TButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MemoMessage: TMemo;
    MenuItem1: TMenuItem;
    MenuClose: TMenuItem;
    MenuInvite: TMenuItem;
    MenuShare: TMenuItem;
    Panel1: TPanel;
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    Splitter1: TSplitter;
    procedure ButtonSendClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure MemoMessageKeyPress(Sender: TObject; var Key: char);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuInviteClick(Sender: TObject);
    procedure MenuShareClick(Sender: TObject);
    procedure RichView1Jump(Sender: TObject; id: Integer);
    procedure RichView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    FromID: String;
    TargetID: String;
    fUsers: TStrings;
    JumpSeq: TStrings;
  public
    { public declarations }
    procedure SetUser(from, target: String);
    procedure RecvMsg(tt: TTalkTo);
    procedure RecvFile(from, filename, mime: String; seq, size, expire: DWord);

    property Users: TStrings read fUsers;
  end;

//var
//  FormChat: TFormChat;

implementation

{$R *.lfm}

uses main, filelist, ChatLabel, CargoCompany, LCLType, DateUtils;

{ TFormChat }

procedure TFormChat.ButtonSendClick(Sender: TObject);
var
  message: String;
  tt: TTalkTo;
  size: DWord;
  data: Pointer;
begin
  message := TrimRight(MemoMessage.Text);
  if message <> '' then
  begin
    tt := TTalkTo.Create;
    tt.functionID := TalkToFunctionIDMessage;
    tt.args := TStringList.Create;
    tt.args.add(TargetID);
    tt.args.add(message);

    data := tt.getData(size);
    FreeAndNil(tt);

    FormMain.SendData(data, size, TalkToID);

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
  FromID := '';
  TargetID := '';
  JumpSeq := TStringList.Create;
  fUsers := nil;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  if Assigned(JumpSeq) then
    FreeAndNil(JumpSeq);
  if Assigned(fUsers) then
    FreeAndNil(fUsers);
  FormMain.ChatClose(TargetID);
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
var
  invite: String;
  tt: TTalkTo;
  size: DWord;
  data: Pointer;
begin
  invite := InputBox('Invite', 'Invite user', '');

  tt := TTalkTo.Create;
  tt.functionID := TalkToFunctionIDGroupInvite;
  tt.args := TStringList.Create;
  tt.args.add(TargetID);
  tt.args.add(invite);

  data := tt.getData(size);
  FreeAndNil(tt);

  FormMain.SendData(data, size, TalkToID);
end;

procedure TFormChat.MenuShareClick(Sender: TObject);
begin
  FormFileList.Share := TargetID;
  FormFileList.Show;
end;

procedure TFormChat.RichView1Jump(Sender: TObject; id: Integer);
var
  cargo: TCargoCompany;
  data: Pointer;
  size: DWord;
begin
  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeDownload;
  cargo.Seq := StrToInt(JumpSeq[id]);

  data := cargo.getData(size);
  FormMain.SendData(data, size, $5454);
  cargo.Free;
end;

procedure TFormChat.RichView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = VK_C) then
  begin
    RichView1.CopyText;
  end else
    FormKeyDown(Sender, Key, Shift);
end;

procedure TFormChat.SetUser(from, target: String);
begin
  if (target[1] = '#') and not Assigned(fUsers) then
  begin
    fUsers := TStringList.Create;
    if FromID <> '' then
      fUsers.Add(FromID);
    if TargetID <> '' then
      fUsers.Add(TargetID);
  end;
  FromID := from;
  TargetID := target;
  Caption := 'Talk To - ' + FormMain.GetNick(target);
end;

procedure TFormChat.RecvMsg(tt: TTalkTo);
var
  from, target, message: String;
  i: Integer;
  textAlign: TRVAlign;
  Chat: TChatLabel;
begin
  if tt.functionID = TalkToFunctionIDMessage then
  begin
    target := tt.args[0];
    from := tt.args[1];
    message := tt.args[2];

    if message = '' then
      exit;

    Chat := TChatLabel.Create(RichView1);

    if (from = FromID) then
    begin
      textAlign := rvalLeft;
      Chat.Skin := FormMain.LeftSkin;
      Chat.LeftSide := True;
    end
    else
    begin
      textAlign := rvalRight;
      Chat.Skin := FormMain.RightSkin;
      Chat.LeftSide := False;
    end;
    Chat.SelectSkin := FormMain.SelectSkin;
    Chat.SelectColor := $7ED3FF;
    Chat.Caption := message;

    //RichView1.AddTextFromNewLine(from + ': ', rvsSubHeading, textAlign);
    //RichView1.AddHotSpot(0, ImageList1, False);
    //RichView1.AddHotSpot(1, ImageList1, False);
    //RichView1.AddTextFromNewLine(aMsg, rvsJump1, textAlign);

    from := FormMain.GetNick(from);
    RichView1.AddTextFromNewLine(from, 0, textAlign);
    RichView1.AddControl(Chat, textAlign);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end
  else if tt.functionID = TalkToFunctionIDGroupInvite then
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

    RichView1.AddTextFromNewLine(message, 0, rvalCenter);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end
  else if tt.functionID = TalkToFunctionIDGroupExit then
  begin
    target := tt.args[0];
    from := tt.args[1];
    i := fUsers.IndexOf(from);
    if i >= 0 then
      fUsers.Delete(i);
    message := 'exit user ' + from;

    RichView1.AddTextFromNewLine(message, 0, rvalCenter);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end;
end;

procedure TFormChat.RecvFile(from, filename, mime: String; seq, size, expire: DWord);
var
  msg: String;
  textAlign: TRVAlign;
  date: TDateTime;
begin
  if (from = FromID) then
  begin
    textAlign := rvalLeft;
  end
  else
  begin
    textAlign := rvalRight;
  end;

  date := UnixToDateTime(expire);
  date := UniversalTimeToLocal(date);
  ShortDateFormat := 'yy-mm-dd';
  LongTimeFormat := 'hh:nn';
  msg := DateTimeToStr(date);

  from := FormMain.GetNick(from);
  RichView1.AddTextFromNewLine(from, 0, textAlign);
  RichView1.AddTextFromNewLine('Download: ' + filename, rvsJump1, textAlign);
  RichView1.AddTextFromNewLine('Size: ' + IntToStr(size), 0, textAlign);
  RichView1.AddTextFromNewLine('Expire: ' + msg, 0, textAlign);

  RichView1.FormatTail;
  RichView1.Invalidate;
  if not Showing then Show;

  JumpSeq.Add(IntToStr(seq));
end;

end.
