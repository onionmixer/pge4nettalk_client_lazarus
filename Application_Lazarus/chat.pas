unit chat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichView, RVStyle, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus;

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
    JumpSeq: TStrings;
  public
    { public declarations }
    procedure SetUser(from, target: String);
    procedure RecvMsg(aMsg: String);
    procedure RecvFile(aMsg, filename, mime: String; size, expire: DWord);
  end;

//var
//  FormChat: TFormChat;

implementation

{$R *.lfm}

uses main, filelist, ChatLabel, CargoCompany, LCLType, DateUtils;

{ TFormChat }

procedure TFormChat.ButtonSendClick(Sender: TObject);
var
  aMsg: String;
begin
  aMsg := TrimRight(MemoMessage.Text);
  if aMsg <> '' then
  begin
    aMsg := 'MESG' + TargetID + '|' + aMsg;
    FormMain.SendMsg(aMsg);
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
  JumpSeq := TStringList.Create;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
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
begin
  invite := InputBox('Invite', 'Invite user', '');
  FormMain.SendMsg('INVT'+TargetID + '|' + invite);
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
  FromID := from;
  TargetID := target;
  Caption := 'Talk To - ' + FormMain.GetNick(target);
end;

procedure TFormChat.RecvMsg(aMsg: String);
var
  cmd, from, target: String;
  cut: Integer;
  textAlign: TRVAlign;
  Chat: TChatLabel;
begin
  cmd := Copy(aMsg, 3, 4);
  if (cmd = 'MESG') then
  begin
    cut := Pos('|', aMsg);
    if cut = 0 then exit;
    target := Copy(aMsg, 7, cut - 7);
    Delete(aMsg, 1, cut);
    cut := Pos('|', aMsg);
    if cut = 0 then exit;
    from := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);

    if aMsg = '' then
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
    Chat.Caption := aMsg;

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
  else if (cmd = 'INVT') then
  begin
    cut := Pos('|', aMsg);
    if cut = 0 then exit;
    target := Copy(aMsg, 7, cut - 7);
    Delete(aMsg, 1, cut);
    from := aMsg;
    aMsg := 'invite user ' + from;

    RichView1.AddTextFromNewLine(aMsg, 0, rvalCenter);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end;
end;

procedure TFormChat.RecvFile(aMsg, filename, mime: String; size, expire: DWord);
var
  from, target, seq, msg: String;
  cut: Integer;
  textAlign: TRVAlign;
  date: TDateTime;
begin
  cut := Pos('|', aMsg);
  if cut = 0 then exit;
  seq := Copy(aMsg, 1, cut - 1);
  Delete(aMsg, 1, cut);
  cut := Pos('|', aMsg);
  if cut = 0 then exit;
  target := Copy(aMsg, 1, cut - 1);
  from := Copy(aMsg, cut + 1, Length(aMsg));

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

  JumpSeq.Add(seq);
end;

end.
