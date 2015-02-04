unit chat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RichView, RVStyle, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, StdCtrls, Menus;

type

  { TFormChat }

  TFormChat = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuClose: TMenuItem;
    MenuSendFile: TMenuItem;
    Panel1: TPanel;
    RichView1: TRichView;
    RVStyle1: TRVStyle;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure Memo1KeyPress(Sender: TObject; var Key: char);
    procedure MenuCloseClick(Sender: TObject);
    procedure MenuSendFileClick(Sender: TObject);
    procedure RichView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
    FromID: String;
    TargetID: String;
    fFileList: TStringList;
  public
    { public declarations }
    procedure SetUser(from, target: String);
    procedure RecvMsg(aMsg: String);
    function GetDropFile: String;
  end;

//var
//  FormChat: TFormChat;

implementation

{$R *.lfm}

uses main, ChatLabel;

{ TFormChat }

procedure TFormChat.Button1Click(Sender: TObject);
var
  aMsg: String;
begin
  aMsg := TrimRight(Memo1.Text);
  if aMsg <> '' then
  begin
    aMsg := 'MESG' + TargetID + '|' + aMsg;
    FormMain.SendMsg(aMsg);
    Memo1.Clear;
  end;
  Memo1.SetFocus;
end;

procedure TFormChat.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  fFileList := nil;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  FormMain.ChatClose(TargetID);
  if fFileList <> nil then
    fFileList.Free;
end;

procedure TFormChat.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
begin
  if fFileList = nil then exit;
  if High(FileNames) < 0 then exit;
  fFileList.Clear;
  for i := 0 to High(FileNames) do
    fFileList.Add(FileNames[i]);
  if not FormMain.SendDropMsg(TargetID) then
    MenuSendFileClick(Sender);
end;

procedure TFormChat.FormShow(Sender: TObject);
begin
  Memo1.SetFocus;
end;

procedure TFormChat.Memo1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    Button1Click(Sender);
  end;
end;

procedure TFormChat.MenuCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormChat.MenuSendFileClick(Sender: TObject);
begin
  FormMain.SendMsg('FILEsnd|' + TargetID);
end;

procedure TFormChat.RichView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Key = Ord('C')) then
  begin
    RichView1.CopyText;
  end;
end;

procedure TFormChat.SetUser(from, target: String);
begin
  FromID := from;
  TargetID := target;
  Caption := 'Talk To - ' + target;
  if (TargetID[1] <> '$') and (TargetID[1] <> '#') then
  begin
    fFileList := TStringList.Create;
    AllowDropFiles := True;
  end;
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
      textAlign := rvalRIght;
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

    RichView1.AddTextFromNewLine(from, 0, textAlign);
    RichView1.AddControl(Chat, textAlign);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end
  else
  begin
    Delete(aMsg, 1, 6);
    RichView1.AddTextFromNewLine(aMsg, rvsJump1);

    RichView1.FormatTail;
    RichView1.Invalidate;
    if not Showing then Show;
  end;
end;

function TFormChat.GetDropFile: String;
begin
  if fFileList.Count > 0 then
  begin
    Result := fFileList[0];
    fFileList.Delete(0);
  end else
    Result := '';
end;

end.

