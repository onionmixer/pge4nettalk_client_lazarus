unit invite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormInvite }

  TFormInvite = class(TForm)
    ButtonCancel: TButton;
    ButtonInvite: TButton;
    ListBoxUsers: TListBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonInviteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    fTargetID: String;
    procedure SetFromID(AValue: String);
  public
    { public declarations }
    procedure Setup(blacklist: TStrings);
    property TargetID: String read fTargetID write SetFromID;
  end;

//var
//  FormInvite: TFormInvite;

implementation

{$R *.lfm}

uses main, LCLType, flcDataStructs, OZFBlahBlah;

{ TFormInvite }

procedure TFormInvite.ButtonCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormInvite.ButtonInviteClick(Sender: TObject);
var
  i: Integer;
  user: PtrInt;
  tt: TOZFBlahBlah;
begin
  if ListBoxUsers.SelCount = 0 then
  begin
    Application.MessageBox('Please select user for invite', 'Confirm', MB_ICONWARNING);
    exit;
  end;

  tt := TOZFBlahBlah.Create;
  tt.functionID := OZFBlahBlahFunctionIDGroupInvite;
  tt.args := TStringList.Create;
  tt.args.add(fTargetID);
  for i := 0 to ListBoxUsers.Items.Count - 1 do
  begin
    if ListBoxUsers.Selected[i] then
    begin
      user := PtrInt(ListBoxUsers.Items.Objects[i]);
      tt.args.add(IntToStr(user));
    end;
  end;

  FormMain.SendData(tt, OZFBlahBlahID);
  FreeAndNil(tt);

  Close;
end;

procedure TFormInvite.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormInvite.SetFromID(AValue: String);
begin
  if fTargetID = AValue then Exit;
  fTargetID := AValue;
  AValue := FormMain.GetNick(AValue);
  Caption := 'invite user to ' + AValue;
end;

procedure TFormInvite.Setup(blacklist: TStrings);
var
  users, nicks: AStringArray;
  i: Integer;
  user: PtrInt;
begin
  ListBoxUsers.Items.BeginUpdate;
  users := FormMain.NickList.Keys;
  nicks := FormMain.NickList.Values;
  ListBoxUsers.Items.Clear;
  for i := 0 to users.Count - 1 do
  begin
    if blacklist.IndexOf(users[i]) > -1 then
      continue;
    user := StrToInt(users[i]);
    ListBoxUsers.Items.AddObject(nicks[i], TObject(user));
  end;
  ListBoxUsers.Items.EndUpdate;
end;

end.

