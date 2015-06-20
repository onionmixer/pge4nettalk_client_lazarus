unit login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormLogin }

  TFormLogin = class(TForm)
    ButtonLogin: TButton;
    EditID: TEdit;
    EditPassWord: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ButtonLoginClick(Sender: TObject);
    procedure EditIDKeyPress(Sender: TObject; var Key: char);
    procedure EditPassWordKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure Fail;
    procedure Retry;
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.lfm}

uses main, IniFiles, OZFTalkTo;

{ TFormLogin }

procedure TFormLogin.ButtonLoginClick(Sender: TObject);
var
  Config: TIniFile;
begin
  if FormMain.Connected then
  begin
    Config := TIniFile.Create(FormMain.ConfigFile);
    Config.WriteString('Login', 'ID', EditID.Text);
    Config.Free;

    FormMain.SignIn(EditID.Text, EditPassword.Text);
    ButtonLogin.Enabled := False;
  end
  else
    Application.MessageBox('서버에 접속하지 못했습니다.', '확인', 0);
end;

procedure TFormLogin.EditIDKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    EditPassword.SetFocus;
  end;
end;

procedure TFormLogin.EditPassWordKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    ButtonLoginClick(Sender);
  end;
end;

procedure TFormLogin.FormCreate(Sender: TObject);
var
  Config: TIniFile;
begin
  Config := TIniFile.Create(FormMain.ConfigFile);
  EditID.Text := Config.ReadString('Login', 'ID', '');
  Config.Free;
end;

procedure TFormLogin.FormShow(Sender: TObject);
begin
  ButtonLogin.Enabled := True;
  if EditID.Text <> '' then
    EditPassword.SetFocus;
end;

procedure TFormLogin.Fail;
begin
  Application.MessageBox('ID 또는 비밀번호를 확인하세요.', '확인', 0);
  ButtonLogin.Enabled := True;
end;

procedure TFormLogin.Retry;
begin
  if FormMain.Connected then
    FormMain.SignIn(EditID.Text, EditPassword.Text);
end;

end.
