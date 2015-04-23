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

uses main;

{ TFormLogin }

procedure TFormLogin.ButtonLoginClick(Sender: TObject);
begin
  if FormMain.Connected then
  begin
    FormMain.SignIn(EditID.Text, EditPassword.Text);
    //FormMain.SendMsg('AUTH' + EditID.Text + '|' + EditPassWord.Text);
    ButtonLogin.Enabled := False;
  end
  else
    Application.MessageBox('서버에 접속하지 못했습니다.', '확인', 0);
end;

procedure TFormLogin.FormShow(Sender: TObject);
begin
  ButtonLogin.Enabled := True;
end;

procedure TFormLogin.Fail;
begin
  Application.MessageBox('ID 또는 비밀번호를 확인하세요.', '확인', 0);
  ButtonLogin.Enabled := True;
end;

procedure TFormLogin.Retry;
begin
  if FormMain.Connected then
    FormMain.SendMsg('AUTH' + EditID.Text + '|' + EditPassWord.Text);
end;

end.
