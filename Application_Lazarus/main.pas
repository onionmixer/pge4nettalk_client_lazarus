unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, ExtCtrls, Contnrs, syncobjs, LMessages, ChatLabel,
  NetworkInterface, chat, transfer;

type

  { TFormMain }

  TFormMain = class(TForm)
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuExit: TMenuItem;
    MenuConnect: TMenuItem;
    MenuItemFileList: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuItemFileListClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
  private
    { private declarations }
    fCubeConn: TNetworkInterface;
    fAuth: Boolean;
    fUser: String;
    fChatForm: TObjectList;
    fChatUser: TStringList;
    fSendForm: TObjectList;
    fSendUser: TStringList;
    fRecvForm: TObjectList;
    fRecvUser: TStringList;
    fFormSync: TCriticalSection;

    fLeftSkin, fRightSkin, fSelectSkin: TChatSkin;

    procedure OnReceive(var Msg: TLMessage); message NI_RECEIVE;
    procedure OnTalkTo(aMsg: String);
    procedure OnSession(aMsg: String);
    procedure OnCargoCompany(aMsg: String);
    procedure OnStatus(var Msg: TLMessage); message NI_STATUS;
    procedure UpdateUserInfo(User, Nick, Group, Status, Image: String);
  public
    { public declarations }
    procedure SignIn(User: String; Password: String);
    procedure SendMsg(aMsg: string);
    procedure SendData(data: Pointer; size, toAddress: DWord);
    function Connected: Boolean;
    function ChatForm(User: String; Add: Boolean = True): TFormChat;
    procedure ChatClose(User: String);
    procedure Join(Room: String; User:String);
    function SendForm(User: String; ID: String): TFormTransfer;
    procedure SendClose(User: String; ID: String);
    function SendDropMsg(User: String): Boolean;
    function RecvForm(User: String; ID: String): TFormTransfer;
    procedure RecvClose(User: String; ID: String);
    function GetNick(User: String): String;
    procedure OnDebug(Sender: TObject; aMsg: String);

    property LeftSkin: TChatSkin read fLeftSkin;
    property RightSkin: TChatSkin read fRightSkin;
    property SelectSkin: TChatSkin read fSelectSkin;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  login, filelist, DateUtils, LazUTF8Classes,
  TwistedKnot, Session, CargoCompany;

{ TFormMain }

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.MenuConnectClick(Sender: TObject);
begin
  if fAuth then
  begin
    SendMsg('QUIT');
  end
  else
  begin
    fCubeConn.Connect;
    FormLogin.Show;
  end;
end;

procedure TFormMain.MenuItemFileListClick(Sender: TObject);
begin
  FormFileList.Share := '';
  FormFileList.Show;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Skin: TBitmap;
begin
  fFormSync := syncobjs.TCriticalSection.Create;
  fAuth := False;
  fCubeConn := TNetworkInterface.Create;
  fChatForm := TObjectList.Create(False);
  fChatUser := TStringList.Create;
  fSendForm := TObjectList.Create;
  fSendUser := TStringList.Create;
  fRecvForm := TObjectList.Create;
  fRecvUser := TStringList.Create;
  MenuConnect.Caption := '&Connect...';

  Skin := TBitmap.Create;

  ImageList1.GetBitmap(0, Skin);
  fLeftSkin := TChatSkin.Create;
  fLeftSkin.LoadFromBitmap(Skin);

  ImageList1.GetBitmap(1, Skin);
  fRightSkin := TChatSkin.Create;
  fRightSkin.LoadFromBitmap(Skin);

  ImageList1.GetBitmap(2, Skin);
  fSelectSkin := TChatSkin.Create;
  fSelectSkin.LoadFromBitmap(Skin);

  Skin.Free;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: Integer;
  Chat: TFormChat;
begin
  SendMsg('QUIT');
  fCubeConn.Free;

  for i := fChatForm.Count - 1 downto 0 do
  begin
    Chat := fChatForm.Items[i] as TFormChat;
    Chat.Free;
  end;
  fChatForm.Free;
  fChatUser.Free;

  fSendForm.Free;
  fSendUser.Free;

  fRecvForm.Free;
  fRecvUser.Free;

  fFormSync.Free;
end;

procedure TFormMain.TreeView1DblClick(Sender: TObject);
var
  user: String;
begin
  if TreeView1.Selected = nil then exit;
  User := IntToStr(Integer(TreeView1.Selected.Data));
  if User = fUser then exit;
  ChatForm(User).Show;
end;

procedure TFormMain.OnReceive(var Msg: TLMessage);
var
  aMsg: String;
  service: Word;
begin
  while true do
  begin
    aMsg := fCubeConn.getMessage;
    if (aMsg = '') then
      break;

    service := (Ord(aMsg[1]) shl 8) or Ord(aMsg[2]);
    if service = SessionID then
      OnSession(aMsg)
    else if service = $5454 then
      OnTalkTo(aMsg)
    else if service = CargoCompanyID then
      OnCargoCompany(aMsg);
  end;
end;

procedure TFormMain.OnTalkTo(aMsg: String);
var
  cmd, sub: String;
  cut: Integer;
  user, room, nick, group, status, image, id: String;
  form: TFormChat;
begin
  cmd := Copy(aMsg, 3, 4);
  if (cmd = 'QUIT') then
  begin
    if fAuth then
    begin
      fAuth := False;
      Caption := 'Talk To';
      MenuConnect.Caption := '&Connect...';
      TreeView1.Items.Clear;
    end
    else
    begin
      FormLogin.Show;
      FormLogin.Fail;
    end;
  end
  else if (cmd = 'AUTH') then
  begin
    fAuth := True;
    fUser := Copy(aMsg, 7, 32);
    MenuConnect.Caption := 'Dis&connect';
    FormLogin.Hide;
    FormFileList.ButtonRefreshClick(nil);
  end
  else if (cmd = 'STAT') then
  begin
    cut := Pos('|', aMsg);
    user := Copy(aMsg, 7, cut - 7);
    Delete(aMsg, 1, cut);
    cut := Pos('|', aMsg);
    nick := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);
    cut := Pos('|', aMsg);
    group := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);
    cut := Pos('|', aMsg);
    status := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);
    image := aMsg;
    updateUserInfo(user, nick, group, status, image);
    if user = fUser then
      Caption := 'Talk To - ' + nick;
  end
  else if (cmd = 'MESG') then
  begin
    cut := Pos('|', aMsg);
    user := Copy(aMsg, 7, cut - 7);
    if user = fUser then
    begin
      user := Copy(aMsg, cut + 1, 32);
      cut := Pos('|', user);
      user := Copy(user, 1, cut - 1);
    end;
    ChatForm(user).RecvMsg(aMsg);
  end
  else if (cmd = 'JOIN') then
  begin
    cut := Pos('|', aMsg);
    room := Copy(aMsg, 7, cut - 7);
    Delete(aMsg, 1, cut);
    user := aMsg;
    Join(room, user);
  end
  else if (cmd = 'INVT') then
  begin
    cut := Pos('|', aMsg);
    room := Copy(aMsg, 7, cut - 7);
    user := Copy(aMsg, cut + 1, 32);
    if user = fUser then
      ChatForm(room).RecvMsg(aMsg)
    else
    begin
      form := ChatForm(room, False);
      if form <> nil then
        form.RecvMsg(aMsg);
    end;
  end
  else if (cmd = 'FILE') then
  begin
    cut := Pos('|', aMsg);
    user := Copy(aMsg, 7, cut - 7);
    ChatForm(user).RecvMsg(aMsg);
  end
  else if (cmd = 'FILE') then
  begin
    OnDebug(Self, aMsg);
    sub := Copy(aMsg, 7, 3);
    Delete(aMsg, 1, 10);
    cut := Pos('|', aMsg);
    if cut = 0 then cut := Length(aMsg) + 1;
    user := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);
    cut := Pos('|', aMsg);
    if cut = 0 then cut := Length(aMsg) + 1;
    id := Copy(aMsg, 1, cut - 1);
    Delete(aMsg, 1, cut);

    if sub = 'snd' then
    begin
      SendForm(user, id);
    end
    else if sub = 'req' then
    begin
      RecvForm(user, id);
    end
    else if sub = 'rej' then
    begin
      SendClose(user, id);
    end
    else if sub = 'abt' then
    begin
      RecvClose(user, id);
    end
    else if sub = 'acs' then
    begin
      SendForm(user, id).Connect(aMsg);
    end
    else if sub = 'acr' then
    begin
      RecvForm(user, id).Connect(aMsg);
    end
    else if sub = 'rcv' then
    begin
      RecvForm(user, id).Recv(aMsg);
    end;
  end
  else
  begin
    //FormChat.RecvMsg(aMsg);
  end;
end;

procedure TFormMain.OnSession(aMsg: String);
var
  response: TSession;
begin
  response := TSession.Create(PChar(aMsg), Length(aMsg));
  if response.functionID = SessionFunctionIDSignIn then
  begin
    if response.errorCode = SessionErrorCodeNone then
      SendMsg('AUTH')
    else
    begin
      FormLogin.Show;
      FormLogin.Fail;
    end;
  end;
end;

procedure TFormMain.OnCargoCompany(aMsg: String);
var
  cargo: TCargoCompany;
  fsOut: TFileStreamUTF8;
  list: TListView;
  item: TListItem;
  i: Integer;
  size: DWord;
  str, seq, filename, sizestr, expire: String;
  date: TDateTime;
begin
  cargo := TCargoCompany.Create(PChar(aMsg), Length(aMsg));

  if cargo.ErrorCode <> 0 then
  begin
    if cargo.ErrorCode = 1 then
      str := 'invalid value'
    else
      str := 'server error';
    Application.MessageBox(PChar(str), 'Cargo Company Error', 0);
  end
  else if cargo.Command = CargoCompanyTypeUpload then
  begin
    SaveDialog1.FileName := cargo.Name;
    if SaveDialog1.Execute then
    begin
      try
        fsOut := TFileStreamUTF8.Create(SaveDialog1.FileName, fmCreate);
        size := fsOut.Write(cargo.Content^, cargo.ContentSize);
        fsOut.Free;

        if cargo.ContentSize <> size then
        begin
          Application.MessageBox('Not enough space', 'Confirm', 0);
        end;
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'File I/O Error', 0);
      end;
    end;
  end
  else if cargo.Command = CargoCompanyTypeResult then
  begin
    list := FormFileList.ListView1;
    list.BeginUpdate;

    seq := IntToStr(cargo.Seq);
    filename := cargo.Name;
    sizestr := IntToStr(cargo.Size);
    date := UnixToDateTime(cargo.Expire);
    date := UniversalTimeToLocal(date);
    ShortDateFormat := 'yy-mm-dd';
    LongTimeFormat := 'hh:nn';
    expire := DateTimeToStr(date);

    item := list.Items.FindCaption(0, filename, False, True, False);
    if item = nil then
    begin
      item := list.Items.Add;
      item.Caption := filename;
      item.SubItems.Add(sizestr);
      item.SubItems.Add(expire);
      item.SubItems.Add(seq);
    end
    else
    begin
      item.SubItems[0] := sizestr;
      item.SubItems[1] := expire;
      item.SubItems[2] := seq;
    end;

    list.AlphaSort;
    list.EndUpdate;
  end
  else if cargo.Command = CargoCompanyTypeRemove then
  begin
    seq := IntToStr(cargo.Seq);
    for i := list.Items.Count - 1 downto 0 do
    begin
      if seq = list.Items[i].SubItems[2] then
      begin
        list.Items[i].Delete;
        break;
      end;
    end;
  end
  else if cargo.Command = CargoCompanyTypeShare then
  begin
    SendMsg('FILE' + IntToStr(cargo.User) + '|' + IntToStr(cargo.Seq));
  end
  else if cargo.Command = CargoCompanyTypeList then
  begin
    list := FormFileList.ListView1;
    list.BeginUpdate;

    for i := 0 to (cargo.Files.count div 4) - 1 do
    begin
      seq := cargo.Files[i * 4];
      filename := cargo.Files[i * 4 + 1];
      sizestr := cargo.Files[i * 4 + 2];
      expire := cargo.Files[i * 4 + 3];
      date := UnixToDateTime(StrToInt(expire));
      date := UniversalTimeToLocal(date);
      ShortDateFormat := 'yy-mm-dd';
      LongTimeFormat := 'hh:nn';
      expire := DateTimeToStr(date);

      item := list.Items.FindCaption(0, filename, False, True, False);
      if item = nil then
      begin
        item := list.Items.Add;
        item.Caption := filename;
        item.SubItems.Add(sizestr);
        item.SubItems.Add(expire);
        item.SubItems.Add(seq);
      end
      else
      begin
        item.SubItems[0] := sizestr;
        item.SubItems[1] := expire;
        item.SubItems[2] := seq;
      end;
    end;

    for i := list.Items.Count - 1 downto 0 do
    begin
      filename := list.Items[i].Caption;
      if cargo.Files.IndexOf(filename) = -1 then
        list.Items[i].Delete;
    end;

    list.AlphaSort;
    list.EndUpdate;
  end;

  cargo.Free;
end;

procedure TFormMain.OnStatus(var Msg: TLMessage);
var
  status: TTwistedKnotStatus;
begin
  status := TTwistedKnotStatus(Msg.WParam);

  if status = TwistedKnotStatusDisconnect then
    StatusBar1.Panels[0].Text := 'Disconnected'
  else
    StatusBar1.Panels[0].Text := 'Connected';

  if status = TwistedKnotStatusConnect then
  begin
    if fAuth then
       FormLogin.Retry;
  end;
end;

procedure TFormMain.UpdateUserInfo(User, Nick, Group, Status, Image: String);
var
  node: TTreeNode;
  userNode: TTreeNode;
  newGroup: Boolean;
  userSeq: Integer;
begin
  if (User = '') or (Nick = '') or (Group = '') then
    exit;

  newGroup := False;
  node := TreeView1.Items.FindTopLvlNode(group);
  if node = nil then
  begin
    if (Status = 'offline') then
      exit;
    newGroup := True;
    node := TreeView1.Items.Add(nil, group);
  end;

  userSeq := StrToInt(user);
  userNode := node.GetFirstChild;
  while (userNode <> nil) and (userNode.Data <> Pointer(userSeq)) do
    userNode := userNode.GetNextSibling;

  if (userNode = nil) and (Status <> 'offline') then
  begin
    userNode := TreeView1.Items.AddChild(node, nick);
    userNode.Data := Pointer(userSeq);
  end
  else if (userNode <> nil) And (Status = 'offline') then
  begin
    userNode.Delete;
    userNode := nil;
  end;

  if newGroup then
    node.Expand(True);
  TreeView1.AlphaSort;
end;

procedure TFormMain.SignIn(User: String; Password: String);
var
  request: TSession;
  data: Pointer;
  size: DWord;
begin
  request := TSession.Create;
  request.functionID := SessionFunctionIDSignIn;
  request.errorCode := SessionErrorCodeNone;
  request.args := TStringList.Create;
  request.args.Add(User);
  request.args.Add(Password);
  request.args.Add('user');

  data := request.getData(size);
  fCubeConn.send(data, size, fCubeConn.getUniqueID, SessionID);

  request.Free;
end;

procedure TFormMain.OnDebug(Sender: TObject; aMsg: String);
begin
  if fChatForm.Count = 0 then exit;
  (fChatForm[0] as TFormChat).RecvMsg('TTNONE' + aMsg);
end;

function TFormMain.ChatForm(User: String; Add: Boolean):TFormChat;
var
  index: Integer;
  form: TFormChat;
begin
  fFormSync.Enter;
  index := fChatUser.IndexOf(user);

  if index = -1 then
  begin
    if Add then
    begin
      form := TFormChat.Create(nil);
      form.SetUser(fUser, user);
      fChatUser.Add(user);
      fChatForm.Add(form);
      form.Show;
    end else
      form := nil;
  end else begin
    form := fChatForm[index] as TFormChat;
  end;

  Result := form;
  fFormSync.Leave;
end;

procedure TFormMain.ChatClose(User: String);
var
  index: Integer;
begin
  fFormSync.Enter;
  index := fChatUser.IndexOf(user);
  if index <> -1 then
  begin
    fChatUser.Delete(index);
    fChatForm.Delete(index);
  end;
  fFormSync.Leave;
end;

procedure TFormMain.Join(Room: String; User: String);
var
  index: Integer;
  form: TFormChat;
begin
  fFormSync.Enter;
  index := fChatUser.IndexOf(User);
  if index <> -1 then
  begin
    form := fChatForm[index] as TFormChat;
    form.SetUser(fUser, Room);
    fChatUser[index] := Room;
  end;
  fFormSync.Leave;
end;

function TFormMain.SendForm(User: String; ID: String): TFormTransfer;
var
  index: Integer;
  form: TFormTransfer;
begin
  fFormSync.Enter;
  index := fSendUser.IndexOf(user + '|' + ID);

  if index = -1 then
  begin
    form := TFormTransfer.Create(nil);
    form.SetUser(user, ID, True);
    fSendUser.Add(user + '|' + ID);
    fSendForm.Add(form);
    form.Show;
  end else begin
    form := fSendForm[index] as TFormTransfer;
  end;

  Result := form;
  fFormSync.Leave;
end;

procedure TFormMain.SendClose(User: String; ID: String);
var
  index: Integer;
begin
  fFormSync.Enter;
  index := fSendUser.IndexOf(user + '|' + ID);
  if index <> -1 then
  begin
    fSendUser.Delete(index);
    fSendForm.Delete(index);
  end;
  fFormSync.Leave;
end;

function TFormMain.SendDropMsg(User: String): Boolean;
var
  i: Integer;
  target: String;
begin
  fFormSync.Enter;
  Result := False;
  for i := 0 to fSendUser.Count - 1 do
  begin
    target := fSendUser[i];
    target := Copy(target, 1, Pos('|', target) - 1);
    if (target = User) then
    begin
      (fSendForm[i] as TFormTransfer).GetDropFilesFromChat;
      Result := True;
      break;
    end;
  end;
  fFormSync.Leave;
end;

function TFormMain.RecvForm(User: String; ID: String): TFormTransfer;
var
  index: Integer;
  form: TFormTransfer;
begin
  fFormSync.Enter;
  index := fRecvUser.IndexOf(user + '|' + ID);

  if index = -1 then
  begin
    form := TFormTransfer.Create(nil);
    form.SetUser(user, ID, False);
    fRecvUser.Add(user + '|' + ID);
    fRecvForm.Add(form);
    form.Show;
  end else begin
    form := fRecvForm[index] as TFormTransfer;
  end;

  Result := form;
  fFormSync.Leave;
end;

procedure TFormMain.RecvClose(User: String; ID: String);
var
  index: Integer;
begin
  fFormSync.Enter;
  index := fRecvUser.IndexOf(user + '|' + ID);
  if index <> -1 then
  begin
    fRecvUser.Delete(index);
    fRecvForm.Delete(index);
  end;
  fFormSync.Leave;
end;

function TFormMain.GetNick(User: String): String;
var
  seq: Integer;
  groupNode, userNode: TTreeNode;
begin
  Result := User;
  seq := StrToInt(User);
  groupNode := TreeView1.Items.GetFirstNode;
  while groupNode <> nil do
  begin
    userNode := groupNode.GetFirstChild;
    while (userNode <> nil) And (userNode.Data <> Pointer(seq)) do
      userNode := userNode.GetNextSibling;
    if userNode <> nil then
    begin
      Result := userNode.Text;
      exit;
    end;
    groupNode := groupNode.GetNextSibling;
  end;
end;

procedure TFormMain.SendMsg(aMsg: string);
var
  packet: AnsiString;
  ptr: Pointer;
begin
  packet := 'TT' + aMsg;
  fCubeConn.Send(Pointer(packet), Length(packet), fCubeConn.getUniqueID, $5454);
end;

procedure TFormMain.SendData(data: Pointer; size, toAddress: DWord);
begin
  fCubeConn.send(data, size, fCubeConn.getUniqueID, toAddress);
end;

function TFormMain.Connected: Boolean;
begin
  Result := fCubeConn.Connected;
end;

end.
