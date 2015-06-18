unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, ExtCtrls, Contnrs, syncobjs, LMessages, ChatLabel,
  NetworkInterface, TwistedKnot, chat, cDataStructs;

type

  { TFormMain }

  TFormMain = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuExit: TMenuItem;
    MenuConnect: TMenuItem;
    MenuFileList: TMenuItem;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuFileListClick(Sender: TObject);
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
    fFileList: TSparseStringArray;
    fNickList: TStringDictionary;
    fFormSync: TCriticalSection;

    fLeftSkin, fRightSkin, fSelectSkin: TChatSkin;

    procedure OnReceive(var Msg: TLMessage); message NI_RECEIVE;
    procedure OnStatus(var Msg: TLMessage); message NI_STATUS;
    procedure OnTalkTo(Receiver: TTwistedKnotReceiver);
    procedure OnSession(Receiver: TTwistedKnotReceiver);
    procedure OnCargoCompany(Receiver: TTwistedKnotReceiver);
    procedure UpdateUserInfo(User, Nick, Group, Status, Image: String);
    procedure ChatCloseAll;
    function GetConnected: Boolean;
  public
    { public declarations }
    procedure SignIn(User: String; Password: String);
    procedure Join(Room: String; User:String);
    function ChatForm(User: String; Add: Boolean = True): TFormChat;
    procedure ChatClose(User: String);
    function SendData(data: Pointer; size, toAddress: DWord):DWord;
    function GetNick(User: String): String;

    property Connected: Boolean read GetConnected;
    property LeftSkin: TChatSkin read fLeftSkin;
    property RightSkin: TChatSkin read fRightSkin;
    property SelectSkin: TChatSkin read fSelectSkin;
    property NickList: TStringDictionary read fNickList;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  login, filelist, DateUtils, LCLType, LazUTF8Classes,
  OZFTalkTo, Session, CargoCompany;

{ TFormMain }

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.MenuConnectClick(Sender: TObject);
var
  tt: TTalkTo;
  size: DWord;
  data: Pointer;
begin
  if fAuth then
  begin
    tt := TTalkTo.Create;
    tt.functionID := TalkToFunctionIDQuit;

    data := tt.getData(size);
    tt.Free;
    SendData(data, size, TalkToID);
  end
  else
  begin
    fCubeConn.Connect;
    FormLogin.Show;
  end;
end;

procedure TFormMain.MenuFileListClick(Sender: TObject);
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
  fFileList := TSparseStringArray.Create;
  fNickList := TStringDictionary.Create;

  MenuConnect.Caption := '&Connect...';
  MenuFileList.Enabled := False;

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
  tt: TTalkTo;
  size: DWord;
  data: Pointer;
  i: Integer;
  Chat: TFormChat;
begin
  tt := TTalkTo.Create;
  tt.functionID := TalkToFunctionIDQuit;

  data := tt.getData(size);
  tt.Free;
  SendData(data, size, TalkToID);

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

  fFileList.Free;
  fNickList.Free;

  fFormSync.Free;
end;

procedure TFormMain.TreeView1DblClick(Sender: TObject);
var
  user: String;
begin
  if TreeView1.Selected = nil then exit;
  User := IntToStr(PtrInt(TreeView1.Selected.Data));
  if User = fUser then exit;
  ChatForm(User).Show;
end;

procedure TFormMain.OnReceive(var Msg: TLMessage);
var
  Receiver: TTwistedKnotReceiver;
  service: Word;
begin
  while true do
  begin
    Receiver := fCubeConn.getReceiver;

    if Receiver = nil then
      break;

    if Receiver.Length < 2 then
    begin
      Receiver.Free;
      continue;
    end;

    Move(Receiver.Buffer^, service, 2);
    {$ifndef FPC_BIG_ENDIAN}
    service := swap(service);
    {$endif}

    if service = SessionID then
      OnSession(Receiver)
    else if service = TalkToID then
      OnTalkTo(Receiver)
    else if service = CargoCompanyID then
      OnCargoCompany(Receiver);

    Receiver.Free;
  end;
end;

procedure TFormMain.OnTalkTo(Receiver: TTwistedKnotReceiver);
var
  tt: TTalkTo;
  user, room, nick, group, status, image, seq: String;
  form: TFormChat;
  cargo: TCargoCompany;
  data: Pointer;
  size, uniqueID: DWord;
begin
  tt := TTalkTo.Create(Receiver.Buffer, Receiver.Length);
  if tt.functionID = TalkToFunctionIDQuit then
  begin
    if fAuth then
    begin
      fAuth := False;
      Caption := 'Talk To';
      MenuConnect.Caption := '&Connect...';
      MenuFileList.Enabled := False;
      TreeView1.Items.Clear;
      ChatCloseAll;
    end
    else
    begin
      FormLogin.Show;
      FormLogin.Fail;
    end;
  end
  else if tt.functionID = TalkToFunctionIDAuth then
  begin
    fAuth := True;
    fUser := tt.args[0];
    MenuConnect.Caption := 'Dis&connect';
    MenuFileList.Enabled := True;
    FormLogin.Hide;
    FormFileList.ButtonRefreshClick(nil);
  end
  else if tt.functionID = TalkToFunctionIDStat then
  begin
    user := tt.args[0];
    nick := tt.args[1];
    group := tt.args[2];
    status := tt.args[3];
    image := tt.args[4];
    if status <> 'offline' then
      fNickList[user] := nick
    else if fNickList.HasKey(User) then
      fNickList.Delete(user);
    updateUserInfo(user, nick, group, status, image);
    if user = fUser then
      Caption := 'Talk To - ' + nick;
  end
  else if tt.functionID = TalkToFunctionIDMessage then
  begin
    user := tt.args[0];
    if user = fUser then
      user := tt.args[1];
    ChatForm(user).RecvMsg(tt);
  end
  else if tt.functionID = TalkToFunctionIDGroupJoin then
  begin
    room := tt.args[0];
    user := tt.args[1];
    Join(room, user);
  end
  else if tt.functionID = TalkToFunctionIDGroupInvite then
  begin
    room := tt.args[0];
    user := tt.args[1];
    if user = fUser then
    begin
      ChatForm(room).RecvMsg(tt);
    end
    else
    begin
      form := ChatForm(room, False);
      if form <> nil then
      begin
        form.RecvMsg(tt);
      end;
    end;
  end
  else if tt.functionID = TalkToFunctionIDGroupExit then
  begin
    room := tt.args[0];
    user := tt.args[1];
    form := ChatForm(room, False);
    if form <> nil then
    begin
      form.RecvMsg(tt);
      if user = fUser then
        form.Free;
    end;
  end
  else if tt.functionID = TalkToFunctionIDFileShare then
  begin
    room := tt.args[0];
    user := tt.args[1];
    seq := tt.args[2];

    cargo := TCargoCompany.Create;
    cargo.Command := CargoCompanyTypeStat;
    cargo.Seq := StrToInt(seq);

    data := cargo.getData(size);
    uniqueID := fCubeConn.getUniqueID;
    fCubeConn.send(data, size, uniqueID, $5454);
    cargo.Free;

    fFileList[uniqueID] := room + '|' + user;
  end;
  if Assigned(tt) then
    FreeAndNil(tt);
end;

procedure TFormMain.OnSession(Receiver: TTwistedKnotReceiver);
var
  response: TSession;
  tt: TTalkTo;
  size: DWord;
  data: Pointer;
begin
  response := TSession.Create(Receiver.Buffer, Receiver.Length);
  if response.functionID = SessionFunctionIDSignIn then
  begin
    if response.errorCode = SessionErrorCodeNone then
    begin
      tt := TTalkTo.Create;
      tt.functionID := TalkToFunctionIDAuth;

      data := tt.getData(size);
      tt.Free;
      SendData(data, size, TalkToID);
    end
    else
    begin
      FormLogin.Show;
      FormLogin.Fail;
    end;
  end;
end;

procedure TFormMain.OnCargoCompany(Receiver: TTwistedKnotReceiver);
var
  tt: TTalkTo;
  cargo: TCargoCompany;
  fsOut: TFileStreamUTF8;
  list: TListView;
  item: TListItem;
  i, cut: Integer;
  size: DWord;
  data: Pointer;
  str, seq, filename, sizestr, mime, expire, user: String;
  date: TDateTime;
  cont: Boolean;
begin
  cargo := TCargoCompany.Create(Receiver.Buffer, Receiver.Length);

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
    cont := SaveDialog1.Execute;
    if cont And FileExistsUTF8(SaveDialog1.FileName) then
      cont := Application.MessageBox('Overwrite exists file?', 'Overwrite', MB_YESNO) = IDYES;
    if cont then
    begin
      try
        fsOut := TFileStreamUTF8.Create(SaveDialog1.FileName, fmCreate);
        size := fsOut.Write(cargo.Content^, cargo.ContentSize);
        fsOut.Free;

        if cargo.ContentSize <> size then
        begin
          Application.MessageBox('Not enough space', 'Confirm', MB_ICONERROR);
        end;
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'File I/O Error', MB_ICONERROR);
      end;
    end;
  end
  else if (cargo.Command = CargoCompanyTypeResult) And
    (cargo.ResultType = CargoCompanyTypeUpload) then
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
  else if (cargo.Command = CargoCompanyTypeResult) And
    (cargo.ResultType = CargoCompanyTypeStat) then
  begin
    if fFileList.HasItem(Receiver.UniqueID) then
    begin
      str := fFileList[Receiver.UniqueID];
      cut := Pos('|', str);
      user := Copy(str, 1, cut - 1);
      str := Copy(str, cut + 1, Length(str));
      if user = fUser then
        user := str;
      ChatForm(user).RecvFile(str, cargo.Name, cargo.Mime,
        cargo.Seq, cargo.Size, cargo.Expire);
      fFileList.Delete(Receiver.UniqueID);
    end;
  end
  else if cargo.Command = CargoCompanyTypeRemove then
  begin
    list := FormFileList.ListView1;
    list.BeginUpdate;

    seq := IntToStr(cargo.Seq);
    for i := list.Items.Count - 1 downto 0 do
    begin
      if seq = list.Items[i].SubItems[2] then
      begin
        list.Items[i].Delete;
        break;
      end;
    end;

    list.EndUpdate;
  end
  else if cargo.Command = CargoCompanyTypeShare then
  begin
    user := FormFileList.GetSharedTarget(Receiver.UniqueID);
    if user <> '' then
    begin
      tt := TTalkTo.Create;

      tt.functionID := TalkToFunctionIDFileShare;
      tt.args := TStringList.Create;
      tt.args.Add(user);
      tt.args.Add(IntToStr(cargo.Seq));

      data := tt.getData(size);
      FreeAndNil(tt);

      SendData(data, size, TalkToID);
    end;
  end
  else if cargo.Command = CargoCompanyTypeList then
  begin
    list := FormFileList.ListView1;
    list.BeginUpdate;

    for i := 0 to (cargo.Args.count div 5) - 1 do
    begin
      seq := cargo.Args[i * 5];
      filename := cargo.Args[i * 5 + 1];
      sizestr := cargo.Args[i * 5 + 2];
      mime := cargo.Args[i * 5 + 3];
      expire := cargo.Args[i * 5 + 4];
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
      if cargo.Args.IndexOf(filename) = -1 then
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

procedure TFormMain.ChatCloseAll;
var
  i: Integer;
  form: TFormChat;
begin
  for i := fChatUser.Count - 1 downto 0 do
  begin
    form := fChatForm[i] as TFormChat;
    form.Free;
  end;
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
{
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
}
function TFormMain.GetNick(User: String): String;
begin
  if fNickList.HasKey(User) then
    Result := fNickList[User]
  else
    Result := User;
end;

function TFormMain.SendData(data: Pointer; size, toAddress: DWord): DWord;
begin
  Result := fCubeConn.getUniqueID;
  fCubeConn.send(data, size, Result, toAddress);
end;

function TFormMain.GetConnected: Boolean;
begin
  Result := fCubeConn.Connected;
end;

end.
