unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, StdCtrls, ExtCtrls, Contnrs, syncobjs, LMessages, ChatLabel,
  NetworkInterface, TwistedKnot, chat, cDataStructs, hash;

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
    Timer1: TTimer;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuConnectClick(Sender: TObject);
    procedure MenuFileListClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
  private
    { private declarations }
    fConfigFile: String;
    fNetwork: TNetworkInterface;
    fNetworkStatus: TTwistedKnotStatus;
    fAuth: Boolean;
    fUser: String;
    fUploadID, fUploadAddress, fUploadStreamID:DWord;
    fUploadSeq, fUploadPos, fUploadSize: DWord;
    fChatForm: TObjectList;
    fChatUser: TStringList;
    fSendForm: TObjectList;
    fSendUser: TStringList;
    fRecvForm: TObjectList;
    fRecvUser: TStringList;
    fRequests: TSparseStringArray;
    fFileNames: TSparseStringArray;
    fFileSizes: TSparseInt64Array;
    fNickList: TStringDictionary;
    fUploadList: TStringList;
    fSenderList: TObjectList;
    fFormSync: TCriticalSection;
    fHashThread: THashThread;

    fLeftSkin, fRightSkin, fSelectSkin: TChatSkin;

    procedure OnNetworkEvent(var Msg: TLMessage); message NI_EVENT;
    procedure OnStatus(var Msg: TLMessage); message NI_STATUS;
    procedure OnTalkTo(Receiver: TTwistedKnotReceiver);
    procedure OnSession(Receiver: TTwistedKnotReceiver);
    procedure OnCargoCompany(Receiver: TTwistedKnotReceiver);
    procedure UpdateUserInfo(User, Nick, Group, Status, Image: String);
    procedure ChatCloseAll;
    function GetConnected: Boolean;
    procedure FileUpload;
  public
    { public declarations }
    procedure SignIn(User: String; Password: String);
    procedure Join(Room: String; User:String);
    function ChatForm(User: String; Add: Boolean = True): TFormChat;
    procedure ChatClose(User: String);
    function SendData(data: Pointer; size, toAddress: DWord):DWord;
    procedure SendData(data: Pointer; size, uniqueID, toAddress: DWord);
    function GetNick(User: String): String;
    procedure AddUpload(Target, FileName: String);
    procedure FileShare(Target, FileSeq: String);
    procedure FileDownload(Seq: DWord);
    procedure HashResult(Target, FileName, MD5, SHA256: String);

    property ConfigFile: String read fConfigFile;
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
  login, filelist,
  LCLType, IniFiles, DateUtils, LazFileUtils, LazUTF8Classes, cHash,
  OZFTalkTo, Session, CargoCompany;

{ TFormMain }

{$IFDEF WINDOWS}
function GetAppName: String;
begin
  Result := 'talkto';
end;
{$ENDIF}

procedure TFormMain.FormCreate(Sender: TObject);
var
  {$IFDEF WINDOWS}
  OldGetAppName: TGetAppNameEvent;
  {$ENDIF}
  Skin: TBitmap;
begin
  {$IFDEF WINDOWS}
  OldGetAppName := OnGetApplicationName;
  OnGetApplicationName := @GetAppName;
  try
    fConfigFile := GetAppConfigDir(False);
    if not DirectoryExistsUTF8(fConfigFile) then
      MkDir(fConfigFile);
    fConfigFile := fConfigFile + 'talkto.ini';
  finally
    OnGetApplicationName := OldGetAppName;
  end;
  {$ELSE}
  fConfigFile := ExpandFileName('~/.talkto.conf');
  {$ENDIF}

  fNetworkStatus := TwistedKnotStatusDisconnect;

  fFormSync := syncobjs.TCriticalSection.Create;
  fAuth := False;
  fUploadID := 0;
  fUploadPos := 0;
  fUploadSize := 0;

  fNetwork := TNetworkInterface.Create;
  fChatForm := TObjectList.Create(False);
  fChatUser := TStringList.Create;
  fSendForm := TObjectList.Create;
  fSendUser := TStringList.Create;
  fRecvForm := TObjectList.Create;
  fRecvUser := TStringList.Create;
  fRequests := TSparseStringArray.Create;
  fFileNames := TSparseStringArray.Create;
  fFileSizes := TSparseInt64Array.Create;
  fNickList := TStringDictionary.Create;
  fUploadList := TStringList.Create;
  fSenderList := TObjectList.create(False);
  fHashThread := THashThread.Create;

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
  fHashThread.Terminate;

  tt := TTalkTo.Create;
  tt.functionID := TalkToFunctionIDQuit;

  data := tt.getData(size);
  tt.Free;
  SendData(data, size, TalkToID);

  fNetwork.Free;

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

  fRequests.Free;
  fNickList.Free;
  fUploadList.Free;
  fSenderList.Free;

  fFormSync.Free;
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
    if not fNetwork.Connected then
      fNetwork.Connect;
    FormLogin.Show;
  end;
end;

procedure TFormMain.MenuFileListClick(Sender: TObject);
begin
  FormFileList.Share := '';
  FormFileList.Show;
end;

procedure TFormMain.MenuExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormMain.StatusBar1Click(Sender: TObject);
var
  seq: DWord;
  cargo: TCargoCompany;
  data: Pointer;
  size: DWord;
begin
  if fUploadID = 0 then
    exit;

  seq := fUploadSeq;

  if Application.MessageBox('Cancel this upload?', 'Confirm', MB_YESNO) <> IDYES then
    exit;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeCancel;
  cargo.Seq := seq;

  data := cargo.getData(size);
  cargo.Free;

  SendData(data, size, CargoCompanyID);
end;

procedure TFormMain.Timer1Timer(Sender: TObject);
const
  units: array[0..4] of String = ('B', 'KB', 'MB', 'GB', 'TB');
var
  item: TTwistedKnotSender;
  sent, total: Double;
  result: String;
  i: Integer;
begin
  if fUploadID = 0 then
  begin
    if fNetworkStatus = TwistedKnotStatusDisconnect then
      StatusBar1.Panels[0].Text := 'Disconnected'
    else
      StatusBar1.Panels[0].Text := 'Connected';
    exit;
  end;

  sent := fUploadPos;
  total := fUploadSize;

  if fUploadAddress <> 0 then
  begin
    sent := sent + fNetwork.querySendProgress(fUploadAddress, fUploadStreamID);
  end;

  for i := 0 to 4 do
  begin
    if sent < 1024 then
      break;
    sent := sent / 1024;
  end;

  if i = 0 then
    result := IntToStr(Trunc(sent)) + ' ' + units[i]
  else
    result := Format('%.2f', [sent]) + ' ' + units[i];

  for i := 0 to 4 do
  begin
    if total < 1024 then
      break;
    total := total / 1024;
  end;

  if i = 0 then
    result := result + ' / ' + IntToStr(Trunc(total)) + ' ' + units[i]
  else
    result := result + ' / ' + Format('%.2f', [total]) + ' ' + units[i];

  if fNetworkStatus = TwistedKnotStatusDisconnect then
    StatusBar1.Panels[0].Text := 'Disconnected, Upload: ' + result
  else
    StatusBar1.Panels[0].Text := 'Connected, Upload: ' + result;
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

procedure TFormMain.OnNetworkEvent(var Msg: TLMessage);
var
  Event: TNetworkEvent;
  Sender: TTwistedKnotSender;
  Receiver: TTwistedKnotReceiver;
begin
  while true do
  begin
    Event := fNetwork.getEvent;
    if Event = nil then
      break;

    if Event.isReceive then
    begin
      if Event.Status <> NetworkInterfaceReceiveComplete then
        continue;

      Receiver := Event.Stream as TTwistedKnotReceiver;

      if Receiver = nil then
        break;

      if Receiver.Length < 2 then
      begin
        Receiver.Free;
        continue;
      end;

      if Receiver.Address = SessionID then
        OnSession(Receiver)
      else if Receiver.Address = TalkToID then
        OnTalkTo(Receiver)
      else if Receiver.Address = CargoCompanyID then
        OnCargoCompany(Receiver);

      Receiver.Free;
    end
    else
    begin
      if Event.UniqueID = fUploadID then
      begin
        fUploadAddress := Event.Address;
        fUploadStreamID := Event.StreamID;
      end;

      if Event.Status <> NetworkInterfaceSendComplete then
        continue;

      Sender := Event.Stream as TTwistedKnotSender;
      Sender.Free;

      fUploadAddress := 0;
      fUploadStreamID := 0;
    end;

    FreeAndNil(Event);
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
  Config: TIniFile;
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
      fNickList.Clear;
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

    ShortDateFormat := 'yy-mm-dd';
    LongTimeFormat := 'hh:nn:ss';

    Config := TIniFile.Create(fConfigFile);
    Config.WriteString('Login', 'Date', DateToStr(Now));
    Config.WriteString('Login', 'Time', TimeToStr(Now));
    Config.Free;
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
    cargo.Free;

    uniqueID := SendData(data, size, CargoCompanyID);
    fRequests[uniqueID] := 'FI' + room + '|' + user;
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
  cargo, response: TCargoCompany;
  fsIn: TFileStreamUTF8;
  fsOut: TFileStreamUTF8;
  list: TListView;
  item: TListItem;
  i, cut: Integer;
  read, size, nextpos: DWord;
  data: Pointer;
  str, seq, filename, sizestr, mime, expire, user: String;
  date: TDateTime;
  cont: Boolean;
begin
  cargo := TCargoCompany.Create(Receiver.Buffer, Receiver.Length);

  if cargo.ErrorCode <> 0 then
  begin
    if (cargo.Command = CargoCompanyTypeResult) And
      (cargo.ResultType = CargoCompanyTypeUpload) then
    begin
      fUploadID := 0;
      FileUpload;
    end;

    if cargo.ErrorCode = 1 then
      str := 'invalid value'
    else
      str := 'server error';

    Application.MessageBox(PChar(str), 'Cargo Company Error', 0);
  end
  else if cargo.Command = CargoCompanyTypeUpload then
  begin
    if fRequests.HasItem(Receiver.UniqueID) then
    begin
      str := fRequests[Receiver.UniqueID];
      cut := Pos('|', str);
      filename := Copy(str, 3, cut - 3);
      fUploadSeq := cargo.seq;

      data := nil;

      try
        fsIn := TFileStreamUTF8.Create(filename, fmOpenRead);
        fsIn.Seek(cargo.Position, soBeginning);
        size := fsIn.Size - cargo.Position;
        if size > 1024*1024 then
          size := 1024*1024;
        data := GetMem(size);
        read := 0;
        if data <> nil then
        begin
          read := fsIn.Read(data^, size);
          FreeAndNil(fsIn);
        end
        else
        begin
          FreeAndNil(fsIn);
          Application.MessageBox('Not enough memory', 'Confirm', MB_ICONERROR);
        end;
      except
        on E: Exception do
        begin
          Application.MessageBox(PChar(E.Message), 'File I/O Error', MB_ICONERROR);
          if data <> nil then
            FreeMem(data);
          data := nil;
        end;
      end;

      if size <> read then
      begin
        Application.MessageBox('Error on file reading', 'Confirm', MB_ICONERROR);
        FreeMem(data);
        data := nil;
      end;

      if data <> nil then
      begin
        response := TCargoCompany.Create;
        response.Command := CargoCompanyTypeTransfer;
        response.Seq := cargo.Seq;
        response.Position := cargo.Position;
        response.Content := data;
        response.ContentSize := size;

        data := response.getData(size);
        response.Free;

        SendData(data, size, Receiver.UniqueID, CargoCompanyID);
        fUploadPos := cargo.Position;
      end;
    end;
  end
  else if cargo.Command = CargoCompanyTypeTransfer then
  begin
    if fRequests.HasItem(Receiver.UniqueID) then
    begin
      str := fRequests[Receiver.UniqueID];
      filename := Copy(str, 3, Length(str));

      try
        if FileExistsUTF8(filename) then
          fsOut := TFileStreamUTF8.Create(filename, fmOpenRead)
        else
          fsOut := TFileStreamUTF8.Create(filename, fmCreate);

        fsOut.Seek(cargo.Position, soBeginning);
        size := fsOut.Write(cargo.Content^, cargo.ContentSize);
        nextpos := fsOut.Position;
        fsOut.Free;

        if cargo.ContentSize <> size then
        begin
          Application.MessageBox('Not enough space', 'Confirm', MB_ICONERROR);
        end;
      except
        on E: Exception do
          Application.MessageBox(PChar(E.Message), 'File I/O Error', MB_ICONERROR);
      end;

      if fFileSizes[cargo.Seq] > nextpos then
      begin
        response := TCargoCompany.Create;
        response.Command := CargoCompanyTypeDownload;
        response.Seq := cargo.Seq;
        response.Position := nextpos;

        data := response.getData(size);
        response.Free;

        SendData(data, size, Receiver.UniqueID, CargoCompanyID);
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
    mime := cargo.Mime;

    item := list.Items.FindCaption(0, filename, False, True, False);
    if item = nil then
    begin
      item := list.Items.Add;
      item.Caption := filename;
      item.SubItems.Add(sizestr);
      item.SubItems.Add(expire);
      item.SubItems.Add(seq);
      item.SubItems.Add(mime);
    end
    else
    begin
      item.SubItems[0] := sizestr;
      item.SubItems[1] := expire;
      item.SubItems[2] := seq;
      item.SubItems[3] := mime;
    end;

    list.AlphaSort;
    list.EndUpdate;

    if fRequests.HasItem(Receiver.UniqueID) then
    begin
      user := fRequests[Receiver.UniqueID];
      fRequests.Delete(Receiver.UniqueID);
      if (Copy(user, 1, 2) = 'UP') then
      begin
        cut := Pos('|', user);
        Delete(user, 1, cut);
        if Length(user) > 0 then
          FileShare(user, seq);
      end;
    end;

    fUploadID := 0;
    FileUpload;
  end
  else if (cargo.Command = CargoCompanyTypeResult) And
    (cargo.ResultType = CargoCompanyTypeCancel) then
  begin
    fUploadID := 0;
    FileUpload;
  end
  else if (cargo.Command = CargoCompanyTypeResult) And
    (cargo.ResultType = CargoCompanyTypeStat) then
  begin
    if fRequests.HasItem(Receiver.UniqueID) then
    begin
      str := fRequests[Receiver.UniqueID];
      fRequests.Delete(Receiver.UniqueID);

      if Copy(str, 1, 2) = 'FI' then
      begin
        Delete(str, 1, 2);
        cut := Pos('|', str);
        user := Copy(str, 1, cut - 1);
        Delete(str, 1, cut);
        if user = fUser then
          user := str;
        ChatForm(user).RecvFile(str, cargo.Name, cargo.Mime,
          cargo.Seq, cargo.Size, cargo.Expire);
        fFileNames[cargo.Seq] := cargo.Name;
        fFileSizes[cargo.Seq] := cargo.Size;
      end;
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
    user := '';
    if fRequests.HasItem(Receiver.UniqueID) then
    begin
      user := fRequests[Receiver.UniqueID];
      fRequests.Delete(Receiver.UniqueID);
      if Copy(user, 1, 2) = 'SH' then
        Delete(user, 1, 2)
      else
        user := '';
    end;
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
        item.SubItems.Add(mime);
      end
      else
      begin
        item.SubItems[0] := sizestr;
        item.SubItems[1] := expire;
        item.SubItems[2] := seq;
        item.SubItems[3] := mime;
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
begin
  fNetworkStatus := TTwistedKnotStatus(Msg.WParam);

  if fNetworkStatus = TwistedKnotStatusDisconnect then
    StatusBar1.Panels[0].Text := 'Disconnected'
  else
    StatusBar1.Panels[0].Text := 'Connected';

  if fNetworkStatus = TwistedKnotStatusConnect then
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
  fNetwork.send(data, size, fNetwork.getUniqueID, SessionID);

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

procedure TFormMain.AddUpload(Target, FileName: String);
begin
  fHashThread.addFile(Target, FileName);
end;

procedure TFormMain.FileShare(Target, FileSeq: String);
var
  cargo: TCargoCompany;
  data: Pointer;
  size, uniqueID: DWord;
  form: TFormChat;
begin
  if Target = '' then
    exit;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeShare;
  cargo.Seq := StrToInt(FileSeq);
  if Target[1] = '#' then
  begin
    form := FormMain.ChatForm(Target, False);
    if Assigned(form) then
    begin
      cargo.Args := TStringList.Create;
      cargo.Args.AddStrings(form.Users);
    end;
  end
  else
  begin
    cargo.Args := TStringList.Create;
    cargo.Args.Add(Target);
  end;

  data := cargo.getData(size);
  cargo.Free;

  uniqueID := SendData(data, size, CargoCompanyID);
  fRequests[uniqueID] := 'SH' + Target;
end;

procedure TFormMain.FileDownload(Seq: DWord);
var
  download: String;
  nextpos: Int64;
  cargo: TCargoCompany;
  data: Pointer;
  size, UniqueID: DWord;
begin
  if not fFileNames.HasItem(Seq) then
  begin
    Application.MessageBox('Invalid action', 'Confirm', MB_ICONERROR);
    exit;
  end;

  download := '';
  nextpos := 0;
  SaveDialog1.FileName := fFileNames[Seq];

  if SaveDialog1.Execute then
  begin
    download := SaveDialog1.FileName;
    if FileExistsUTF8(download) then
    begin
      if Application.MessageBox('Resume download?', 'Resume', MB_YESNO) = IDYES then
        nextpos := FileSizeUTF8(download);
    end;
  end;

  if download = '' then
    exit;

  if (nextpos = 0) and FileExistsUTF8(download) then
  begin
    DeleteFileUTF8(download);
  end;

  cargo := TCargoCompany.Create;
  cargo.Command := CargoCompanyTypeDownload;
  cargo.Seq := Seq;
  cargo.Position := nextpos;

  data := cargo.getData(size);
  cargo.Free;

  UniqueID := SendData(data, size, CargoCompanyID);
  fRequests[UniqueID] := 'DN' + download;
end;

procedure TFormMain.HashResult(Target, FileName, MD5, SHA256: String);
begin
  if SHA256 = '' then
  begin
    Application.MessageBox(PChar(MD5), 'File I/O Error', MB_ICONERROR);
    exit;
  end;

  fFormSync.Enter;
  fUploadList.Add(Target + '|' + FileName + '|' + MD5 + '|' + SHA256);
  fFormSync.Leave;
  if fUploadID = 0 then
    FileUpload;
end;

function TFormMain.SendData(data: Pointer; size, toAddress: DWord): DWord;
begin
  Result := fNetwork.getUniqueID;
  fNetwork.send(data, size, Result, toAddress);
end;

procedure TFormMain.SendData(data: Pointer; size, uniqueID, toAddress: DWord);
begin
  fNetwork.send(data, size, uniqueID, toAddress);
end;

function TFormMain.GetConnected: Boolean;
begin
  Result := fNetwork.Connected;
end;

procedure TFormMain.FileUpload;
var
  upload, user: String;
  cut: Integer;
  filesize: Int64;
  cargo: TCargoCompany;
  size: DWord;
  data: Pointer;
  uniqueID: DWord;
  md5: String;
  sha256: String;
begin
  while True do
  begin
    upload := '';
    fFormSync.Enter;
    if (fUploadID = 0) And (fUploadList.Count > 0) then
    begin
      upload := fUploadList[0];
      fUploadList.Delete(0);
    end;
    fFormSync.Leave;
    if upload = '' then
      break;

    cut := Pos('|', upload);
    user := Copy(upload, 1, cut - 1);
    Delete(upload, 1, cut);

    cut := Pos('|', upload);
    sha256 := Copy(upload, cut + 1, Length(upload));
    upload := Copy(upload, 1, cut - 1);

    cut := Pos('|', sha256);
    md5 := Copy(sha256, 1, cut - 1);
    Delete(sha256, 1, cut);

    if not FileExistsUTF8(upload) then
    begin
      Application.MessageBox('File not found', 'Confirm', MB_ICONERROR);
      continue;
    end;

    if DirectoryExistsUTF8(upload) then
    begin
      Application.MessageBox('Invalid file', 'Confirm', MB_ICONERROR);
      continue;
    end;

    filesize := FileSizeUTF8(upload);

    cargo := TCargoCompany.Create;
    cargo.Command := CargoCompanyTypeUploadRequest;
    cargo.Name := ExtractFileName(upload);
    cargo.Size := filesize;
    cargo.MD5 := md5;
    cargo.SHA256 := sha256;

    data := cargo.getData(size);
    cargo.Free;

    uniqueID :=  SendData(data, size, CargoCompanyID);
    fRequests[uniqueID] := 'UP' + upload + '|' + user;

    fUploadID := uniqueID;
    fUploadPos := 0;
    fUploadSize := filesize;
    fUploadAddress := 0;
    fUploadStreamID := 0;

    break;
  end;
end;

end.
