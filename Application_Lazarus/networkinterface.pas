unit NetworkInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Contnrs, TwistedKnot, LCLIntf, LMessages;

const
  NI_EVENT = LM_USER + 101;
  NI_STATUS = NI_EVENT + 1;
  NetworkInterfaceSendStart = 1;
  NetworkInterfaceSendComplete = 2;
  NetworkInterfaceReceiveStart = 3;
  NetworkInterfaceReceiveComplete = 4;

type

  { TNetworkInterface }

  TNetworkInterface = class(TTwistedKnot)
  private
    fConnection: TTwistedKnotConnection;
    fKeepAlive: TThread;
    fLastUniqueID: DWord;
    fSection: TCriticalSection;
    fItems: TObjectList;
    fStatus: TStringList;

    function getConnected: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Reconnect;
    procedure Close;
    procedure send(Data: Pointer; Len: Integer; UniqueID, toAddress:DWord);
    function getItem(var Status: Integer): TObject;

    function getUniqueID:DWord;

    procedure notify(Status: TTwistedKnotStatus); override;
    procedure sendStart(Sender: TTwistedKnotSender); override;
    procedure sendCompleted(Sender: TTwistedKnotSender); override;
    function canReceive(UniqueID, From, Length: Cardinal): Boolean; override;
    procedure receiveStart(Receiver: TTwistedKnotReceiver); override;
    procedure receiveCompleted(Receiver: TTwistedKnotReceiver); override;

    property Connected: Boolean read getConnected;
  end;

implementation

uses
  main, DateUtils;

const
  PublicKey =
    'MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDELgZ/OqvlVGkWOPrRKB26IOss' +
    'jHeRHKaCaGiaw8pJ7GATrileaqntWukseWGRIsO6CnjosWUcH0NOW4BGjCq4maXr' +
    'LH8N4EzXKJoFuGKdELDUfEZdozHnAOKY2IiFAKjPSAeUL2Rd5tg7hbIwum0JV9yy' +
    'mFz7gnP4jKQjCFunBQIDAQAB';
  ServerHost = 'tknot.steelozcore.net';
  ServerPort = 4430;

type

  { TKeepAliveThread }

  TKeepAliveThread = class(TThread)
  private
    fConnection: TTwistedKnotConnection;
  protected
    procedure Execute; override;
  public
    constructor Create;

    property Connection: TTwistedKnotConnection write fConnection;
  end;

{ TKeepAliveThread }

constructor TKeepAliveThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  fConnection := nil;
end;

procedure TKeepAliveThread.Execute;
begin
  while not Terminated do
  begin
    if Assigned(fConnection) and fConnection.Connected then
    begin
      fConnection.Ping;
      fConnection.RetryDelayed;

      if SecondsBetween(Now, fConnection.LastReceived) > 60 then
      begin
        fConnection.Connect;
      end;
    end;

    Sleep(3000);
  end;
end;

{ TNetworkInterface }

function TNetworkInterface.getConnected: Boolean;
begin
  Result := fConnection.Connected and fConnection.Handshaked;
end;

constructor TNetworkInterface.Create;
begin
  fLastUniqueID := 0;
  fSection := syncobjs.TCriticalSection.Create;
  fItems := TObjectList.Create(False);
  fStatus := TStringList.Create;

  fConnection := TTwistedKnotConnection.Create;
  fConnection.PublicKey := PublicKey;
  fConnection.Address := ServerHost;
  fConnection.Port := ServerPort;
  fConnection.Handler := self;
  fConnection.Start;

  fKeepAlive := TKeepAliveThread.Create;
  (fKeepAlive as TKeepAliveThread).Connection := fConnection;
  fKeepAlive.Start;
end;

destructor TNetworkInterface.Destroy;
begin
  (fKeepAlive as TKeepAliveThread).Connection := nil;
  fKeepAlive.Terminate;

  fConnection.Handler := nil;
  fConnection.Terminate;
  fConnection.Close;
  fConnection := nil;

  fSection.Free;
  fItems.Free;
  fStatus.Free;
end;

procedure TNetworkInterface.Connect;
begin
  if fConnection.Connected then
  begin
    fConnection.Close;
    while fConnection.Connected do
      Sleep(1);
  end;

  fConnection.Connect;
end;

procedure TNetworkInterface.Reconnect;
begin
  fConnection.Connect;
end;

procedure TNetworkInterface.Close;
begin
  fConnection.Close;
end;

procedure TNetworkInterface.send(Data: Pointer; Len: Integer; UniqueID,
  toAddress: DWord);
begin
  fConnection.Send(Data, Len, UniqueID, toAddress);
end;

function TNetworkInterface.getItem(var Status: Integer): TObject;
begin
  Result := nil;
  Status := 0;

  fSection.Acquire;
  if fItems.Count > 0 then
  begin
    Result := fItems.Items[0];
    Status := StrToInt(fStatus[0]);
    fItems.Delete(0);
    fStatus.Delete(0);
  end;
  fSection.Release;
end;

function TNetworkInterface.getUniqueID: DWord;
begin
  fSection.Acquire;
  fLastUniqueID := DWord(fLastUniqueID + 1);
  Result := fLastUniqueID;
  fSection.Release;
end;

procedure TNetworkInterface.notify(Status: TTwistedKnotStatus);
begin
  PostMessage(FormMain.Handle, NI_STATUS, Ord(Status), 0);
end;

procedure TNetworkInterface.sendStart(Sender: TTwistedKnotSender);
begin
  fSection.Acquire;
  fItems.Add(Sender);
  fStatus.Add(IntToStr(NetworkInterfaceSendStart));
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

procedure TNetworkInterface.sendCompleted(Sender: TTwistedKnotSender);
begin
  fSection.Acquire;
  fItems.Add(Sender);
  fStatus.Add(IntToStr(NetworkInterfaceSendComplete));
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

function TNetworkInterface.canReceive(UniqueID, From, Length: Cardinal
  ): Boolean;
begin
  Result := True;
end;

procedure TNetworkInterface.receiveStart(Receiver: TTwistedKnotReceiver);
begin
  fSection.Acquire;
  fItems.Add(Receiver);
  fStatus.Add(IntToStr(NetworkInterfaceReceiveStart));
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

procedure TNetworkInterface.receiveCompleted(Receiver: TTwistedKnotReceiver);
begin
  fSection.Acquire;
  fItems.Add(Receiver);
  fStatus.Add(IntToStr(NetworkInterfaceReceiveComplete));
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

end.

