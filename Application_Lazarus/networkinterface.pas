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

  { TNetworkEvent }

  TNetworkEvent = class(TObject)
  private
    fStatus: Integer;
    fReceive: Boolean;
    fAddress: DWord;
    fUniqueID: DWord;
    fStreamID: DWord;
    fStream: TTwistedKnotStream;
  public
    constructor Create(Status: Integer; Address, UniqueID, StreamID: DWord);
    constructor Create(Status: Integer; Stream: TTwistedKnotStream);
    destructor Destroy; override;

    property Status: Integer read fStatus;
    property isReceive: Boolean read fReceive;
    property Address: DWord read fAddress;
    property UniqueID: DWord read fUniqueID;
    property StreamID: Dword read fStreamID;
    property Stream: TTwistedKnotStream read fStream;
  end;

  { TNetworkInterface }

  TNetworkInterface = class(TTwistedKnot)
  private
    fConnection: TTwistedKnotConnection;
    fKeepAlive: TThread;
    fLastUniqueID: DWord;
    fSection: TCriticalSection;
    fEventList: TObjectList;
    fStatus: TStringList;

    function getConnected: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Reconnect;
    procedure Close;
    procedure send(Data: Pointer; Len: Integer; UniqueID, toAddress:DWord);
    function querySendProgress(Address, StreamID: DWord): DWord;
    function getEvent: TNetworkEvent;

    function getUniqueID:DWord;

    procedure notify(Status: TTwistedKnotStatus); override;
    procedure sendStart(Address, UniqueID, StreamID: DWord); override;
    procedure sendCompleted(Sender: TTwistedKnotSender); override;
    function canReceive(Address, UniqueID, Length: Cardinal): Boolean; override;
    procedure receiveStart(Address, UniqueID, StreamID: DWord); override;
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

{ TNetworkEvent }

constructor TNetworkEvent.Create(Status: Integer; Address, UniqueID,
  StreamID: DWord);
begin
  inherited Create;

  fStatus := Status;
  fAddress := Address;
  fUniqueID := UniqueID;
  fStreamID := StreamID;
  fStream := nil;

  fReceive := (fStatus >= NetworkInterfaceReceiveStart);
end;

constructor TNetworkEvent.Create(Status: Integer; Stream: TTwistedKnotStream);
begin
  inherited Create;

  fStatus := Status;
  fAddress := Stream.Address;
  fUniqueID := Stream.UniqueID;
  fStreamID := Stream.StreamID;
  fStream := Stream;

  fReceive := (fStatus >= NetworkInterfaceReceiveStart);
end;

destructor TNetworkEvent.Destroy;
begin
  inherited Destroy;
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
  fLastUniqueID := 1;
  fSection := syncobjs.TCriticalSection.Create;
  fEventList := TObjectList.Create(False);
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
  fEventList.Free;
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

function TNetworkInterface.querySendProgress(Address, StreamID: DWord): DWord;
begin
  Result := fConnection.querySendProgress(Address, StreamID);
end;

function TNetworkInterface.getEvent: TNetworkEvent;
begin
  Result := nil;

  fSection.Acquire;
  if fEventList.Count > 0 then
  begin
    Result := fEventList[0] as TNetworkEvent;
    fEventList.Delete(0);
  end;
  fSection.Release;
end;

function TNetworkInterface.getUniqueID: DWord;
begin
  fSection.Acquire;
  fLastUniqueID := DWord(fLastUniqueID + 1);
  if fLastUniqueID = 0 then
    fLastUniqueID := 1;
  Result := fLastUniqueID;
  fSection.Release;
end;

procedure TNetworkInterface.notify(Status: TTwistedKnotStatus);
begin
  PostMessage(FormMain.Handle, NI_STATUS, Ord(Status), 0);
end;

procedure TNetworkInterface.sendStart(Address, UniqueID, StreamID: DWord);
var
  event: TNetworkEvent;
begin
  event := TNetworkEvent.Create(NetworkInterfaceSendStart, Address, UniqueID, StreamID);
  fSection.Acquire;
  fEventList.Add(event);
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

procedure TNetworkInterface.sendCompleted(Sender: TTwistedKnotSender);
var
  event: TNetworkEvent;
begin
  event := TNetworkEvent.Create(NetworkInterfaceSendComplete, Sender);
  fSection.Acquire;
  fEventList.Add(event);
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

function TNetworkInterface.canReceive(Address, UniqueID, Length: Cardinal
  ): Boolean;
begin
  Result := True;
end;

procedure TNetworkInterface.receiveStart(Address, UniqueID, StreamID: DWord);
var
  event: TNetworkEvent;
begin
  event := TNetworkEvent.Create(NetworkInterfaceReceiveStart, Address, UniqueID, StreamID);
  fSection.Acquire;
  fEventList.Add(event);
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

procedure TNetworkInterface.receiveCompleted(Receiver: TTwistedKnotReceiver);
var
  event: TNetworkEvent;
begin
  event := TNetworkEvent.Create(NetworkInterfaceReceiveComplete, Receiver);
  fSection.Acquire;
  fEventList.Add(event);
  fSection.Release;
  PostMessage(FormMain.Handle, NI_EVENT, 0, 0);
end;

end.

