unit NetworkInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, Contnrs, TwistedKnot, LCLIntf, LMessages;

const
  NI_RECEIVE = LM_USER + 101;
  NI_STATUS = NI_RECEIVE + 1;

type

  { TNetworkInterface }

  TNetworkInterface = class(TTwistedKnot)
  private
    fConnection: TTwistedKnotConnection;
    fLastUniqueID: DWord;
    fSection: TCriticalSection;
    fReceivers: TObjectList;

    function getConnected: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect;
    procedure Reconnect;
    procedure Close;
    procedure send(Data: Pointer; Len: Integer; UniqueID, toAddress:DWord);
    function getMessage:String;

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
  main;

const
  PublicKey =
    'MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDELgZ/OqvlVGkWOPrRKB26IOss' +
    'jHeRHKaCaGiaw8pJ7GATrileaqntWukseWGRIsO6CnjosWUcH0NOW4BGjCq4maXr' +
    'LH8N4EzXKJoFuGKdELDUfEZdozHnAOKY2IiFAKjPSAeUL2Rd5tg7hbIwum0JV9yy' +
    'mFz7gnP4jKQjCFunBQIDAQAB';
  ServerHost = 'lookandwalk.com';
  //ServerHost = '10.0.2.2';
  ServerPort = 4430;

{ TNetworkInterface }

function TNetworkInterface.getConnected: Boolean;
begin
  Result := fConnection.Connected and fConnection.Handshaked;
end;

constructor TNetworkInterface.Create;
begin
  fLastUniqueID := 0;
  fSection := syncobjs.TCriticalSection.Create;
  fReceivers := TObjectList.Create(False);

  fConnection := TTwistedKnotConnection.Create;
  fConnection.PublicKey := PublicKey;
  fConnection.Address := ServerHost;
  fConnection.Port := ServerPort;
  fConnection.DefaultHandler := self;
  fConnection.Start;
end;

destructor TNetworkInterface.Destroy;
begin
  fConnection.DefaultHandler := nil;
  fConnection.Terminate;
  fConnection.Close;
  fConnection := nil;

  fSection.Free;
  fReceivers.Free;
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
  fConnection.Send(Data, Len, UniqueID, toAddress, nil);
end;

function TNetworkInterface.getMessage: String;
var
  Receiver: TTwistedKnotReceiver;
  data: String;
begin
  Result := '';
  Receiver := nil;

  fSection.Acquire;
  if fReceivers.Count > 0 then
  begin
    Receiver := TTwistedKnotReceiver(fReceivers.Items[0]);
    fReceivers.Delete(0);
  end;
  fSection.Release;

  if Receiver = nil then
    exit;

  SetLength(Result, Receiver.Length);
  Move(Receiver.Buffer^, PChar(Result)^, Receiver.Length);
  Receiver.Free;
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

end;

procedure TNetworkInterface.sendCompleted(Sender: TTwistedKnotSender);
begin
  Sender.Free;
end;

function TNetworkInterface.canReceive(UniqueID, From, Length: Cardinal
  ): Boolean;
begin
  Result := True;
end;

procedure TNetworkInterface.receiveStart(Receiver: TTwistedKnotReceiver);
begin

end;

procedure TNetworkInterface.receiveCompleted(Receiver: TTwistedKnotReceiver);
begin
  fSection.Acquire;
  fReceivers.Add(Receiver);
  fSection.Release;
  PostMessage(FormMain.Handle, NI_RECEIVE, 0, 0);
end;

end.

