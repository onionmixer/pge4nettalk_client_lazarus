{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcHTTPServer.pas                                        }
{   File version:     5.06                                                     }
{   Description:      HTTP server.                                             }
{                                                                              }
{   Copyright:        Copyright (c) 2011-2016, David J Butler                  }
{                     All rights reserved.                                     }
{                     This file is licensed under the BSD License.             }
{                     See http://www.opensource.org/licenses/bsd-license.php   }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2011/05/29  0.01  Initial development.                                     }
{   2011/06/13  0.02  Further development.                                     }
{   2011/06/21  0.03  Request and response flow.                               }
{   2011/06/25  0.04  HTTPS.                                                   }
{   2015/03/12  0.05  Improvements.                                            }
{   2016/01/09  5.06  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTTP.inc}

unit flcHTTPServer;

interface

uses
  { System }
  SysUtils,
  Classes,
  SyncObjs,
  { Fundamentals }
  flcUtils,
  flcStrings,
  flcSocketLib,
  { TCP }
  flcTCPConnection,
  flcTCPServer,
  { HTTP }
  flcHTTPUtils;



{                                                                              }
{ THTTP4Server                                                                 }
{                                                                              }
const
  HTTP_SERVER_DEFAULT_MaxBacklog = 8;
  HTTP_SERVER_DEFAULT_MaxClients = -1;

type
  THTTPServerLogType = (
    sltDebug,
    sltInfo,
    sltError);

  THTTPServerAddressFamily = (
    safIP4,
    safIP6);

  {$IFDEF HTTP_TLS}
  THTTPSServerOption = (
    ssoDontUseSSL3,
    ssoDontUseTLS10,
    ssoDontUseTLS11,
    ssoDontUseTLS12);

  THTTPSServerOptions = set of THTTPSServerOption;
  {$ENDIF}

  TF5HTTPServer = class;

  THTTPServerClientState = (
    hscsInit,
    hscsAwaitingRequest,
    hscsReceivedRequestHeader,
    hscsReceivingContent,
    hscsRequestComplete,
    hscsPreparingResponse,
    hscsAwaitingPreparedResponse,
    hscsSendingResponseHeader,
    hscsSendingContent,
    hscsResponseComplete,
    hscsResponseCompleteAndClosing,
    hscsResponseCompleteAndClosed,
    hscsRequestInterruptedAndClosed);

  THTTPServerClient = class
  private
    // parameters
    FHTTPServer : TF5HTTPServer;
    FTCPClient  : TTCPServerClient;

    // state
    FLock       : TCriticalSection;
    FState      : THTTPServerClientState;
    FHTTPParser : THTTPParser;

    FRequest              : THTTPRequest;
    FRequestContentReader : THTTPContentReader;

    FResponse              : THTTPResponse;
    FResponseContentWriter : THTTPContentWriter;
    FResponseReady         : Boolean;

    procedure Init;

    procedure Log(const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: THTTPServerLogType; const Msg: String; const Args: array of const; const LogLevel: Integer = 0); overload;

    procedure Lock;
    procedure Unlock;

    function  GetState: THTTPServerClientState;
    function  GetStateStr: RawByteString;
    procedure SetState(const State: THTTPServerClientState);

    function  GetRemoteAddr: TSocketAddr;
    function  GetRemoteAddrStr: RawByteString;

    procedure TriggerStateChanged;
    procedure TriggerRequestHeader;
    procedure TriggerRequestContentBuffer(const Buf; const Size: Integer);
    procedure TriggerRequestContentComplete;
    procedure TriggerPrepareResponse;
    procedure TriggerResponseComplete;

    procedure TCPClientStateChange;
    procedure TCPClientRead;
    procedure TCPClientWrite;
    procedure TCPClientClose;

    procedure ContentReaderLog(const Sender: THTTPContentReader; const LogMsg: String; const LogLevel: Integer);
    function  ContentReaderReadProc(const Sender: THTTPContentReader; var Buf; const Size: Integer): Integer;
    procedure ContentReaderContentProc(const Sender: THTTPContentReader; const Buf; const Size: Integer);
    procedure ContentReaderContentCompleteProc(const Sender: THTTPContentReader);

    procedure ContentWriterLog(const Sender: THTTPContentWriter; const LogMsg: String);
    function  ContentWriterWriteProc(const Sender: THTTPContentWriter; const Buf; const Size: Integer): Integer;

    procedure SendStr(const S: RawByteString);

    procedure Start;

    procedure ClearResponse;

    procedure ReadRequestHeader;
    procedure ProcessRequestHeader;

    procedure InitRequestContent;
    procedure ReadRequestContent;
    procedure FinaliseRequestContent;

    procedure SetRequestComplete;

    procedure InitResponse;
    procedure PrepareResponse;
    procedure InitResponseContent;
    procedure SendResponseContent;
    procedure ResponsePrepared;
    procedure SendResponse;
    procedure SetResponseComplete;

    function  GetRequestMethod: RawByteString;
    function  GetRequestURI: RawByteString;
    function  GetRequestHost: RawByteString;
    function  GetRequestCookie: RawByteString;
    function  GetRequestHasContent: Boolean;
    function  GetRequestContentType: RawByteString;

    function  GetRequestRecordPtr: PHTTPRequest;

    function  GetResponseCode: Integer;
    procedure SetResponseCode(const ResponseCode: Integer);
    function  GetResponseMsg: RawByteString;
    procedure SetResponseMsg(const ResponseMsg: RawByteString);
    function  GetResponseContentType: RawByteString;
    procedure SetResponseContentType(const ResponseContentType: RawByteString);

    function  GetResponseRecordPtr: PHTTPResponse;

    function  GetRequestContentStream: TStream;
    procedure SetRequestContentStream(const RequestContentStream: TStream);
    function  GetRequestContentFileName: String;
    procedure SetRequestContentFileName(const RequestContentFileName: String);
    function  GetRequestContentStr: RawByteString;
    function  GetRequestContentReceivedSize: Int64;

    function  GetResponseContentMechanism: THTTPContentWriterMechanism;
    procedure SetResponseContentMechanism(const ResponseContentMechanism: THTTPContentWriterMechanism);
    function  GetResponseContentStr: RawByteString;
    procedure SetResponseContentStr(const ResponseContentStr: RawByteString);
    function  GetResponseContentStream: TStream;
    procedure SetResponseContentStream(const ResponseContentStream: TStream);
    function  GetResponseContentFileName: String;
    procedure SetResponseContentFileName(const ResponseContentFileName: String);
    procedure SetResponseReady(const ResponseReady: Boolean);

  public
    constructor Create(
                const HTTPServer: TF5HTTPServer;
                const TCPClient: TTCPServerClient);
    destructor Destroy; override;

    property  State: THTTPServerClientState read GetState;
    property  StateStr: RawByteString read GetStateStr;

    property  RemoteAddr: TSocketAddr read GetRemoteAddr;
    property  RemoteAddrStr: RawByteString read GetRemoteAddrStr;

    property  RequestRecord: THTTPRequest read FRequest;
    property  RequestRecordPtr: PHTTPRequest read GetRequestRecordPtr;

    property  RequestMethod: RawByteString read GetRequestMethod;
    property  RequestURI: RawByteString read GetRequestURI;
    property  RequestHost: RawByteString read GetRequestHost;
    property  RequestCookie: RawByteString read GetRequestCookie;
    property  RequestHasContent: Boolean read GetRequestHasContent;
    property  RequestContentType: RawByteString read GetRequestContentType;
    property  RequestContentStr: RawByteString read GetRequestContentStr;
    property  RequestContentStream: TStream read GetRequestContentStream write SetRequestContentStream;
    property  RequestContentFileName: String read GetRequestContentFileName write SetRequestContentFileName;
    property  RequestContentReceivedSize: Int64 read GetRequestContentReceivedSize;

    property  ResponseRecord: THTTPResponse read FResponse write FResponse;
    property  ResponseRecordPtr: PHTTPResponse read GetResponseRecordPtr;

    property  ResponseCode: Integer read GetResponseCode write SetResponseCode;
    property  ResponseMsg: RawByteString read GetResponseMsg write SetResponseMsg;

    property  ResponseContentType: RawByteString read GetResponseContentType write SetResponseContentType;
    property  ResponseContentMechanism: THTTPContentWriterMechanism read GetResponseContentMechanism write SetResponseContentMechanism;
    property  ResponseContentStr: RawByteString read GetResponseContentStr write SetResponseContentStr;
    property  ResponseContentStream: TStream read GetResponseContentStream write SetResponseContentStream;
    property  ResponseContentFileName: String read GetResponseContentFileName write SetResponseContentFileName;
    property  ResponseReady: Boolean read FResponseReady write SetResponseReady;

    procedure SetResponseOKHtmlStr(const HtmlStr: RawByteString);
    procedure SetResponseOKFile(const ContentType: THTTPContentTypeEnum;
              const FileName: String);
    procedure SetResponseNotFound;
    procedure SetResponseRedirect(const Location: RawByteString);

    procedure Disconnect;
  end;

  THTTPServerEvent = procedure (Server: TF5HTTPServer) of object;
  THTTPServerLogEvent = procedure (Server: TF5HTTPServer; LogType: THTTPServerLogType; Msg: String; LogLevel: Integer) of object;
  THTTPServerClientEvent = procedure (Server: TF5HTTPServer; Client: THTTPServerClient) of object;
  THTTPServerClientContentEvent = procedure (Server: TF5HTTPServer; Client: THTTPServerClient; const Buf; const Size: Integer) of object;

  TF5HTTPServer = class(TComponent)
  protected
    // events
    FOnLog              : THTTPServerLogEvent;
    FOnStart            : THTTPServerEvent;
    FOnStop             : THTTPServerEvent;
    FOnActive           : THTTPServerEvent;
    FOnInactive         : THTTPServerEvent;
    FOnRequestHeader    : THTTPServerClientEvent;
    FOnRequestContent   : THTTPServerClientContentEvent;
    FOnRequestComplete  : THTTPServerClientEvent;
    FOnPrepareResponse  : THTTPServerClientEvent;
    FOnResponseComplete : THTTPServerClientEvent;

    // parameters
    FAddressFamily  : THTTPServerAddressFamily;
    FBindAddressStr : AnsiString;
    FServerPort     : Integer;
    FMaxBacklog     : Integer;
    FMaxClients     : Integer;
    FServerName     : RawByteString;

    {$IFDEF HTTP_TLS}
    FHTTPSEnabled : Boolean;
    FHTTPSOptions : THTTPSServerOptions;
    {$ENDIF}

    FRequestContentMechanism  : THTTPContentReaderMechanism;
    FResponseContentMechanism : THTTPContentWriterMechanism;

    FUserObject : TObject;
    FUserData   : Pointer;
    FUserTag    : NativeInt;

    // state
    FLock             : TCriticalSection;
    FActive           : Boolean;
    FActivateOnLoaded : Boolean;
    FTCPServer        : TF5TCPServer;

    procedure Init; virtual;
    procedure InitTCPServer;
    procedure InitDefaults; virtual;

    procedure Loaded; override;

    procedure Log(const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer = 0); overload;
    procedure Log(const LogType: THTTPServerLogType; const Msg: String; const Args: array of const; const LogLevel: Integer = 0); overload;

    procedure Lock;
    procedure Unlock;

    procedure CheckNotActive;

    procedure SetAddressFamily(const AddressFamily: THTTPServerAddressFamily);
    procedure SetBindAddress(const BindAddressStr: AnsiString);
    procedure SetServerPort(const ServerPort: Integer);
    procedure SetMaxBacklog(const MaxBacklog: Integer);
    procedure SetMaxClients(const MaxClients: Integer);

    procedure SetServerName(const ServerName: RawByteString);

    {$IFDEF HTTP_TLS}
    procedure SetHTTPSEnabled(const HTTPSEnabled: Boolean);
    procedure SetHTTPSOptions(const HTTPSOptions: THTTPSServerOptions);
    {$ENDIF}

    procedure SetRequestContentMechanism(const RequestContentMechanism: THTTPContentReaderMechanism);

    procedure TriggerStart;
    procedure TriggerStop;
    procedure TriggerActive;
    procedure TriggerInactive;
    procedure TriggerRequestHeader(const Client: THTTPServerClient);
    procedure TriggerRequestContent(const Client: THTTPServerClient; const Buf; const Size: Integer);
    procedure TriggerRequestComplete(const Client: THTTPServerClient);
    procedure TriggerPrepareResponse(const Client: THTTPServerClient);
    procedure TriggerResponseComplete(const Client: THTTPServerClient);

    procedure TCPServerLog(Sender: TF5TCPServer; LogType: TTCPLogType; Msg: String; LogLevel: Integer);
    procedure TCPServerStateChanged(Sender: TF5TCPServer; State: TTCPServerState);
    procedure TCPServerClientAccept(Sender: TF5TCPServer; Address: TSocketAddr;
              var AcceptClient: Boolean);
    procedure TCPServerClientCreate(Sender: TTCPServerClient);
    procedure TCPServerClientAdd(Sender: TTCPServerClient);
    procedure TCPServerClientRemove(Sender: TTCPServerClient);
    procedure TCPServerClientStateChange(Sender: TTCPServerClient);
    procedure TCPServerClientRead(Sender: TTCPServerClient);
    procedure TCPServerClientWrite(Sender: TTCPServerClient);
    procedure TCPServerClientClose(Sender: TTCPServerClient);

    procedure ClientLog(const Client: THTTPServerClient; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
    procedure ClientStateChanged(const Client: THTTPServerClient);
    procedure ClientRequestHeader(const Client: THTTPServerClient);
    procedure ClientRequestContentBuffer(const Client: THTTPServerClient; const Buf; const Size: Integer);
    procedure ClientRequestContentComplete(const Client: THTTPServerClient);
    procedure ClientPrepareResponse(const Client: THTTPServerClient);
    procedure ClientResponseComplete(const Client: THTTPServerClient);

    procedure SetupTCPServer;

    procedure DoStart;
    procedure DoStop;
    procedure SetActive(const Active: Boolean);

    function  GetClientCount: Integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property  OnLog: THTTPServerLogEvent read FOnLog write FOnLog;

    property  OnStart: THTTPServerEvent read FOnStart write FOnStart;
    property  OnStop: THTTPServerEvent read FOnStop write FOnStop;
    property  OnActive: THTTPServerEvent read FOnActive write FOnActive;
    property  OnInactive: THTTPServerEvent read FOnInactive write FOnInactive;

    property  OnRequestHeader: THTTPServerClientEvent read FOnRequestHeader write FOnRequestHeader;
    property  OnRequestContent: THTTPServerClientContentEvent read FOnRequestContent write FOnRequestContent;
    property  OnRequestComplete: THTTPServerClientEvent read FOnRequestComplete write FOnRequestComplete;
    property  OnPrepareResponse: THTTPServerClientEvent read FOnPrepareResponse write FOnPrepareResponse;
    property  OnResponseComplete: THTTPServerClientEvent read FOnResponseComplete write FOnResponseComplete;

    property  AddressFamily: THTTPServerAddressFamily read FAddressFamily write SetAddressFamily default safIP4;
    property  BindAddress: AnsiString read FBindAddressStr write SetBindAddress;
    property  ServerPort: Integer read FServerPort write SetServerPort;
    property  MaxBacklog: Integer read FMaxBacklog write SetMaxBacklog default HTTP_SERVER_DEFAULT_MaxBacklog;
    property  MaxClients: Integer read FMaxClients write SetMaxClients default HTTP_SERVER_DEFAULT_MaxClients;

    property  ServerName: RawByteString read FServerName write SetServerName;
    
    {$IFDEF HTTP_TLS}
    property  HTTPSEnabled: Boolean read FHTTPSEnabled write SetHTTPSEnabled default False;
    property  HTTPSOptions: THTTPSServerOptions read FHTTPSOptions write SetHTTPSOptions default [];
    {$ENDIF}

    property  RequestContentMechanism: THTTPContentReaderMechanism read FRequestContentMechanism write SetRequestContentMechanism default hcrmEvent;

    property  Active: Boolean read FActive write SetActive default False;

    property  TCPServer: TF5TCPServer read FTCPServer;
    
    property  ClientCount: Integer read GetClientCount;

    property  UserObject: TObject read FUserObject write FUserObject;
    property  UserData: Pointer read FUserData write FUserData;
    property  UserTag: NativeInt read FUserTag write FUserTag;
  end;

  EHTTPServer = class(Exception);



{                                                                              }
{ Component                                                                    }
{                                                                              }
type
  TfclHTTPServer = class(TF5HTTPServer)
  published
    property  OnLog;

    property  OnStart;
    property  OnStop;
    property  OnActive;
    property  OnInactive;

    property  OnRequestHeader;
    property  OnRequestContent;
    property  OnRequestComplete;
    property  OnPrepareResponse;
    property  OnResponseComplete;

    property  AddressFamily;
    property  BindAddress;
    property  ServerPort;
    property  MaxBacklog;
    property  MaxClients;

    property  ServerName;

    {$IFDEF HTTP_TLS}
    property  HTTPSEnabled;
    property  HTTPSOptions;
    {$ENDIF}

    property  RequestContentMechanism;

    property  Active;
  end;

{$IFDEF HTTPSERVER_CUSTOM}
  {$INCLUDE cHTTPServerIntf.inc}
{$ENDIF}



implementation

{$IFDEF HTTP_TLS}
uses
  {$IFDEF HTTPSERVER_CUSTOM}
    {$INCLUDE cHTTPServerUses.inc}
  {$ENDIF}
  flcTLSServer;
{$ENDIF}



{                                                                              }
{ HTTP Server constants                                                        }
{                                                                              }
const
  HTTPSERVER_PORT     = 80;
  HTTPSERVER_PORT_STR = '80';

  HTTPSERVER_RequestHeader_MaxSize  = 16384;
  HTTPSERVER_RequestHeader_Delim    = #13#10#13#10;
  HTTPSERVER_RequestHeader_DelimLen = Length(HTTPSERVER_RequestHeader_Delim);



{                                                                              }
{ Errors and debug strings                                                     }
{                                                                              }
const
  SError_NotAllowedWhileActive = 'Operation not allowed while active';

const
  SClientState : array[THTTPServerClientState] of RawByteString = (
      'Initialise',
      'AwaitingRequest',
      'ReceivedRequestHeader',
      'ReceivingContent',
      'RequestComplete',
      'PreparingResponse',
      'AwaitingPreparedResponse',
      'SendingResponseHeader',
      'SendingContent',
      'ResponseComplete',
      'ResponseCompleteAndClosing',
      'ResponseCompleteAndClosed',
      'RequestInterruptedAndClosed');



{                                                                              }
{ THTTPServerClient                                                            }
{                                                                              }
constructor THTTPServerClient.Create(const HTTPServer: TF5HTTPServer; const TCPClient: TTCPServerClient);
begin
  Assert(Assigned(HTTPServer));
  Assert(Assigned(TCPClient));
  //
  inherited Create;
  FHTTPServer := HTTPServer;
  FTCPClient := TCPClient;
  Init;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Initialised');
  {$ENDIF}
end;

procedure THTTPServerClient.Init;
begin
  FLock := TCriticalSection.Create;
  FState := hscsInit;

  InitHTTPRequest(FRequest);
  InitHTTPResponse(FResponse);

  FHTTPParser := THTTPParser.Create;

  FRequestContentReader := THTTPContentReader.Create(
      ContentReaderReadProc,
      ContentReaderContentProc,
      ContentReaderContentCompleteProc);
  FRequestContentReader.OnLog := ContentReaderLog;
  FRequestContentReader.Mechanism := FHTTPServer.FRequestContentMechanism;

  FResponseContentWriter := THTTPContentWriter.Create(ContentWriterWriteProc);
  FResponseContentWriter.OnLog := ContentWriterLog;
  FResponseContentWriter.Mechanism := hctmNone;
end;

destructor THTTPServerClient.Destroy;
begin
  FreeAndNil(FResponseContentWriter);
  FreeAndNil(FRequestContentReader);
  FreeAndNil(FHTTPParser);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure THTTPServerClient.Log(const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
begin
  if Assigned(FHTTPServer) then
    FHTTPServer.ClientLog(self, LogType, Msg, LogLevel);
end;

procedure THTTPServerClient.Log(const LogType: THTTPServerLogType; const Msg: String; const Args: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(Msg, Args), LogLevel);
end;

procedure THTTPServerClient.Lock;
begin
  if Assigned(FLock) then
    FLock.Acquire;
end;

procedure THTTPServerClient.Unlock;
begin
  if Assigned(FLock) then
    FLock.Release;
end;

function THTTPServerClient.GetState: THTTPServerClientState;
begin
  Lock;
  try
    Result := FState;
  finally
    Unlock;
  end;
end;

function THTTPServerClient.GetStateStr: RawByteString;
begin
  Result := SClientState[GetState];
end;

procedure THTTPServerClient.SetState(const State: THTTPServerClientState);
begin
  Lock;
  try
    Assert(State <> FState);
    FState := State;
  finally
    Unlock;
  end;
  TriggerStateChanged;
end;

function THTTPServerClient.GetRemoteAddr: TSocketAddr;
begin
  Result := FTCPClient.RemoteAddr;
end;

function THTTPServerClient.GetRemoteAddrStr: RawByteString;
begin
  Result := FTCPClient.RemoteAddrStr;
end;

procedure THTTPServerClient.TriggerStateChanged;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'State:%s', [GetStateStr]);
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientStateChanged(self);
end;

procedure THTTPServerClient.TriggerRequestHeader;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'RequestHeader:');
  Log(sltDebug, String(HTTPRequestToStr(FRequest)));
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientRequestHeader(self);
end;

procedure THTTPServerClient.TriggerRequestContentBuffer(const Buf; const Size: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'RequestContentBuffer');
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientRequestContentBuffer(self, Buf, Size);
end;

procedure THTTPServerClient.TriggerRequestContentComplete;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'RequestContentComplete');
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientRequestContentComplete(self);
end;

procedure THTTPServerClient.TriggerPrepareResponse;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'PrepareResponse');
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientPrepareResponse(self);
end;

procedure THTTPServerClient.TriggerResponseComplete;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ResponseComplete');
  {$ENDIF}
  Assert(Assigned(FHTTPServer));
  FHTTPServer.ClientResponseComplete(self);
end;

procedure THTTPServerClient.TCPClientStateChange;
begin
end;

procedure THTTPServerClient.TCPClientRead;
begin
  Assert(FState in [
      hscsInit,                            // ??
      hscsAwaitingRequest,
      hscsReceivingContent,
      hscsResponseComplete,
      hscsResponseCompleteAndClosing,      // ??
      hscsResponseCompleteAndClosed]);     // ??
  if FState = hscsResponseComplete then
    SetState(hscsAwaitingRequest);
  if FState = hscsAwaitingRequest then
    ReadRequestHeader;
  if FState = hscsReceivedRequestHeader then
    if FRequest.HasContent then
      begin
        InitRequestContent;
        SetState(hscsReceivingContent);
      end
    else
      SetRequestComplete;
  if FState = hscsReceivingContent then
    ReadRequestContent;
end;

procedure THTTPServerClient.TCPClientWrite;
begin
end;

procedure THTTPServerClient.TCPClientClose;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPClient_Close');
  {$ENDIF}
  if FState in [hscsInit,
                hscsResponseCompleteAndClosed,
                hscsRequestInterruptedAndClosed] then
    exit;
  if FState in [hscsResponseComplete,
                hscsResponseCompleteAndClosing] then
    SetState(hscsResponseCompleteAndClosed);
  if FState in [hscsAwaitingRequest,
                hscsReceivedRequestHeader,
                hscsReceivingContent,
                hscsRequestComplete,
                hscsPreparingResponse,
                hscsSendingResponseHeader,
                hscsSendingContent] then
    SetState(hscsRequestInterruptedAndClosed);
end;

procedure THTTPServerClient.ContentReaderLog(const Sender: THTTPContentReader; const LogMsg: String; const LogLevel: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, Format('ContentReader:%s', [LogMsg]), LogLevel + 1);
  {$ENDIF}
end;

function THTTPServerClient.ContentReaderReadProc(const Sender: THTTPContentReader;
    var Buf; const Size: Integer): Integer;
begin
  Assert(Assigned(FTCPClient));
  Assert(FState in [hscsReceivingContent]);
  //
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ContentReader_Read');
  {$ENDIF}
  Result := FTCPClient.Connection.Read(Buf, Size);
end;

procedure THTTPServerClient.ContentReaderContentProc(const Sender: THTTPContentReader;
    const Buf; const Size: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ContentReader_Content');
  {$ENDIF}
  TriggerRequestContentBuffer(Buf, Size);
end;

procedure THTTPServerClient.ContentReaderContentCompleteProc(const Sender: THTTPContentReader);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ContentReader_ContentComplete');
  {$ENDIF}
  FinaliseRequestContent;
  SetRequestComplete;
end;

procedure THTTPServerClient.ContentWriterLog(const Sender: THTTPContentWriter;
          const LogMsg: String);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, Format('ContentWriter:%s', [LogMsg]), 1);
  {$ENDIF}
end;

function THTTPServerClient.ContentWriterWriteProc(const Sender: THTTPContentWriter;
         const Buf; const Size: Integer): Integer;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ContentWriter_Write');
  {$ENDIF}
  Result := FTCPClient.Connection.Write(Buf, Size);
end;

procedure THTTPServerClient.SendStr(const S: RawByteString);
begin
  Assert(Assigned(FTCPClient));
  FTCPClient.Connection.WriteStrB(S);
end;

procedure THTTPServerClient.Start;
begin
  Assert(FState = hscsInit);
  SetState(hscsAwaitingRequest);
end;

procedure THTTPServerClient.ClearResponse;
begin
  ClearHTTPResponse(FResponse);
  FResponseReady := False;
  FResponseContentWriter.Clear;
  FResponseContentWriter.Mechanism := hctmNone;
end;

procedure THTTPServerClient.ReadRequestHeader;
const
  HdrBufSize = HTTPSERVER_RequestHeader_MaxSize + HTTPSERVER_RequestHeader_DelimLen;
var
  HdrBuf : array[0..HdrBufSize - 1] of Byte;
  HdrLen : Integer;
begin
  Assert(Assigned(FTCPClient));
  Assert(FState in [hscsAwaitingRequest]);
  //
  HdrLen := FTCPClient.Connection.PeekDelimited(
      HdrBuf[0], HdrBufSize,
      HTTPSERVER_RequestHeader_Delim,
      HTTPSERVER_RequestHeader_MaxSize);
  if HdrLen < 0 then
    exit;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'RequestHeader:%db', [HdrLen]);
  {$ENDIF}
  ClearHTTPRequest(FRequest);
  FHTTPParser.SetTextBuf(HdrBuf[0], HdrLen);
  FHTTPParser.ParseRequest(FRequest);
  if not FRequest.HeaderComplete then
    begin
      {$IFDEF HTTP_DEBUG}
      Log(sltDebug, 'RequestHeader:BadFormat:ClosingConnection');
      {$ENDIF}
      FTCPClient.Close;
      exit;
    end;
  FTCPClient.Connection.Discard(HdrLen);
  ClearResponse;
  ProcessRequestHeader;
  SetState(hscsReceivedRequestHeader);
  TriggerRequestHeader;
end;

procedure THTTPServerClient.ProcessRequestHeader;
begin
end;

procedure THTTPServerClient.InitRequestContent;
begin
  FRequestContentReader.InitReader(FRequest.Header.CommonHeaders);
end;

procedure THTTPServerClient.ReadRequestContent;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ReadRequestContent');
  {$ENDIF}
  FRequestContentReader.Process;
end;

procedure THTTPServerClient.FinaliseRequestContent;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'FinaliseRequestContent');
  {$ENDIF}
end;

procedure THTTPServerClient.SetRequestComplete;
begin
  SetState(hscsRequestComplete);
  InitResponse;
  TriggerRequestContentComplete;
  SetState(hscsPreparingResponse);
  PrepareResponse;
  if not FResponseReady then
    begin
      SetState(hscsAwaitingPreparedResponse);
      exit;
    end;
  ResponsePrepared;
end;

procedure THTTPServerClient.InitResponse;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'InitResponse');
  {$ENDIF}
  FResponse.StartLine.Version := FRequest.StartLine.Version;
  if FRequest.StartLine.Version.Version = hvHTTP11 then
    case FRequest.Header.CommonHeaders.Connection.Value of
      hcfClose     : FResponse.Header.CommonHeaders.Connection.Value := hcfClose;
      hcfKeepAlive : FResponse.Header.CommonHeaders.Connection.Value := hcfKeepAlive;
    end;
  FResponse.Header.CommonHeaders.Date.Value := hdDateTime;
  FResponse.Header.CommonHeaders.Date.DateTime := Now;
  if FHTTPServer.FServerName <> '' then
    FResponse.Header.FixedHeaders[hntServer] := FHTTPServer.FServerName;
end;

procedure THTTPServerClient.PrepareResponse;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'PrepareResponse');
  {$ENDIF}
  TriggerPrepareResponse;
  if FResponse.StartLine.Msg = hslmNone then
    FResponse.StartLine.Msg := HTTPResponseCodeToStartLineMessage(FResponse.StartLine.Code);
end;

procedure THTTPServerClient.InitResponseContent;
var HasContent : Boolean;
    ContentLen : Int64;
    B : Int64;
begin
  FResponseContentWriter.InitContent(HasContent, ContentLen);
  if not HasContent then
    B := 0
  else
    B := ContentLen;
  FResponse.Header.CommonHeaders.ContentLength.Value := hcltByteCount;
  FResponse.Header.CommonHeaders.ContentLength.ByteCount := B;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, Format('InitResponseContent:%d:%db:%db', [Ord(HasContent), ContentLen, B]));
  {$ENDIF}
end;

procedure THTTPServerClient.SendResponseContent;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'SendResponseContent');
  {$ENDIF}
  FResponseContentWriter.SendContent;
end;

procedure THTTPServerClient.ResponsePrepared;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ResponsePrepared');
  {$ENDIF}
  SendResponse;
end;

procedure THTTPServerClient.SendResponse;
var ResponseHdr : RawByteString;
begin
  InitResponseContent;
  SetState(hscsSendingResponseHeader);
  ResponseHdr := HTTPResponseToStr(FResponse);
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ResponseHeader:');
  Log(sltDebug, String(ResponseHdr));
  {$ENDIF}
  SendStr(HTTPResponseToStr(FResponse));
  SetState(hscsSendingContent);
  SendResponseContent;
  if not FResponseContentWriter.ContentComplete then
    exit;
  SetResponseComplete;
end;

procedure THTTPServerClient.SetResponseComplete;
begin
  Assert(FState = hscsSendingContent);
  SetState(hscsResponseComplete);
  TriggerResponseComplete;
  if (FRequest.StartLine.Version.Version = hvHTTP10) or
     (FRequest.Header.CommonHeaders.Connection.Value = hcfClose) or
     (FResponse.Header.CommonHeaders.Connection.Value = hcfClose) then
    begin
      {$IFDEF HTTP_DEBUG}
      Log(sltDebug, 'SetResponseComplete:ConnectionClose');
      {$ENDIF}
      FTCPClient.Connection.Shutdown;
      SetState(hscsResponseCompleteAndClosing);
    end;
end;

function THTTPServerClient.GetRequestMethod: RawByteString;
begin
  Result := HTTPMethodToStr(FRequest.StartLine.Method);
end;

function THTTPServerClient.GetRequestURI: RawByteString;
begin
  Result := FRequest.StartLine.URI;
end;

function THTTPServerClient.GetRequestHost: RawByteString;
begin
  Result := FRequest.Header.FixedHeaders[hntHost];
end;

function THTTPServerClient.GetRequestCookie: RawByteString;
begin
  Result := HTTPCookieFieldValueToStr(FRequest.Header.Cookie);
end;

function THTTPServerClient.GetRequestHasContent: Boolean;
begin
  Result := FRequest.HasContent;
end;

function THTTPServerClient.GetRequestContentType: RawByteString;
begin
  Result := HTTPContentTypeValueToStr(FRequest.Header.CommonHeaders.ContentType);
end;

function THTTPServerClient.GetRequestRecordPtr: PHTTPRequest;
begin
  Result := @FRequest;
end;

function THTTPServerClient.GetResponseCode: Integer;
begin
  Result := FResponse.StartLine.Code;
end;

procedure THTTPServerClient.SetResponseCode(const ResponseCode: Integer);
begin
  FResponse.StartLine.Code := ResponseCode;
end;

function THTTPServerClient.GetResponseMsg: RawByteString;
begin
  Result := FResponse.StartLine.CustomMsg;
end;

procedure THTTPServerClient.SetResponseMsg(const ResponseMsg: RawByteString);
begin
  FResponse.StartLine.Msg := hslmCustom;
  FResponse.StartLine.CustomMsg := ResponseMsg;
end;

function THTTPServerClient.GetResponseContentType: RawByteString;
begin
  Result := HTTPContentTypeValueToStr(FResponse.Header.CommonHeaders.ContentType);
end;

procedure THTTPServerClient.SetResponseContentType(const ResponseContentType: RawByteString);
begin
  FResponse.Header.CommonHeaders.ContentType.Value := hctCustomString;
  FResponse.Header.CommonHeaders.ContentType.CustomStr := ResponseContentType;
end;

function THTTPServerClient.GetResponseRecordPtr: PHTTPResponse;
begin
  Result := @FResponse;
end;

function THTTPServerClient.GetRequestContentStream: TStream;
begin
  Result := FRequestContentReader.ContentStream;
end;

procedure THTTPServerClient.SetRequestContentStream(const RequestContentStream: TStream);
begin
  FRequestContentReader.ContentStream := RequestContentStream;
end;

function THTTPServerClient.GetRequestContentFileName: String;
begin
  Result := FRequestContentReader.ContentFileName;
end;

procedure THTTPServerClient.SetRequestContentFileName(const RequestContentFileName: String);
begin
  FRequestContentReader.ContentFileName := RequestContentFileName;
end;

function THTTPServerClient.GetRequestContentStr: RawByteString;
begin
  Result := FRequestContentReader.ContentString;
end;

function THTTPServerClient.GetRequestContentReceivedSize: Int64;
begin
  Result := FRequestContentReader.ContentReceivedSize;
end;

function THTTPServerClient.GetResponseContentMechanism: THTTPContentWriterMechanism;
begin
  Result := FResponseContentWriter.Mechanism;
end;

procedure THTTPServerClient.SetResponseContentMechanism(const ResponseContentMechanism: THTTPContentWriterMechanism);
begin
  FResponseContentWriter.Mechanism := ResponseContentMechanism;
end;

function THTTPServerClient.GetResponseContentStr: RawByteString;
begin
  Result := FResponseContentWriter.ContentString;
end;

procedure THTTPServerClient.SetResponseContentStr(const ResponseContentStr: RawByteString);
begin
  FResponseContentWriter.Mechanism := hctmString;
  FResponseContentWriter.ContentString := ResponseContentStr;
end;

function THTTPServerClient.GetResponseContentStream: TStream;
begin
  Result := FResponseContentWriter.ContentStream;
end;

procedure THTTPServerClient.SetResponseContentStream(const ResponseContentStream: TStream);
begin
  FResponseContentWriter.Mechanism := hctmStream;
  FResponseContentWriter.ContentStream := ResponseContentStream;
end;

function THTTPServerClient.GetResponseContentFileName: String;
begin
  Result := FResponseContentWriter.ContentFileName;
end;

procedure THTTPServerClient.SetResponseContentFileName(const ResponseContentFileName: String);
begin
  FResponseContentWriter.Mechanism := hctmFile;
  FResponseContentWriter.ContentFileName := ResponseContentFileName;
end;

procedure THTTPServerClient.SetResponseReady(const ResponseReady: Boolean);
begin
  if not ResponseReady then
    exit;
  Assert(FState in [hscsInit, hscsAwaitingRequest, hscsReceivedRequestHeader,
                    hscsReceivingContent, hscsRequestComplete, hscsPreparingResponse,
                    hscsAwaitingPreparedResponse]);
  FResponseReady := ResponseReady;
  if FState = hscsAwaitingPreparedResponse then
    ResponsePrepared;
end;

procedure THTTPServerClient.SetResponseOKHtmlStr(const HtmlStr: RawByteString);
var ContentType : THTTPContentTypeEnum;
begin
  ResponseCode := HTTP_ResponseCode_OK;
  if Length(HtmlStr) > 0 then
    ContentType := hctTextHtml
  else
    ContentType := hctNone;
  ResponseRecordPtr^.Header.CommonHeaders.ContentType.Value := ContentType;
  ResponseContentMechanism := hctmString;
  ResponseContentStr := HtmlStr;
  ResponseReady := True;
end;

procedure THTTPServerClient.SetResponseOKFile(const ContentType: THTTPContentTypeEnum;
          const FileName: String);
begin
  ResponseCode := HTTP_ResponseCode_OK;
  ResponseRecordPtr^.Header.CommonHeaders.ContentType.Value := ContentType;
  ResponseContentMechanism := hctmFile;
  ResponseContentFileName := FileName;
  ResponseReady := True;
end;

procedure THTTPServerClient.SetResponseNotFound;
begin
  ResponseCode := HTTP_ResponseCode_NotFound;
  ResponseRecordPtr^.Header.CommonHeaders.Connection.Value := hcfClose;
  ResponseReady := True;
end;

procedure THTTPServerClient.SetResponseRedirect(const Location: RawByteString);
begin
  ResponseCode := HTTP_ResponseCode_SeeOther;
  ResponseRecordPtr^.Header.FixedHeaders[hntLocation] := Location;
  ResponseReady := True;
end;

procedure THTTPServerClient.Disconnect;
begin
  if Assigned(FTCPClient) then
    FTCPClient.Close;
end;



{                                                                              }
{ THTTP4Server                                                                 }
{                                                                              }
constructor TF5HTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

procedure TF5HTTPServer.Init;
begin
  FLock := TCriticalSection.Create;
  InitDefaults;
  InitTCPServer;
end;

procedure TF5HTTPServer.InitTCPServer;
begin
  Assert(not Assigned(FTCPServer));
  FTCPServer := TF5TCPServer.Create(nil);
  
  FTCPServer.OnLog               := TCPServerLog;
  FTCPServer.OnStateChanged      := TCPServerStateChanged;
  FTCPServer.OnClientAccept      := TCPServerClientAccept;
  FTCPServer.OnClientCreate      := TCPServerClientCreate;
  FTCPServer.OnClientAdd         := TCPServerClientAdd;
  FTCPServer.OnClientRemove      := TCPServerClientRemove;
  FTCPServer.OnClientStateChange := TCPServerClientStateChange;
  FTCPServer.OnClientRead        := TCPServerClientRead;
  FTCPServer.OnClientWrite       := TCPServerClientWrite;
  FTCPServer.OnClientClose       := TCPServerClientClose;
end;

procedure TF5HTTPServer.InitDefaults;
begin
  FAddressFamily  := safIP4;
  FBindAddressStr := '0.0.0.0';
  FServerPort     := HTTPSERVER_PORT;
  FMaxBacklog     := HTTP_SERVER_DEFAULT_MaxBacklog;
  FMaxClients     := HTTP_SERVER_DEFAULT_MaxClients;
  {$IFDEF HTTP_TLS}
  FHTTPSEnabled   := False;
  {$ENDIF}
  FRequestContentMechanism := hcrmEvent;
  FResponseContentMechanism := hctmNone;
end;

destructor TF5HTTPServer.Destroy;
begin
  FreeAndNil(FTCPServer);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TF5HTTPServer.Loaded;
begin
  inherited Loaded;
  if FActivateOnLoaded then
    DoStart;
end;

procedure TF5HTTPServer.Log(const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog(self, LogType, Msg, LogLevel);
end;

procedure TF5HTTPServer.Log(const LogType: THTTPServerLogType; const Msg: String; const Args: array of const; const LogLevel: Integer);
begin
  Log(LogType, Format(Msg, Args), LogLevel);
end;

procedure TF5HTTPServer.Lock;
begin
  if Assigned(FLock) then
    FLock.Acquire;
end;

procedure TF5HTTPServer.Unlock;
begin
  if Assigned(FLock) then
    FLock.Release;
end;

procedure TF5HTTPServer.CheckNotActive;
begin
  if not (csDesigning in ComponentState) then
    if FActive then
      raise EHTTPServer.Create(SError_NotAllowedWhileActive);
end;

procedure TF5HTTPServer.SetAddressFamily(const AddressFamily: THTTPServerAddressFamily);
begin
  if AddressFamily = FAddressFamily then
    exit;
  CheckNotActive;
  FAddressFamily := AddressFamily;
end;

procedure TF5HTTPServer.SetBindAddress(const BindAddressStr: AnsiString);
begin
  if BindAddressStr = FBindAddressStr then
    exit;
  CheckNotActive;
  FBindAddressStr := BindAddressStr;
end;

procedure TF5HTTPServer.SetServerPort(const ServerPort: Integer);
begin
  if ServerPort = FServerPort then
    exit;
  CheckNotActive;
  FServerPort := ServerPort;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ServerPort:%d', [ServerPort]);
  {$ENDIF}
end;

procedure TF5HTTPServer.SetMaxBacklog(const MaxBacklog: Integer);
begin
  FMaxBacklog := MaxBacklog;
end;

procedure TF5HTTPServer.SetMaxClients(const MaxClients: Integer);
begin
  FMaxClients := MaxClients;
end;

procedure TF5HTTPServer.SetServerName(const ServerName: RawByteString);
begin
  if ServerName = FServerName then
    exit;
  CheckNotActive;
  FServerName := ServerName;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'ServerName:%s', [ServerName]);
  {$ENDIF}
end;

{$IFDEF HTTP_TLS}
procedure TF5HTTPServer.SetHTTPSEnabled(const HTTPSEnabled: Boolean);
begin
  if HTTPSEnabled = FHTTPSEnabled then
    exit;
  CheckNotActive;
  FHTTPSEnabled := HTTPSEnabled;
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'HTTPSEnabled:%d', [Ord(HTTPSEnabled)]);
  {$ENDIF}
end;

procedure TF5HTTPServer.SetHTTPSOptions(const HTTPSOptions: THTTPSServerOptions);
begin
  if HTTPSOptions = FHTTPSOptions then
    exit;
  CheckNotActive;
  FHTTPSOptions := HTTPSOptions;
end;
{$ENDIF}

procedure TF5HTTPServer.SetRequestContentMechanism(const RequestContentMechanism: THTTPContentReaderMechanism);
begin
  if RequestContentMechanism = FRequestContentMechanism then
    exit;
  CheckNotActive;
  FRequestContentMechanism := RequestContentMechanism;
end;

procedure TF5HTTPServer.TriggerStart;
begin
  if Assigned(FOnStart) then
    FOnStart(self);
end;

procedure TF5HTTPServer.TriggerStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

procedure TF5HTTPServer.TriggerActive;
begin
  if Assigned(FOnActive) then
    FOnActive(self);
end;

procedure TF5HTTPServer.TriggerInactive;
begin
  if Assigned(FOnInactive) then
    FOnInactive(self);
end;

procedure TF5HTTPServer.TriggerRequestHeader(const Client: THTTPServerClient);
begin
  if Assigned(FOnRequestHeader) then
    FOnRequestHeader(self, Client);
end;

procedure TF5HTTPServer.TriggerRequestContent(const Client: THTTPServerClient; const Buf; const Size: Integer);
begin
  if Assigned(FOnRequestContent) then
    FOnRequestContent(self, Client, Buf, Size);
end;

procedure TF5HTTPServer.TriggerRequestComplete(const Client: THTTPServerClient);
begin
  if Assigned(FOnRequestComplete) then
    FOnRequestComplete(self, Client);
end;

procedure TF5HTTPServer.TriggerPrepareResponse(const Client: THTTPServerClient);
begin
  if Assigned(FOnPrepareResponse) then
    FOnPrepareResponse(self, Client);
end;

procedure TF5HTTPServer.TriggerResponseComplete(const Client: THTTPServerClient);
begin
  if Assigned(FOnResponseComplete) then
    FOnResponseComplete(self, Client);
end;

procedure TF5HTTPServer.TCPServerLog(Sender: TF5TCPServer; LogType: TTCPLogType; Msg: String; LogLevel: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer:%s', [Msg], LogLevel + 1);
  {$ENDIF}
end;

procedure TF5HTTPServer.TCPServerStateChanged(Sender: TF5TCPServer; State: TTCPServerState);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ServerStateChange:%s', [Sender.StateStr]);
  {$ENDIF}
end;

procedure TF5HTTPServer.TCPServerClientAccept(Sender: TF5TCPServer; Address: TSocketAddr;
          var AcceptClient: Boolean);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientAccept');
  {$ENDIF}
end;

procedure TF5HTTPServer.TCPServerClientCreate(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientCreate');
  {$ENDIF}
  C := THTTPServerClient.Create(self, Sender);
  Sender.UserObject := C;
end;

procedure TF5HTTPServer.TCPServerClientAdd(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientAdd');
  {$ENDIF}
  Assert(Sender.UserObject is THTTPServerClient);
  C := THTTPServerClient(Sender.UserObject);
  C.Start;
end;

procedure TF5HTTPServer.TCPServerClientRemove(Sender: TTCPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientRemove');
  {$ENDIF}
  Assert(not Assigned(Sender.UserObject) or (Sender.UserObject is THTTPServerClient));
  Sender.UserObject.Free;
  Sender.UserObject := nil;
end;

procedure TF5HTTPServer.TCPServerClientStateChange(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientStateChange:%s', [Sender.StateStr]);
  {$ENDIF}
  C := Sender.UserObject as THTTPServerClient;
  C.TCPClientStateChange;
end;

procedure TF5HTTPServer.TCPServerClientRead(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientRead');
  {$ENDIF}
  C := Sender.UserObject as THTTPServerClient;
  C.TCPClientRead;
end;

procedure TF5HTTPServer.TCPServerClientWrite(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientWrite');
  {$ENDIF}
  Assert(Sender.UserObject is THTTPServerClient);
  C := THTTPServerClient(Sender.UserObject);
  C.TCPClientWrite;
end;

procedure TF5HTTPServer.TCPServerClientClose(Sender: TTCPServerClient);
var C : THTTPServerClient;
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'TCPServer_ClientClose');
  {$ENDIF}
  Assert(Sender.UserObject is THTTPServerClient);
  C := THTTPServerClient(Sender.UserObject);
  C.TCPClientClose;
end;

procedure TF5HTTPServer.ClientLog(const Client: THTTPServerClient; const LogType: THTTPServerLogType; const Msg: String; const LogLevel: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(LogType, 'Client:%s', [Msg], LogLevel + 1);
  {$ENDIF}
end;

procedure TF5HTTPServer.ClientStateChanged(const Client: THTTPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_StateChange');
  {$ENDIF}
end;

procedure TF5HTTPServer.ClientRequestHeader(const Client: THTTPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_RequestHeader');
  {$ENDIF}
  TriggerRequestHeader(Client);
end;

procedure TF5HTTPServer.ClientRequestContentBuffer(const Client: THTTPServerClient; const Buf; const Size: Integer);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_RequestContentBuffer');
  {$ENDIF}
  TriggerRequestContent(Client, Buf, Size);
end;

procedure TF5HTTPServer.ClientRequestContentComplete(const Client: THTTPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_RequestContentComplete');
  {$ENDIF}
  TriggerRequestComplete(Client);
end;

procedure TF5HTTPServer.ClientPrepareResponse(const Client: THTTPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_PrepareResponse');
  {$ENDIF}
  TriggerPrepareResponse(Client);
end;

procedure TF5HTTPServer.ClientResponseComplete(const Client: THTTPServerClient);
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'Client_ResponseComplete');
  {$ENDIF}
  TriggerResponseComplete(Client);
end;

procedure TF5HTTPServer.SetupTCPServer;
var AF : TIPAddressFamily;
    {$IFDEF HTTP_TLS}
    TLSOpt : TTLSServerOptions;
    {$ENDIF}
begin
  {$IFDEF HTTP_DEBUG}
  Log(sltDebug, 'SetupTCPServer');
  {$ENDIF}

  Assert(Assigned(FTCPServer));

  case FAddressFamily of
    safIP4 : AF := iaIP4;
    safIP6 : AF := iaIP6;
  else
    raise EHTTPServer.Create('Invalid parameter');
  end;
  FTCPServer.AddressFamily := AF;
  FTCPServer.BindAddress   := FBindAddressStr;
  FTCPServer.ServerPort    := FServerPort;
  {$IFDEF HTTP_TLS}
  FTCPServer.TLSEnabled := FHTTPSEnabled;
  TLSOpt := [];
  if ssoDontUseSSL3 in FHTTPSOptions then
    Include(TLSOpt, tlssoDontUseSSL3);
  if ssoDontUseTLS10 in FHTTPSOptions then
    Include(TLSOpt, tlssoDontUseTLS10);
  if ssoDontUseTLS11 in FHTTPSOptions then
    Include(TLSOpt, tlssoDontUseTLS11);
  if ssoDontUseTLS12 in FHTTPSOptions then
    Include(TLSOpt, tlssoDontUseTLS12);
  FTCPServer.TLSServer.Options := TLSOpt;
  {$ENDIF}
end;

procedure TF5HTTPServer.DoStart;
begin
  Assert(not FActive);

  Log(sltInfo, 'Start');
  TriggerStart;
  SetupTCPServer;
  FTCPServer.Start;

  FActive := True;
  Log(sltInfo, 'Active');
  TriggerActive;
end;

procedure TF5HTTPServer.DoStop;
begin
  Assert(FActive);
  Assert(Assigned(FTCPServer));

  Log(sltInfo, 'Stop');
  TriggerStop;
  FTCPServer.Stop;

  FActive := False;
  Log(sltInfo, 'Inactive');
  TriggerInactive;
end;

procedure TF5HTTPServer.SetActive(const Active: Boolean);
begin
  if Active = FActive then
    exit;
  if Active then
    DoStart
  else
    DoStop;
end;

function TF5HTTPServer.GetClientCount: Integer;
begin
  Result := FTCPServer.ClientCount;
end;



{$IFDEF HTTPSERVER_CUSTOM}
  {$INCLUDE cHTTPServerImpl.inc}
{$ENDIF}



end.


