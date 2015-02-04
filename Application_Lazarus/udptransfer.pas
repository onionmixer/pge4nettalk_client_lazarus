unit udptransfer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  Windows, WinSock2,
  {$ENDIF}
  {$IFDEF UNIX}
  BaseUnix, UnixUtil, NetDB,
  {$ENDIF}
  Sockets, NetUtils, FileUtil;


type

  { TUDPTransfer }

  TTransferProgress = procedure (Sender: TObject; Progress: Int64) of object;
  TTransferDebug = procedure (Sender: TObject; aMsg: String) of object;

  TUDPTransfer = class(TThread)
  private
    fOnProgress: TTransferProgress;
    fOnDebug: TTransferDebug;

    fAddr: TSockAddr;
    fReady: Boolean;
    fSocket: Integer;

    fStart: Boolean;
    fFile: THandle;
    fProgress: Int64;
  protected
    procedure Execute; override;
    procedure OnUpdate;
  public
    constructor Create;
    destructor Destroy; override;

    function SendTo(Addr: String; Port: Integer; Msg: String): Boolean;
    procedure Send(FileName: String);
    procedure Recv(FileName: String);
    procedure Stop;

    property Ready: Boolean read fReady;
    property OnProgress: TTransferProgress read fOnProgress write fOnProgress;
    property OnDebug: TTransferDebug read fOnDebug write fOnDebug;
  end;

implementation

{ TUDPTransfer }

{$IFDEF WINDOWS}
function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
begin
  Result := Select(nfds, readfds, writefds, exceptfds, timeout);
end;

function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
begin
  Result := 0;
  if FD_ISSET(Socket, FDSet) then Result := 1;
end;

procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
begin
  FD_SET(Socket, FDSet);
end;

procedure fpFD_ZERO(var FDSet: TFDSet); inline;
begin
  FD_ZERO(FDSet);
end;
{$ENDIF}

function FileSizeUTF8(FileName: String): Int64;
{$IFNDEF UNIX}
var
  sr: TSearchRec;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result:=filesize(FileName);
{$ELSE}
  if FindFirstUTF8(FileName, faAnyFile, sr ) = 0 then
  begin
     Result := Int64(sr.FindData.nFileSizeHigh);
     Result := (Result shl 32) + Int64(sr.FindData.nFileSizeLow)
  end
  else
     Result := 0;
  FindCloseUTF8(sr);
{$ENDIF}
end;

procedure TUDPTransfer.Execute;
var
  readfds: TFDSet;
  nfds: Integer;
  addr: TSockAddr;
  addr_len: TSockLen;
  buf: Array[0..1024] of Char;
  len: Integer;
  timeout: TTimeVal;
  size: Integer;
  ret: Int64;
begin
  nfds := 1;
  while not Terminated do
  begin
    if (fSocket < 0) then
    begin
      Sleep(1000);
      continue;
    end;

    {$IFNDEF WINDOWS}
    nfds := fSocket + 1;
    {$ENDIF}

    fpFD_Zero(readfds);
    fpFD_Set(fSocket, readfds);
    timeout.tv_sec := 3;
    timeout.tv_usec := 0;

    fpSelect(nfds, @readfds, nil, nil, @timeout);
    if (fSocket < 0) or Terminated then continue;

    if fpFD_ISSET(fSocket, readfds) = 1 then
    begin
      addr_len := sizeof(addr);
      len := fpRecvFrom(fSocket, @buf, sizeof(buf), 0, @addr, @addr_len);
      if len = -1 then
      begin
        CloseSocket(fSocket);
        fSocket := -1;
        continue;
      end;

      if (len = 4) and (buf[0] = 'p') and ((buf[1] = 'i') or (buf[1] = 'o')) and
        (buf[2] = 'n') and (buf[3] = 'g') then
      begin
        if buf[1] = 'i' then
        begin
          buf[1] := 'o';
          fpSendTo(fSocket, @buf, 4, 0, @addr, sizeof(addr));
        end;
        if not fReady then
        begin
          fReady := True;
          fProgress := 0;
          Synchronize(@OnUpdate);
        end;
      end
      else if (len = 4) and (buf[0] = 'r') and (buf[1] = 'e') and
        (buf[2] = 'c') and (buf[3] = 'v') then
      begin
        ret := FileRead(fFile, buf[4], 512);
        if ret > 0 then
        begin
          buf[0] := 't';
          buf[1] := 'r';
          buf[2] := Chr(ret shr 8);
          buf[3] := Chr(ret and $FF);
          fpSendTo(fSocket, @buf, ret + 4, 0, @fAddr, sizeof(fAddr));
          fProgress := FileSeek(fFile, 0, fsFromCurrent);
          Synchronize(@OnUpdate);
        end else begin
          fStart := False;
          FileClose(fFile);
          fFile := -1;
          fProgress := 0;
          Synchronize(@OnUpdate);
        end;
      end
      else if (len > 4) and (buf[0] = 't') and (buf[1] = 'r') then
      begin
        size := Ord(buf[2]);
        size := (size shl 8) or Ord(buf[3]);
        ret := FileWrite(fFile, buf[4], size);
        buf := 'recv';
        fpSendTo(fSocket, @buf, 4, 0, @addr, sizeof(addr));
        fProgress := FileSeek(fFile, 0, fsFromCurrent);
        Synchronize(@OnUpdate);
      end else begin
        buf := 'ping';
        fpSendTo(fSocket, @buf, 4, 0, @fAddr, sizeof(fAddr));
      end;
    end else begin
      buf := 'ping';
      fpSendTo(fSocket, @buf, 4, 0, @fAddr, sizeof(fAddr));
    end;
  end;
  fProgress := -1;
  Synchronize(@OnUpdate);
end;

procedure TUDPTransfer.OnUpdate;
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, fProgress);
end;

constructor TUDPTransfer.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  fSocket := -1;
  fStart := False;
  fOnProgress := nil;
  fOnDebug := nil;
end;

destructor TUDPTransfer.Destroy;
begin
  if fSocket <> -1 then CloseSocket(fSocket);
  fSocket := -1;
  if fFile <> -1 then FileClose(fFile);
  inherited Destroy;
end;

function TUDPTransfer.SendTo(Addr: String; Port: Integer; Msg: String): Boolean;
var
  buf: Array[0..100] of Char;
  len: Integer;
  opt: DWord;
begin
  Result := False;
  if fSocket = -1 then
  begin
    fSocket := fpSocket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if fSocket = -1 then exit;
    {$IFDEF WINDOWS}
    opt := 1;
    ioctlsocket(fSocket, FIONBIO, @opt);
    {$ENDIF}
    {$IFDEF UNIX}
    opt := FpFcntl(fSocket, F_GetFl);
    opt := opt or O_NONBLOCK;
    FpFcntl(fSocket, F_SetFl, opt);
    {$ENDIF}
  end;

  FillByte(fAddr, SizeOf(fAddr), 0);
  fAddr.sin_family := AF_INET;
  fAddr.sin_port := htons(Port);
  fAddr.sin_addr := GetHostAddr(Addr);

  buf := Msg;
  len := Length(Msg);
  len := fpSendTo(fSocket, @buf, len, 0, @fAddr, sizeof(fAddr));

  if fOnDebug <> nil then
    fOnDebug(Self, '>>' + Msg + ' ' + NetAddrToStr(fAddr.sin_addr) + ':' + IntToStr(ntohs(fAddr.sin_port)));

  if len = -1 then
  begin
    len := fSocket;
    fSocket := -1;
    CloseSocket(len);
    exit;
  end;

  Result := True;
end;

procedure TUDPTransfer.Send(FileName: String);
begin
  if not fReady then exit;
  fFile := FileOpenUTF8(FileName, fmOpenRead);
  fStart := True;
end;

procedure TUDPTransfer.Recv(FileName: String);
var
  buf: Array[0..4] of Char;
begin
  if fFile <> -1 then FileClose(fFile);
  if FileExistsUTF8(FileName) then
    fFile := FileOpenUTF8(FileName, fmOpenWrite)
  else
    fFile := FileCreateUTF8(FileName);
  buf := 'recv';
  fpSendTo(fSocket, @buf, 4, 0, @fAddr, sizeof(fAddr));
  fStart := True;
end;

procedure TUDPTransfer.Stop;
begin
  Terminate;
  if fSocket >= 0 then
    CloseSocket(fSocket);
end;

end.

