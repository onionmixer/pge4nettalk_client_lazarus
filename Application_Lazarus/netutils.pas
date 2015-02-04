unit NetUtils;

interface

uses
{$IFDEF WINDOWS}
  WinSock2,
{$ENDIF}
  Sockets;

function InAddrToStr(const addr: TInAddr):String; inline;
function GetHostAddr(const host: AnsiString):TInAddr;
function GetLocalAddr:TInAddr;

{$IFDEF WINDOWS}
function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
procedure fpFD_CLR(const Socket: Longint; var FDSet: TFDSet); inline;
procedure fpFD_SET(const Socket: Longint; var FDSet: TFDSet); inline;
procedure fpFD_ZERO(var FDSet: TFDSet); inline;
{$ENDIF}

procedure SetBlockingMode(const Socket: Longint; Mode: Boolean);

implementation

uses
{$IFDEF UNIX}
  NetDB, BaseUnix,
{$ENDIF}
  SysUtils;

{$IFNDEF WINDOWS}
const
  MAX_ADJUSTMENT = 10;
  IPPROTO_IP     = 0;
  IF_NAMESIZE    = 16;
  SIOCGIFCONF    = $8912;
  SIOCGIFHWADDR  = $8927;

type
  {$packrecords c}
  tifr_ifrn = record
    case integer of
      0 : (ifrn_name: array [0..IF_NAMESIZE-1] of char);
  end;
  tifmap = record
    mem_start : culong;
    mem_end   : culong;
    base_addr : cushort;
    irq       : cuchar;
    dma       : cuchar;
    port      : cuchar;
  end;
  PIFrec = ^TIFrec;
  TIFrec = record
    ifr_ifrn : tifr_ifrn;
    case integer of
      0 : (ifru_addr      : TSockAddr);
      1 : (ifru_dstaddr   : TSockAddr);
      2 : (ifru_broadaddr : TSockAddr);
      3 : (ifru_netmask   : TSockAddr);
      4 : (ifru_hwaddr    : TSockAddr);
      5 : (ifru_flags     : cshort);
      6 : (ifru_ivalue    : cint);
      7 : (ifru_mtu       : cint);
      8 : (ifru_map       : tifmap);
      9 : (ifru_slave     : Array[0..IF_NAMESIZE-1] of char);
      10 : (ifru_newname  : Array[0..IF_NAMESIZE-1] of char);
      11 : (ifru_data     : pointer);
  end;
  TIFConf = record
    ifc_len : cint;
    case integer of
      0 : (ifcu_buf : pointer);
      1 : (ifcu_req : ^tifrec);
  end;
{$ENDIF}

function InAddrToStr(const addr: TInAddr):String; inline;
begin
  InAddrToStr := Format('%d.%d.%d.%d', [addr.s_bytes[1], addr.s_bytes[2], addr.s_bytes[3], addr.s_bytes[4]]);
end;

function GetHostAddr(const host: AnsiString):TInAddr;
var
{$IFDEF WINDOWS}
  he: PHostEnt;
{$ENDIF}
{$IFDEF UNIX}
  he: THostEntry;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  GetHostAddr.s_addr := INADDR_NONE;
  he := GetHostByName(PChar(host));
  if he <> nil then
    GetHostAddr.s_addr := PCardinal(he^.h_addr_list[0])^;
{$ENDIF}
{$IFDEF UNIX}
  GetHostAddr := StrToNetAddr(host);
  if GetHostAddr.s_addr <> INADDR_ANY then exit;
  if GetHostByName(PChar(host), he) then
    GetHostAddr.s_addr := htonl(he.Addr.s_addr)
  else if ResolveHostByName(PChar(host), he) then
    GetHostAddr := he.Addr;
{$ENDIF}
end;

function GetLocalAddr:TInAddr;
var
{$IFDEF WINDOWS}
  host: array[0..1024] of char;
{$ELSE}
  socket: Longint;
  ifc: TIFConf;
  ifr: array[1..16] of TIFRec;
  i: Integer;
{$ENDIF}
begin
  GetLocalAddr.s_addr := INADDR_NONE;
{$IFDEF WINDOWS}
  if GetHostName(host, SizeOf(host)) = 0 then
    GetLocalAddr := GetHostAddr(host);
{$ELSE}
  socket := fpSocket(AF_INET, SOCK_DGRAM, 0);
  if socket >= 0 then
  begin
    ifc.ifc_len := SizeOf(ifr);
    ifc.ifcu_req := @ifr;
    if fpIoctl(socket, SIOCGIFCONF, @ifc) >= 0 then
    begin
      for I := 1 to ifc.ifc_len div SizeOf(TIFRec) do
      begin
        if String(ifr[I].ifr_ifrn.ifrn_name) = 'lo' then continue;
        GetLocalAddr := ifr[I].ifru_addr.sin_addr;
        break;
      end;
    end;
    CloseSocket(socket);
  end;
{$ENDIF}
end;

{$IFDEF WINDOWS}
function fpSelect(const nfds: Integer; const readfds, writefds, exceptfds: PFDSet;
                  const timeout: PTimeVal): Longint; inline;
begin
  fpSelect := Select(nfds, readfds, writefds, exceptfds, timeout);
end;

function fpFD_ISSET(const Socket: Longint; var FDSet: TFDSet): Integer; inline;
begin
  fpFD_ISSET := 0;
  if FD_ISSET(Socket, FDSet) then
    fpFD_ISSET := 1;
end;

procedure fpFD_CLR(const Socket: Longint; var FDSet: TFDSet); inline;
begin
  FD_CLR(Socket, FDSet);
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

procedure SetBlockingMode(const Socket: Longint; Mode: Boolean);
var
{$IFDEF WINDOWS}
  opt: DWord;
{$ENDIF}
{$IFDEF UNIX}
  opt: Longint;
{$ENDIF}
begin
{$IFDEF WINDOWS}
  if Mode then opt := 0
  else opt := 1;
  IoctlSocket(Socket, WinSock2.FIONBIO, @opt);
{$ENDIF}
{$IFDEF UNIX}
  opt := fpFcntl(Socket, F_GetFl);
  if Mode then
    opt := opt and (not O_NONBLOCK)
  else
    opt := opt or O_NONBLOCK;
  fpFcntl(Socket, F_SetFl, opt);
{$ENDIF}
end;

end.
