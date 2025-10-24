unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

procedure LogInfo(const Msg: String); overload;
procedure LogInfo(const Fmt: String; const Args: array of const); overload;

{$IFDEF DEBUG}
procedure LogDebug(const Msg: String); overload;
procedure LogDebug(const Fmt: String; const Args: array of const); overload;
{$ENDIF}

implementation

function GetTimestamp: String;
var
  Now: TDateTime;
begin
  Now := SysUtils.Now;
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

procedure LogInfo(const Msg: String);
begin
  WriteLn('[', GetTimestamp, '][INFO] ', Msg);
end;

procedure LogInfo(const Fmt: String; const Args: array of const);
begin
  WriteLn('[', GetTimestamp, '][INFO] ', Format(Fmt, Args));
end;

{$IFDEF DEBUG}
procedure LogDebug(const Msg: String);
begin
  WriteLn('[', GetTimestamp, '][DEBUG] ', Msg);
end;

procedure LogDebug(const Fmt: String; const Args: array of const);
begin
  WriteLn('[', GetTimestamp, '][DEBUG] ', Format(Fmt, Args));
end;
{$ENDIF}

end.
