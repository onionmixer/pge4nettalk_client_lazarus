unit RVFreeReg;
interface

  {$I RV_Defs.inc}

uses
  Classes,
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  RichView, RVStyle;

procedure Register;

implementation

{--------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('RichView', [TRVStyle, TRichView]);
end;

initialization
{$I richview.lrs}

end.
