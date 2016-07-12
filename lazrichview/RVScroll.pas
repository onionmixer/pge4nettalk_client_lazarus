unit RVScroll;

interface

uses
  {$IFDEF FPC}
  RVLazIntf, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  LMessages, SysUtils, Classes, Forms, Controls, Graphics;

type
  { TRVScroller }

  TRVScroller = class(TCustomControl)
  private
    FTracking: Boolean;
    FFullRedraw: Boolean;
    FVScrollVisible: Boolean;
    FOnVScrolled: TNotifyEvent;
    function GetVScrollPos: Integer;
    procedure SetVScrollPos(Pos: Integer);
    function GetVScrollMax: Integer;
    procedure SetVScrollVisible(vis: Boolean);
  protected
    SmallStep, HPos, VPos, XSize, YSize: Integer;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateScrollBars(XS, YS: Integer);
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure SetVPos(p: Integer);
    procedure SetHPos(p: Integer);
    procedure Paint; override;
    //procedure ScrollChildren(dx: Integer);
    procedure UpdateChildren; virtual;
    property FullRedraw: Boolean read FFullRedraw write FFullRedraw;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);override;
    procedure EraseBackground(DC: HDC); override;
    procedure ScrollTo(y: Integer);
    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property VScrollMax: Integer read GetVScrollMax;
  published
    { Published declarations }
    property Visible;
    property TabStop;
    property TabOrder;
    property Align;
    property HelpContext;
    property Tracking: Boolean read FTracking write FTracking;
    property VScrollVisible: Boolean read FVScrollVisible write SetVScrollVisible;
    property OnVScrolled: TNotifyEvent read FOnVScrolled write FOnVScrolled;
  end;

implementation
{------------------------------------------------------}
constructor TRVScroller.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  FTracking := True;
  FFullRedraw := False;
  FVScrollVisible := True;
end;
{------------------------------------------------------}
procedure TRVScroller.EraseBackground(DC: HDC);
begin
  // Nothing
end;
{------------------------------------------------------}
procedure TRVScroller.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);   //CreateWindow
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_HSCROLL or WS_VSCROLL;
end;
{------------------------------------------------------}
procedure  TRVScroller.CreateWnd;
begin
  inherited CreateWnd;
  SmallStep := 1;
  VPos := 0;
  HPos := 0;
  UpdateScrollBars(ClientWidth, ClientHeight);
end;
{------------------------------------------------------}
procedure TRVScroller.UpdateScrollBars(XS, YS: Integer);
var
  ScrollInfo: TScrollInfo;
begin
  XSize := XS;
  YSize := (YS + SmallStep - 1) div SmallStep;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nPage := ClientHeight;
  ScrollInfo.nMax := YSize - 1;
  ScrollInfo.nPos := VPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  if not FVScrollVisible then
    ShowScrollBar(Handle, SB_VERT, FVScrollVisible);

  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nMax := XSize - 1;
  ScrollInfo.nPage := ClientWidth;
  ScrollInfo.nPos := HPos;
  ScrollInfo.nTrackPos := 0;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);

  UpdateChildren;
end;
{------------------------------------------------------}
procedure TRVScroller.UpdateChildren;
begin
  // Nothing
end;
{------------------------------------------------------}
procedure TRVScroller.WMHScroll(var Message: TWMHScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetHPos(HPos - (ClientWidth div 8) - 1);
      SB_LINEDOWN: SetHPos(HPos + (ClientWidth div 8) + 1);
      SB_PAGEUP: SetHPos(HPos - ClientWidth);
      SB_PAGEDOWN: SetHPos(HPos + ClientWidth);
      SB_THUMBPOSITION: SetHPos(Pos);
      SB_THUMBTRACK: if FTracking then SetHPos(Pos);
      SB_TOP: SetHPos(0);
      SB_BOTTOM: SetHPos(XSize);
    end;
end;
{------------------------------------------------------}
procedure TRVScroller.WMVScroll(var Message: TWMVScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetVPos(VPos - (ClientHeight div 8) - 1);
      SB_LINEDOWN: SetVPos(VPos + (ClientHeight div 8) + 1);
      SB_PAGEUP: SetVPos(VPos - ClientHeight);
      SB_PAGEDOWN: SetVPos(VPos + ClientHeight);
      SB_THUMBPOSITION: SetVPos(Pos);
      SB_THUMBTRACK: if FTracking then SetVPos(Pos);
      SB_TOP: SetVPos(0);
      SB_BOTTOM: SetVPos(YSize);
    end;
end;
{------------------------------------------------------}
procedure TRVScroller.WMKeyDown(var Message: TWMKeyDown);
var vScrollNotify, hScrollNotify: Integer;
begin
  vScrollNotify := -1;
  hScrollNotify := -1;
  with Message do
    case CharCode of
        VK_UP:
            vScrollNotify := SB_LINEUP;
        VK_PRIOR:
            vScrollNotify := SB_PAGEUP;
        VK_NEXT:
            vScrollNotify := SB_PAGEDOWN;
        VK_DOWN:
            vScrollNotify := SB_LINEDOWN;
        VK_HOME:
            vScrollNotify := SB_TOP;
        VK_END:
            vScrollNotify := SB_BOTTOM;
        VK_LEFT:
            hScrollNotify := SB_LINELEFT;
        VK_RIGHT:
            hScrollNotify := SB_LINERIGHT;
    end;
  if (vScrollNotify <> -1) then
        Perform(WM_VSCROLL, vScrollNotify, 0);
  if (hScrollNotify <> -1) then
        Perform(WM_HSCROLL, hScrollNotify, 0);
  {$IFDEF FPC}
  inherited WMKeyDown(Message);
  {$ELSE}
  inherited;
  {$ENDIF}
end;
{------------------------------------------------------}
procedure TRVScroller.SetVPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
begin
  OldPos := VPos;
  VPos := GetVScrollMax;
  if (VPos > p) then VPos := p;
  if (VPos < 0) then VPos := 0;
  //VPos := p;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := VPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  VPos := ScrollInfo.nPos;

  if OldPos <> VPos then begin
    if FFullRedraw then begin
      UpdateChildren;
      Refresh;
    end
    else begin
      UpdateChildren;
      Invalidate;
    end;
    if Assigned(FOnVScrolled) then FOnVScrolled(Self);
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.SetHPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
begin
  OldPos := HPos;
  HPos := p;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_POS;
  SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
  GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
  HPos := ScrollInfo.nPos;

  if OldPos-HPos <> 0 then begin
    if FFullRedraw then begin
      UpdateChildren;
      Refresh;
    end
    else begin
      UpdateChildren;
      Invalidate;
    end;
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.Paint;
var i: Integer;
begin
  with Canvas do
  begin
    Font.Color := clRed;
    Font.Size := 5;
    //FillRect(ClipRect);
    for i := ClipRect.Top div SmallStep - 1 to ClipRect.Bottom div SmallStep + 1 do
      TextOut(-HPos, i*SmallStep, IntToStr(i+VPos));
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollTo(y: Integer);
begin
  SetVPos(y div SmallStep);
end;
{-------------------------------------------------------}
function TRVScroller.GetVScrollPos: Integer;
begin
  GetVScrollPos := VPos;
end;
{-------------------------------------------------------}
procedure TRVScroller.SetVScrollPos(Pos: Integer);
begin
  SetVPos(Pos);
end;
{-------------------------------------------------------}
function TRVScroller.GetVScrollMax: Integer;
var ScrollInfo: TScrollInfo;
begin
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.nPos := HPos;
  ScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
  GetScrollInfo(Handle, SB_VERT, ScrollInfo);
  GetVScrollMax := ScrollInfo.nMax - ScrollInfo.nPage;
end;
{-------------------------------------------------------}
procedure TRVScroller.SetVScrollVisible(vis: Boolean);
begin
  FVScrollVisible := vis;
  if HandleAllocated then
    ShowScrollBar(Handle, SB_VERT, vis);
end;
{-------------------------------------------------------}
procedure TRVScroller.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TRVScroller.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if Message.WheelDelta > 0 then
    Perform(WM_VSCROLL, SB_LINEUP, 0)
  else if Message.WheelDelta < 0 then
    Perform(WM_VSCROLL, SB_LINEDOWN, 0)
  else
    inherited;
end;

end.
