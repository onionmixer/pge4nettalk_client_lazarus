unit ChatLabel;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichView, RVStyle, Contnrs, ExtCtrls, LMessages;

type

  TChatLineInfo = class
    Index: Integer;
    Offset: Integer;
    Length: Integer;
    Width: Integer;
    Height: Integer;
    LineBreak: Boolean;
    Link: UTF8String;
  end;

  { TChatSkin }

  TChatSkin = class(TObject)
  private
    fShadowSize: Integer;
    fTopSpace, fBottomSpace: Integer;
    fLeftSpace, fRightSpace, fTipOutSize: Integer;
    fMinWidth ,fMinHeight: Integer;
    fSkin: array[1..11] of TBitmap;
    fSkinLoaded: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; Width, Height, Skip, View, Scroll: Integer;
      LeftSide: Boolean);
    procedure LoadFromBitmap(Skin: TBitmap);

    procedure SizeCalc(var Width, Height: Integer);
    procedure TextPos(CanvasHeight, TextHeight: Integer; LeftSide: Boolean;
      var X, Y: Integer);

    function MaxWidth(AWidth: Integer): Integer;
  end;

  { TChatLabel }

  TChatLabel = class(TCustomLabel)
  private
    fBitmap: TBitmap;
    fSelectColor: TColor;
    fSkin, fSelectSkin: TChatSkin;
    fLeftSide: Boolean;

    fCaption: UTF8String;
    fLines: TStringList;
    fLineInfo: TObjectList;
    fNeedUpdate: Boolean;

    fMaxWidth: Integer;
    fTextWidth, fTextHeight: Integer;
    fLineGap: Integer;

    fStyle: TRVStyle;
    fStyleNo: Integer;

    fAniTimer: TTimer;
    fShowTime: QWord;
    fFullDraw: Boolean;

    fSelStartLine, fSelStartPos: Integer;
    fSelEndLine, fSelEndPos: Integer;
    fSelect: Boolean;
    fSelectAll: Boolean;
    fScrollDelta: Integer;
    fClickLine: Integer;

    fMouseX, fMouseY: Integer;

    procedure DoAniTimerOnTimer(Sender: TObject);
    function FindLineInfo(X, Y:Integer): Integer;
    procedure FindLinePos(X, Y: Integer; var Line, Pos: Integer);
    function GetSelectText: UTF8String;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure CalculateSize(MaxWidth: integer; var NeededWidth,
      NeededHeight: integer);

    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);override;
    procedure DblClick; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function  GetTextBuf(Buffer: PChar; BufSize: Integer): Integer; override;
    function  GetTextLen: Integer; override;
    procedure SetTextBuf(Buffer: PChar); override;
    procedure Paint; override;
    procedure SelectAll;
    procedure Deselect;

    procedure MouseUpOnParent(Button: TMouseButton; X, Y: Integer);
    procedure ScrollDeltaOnParent(Delta: Integer);
    procedure ForceCalculateSize;
  published
    property Skin: TChatSkin write fSkin;
    property SelectSkin: TChatSkin write fSelectSkin;
    property SelectColor: TColor write fSelectColor;
    property LeftSide: Boolean read fLeftSide write fLeftSide;
    property MaxWidth: Integer read fMaxWidth;
    property Style: TRVStyle read fStyle write fStyle;
    property StyleNo: Integer read fStyleNo write fStyleNo;
    property SelectText: UTF8String read GetSelectText;
    property NeedUpdate: Boolean read fNeedUpdate;

    property Caption;
    property Color;
    property Font;
  end;

implementation

uses LCLType, LCLIntf, LazUTF8, Types, LclProc;

{ TChatSkin }

constructor TChatSkin.Create;
begin
  inherited Create;
  fSkinLoaded := False;
  fShadowSize := 0;
  fTopSpace := 0;
  fBottomSpace := 0;
  fLeftSpace := 0;
  fRightSpace := 0;
  fTipOutSize := 0;
  fMinWidth := 0;
  fMinHeight := 0;
end;

destructor TChatSkin.Destroy;
var
  i: Integer;
begin
  if fSkinLoaded then
  begin
    for i := 1 to 11 do
      fSkin[i].Free;
    fSkinLoaded := False;
  end;
  inherited Destroy;
end;

procedure TChatSkin.Draw(Canvas: TCanvas; Width, Height, Skip, View,
  Scroll: Integer; LeftSide: Boolean);

procedure FillBitmap(X1, Y1, X2, Y2: Integer; Canvas: TCanvas; Tile: TBitmap);
var
  X, W, H: Integer;
  SrcRect, DstRect: TRect;
begin
  while Y1 < Y2 do
  begin
    X := X1;
    if (Y2 - Y1) < Tile.Height then
      H := Y2 - Y1
    else
      H := Tile.Height;
    while X < X2 do
    begin
      if (X2 - X) < Tile.Width then
        W := X2 - X
      else
        W := Tile.Width;
      SrcRect := Rect(0, 0, W, H);
      DstRect := Rect(X, Y1, X + W, Y1 + H);
      Canvas.CopyRect(DstRect, Tile.Canvas, SrcRect);
      Inc(X, Tile.Width);
    end;
    Inc(Y1, Tile.Height);
  end;
end;

var
  X1, X2: Integer;
begin
  if not fSkinLoaded then exit;

  if LeftSide then
  begin
    X1 := 7 - Scroll;
    X2 := Width - Scroll;
  end else begin
    X1 := 0 + Scroll;
    X2 := Width - 5 + Scroll;
  end;

  FillBitmap(X1, Skip, X2, Skip + View, Canvas, fSkin[5]);
  FillBitmap(X1, Skip, X1 + fSkin[4].Width, Skip + View, Canvas, fSkin[4]);
  FillBitmap(X2 - fSkin[6].Width, Skip, X2, Skip + View, Canvas, fSkin[6]);

  if Skip < fSkin[2].Height then
  begin
    FillBitmap(X1, 0, X2, fSkin[2].Height, Canvas, fSkin[2]);
    Canvas.Draw(X1, 0, fSkin[1]);
    Canvas.Draw(X2 - fSkin[3].Width, 0, fSkin[3]);
  end;

  if (Skip + View) > (Height - fSkin[8].Height) then
  begin
    FillBitmap(X1, Height - fSkin[8].Height, X2, Height, Canvas, fSkin[8]);
    Canvas.Draw(X1, Height - fSkin[7].Height, fSkin[7]);
    Canvas.Draw(X2 - fSkin[9].Width, Height - fSkin[9].Height, fSkin[9]);
  end;

  if LeftSide then
    Canvas.Draw(X1 - 7, 4, fSkin[10])
  else
    Canvas.Draw(X2 - 3, 4, fSkin[11]);
end;

procedure TChatSkin.LoadFromBitmap(Skin: TBitmap);

function GetImage(X, Y, W, H: Integer): TBitmap;
var
  SrcRect, DstRect: TRect;
begin
  SrcRect := Rect(X, Y, X + W, Y + H);
  DstRect := Rect(0, 0, W, H);
  Result := TBitmap.Create;
  Result.Width := W;
  Result.Height := H;
  Result.Canvas.CopyRect(DstRect, Skin.Canvas, SrcRect);
end;

var
  i: Integer;
begin
  if fSkinLoaded then
  begin
    for i := 1 to 11 do
      fSkin[i].Free;
    fSkinLoaded := False;
  end;

  fSkin[1] := GetImage(1, 1, 7, 13);
  fSkin[2] := GetImage(9, 1, 4, 13);
  fSkin[3] := GetImage(14, 1, 7, 13);
  fSkin[4] := GetImage(1, 15, 7, 4);
  fSkin[5] := GetImage(9, 15, 4, 4);
  fSkin[6] := GetImage(14, 15, 7, 4);
  fSkin[7] := GetImage(1, 20, 7, 7);
  fSkin[8] := GetImage(9, 20, 4, 7);
  fSkin[9] := GetImage(14, 20, 7, 7);

  fSkin[10] := GetImage(22, 1, 10, 16);
  fSkin[11] := GetImage(22, 18, 10, 16);

  fTopSpace := 5;
  fBottomSpace := 8;
  fLeftSpace := 7;
  fRightSpace := 7;
  fTipOutSize := 7;

  fMinHeight := 13 + 4 + 7;
  fMinWidth := 7 + 4 + 7 + 7;

  fSkinLoaded := True;
end;

procedure TChatSkin.SizeCalc(var Width, Height: Integer);
begin
  Inc(Width, fLeftSpace + fRightSpace + fTipOutSize);
  if Width < fMinWidth then
    Width := fMinWidth;

  Inc(Height, fTopSpace + fBottomSpace);
  if Height < fMinHeight then
    Height := fMinHeight;
end;

procedure TChatSkin.TextPos(CanvasHeight, TextHeight: Integer;
  LeftSide: Boolean; var X, Y: Integer);
begin
  if LeftSide then
    X := fTipOutSize + fLeftSpace
  else
    X := fLeftSpace;

  Y := 0;
  if CanvasHeight > (TextHeight + fTopSpace + fBottomSpace) then
    Y := (CanvasHeight - TextHeight - fTopSpace - fBottomSpace) div 2;
  Inc(Y, fTopSpace);
end;

function TChatSkin.MaxWidth(AWidth: Integer): Integer;
begin
  if AWidth < fMinWidth then AWidth := fMinWidth;
  Result := AWidth - (fLeftSpace + fRightSpace + fTipOutSize);
end;

{ TChatLabel }

procedure TChatLabel.DoAniTimerOnTimer(Sender: TObject);
begin
  if fFullDraw then
    fAniTimer.Enabled := False
  else
    Invalidate;
end;

function TChatLabel.FindLineInfo(X, Y: Integer): Integer;
var
  TextX, TextY, LineX, LineY, i: Integer;
  Info: TChatLineInfo;
begin
  Result := -1;

  fSkin.TextPos(Height, fTextHeight, fLeftSide, TextX, TextY);
  Dec(X, TextX);
  Dec(Y, TextY);

  if Y < 0 then exit;
  if Y > fTextHeight then exit;
  if X < 0 then exit;

  LineX := 0;
  LineY := 0;
  for i := 0 to fLineInfo.Count - 1 do
  begin
    Info := fLineInfo[i] as TChatLineInfo;
    if (LineY + Info.Height) < Y then
    begin
      if Info.LineBreak then
        Inc(LineY, Info.Height);
      continue;
    end;

    if (LineX + Info.Width) < X then
    begin
      if Info.LineBreak then exit;
      Inc(LineX, Info.Width);
      continue;
    end;

    Result := i;
    exit;
  end;
end;

procedure TChatLabel.FindLinePos(X, Y: Integer; var Line, Pos: Integer);
var
  TextX, TextY, LineX, LineY, i: Integer;
  Info: TChatLineInfo;
  FindText: UTF8String;
  DC: HDC;
  Size: TSize;
begin
  fSkin.TextPos(Height, fTextHeight, fLeftSide, TextX, TextY);
  Dec(X, TextX);
  Dec(Y, TextY);

  if Y < 0 then
  begin
    Line := 0;
    Pos := 0;
    exit;
  end;

  if Y > fTextHeight then
  begin
    Line := fLineInfo.Count;
    Pos := 0;
    exit;
  end;

  LineX := 0;
  LineY := 0;
  Line := fLineInfo.Count;
  Pos := 0;
  for i := 0 to fLineInfo.Count - 1 do
  begin
    Info := fLineInfo[i] as TChatLineInfo;
    if (LineY + Info.Height) < Y then
    begin
      if Info.LineBreak then
        Inc(LineY, Info.Height);
      continue;
    end;

    if X <= 0 then
    begin
      Line := i;
      Pos := 0;
      break;
    end;

    if (LineX + Info.Width) < X  then
    begin
      if Info.LineBreak then
      begin
        Line := i;
        Pos := Info.Length;
        break;
      end;

      Inc(LineX, Info.Width);
      continue;
    end;

    Line := i;
    FindText := UTF8Copy(fLines[Info.Index], Info.Offset, Info.Length);
    Pos := fBitmap.Canvas.TextFitInfo(FindText, X - LineX);
    break;
  end;
end;

function TChatLabel.GetSelectText: UTF8String;
var
  SelStartLine, SelStartPos, SelEndLine, SelEndPos: Integer;
  TextStart, TextEnd, i: Integer;
begin
  Result := '';

  if (fSelStartLine = fSelEndLine) and (fSelStartPos = fSelEndPos) then
  begin
    SelStartLine := -1;
    SelEndLine := -1;
  end
  else if (fSelStartLine < fSelEndLine) or
    ((fSelStartLine = fSelEndLine) and (fSelStartPos < fSelEndPos)) then
  begin
    SelStartLine := fSelStartLine;
    SelStartPos := fSelStartPos;
    SelEndLine := fSelEndLine;
    SelEndPos := fSelEndPos;
  end
  else
  begin
    SelEndLine := fSelStartLine;
    SelEndPos := fSelStartPos;
    SelStartLine := fSelEndLine;
    SelStartPos := fSelEndPos;
  end;

  if (SelStartLine < 0) or (SelEndLine < 0) then exit;

  if (SelStartLine < fLineInfo.Count) then
    TextStart := (fLineInfo[SelStartLine] as TChatLineInfo).Index
  else
    TextStart := fLines.Count;

  if (SelEndLine < fLineInfo.Count) then
    TextEnd := (fLineInfo[SelEndLine] as TChatLineInfo).Index
  else
    TextEnd := fLines.Count;

  if TextStart = TextEnd then
  begin
    if TextStart = fLines.Count then exit;
    Inc(SelStartPos, (fLineInfo[SelStartLine] as TChatLineInfo).Offset);
    Inc(SelEndPos, (fLineInfo[SelEndLine] as TChatLineInfo).Offset);
    Result := UTF8Copy(fLines[TextStart], SelStartPos,
      SelEndPos - SelStartPos);
  end else begin
    Inc(SelStartPos, (fLineInfo[SelStartLine] as TChatLineInfo).Offset);
    Result := UTF8Copy(fLines[TextStart], SelStartPos,
      UTF8Length(fLines[TextStart]) - SelStartPos + 1);
    for i := TextStart + 1 to TextEnd - 1 do
      Result := Result + #10 + fLines[i];
    if TextEnd = fLines.Count then exit;
    Inc(SelEndPos, (fLineInfo[SelEndLine] as TChatLineInfo).Offset);
    Result := Result + #10 + UTF8Copy(fLines[TextEnd], 1, SelEndPos - 1);
  end;
end;

function TChatLabel.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  Result := 0;
  if BufSize <= 0 then Exit;

  if Length(fCaption) >= BufSize then
  begin
    StrPLCopy(Buffer, fCaption, BufSize - 1);
    Result := BufSize - 1;
  end else begin
    StrPCopy(Buffer, fCaption);
    Result := length(fCaption);
  end;
end;

function TChatLabel.GetTextLen: Integer;
begin
  Result := Length(fCaption);
end;

procedure TChatLabel.SetTextBuf(Buffer: PChar);
var
  S: UTF8String;
  AWidth, AHeight: Integer;
begin
  S := StringReplace(TrimRight(UTF8String(Buffer)), #13#10, #10,
    [rfReplaceAll]);
  if fCaption = S then exit;
  fCaption := S;
  fLines.Text := S;
  fNeedUpdate := True;
  fShowTime := GetTickCount64;
  CalculatePreferredSize(AWidth, AHeight, False);
  Width := AWidth;
  Height := AHeight;
end;

procedure TChatLabel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  AWidth: Integer;
begin
  if (Owner = nil) or not (Owner is TWinControl) then
  begin
    inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
      WithThemeSpace);
  end else begin
    AWidth := (Owner as TWinControl).ClientWidth * 8 div 10;
    if AWidth <> fMaxWidth then
    begin
      fMaxWidth := AWidth;
      fNeedUpdate := True;
    end;

    if fStyle <> nil then
    begin
      if fStyle.TextStyles[fStyleNo].FontName <> Font.Name then
      begin
        Font.Name := fStyle.TextStyles[fStyleNo].FontName;
        fBitmap.Canvas.Font.Name := Font.Name;
        fNeedUpdate := True;
      end;

      if fStyle.TextStyles[fStyleNo].Size <> Font.Size then
      begin
        Font.Size := fStyle.TextStyles[fStyleNo].Size;
        fBitmap.Canvas.Font.Size := Font.Size;
        fNeedUpdate := True;
      end;

      if fStyle.TextStyles[fStyleNo].Color <> Font.Color then
      begin
        Font.Color := fStyle.TextStyles[fStyleNo].Color;
        fBitmap.Canvas.Font.Color := Font.Color;
        fNeedUpdate := True;
      end;

      if fStyle.TextStyles[fStyleNo].Style <> Font.Style then
      begin
        Font.Style := fStyle.TextStyles[fStyleNo].Style;
        fBitmap.Canvas.Font.Style := Font.Style;
        fNeedUpdate := True;
      end;
    end;

    CalculateSize(fSkin.MaxWidth(AWidth),PreferredWidth,PreferredHeight);

    fSkin.SizeCalc(PreferredWidth, PreferredHeight);
  end;
end;

procedure TChatLabel.CalculateSize(MaxWidth: integer; var NeededWidth,
  NeededHeight: integer);

var
  Str: UTF8String;
  LenByte, NextLink, LinkSize: Integer;

procedure FindLink;
var
  i: Integer;
begin
  NextLink := NextLink + LinkSize;
  LinkSize := 0;
  for i := NextLink + LinkSize to LenByte - 7 do
  begin
    if (Str[i] = 'h') and (Str[i + 4] = ':') and
      (Str[i + 7] <> ' ') and (Copy(Str, i, 7) = 'http://') then
    begin
      NextLink := i;
      LinkSize := 7;
      while (NextLink + LinkSize) <= LenByte do
      begin
        if Str[NextLink + LinkSize] = ' ' then break;
        Inc(LinkSize);
      end;
      break;
    end;
  end;
  if LinkSize = 0 then
    NextLink := 0;
end;

var
  Line: UTF8String;
  Total, Index, Offs, OffsByte, Len:Integer;
  LineHeight, LineOffs, LineOffsByte: Integer;
  LineLink, PreText, LinkText, PostText, Link: UTF8String;
  Max, i: Integer;
  Size: TSize;
  Info: TChatLineInfo;
begin
  if not fNeedUpdate then
  begin
    NeededWidth := fTextWidth;
    NeededHeight := fTextHeight;
    exit;
  end;

  NeededWidth := 0;
  NeededHeight := 0;
  Total := fLines.Count;

  if Total = 0 then exit;

  Len := 0;
  Offs := 1;
  Index := -1;
  fLineInfo.Clear;
  try
    while True do
    begin
      if (Offs > Len) then
      begin
        Inc(Index);
        if Index >= Total then break;
        Str := TrimRight(fLines[Index]);
        Offs := 1;
        OffsByte := 0;
        Len := UTF8Length(Str);
        LenByte := Length(Str);
        NextLink := 1;
        LinkSize := 0;
        Link := '';
        FindLink;
      end;

      if Len = 0 then
      begin
        Info := TChatLineInfo.Create;
        Info.Index := Index;
        Info.Offset := 0;
        Info.Length := 0;
        Info.Width := 0;
        Info.Height := fBitmap.Canvas.TextHeight('A') + fLineGap;
        Info.LineBreak := True;
        Info.Link := '';
        fLineInfo.Add(Info);
        Inc(NeededHeight, Info.Height);
        continue;
      end;

      Max := fBitmap.Canvas.TextFitInfo(Copy(Str, OffsByte + 1, LenByte), MaxWidth);
      if Max = 0 then Max := 1;
      Line := UTF8Copy(PChar(Str) + OffsByte, 1, Max + 1);
      if ((Offs + Max - 1) < Len) then
      begin
        for i := Length(Line) downto 1 do
        begin
          if Line[i] = ' ' then
          begin
            Max := UTF8Length(PChar(Line), i - 1);
            break;
          end;
        end;
      end;
      Line := UTF8Copy(PChar(Str) + OffsByte, 1, Max);

      if (NextLink + LinkSize - 1) <= OffsByte then
      begin
        Link := '';
        FindLink;
      end;

      if (NextLink > OffsByte) then
        Link := Copy(Str, NextLink, LinkSize);

      Size := fBitmap.Canvas.TextExtent(Line);
      if Size.cx > NeededWidth then
        NeededWidth := Size.cx;
      Inc(NeededHeight, Size.cy + fLineGap);

      if Link = '' then
      begin
        Info := TChatLineInfo.Create;
        Info.Index := Index;
        Info.Offset := Offs;
        Info.Length := Max;
        Info.Width := Size.cx;
        Info.Height := Size.cy + fLineGap;
        Info.Link := '';
        Info.LineBreak := True;
        fLineInfo.Add(Info);
      end else begin
        LineLink := Line;
        LineHeight := Size.cy + fLineGap;
        LineOffs := Offs;
        LineOffsByte := OffsByte;

        while True do
        begin
          PreText := '';
          PostText := '';

          if NextLink > LineOffsByte then
            PreText := Copy(LineLink, 1, NextLink - LineOffsByte - 1);

          if (NextLink + LinkSize) < (LineOffsByte + Length(LineLink)) then
            PostText := Copy(LineLink, NextLink + LinkSize - LineOffsByte,
              Length(LineLink));

          LinkText := Copy(LineLink, Length(PreText) + 1,
            Length(LineLink) - Length(PreText) - Length(PostText));

          if PreText <> '' then
          begin
            Info := TChatLineInfo.Create;
            Info.Index := Index;
            Info.Offset := LineOffs;
            Info.Length := UTF8Length(PreText);
            Info.Width := fBitmap.Canvas.TextWidth(PreText);
            Info.Height := LineHeight;
            Info.Link := '';
            Info.LineBreak := False;
            fLineInfo.Add(Info);
            Inc(LineOffs, Info.Length);
          end;

          Info := TChatLineInfo.Create;
          Info.Index := Index;
          Info.Offset := LineOffs;
          Info.Length := UTF8Length(LinkText);
          Info.Width := fBitmap.Canvas.TextWidth(LinkText);
          Info.Height := LineHeight;
          Info.Link := Link;
          Info.LineBreak := (PostText = '');
          fLineInfo.Add(Info);
          Inc(LineOffs, Info.Length);

          if PostText = '' then break;

          FindLink;

          if (NextLink = 0) or (NextLink > LineOffsByte + Length(LineLink)) then
          begin
            Link := '';
            Info := TChatLineInfo.Create;
            Info.Index := Index;
            Info.Offset := LineOffs;
            Info.Length := UTF8Length(PostText);
            Info.Width := fBitmap.Canvas.TextWidth(PostText);
            Info.Height := LineHeight;
            Info.Link := '';
            Info.LineBreak := True;
            fLineInfo.Add(Info);
            break;
          end;

          LineLink := PostText;
          Link := Copy(Str, NextLink, LinkSize);
          Inc(LineOffsByte, Length(PreText) + Length(LinkText))
        end;
      end;

      Inc(Offs, Max);
      Inc(OffsByte, Length(Line));
      if (OffsByte < LenByte) and (Str[OffsByte + 1] = ' ') then
      begin
        inc(Offs);
        inc(OffsByte);
      end;
    end;
  finally
    if (fTextWidth <> NeededWidth) or (fTextHeight <> NeededHeight) then
    begin
      fTextWidth := NeededWidth;
      fTextHeight := NeededHeight;
    end;
    fNeedUpdate := False;
    if not fFullDraw then
      fAniTimer.Enabled := True;
  end;
end;

procedure TChatLabel.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if Message.WheelDelta > 0 then
    Parent.Perform(LM_VSCROLL, SB_LINEUP, 0)
  else if Message.WheelDelta < 0 then
    Parent.Perform(LM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TChatLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Bottom, ScrollDelta, Line, Pos: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  fMouseX := X;
  fMouseY := Y;

  if fSelect then
  begin
    if Owner is TRichView then
    begin
      ScrollDelta := 0;
      if Top < 0 then
      begin
        if Y < (-Top) then ScrollDelta := -1;
        if Y < (-Top - 20) then ScrollDelta := -10;
      end;
      Bottom := (Owner as TRichView).ClientHeight - Top;
      if (Bottom) < ClientHeight then
      begin
        if Y > Bottom then ScrollDelta := 1;
        if Y > (Bottom + 20) then ScrollDelta := 10;
      end;

      if ScrollDelta <> fScrollDelta then
      begin
        (Owner as TRichView).ScrollOnControl(Self, ScrollDelta);
        fScrollDelta := ScrollDelta;
      end;
    end;

    if (Top < 0) and (Y < Top) then
      FindLinePos(0, Top, Line, Pos)
    else
      FindLinePos(X, Y, Line, Pos);

    if (fSelEndLine <> Line) or (fSelEndPos <> Pos) then
    begin
      fSelEndLine := Line;
      fSelEndPos := Pos;
      Invalidate;
    end;
  end else begin
    Line := FindLineInfo(X, Y);
    if Line = -1 then
      Cursor := crDefault
    else if (fLineInfo[Line] as TChatLineInfo).Link = '' then
      Cursor := crIBeam
    else
      Cursor := crHandPoint;
  end;
end;

procedure TChatLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Line: Integer;
begin
  if Button <> mbLeft then exit;
  if fSelect then
  begin
    Cursor := crDefault;
    fSelect := False;
    if (fClickLine >= 0) and (fClickLine = fSelStartLine) and
      (fSelStartLine = fSelEndLine) and (fSelStartPos = fSelEndPos) and
      ((fLineInfo[fClickLine] as TChatLineInfo).Link <> '') then
    begin
      OpenURL((fLineInfo[fClickLine] as TChatLineInfo).Link);
    end;
    fClickLine := -1;
  end;
  if Owner is TRichView then
    (Owner as TRichView).MouseUpOnControl(Self, Button, X, Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TChatLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Line, Pos: Integer;
begin
  if Button <> mbLeft then exit;
  fClickLine := FindLineInfo(X, Y);
  FindLinePos(X, Y, Line, Pos);
  fSelStartLine := Line;
  fSelStartPos := Pos;
  fSelEndLine := Line;
  fSelEndPos := Pos;
  fSelect := True;
  fScrollDelta := 0;
  Cursor := crIBeam;
  (Parent as TRichView).SelectOnControl(Self);
  Invalidate;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TChatLabel.DblClick;
var
  Line, Pos, LineIndex, StartPos, EndPos, i: Integer;
  LineInfo: TChatLineInfo;
  Str: UTF8String;
begin
  inherited DblClick;

  fSelect := False;
  fSelStartLine := -1;
  fSelStartPos := -1;
  fSelEndLine := -1;
  fSelEndPos := -1;
  Invalidate;

  FindLinePos(fMouseX, fMouseY, Line, Pos);
  if (Line < 0) or (Line >= fLineInfo.Count) then exit;
  LineInfo := fLineInfo[Line] as TChatLineInfo;
  LineIndex := LineInfo.Index;

  Str := UTF8Copy(fLines[LineIndex], 1, LineInfo.Offset + Pos);
  StartPos := 0;
  if Str[Length(Str)] = ' ' then exit;
  for i := Length(Str) - 1 downto 1 do
    if Str[i] = ' ' then
    begin
      StartPos := i;
      break;
    end;
  StartPos := UTF8Length(Copy(Str, 1, StartPos + 1));

  Str := UTF8Copy(fLines[LineIndex], StartPos + 1,
    UTF8Length(fLines[LineIndex]) - StartPos);
  EndPos := 0;
  for i := 1 to Length(Str) do
    if Str[i] = ' ' then
    begin
      EndPos := i;
      break;
    end;
  if EndPos = 0 then
    EndPos := UTF8Length(fLines[LineIndex]) + 1
  else
    EndPos := UTF8Length(Copy(Str, 1, i)) + StartPos;

  for i := Line downto 0 do
  begin
    LineInfo := fLineInfo[i] as TChatLineInfo;
    if LineInfo.Index <> LineIndex then exit;
    if LineInfo.Offset > StartPos then continue;
    fSelStartLine := i;
    fSelStartPos := StartPos - LineInfo.Offset;
    break;
  end;

  for i := Line to fLineInfo.Count - 1 do
  begin
    LineInfo := fLineInfo[i] as TChatLineInfo;
    if LineInfo.Index <> LineIndex then
    begin
      fSelStartLine := -1;
      fSelStartPos := -1;
      exit;
    end;
    if (LineInfo.Offset + LineInfo.Length) < EndPos then continue;
    fSelEndLine := i;
    fSelEndPos := EndPos - LineInfo.Offset;
    break;
  end;

  (Parent as TRichView).SelectOnControl(Self);
  Invalidate;
end;

procedure TChatLabel.Paint;
var
  Bottom, Line, Pos: Integer;
  Skip, View, i: Integer;
  LineInfo: TChatLineInfo;
  TextBorder, TextLeft, TextTop, X, W: integer;
  LabelText, PreText, SelText, PostText: UTF8String;
  FontColor, OldFontColor: TColor;
  Scroll: Integer;
  SelStartLine, SelStartPos, SelEndLine, SelEndPos: Integer;
  OnSelect: Boolean;
  ClickStart, ClickEnd: Integer;
  ClickLink: UTF8String;
begin
  if not Assigned(Parent) then exit;

  if fSelect and (fScrollDelta <> 0) then
  begin
    if fScrollDelta < 0 then
      FindLinePos(0, -Top, Line, Pos)
    else begin
      Bottom := (Owner as TRichView).ClientHeight - Top;
      FindLinePos(Width, Bottom, Line, Pos);
    end;

    fSelEndLine := Line;
    fSelEndPos := Pos;
  end;

  if (fSelStartLine = fSelEndLine) and (fSelStartPos = fSelEndPos) then
  begin
    SelStartLine := -1;
    SelEndLine := -1;
  end
  else if (fSelStartLine < fSelEndLine) or
    ((fSelStartLine = fSelEndLine) and (fSelStartPos < fSelEndPos)) then
  begin
    SelStartLine := fSelStartLine;
    SelStartPos := fSelStartPos;
    SelEndLine := fSelEndLine;
    SelEndPos := fSelEndPos;
  end
  else
  begin
    SelEndLine := fSelStartLine;
    SelEndPos := fSelStartPos;
    SelStartLine := fSelEndLine;
    SelStartPos := fSelEndPos;
  end;

  if fFullDraw then
    Scroll := 0
  else if not fFullDraw then
  begin
    Scroll := Width - (fMaxWidth * (GetTickCount64 - fShowTime + 20) div 200);
    if Scroll < 0 then
    begin
      fFullDraw := True;
      Scroll := 0;
    end;
  end;

  fSkin.TextPos(Height, fTextHeight, fLeftSide, TextBorder, TextTop);
  if fLeftSide then
    Dec(TextBorder, Scroll)
  else
    Inc(TextBorder, Scroll);

  if Top <= 0 then
  begin
    Skip := -Top;
    View := Height - Skip;
    if View > Parent.ClientHeight then
      View := Parent.ClientHeight;
  end
  else
  begin
    Skip := 0;
    View := Parent.ClientHeight - Top;
    if View > Height then View := Height;
  end;

  if View <= 0 then exit;

  if (SelEndLine = fLineInfo.Count - 1) and
    (SelEndPos >= (fLineInfo[SelEndLine] as TChatLineInfo).Length) then
  begin
    SelEndLine := fLineInfo.Count;
    SelEndPos := 0;
  end;

  if (SelStartLine = 0) and (SelStartPos = 0) and
    (SelEndLine = fLineInfo.Count) then
    fSelectSkin.Draw(Canvas, Width, Height, Skip, View, Scroll, fLeftSide)
  else
    fSkin.Draw(Canvas, Width, Height, Skip, View, Scroll, fLeftSide);

  ClickStart := fLineInfo.Count;
  ClickEnd := 0;

  if fSelect and (fClickLine >= 0) and (fClickLine = fSelStartLine) and
    (fSelStartLine = fSelEndLine) and (fSelStartPos = fSelEndPos) and
    ((fLineInfo[fSelStartLine] as TChatLineInfo).Link <> '') then
  begin
    ClickLink := (fLineInfo[fClickLine] as TChatLineInfo).Link;
    ClickStart := fClickLine;
    while (ClickStart > 0) do
    begin
      if (fLineInfo[ClickStart - 1] as TChatLineInfo).LineBreak then
        break;
      if (fLineInfo[ClickStart - 1] as TChatLineInfo).Link <> ClickLink then
        break;
      Dec(ClickStart);
    end;

    ClickEnd := fClickLine;
    while (ClickEnd < fLineInfo.Count - 1) do
    begin
      if (fLineInfo[ClickEnd + 1] as TChatLineInfo).Link <> ClickLink then
        break;
      Inc(ClickEnd);
      if (fLineInfo[ClickEnd] as TChatLineInfo).LineBreak then
        break;
    end;
  end;

  with Canvas do
  begin
    Brush.Color := fSelectColor;
    Brush.Style := bsClear;
    Font := Self.Font;
    OldFontColor := Font.Color;
    OnSelect := False;
    TextLeft := TextBorder;
    for i := 0 to fLineInfo.Count - 1 do
    begin
      if TextTop > (Skip + View) then break;
      LineInfo := fLineInfo[i] as TChatLineInfo;
      if LineInfo.Link = '' then
      begin
        FontColor := OldFontColor;
        Font.Style := Font.Style - [fsUnderline];
      end else begin
        if (i >= ClickStart) and (i <= ClickEnd) then
          FontColor := clPurple
        else
          FontColor := clBlue;
        Font.Style := Font.Style + [fsUnderline];
      end;
      if (TextTop + LineInfo.Height) < Skip then
      begin
        if i = SelStartLine then OnSelect := True;
        if i = SelEndLine then OnSelect := False;
        Inc(TextLeft, LineInfo.Width);
        if LineInfo.LineBreak then
        begin
          Inc(TextTop, LineInfo.Height);
          TextLeft := TextBorder;
        end;
        continue;
      end;
      LabelText := UTF8Copy(fLines[LineInfo.Index], LineInfo.Offset,
        LineInfo.Length);
      if (i <> SelStartLine) and (i <> SelEndLine) then
      begin
        if OnSelect then
        begin
          Brush.Style := bsSolid;
          FillRect(TextLeft, TextTop, TextLeft + LineInfo.Width + 1,
            TextTop + LineInfo.Height);
          Brush.Style := bsClear;
        end;
        if Assigned(fStyle) and fStyle.Shadow then
        begin
          Font.Color := clWhite;
          TextOut(TextLeft + 1, TextTop + 1, LabelText);
        end;
        Font.Color := FontColor;
        TextOut(TextLeft, TextTop, LabelText);
      end else begin
        PreText := '';
        PostText := '';
        if (i = SelStartLine) then
          PreText := UTF8Copy(LabelText, 1, SelStartPos);
        if (i = SelEndLine) then
          PostText := UTF8Copy(LabelText, SelEndPos + 1,
            UTF8Length(LabelText) - SelEndPos);
        SelText := UTF8Copy(LabelText, UTF8Length(PreText) + 1,
          UTF8Length(LabelText) - UTF8Length(PreText) - UTF8Length(PostText));

        X := TextLeft;

        if PreText <> '' then
        begin
          if Assigned(fStyle) and fStyle.Shadow then
          begin
            Font.Color := clWhite;
            TextOut(X + 1, TextTop + 1, PreText);
          end;
          Font.Color := FontColor;
          TextOut(X, TextTop, PreText);
          Inc(X, TextWidth(PreText));
        end;

        if SelText <> '' then
        begin
          Brush.Style := bsSolid;
          W := TextWidth(SelText);
          FillRect(X, TextTop, X + W + 1, TextTop + LineInfo.Height);
          Brush.Style := bsClear;
          if Assigned(fStyle) and fStyle.Shadow then
          begin
            Font.Color := clWhite;
            TextOut(X + 1, TextTop + 1, SelText);
          end;
          Font.Color := FontColor;
          TextOut(X, TextTop, SelText);
          Inc(X, W);
        end;

        if PostText <> '' then
        begin
          if Assigned(fStyle) and fStyle.Shadow then
          begin
            Font.Color := clWhite;
            TextOut(X + 1, TextTop + 1, PostText);
          end;
          Font.Color := FontColor;
          TextOut(X, TextTop, PostText);
        end;

        if i = SelStartLine then OnSelect := True;
        if i = SelEndLine then OnSelect := False;
      end;

      Inc(TextLeft, LineInfo.Width);
      if LineInfo.LineBreak then
      begin
        Inc(TextTop, LineInfo.Height);
        TextLeft := TextBorder;
      end;
    end;
    Font.Color := OldFontColor;
  end;
end;

procedure TChatLabel.SelectAll;
begin
  fSelect := False;
  if (fSelStartLine <> 0) or (fSelStartPos <> 0) or
    (fSelEndLine <> fLineInfo.Count) or (fSelEndPos <> 0) then
  begin
    fSelStartLine := 0;
    fSelStartPos := 0;
    fSelEndLine := fLineInfo.Count;
    fSelEndPos := 0;
    Invalidate;
  end;
end;

procedure TChatLabel.Deselect;
begin
  fSelect := False;
  if (fSelStartLine <> -1) or (fSelEndLine <> -1) then
  begin
    fSelStartLine := -1;
    fSelStartPos := -1;
    fSelEndLine := -1;
    fSelEndPos := -1;
    Invalidate;
  end;
end;

procedure TChatLabel.MouseUpOnParent(Button: TMouseButton; X, Y: Integer);
begin
  if Button <> mbLeft then exit;
  if fSelect then
  begin
    Cursor := crDefault;
    fSelect := False;
  end;
end;

procedure TChatLabel.ScrollDeltaOnParent(Delta: Integer);
begin
  fScrollDelta := Delta;
end;

procedure TChatLabel.ForceCalculateSize;
var
  AWidth, AHeight: Integer;
begin
  CalculatePreferredSize(AWidth, AHeight, False);
  Width := AWidth;
  Height := AHeight;
end;

constructor TChatLabel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  fBitmap := TBitmap.Create;
  fBitmap.Canvas.Font.Name := Font.Name;
  fBitmap.Canvas.Font.Size := Font.Size;
  fBitmap.Canvas.Font.Color := Font.Color;
  fBitmap.Canvas.Font.Style := Font.Style;

  fLines := TStringList.Create;
  fLineInfo := TObjectList.Create;

  fLeftSide := True;
  fSkin := nil;
  fSelectSkin := nil;
  SetWordWrap(True);

  fSelStartLine := -1;
  fSelEndLine := -1;

  if (TheOwner <> nil) and (TheOwner is TRichView) then
    Style := (TheOwner as TRichView).Style
  else
    Style := nil;
  StyleNo := 0;

  fMaxWidth := 0;
  fTextWidth := 0;
  fTextHeight := 0;
  fLineGap := 2;
  fNeedUpdate := True;
  fShowTime := 0;
  fFullDraw := False;

  fAniTimer := TTimer.Create(Self);
  fAniTimer.Enabled := False;
  fAniTimer.Interval := 10;
  fAniTimer.OnTimer := @DoAniTimerOnTimer;
end;

destructor TChatLabel.Destroy;
var
  i: Integer;
begin
  fBitmap.Free;
  fAniTimer.OnTimer := nil;
  fAniTimer.Free;
  fLineInfo.Free;
  fLines.Free;
  inherited Destroy;
end;

end.
