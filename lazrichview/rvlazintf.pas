unit RVLazIntf;

{$mode objfpc}{$H+}

interface

uses Types, LCLType, LMessages;

type
  TWMSize           = TLMSize;
  TWMEraseBkgnd     = TLMEraseBkgnd;
  TMessage          = TLMessage;
  TSize             = Types.TSize;
  TWMHScroll        = TLMHScroll;
  TWMVScroll        = TLMVScroll;
  TWMKeyDown        = TLMKeyDown;
  TWMGetDlgCode     = TLMNoParams;
  
  
const
  WM_SIZE           = LM_SIZE;
  WM_ERASEBKGND     = LM_ERASEBKGND;
  WM_HSCROLL        = LM_HSCROLL;
  WM_VSCROLL        = LM_VSCROLL;
  WM_KEYDOWN        = LM_KEYDOWN;
  WM_GETDLGCODE     = LM_GETDLGCODE;
  CM_MOUSELEAVE     = $B000+20;
  
// todo:
//        - this should go to TWidgetset.GetTextExtentExPoint
//          lets keep it here by now
//        - merge warning message in TWidgetset.GetTextExtentExPoint
//          about
function MyGetTextExtentExPoint(DC:HDC; Str:PChar; Count,MaxWidth:Integer;
  AMax: PInteger; PartialWidths:PInteger; var sz: TSize): boolean;

  
implementation

uses LCLIntf, lazutf8;


function MyGetTextExtentExPoint(DC:HDC; Str:PChar; Count,MaxWidth:Integer;
  AMax: PInteger; PartialWidths:PInteger; var sz: TSize): boolean;
var
  TestCount: Integer;
  TestSize : TSize;
  Increment: Integer;
  UTF8Start: Integer;

  function CalcTestSize: boolean;
  begin
    result := GetTextExtentPoint(DC,Str,TestCount,TestSize);
  end;

  procedure Report;
  begin
    GetTextExtentExPoint(DC, Str, Count, MaxWidth, @TestCount, nil, TestSize);
    if (TestCount<>AMax^) or (TestSize.Cx<>Sz.Cx) then begin
      WriteLn('------> Diferencia');
      WriteLn('    AMax^=',AMax^,    '       Sz.Cx=',Sz.Cx);
      WriteLn('TestCount=',TestCount,' TestSize.Cx=',testSize.Cx);
    end;
  end;

  function UTF8CharStart(BytePos: Integer): Integer;
  begin
    if (BytePos > Count) then
      Result := Count
    else if (BytePos < 0) then
      Result := 0
    else
    begin
      while (BytePos > 0) and (ord(Str[BytePos]) and %11000000=%10000000) do
        dec(BytePos);
      Result := BytePos;
    end;
  end;

begin
  TestCount := UTF8CharStart(Count);
  //TestCount := Count;

  result := CalcTestSize;
  if not result then
    exit;

  Sz := TestSize;
  AMax^ := TestCount;

  if (TestSize.Cx=0) or (TestSize.cx<MaxWidth) then begin
    //Report;
    exit;
  end;

  TestCount := (MaxWidth * TestCount) div TestSize.Cx;
  TestCount := UTF8CharStart(TestCount);

  Result := CalcTestSize;
  if not result then
    exit;

  AMax^ := TestCount;

  if TestSize.cx<MaxWidth then Increment:=1 else
  if TestSize.cx>MaxWidth then Increment:=-1
  else                         Increment:=0;

  while ((Increment>0)and(TestCount<Count)) or
        ((Increment<0)and(TestCount>0))
  do begin
    if (Increment > 0) then
    begin
      Inc(TestCount, UTF8CharacterLength(Str + TestCount));
    end else begin
      TestCount := UTF8CharStart(TestCount - 1);
    end;
    //Inc(TestCount, Increment);
    result := CalcTestSize;

    if not Result or
      ((Increment>0)and(TestSize.cx>MaxWidth))
    then
      // no valid or old AMax was correct
      break;

    AMax^ := TestCount;

    if ((Increment<0)and(TestSize.cx<=MaxWidth))
    then
      // AMax just become correct
      break;
  end;
  //Report;
end;


end.

