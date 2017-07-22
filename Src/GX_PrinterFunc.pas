unit GX_PrinterFunc;

{$I GX_CondDefine.inc}

{$IFNDEF ACEREPORTER}
interface implementation
{$ELSE ACEREPORTER}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Classes, Windows, Printers, Controls, Forms, Graphics,
  ACEOut;

function GetTextWidth(C: TAceCanvas; const S: string): Integer;
function GetTextHeight(C: TAceCanvas; const S: string): Integer;
procedure DrawBitmap(C: TAceCanvas; ilList: TImageList; Index: Integer; X,Y: Integer);
function GetPageHeight: Integer;
function GetPageWidth: Integer;
procedure FrameRect(AceCanvas: TAceCanvas;Rect:TRect);

var
  TestForm: TForm = nil;

const
  PR_OffsetX = 30;
  PR_OffsetY = 20;

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  SysUtils;

procedure CreateTestForm;
begin
  if not Assigned(TestForm) then
  begin
    TestForm := TForm.Create(nil);
    TestForm.Name := 'GX_PrinterFunc_TestForm_999';
  end;
end;

function GetTextHeight(C: TAceCanvas; const S: string): Integer;
begin
  CreateTestForm;
  with TestForm.Canvas do
  begin
    Font.Assign(C.Font);
    Result := TextHeight(S);
  end;
end;

function GetTextWidth(C: TAceCanvas; const S: string): Integer;
begin
  CreateTestForm;
  with TestForm.Canvas do
  begin
    Font.Assign(C.Font);
    Result:=TextWidth(S);
  end;
end;

procedure FrameRect(AceCanvas: TAceCanvas; Rect: TRect);
begin
  with AceCanvas do
  begin
    MoveTo(Rect.Left,Rect.Top);
    LineTo(Rect.Right,Rect.Top);
    LineTo(Rect.Right,Rect.Bottom);
    LineTo(Rect.Left,Rect.Bottom);
    LineTo(Rect.Left,Rect.Top);
  end;
end;

procedure DrawBitmap(C: TAceCanvas; ilList: TImageList; Index: Integer; X, Y: Integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    ilList.GetBitmap(Index, Bitmap);
    C.Draw(X, Y, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

function GetPageHeight: Integer;
begin
  Result := Printer.PageHeight;
end;

function GetPageWidth: Integer;
begin
  Result := Printer.PageWidth;
end;

initialization

finalization
  TestForm.Free;
  TestForm := nil;
{$ENDIF ACEREPORTER}

end.
