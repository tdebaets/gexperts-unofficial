unit GX_AsciiChart;

{$I GX_CondDefine.inc}

//! StH: This unit has **NOT** been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{  A form that displays a character chart for use with delphi.
   Nominally intended for inclusion with GExperts an excellent collection
   of Delphi experts by Gerald Nunn - gnunn@interlog.com}
{ Disclaimer : This code is freeware.
               It may be used by anyone in any way that they desire.
               If this code or any derivative of it causes the outbreak
               of World War 3 or any lesser event then you assume
               all responsibility for such actions.
      Regards,
               Taz Higgins - taz@taz.compulink.co.uk © 1997}

{Added in the following features and fixes/updates in 1.04
1)  Save and Restore the text in the text box to the registry
2)  Draw in all the 0-31 characters as the ASCII code
3)  Added in a pair of toggle button that displays the character value as
 either Integer or Hex.
4)  Tidied up the formcreate routine a little, moved the windowplacement
 stuff to the end so that drawing hapens at the correct time, and removed
 many unnecessary begin/end blocks.
5)  Removes the 3 font sizing buttons and replaced them with an editbox/updown
 so that it's much smoother, and you can have whatever font size you want, not
 being restricted to the pregenerated sizes.  Limits 6 to 20.
6)  I check that I am changing fontsize to remove an unnecessary redraw if
 clicking on the same size menu choice as already selected.
7)  Use setbounds rather than left then width in formresize and in
 showfontpalette to improve drawing of the edit box a little.
8)  Updated the formpaint routine to improve the drawing speed and reduce
 screen flicker by removing the frame3d routine and inlining it, in
 dedicated routines, drawing left and top first, then changing pen
 and drawing bottom and right.  Lots of reductions in assignments to the
 canvas which really speeds things up.
9)  Added OldHorizMult and OldVertMult variables to the form, used in the
 formsize to determine if we really need to redrawn the screen in order
 to reduce screen flicker on resizing.  Makes a big difference
10) Changed the fontsize routine to cast the sender as TComponent to read
 the tag property from - makes it more generic.
11) Modified the DrawCharacter routine to accept the passed HorizMult and
 VertMult values already calculated, that way I don't need to
 recalculate them in the DrawCharacter routine speeds drawing up.
12) Moved the form level variables to be private rather than public.  They
 don't need to be public, so they shouldn't be.
13) The fontsize of the textbox now fixed at 8 pt.  No need to vary it and it
 solves a problem with large fonts
14) Changed the drawcharacters routine to solve incorrect clipping/drawing
 problems - text will get clipped at the frame edges for that cell.
15) Changed the MinMax sizes to use the systemmetrics frame and caption sizes
 so that things are sized better.
16) Added in some custom hint processing when over characters values 0-31 to show
 a textual interpretation of the character.  When over any other character it
 shows a larger version of that character.  The font used is always that of the
 form, the size is adjustable, using the value as stored in
 'Software\GExperts\character chart\Zoom Font Size'
 Gerald, perhaps you should add something for this in GExperts config;
19) Changed the requirements to double click to put chartacters in the edit box
 to be a single click instead.
}

interface

uses
  SysUtils, Windows, Messages,
  Graphics, Forms, ExtCtrls, Controls,
  Classes, StdCtrls, Buttons, Menus, Registry, ComCtrls,
  ToolIntf, ExptIntf,
  GX_Experts;

type
  TfmAsciiChart = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    btnCharHigh: TSpeedButton;
    FontComboName: TComboBox;
    txtChars: TEdit;
    PopupMenu1: TPopupMenu;
    ShowFontPalette1: TMenuItem;
    N1: TMenuItem;
    About1: TMenuItem;
    ShowLowCharacters1: TMenuItem;
    ShowHighCharacters1: TMenuItem;
    N2: TMenuItem;
    FontSize8: TMenuItem;
    FontSize10: TMenuItem;
    FontSize12: TMenuItem;
    N3: TMenuItem;
    btnCharInt: TSpeedButton;
    N4: TMenuItem;
    CVInt: TMenuItem;
    CVHex: TMenuItem;
    HintTimer: TTimer;
    FontSizeEdit: TEdit;
    FontSizeUpDown: TUpDown;
    btnCharLow: TSpeedButton;
    btnCharHex: TSpeedButton;
    StatusBar: TStatusBar;
    HintActive1: TMenuItem;
    Help: TMenuItem;
    procedure FormPaint(Sender: TObject);
    procedure btnCharHighClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontComboNameChange(Sender: TObject);
    procedure btnSizeClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure About1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ShowFontPalette1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure btnCharIntClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HintTimerTimer(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FontSizeUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FontSizeEditChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure HintActive1Click(Sender: TObject);
    procedure FontComboNameEnter(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  private
    BaseNum: Integer;
    CharPos: Integer;
    FontSize: Integer;
    FontName: string;
    ShowFontPalette: Boolean;
    ShowHex: Boolean;
    OldHorizMult: Integer;
    OldVertMult: Integer;
    FActive: Boolean;
    FHint: THintWindow;
    OldCharPos: Integer;
    ZoomFontSize: Integer;
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure DrawCharacter(const CharValue: Integer; const CharText: string; const HorizMult, VertMult: Integer);
    procedure GetFonts;
    procedure SetFontName(const NewFontName: string);
    procedure KillHint;
  public
    procedure DoHint(Sender: TObject);
    procedure DoDeactivate(Sender: TObject);
  end;

  TASCIIExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

var
  fmAsciiChart: TfmAsciiChart = nil;
  ASCIIExpert: TASCIIExpert = nil;

implementation

{$R *.DFM}

uses
  GX_GExperts, GX_ConfigurationInfo;

const
  DescLow: array[0..31] of string =
    ('Null', 'Start of Header', 'Start of Text', 'End of Text',
    'End of Transmission', 'Enquiry', 'Acknowledgement', 'Bell',
    'Backspace', 'Horizontal Tab', 'Linefeed', 'Vertical Tab',
    'Form Feed', 'Carriage Return', 'Shift Out', 'Shift in',
    'Delete', 'Device Control 1', 'Device Control 2', 'Device Control 3',
    'Device Control 4', 'Negative Acknowledge', 'Synchronize', 'End Block',
    'Cancel', 'End Message', 'Sub', 'Escape',
    'Form Separator', 'Group Separator', 'Record Separator', 'Unit Separator');

procedure TfmAsciiChart.FormCreate(Sender: TObject);
var
  Reg: TRegistry;
  BaseRegKey: string;
  Place: TWindowPlacement;
  SetPlace: Boolean;
begin
  // do not localize any of the following items
  GetFonts;
  { Initial values - if they don't exist in the registry
    then everything is still OK }
  BaseNum := 0;
  FontSize := 8;
  FontName := 'MS Sans Serif';
  ShowFontPalette := True;
  ShowHex := False;
  SetPlace := False;
  OldHorizMult := 0;
  OldVertMult := 0;
  ZoomFontSize := 32;
  BaseRegKey := ConfigInfo.RegKey+'\GExperts\ASCII Chart';
  Reg := TRegistry.Create;
  try
    // Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(BaseRegKey, False) then
    begin
      if Reg.ValueExists('Font Size') then
        FontSize := Reg.ReadInteger('Font Size');
      if Reg.ValueExists('Font Name') then
        FontName := Reg.ReadString('Font Name');
      if Reg.ValueExists('Font Base') then
        BaseNum := Reg.ReadInteger('Font Base');
      if Reg.ValueExists('Show Font Palette') then
        ShowFontPalette := Reg.ReadBool('Show Font Palette');
      if Reg.ValueExists('Show Hex') then
        ShowHex := Reg.ReadBool('Show Hex');
      if Reg.ValueExists('Zoom Font Size') then
        ZoomFontSize := Reg.ReadInteger('Zoom Font Size');
      if Reg.ValueExists('Edit Display Text') then
        txtChars.Text := Reg.ReadString('Edit Display Text');
      if Reg.ValueExists('Show Hint') then
        HintActive1.Checked := Reg.ReadBool('Show Hint');
      if Reg.ValueExists('Window Location') then
      begin
        Reg.ReadBinaryData('Window Location', Place, SizeOf(Place));
        SetPlace := True;
      end;
    end;
  finally
    Reg.Free;
  end;

  if BaseNum = 0 then
    btnCharLow.Down := True
  else
    btnCharHigh.Down := True;

  if ShowHex = True then
    btnCharHex.Down := True
  else
    btnCharInt.Down := True;

  FontSizeUpDown.Position := FontSize;
  SetFontName(FontName);
  txtChars.Font.Name := FontName;
  ShowFontPalette := not ShowFontPalette;
  ShowFontPalette1Click(Self);
  if SetPlace then
    SetWindowPlacement(Handle, @Place);
  Application.OnHint := DoHint;
  Application.OnDeactivate := DoDeactivate;
end;

procedure TfmAsciiChart.FormDestroy(Sender: TObject);
begin
  fmAsciiChart := nil;
end;

procedure TfmAsciiChart.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Reg: TRegistry;
  BaseRegKey: string;
  Place: TWindowPlacement;
begin
  // do not localize any of the following files
  BaseRegKey := ConfigInfo.RegKey+'\GExperts\ASCII Chart';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(BaseRegKey, True) then
    begin
      Reg.WriteInteger('Font Size', FontSize);
      Reg.WriteString('Font Name', FontName);
      Reg.WriteInteger('Font Base', BaseNum);
      Reg.WriteBool('Show Font Palette', ShowFontPalette);
      Reg.WriteString('Edit Display Text', txtChars.Text);
      Reg.WriteBool('Show Hex', ShowHex);
      Reg.WriteInteger('Zoom Font Size', ZoomFontSize);
      Reg.WriteBool('Show Hint', HintActive1.Checked);
      Place.length := SizeOf(TWindowPlacement);
      GetWindowPlacement(Handle, @Place);
      Reg.WriteBinaryData('Window Location', Place, SizeOf(Place));
    end;
  finally
    Reg.Free;
  end;
end;

procedure TfmAsciiChart.FormPaint(Sender: TObject);
{ It's much quicker to draw the characters in one style, change
  styles then draw all the others in the other style than do each draw
  one after another changing styles as I go along }
var
  I, J: Integer; { general loop counters }
  X, Y: Integer; { screen pixel locations }
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  Start: Integer; { low charnum for character rendering }
begin
  if BaseNum = 0 then Start := 32 else Start := 0;
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - Panel1.Height - StatusBar.Height) div 16;
  Canvas.Brush.Style := bsClear;
  { draw the character value as Int or Hex on screen }
  Canvas.Font.Name := 'MS Sans Serif';  // do not localize
  Canvas.Font.Size := 8;
  Canvas.Font.Color := clGrayText;
  { Only do the if check once for improved speed rather than every iteration }
  if ShowHex then
  begin
    for I := 0 to 127 do
    begin
      X := I div 16;
      Y := I mod 16;
      Canvas.TextOut(X * HorizMult + 2, Y * VertMult + 28, IntToHex(BaseNum + I, 2));
    end;
  end
  else
  begin
    for I := 0 to 127 do
    begin
      X := I div 16;
      Y := I mod 16;
      Canvas.TextOut(X * HorizMult + 2, Y * VertMult + 28, IntToStr(BaseNum + I));
    end;
  end;
  { Draw in the characters 0-31 if required }
  Canvas.Font.Color := clWindowText;
  if BaseNum = 0 then
  begin
    DrawCharacter(0,  'NUL', HorizMult, VertMult); // Ctrl @, NULL
    DrawCharacter(1,  'SOH', HorizMult, VertMult); // Ctrl A, Start of Header
    DrawCharacter(2,  'STX', HorizMult, VertMult); // Ctrl B,Start of Text
    DrawCharacter(3,  'ETX', HorizMult, VertMult); // Ctrl C,End of Text
    DrawCharacter(4,  'EOT', HorizMult, VertMult); // Ctrl D,End of Transmission
    DrawCharacter(5,  'ENQ', HorizMult, VertMult); // Ctrl E,Enquiry
    DrawCharacter(6,  'ACK', HorizMult, VertMult); // Ctrl F,Acknowlodge
    DrawCharacter(7,  'BEL', HorizMult, VertMult); // Ctrl G,Bell
    DrawCharacter(8,  'BS',  HorizMult, VertMult); // Ctrl H,Backspace
    DrawCharacter(9,  'TAB', HorizMult, VertMult); // Ctrl I,Horizontal Tab
    DrawCharacter(10, 'LF',  HorizMult, VertMult); // Ctrl J,Linefeed
    DrawCharacter(11, 'VT',  HorizMult, VertMult); // Ctrl K,Vertical Tab
    DrawCharacter(12, 'FF',  HorizMult, VertMult); // Ctrl L,Form Feed
    DrawCharacter(13, 'CR',  HorizMult, VertMult); // Ctrl M,Carridge Return
    DrawCharacter(14, 'SO',  HorizMult, VertMult); // Ctrl N,Shift Out
    DrawCharacter(15, 'SI',  HorizMult, VertMult); // Ctrl O,Shift in
    DrawCharacter(16, 'DLE', HorizMult, VertMult); // Ctrl P,Delete
    DrawCharacter(17, 'DC1', HorizMult, VertMult); // Ctrl Q,Device Control 1
    DrawCharacter(18, 'DC2', HorizMult, VertMult); // Ctrl R,Device Control 2
    DrawCharacter(19, 'DC3', HorizMult, VertMult); // Ctrl S,Device Control 3
    DrawCharacter(20, 'DC4', HorizMult, VertMult); // Ctrl T,Device Control 4
    DrawCharacter(21, 'NAK', HorizMult, VertMult); // Ctrl U,Negative Acknowledge
    DrawCharacter(22, 'SYN', HorizMult, VertMult); // Ctrl V,Synchronise
    DrawCharacter(23, 'ETB', HorizMult, VertMult); // Ctrl W,End Block ??
    DrawCharacter(24, 'CAN', HorizMult, VertMult); // Ctrl X,Cancel
    DrawCharacter(25, 'EM',  HorizMult, VertMult); // Ctrl Y,End Message
    DrawCharacter(26, 'SUB', HorizMult, VertMult); // Ctrl Z,Sub
    DrawCharacter(27, 'ESC', HorizMult, VertMult); // Ctrl [,Escape
    DrawCharacter(28, 'FS',  HorizMult, VertMult); // Ctrl \,Form Separator
    DrawCharacter(29, 'GS',  HorizMult, VertMult); // Ctrl ],Group Separator
    DrawCharacter(30, 'RS',  HorizMult, VertMult); // Ctrl ^,Record Separator
    DrawCharacter(31, 'US',  HorizMult, VertMult); // Ctrl _,Unit Separator
  end;

  { draw the character of that number on screen }
  with Canvas.Font do
  begin
    Name := FontName;
    Size := FontSize;
  end;

  for I := Start to 127 do
    DrawCharacter(I, Chr(BaseNum + I), HorizMult, VertMult);

  { Draw the boxes on the screen }
  { Only two colour assignments to canvas speeds things up }
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  { 1) draw left and top sides }
  Canvas.Pen.Color := clBtnHighlight;
  for I := 0 to 7 do
    for J := 0 to 15 do
      Canvas.PolyLine([Point(I * HorizMult, (J + 1) * VertMult + 24),
        Point(I * HorizMult, J * VertMult + 25),
          Point((I + 1) * HorizMult - 1, J * VertMult + 25)]);
  { 2) draw right and bottom sides }
  Canvas.Pen.Color := clBtnShadow;
  for I := 0 to 7 do
    for J := 0 to 15 do
      Canvas.PolyLine([Point((I + 1) * HorizMult - 1, J * VertMult + 25),
        Point((I + 1) * HorizMult - 1, (J + 1) * VertMult + 24),
          Point(I * HorizMult - 1, (J + 1) * VertMult + 24)]);
end;

procedure TfmAsciiChart.DrawCharacter(const CharValue: Integer; const CharText: string; const HorizMult, VertMult: Integer);
{ This draws the text on the screen at the relevant location }
var
  X, Y: Integer; { Screen Locations }
  MyRect: TRect; { general drawing reectangle }
  VOffset, HOffset: Integer; { V and H offsets for bounding box of character in font }
begin
  X := CharValue div 16;
  Y := CharValue mod 16;
  VOffset := (VertMult - Canvas.TextHeight(CharText)) div 2;
  HOffset := (HorizMult - 24 - Canvas.TextWidth(CharText)) div 2;
  MyRect.Left := X * HorizMult + 24;
  MyRect.Right := (X + 1) * HorizMult;
  MyRect.Top := Y * VertMult + 26;
  MyRect.Bottom := (Y + 1) * VertMult + 26;
  Canvas.TextRect(MyRect, MyRect.Left + HOffset, MyRect.Top + VOffset, CharText);
end;

procedure TfmAsciiChart.FormResize(Sender: TObject);
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
begin
  if ShowFontPalette then
    txtChars.SetBounds(284, 0, Self.ClientWidth - 284, txtChars.Height)
  else
    txtChars.SetBounds(0, 0, Self.ClientWidth, txtChars.Height);
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - Panel1.Height - StatusBar.Height) div 16;
  if (HorizMult <> OldHorizMult) or (VertMult <> OldVertMult) then
  begin
    OldHorizMult := HorizMult;
    OldVertMult := VertMult;
    Self.Refresh;
  end;
  KillHint;
end;

procedure TfmAsciiChart.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{ charpos is the ordinal value of the cell clicked on }
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  XPos, YPos: Integer; { X and Y cells clicked on }
begin
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - Panel1.Height - StatusBar.Height) div 16;
  XPos := X div HorizMult;
  YPos := (Y - 25) div VertMult;
  { only generate charpos if clicking inside the boundaries of the cells
    avoids the clicking beyond the right/bottom extents of the cells }
  if (XPos < 8) and (YPos < 16) then
    CharPos := BaseNum + XPos * 16 + YPos
  else
    CharPos := -1;
  if Button = mbRight then
    PopupMenu1.Popup(X + Left + GetSystemMetrics(SM_CXFRAME),
      Y + Self.Top + GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION))
  else
  begin
    if (CharPos > -1) and (CharPos < 256) then
      txtChars.Text := txtChars.Text + Chr(CharPos);
  end;
end;

procedure TfmAsciiChart.btnCharHighClick(Sender: TObject);
{ this draw characters 128-255 }
begin
  if BaseNum = 0 then
  begin
    BaseNum := 128;
    btnCharHigh.Down := True;
  end
  else
  begin
    BaseNum := 0;
    btnCharLow.Down := True;
  end;
  Self.Refresh;
end;

procedure TfmAsciiChart.FontComboNameChange(Sender: TObject);
{ this updates the font used for drawing characters }
begin
  FontName := FontComboName.Text;
  txtChars.Font.Name := FontName;
  Self.Refresh;
end;

procedure TfmAsciiChart.btnSizeClick(Sender: TObject);
{ the tag property of the speedbuttons or menu items is used to
  hold the newfont size - Tag is a property of TComponent so
  cast the sender as that to read the property }
var
  NewFontSize: Integer; { Size of the font I will be changing to }
begin
  NewFontSize := (Sender as TComponent).Tag;
  case NewFontSize of
    8:  FontSizeUpDown.Position := 8;
    10: FontSizeUpDown.Position := 10;
    12: FontSizeUpDown.Position := 12;
  else
    Exit;
  end;
  if NewFontSize = FontSize then Exit; { No change so no need to redraw }
  FontSize := NewFontSize;
  Self.Refresh;
end;

procedure TfmAsciiChart.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmAsciiChart.ShowFontPalette1Click(Sender: TObject);

  procedure SetControlsEnabled(const Enabled: Boolean);
  begin
    FontComboName.Visible := Enabled;
    FontSizeUpDown.Visible := Enabled;
    FontSizeEdit.Visible := Enabled;
    btnCharLow.Visible := Enabled;
    btnCharHigh.Visible := Enabled;
    btnCharInt.Visible := Enabled;
    btnCharHex.Visible := Enabled;
  end;

begin
  ShowFontPalette := not ShowFontPalette;
  if ShowFontPalette then
  begin
    SetControlsEnabled(True);
    txtChars.SetBounds(278, 0, Self.ClientWidth - 278, txtChars.Height);
  end
  else
  begin
    SetControlsEnabled(False);
    txtChars.SetBounds(0, 0, Self.ClientWidth, txtChars.Height);
  end;
end;

procedure TfmAsciiChart.PopupMenu1Popup(Sender: TObject);
begin
  { check low/high menu item }
  ShowLowCharacters1.Checked := (BaseNum = 0);
  { select the correct fontsize element }
  case FontSize of
    8: FontSize8.Checked := True;
    10: FontSize10.Checked := True;
    12: FontSize12.Checked := True;
  else
    // Do Nothing
  end;
  { check the show font palette item }
  ShowFontPalette1.Checked := ShowFontPalette;
  { Check the hex/integer menu item }
  CVHex.Checked := ShowHex;
end;

procedure TfmAsciiChart.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    PtMinTrackSize.X := 400 + 2 * GetSystemMetrics(SM_CXFRAME);
    PtMinTrackSize.Y := 331 + 2 * GetSystemMetrics(SM_CYFRAME) + GetSystemMetrics(SM_CYCAPTION);
  end;
  Msg.Result := 0;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
                       FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count - 1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;

procedure TfmAsciiChart.GetFonts;
var
  DC: HDC;
  LFont: TLogFont;
begin
  DC := GetDC(0);
  try
    { obviously for a Win95/98/NT version only simplify this bit }  //! ?????
    if Lo(GetVersion) >= 4 then
    begin
      FillChar(LFont, SizeOf(LFont), 0);
      LFont.lfCharset := DEFAULT_CHARSET;
      EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, LongInt(FontComboName.Items), 0);
    end
    else
      EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontComboName.Items));
    FontComboName.Sorted := True;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TfmAsciiChart.SetFontName(const NewFontName: string);
{ this sets the font name in the combo to be correct -
  otherwise it would be blank }
var
  I: Integer;
begin
  if FontComboName.Text = NewFontName then Exit;
  with FontComboName do
    for I := 0 to Items.Count - 1 do
    begin
      if CompareText(Items[I], NewFontName) = 0 then
      begin
        ItemIndex := I;
        Break;
      end;
    end;
end;

procedure TfmAsciiChart.btnCharIntClick(Sender: TObject);
begin
  ShowHex := not ShowHex;
  if ShowHex then
    btnCharHex.Down := True
  else
    btnCharInt.Down := True;
  Self.Refresh;
end;

procedure TfmAsciiChart.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{ charpos is the ordinal value of the cell clicked on }
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  XPos, YPos: Integer; { X and Y cells clicked on }
  TheRect: Trect; { the drawing area of the custom hint }
  TheString: string; { the hint string }
  ThePoint: TPoint; { A point variable, used to offset my rect }
  CharPos: Integer; { Override the global Charpos variable }
begin
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - Panel1.Height - StatusBar.Height) div 16;
  XPos := X div HorizMult;
  YPos := (Y - 25) div VertMult;
  { only generate charpos if clicking inside the boundaries of the cells
    avoids the clicking beyond the right/bottom extents of the cells }
  if (XPos < 8) and (YPos < 16) then
    CharPos := BaseNum + XPos * 16 + YPos
  else
    Exit;
  { create my custom hint }
  if (OldCharPos <> CharPos) and Self.Active then
  begin
    KillHint;
    FHint := THintWindow.Create(Self);
    FHint.Color := clInfoBK;
    if (BaseNum = 0) and (CharPos < 32) and (OldCharPos <> CharPos) then
    begin
      TheString := DescLow[CharPos];
      StatusBar.Font.Name := 'MS Sans Serif'; // do not localize
    end
    else
    begin
      TheString := Chr(CharPos);
      StatusBar.Font.Name := FontName;
      with FHint.Canvas.Font do
      begin
        Charset := DEFAULT_CHARSET;
        Name := FontName;
        Size := ZoomFontSize;
      end;
    end;
    TheRect := FHint.CalcHintRect(Screen.Width, TheString, nil);
    ThePoint := ClientToScreen(Point((XPos + 1) * HorizMult - 1, (YPos + 1) * VertMult + 24));
    OffsetRect(TheRect, ThePoint.x, ThePoint.Y);
    if HintActive1.Checked then
      FHint.ActivateHint(theRect, TheString);
    StatusBar.SimpleText := TheString;
    FActive := True;
    HintTimer.Enabled := True;
    OldCharPos := CharPos;
  end;
end;

procedure TfmAsciiChart.KillHint;
begin
  FActive := False;
  if Assigned(FHint) then
  begin
    FHint.ReleaseHandle;
    FHint.Free;
    FHint := nil;
  end;
  HintTimer.Enabled := False;
end;

procedure TfmAsciiChart.HintTimerTimer(Sender: TObject);
begin
  HintTimer.Enabled := False;
  KillHint;
end;

procedure TfmAsciiChart.DoHint(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.FormDeactivate(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.DoDeactivate(Sender: TObject);
begin
  KillHint;
end;

procedure TfmAsciiChart.FontSizeUpDownClick(Sender: TObject;
  Button: TUDBtnType);
begin
  FontSize := FontSizeUpDown.Position;
  Self.Refresh;
end;

procedure TfmAsciiChart.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{ charpos is the ordinal value of the cell clicked on }
var
  HorizMult, VertMult: Integer; { logical screen width/height segments }
  XPos, YPos: Integer; { X and Y cells clicked on }
begin
  HorizMult := Self.ClientWidth div 8;
  VertMult := (Self.ClientHeight - Panel1.Height - StatusBar.Height) div 16;
  XPos := X div HorizMult;
  YPos := (Y - 25) div VertMult;
  { only generate charpos if clicking inside the boundaries of the cells
    avoids the clicking beyond the right/bottom extents of the cells }
  if (XPos < 8) and (YPos < 16) then
    CharPos := BaseNum + XPos * 16 + YPos
  else
    CharPos := -1;
end;

procedure TfmAsciiChart.FontSizeEditChange(Sender: TObject);
var
  NewFontSize: Integer;
begin
  NewFontSize := StrToIntDef(FontSizeEdit.Text, 8);
  if (NewFontSize < 6) or (NewFontSize > 20) or (NewFontSize = FontSize) then
    Exit;
  FontSize := NewFontSize;
  Self.Refresh;
end;

procedure TfmAsciiChart.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := $0;
    Close;
  end;
end;

procedure TfmAsciiChart.FontComboNameEnter(Sender: TObject);
begin
  FontComboName.Perform(CB_SETDROPPEDWIDTH, 175, 0);
end;

procedure TfmAsciiChart.HintActive1Click(Sender: TObject);
begin
  HintActive1.Checked := not HintActive1.Checked;
end;

procedure TfmAsciiChart.HelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 21);
end;

//************************ TASCIIExpert ********************

constructor TASCIIExpert.Create;
begin
  inherited Create;
  ASCIIExpert := Self;
  ShortCut := Menus.ShortCut(Word('A'), [ssCtrl]);
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TASCIIExpert.Destroy;
begin
  ASCIIExpert := nil;
  fmAsciiChart.Free;
  fmAsciiChart := nil;
  inherited Destroy;
end;

function TASCIIExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&ASCII Chart';
begin
  Result := SMenuCaption;
end;

function TASCIIExpert.GetMenuName: string;
begin
  Result := 'GX_ASCII';
end;

function TASCIIExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TASCIIExpert.GetName: string;
begin
  Result := 'ASCII_Window';
end;

function TASCIIExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'ASCII Chart';
begin
  Result := SDisplayName;
end;

procedure TASCIIExpert.Click(Sender: TObject);
begin
  if fmAsciiChart = nil then
    fmAsciiChart := TfmAsciiChart.Create(nil);
  if fmAsciiChart.WindowState = wsMinimized then
    fmAsciiChart.WindowState := wsNormal;
  fmAsciiChart.Show;
end;

function TASCIIExpert.IconFileName: string;
begin
  Result := 'ASCII';
end;

procedure TASCIIExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      fmAsciiChart.Free;
      fmAsciiChart := nil;
    end;
  end;
end;

initialization
  RegisterGX_Expert(TASCIIExpert);
end.

