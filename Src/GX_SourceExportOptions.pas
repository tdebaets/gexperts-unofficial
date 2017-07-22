unit GX_SourceExportOptions;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, ColorGrd,
  mwCustomEdit,
  // mwEdit syntax highlighters
  mwHighlighter, mwPasSyn, dcjCppSyn, hkHtmlSyn, wmSqlSyn;

type
  TfmSourceExportOptions = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    rbxCopySettings: TRadioGroup;
    cbxAttributes: TComboBox;
    gbxAttributes: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    chkStrikeOut: TCheckBox;
    btnLoadIde: TButton;
    lblElement: TLabel;
    procedure AttributeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxAttributesChange(Sender: TObject);
    procedure btnLoadIdeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FmwSampleEditor: TmwCustomEdit;
    {$IFDEF GX_BCB}
    HiddenGrid: TColorGrid;
    ColorGrid: TColorGrid;
    {$ELSE not GX_BCB}
    HiddenGrid: TColorGrid;
    ColorGrid: TColorGrid;
    {$ENDIF GX_BCB}
    NoChangeEvents: Boolean;
    function GetColorIndexFromColor(Color: TColor; IsBackground: Boolean): Integer;
    procedure mwEditSelectionChange(Sender: TObject);
  public
    property mwSampleEditor: TmwCustomEdit read FmwSampleEditor write FmwSampleEditor;
  end;

implementation

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_GenFunc, GX_VerDepConst, Dialogs;

{$R *.DFM}

procedure TfmSourceExportOptions.FormCreate(Sender: TObject);
const
  PasSampleString =
    '{ Syntax Highlighting }'+#13#10+
    'procedure TForm1.Button1Click(Sender: TObject);'+#13#10+
    'var'+#13#10+
    '  Number, I, X: Integer;'+#13#10+
    'begin'+#13#10+
    '  Number := 123456;'+#13#10+
    '  Caption := '#39'The number is '#39' + IntToStr(Number);'+#13#10+
    '  asm'+#13#10+
    '    MOV AX, 2345H'+#13#10+
    '  end'+#13#10+
    '  I  := X + Number;'+#13#10+
    'end;';
  CppSampleString =
    '// Syntax Highlighting'+#13#10+
    'void __fastcall TForm1::Button1Click(TObject *Sender)'+#13#10+
    '{'+#13#10+
    '  Caption = "The number is " + IntToStr(i);'+#13#10+
    '  int i = 12345;'+#13#10+
    '  double d = 123.45;'+#13#10+
    '  char c = ''a'';'+#13#10+
    '#ifdef FULLMOON'+#13#10+
    '  asm {'+#13#10+
    '    MOV AX, 0x0E07'+#13#10+
    '    XOR BX, BX'+#13#10+
    '    INT 0x10 // Makes the system beep'+#13#10+
    '  }'+#13#10+
    '  j += #; // Illegal character'+#13#10+
    '#endif';
  SqlSampleString =
    '/*'+#13#10+
    'SQL Comment'+#13#10+
    '*/'+#13#10+
    'select a.FileID, a.FileName, c.AuthorID, c.AuthorName'+#13#10+
    '  from "Files.db" a, "FilesAut.db" b, "Authors.db" c'+#13#10+
    '    where a.FileID = b.FileID and'+#13#10+
    '      a.FileID in ( select z.FileID'+#13#10+
    '        from "Files.db" z'+#13#10+
    '        group by z.FileID'+#13#10+
    '        having count(*) > 1 )'+#13#10+
    '  order by FileID';
  HtmlSampleString =
    '<!-- HTML Comment -->'+#13#10+
    '  <A HREF="http://www.gexperts.org" Target="_top">Link</A>'+#13#10+
    '  Follow this link &amp; and read for more information.'+#13#10+
    '</BODY>'+#13#10+
    '</HTML>';
var
  i: Integer;
  GXHighlighter: TGXSyntaxParser;
begin
  // Destroyed with form
  {$IFDEF GX_BCB}
  ColorGrid := TColorGrid.Create(Self);
  HiddenGrid := TColorGrid.Create(Self);
  {$ELSE GX_BCB}
  ColorGrid := TColorGrid.Create(Self);
  HiddenGrid := TColorGrid.Create(Self);
  {$ENDIF GX_BCB}
  HiddenGrid.Visible := False;
  HiddenGrid.Parent := Self;
  with ColorGrid do
  begin
    Name := 'ColorGrid';
    Parent := gbxAttributes;
    Left := 8;
    Top := 16;
    Width := 100;
    Height := 100;
    ClickEnablesColor := True;
    BackgroundIndex := 15;
    TabOrder := 0;
    TabStop := True;
    OnChange := AttributeChange;
  end;

  mwSampleEditor := TmwCustomEdit.Create(Self);
  with mwSampleEditor do
  begin
    Parent := Self;
    ReadOnly := True;
    Left := 8;
    Top := 160;
    Width := 393;
    Height := 171;
    TabOrder := 4;
    Gutter.Width := 0;
    Options := Options + [mweoNoCaret, mweoNoSelection];

    OnSelectionChange := MwEditSelectionChange;
    GXHighlighter := GetGXHighLighterForCurrentSourceEditor;
    SetmwEditHighLighter(mwSampleEditor, GXHighlighter);
    case GXHighlighter of
      gxpPAS:  Lines.Text := PasSampleString;
      gxpCPP:  Lines.Text := CppSampleString;
      gxpHTML: Lines.Text := HtmlSampleString;
      gxpSQL:  Lines.Text := SqlSampleString;
      else     Lines.Text := 'Unknown File Type';
    end;
    if GXHighlighter in [gxpHTML, gxpSQL] then
      btnLoadIde.Enabled := False;
    // !! mwEdit .9x bug: It adds an extra blank line here as a bug workaround
    if Trim(Lines[0]) = '' then
      Lines.Delete(0);
  end;

  {$IFDEF GX_VER120_up}
  BorderStyle := bsSizeable;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  {$ENDIF GX_VER120_up}

  btnLoadIdeClick(Sender);
  for i := 0 to mwSampleEditor.HighLighter.AttrCount - 1 do
    cbxAttributes.Items.Add(mwSampleEditor.HighLighter.Attribute[i].Name);
  cbxAttributes.ItemIndex := 0;
  cbxAttributesChange(Self);
end;

procedure TfmSourceExportOptions.AttributeChange(Sender: TObject);
var
  Attr: TmwHighLightAttributes;
  AttrStyle: TFontStyles;
begin
  if NoChangeEvents then
    Exit;
  Attr := TmwHighLightAttributes.Create(cbxAttributes.Items[cbxAttributes.ItemIndex]);
  try
    AttrStyle := [];
    Attr.Foreground := ColorGrid.ForegroundColor;
    Attr.Background := ColorGrid.BackgroundColor;
    if chkBold.Checked then
      Include(AttrStyle, fsBold);
    if chkItalic.Checked then
      Include(AttrStyle, fsItalic);
    if chkUnderLine.Checked then
      Include(AttrStyle, fsUnderline);
    if chkStrikeOut.Checked then
      Include(AttrStyle, fsStrikeOut);
    Attr.Style := AttrStyle;
    mwSampleEditor.HighLighter.Attribute[cbxAttributes.ItemIndex].Assign(Attr);
    mwSampleEditor.Refresh;
  finally
    Attr.Free;
  end;
end;

procedure TfmSourceExportOptions.cbxAttributesChange(Sender: TObject);
var
  Attr: TmwHighLightAttributes;
begin
  Attr := TmwHighLightAttributes.Create('');
  try
    Attr.Assign(mwSampleEditor.Highlighter.Attribute[cbxAttributes.ItemIndex]);
    NoChangeEvents := True;
    try
      // Buggy!
      //ColorGrid.ForegroundIndex := ColorGrid.ColorToIndex(Attr.Foreground);
      //ColorGrid.BackgroundIndex := ColorGrid.ColorToIndex(Attr.Background);
      ColorGrid.ForegroundIndex := GetColorIndexFromColor(Attr.Foreground, False);
      ColorGrid.BackgroundIndex := GetColorIndexFromColor(Attr.Background, True);

      chkBold.Checked := (fsBold in Attr.Style);
      chkItalic.Checked := (fsItalic in Attr.Style);
      chkUnderLine.Checked := (fsUnderline in Attr.Style);
      chkStrikeOut.Checked := (fsStrikeOut in Attr.Style);
    finally
      NoChangeEvents := False;
      AttributeChange(nil);
    end;
  finally
    Attr.Free;
  end;
end;

procedure TfmSourceExportOptions.btnLoadIdeClick(Sender: TObject);
resourcestring
  ErrorLoadingIDESettings = 'Error loading the IDE editor''s highlighter configuration.' + #13#10 +
    'Please open the IDE''s configuration dialog to change the editor' + #13#10 +
    'colors, make a trivial change, and confirm to store the settings.';
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    mwSampleEditor.HighLighter.EnumUserSettings(sl);
    for i := sl.Count - 1 downto 0 do
    begin
      if Pos(MajorVersionNumberChar + '.', sl[i]) = 1 then
      begin
        if not mwSampleEditor.HighLighter.UseUserSettings(i) then
          MessageDlg(ErrorLoadingIDESettings, mtWarning, [mbOK], 0);
        Break;
      end;
    end
  finally
    sl.Free;
  end;
end;

// Temporary hack, since TColorGrid.ColorToIndex seems buggy?
function TfmSourceExportOptions.GetColorIndexFromColor(Color: TColor; IsBackground: Boolean): Integer;
var
 i: Integer;
begin
  for i := 0 to 15 do
  begin
    HiddenGrid.ForegroundIndex := i;
    if ColorToRGB(HiddenGrid.ForegroundColor) = ColorToRGB(Color) then
    begin
      Result := HiddenGrid.ForegroundIndex;
      Exit;
    end;
  end;
  // Fallback for unknown colors
  {$IFOPT D+}SendDebugEx('Source Export: Unknown color requested!', mtError);{$ENDIF}

  if IsBackground then
    Result := 15 // White
  else
    Result := 0; // Black
end;

procedure TfmSourceExportOptions.mwEditSelectionChange(Sender: TObject);
var
  DummyStr: string;
  Token: TmwHighLightAttributes;
  i: Integer;
begin
  mwSampleEditor.GetHighlighterAttriAtRowCol(mwSampleEditor.CaretXY, DummyStr, Token);
  if Token = nil then
    Exit;
  for i := 0 to mwSampleEditor.HighLighter.AttrCount - 1 do
  begin
    if Token.Name = mwSampleEditor.HighLighter.Attribute[i].Name then
    begin
      cbxAttributes.ItemIndex := i;
      cbxAttributes.OnChange(Self);
      Break;
    end;
  end;
end;

procedure TfmSourceExportOptions.FormShow(Sender: TObject);
begin
  // Doing this in FormCreate causes the buttons to be placed badly in D5
  {$IFDEF GX_VER120_up}
  mwSampleEditor.Anchors := [akLeft, akTop, akBottom, akRight];
  btnOK.Anchors := [akBottom, akRight];
  btnCancel.Anchors := btnOK.Anchors;
  {$ENDIF GX_VER120_up}
end;

end.
