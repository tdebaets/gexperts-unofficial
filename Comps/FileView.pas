unit FileView;

{$I GX_CondDefine.inc}

{.$DEFINE GX_COMP_PACKAGE}
{.$UNDEF SYNTAXMEMO}

{$IFDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF SYNTAXMEMO}

{$IFDEF MWEDIT}
{$UNDEF SYNTAXMEMO}
{$DEFINE GX_ENHANCED_EDITOR}
{$ENDIF MWEDIT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF SYNTAXMEMO}
  SyntaxEd, SynParse,
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  mwCustomEdit, mwPasSyn, DcjCppSyn, hkhtmlsyn, wmSQLsyn, mwGeneralSyn,
  {$ENDIF MWEDIT}
  {$IFDEF HAVEJPEG}
  JPEG,
  {$ENDIF HAVEJPEG}
  {$IFNDEF GX_COMP_PACKAGE}
  GX_GenFunc,
  {$ENDIF}
  Dialogs, ExtCtrls, ComCtrls, StdCtrls;

type
  TFileViewer = class(TCustomPanel)
  private
    FImage: TImage;
    FRichEdit: TRichEdit;
    {$IFDEF SYNTAXMEMO}
    FSyntaxEd: TSyntaxMemo;
    {$ENDIF SYNTAXMEMO}
    FLoadedFile: string;
    {$IFDEF MWEDIT}
    FmwEdit: TmwCustomEdit;
    FGXSyntaxParser: TGXSyntaxParser;
    {$ENDIF MWEDIT}
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
    procedure Clear;
  published
    property Align;
    property LoadedFile: string read FLoadedFile write FLoadedFile;
  end;

procedure Register;

implementation

constructor TFileViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelInner := bvNone;
  BevelOuter := bvLowered;
  Color := clWindow;
  if not (csDesigning in ComponentState) then
  begin
    {Image Viewer}
    FImage := TImage.Create(nil);
    FImage.Parent := Self;
    FImage.Align := alClient;
    FImage.Visible := False;

    {Rich Text Viewer}
    FRichEdit := TRichEdit.Create(nil);

    FRichEdit.Parent := Self;
    FRichEdit.Align := alClient;
    FRichEdit.WordWrap := False;
    FRichEdit.ScrollBars := ssBoth;
    FRichEdit.ReadOnly := True;
    FRichEdit.Visible := False;
    FRichEdit.BorderStyle := bsNone;

    {$IFDEF SYNTAXMEMO}
    {Syntax Memo}
    FSyntaxEd := TSyntaxMemo.Create(nil);
    with FSyntaxEd do
    begin
      Parent := Self;
      Align := alClient;
      Visible := False;
      BorderStyle := bsNone;
      // ReadOnly := True;
      Gutter := 0;
      //SetupSyntaxParsers(FSyntaxEd, [gxpPAS, gxpCPP, gxpHTML, gxpSQL]);
      Options := Options - [smoShowGutter];
    end;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    {mwEdit Control}
    FmwEdit := TmwCustomEdit.Create(nil);
    with FmwEdit do
    begin
      Parent := Self;
      Align := alClient;
      Visible := False;
      BorderStyle := bsNone;
      ReadOnly := True;
      Gutter.Width := 0;
    end;
    {$ENDIF MWEDIT}
  end;
end;

destructor TFileViewer.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    FImage.Free;
    FImage := nil;

    FRichEdit.Free;
    FRichEdit := nil;

    {$IFDEF SYNTAXMEMO}
    FSyntaxEd.Free;
    FSyntaxEd := nil;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    FmwEdit.Highlighter.Free;
    FmwEdit.Highlighter := nil;
    FmwEdit.Free;
    FmwEdit := nil;
    {$ENDIF MWEDIT}
  end;

  inherited Destroy;
end;

procedure TFileViewer.Clear;
begin
  LoadedFile := '';
  FImage.Visible := False;
  FRichEdit.Visible := False;
  {$IFDEF SYNTAXMEMO}
  FSyntaxEd.Visible := False;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  FmwEdit.Visible := False;
  {$ENDIF MWEDIT}
end;

procedure TFileViewer.LoadFromFile(FileName: string);

  {$IFDEF SYNTAXMEMO}
  procedure AssignParser(Parser: TGXSyntaxParser);
  begin
    {$IFNDEF GX_COMP_PACKAGE}
    FSyntaxEd.Parser2 := GetSyntaxParser(Parser);
    {$ENDIF}
    FSyntaxEd.ActiveParser := 2;
    FSyntaxEd.Gutter := 0;
    FRichEdit.Visible := False;
    FImage.Visible := False;
    FSyntaxEd.LoadFromFile(FileName);
    (FSyntaxEd.Parser2 as TSyntaxMemoParser).UpdateEditors;
    FSyntaxEd.Visible := True;
  end;
  {$ENDIF SYNTAXMEMO}

  {$IFDEF MWEDIT}
  procedure AssignParser(Parser: TGXSyntaxParser);
  begin
    if (FGXSyntaxParser <> Parser) then
    begin
      SetmwEditHighLighter(FmwEdit, Parser);
      FGXSyntaxParser := Parser;
    end;
    FRichEdit.Visible := False;
    FImage.Visible := False;
    FmwEdit.Lines.LoadFromFile(FileName);
    FmwEdit.Visible := True;
  end;
  {$ENDIF MWEDIT}

var
  Ext: string;
begin
  LoadedFile := FileName;
  {$IFDEF SYNTAXMEMO}
  FSyntaxEd.Lines.BeginUpdate;
  if FSyntaxEd.Gutter > 0 then
    FSyntaxEd.Gutter := 0;
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  FmwEdit.BeginUpdate;
  {$ENDIF MWEDIT}
  FRichEdit.Lines.BeginUpdate;
  try
    {$IFNDEF GX_COMP_PACKAGE}
    Ext := ExtractUpperFileExt(FileName);
    {$ENDIF}

    if {$IFDEF HAVEJPEG} (Ext = '.JPG') or {$ENDIF HAVEJPEG}
       (Ext = '.BMP') or (Ext = '.ICO') then
    begin
      FRichEdit.Visible := False;
      {$IFDEF SYNTAXMEMO}
      FSyntaxEd.Visible := False;
      {$ENDIF SYNTAXMEMO}
      {$IFDEF MWEDIT}
      FmwEdit.Visible := False;
      {$ENDIF MWEDIT}
      FImage.Picture.LoadFromFile(Filename);
      FImage.Visible := True;
    end
    else
    if (Ext = '.RTF')
    {$IFNDEF GX_ENHANCED_EDITOR}
        or (Ext = '.TXT') or (Ext = '.ASC') or (Ext = '.ME') or (Ext = '.INI')
        or (Ext = '.PAS') or (Ext = '.DPR') or (Ext = '.INC') or (Ext = '.DPK')
        or (Ext = '.HTML') or (Ext = '.HTM') or (Ext = '.DIZ')
        or (Ext = '.C') or (Ext = '.CPP') or (Ext = '.H') or (Ext = '.HPP')
        or (Ext = '.SQL')
    {$ENDIF GX_ENHANCED_EDITOR}
    then
    begin
      FRichEdit.PlainText := not(Ext = '.RTF');
      FImage.Visible := False;
      {$IFDEF SYNTAXMEMO}
      FSyntaxEd.Visible := False;
      {$ENDIF SYNTAXMEMO}
      {$IFDEF MWEDIT}
      FmwEdit.Visible := False;
      {$ENDIF MWEDIT}
      FRichEdit.Lines.LoadFromFile(Filename);
      FRichEdit.Visible := True;
    end
    {$IFDEF GX_ENHANCED_EDITOR}
    else
    { TODO -oArentJan -cFeature : Include the other mwEdit highlighters? }
    if (Ext = '.TXT') or (Ext = '.ASC') or (Ext = '.ME') or (Ext = '.DIZ')or (Ext = '.INI') then
    begin
      {$IFDEF SYNTAXMEMO}
      FSyntaxEd.ActiveParser := 1;
      FRichEdit.Visible := False;
      FImage.Visible := False;
      FSyntaxEd.LoadFromFile(FileName);
      FSyntaxEd.Visible := True;
      {$ENDIF SYNTAXMEMO}
      {$IFDEF MWEDIT}
      AssignParser(gxpNone)
      {$ENDIF MWEDIT}
    end
    else
      if (Ext = '.PAS') or (Ext = '.DPR') or (Ext = '.INC') or (Ext = '.DPK') then
        AssignParser(gxpPAS)
      else
      if (Ext = '.HTML') or (Ext = '.HTM') then
        AssignParser(gxpHTML)
      else
      if (Ext = '.C') or (Ext = '.CPP') or (Ext = '.H') or (Ext = '.HPP') then
        AssignParser(gxpCPP)
      else
      if (Ext = '.SQL') then
        AssignParser(gxpSQL)
      {$ENDIF GX_ENHANCED_EDITOR}
      else
        Clear;

    {$IFDEF SYNTAXMEMO}
      FSyntaxEd.Options := FSyntaxEd.Options - [smoShowGutter];
      FSyntaxEd.Gutter := 0;
    {$ENDIF SYNTAXMEMO}

  finally
    {$IFDEF SYNTAXMEMO}
    FSyntaxEd.Lines.EndUpdate;
    {$ENDIF SYNTAXMEMO}
    {$IFDEF MWEDIT}
    FmwEdit.EndUpdate;
    {$ENDIF MWEDIT}
    FRichEdit.Lines.EndUpdate;
  end;
end;

procedure Register;
begin
  RegisterComponents('User', [TFileViewer]);
end;

end.

