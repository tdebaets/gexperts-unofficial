unit GX_SourceExport;

// Original Author: ArentJan Banck <ajbanck@davilex.nl>

// TODO -oArentJan -cFeature : do not scroll the mwEdit preview when copying
// TODO -oArentJan -cFeature : Background colors in HTML export?

{$I GX_CondDefine.inc}

{$IFNDEF MWEDIT}
interface implementation
{$ELSE MWEDIT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  ComCtrls, Menus, Buttons, StdCtrls, ExtCtrls, ToolWin,
  // mwEdit
  mwCustomEdit, mwExport,
  // GExperts
  GX_Experts;

type
  TCopyFormat = (cfText, cfHTMLFragment, cfRTFFragment);

  TSourceExportExpert = class;

  TfmSourceExport = class(TForm)
    pnlFooter: TPanel;
    btnCopy: TButton;
    btnPrint: TButton;
    btnConfig: TButton;
    dlgSave: TSaveDialog;
    btnSave: TButton;
    edtTitle: TEdit;
    lblTitle: TLabel;
    btnClose: TButton;
    ToolBar: TToolBar;
    tbnSave: TToolButton;
    tbnCopy: TToolButton;
    tbnPrint: TToolButton;
    tbnConfigure: TToolButton;
    tbnHelp: TToolButton;
    imlToolbar: TImageList;
    pmuCopy: TPopupMenu;
    mitCopyHtml: TMenuItem;
    mitCopyRtf: TMenuItem;
    mitCopy: TMenuItem;
    procedure sbHelpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure sbPrintClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure sbCopyClick(Sender: TObject);
    procedure sbConfigureClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure mitCopyHtmlClick(Sender: TObject);
    procedure tbnCopyClick(Sender: TObject);
    procedure mitCopyRtfClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pmuCopyPopup(Sender: TObject);
  private
    FHasBlock: Boolean;
    FmwCustomEdit: TmwCustomEdit;
    procedure GetSettings;
    procedure CopyToClipboard(CopyFormat: TCopyFormat);
    procedure FillEditControlWithIdeData(UseBlockIfAvailable: Boolean);
    procedure ExportAsFile(const FileName: string; Exporter: TmwCustomExport);
  end;

  TSourceExportExpert = class(TGX_EnhExpert)
  private
    // Persistent configuration options
    FPrintLineNumbers: Boolean;
    FDefaultCopyFormat: TCopyFormat;
    FSaveDir: string;
    FSaveFilter: Integer;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Configure; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}

uses
  Registry,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  mwHighLighter, mwHtmlExport, mwRtfExport,
  GX_GenFunc, GX_GExperts, GX_ConfigurationInfo,
  GX_SourceExportOptions;

const
  HighLighterDefaultRegKey = '\GExperts\SourceExport\Highlighters\';

var
  SourceExportExpert: TSourceExportExpert = nil;

procedure TfmSourceExport.FillEditControlWithIdeData(UseBlockIfAvailable: Boolean);
var
  MemStream: TMemoryStream;
begin
  Screen.Cursor := crHourglass;
  try
    Assert(FmwCustomEdit <> nil);

    FmwCustomEdit.Lines.BeginUpdate;
    FmwCustomEdit.ClearAll;

    SetmwEditHighLighter(FmwCustomEdit, GetGXHighLighterForCurrentSourceEditor);

    MemStream := TMemoryStream.Create;
    try
      FHasBlock := ReadSourceEditorBlockSelection(MemStream);
      MemStream.Position := 0;
      FmwCustomEdit.Lines.LoadFromStream(MemStream);
    finally
      MemStream.Free;
    end;

  finally
    FmwCustomEdit.Lines.EndUpdate;
    Screen.Cursor := crDefault;
  end
end;

procedure TfmSourceExport.GetSettings;
resourcestring
  RS_CopyText = 'Copy';
  RS_CopyHTML = 'Copy as HTML';
  RS_CopyRTF  = 'Copy as RTF';
var
  CaptionText: string;
begin
  Assert(SourceExportExpert <> nil);
  Assert(FmwCustomEdit <> nil);

  FmwCustomEdit.HighLighter.
    LoadFromRegistry(HKEY_CURRENT_USER, ConfigInfo.RegKey +
        HighLighterDefaultRegKey + FmwCustomEdit.HighLighter.LanguageName);

  case SourceExportExpert.FDefaultCopyFormat of
    cfText:         CaptionText := RS_CopyText;
    cfHTMLFragment: CaptionText := RS_CopyHTML;
    cfRTFFragment:  CaptionText := RS_CopyRTF;
    else            Assert(False, 'Illegal TCopyFormat');
  end;

  btnCopy.Caption := '&' + CaptionText;
  tbnCopy.Hint := CaptionText;
  mitCopy.Default := True;
end;

procedure TfmSourceExport.FormActivate(Sender: TObject);
resourcestring
  RS_DialogFragmentExportTitle = 'Fragment of %s';
var
  CurrentModuleFileName: string;
begin
  CurrentModuleFileName := GetFileNameOfCurrentModule;
  CurrentModuleFileName := ExtractFilename(CurrentModuleFileName);

  FillEditControlWithIdeData(True);
  GetSettings;
  if FHasBlock then
    edtTitle.Text := Format(RS_DialogFragmentExportTitle, [CurrentModuleFileName])
  else
    edtTitle.Text := CurrentModuleFileName;
end;

procedure TfmSourceExport.ExportAsFile(const FileName: string;
  Exporter: TmwCustomExport);
resourcestring
  SExportNotSupported = 'Export not supported for this syntax highlighter';
begin
  Assert(FmwCustomEdit <> nil);

  if hcExportable in FmwCustomEdit.HighLighter.Capability then
    FmwCustomEdit.ExportToFile(False, dlgSave.FileName, Exporter)
  else
    ShowMessage(SExportNotSupported);
end;

procedure TfmSourceExport.btnSaveClick(Sender: TObject);
resourcestring
  RS_DialogTitle = 'Save %s As';
var
  CurrentIdeDir: string;
  Exporter: TmwCUstomExport;
begin
  Assert(SourceExportExpert <> nil);

  try
    dlgSave.Title := Format(RS_DialogTitle, [edtTitle.Text]);
    dlgSave.FileName := ChangeFileExt(edtTitle.Text, '');

    dlgSave.InitialDir := SourceExportExpert.FSaveDir;
    dlgSave.FilterIndex := SourceExportExpert.FSaveFilter;

    CurrentIdeDir := GetCurrentDir;
    try
      if dlgSave.Execute then
      begin
        SourceExportExpert.FSaveDir := ExtractFilePath(ExpandFileName(dlgSave.FileName));
        SourceExportExpert.FSaveFilter := dlgSave.FilterIndex;
        if dlgSave.FilterIndex = 1 then
          Exporter := TmwHtmlExport.Create(nil)
        else
          Exporter := TmwRtfExport.Create(nil);
        try
          Exporter.Title := edtTitle.Text;
          ExportAsFile(dlgSave.FileName, Exporter);
        finally
          Exporter.Free;
        end;
      end;
    finally
      SetCurrentDir(CurrentIdeDir);
    end;
  except
    on E: Exception do
    begin
      ProcessException(E);
      raise;
    end;
  end;
end;

procedure TfmSourceExport.CopyToClipboard(CopyFormat: TCopyFormat);

  procedure CopyExportData;
  var
    Exporter: TmwCustomExport;
  begin
      case CopyFormat of
        cfText:
          begin
            // This shared exporter idea isn't really that safe, but oh well...
            TmwHtmlExport.GetSharedExporter.Title := edtTitle.Text;
            FmwCustomEdit.ExportToClipboard(False, nil); // All formats
          end;
        cfHTMLFragment:
          begin
            Exporter := TmwHtmlExport.GetSharedExporter;
            Exporter.Title := edtTitle.Text;
            (Exporter as TmwHtmlExport).CreateHTMLFragment := True;
            FmwCustomEdit.ExportToClipboard(False, Exporter);
          end;
        cfRTFFragment:
          begin
            Exporter := TmwRtfExport.GetSharedExporter;
            // RTF fragments are not supported yet.  Is it even useful?
            FmwCustomEdit.ExportToClipboard(False, Exporter);
          end;
      else
        Assert(False, 'Invalid CopyFormat in CopyToClipboard');
      end;
  end;

var
  CursorPos: TPoint;
begin
  Assert(FmwCustomEdit <> nil);
  try
    if FmwCustomEdit.SelAvail then
      CopyExportData
    else
    begin
      CursorPos := FmwCustomEdit.CaretXY;
      try
        FmwCustomEdit.SelectAll;
        CopyExportData;
      finally
        FmwCustomEdit.CaretXY := CursorPos;
        FmwCustomEdit.BlockBegin := CursorPos;
        FmwCustomEdit.BlockEnd := CursorPos;
      end;
    end;

  except
    on E: Exception do
    begin
      ProcessException(E);
      raise;
    end;
  end;
end;

procedure TfmSourceExport.sbCopyClick(Sender: TObject);
begin
  CopyToClipboard(cfText);
end;

procedure TfmSourceExport.mitCopyHtmlClick(Sender: TObject);
begin
  CopyToClipboard(cfHTMLFragment);
end;

procedure TfmSourceExport.mitCopyRtfClick(Sender: TObject);
begin
  CopyToClipboard(cfRTFFragment);
end;

procedure TfmSourceExport.tbnCopyClick(Sender: TObject);
begin
  Assert(SourceExportExpert <> nil);
  CopyToClipboard(SourceExportExpert.FDefaultCopyFormat);
end;

procedure TfmSourceExport.sbPrintClick(Sender: TObject);
var
  Options: TmwPrintOptions;
begin
  try
    Assert(FmwCustomEdit <> nil);
    Assert(SourceExportExpert <> nil);

    FmwCustomEdit.Gutter.ShowLineNumbers := SourceExportExpert.FPrintLineNumbers;

    Options.SelectedOnly := FmwCustomEdit.SelAvail;
    Options.Highlighted := (FmwCustomEdit.HighLighter <> nil);
    Options.WrapLongLines := False; // Not implemented yet!

    Options.Copies := 1;
    Options.PrintRange := Rect(0, 0, 0, 0); // Not using it here.
    Options.MarginUnits := muThousandthsOfInches;
    Options.Margins := Rect(500, 500, 500, 500); // 1/2 inch margins

    Options.Title := edtTitle.Text;

    Options.Header := TStringList.Create;
    Options.Footer := TStringList.Create;
    Screen.Cursor := crHourGlass;
    try
      //Options.Header.AddObject('Title: $title$', TObject(hfaCenter));
      //Options.Header.AddObject('Printing Time and Date: $time$ $date$',TObject(hfaLeft));
      //Options.Header.Add('');

      //Options.Footer.Add('');
      //Options.Footer.AddObject('Page Number: $pagenum$ Printing Date/Time: $datetime$', TObject(hfaRight));

      // Use whatever font the editor has.
      FmwCustomEdit.Print(nil, Options);
    finally
      Options.Header.Free;
      Options.Footer.Free;
      Screen.Cursor := crDefault;

      FmwCustomEdit.Gutter.ShowLineNumbers := False;
    end;

  except
    on E: Exception do
    begin
      ProcessException(E);
      raise;
    end;
  end;
end;

procedure TfmSourceExport.sbConfigureClick(Sender: TObject);
begin
  Assert(SourceExportExpert <> nil);

  SourceExportExpert.Configure;
  GetSettings;
end;

procedure TfmSourceExport.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 26);
end;

procedure TfmSourceExport.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmSourceExport.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Assert(SourceExportExpert <> nil);
  SourceExportExpert.SaveSettings;
end;

procedure TfmSourceExport.FormCreate(Sender: TObject);
begin
  // Destroyed with form
  FmwCustomEdit := TmwCustomEdit.Create(Self);
  with FmwCustomEdit do
  begin
    Lines.Clear;
    Parent := Self;
    Align := alClient;
    TabOrder := 0;
    Gutter.Width := 0;
  end;
  // VCL versions less than 4.0 don't support ofEnableSizing
  {$IFDEF GX_VER120_up}
  dlgSave.Options := dlgSave.Options + [ofEnableSizing];
  {$ENDIF GX_VER120_up}
end;

procedure TfmSourceExport.pmuCopyPopup(Sender: TObject);
begin
  mitCopy.Default := False;
  mitCopyHtml.Default := False;
  mitCopyRtf.Default := False;
  case SourceExportExpert.FDefaultCopyFormat of
    cfText:         mitCopy.Default := True;
    cfHTMLFragment: mitCopyHtml.Default := True;
    cfRTFFragment:  mitCopyRtf.Default := True;
    else            Assert(False, 'Illegal TCopyFormat');
  end;
end;

//************************ TSourceExportExpert ********************

constructor TSourceExportExpert.Create;
begin
  inherited Create;

  ShortCut := 0;
  HasMenuItem := True;
  SourceExportExpert := Self;
end;

procedure TSourceExportExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    else
      // Modal expert, so nothing here to initialize
    begin
      // Modal expert, so nothing here to free
    end;
  end;
end;

procedure TSourceExportExpert.Click(Sender: TObject);
begin
  with TfmSourceExport.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TSourceExportExpert.LoadSettings;
var
  NewCopyFormat: TCopyFormat;
begin
  inherited LoadSettings;
  // Do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    FPrintLineNumbers := ReadBool('SourceExport', 'Print Line Numbers', False);
    NewCopyFormat := TCopyFormat(ReadInteger('SourceExport', 'Copy Format', 0));
    Assert(NewCopyFormat in [Low(TCopyFormat)..High(TCopyFormat)]);
    FDefaultCopyFormat := NewCopyFormat;
    FSaveDir := ReadString('SourceExport', 'Save Directory', '');
    FSaveFilter := ReadInteger('SourceExport', 'Save Format', 1);
  finally
    Free;
  end;
end;

procedure TSourceExportExpert.SaveSettings;
begin
  inherited SaveSettings;
  // Do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteBool('SourceExport', 'Print Line Numbers', FPrintLineNumbers);
    WriteInteger('SourceExport', 'Copy Format', Ord(FDefaultCopyFormat));
    WriteString('SourceExport', 'Save Directory', FSaveDir);
    WriteInteger('SourceExport', 'Save Format', FSaveFilter);
  finally
    Free;
  end;
end;

procedure TSourceExportExpert.Configure;
var
  Dlg: TfmSourceExportOptions;
  HighLighterRegKey: string;
  NewCopyFormat: TCopyFormat;
begin
  Dlg := TfmSourceExportOptions.Create(nil);
  try
    HighLighterRegKey := ConfigInfo.RegKey + HighLighterDefaultRegKey
        + Dlg.mwSampleEditor.HighLighter.LanguageName;
    Dlg.rbxCopySettings.ItemIndex := Ord(FDefaultCopyFormat);
    Dlg.mwSampleEditor.HighLighter.LoadFromRegistry(HKEY_CURRENT_USER, HighLighterRegKey);
    if Dlg.ShowModal = mrOK then
    begin
      Dlg.mwSampleEditor.HighLighter.SaveToRegistry(HKEY_CURRENT_USER, HighLighterRegKey);

      NewCopyFormat := TCopyFormat(Dlg.rbxCopySettings.ItemIndex);
      Assert(NewCopyFormat in [Low(TCopyFormat)..High(TCopyFormat)]);
      FDefaultCopyFormat := NewCopyFormat;

      SaveSettings;
    end;
  finally
    Dlg.Free;
  end;
end;

function TSourceExportExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Source Export...';
begin
  Result := SMenuCaption;
end;

function TSourceExportExpert.GetMenuName: string;
begin
  Result := 'GX_SourceExport'; // Do not localize
end;

function TSourceExportExpert.GetMenuMask: string;
begin
  Result := '*.PAS;*.INC;*.DPR;*.TXT;*.CPP;*.HPP;*.C;*.H;*.SQL;*.HTM;*.HTML'; // Do not localize
end;

function TSourceExportExpert.GetName: string;
begin
  Result := 'SourceExport'; // Do not localize
end;

function TSourceExportExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Source Export';
begin
  Result := SDisplayName;
end;

function TSourceExportExpert.IconFileName: string;
begin
  Result := 'SourceExport'; // Do not localize
end;

initialization
  RegisterGX_Expert(TSourceExportExpert);

{$ENDIF MWEDIT}
end.
