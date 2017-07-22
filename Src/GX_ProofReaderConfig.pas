unit GX_ProofReaderConfig;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{$IFNDEF GX_NOBDE}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ComCtrls, StdCtrls, Buttons, Mask,
  DB, DBGrids, DBCtrls,
  EditIntf,
  GX_MessageBox, GX_ProofReaderDM;

type
  TfmProofreaderConfig = class(TForm)
    pcAPR: TPageControl;
    tsReplacement: TTabSheet;
    tsDictionary: TTabSheet;
    tsHistory: TTabSheet;
    lblRules: TLabel;
    cbLanguage: TComboBox;
    cbBeep: TCheckBox;
    cbReplacerActive: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    btnDisable: TButton;
    dbgDictionary: TDBGrid;
    gbReplaceIf: TGroupBox;
    cbOtherLetter: TCheckBox;
    cbNoLetter: TCheckBox;
    cbMoreLetter: TCheckBox;
    cbCaseDiffer: TCheckBox;
    cbMixed: TCheckBox;
    cbNoFirstOther: TCheckBox;
    cbEnableDictionary: TCheckBox;
    dbgReplacement: TDBGrid;
    btnImport: TButton;
    dlgGetWordlist: TOpenDialog;
    btnExport: TButton;
    dlgPutWordlist: TSaveDialog;
    grdHistory: TStringGrid;
    cbEnableCompiler: TCheckBox;
    cbNearbyLetter: TCheckBox;
    procedure pcAPRChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDisableClick(Sender: TObject);
    procedure cbEnableDicitionaryClick(Sender: TObject);
    procedure cbEnableCompilerClick(Sender: TObject);
    procedure cbReplacerActiveClick(Sender: TObject);
    procedure pcAPRChanging(Sender: TObject; var AllowChange: Boolean);
    procedure btnCancelClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbOtherLetterClick(Sender: TObject);
  private
    fmProofReaderDM: TdmProofReader;
    procedure UpdateHistoryGrid;
    function GetCurrentModuleSyntaxHighlighter: TSyntaxHighlighter;
  public
    property ProofReaderDM: TdmProofReader read fmProofReaderDM write fmProofReaderDM;
  end;

  TShowD4KibitzMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

implementation

uses
  ToolIntf, ExptIntf,
  DBTables,
  {$IFDEF GX_VER140_up}
  Variants,
  {$ENDIF GX_VER140_up}
  GX_GenFunc, GX_GExperts, GX_ConfigurationInfo;

{$R *.DFM}

procedure TfmProofreaderConfig.UpdateHistoryGrid;
resourcestring
  SReplacementInfoHeader = 'Correction';
  SReplacementTime = 'Time';
var
  i: Integer;
  CorrectionHistory: TCorrectionHistory;
begin
  Assert(fmProofReaderDM <> nil);
  CorrectionHistory := fmProofReaderDM.History;
  grdHistory.RowCount := Max(CorrectionHistory.Count + 1, 2);
  grdHistory.Cells[0, 0] := SReplacementInfoHeader;
  grdHistory.Cells[1, 0] := SReplacementTime;
  for i := 0 to CorrectionHistory.Count - 1 do
  begin
    grdHistory.Cells[0, i + 1] := CorrectionHistory.GetItem(i).InfoString;
    grdHistory.Cells[1, i + 1] := FormatDateTime('hh:nn:ss', CorrectionHistory.GetItem(i).Time);
  end;
  btnDisable.Enabled := (CorrectionHistory.Count > 0);
end;

procedure TfmProofreaderConfig.pcAPRChange(Sender: TObject);
resourcestring
  SWordTableCreated = 'Word replacement table has been created.';
  SDictionaryTableCreated = 'Dictionary table has been created.';
var
  OperatingTable: TTable;
begin
  Assert(fmProofReaderDM <> nil);
  case pcAPR.ActivePage.Tag of
    0:
      begin
        OperatingTable := fmProofReaderDM.tReplacement;
        try
          OperatingTable.Open;
        except
          on E: Exception do
          begin
            OperatingTable.CreateTable;
            // Do not localize
            OperatingTable.AddIndex('idxReplacementPrimary', 'Language;Typed', [ixPrimary]);
            OperatingTable.Open;
            MessageDlg(SWordTableCreated, mtInformation, [mbOK], 0);
          end;
        end;
      end;
    1:
      begin
        OperatingTable := fmProofReaderDM.tDictionary;
        try
          OperatingTable.Open;
        except
          on E: Exception do
          begin
            OperatingTable.CreateTable;
            // Do not localize
            OperatingTable.AddIndex('idxDictionaryPrimary', 'Language;Word', [ixPrimary]);
            OperatingTable.Open;
            MessageDlg(SDictionaryTableCreated, mtInformation, [mbOK], 0);
          end;
        end;
      end;
    2: { nothing };
  else
    raise Exception.Create('Invalid tag value'); // do not localize
  end;
  cbLanguageChange(nil);
end;

function TfmProofreaderConfig.GetCurrentModuleSyntaxHighlighter: TSyntaxHighlighter;
var
  IModuleInterface: TIModuleInterface;
  IEditorInterface: TIEditorInterface;
  CurrentFile: string;
begin
  Assert(ToolServices <> nil);

  Result := shNone;

  CurrentFile := ToolServices.GetCurrentFile;
  IModuleInterface := ToolServices.GetModuleInterface(CurrentFile);
  if IModuleInterface <> nil then
  try
    IEditorInterface := IModuleInterface.GetEditorInterface;
    if IEditorInterface <> nil then
    try
      Result := IEditorInterface.SetSyntaxHighlighter(shQuery);
    finally
      IEditorInterface.Free;
    end;
  finally
    IModuleInterface.Free;
  end;
end;

procedure TfmProofreaderConfig.FormShow(Sender: TObject);
var
  i: TReplacementSource;
begin
  Assert(fmProofReaderDM <> nil);

  // Fill combo box with "languages" to select from
  for i := Low(TReplacementSource) to High(TReplacementSource) do
    cbLanguage.Items.Add(ReplacementSourceText[i]);

  // Initialize option settings
  cbBeep.Checked := fmProofReaderDM.BeepOnReplace;
  cbReplacerActive.Checked := fmProofReaderDM.ReplacerActive;
  cbEnableDictionary.Checked := fmProofReaderDM.DictionaryActive;
  cbEnableCompiler.Checked := fmProofReaderDM.CompilerActive;
  cbCaseDiffer.Checked := fmProofReaderDM.DictionaryCaseDiffer;
  cbOtherLetter.Checked := fmProofReaderDM.OtherLetter;
  cbNearbyLetter.Checked := fmProofReaderDM.NearbyLetter;
  cbNoLetter.Checked := fmProofReaderDM.NoLetter;
  cbMoreLetter.Checked := fmProofReaderDM.MoreLetter;
  cbMixed.Checked := fmProofReaderDM.MixedLetter;
  cbNoFirstOther.Checked := fmProofReaderDM.NoFirstOther;

  // C++Builder and Delphi 3 does not have compiler
  // support for proof-reading:
  {$IFDEF GX_BCB}
    cbEnableCompiler.Checked := False;
    cbEnableCompiler.Enabled := False;
  {$ENDIF GX_BCB}
  {$IFNDEF GX_VER120_up}
    cbEnableCompiler.Checked := False;
    cbEnableCompiler.Enabled := False;
  {$ENDIF GX_VER120_up}

  case GetCurrentModuleSyntaxHighlighter of
    shNone,
    shQuery,
    shPascal: cbLanguage.ItemIndex := Ord(rtPasSrc);
    {$IFDEF GX_VER120_up}
    shC:      cbLanguage.ItemIndex := Ord(rtCPPSrc);
    {$ENDIF GX_VER120_up}
    shSQL:    cbLanguage.ItemIndex := Ord(rtSQLSrc);
  end;
  UpdateHistoryGrid;
  pcAPRChange(nil);
end;

procedure TfmProofreaderConfig.btnOKClick(Sender: TObject);
begin
  Assert(fmProofReaderDM <> nil);
  // Store options
  fmProofReaderDM.BeepOnReplace := cbBeep.Checked;
  fmProofReaderDM.ReplacerActive := cbReplacerActive.Checked;
  fmProofReaderDM.DictionaryActive := cbEnableDictionary.Checked;
  fmProofReaderDM.CompilerActive := cbEnableCompiler.Checked;
  fmProofReaderDM.DictionaryCaseDiffer := cbCaseDiffer.Checked;
  fmProofReaderDM.OtherLetter := cbOtherLetter.Checked;
  fmProofReaderDM.NearbyLetter := cbNearbyLetter.Checked;
  fmProofReaderDM.NoLetter := cbNoLetter.Checked;
  fmProofReaderDM.MoreLetter := cbMoreLetter.Checked;
  fmProofReaderDM.MixedLetter := cbMixed.Checked;
  fmProofReaderDM.NoFirstOther := cbNoFirstOther.Checked;
  // Reload tables based on the new settings
  fmProofReaderDM.ReLoad;
end;

procedure TfmProofreaderConfig.cbLanguageChange(Sender: TObject);
var
  OperatingTable: TTable;
begin
  Assert(fmProofReaderDM <> nil);

  case pcAPR.ActivePage.Tag of
    0:
      OperatingTable := fmProofReaderDM.tReplacement;
    1:
      OperatingTable := fmProofReaderDM.tDictionary;
    2:
      OperatingTable := nil;
  else
    raise Exception.Create('Invalid tag value');  // do not localize
  end;

  if OperatingTable <> nil then
  begin
    OperatingTable.Filtered := True;
    // do not localize
    OperatingTable.Filter := 'Language=' + IntToStr(cbLanguage.ItemIndex);
  end;

  fmProofReaderDM.DefaultLanguage := TReplacementSource(cbLanguage.ItemIndex);
end;

procedure TfmProofreaderConfig.btnDisableClick(Sender: TObject);
resourcestring
  SRuleRemoved = 'Rule has been removed from replacement table.';
  SWordAdded = 'The word "%s" has been added to the dictionary.';
  SWordNotFound = 'The AutoCorrect word ''%s'' could not be found (%d)';
var
  Correction: TCorrectionItem;
begin
  Assert(fmProofReaderDM <> nil);
  try
    Correction := fmProofReaderDM.History.GetItem(grdHistory.Row - 1);
    if Correction <> nil then
    begin
      case Correction.CorrectionKind of

        ckAutoCorrection:
          with fmProofReaderDM.tReplacement do
          begin
            Filtered := False;
            Open;
            if Locate('Language;Typed', VarArrayOf([Ord(Correction.SourceLanguage), Correction.OriginalText]), [loCaseInsensitive]) then
            begin
              Delete;
              MessageDlg(SRuleRemoved, mtInformation, [mbOK], 0);
            end
            else
              MessageDlg(Format(SWordNotFound, [Correction.OriginalText, Ord(Correction.SourceLanguage)]),
                          mtInformation, [mbOK], 0);
          end;

        ckWord:
          with fmProofReaderDM.tDictionary do
          begin
            Filtered := False;
            Open;
            AppendRecord([Ord(Correction.SourceLanguage), Correction.OriginalText]);
            MessageDlg(Format(SWordAdded, [Correction.OriginalText]),
                        mtInformation, [mbOK], 0);
          end;
      end; // case
    end;
  except
    on E: Exception do
    begin
      // nothing
    end;
  end;
end;

procedure TfmProofreaderConfig.cbEnableDicitionaryClick(Sender: TObject);
var
  i: Integer;
begin
  dbgDictionary.Enabled := cbEnableDictionary.Checked;
  dbgDictionary.Repaint;
  for i := 0 to gbReplaceIf.ControlCount - 1 do
    gbReplaceIf.Controls[i].Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked;
  cbNearbyLetter.Enabled := cbNearbyLetter.Enabled and cbOtherLetter.Checked;
end;

procedure TfmProofreaderConfig.cbEnableCompilerClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to gbReplaceIf.ControlCount - 1 do
    gbReplaceIf.Controls[i].Enabled := cbEnableDictionary.Checked or cbEnableCompiler.Checked;
  cbNearbyLetter.Enabled := cbNearbyLetter.Enabled and cbOtherLetter.Checked;

  {$IFDEF VER120}
  // Delphi 4.0 has stability problems with the kibitz compiler
  ShowGxMessageBox(TShowD4KibitzMessage);
  {$ENDIF VER120}
end;

procedure TfmProofreaderConfig.cbReplacerActiveClick(Sender: TObject);
begin
  dbgReplacement.Enabled := cbReplacerActive.Checked
end;

procedure TfmProofreaderConfig.pcAPRChanging(Sender: TObject; var AllowChange: Boolean);
var
  OperatingTable: TTable;
begin
  Assert(fmProofReaderDM <> nil);

  case pcAPR.ActivePage.Tag of
    0:
      OperatingTable := fmProofReaderDM.tReplacement;
    1:
      OperatingTable := fmProofReaderDM.tDictionary;
    2:
      OperatingTable := nil;
  else
    raise Exception.Create('Invalid tag value');
  end;

  if OperatingTable <> nil then
    with OperatingTable do
    try
      if State in [dsEdit, dsInsert] then
        Post;
    except
      on E: Exception do
      begin
        Cancel;
        { swallow exception }
      end;
    end;
end;

procedure TfmProofreaderConfig.btnCancelClick(Sender: TObject);
begin
  Assert(fmProofReaderDM <> nil);

  fmProofReaderDM.Reload;
end;

procedure TfmProofReaderConfig.btnImportClick(Sender: TObject);
var
  Words: TStringList;
  i: Integer;
  CurrentIdeFolder: string;
begin
  Assert(fmProofReaderDM <> nil);

  CurrentIdeFolder := GetCurrentDir;
  try
    if not dlgGetWordlist.Execute then
      Exit;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;

  if FileExists(dlgGetWordlist.FileName) then
  begin
    Words := TStringList.Create;
    try
      Words.LoadFromFile(dlgGetWordlist.FileName);
      with fmProofReaderDM.tDictionary do
      begin
        DisableControls;
        try
          for i := 0 to Words.Count - 1 do
          try
            Insert;
            FieldByName('Word').AsString := Trim(Words[i]);
            FieldByName('Language').AsInteger := cbLanguage.ItemIndex;
            Post;
          except
            on E: Exception do
            begin
              Cancel;
              { swallow exception }
            end;
          end;
        finally
          EnableControls;
        end;
      end;
    finally
      Words.Free;
    end;
  end;
end;

procedure TfmProofreaderConfig.btnExportClick(Sender: TObject);
var
  AFile: TextFile;
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if not dlgPutWordlist.Execute then
      Exit;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;

  AssignFile(AFile, dlgPutWordlist.FileName);
  Rewrite(AFile);
  with fmProofReaderDM.tDictionary do
  try
    DisableControls;
    try
      First;
      while not Eof do
      begin
        WriteLn(AFile, FieldByName('Word').AsString);
        Next;
      end;
    finally
      EnableControls;
    end;
  finally
    CloseFile(AFile)
  end;
end;

procedure TfmProofreaderConfig.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 20);
end;

procedure TfmProofreaderConfig.cbOtherLetterClick(Sender: TObject);
begin
  cbNearbyLetter.Enabled := cbOtherLetter.Checked and cbOtherLetter.Enabled;
end;

{ TShowD4KibitzMessage }

function TShowD4KibitzMessage.GetMessage: string;
resourcestring
  SKibitzCompilerProblems =
  'Please note that the Delphi 4 kibitz compiler used for context based word '+
  'matching is not perfectly reliable.'#13 +
  #13 +
  'Often, it may not work for Code Proofreader due to internal compiler problems. ' +
  'These kibitz compiler problems may sometimes even require a restart of the IDE ' +
  'before you are able to build your application.'#13 +
  #13 +
  'It is highly unlikely, though, that the use of this option in the Code '+
  'Proofreader will cause data loss.';
begin
  Result := SKibitzCompilerProblems;
end;

{$ELSE GX_NOBDE}
interface implementation
{$ENDIF GX_NOBDE}

end.

