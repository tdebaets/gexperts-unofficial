unit GX_GrepResults;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GX_Search, ToolIntf, ExptIntf, Buttons, ComCtrls,
  {$IFDEF GX_VER140_up}
  ComponentDesigner,
  {$ELSE not GX_VER140_up}
  {$IFNDEF GX_BCB} {$IFNDEF GX_NotPackageBuilt}
  LibIntf,
  {$ENDIF GX_NotPackageBuilt} {$ENDIF GX_BCB}
  {$ENDIF}
  {$IFDEF ACEREPORTER}
  ACEOut, ACEFile, // Errors here mean Ace is not found: see GX_CondDefine.inc
  {$ENDIF ACEREPORTER}

  EditIntf, Registry, Menus, GX_Experts, DropSource, GX_IdeDock;

type
  TGrepAction = (gaAllFilesGrep, gaCurrentOnlyGrep, gaOpenFilesGrep, gaDirGrep);

  TSearchResult = class(TCollectionItem)
  private
    FLine: string;
    FLineNo: Integer;
    FSPos: Integer;
    FEPos: Integer;
  published
    property Line: string read FLine write FLine;
    property LineNo: Integer read FLineNo write FLineNo;
    property SPos: Integer read FSPos write FSPos;
    property EPos: Integer read FEPos write FEPos;
  end;

  TSearchResults = class(TCollection)
  private
    FExpanded: Boolean;
    FFileName: string;
    function GetItem(Index: Integer): TSearchResult;
    procedure SetItem(Index: Integer; Value: TSearchResult);
  public
    constructor Create;
    function Add: TSearchResult;
    property Expanded: Boolean read FExpanded write FExpanded;
    property FileName: string read FFileName write FFileName;
    property Items[Index: Integer]: TSearchResult read GetItem write SetItem; default;
  end;

  // Saved grep settings (used for refresh)
  TGrepSettings = packed record
    NoComments: Boolean;
    NoCase: Boolean;
    WholeWord: Boolean;
    RegEx: Boolean;
    IncludeSubdirs: Boolean;
    Directory: string;
    Mask: string;
    Pattern: string;
    GrepAction: TGrepAction;
    CanRefresh: Boolean;
  end;

  TGrepExpert = class;

  TfmGrepResults = class(TfmIdeDockForm)
    pnlToolbar: TPanel;
    StatusBar: TStatusBar;
    sbSearch: TSpeedButton;
    sbGoto: TSpeedButton;
    sbContract: TSpeedButton;
    sbExpand: TSpeedButton;
    sbAbort: TSpeedButton;
    sbHelp: TSpeedButton;
    sbFont: TSpeedButton;
    dlgGrepFont: TFontDialog;
    lbResults: TListBox;
    sbPrint: TSpeedButton;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuSearch: TMenuItem;
    mnuExit: TMenuItem;
    mnuFileSep: TMenuItem;
    mnuPrint: TMenuItem;
    mnuList: TMenuItem;
    mnuContract: TMenuItem;
    mnuExpand: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFont: TMenuItem;
    mnuHelpHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuAbort: TMenuItem;
    mnuGotoSelected: TMenuItem;
    mnuRefresh: TMenuItem;
    sbRefresh: TSpeedButton;
    mnuOnTop: TMenuItem;
    sbOnTopUp: TSpeedButton;
    sbOnTopDown: TSpeedButton;
    procedure btnNewClick(Sender: TObject);
    procedure sbGotoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbContractClick(Sender: TObject);
    procedure sbExpandClick(Sender: TObject);
    procedure sbAbortClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbResultsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure sbFontClick(Sender: TObject);
    procedure lbResultsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure sbPrintClick(Sender: TObject);
    procedure mnuHelpClick(Sender: TObject);
    procedure mnuContractClick(Sender: TObject);
    procedure mnuExpandClick(Sender: TObject);
    procedure mnuSearchClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuFontClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuAbortClick(Sender: TObject);
    procedure mnuGotoSelectedClick(Sender: TObject);
    procedure mnuTopClick(Sender: TObject);
    procedure lbResultsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mnuRefreshClick(Sender: TObject);
    procedure mnuOnTopClick(Sender: TObject);
    procedure sbOnTopClick(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
  private
    Total: Integer;
    DragSource: TDropFileSource;
    DragPoint: TPoint;
    GrepSettings: TGrepSettings;
    {$IFDEF GX_BCB}
    DupeFileList: TStringList;
    {$ENDIF GX_BCB}
    procedure FoundIt(Sender: TObject; LineNo: Integer; Line: string; SPos, EPos: Integer);
    procedure StartSearch(Sender: TObject);
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ExpandContract(n: Integer);
    procedure ResizeListBox;
    {$IFDEF ACEREPORTER}
    procedure PreviewReport(AceFile: TAceFile);
    {$ENDIF ACEREPORTER}
    procedure ClearResultsListbox;
  protected
    function GetOnTop: Boolean;
    procedure SetOnTop(Value: Boolean);
    procedure WMExitSizeMove(var Message: TMessage); message WM_EXITSIZEMOVE;
  public
    SAbort: Boolean;
    Searching: Boolean;
    OpenFiles: Boolean;
    IncludeDFM: Boolean;
    Results: TSearchResults;
    Searcher: TSearcher;
    FileCount: Integer;
    GrepExpert: TGrepExpert;
    procedure Execute(Refresh: Boolean);
    property OnTop: Boolean read GetOnTop write SetOnTop;
  end;

  TGrepExpert = class(TGX_EnhExpert)
  private
    FGrepMiddle: Boolean;
    FGrepSave: Boolean;
    FGrepANSICompatible: Boolean;
    FSearchList: TStrings;
    FMaskList: TStrings;
    FDirList: TStrings;
    FGrepNoCase: Boolean;
    FGrepNoComments: Boolean;
    FGrepDFM: Boolean;
    FGrepSearch: Integer;
    FGrepSub: Boolean;
    FGrepWholeWord: Boolean;
    FGrepRegEx: Boolean;
    procedure SetSearchList(New: TStrings);
    procedure SetMaskList(New: TStrings);
    procedure SetDirList(New: TStrings);
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ShowModal;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    property GrepMiddle: Boolean read FGrepMiddle write FGrepMiddle;
    property GrepSave: Boolean read FGrepSave write FGrepSave;
    property GrepANSICompatible: Boolean  read FGrepANSICompatible write FGrepANSICompatible;
    property GrepNoCase: Boolean read FGrepNoCase write FGrepNoCase;
    property GrepNoComments: Boolean read FGrepNoComments write FGrepNoComments;
    property GrepDFM: Boolean read FGrepDFM write FGrepDFM;
    property GrepSearch: Integer read FGrepSearch write FGrepSearch;
    property GrepSub: Boolean read FGrepSub write FGrepSub;
    property GrepWholeWord: Boolean read FGrepWholeWord write FGrepWholeWord;
    property GrepRegEx: Boolean read FGrepRegEx write FGrepRegEx;

    property SearchList: TStrings read FSearchList write SetSearchList;
    property MaskList: TStrings read FMaskList write SetMaskList;
    property DirList: TStrings read FDirList write SetDirList;
  end;

var
  fmGrepResults: TfmGrepResults = nil;
  StandAlone: Boolean = False;
  GrepStandAlone: TGrepExpert = nil;

procedure ShowGrep; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
//procedure CloseGrep; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

function GrepFile(Param: Pointer; const FileName, Unitname, FormName: string): Boolean; stdcall;

implementation

{$R *.DFM}

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  {$IFDEF ACEREPORTER}
  GX_PrintPreview, GX_PrinterFunc,
  {$ENDIF ACEREPORTER}
  {$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
  {$ENDIF GX_UseNativeToolsApi}
  GX_GrepSearch, GX_EditReader, GX_GenFunc, GX_GrepOptions,
  GX_GExperts, GX_ConfigurationInfo, FileCtrl;

{$IFDEF GX_BCB}
var
  BCB_SwapSourceFormView: procedure;
  BCB_HasForm: function: Boolean;
{$ENDIF GX_BCB}

resourcestring
  SPrintingDisabled = 'Printing is disabled (Ace Reporter has not been built into the expert)';

function MyTrim(var st: string): Integer;
begin
  Result := 0;
  while (Length(st) > 0) and (st[1] in [#9, #32]) do
  begin
    Delete(st, 1, 1);
    Inc(Result);
  end;
end;

function GrepFile(Param: Pointer; const FileName, UnitName, FormName: string): Boolean; stdcall;
var
  Form: TfmGrepResults;
  ModIntf: TIModuleInterface;
  DfmModIntf: TIModuleInterface;
begin
  Result := False;
  ModIntf := nil;
  DfmModIntf := nil;
  try
    Form := TfmGrepResults(Param);
    Application.ProcessMessages;
    //{$IFOPT D+} SendDebug('------: Searching project file ' + FileName + ' Unit: ' + UnitName + ' Form: ' + FormName); {$ENDIF}
    Result := not Form.SAbort;
    if IsDprOrPas(FileName) {$IFDEF GX_BCB} or IsCppSourceModule(FileName) {$ENDIF GX_BCB} then
    begin
      {$IFDEF GX_BCB}
      if Form.DupeFileList.IndexOf(Filename) > -1 then
        Exit;
      Form.DupeFileList.Add(FileName);
      {$ENDIF GX_BCB}

      Form.Results := nil;
      try
        ModIntf := ToolServices.GetModuleInterface(FileName);
        DfmModIntf := ToolServices.GetModuleInterface(ChangeFileExt(FileName, '.dfm'));
        if (ModIntf = nil) and (DfmModIntf <> nil) then
          Form.Searcher.FileName := ChangeFileExt(FileName, '.dfm')
        else
          Form.Searcher.FileName := Filename;
      finally
        ModIntf.Free;
        DfmModIntf.Free;
      end;
      if Form.OpenFiles and (Form.Searcher.Mode = mmFile) then
        Exit;

      Form.Searcher.Execute;
      Inc(Form.FileCount);
      Form.Results := nil;
    end;
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebugEx('GrepFile: ' + E.Message, mtError); {$ENDIF}
    end;
  end;
end;

constructor TSearchResults.Create;
begin
  inherited Create(TSearchResult);
end;

function TSearchResults.Add: TSearchResult;
begin
  Result := TSearchResult(inherited Add);
end;

function TSearchResults.GetItem(Index: Integer): TSearchResult;
begin
  Result := TSearchResult(inherited GetItem(Index));
end;

procedure TSearchResults.SetItem(Index: Integer; Value: TSearchResult);
begin
  inherited SetItem(Index, Value);
end;

procedure TfmGrepResults.StartSearch(Sender: TObject);
resourcestring
  SProcessing = 'Processing ';
begin
  StatusBar.Panels.Items[0].Text := SProcessing + Searcher.FileName;
  //{$IFOPT D+} SendDebug('Searching File: ' + Searcher.FileName); {$ENDIF}
  StatusBar.Repaint;
end;

procedure TfmGrepResults.FoundIt(Sender: TObject; LineNo: Integer; Line: string; SPos, EPos: Integer);
var
  AResult: TSearchResult;
begin
  Application.ProcessMessages;
  Inc(Total);
  if (Results = nil) or (Results.FileName <> Searcher.FileName) then
  begin
    Results := TSearchResults.Create;
    Results.FileName := Searcher.FileName;
    lbResults.Items.AddObject(Searcher.FileName, Results);
  end;
  AResult := Results.Add;
  AResult.Line := Line;
  AResult.LineNo := LineNo;
  AResult.SPos := SPos;
  AResult.EPos := EPos;
end;

procedure TfmGrepResults.Execute(Refresh: Boolean);
resourcestring
  SNoFileOpen = 'No file is currently open';
var
  Dlg: TfmGrepSearch;
  SStart: Integer;
  SEnd: Integer;

  procedure CurrentOnlyGrep;
  var
    CurrentFile: string;
    ModIntf: TIModuleInterface;
    FormIntf: TIFormInterface;
  begin
    Results := nil;
    FormIntf := nil;
    if ToolServices = nil then Exit;
    CurrentFile := ToolServices.GetCurrentFile;

    ModIntf := ToolServices.GetModuleInterface(CurrentFile);
    try
      {$IFDEF GX_BCB}
      if ModIntf = nil then
      begin
        CurrentFile := ChangeFileExt(CurrentFile, '.cpp');
        ModIntf := ToolServices.GetModuleInterface(CurrentFile);
      end;
      {$ENDIF GX_BCB}
      if ModIntf = nil then
      begin
        CurrentFile := ChangeFileExt(CurrentFile, '.pas');
        ModIntf := ToolServices.GetModuleInterface(CurrentFile);
      end;
      if ModIntf <> nil then
        FormIntf := ModIntf.GetFormInterface;

      // If the current module is a DFM but we can't get a form interafce,
      // it is being viewed as text, and is treated as the
      // current source module
      if IsDfm(CurrentFile) and (FormIntf <> nil) then
      begin
        {$IFDEF GX_BCB}
        if FileExists(ChangeFileExt(CurrentFile, '.cpp')) or
           ToolServices.IsFileOpen(ChangeFileExt(CurrentFile, '.cpp')) then
        begin
          CurrentFile := ChangeFileExt(CurrentFile, '.cpp');
        end
        else
        {$ENDIF GX_BCB}
        CurrentFile := ChangeFileExt(CurrentFile, '.pas');
      end;
    finally
      ModIntf.Free;
      FormIntf.Free;
    end;
    if CurrentFile <> '' then
    begin
      Searcher.FileName := CurrentFile;
      Searcher.Execute;
      Inc(FileCount);
    end
    else
      MessageDlg(SNoFileOpen, mtError, [mbOK], 0);
  end;

  procedure AllFilesGrep;
  begin
    Self.Cursor := crHourglass;
    try
      ToolServices.EnumProjectUnits(GrepFile, Pointer(Self));
    finally
      Self.Cursor := crDefault;
    end;
  end;

  procedure DirGrep(Dir, Mask: string);
  var
    Search: TSearchRec;
    Result: Integer;
    S: TStringList;
    i: Integer;
  begin
    //{$IFOPT D+}SendDebug('DirSearch on:' +Dir+' Mask: '+Mask);{$ENDIF}
    Dir := AddSlash(Dir);
    S := TStringList.Create;
    try
      for i := 1 to Length(Mask) do
        if Mask[i] in [';', ','] then
          Mask[i] := #13;

      S.Text := Mask;

      // First scan sub-directories if option is selected
      if GrepSettings.IncludeSubdirs then
      begin
        Result := FindFirst(Dir + '*.*', faAnyFile, Search);
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
            begin
              if (Search.Name <> '.') and (Search.Name <> '..') then
                DirGrep(Dir + Search.Name, Mask);
            end;
            Result := FindNext(Search);
          end;
        finally
          FindClose(Search);
        end;
      end;

      for i := 0 to S.Count-1 do
      begin
        if SAbort then Break;
        Result := FindFirst(Dir + Trim(S.Strings[i]), faAnyFile, Search);
        try
          while Result = 0 do
          begin
            if (Search.Attr and faDirectory) <> 0 then
              Result := FindNext(Search)
            else
            begin
              Results := nil;
              Searcher.FileName := Dir + Search.Name;
              Searcher.Execute;

              Application.ProcessMessages;
              if SAbort then Break;

              Inc(FileCount);
              Result := FindNext(Search);
            end;
          end;
        finally
          FindClose(Search);
        end;
      end;
    finally
      S.Free;
    end;
  end;

resourcestring
  SGrepActive = 'A Grep search is currently active; either abort it or wait until it is finished.';
  SGrepStatistics = '%d files in %g seconds';
  SMatches = ' matches';
begin
  //! StH: This code needs some cleanup attention
  if Searching then
  begin
    MessageDlg(SGrepActive, mtInformation, [mbOK], 0);
    Exit;
  end;

  try
    if not (Refresh and GrepSettings.CanRefresh) then
    begin
    Dlg := TfmGrepSearch.Create(nil);
    try
      if Dlg.ShowModal <> mrOk then
        Exit;
        StatusBar.Panels.Items[1].Text := '';

        IncludeDFM := Dlg.cbDFM.Checked;

        // Save dialog settings to local vars
        // TODO: Restore once NoComments works
        GrepSettings.NoComments := False; //Dlg.cbNoComments.Checked;
        GrepSettings.NoCase := Dlg.cbNoCase.Checked;
        GrepSettings.WholeWord := Dlg.cbWholeWord.Checked;
        GrepSettings.RegEx := Dlg.cbRegEx.Checked;
        GrepSettings.Pattern := Dlg.cbText.Text;
        GrepSettings.IncludeSubdirs := Dlg.cbInclude.Checked;
        if Dlg.rbAllFiles.Checked then
          GrepSettings.GrepAction := gaAllFilesGrep
        else if Dlg.rbCurrentOnly.Checked then
          GrepSettings.GrepAction := gaCurrentOnlyGrep
        else if Dlg.rbOpenFiles.Checked then
          GrepSettings.GrepAction := gaOpenFilesGrep
        else
        begin
          GrepSettings.Mask := Dlg.cbMasks.Text;
          GrepSettings.GrepAction := gaDirGrep;
          // Require a full path as not to confuse the IDE when opening files
          GrepSettings.Directory := Dlg.cbDirectory.Text;
          if not DirectoryExists(GrepSettings.Directory) then
            raise Exception.Create('The specified search directory does not exist');
        end;
        GrepSettings.CanRefresh := True;
      finally
        Dlg.Free;
      end;
    end;

    try
      Searching := True;
      BringToFront;
      Total := 0;
      FileCount := 0;
      SAbort := False;
      OpenFiles := False;

      sbSearch.Enabled := False;
      sbRefresh.Enabled := False;
      sbPrint.Enabled := False;
      sbGoto.Enabled := False;
      sbExpand.Enabled := False;
      sbContract.Enabled := False;
      sbFont.Enabled := False;
      sbOnTopDown.Enabled := False;
      sbOnTopUp.Enabled := False;
      sbAbort.Enabled := True;

      IdeDockManager.ShowForm(Self);

      SStart := GetTickCount;
      Self.Cursor := crHourglass;
      Searcher := TSearcher.Create('');
      try
        Searcher.BufSize := 30000;
        Searcher.OnFound := FoundIt;
        Searcher.OnStartSearch := StartSearch;

        // TODO: Restore once NoComments works
        Searcher.NoComments := False; //GrepSettings.NoComments;
        Searcher.IncludeDFM := IncludeDFM;
        if GrepSettings.NoCase then
          Searcher.SearchOptions := [soCaseSensitive];
        if GrepSettings.WholeWord then
          Searcher.SearchOptions := Searcher.SearchOptions + [soWholeWord];
        if GrepSettings.RegEx then
          Searcher.SearchOptions := Searcher.SearchOptions + [soRegEx];
        Searcher.ANSICompatible := GrepExpert.GrepANSICompatible;

        ClearResultsListbox;
        Searcher.SetPattern(GrepSettings.Pattern);

        Application.ProcessMessages;
        {$IFDEF GX_BCB}
        DupeFileList := TStringList.Create;
        try
        {$ENDIF GX_BCB}

        case GrepSettings.GrepAction of
          gaAllFilesGrep: AllFilesGrep;
          gaCurrentOnlyGrep: CurrentOnlyGrep;
          gaOpenFilesGrep:
            begin
              OpenFiles := True;
              AllFilesGrep;
            end;
          gaDirGrep:
            begin
              if Length(Trim(GrepSettings.Mask)) = 0 then
              {$IFDEF GX_BCB}
                DirGrep(GrepSettings.Directory, '*.cpp;*.hpp;*.pas;*.h')
              {$ELSE}
                DirGrep(GrepSettings.Directory, '*.pas;*.dpr;*.inc')
              {$ENDIF GX_BCB}
              else
                DirGrep(GrepSettings.Directory, UpperCase(GrepSettings.Mask));
            end;
        end;	// end case
        {$IFDEF GX_BCB}
        finally
          DupeFileList.Free;
        end;
        {$ENDIF GX_BCB}

      finally
        Searching := False;

        SEnd := GetTickCount;
        Searcher.Free;
        Self.Cursor := crDefault;

        StatusBar.Panels.Items[0].Text := Format(SGrepStatistics, [FileCount, (SEnd - SStart) / 1000]);

        lbResults.Refresh;
        lbResults.Sorted := True;
        lbResults.Sorted := False;
        if lbResults.Items.Count = 1 then
        begin
          lbResults.ItemIndex := 0;
          sbExpandClick(sbExpand);
        end;
      end;
    finally
      sbPrint.Enabled := True;
      sbSearch.Enabled := True;
      sbRefresh.Enabled := True;
      sbExpand.Enabled := True;
      sbContract.Enabled := True;
      sbFont.Enabled := True;
      sbOnTopDown.Enabled := True;
      sbOnTopUp.Enabled := True;

      if ToolServices <> nil then
        sbGoto.Enabled := True;
      sbAbort.Enabled := False;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
  StatusBar.Panels.Items[1].Text := IntToStr(Total) + SMatches;
end;

procedure TfmGrepResults.ClearResultsListbox;
var
  i: Integer;
  j: Integer;
begin
  for i := 0 to lbResults.Items.Count - 1 do
  begin
    if lbResults.Items.Objects[i] is TSearchResults then
    begin
      j := i + 1;
      // First nil out all child item pointers before freeing the parent collection
      while (j < lbResults.Items.Count) and (lbResults.Items.Objects[j] is TSearchResult) do
      begin
        lbResults.Items.Objects[j] := nil;
        Inc(j);
      end;
      TSearchResults(lbResults.Items.Objects[i]).Free;
      lbResults.Items.Objects[i] := nil;
    end;
  end;
  lbResults.Clear;
end;

procedure TfmGrepResults.lbResultsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  DragTreshold: Integer;
begin
  {$IFNDEF GX_VER120_up}
  DragTreshold := 10;
  {$ELSE  GX_VER120_up}
  DragTreshold := Mouse.DragThreshold;
  {$ENDIF  GX_VER120_up}
  // Make sure mouse has moved at least threshold pixels before starting a drag
  if (DragPoint.X = -1) or ((Shift <> [ssLeft]) and (Shift <> [ssRight])) or
    ((Abs(DragPoint.X - X) < DragTreshold) and (Abs(DragPoint.Y - Y) < DragTreshold)) then
  begin
    Exit;
  end;

  i := lbResults.ItemAtPos(Point(X, Y), True);

  if i >= 0 then
  begin
    DragSource.Files.Clear;

    if lbResults.Items.Objects[i] is TSearchResults then
      DragSource.Files.Add(TSearchResults(lbResults.Items.Objects[i]).FFileName)
    else if lbResults.Items.Objects[i] is TSearchResult then
      DragSource.Files.Add(TSearchResults(TSearchResult(lbResults.Items.Objects[i]).Collection).FFileName);

    if DragSource.Files.Count > 0 then
      DragSource.Execute;
  end;
end;

procedure TfmGrepResults.lbResultsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DragPoint := Point(X, Y);
end;

procedure TfmGrepResults.btnNewClick(Sender: TObject);
begin
  Execute(False);
end;

procedure TfmGrepResults.sbGotoClick(Sender: TObject);
var
  Result: TSearchResult;
  EditRead: TEditReader;
  ModIntf: TIModuleInterface;
  CurrentFileName: string;
  CurrentFormName: string;
  {$IFDEF GX_UseNativeToolsApi}
  {$IFNDEF GX_BCB}
  {$IFDEF GX_VER140_up}
  {$ELSE not GX_VER140_up}
  Form: TIForm;
  Module: IDesignerModule;
  {$ENDIF}
  {$ENDIF GX_BCB}
  {$IFDEF GX_VER130_up}
  IModule: IOTAModule;
  ISourceEditor : IOTASourceEditor;
  IEditActions : IOTAEditActions;
  i: Integer;
  {$ENDIF GX_VER130_up}
  {$ENDIF GX_UseNativeToolsApi}
resourcestring
  SCouldNotOpenFile = 'Could not open file: ';
begin
  if (lbResults.ItemIndex < 0) then
    Exit;
  if (lbResults.Items.Objects[lbResults.ItemIndex] is TSearchResults) then
  begin
    ExpandContract(lbResults.ItemIndex);
    Exit;
  end;

  Result := TSearchResult(lbResults.Items.Objects[lbResults.ItemIndex]);
  if Result = nil then Exit;
  CurrentFileName := TSearchResults(Result.Collection).FileName;
  CurrentFormName := '';

  if StandAlone then
  begin
    // do not localize
    GXShellExecute(CurrentFileName, '', True);
    Exit;
  end;

  try
    if ToolServices = nil then Exit;
    {$IFOPT D+}SendDebug('Starting goto line operation to: ' +CurrentFileName); {$ENDIF}

    if IsDfm(CurrentFileName) then
    begin
      if ToolServices.GetCurrentFile = CurrentFileName then
      begin
      {$IFDEF GX_BCB}
        BCB_SwapSourceFormView;
      {$ELSE}
        {$IFDEF GX_UseNativeToolsApi}
        {$IFDEF GX_VER140_up}
        IModule := (BorlandIDEServices as IOTAModuleServices).FindModule(CurrentFileName);
        if IModule <> nil then
        for i := 0 to IModule.GetModuleFileCount - 1 do
        begin
          if IModule.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, ISourceEditor) = S_OK then
          begin
            if ISourceEditor.EditViewCount > 0 then
              if ISourceEditor.EditViews[0].QueryInterface(IOTAEditActions, IEditActions) = S_OK then
                IEditActions.SwapSourceFormView;
            Break;
          end;
        end;
        {$ELSE not GX_VER140_up}
        Form := CompLib.GetActiveForm;
        if Form <> nil then
        begin
          Module := Form.GetModule;
          if Module <> nil then
            Module.SwapSourceFormView;
        end;
        {$ENDIF}
        {$ENDIF GX_UseNativeToolsApi}
      {$ENDIF GX_BCB}
      end;
      CurrentFormName := CurrentFileName;
      {$IFDEF GX_VER120_up}  // D3 requires the DFM module be open, not the PAS/CPP
      ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
      try
        if ModIntf = nil then
        begin
          {$IFDEF GX_BCB}
          CurrentFileName := ChangeFileExt(CurrentFileName, '.cpp'); // This might be a PAS as well!
          {$ELSE}
          CurrentFileName := ChangeFileExt(CurrentFileName, '.pas');
          {$ENDIF GX_BCB}
        end;
      finally
        ModIntf.Free;
        //ModIntf := nil;
      end;
      {$ENDIF GX_VER120_up}
    end;

    {$IFOPT D+}SendDebug('Getting Module ' + CurrentFileName); {$ENDIF}
    ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
    try
      if ModIntf = nil then
      begin
        {$IFOPT D+}SendDebug('Did not get module, now opening file'); {$ENDIF}
        if FileExists(CurrentFileName) then
          ToolServices.OpenFile(CurrentFileName);
        ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
        if ModIntf = nil then
        begin
          // Happens in D3 when jumping into an unsaved DFM.  Anyone have a fix?
          MessageDlg(SCouldNotOpenFile + CurrentFileName, mtError, [mbOK], 0);
          Exit;
        end;
      end;
    finally
      ModIntf.Free;
      //ModIntf := nil;
    end;

    { TODO -oAnyone -cBug :
      Under Delphi 5+, once the DFM is open as text, the PAS can not be
      focused the first time this method is called. }
    if CurrentFormName = '' then
    begin
      {$IFDEF GX_UseNativeToolsApi} {$IFDEF GX_VER130_up}
      // Delphi 5/BCB5+ require that we manually swap the form and source views
      // Temporary hack, until we move this functionality to GX_OtaUtil
      if ExtractUpperFileExt(ToolServices.GetCurrentFile) = '.DFM' then
      begin
        IModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
        if IModule <> nil then
        for i := 0 to IModule.GetModuleFileCount - 1 do
        begin
          if IModule.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, ISourceEditor) = S_OK then
          begin
            if ISourceEditor.EditViewCount > 0 then
              if ISourceEditor.EditViews[0].QueryInterface(IOTAEditActions, IEditActions) = S_OK then
              begin
                IEditActions.SwapSourceFormView;
                IEditActions.SwapSourceFormView;
              end;
            Break;
          end;
        end;
      end;
      {$ENDIF GX_VER130_up} {$ENDIF GX_UseNativeToolsApi}
      // Since this edit reader is destroyed almost
      // immediately, do not call FreeFileData
      EditRead := TEditReader.Create(CurrentFileName);
      try
        {$IFOPT D+}SendDebug('Showing source code'); {$ENDIF}
        if GrepExpert.GrepMiddle then
          EditRead.GotoOffSetLine(Result.LineNo)
        else
          EditRead.GotoLine(Result.LineNo);
      finally
        EditRead.Free;
      end;
    end
    else // We are opening text DFM, not a regular source code unit
    begin
      {$IFOPT D+}SendDebug('Showing form '+CurrentFormName); {$ENDIF}

      {$IFDEF GX_BCB}
      BCB_SwapSourceFormView;
      //if BCB_HasForm then
      begin
      {$ELSE not GX_BCB}
      begin
        {$IFDEF GX_UseNativeToolsApi}
        {$IFDEF GX_VER140_up}
        IModule := (BorlandIDEServices as IOTAModuleServices).FindModule(CurrentFileName);
        if IModule <> nil then
        for i := 0 to IModule.GetModuleFileCount - 1 do
        begin
          if IModule.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, ISourceEditor) = S_OK then
          begin
            if ISourceEditor.EditViewCount > 0 then
              if ISourceEditor.EditViews[0].QueryInterface(IOTAEditActions, IEditActions) = S_OK then
                IEditActions.SwapSourceFormView;
            Break;
          end;
        end;
        {$ELSE not GX_VER140_up}
        Form := CompLib.GetActiveForm;
        if Form <> nil then
        begin
          Module := Form.GetModule;
          if Module <> nil then
            Module.SwapSourceFormView;
        end;
        {$ENDIF}
        {$ENDIF GX_UseNativeToolsApi}
        {$ENDIF GX_BCB}

        // Since this edit reader is destroyed almost
        // immediately, do not call FreeFileData
        EditRead := TEditReader.Create(CurrentFormName);
        try
          if GrepExpert.GrepMiddle then
            EditRead.GotoOffSetLine(Result.LineNo)
          else
            EditRead.GotoLine(Result.LineNo);
        finally
          EditRead.Free;
        end;
      end; // BCB_HasForm
    end; // else CurrentFormName <> ''
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmGrepResults.FormResize(Sender: TObject);
begin
  StatusBar.Panels.Items[0].Width := StatusBar.Width - 100;
  Invalidate;
end;

procedure TfmGrepResults.sbContractClick(Sender: TObject);
var
  i: Integer;
begin
  Self.Enabled := False;
  lbResults.Items.BeginUpdate;
  try
    i := 0;
    while i <= lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResult then
        lbResults.Items.Delete(i)
      else
      begin
        TSearchResults(lbResults.Items.Objects[i]).Expanded := False;
        Inc(i);
      end;
  finally
    lbResults.Items.EndUpdate;
    Self.Enabled := True;
  end;
end;

procedure TfmGrepResults.sbExpandClick(Sender: TObject);

  function Expand(n: Integer): Integer;
  var
    Results: TSearchResults;
    t: Integer;
  begin
    Results := TSearchResults(lbResults.Items.Objects[n]);
    for t := Results.Count - 1 downto 0 do
      lbResults.Items.InsertObject(n + 1, Results.Items[t].Line, Results.Items[t]);
    Results.Expanded := True;
    Result := n + Results.Count - 1;
  end;

var
  i: Integer;
begin
  Self.Enabled := False;
  lbResults.Items.BeginUpdate;
  try
    i := 0;
    while i <= lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResults then
      begin
        if not TSearchResults(lbResults.Items.Objects[i]).Expanded then
          i := Expand(i);
        Inc(i);
      end
      else
        Inc(i);
  finally
    lbResults.Items.EndUpdate;
    Self.Enabled := True;
  end;
end;

procedure TfmGrepResults.sbAbortClick(Sender: TObject);
begin
  SAbort := True;
  {$IFOPT D+} SendDebug('Grep abort requested'); {$ENDIF}
end;

procedure TfmGrepResults.FormDestroy(Sender: TObject);
begin
  try
    ClearResultsListbox;
    fmGrepResults := nil;
    SAbort := True;
    SaveSettings;
    DragSource.Free;
    DragSource := nil;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
  inherited;
end;

procedure TfmGrepResults.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 3);
end;

procedure TfmGrepResults.mnuHelpClick(Sender: TObject);
begin
  sbHelpClick(Self);
end;

procedure TfmGrepResults.mnuContractClick(Sender: TObject);
begin
  if sbContract.Enabled then
    sbContractClick(Self);
end;

procedure TfmGrepResults.mnuExpandClick(Sender: TObject);
begin
  if sbExpand.Enabled then
    sbExpandClick(Self);
end;

procedure TfmGrepResults.mnuSearchClick(Sender: TObject);
begin
  if sbSearch.Enabled then
    Execute(False);
end;

procedure TfmGrepResults.mnuPrintClick(Sender: TObject);
begin
  if sbPrint.Enabled then
    sbPrintClick(Self);
end;

procedure TfmGrepResults.mnuExitClick(Sender: TObject);
begin
  if StandAlone then
    ModalResult := mrCancel
  else
    Hide;
end;

procedure TfmGrepResults.mnuFontClick(Sender: TObject);
begin
  if sbFont.Enabled then
    sbFontClick(Self);
end;

procedure TfmGrepResults.mnuHelpAboutClick(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmGrepResults.mnuAbortClick(Sender: TObject);
begin
  if sbAbort.Enabled then
    sbAbort.Click;
end;

procedure TfmGrepResults.mnuGotoSelectedClick(Sender: TObject);
begin
  if sbGoto.Enabled then
    sbGoto.Click;
end;

procedure TfmGrepResults.mnuTopClick(Sender: TObject);
begin
  mnuSearch.Enabled := sbSearch.Enabled;
  mnuRefresh.Enabled := sbRefresh.Enabled;
  mnuPrint.Enabled := sbPrint.Enabled;
  mnuAbort.Enabled := sbAbort.Enabled;
  mnuGotoSelected.Enabled := sbGoto.Enabled;
  mnuExpand.Enabled := sbExpand.Enabled;
  mnuContract.Enabled := sbContract.Enabled;
  mnuFont.Enabled := sbFont.Enabled;
end;

procedure TfmGrepResults.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below strings
  if ConfigInfo  = nil then
    Exit;
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('GrepResult', 'Left', Left);
    RegIni.WriteInteger('GrepResult', 'Top', Top);
    RegIni.WriteInteger('GrepResult', 'Width', Width);
    RegIni.WriteInteger('GrepResult', 'Height', Height);
    RegIni.WriteBool('GrepResult', 'OnTop', OnTop);
    SaveFont(RegIni, 'GrepResult', lbResults.Font);
  finally
    RegIni.Free;
  end;
end;

procedure TfmGrepResults.LoadSettings;
const
  TopMenu = $2FF;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below strings
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    Left := RegIni.ReadInteger('GrepResult', 'Left', Left);
    Top := RegIni.ReadInteger('GrepResult', 'Top', Top);
    Width := RegIni.ReadInteger('GrepResult', 'Width', Width);
    Height := RegIni.ReadInteger('GrepResult', 'Height', Height);
    OnTop := RegIni.ReadBool('GrepResult', 'OnTop', False);
    LoadFont(RegIni, 'GrepResult', lbResults.Font);
  finally
    RegIni.Free;
  end;
end;

procedure TfmGrepResults.FormCreate(Sender: TObject);
begin
  inherited;
  OnTop := False;
  Searching := False;
  CenterForm(Self);
  LoadSettings;
  ResizeListBox;
  DragSource := TDropFileSource.Create(nil);
end;

procedure TfmGrepResults.lbResultsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: Integer;
begin
  if Button = mbLeft then
  begin
    p := lbResults.ItemAtPos(Point(X, Y), True);
    if p <> -1 then
      if lbResults.Items.Objects[p] is TSearchResults then
        ExpandContract(p);
  end;
end;

procedure TfmGrepResults.ExpandContract(n: Integer);
var
  Results: TSearchResults;
  i: Integer;
begin
  if (n < 0) or (n > lbResults.Items.Count - 1) or Searching then
    Exit;
  if lbResults.Items.Objects[n] is TSearchResults then
  begin
    try
      lbResults.Items.BeginUpdate;
      Results := TSearchResults(lbResults.Items.Objects[n]);
      if Results.Expanded then
      begin
        while (n + 1 <= lbResults.Items.Count - 1) and
              (not (lbResults.Items.Objects[n + 1] is TSearchResults)) do
        begin
          lbResults.Items.Delete(n + 1);
        end;
        Results.Expanded := False;
      end
      else
      begin
        for i := Results.Count - 1 downto 0 do
          lbResults.Items.InsertObject(n + 1, Results.Items[i].Line, Results.Items[i]);
        Results.Expanded := True;
      end
    finally
      lbResults.Items.EndUpdate;
    end;
  end;
end;

procedure TfmGrepResults.lbResultsKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '+',
    '-':  ExpandContract(lbResults.ItemIndex);
    #13:  sbGotoClick(sbGoto);
  end;
end;

procedure TfmGrepResults.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
  begin
    if Searching then
      SAbort := True
    else
    begin
      if StandAlone then
        ModalResult := mrCancel
      else
        Hide;
    end;
  end;
end;

procedure TfmGrepResults.sbFontClick(Sender: TObject);
begin
  dlgGrepFont.Font.Assign(lbResults.Font);
  if dlgGrepFont.Execute then
  begin
    lbResults.Font.Assign(dlgGrepFont.Font);
    ResizeListBox;
  end;
end;

procedure TfmGrepResults.ResizeListBox;
begin
  with lbResults do
  begin
    Canvas.Font.Assign(Font);
    ItemHeight := Canvas.TextHeight('W') + 3;  // "W" is any character
    Refresh;
  end;
end;

procedure TfmGrepResults.lbResultsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  TopColor: TColor;
  BottomColor: TColor;
  ResultsCanvas: TCanvas;
  c: Integer;
  p: Integer;
  i: Integer;
  st: string;
  Result: TSearchResult;
  sb: TColor;
  sf: TColor;
  nb: TColor;
  nf: TColor;
resourcestring
  SItemMatches = 'Matches: ';
begin
  ResultsCanvas := lbResults.Canvas;
  TopColor := clBtnHighlight;
  BottomColor := clBtnShadow;
  // Paint an expandable search file header (gray)
  if lbResults.Items.Objects[Index] is TSearchResults then
  begin
    ResultsCanvas.Brush.Color := clBtnFace;
    ResultsCanvas.Font.Color := clBtnText;
    ResultsCanvas.FillRect(Rect);
    Rect.Right := Rect.Right + 2;
    if odSelected in State then
      Frame3D(ResultsCanvas, Rect, BottomColor, TopColor, 1)
    else
      Frame3D(ResultsCanvas, Rect, TopColor, BottomColor, 1);
    i := ResultsCanvas.TextWidth('+');
    ResultsCanvas.TextOut(Rect.Left + i + 8, Rect.Top, lbResults.Items[Index]);
    //c:=Rect.Top+((Rect.Bottom-Rect.Top) div 2);
    if TSearchResults(lbResults.Items.Objects[Index]).Expanded then
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '-')
    else
      ResultsCanvas.TextOut(Rect.Left + 3, Rect.Top, '+');
    st := SItemMatches + IntToStr(TSearchResults(lbResults.Items.Objects[Index]).Count);
    p := ResultsCanvas.TextWidth(SItemMatches + '00000') + 10; // do not localize
    if (ResultsCanvas.TextWidth(lbResults.Items[Index]) + i + 7) <= Rect.Right - p then
      ResultsCanvas.TextOut(lbResults.ClientWidth - p, Rect.Top, st);
  end
  else   // Paint a search match line number and highlighted match
  begin
    Result := TSearchResult(lbResults.Items.Objects[Index]);
    if odSelected in State then
    begin
      nb := clHighLight;
      nf := clHighLightText;
      sb := clWindow;
      sf := clWindowText;
    end
    else
    begin
      sb := clHighLight;
      sf := clHighLightText;
      nb := clWindow;
      nf := clWindowText;
    end;
    ResultsCanvas.Brush.Color := nb;
    ResultsCanvas.Font.Color := nf;
    ResultsCanvas.FillRect(Rect);
    ResultsCanvas.TextOut(Rect.Left + 10, Rect.Top + 1, IntToStr(Result.LineNo));
    p := 60;
    st := lbResults.Items[Index];
    c := MyTrim(st);

    // Paint the match line
    for i := 1 to Length(st) do
    begin
      if (i >= Result.SPos - c) and (i <= Result.EPos - c) then
      begin
        ResultsCanvas.Font.Color := sf;
        ResultsCanvas.Brush.Color := sb;
      end
      else
      begin
        ResultsCanvas.Font.Color := nf;
        ResultsCanvas.Brush.Color := nb;
      end;

      // Support printing of MBCS results ("Che Ming" <cheming6150@sina.com>)
      case ByteType(st, i) of
        mbSingleByte:
        begin
          ResultsCanvas.TextOut(Rect.Left + p, Rect.Top + 1, Copy(st, i, 1));
          p := p + ResultsCanvas.TextWidth(Copy(st, i, 1));
        end;
        mbLeadByte:
        begin
          ResultsCanvas.TextOut(Rect.Left + p, Rect.Top + 1, Copy(st, i, 2));
          p := p + ResultsCanvas.TextWidth(Copy(st, i, 2));
        end;
        mbTrailByte: Continue;
      end;
    end;
  end;
end;

procedure TfmGrepResults.WMExitSizeMove(var Message: TMessage);
begin
  lbResults.Repaint;
end;

procedure TfmGrepResults.sbPrintClick(Sender: TObject);
{$IFNDEF ACEREPORTER}
var
  RichEdit: TRichEdit;
  Results: TSearchResults;
  Line: string;
  i, j, c: Integer;
  LinePos: Integer;

begin
  if lbResults.Items.Count = 0 then
    Exit;
  RichEdit := TRichEdit.Create(Self);
  try
    RichEdit.Visible := False;
    RichEdit.Parent := Self;
    RichEdit.Font.Name := 'Arial';
    RichEdit.Font.Size := 10;
    RichEdit.Clear;

    for i := 0 to lbResults.Items.Count - 1 do
      if lbResults.Items.Objects[i] is TSearchResults then
      begin
        RichEdit.Lines.Add('');  // space between fileresults

        Results := TSearchResults(lbResults.Items.Objects[i]);

        RichEdit.SelAttributes.Style := [fsBold];
        RichEdit.Lines.Add(Results.FileName);
        RichEdit.SelAttributes.Style := [];

        for j := 0 to Results.Count - 1 do
        begin
          LinePos := RichEdit.GetTextLen;
          Line := Results.Items[j].Line;
          c := MyTrim(Line);
          with RichEdit do
          begin
            Lines.Add(Format('  %5d'#9, [Results.Items[j].LineNo]) + Line);
            // Now make the found Text bold
            SelStart := LinePos + 7 - c + Results.Items[j].SPos;
            SelLength := Results.Items[j].EPos - Results.Items[j].SPos + 1;
            SelAttributes.Style := [fsBold];
            SelLength := 0;
            SelAttributes.Style := [];
          end;
        end;
      end;
    RichEdit.Print('GExperts - Grep Search Results');
  finally
    RichEdit.Free;
  end;
end;
{$ELSE}
  procedure PrintResult(Result: TSearchResult; AceCanvas: TAceCanvas; Rect: TRect);
  var
    i, p, c: Integer;
    sf, sb, nf, nb: TColor;
    st: string;
  begin
    sf := clWhite;
    sb := clBlack;

    nf := clBlack;
    nb := clWhite;
    p := 0;
    Acecanvas.Font.Style := [];
    st := Result.Line;
    c := MyTrim(st);
    for i := 1 to Length(st) do
    begin
      if (i >= Result.SPos - c) and (i <= Result.EPos - c) then
      begin
        AceCanvas.Font.Color := sf;
        AceCanvas.Brush.Color := sb;
      end
      else
      begin
        AceCanvas.Font.Color := nf;
        AceCanvas.Brush.Color := nb;
      end;
      AceCanvas.TextOut(Rect.Left + p, Rect.Top + 1, Copy(st, i, 1));
      p := p + GetTextWidth(AceCanvas, Copy(st, i, 1));
    end;
    AceCanvas.Font.Color := clBlack;
    AceCanvas.Brush.Color := clWhite;
  end;

var
  Results: TSearchResults;
  Row: Integer;
  RowHeight: Integer;
  AOut: TAceOutPut;
  i, j: Integer;

resourcestring
  SAceResults = 'Search Results';
begin
  if lbResults.Items.Count = 0 then
    Exit;
  Row := 40;
  Screen.Cursor := crHourglass;
  AOut := TAceOutPut.Create;
  try
    AOut.Destination := adAceFile;
    AOut.Description := SAceResults;
    AOut.BeginDoc;
    try
      for i := 0 to lbResults.Items.Count - 1 do
        if lbResults.Items.Objects[i] is TSearchResults then
        begin
          Results := TSearchResults(lbResults.Items.Objects[i]);
          with Aout.AceCanvas do
          begin
            Font.Name := 'Arial'; // do not localize
            Font.Size := 10;
            Font.Style := [fsBold];
            Font.Color := clBlack;
            TextOut(40, Row, Results.FileName);
            Row := Row + GetTextHeight(AOut.AceCanvas, Results.FileName) + 4;
          end;
          RowHeight := GetTextHeight(AOut.AceCanvas, 'W'); // "W" is any character
          for j := 0 to Results.Count - 1 do
            with AOut.AceCanvas do
            begin
              Font.Style := [];
              TextOut(60, Row, IntToStr(Results.Items[j].LineNo));
              PrintResult(Results.Items[j],
                          AOut.AceCanvas,
                          Rect(100, Row, GetPageWidth, Row + RowHeight + 1));
              Row := Row + RowHeight + 2;
              if Row + ((RowHeight + 1) * 3) > AOut.AceFile.PageHeight then
              begin
                AOut.NewPage;
                Row := 20;
              end;
            end;
          Row := Row + RowHeight * 2;
          if Row + ((RowHeight + 1) * 3) > AOut.AceFile.PageHeight then
          begin
            AOut.NewPage;
            Row := 20;
          end;
        end;
    finally
      AOut.EndDoc;
      Screen.Cursor := crDefault;
    end;
    PreviewReport(AOut.AceFile);
    // Set output.AceFile to nil so AcePreview file is not freed with output.AceFile
    AOut.AceFile := nil;
  finally
    AOut.Free;
  end;
end;

procedure TfmGrepResults.PreviewReport(AceFile: TAceFile);
begin
  with TfmPreview.Create(nil) do
  try
    AcePreview.LoadFromAceFile(AceFile);
    ShowModal;
  finally
    Free;
  end;
end;
{$ENDIF ACEREPORTER}

procedure TfmGrepResults.mnuRefreshClick(Sender: TObject);
begin
  Execute(True);
end;

procedure TfmGrepResults.mnuOnTopClick(Sender: TObject);
begin
  inherited;
  OnTop := not OnTop
end;

procedure TfmGrepResults.SetOnTop(Value: Boolean);
begin
  mnuOnTop.Checked := Value;
  sbOnTopUp.Visible := not Value;
  sbOnTopDown.Visible := Value;

  if Value then
    SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE)
  else
    SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE + SWP_NOSIZE);
end;

function TfmGrepResults.GetOnTop: Boolean;
begin
  Result := mnuOnTop.Checked;
end;

procedure TfmGrepResults.sbOnTopClick(Sender: TObject);
begin
  OnTop := not OnTop;
end;

procedure TfmGrepResults.sbRefreshClick(Sender: TObject);
begin
  Execute(True);
end;

{ TGrepExpert }

constructor TGrepExpert.Create;
begin
  inherited Create;
  FSearchList := TStringList.Create;
  FMaskList := TStringList.Create;
  FDirList := TStringList.Create;
  FGrepSave := True;
  FGrepANSICompatible := False;
  ShortCut := Menus.ShortCut(Word('R'), [ssCtrl, ssAlt]);
  fmGrepResults := TfmGrepResults.Create(nil);
  if not StandAlone then
    IdeDockManager.RegisterDockableForm(TfmGrepResults, fmGrepResults, 'fmGrepResults');
  fmGrepResults.GrepExpert := Self;
end;

destructor TGrepExpert.Destroy;
begin
  IdeDockManager.UnRegisterDockableForm(fmGrepResults, 'fmGrepResults');

  fmGrepResults.Free;
  fmGrepResults := nil;

  SaveSettings;

  FSearchList.Free;
  FSearchList := nil;

  FMaskList.Free;
  FMaskList := nil;

  FDirList.Free;
  FDirList := nil;

  inherited Destroy;
end;

function TGrepExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Grep &Results';
begin
  Result := SMenuCaption;
end;

function TGrepExpert.GetMenuName: string;
begin
  Result := 'GX_GrepResults';
end;

function TGrepExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TGrepExpert.GetName: string;
begin
  Result := 'Grep_Results';
end;

function TGrepExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Grep Results';
begin
  Result := SDisplayName;
end;

procedure TGrepExpert.Click(Sender: TObject);
begin
  IdeDockManager.ShowForm(fmGrepResults)
end;

procedure TGrepExpert.ShowModal;
begin
  fmGrepResults.ShowModal;
end;

procedure TGrepExpert.Configure;
var
  Dlg: TfmGrepOptions;
begin
  Dlg := TfmGrepOptions.Create(nil);
  try
    with Dlg do
    begin
      chkGrepSave.Checked := GrepSave;
      chkGrepMiddle.Checked := GrepMiddle;
      chkGrepANSI.Checked := GrepANSICompatible;
      ShowModal;
      if ModalResult = mrOK then
      begin
        GrepSave := chkGrepSave.Checked;
        GrepMiddle := chkGrepMiddle.Checked;
        GrepANSICompatible := chkGrepANSI.Checked;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TGrepExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('Grep', 'NoCase', FGrepNoCase);
    RegIni.WriteBool('Grep', 'NoComments', FGrepNoComments);
    RegIni.WriteBool('Grep', 'DFM', FGrepDFM);
    RegIni.WriteInteger('Grep', 'Search', FGrepSearch);
    RegIni.WriteBool('Grep', 'SubDirectories', FGrepSub);
    RegIni.WriteBool('Grep', 'Save', FGrepSave);
    RegIni.WriteBool('Grep', 'ANSICompatible', FGrepANSICompatible);
    RegIni.WriteBool('Grep', 'Whole Word', FGrepWholeWord);
    RegIni.WriteBool('Grep', 'Middle', FGrepMiddle);
    RegIni.WriteBool('Grep', 'RegEx', FGrepRegEx);

    WriteStringList(RegIni, DirList, 'GrepDirList', 'GrepDir');
    WriteStringList(RegIni, SearchList, 'GrepSearchList', 'GrepSearch');
    WriteStringList(RegIni, MaskList, 'GrepMaskList', 'GrepMask');
  finally
    RegIni.Free;
  end;
end;

procedure TGrepExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;
  // do not localize any of the following lines
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FGrepNoCase := RegIni.ReadBool('Grep', 'NoCase', False);
    // TODO: Restore once NoComments works
    FGrepNoComments := False; //RegIni.ReadBool('Grep', 'NoComments', False);
    FGrepDFM := RegIni.ReadBool('Grep', 'DFM', False);
    FGrepSearch := RegIni.ReadInteger('Grep', 'Search', 0);
    FGrepSub := RegIni.ReadBool('Grep', 'SubDirectories', False);
    FGrepSave := RegIni.ReadBool('Grep', 'Save', True);
    FGrepANSICompatible := RegIni.ReadBool('Grep', 'ANSICompatible', False);
    FGrepWholeWord := RegIni.ReadBool('Grep', 'Whole Word', False);
    FGrepMiddle := RegIni.ReadBool('Grep', 'Middle', FGrepMiddle);
    FGrepWholeWord := RegIni.ReadBool('Grep', 'RegEx', False);

    ReadStringList(RegIni, DirList, 'GrepDirList', 'GrepDir');
    ReadStringList(RegIni, SearchList, 'GrepSearchList', 'GrepSearch');
    ReadStringList(RegIni, MaskList, 'GrepMaskList', 'GrepMask');
    if MaskList.Count = 0 then
    begin
      {$IFDEF GX_BCB}
      MaskList.Add('*.pas;*.dpr;*.cpp;*.hpp;*.h'); // for C++Builder
      {$ENDIF GX_BCB}
      MaskList.Add('*.pas;*.dpr');
    end;
  finally
    RegIni.Free;
  end;
end;

procedure TGrepExpert.SetSearchList(New: TStrings);
begin
  FSearchList.Assign(New);
end;

procedure TGrepExpert.SetMaskList(New: TStrings);
begin
  FMaskList.Assign(New);
end;

procedure TGrepExpert.SetDirList(New: TStrings);
begin
  FDirList.Assign(New);
end;

function TGrepExpert.IconFileName: string;
begin
  Result := 'GrepResults';
end;

procedure TGrepExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    begin
      if fmGrepResults = nil then
        fmGrepResults := TfmGrepResults.Create(nil);
      fmGrepResults.GrepExpert := Self;
    end
    else
    begin
      fmGrepResults.Free;
      fmGrepResults := nil;
    end;
  end;
end;

procedure ShowGrep;
begin
  StandAlone := True;
  {$IFOPT D+}SendDebug('Showing grep expert'); {$ENDIF}
  GrepStandAlone := TGrepExpert.Create;
  try
    {$IFOPT D+}SendDebug('Created grep window'); {$ENDIF}
    GrepStandAlone.LoadSettings;
    GrepStandAlone.ShowModal;
    GrepStandAlone.SaveSettings;
  finally
    GrepStandAlone.Free;
  end;
end;

initialization
  RegisterGX_Expert(TGrepExpert);

{$IFDEF GX_BCB}
  // We quite deliberately use name-mangled functions
  // to add a layer of protection.
  BCB_SwapSourceFormView := GetProcAddress(HINSTANCE, '@BCB_SwapSourceFormViewImplementation$qqrv');
  Assert(@BCB_SwapSourceFormView <> nil);

  BCB_HasForm := GetProcAddress(HINSTANCE, '@BCB_HasFormImplementation$qqrv');
  Assert(@BCB_HasForm <> nil);
{$ENDIF GX_BCB}

end.

