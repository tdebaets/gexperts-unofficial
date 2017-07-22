unit GX_ProjDepend;

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  ComCtrls, ExtCtrls, GX_EditReader, ToolIntf, ExptIntf, mPasLex,
  Buttons, Registry, Menus, GX_Experts;

type
  TUnitInfo = class(TObject)
  public
    UnitName: string;
    FileName: string;
  end;

  TfmProjDepend = class;

  TProjectNotifier = class(TIAddInNotifier)
  private
    fmDepend: TfmProjDepend;
  public
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
  end;

  TfmProjDepend = class(TForm)
    pnlToolbar: TPanel;
    StatusBar: TStatusBar;
    tvUnits: TTreeView;
    Splitter1: TSplitter;
    sbRefresh: TSpeedButton;
    sbAbort: TSpeedButton;
    Panel2: TPanel;
    pcData: TPageControl;
    tshUnitUses: TTabSheet;
    lvUnitUses: TListView;
    tshUsedby: TTabSheet;
    lvUsedBy: TListView;
    tshIndirect: TTabSheet;
    lvIndirect: TListView;
    ilFiles: TImageList;
    pmTreeview: TPopupMenu;
    pmList: TPopupMenu;
    mitOpenUnit: TMenuItem;
    OpenUnit2: TMenuItem;
    N1: TMenuItem;
    Properties1: TMenuItem;
    sbHelp: TSpeedButton;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Refresh1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    Help2: TMenuItem;
    N3: TMenuItem;
    About1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbRefreshClick(Sender: TObject);
    procedure sbAbortClick(Sender: TObject);
    procedure tvUnitsExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure Panel2Resize(Sender: TObject);
    procedure tvUnitsChange(Sender: TObject; Node: TTreeNode);
    procedure pcDataChange(Sender: TObject);
    procedure mitOpenUnitClick(Sender: TObject);
    procedure OpenUnit2Click(Sender: TObject);
    procedure pmListPopup(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure tvUnitsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure pmTreeviewPopup(Sender: TObject);
    procedure lvColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvIndirectCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure tvUnitsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    ProjectNotifier: TProjectNotifier;
    procedure ShowUnitsUsed;
    procedure ShowUsedBy;
    procedure IndirectDepend;
    function GetFileName(const uName: string): string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure OpenUnit(const UnitName: string);
  private
    RefreshList: Boolean;
    UnitList: TStringList;
    FileList: TStringList;
    Abort: Boolean;
    UnitUsesSortColumn: Integer;
    UsedBySortColumn: Integer;
    IndirectSortColumn: Integer;
    function FindUnit(const uName: string): Integer;
    procedure BuildUses;
    procedure Clear;
  end;

  TDependExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  private
    FScanEntireUnit: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;

var
  fmProjDepend: TfmProjDepend = nil;
  DependExpert: TDependExpert = nil;
  RootNode: TTreeNode = nil;

implementation

{$R *.DFM}
{$R ProjDepend.res}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_ProjDependProp, GX_GExperts, GX_ConfigurationInfo, GX_GenFunc,
  TypInfo, CommCtrl;

procedure TProjectNotifier.EventNotification(
  NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  // Nothing
end;

procedure TProjectNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = fnProjectOpened then
  begin
    fmDepend.Clear;
    if fmDepend.Visible then
      fmDepend.BuildUses
    else
      fmDepend.RefreshList := True;
  end;
end;

procedure TfmProjDepend.FormCreate(Sender: TObject);
resourcestring
  SCouldNotLoadBitmaps = 'Could not load dependency bitmaps.';
begin
  RefreshList := False;
  with tvUnits do
    SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) or TVS_NOTOOLTIPS);
  UnitList := TStringList.Create;
  FileList := TStringList.Create;
  if not ilFiles.ResourceLoad(rtBitmap, 'VCLOPEN', clFuchsia) then  // Do not localize
    MessageDlg(SCouldNotLoadBitmaps, mtWarning, [mbOK], 0);
  UnitUsesSortColumn := -1;
  IndirectSortColumn := -1;
  UsedBySortColumn := -1;

  CenterForm(Self);
  LoadSettings;
  ProjectNotifier := TProjectNotifier.Create;
  ProjectNotifier.fmDepend := Self;
  ToolServices.AddNotifier(ProjectNotifier);
end;

procedure TfmProjDepend.FormDestroy(Sender: TObject);
begin
  fmProjDepend := nil;
  Clear;
  UnitList.Free;
  UnitList := nil;
  FileList.Free;
  FileList := nil;
  SaveSettings;
  ToolServices.RemoveNotifier(ProjectNotifier);
  ProjectNotifier.Free;
  ProjectNotifier := nil;
end;

procedure TfmProjDepend.Clear;
var
  i: Integer;
begin
  if UnitList <> nil then
  begin
  for i := 0 to UnitList.Count-1 do
    UnitList.Objects[i].Free;
  UnitList.Clear;
  end;

  if FileList <> nil then
  begin
    for i := 0 to FileList.Count-1 do
      FileList.Objects[i].Free;
    FileList.Clear;
  end;
end;

function LoadFileDepend(Param: Pointer; const FileName, Unitname,
  FormName: string): Boolean; stdcall;
var
  Form: TfmProjDepend;
  EditRead: TEditReader;
  MS: TMemoryStream;
  Parser: TmwPasLex;
  nUses: Integer;
  Index: Integer;
  Node: TTreeNode;
  UList: TStringList;
  uInfo: TUnitInfo;

begin
  Result := True;
  if FileName = ToolServices.GetProjectName then Exit;
  nUses := 0;
  UList := nil;
  //Parser := nil;
  Form := TfmProjDepend(Param);
  with Form.StatusBar do
  begin
    SimpleText := FileName;
    Repaint;
  end;

  if Form.Abort then
  begin
    Result := False;
    Form.sbAbort.Enabled := False;
    Form.sbRefresh.Enabled := True;
    Exit;
  end;

  if not IsDprOrPas(FileName) then
    Exit;
  if FormName = '' then
    Index := 2
  else
    Index := 3;
  //{$IFOPT D+}SendDebug('Found Project Unit: '+UnitName);{$ENDIF}
  UInfo := TUnitInfo.Create;
  UInfo.UnitName := UnitName;
  UInfo.FileName := FileName;
  Form.FileList.AddObject(UnitName, uInfo);
  MS := TMemoryStream.Create;
  try
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    try
      EditRead := TEditReader.Create(FileName);
    except
      on E: Exception do
      begin
        // Warn, but skip project files that don't exist (dcu only, etc.?)
        MessageDlg(E.Message, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;
    try
      EditRead.SaveToStream(MS);
    finally
      EditRead.Free;
    end;
    Parser := TmwPasLex.Create;
    try
      //{$IFOPT D+}SendDebug('Parsing '+Unitname);{$ENDIF}
      Parser.Origin := MS.Memory;
      //{$IFOPT D+}SendDebug('First: '+Parser.Token+'('+GetEnumName(TypeInfo(TTokenKind), Integer(Parser.TokenID))+')');{$ENDIF}
      while not (Parser.TokenID in [tkUnit, tkNull, tkLibrary, tkProgram]) do
      begin
        Parser.NextNoJunk;
        //{$IFOPT D+}SendDebug('Looking for Unit, found: '+Parser.Token);{$ENDIF}
      end;
      //{$IFOPT D+}SendDebug('L/P/U Found: '+Parser.Token);{$ENDIF}
      if Parser.TokenID in [tkUnit, tkLibrary, tkProgram] then
      begin
        Parser.NextNoJunk;
        //{$IFOPT D+}SendDebug('ID is: '+Parser.Token);{$ENDIF}
        if Parser.TokenID = tkIdentifier then
        begin
          Node := Form.tvUnits.Items.AddChild(RootNode, Parser.Token);
          //{$IFOPT D+}SendDebug('Added '+Parser.Token+' to '+RootNode.Text);{$ENDIF}
          Node.HasChildren := True;
          Node.ImageIndex := Index;
          Node.SelectedIndex := Index;
          UList := TStringList.Create;
          Form.UnitList.AddObject(Parser.Token, UList);
        end;
      end;
      if UList = nil then
        Exit;
      while Parser.TokenID <> tkNull do
      begin
        if Parser.TokenID = tkUses then
        begin
          Inc(nUses);
          Parser.NextNoJunk;
          while (Parser.TokenID <> tkSemiColon) and (Parser.TokenID <> tkNull) do
          begin
            if Parser.TokenID = tkIdentifier then
              UList.Add(Parser.Token);
            Parser.NextNoJunk;
            if Parser.TokenID = tkIn then
              while not (Parser.TokenID in [tkSemiColon, tkComma, tkNull]) do
                Parser.NextNoJunk;
          end;
        end;
        if ((DependExpert <> nil) and (not DependExpert.FScanEntireUnit)) and (nUses >= 2) then
          Break;
        Parser.NextNoJunk;
      end;
    finally
      Parser.Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure TfmProjDepend.BuildUses;
resourcestring
  SParsingUnits = 'Parsing units...';
begin
  tvUnits.Items.BeginUpdate;
  Screen.Cursor := crHourglass;
  sbRefresh.Enabled := False;
  sbAbort.Enabled := True;
  try
    Clear;
    Abort := False;
    // Clear current scroll box
    lvIndirect.Items.Clear;
    tvUnits.Items.Clear;
    // Start parsing
    StatusBar.SimpleText := SParsingUnits;
    StatusBar.Repaint;
    RootNode := tvUnits.Items.Add(nil, ExtractFileName(ToolServices.GetProjectName));
    RootNode.ImageIndex := 4;
    RootNode.SelectedIndex := 4;
    ToolServices.EnumProjectUnits(LoadFileDepend, Pointer(Self));
    RootNode.Expand(False);
    tvUnits.AlphaSort;
  finally
    sbRefresh.Enabled := True;
    sbAbort.Enabled := False;
    Screen.Cursor := crDefault;
    tvUnits.Items.EndUpdate;
  end;
end;

procedure TfmProjDepend.sbRefreshClick(Sender: TObject);
begin
  BuildUses;
end;

procedure TfmProjDepend.sbAbortClick(Sender: TObject);
begin
  Abort := True;
end;

function TfmProjDepend.FindUnit(const uName: string): Integer;
begin
  Result := UnitList.Count-1;
  while Result >= 0 do
  begin
    if CompareText(uName, UnitList.Strings[Result]) = 0 then
      Break;

    Dec(Result);
  end;
end;

function TfmProjDepend.GetFileName(const uName: string): string;
var
  UInfo: TUnitInfo;
  i: Integer;
begin
  Result := '';
  i := FileList.IndexOf(UpperCase(UName));
  if i >= 0 then
  begin
    UInfo := TUnitInfo(FileList.Objects[i]);
    Result := UInfo.FileName;
  end;
end;

procedure TfmProjDepend.tvUnitsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  i, j, k: Integer;
  List: TStringList;
  CNode: TTreeNode;
begin
  // First clear out existing objects
  AllowExpansion := True;
  if Node = RootNode then
    Exit;
  while Node.Count > 0 do
    Node.Item[0].Free;
  i := FindUnit(Node.Text);
  if i >= 0 then
  begin
    List := TStringList(UnitList.Objects[i]);
    for j := 0 to List.Count - 1 do
    begin
      CNode := tvUnits.Items.AddChild(Node, List.Strings[j]);
      CNode.SelectedIndex := 2;
      CNode.ImageIndex := 2;
      k := FindUnit(List.Strings[j]);
      CNode.HasChildren := ((k > -1) and (TStringList(UnitList.Objects[k]).Count > 0));
    end;
    if List.Count = 0 then
      Node.HasChildren := False;
  end
  else
    Node.HasChildren := False;
end;

procedure TfmProjDepend.Panel2Resize(Sender: TObject);
begin
  with lvUnitUses do
    Columns[1].Width := ClientWidth - Columns[0].Width;
  with lvUsedBy do
    Columns[1].Width := ClientWidth - Columns[0].Width;
  with lvIndirect do
    Columns[1].Width := ClientWidth - Columns[0].Width;
end;

procedure TfmProjDepend.tvUnitsChange(Sender: TObject; Node: TTreeNode);
begin
  pcDataChange(pcData);
end;

procedure TfmProjDepend.pcDataChange(Sender: TObject);
begin
  if pcData.ActivePage = tshUnitUses then
    ShowUnitsUsed
  else
  if pcData.ActivePage = tshUsedBy then
    ShowUsedby
  else
  if pcData.ActivePage = tshIndirect then
    IndirectDepend;
  // VCL 4.0 bug
  {$IFDEF GX_VER120_up}
  pcData.ActivePage.Realign;
  Panel2Resize(Self);
  {$ENDIF GX_VER120_up}
end;

procedure TfmProjDepend.ShowUnitsUsed;
var
  i, j: Integer;
  List: TStringList;
  ListItem: TListItem;
begin
  lvUnitUses.Items.BeginUpdate;
  try
    lvUnitUses.Items.Clear;
    if tvUnits.Selected = nil then
      Exit;
    i := FindUnit(tvUnits.Selected.Text);
    if i >= 0 then
    begin
      List := TStringList(UnitList.Objects[i]);
      for j := 0 to List.Count - 1 do
      begin
        ListItem := lvUnitUses.Items.Add;
        ListItem.Caption := List.Strings[j];
        ListItem.SubItems.Add(GetFileName(List.Strings[j]));
      end;
    end;
  finally
    lvUnitUses.Items.EndUpdate
  end;
end;

procedure TfmProjDepend.ShowUsedBy;
var
  i, j: Integer;
  List: TStringList;
  ListItem: TListItem;
begin
  lvUsedBy.Items.BeginUpdate;
  try
    lvUsedBy.Items.Clear;
    if tvUnits.Selected = nil then
      Exit;
    for i := 0 to UnitList.Count - 1 do
    begin
      List := TStringList(UnitList.Objects[i]);
      for j := 0 to List.Count - 1 do
        if CompareText(tvUnits.Selected.Text, List.Strings[j]) = 0 then
        begin
          ListItem := lvUsedBy.Items.Add;
          ListItem.Caption := UnitList.Strings[i];
          ListItem.SubItems.Add(GetFileName(UnitList.Strings[i]));
        end;
    end;
  finally
    lvusedBy.Items.EndUpdate;
  end;
end;

procedure TfmProjDepend.IndirectDepend;
var
  List: TStringList;

  // The performance of this routine is hardly optimal, but at least it
  // works correctly (as opposed to what it replaced).
  function UsedUnitIsInCommaSeparatedList(const UsedUnit, CommaList: string): Boolean;
  var
    List: TStrings;
  begin
    List := TStringList.Create;
    try
      List.CommaText := CommaList;
      Result := (List.IndexOf(UsedUnit) >= 0);
    finally
      List.Free;
    end;
  end;

  procedure AddListViewUsesEntries(const UsingUnit, UsedUnit: string);
  var
    i: Integer;
    ExistingUnitList: string;
    ListItem: TListItem;
  begin
    i := lvIndirect.Items.Count - 1;
    while i >= 0 do
    begin
      if AnsiCompareText(UsingUnit, lvIndirect.Items[i].Caption) = 0 then
        Break;
      Dec(i);
    end;

    if i < 0 then
    begin
      ListItem := lvIndirect.Items.Add;
      ListItem.Caption := UsingUnit;
      ListItem.SubItems.Add(UsedUnit);
    end
    else
    begin
      ExistingUnitList := lvIndirect.Items[i].SubItems[0];
      if not UsedUnitIsInCommaSeparatedList(UsedUnit, ExistingUnitList) then
      begin
        ExistingUnitList := ExistingUnitList + ', ' + UsedUnit;
        lvIndirect.Items[i].SubItems[0] := ExistingUnitList;
      end;
    end;
  end;

  procedure AddItems(UnitName: string);
  var
    i, j: Integer;
    MList: TStringList;
  begin
    i := FindUnit(UnitName);
    if i >= 0 then
    begin
      MList := TStringList(UnitList.Objects[i]);
      for j := 0 to MList.Count-1 do
      begin
        AddListViewUsesEntries(MList.Strings[j], UnitName);
        if List.IndexOf(MList.Strings[j]) < 0 then
        begin
          List.Add(MList.Strings[j]);
          AddItems(MList.Strings[j]);
        end;
      end;
    end;
  end;

begin
  if tvUnits.Selected = nil then
    Exit;
  Screen.Cursor := crHourglass;
  List := TStringList.Create;
  try
    AddItems(tvUnits.Selected.Text);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmProjDepend.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // Do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('Depend', 'Left', Left);
    RegIni.WriteInteger('Depend', 'Top', Top);
    RegIni.WriteInteger('Depend', 'Width', Width);
    RegIni.WriteInteger('Depend', 'Height', Height);
    RegIni.WriteInteger('Depend', 'Splitter', tvUnits.Width);
  finally
    RegIni.Free;
  end;
end;

procedure TfmProjDepend.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // Do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    Left := RegIni.ReadInteger('Depend', 'Left', Left);
    Top := RegIni.ReadInteger('Depend', 'Top', Top);
    Width := RegIni.ReadInteger('Depend', 'Width', Width);
    Height := RegIni.ReadInteger('Depend', 'Height', Height);
    tvUnits.Width := RegIni.ReadInteger('Depend', 'Splitter', tvUnits.Width);
  finally
    RegIni.Free;
  end;
end;

procedure TfmProjDepend.mitOpenUnitClick(Sender: TObject);
begin
  if tvUnits.Selected <> nil then
    OpenUnit(tvUnits.Selected.Text);
end;

procedure TfmProjDepend.OpenUnit(const UnitName: string);
var
  i: Integer;
  CurrentFileName: string;
resourcestring
  SFileDoesNotExist = '%s does not exist';
  SOpenError = 'This unit is not in the current project.'#13#10 +
               'A later version of GExperts might be able to open units on the library path.';
begin
  i := FileList.IndexOf(UnitName);
  if i >= 0 then
  begin
    CurrentFileName := TUnitInfo(FileList.Objects[i]).FileName;
    if FileExists(CurrentFileName) then
      ToolServices.OpenFile(CurrentFileName)
    else
      MessageDlg(Format(SFileDoesNotExist, [CurrentFileName]), mtError, [mbOK], 0);
  end
  else
    MessageDlg(SOpenError, mtInformation, [mbOK], 0);
end;

procedure TfmProjDepend.OpenUnit2Click(Sender: TObject);

  procedure CallOpenUnit(lv: TListView);
  begin
    if lv.Selected <> nil  then
      OpenUnit(lv.Selected.Caption);
  end;

begin
  if pcData.ActivePage = tshUnitUses then
    CallOpenUnit(lvUnitUses)
  else
  if pcData.ActivePage = tshUsedBy then
    CallOpenUnit(lvUsedBy)
  else
  if pcData.ActivePage = tshIndirect then
    CallOpenUnit(lvIndirect);
end;

procedure TfmProjDepend.pmListPopup(Sender: TObject);
var
  IsVisible: Boolean;
begin
  IsVisible := (pcData.ActivePage = tshIndirect);

  Properties1.Visible := IsVisible;
  N1.Visible := IsVisible;
end;

procedure TfmProjDepend.Properties1Click(Sender: TObject);
begin
  if lvIndirect.Selected = nil then
    Exit;
  with TfmProjDependProp.Create(nil) do
  try
    laFileName.Caption := lvIndirect.Selected.Caption;
    lbxSource.Items.CommaText := lvIndirect.Selected.SubItems[0];
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfmProjDepend.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 6);
end;

procedure TfmProjDepend.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfmProjDepend.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmProjDepend.pmTreeviewPopup(Sender: TObject);
begin
  mitOpenUnit.Enabled := (tvUnits.Selected <> RootNode);
end;

procedure TfmProjDepend.FormActivate(Sender: TObject);
begin
  if RefreshList then
  begin
    RefreshList := False;
    BuildUses;
  end;
end;

procedure TfmProjDepend.tvUnitsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TfmProjDepend.lvColumnClick(Sender: TObject; Column: TListColumn);
begin
  if Sender = lvUnitUses then
    UnitUsesSortColumn := Column.Index;
  if Sender = lvUsedBy then
    UsedBySortColumn   := Column.Index;
  if Sender = lvIndirect then
    IndirectSortColumn := Column.Index;

  (Sender as TCustomListView).AlphaSort;
end;

procedure TfmProjDepend.lvIndirectCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
  SortColomn: Integer;
begin
  SortColomn := 0;
  if Sender = lvUnitUses then
    SortColomn := UnitUsesSortColumn;
  if Sender = lvUsedBy then
    SortColomn := UsedBySortColumn;
  if Sender = lvIndirect then
    SortColomn := IndirectSortColumn;
  if not(SortColomn in [0,1]) then
    SortColomn := 0;
  if SortColomn = 0 then
    Compare := CompareText(Item1.Caption,Item2.Caption)
  else begin
   ix := SortColomn - 1;
   Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);
  end;
end;

procedure TfmProjDepend.tvUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Don't allow Grey-'*' for expand all, since we might have circular dependencies
  if Key = 106 then
    Key := 0;
end;

{ TDependExpert }

constructor TDependExpert.Create;
begin
  inherited Create;
  DependExpert := Self;
  HasConfigOptions := True;
  HasMenuItem := True;
  {$IFDEF GX_BCB}
  // Default to inactive for BCB until we correctly support it
  DefaultActive := False;
  {$ENDIF GX_BCB}
end;

destructor TDependExpert.Destroy;
begin
  DependExpert := nil;

  fmProjDepend.Free;
  fmProjDepend := nil;

  inherited Destroy;
end;

procedure TDependExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      fmProjDepend.Free;
      fmProjDepend := nil;
    end;
  end;
end;

function TDependExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Project &Dependencies';
begin
  Result := SMenuCaption;
end;

function TDependExpert.GetMenuName: string;
begin
  Result := 'GX_Depend';
end;

function TDependExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TDependExpert.GetName: string;
begin
  Result := 'Dependency_Expert';
end;

function TDependExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Project Dependencies';
begin
  Result := SDisplayName;
end;

procedure TDependExpert.Configure;
resourcestring
  SConfigureExplanation = 'Do you want Project Dependencies to scan the entire unit for uses clauses?'#13#13+
                          'This is slower, but will also find additional, unusual uses clauses that have been '+
                          'conditionally defined.';
begin
  FScanEntireUnit := (MessageDlg(SConfigureExplanation, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TDependExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteBool('Depend', 'ScanEntireUnit', FScanEntireUnit);
  finally
    RegIni.Free;
  end;
end;

procedure TDependExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FScanEntireUnit := RegIni.ReadBool('Depend', 'ScanEntireUnit', False);
  finally
    RegIni.Free;
  end;
end;

procedure TDependExpert.Click(Sender: TObject);
begin
  if fmProjDepend = nil then
  begin
    fmProjDepend := TfmProjDepend.Create(nil);
    fmProjDepend.BuildUses;
  end;
  if fmProjDepend.WindowState = wsMinimized then
    fmProjDepend.WindowState := wsNormal;
  fmProjDepend.Show;
end;

function TDependExpert.IconFileName: string;
begin
  Result := 'Depend';
end;

initialization
  RegisterGX_Expert(TDependExpert);
end.

