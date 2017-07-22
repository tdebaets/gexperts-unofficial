// Original Author: John Hansen <John_Hansen@tcsmgmt.com>
unit GX_ProjOptionSets;

{$I GX_CondDefine.inc}

{$IFDEF GX_UseNativeToolsApi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, GX_Experts, ActiveX, GX_IdeDock, E_StgStr,
  TypInfo, Menus, ToolsApi, ComCtrls, GX_MessageBox;

type
  TfmProjOptionSets = class;

  TProjOptionSetsExpert = class(TGX_EnhExpert)
  private
    function GetStorageFile: string;
  protected
    FStoragePath: string;
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
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Configure; override;
    property StorageFile: string read GetStorageFile;
  end;

  TfmProjOptionSets = class(TfmIdeDockForm)
    pnlLeft: TPanel;
    grpSets: TGroupBox;
    pnlOSInner: TPanel;
    lstSets: TListBox;
    pmuPrjOptions: TPopupMenu;
    mniPrjClearAll: TMenuItem;
    mniPrjCheckAll: TMenuItem;
    splLeftRight: TSplitter;
    mniPrjSortByName: TMenuItem;
    mniPrjSortByCheckmark: TMenuItem;
    mniPrjAscending: TMenuItem;
    mniPrjDescending: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    mniModifyPrjOptionValues: TMenuItem;
    pnlRight: TPanel;
    pcSettings: TPageControl;
    tsProjectSettings: TTabSheet;
    tsEnvironmentSettings: TTabSheet;
    lblProjectSettings: TLabel;
    pnlFilterComboHost: TPanel;
    cbFilter: TComboBox;
    pnlCheckListHost: TPanel;
    pmuEnvOptions: TPopupMenu;
    mniModifyEnvOptionValues: TMenuItem;
    lblCheckListNot: TLabel;
    btnAdd: TButton;
    btnDelete: TButton;
    btnSave: TButton;
    btnApply: TButton;
    btnHelp: TButton;
    procedure lstSetsClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mniPrjClearAllClick(Sender: TObject);
    procedure mniPrjCheckAllClick(Sender: TObject);
    procedure mniPrjSortByCheckmarkClick(Sender: TObject);
    procedure mniPrjDescendingClick(Sender: TObject);
    procedure pmuPrjOptionsPopup(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbFilterChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure splLeftRightMoved(Sender: TObject);
    procedure mniModifyEnvOptionValuesClick(Sender: TObject);
    procedure splLeftRightCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure pnlOSInnerResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    lstPrjOptions: TCheckListBox;
    lstEnvOptions: TCheckListBox;
    FPrjSetOptions: TStringList;
    FEnvSetOptions: TStringList;
    FSetChanged: Boolean;
    FLastLoadedSet: string;
    FStorage: TStructuredStorage;
    FPrjOptions: IOTAProjectOptions;
    FEnvOptions: IOTAEnvironmentOptions;
    FSplitterRatio: Double;
    function ProjOptsExpert: TProjOptionSetsExpert;
    procedure lstEnvironmentOptClickCheck(Sender: TObject);
    procedure lstProjectOptClickCheck(Sender: TObject);
    procedure ProcessListboxCheckClick(Listbox: TCheckListBox; Options: TStringList);
    procedure HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
    procedure SetupListOptionsControls;
    procedure FillFilterBox;
    procedure UpdateButtonState;
    procedure GenericClearOptionList(AList: TCheckListBox);
    procedure ClearEnvOptionList;
    procedure ClearPrjOptionList;
    procedure SetAllChecks(AList: TCheckListBox; ACheckFlag: Boolean);
    procedure LoadPrjOptionList;
    procedure LoadEnvOptionList;
    procedure LoadOptionSetList;
    procedure LoadSetOptions;
    procedure SaveSetOptions;
    procedure ApplySetOptions;
    procedure DeleteSetFromStorage;
    procedure UpdateSetListInStorage;
    function GetCheckedCount(AList: TCheckListBox): Integer;
    function GetValueAsString(AValue: Variant; AKind: TTypeKind): string;
    function GetPrjOptionValue(const AOption: string): string;
    function GetEnvOptionValue(const AOption: string): string;
    procedure SetPrjOptionValue(const AOption, AValue: string);
    procedure SetEnvOptionValue(const AOption, AValue: string);
    function GetProjectOptions: IOTAProjectOptions;
    function GetEnvironmentOptions: IOTAEnvironmentOptions;
    function GetCurrentSetName: string;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure RefreshPrjCheckmarks;
    procedure RefreshEnvCheckmarks;
    procedure AddNewOptionSet(SetName: string);
    function HaveSelectedSet: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TShowBuggyOTAMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function ShouldShow: Boolean; override;
  end;

var
  fmProjOptionSets: TfmProjOptionSets;
  PrjOptSetsExpert: TProjOptionSetsExpert;

implementation

{$R *.DFM}

uses
  GX_VerDepConst,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  {$IFDEF GX_VER140_up}
  Variants,
  {$ENDIF GX_VER140_up}
  ActnList, Registry, Clipbrd,
  GX_GExperts, GX_ConfigurationInfo,
  GX_GenFunc, GX_GetIdeVersion,
  GX_ProjOptOptions, GX_ProjOptMap;

resourcestring
  SOptValue = ' value';
  SOptSaved = 'saved: ';
  SOptCurrent = 'current: ';
  SValUnknown = 'unknown';

const
  CRLF = #13#10;

type
  TKindObject = class(TObject)
  private
    FOptionKind: TTypeKind;
  public
    property OptionKind: TTypeKind read FOptionKind write FOptionKind;
  end;

TJCHListSortCompare = function(Item1, Item2: Integer): Integer of object;
  TGetHintEvent = procedure(Sender: TObject; const CursorPos: TPoint; var HintStr: string) of object;

  TCheckListBoxWithHints = class(TCheckListBox)
  private
    FOnGetHint: TGetHintEvent;
    FSortAscend: Boolean;
    FSortByString: Boolean;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetSortAscend(const Value: Boolean);
    procedure SetSortByString(const Value: Boolean);
    procedure QuickSort(L, R: Integer; SCompare: TJCHListSortCompare);
  protected
    procedure DoOnGetHint(const CursorPos: TPoint; var HintStr: string); // virtual;
    function  CompareByStringAscending(Item1, Item2: Integer): Integer;
    function  CompareByStringDescending(Item1, Item2: Integer): Integer;
    function  CompareByCheckAscending(Item1, Item2: Integer): Integer;
    function  CompareByCheckDescending(Item1, Item2: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SortList(Compare: TJCHListSortCompare);
    procedure Resort;
    property SortAscending: Boolean read FSortAscend write SetSortAscend;
    property SortByString: Boolean read FSortByString write SetSortByString;
    property OnGetHint: TGetHintEvent read FOnGetHint write FOnGetHint;
  end;

function BoolToStr(PValue: Boolean): string;
const
  BoolChar: array[Boolean] of Char = ('F', 'T');
begin
  Result := BoolChar[PValue];
end;

function StrToBool(const AString: string): Boolean;
begin
  Result := (AString = 'T');
end;

{ TCheckListBoxWithHints }

procedure TCheckListBoxWithHints.QuickSort(L, R: Integer; SCompare: TJCHListSortCompare);
var
  I, J, P: Integer;
  tmpObj: TObject;
  tmpStr: string;
  tmpChecked: Boolean;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(I, P) < 0 do Inc(I);
      while SCompare(J, P) > 0 do Dec(J);
      if I <= J then
      begin
        // exchange I and J
        tmpStr           := Items[I];
        tmpObj           := Items.Objects[I];
        tmpChecked       := Self.Checked[I];

        Items[I]         := Items[J];
        Items.Objects[I] := Items.Objects[J];
        Self.Checked[I]  := Self.Checked[J];

        Items[J]         := tmpStr;
        Items.Objects[J] := tmpObj;
        Self.Checked[J]  := tmpChecked;
        if P = I then
          P := J
        else if P = J then
          P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCheckListBoxWithHints.CMHintShow(var Message: TMessage);
var
  NewHintStr: string;
begin
  NewHintStr := TCMHintShow(Message).HintInfo^.HintStr;
  DoOnGetHint(TCMHintShow(Message).HintInfo^.CursorPos, NewHintStr);
  TCMHintShow(Message).HintInfo^.HintStr := NewHintStr;
  inherited;
end;

constructor TCheckListBoxWithHints.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortAscend := True;
  FSortByString := True;
end;

procedure TCheckListBoxWithHints.DoOnGetHint(const CursorPos: TPoint; var HintStr: string);
begin
  if Assigned(FOnGetHint) then
    FOnGetHint(Self, CursorPos, HintStr);
end;

procedure TCheckListBoxWithHints.SetSortAscend(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortAscend := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SetSortByString(const Value: Boolean);
begin
  if Sorted then
    Exit;
  FSortByString := Value;
  Resort;
end;

procedure TCheckListBoxWithHints.SortList(Compare: TJCHListSortCompare);
begin
  if Items.Count = 0 then Exit;
  Items.BeginUpdate;
  try
    QuickSort(0, Items.Count - 1, Compare);
  finally
    Items.EndUpdate;
  end;
end;

procedure TCheckListBoxWithHints.Resort;
begin
  if SortByString then
  begin
    if SortAscending then
      SortList(CompareByStringAscending)
    else
      SortList(CompareByStringDescending);
  end
  else
  begin
    if SortAscending then
      SortList(CompareByCheckAscending)
    else
      SortList(CompareByCheckDescending);
  end;
end;

function TCheckListBoxWithHints.CompareByCheckAscending(Item1, Item2: Integer): Integer;
begin
  Result := 0;
  if Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := -1
  else if Self.Checked[Item1] and Self.Checked[Item2] then
    Result := CompareText(Items[Item1], Items[Item2])
  else if not Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := CompareText(Items[Item1], Items[Item2])
  else if not Self.Checked[Item1] and Self.Checked[Item2] then
    Result := 1;
end;

function TCheckListBoxWithHints.CompareByCheckDescending(Item1, Item2: Integer): Integer;
begin
  Result := 0;
  if Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := 1
  else if Self.Checked[Item1] and Self.Checked[Item2] then
    Result := CompareText(Items[Item2], Items[Item1])
  else if not Self.Checked[Item1] and not Self.Checked[Item2] then
    Result := CompareText(Items[Item2], Items[Item1])
  else if not Self.Checked[Item1] and Self.Checked[Item2] then
    Result := -1;
end;

function TCheckListBoxWithHints.CompareByStringAscending(Item1, Item2: Integer): Integer;
begin
  Result := CompareText(Items[Item1], Items[Item2]);
end;

function TCheckListBoxWithHints.CompareByStringDescending(Item1, Item2: Integer): Integer;
begin
  Result := CompareText(Items[Item2], Items[Item1]);
end;

{ TProjOptionSetsExpert }

procedure TProjOptionSetsExpert.Click(Sender: TObject);
begin
  // If the form doesn't exist, create it
  if fmProjOptionSets = nil then
    fmProjOptionSets := TfmProjOptionSets.Create(nil);
  // Show the form using IdeDockManager.ShowForm
  IdeDockManager.ShowForm(fmProjOptionSets);
end;

procedure TProjOptionSetsExpert.Configure;
begin
  with TfmProjOptOptions.Create(nil) do
  try
    Path := FStoragePath;
    if ShowModal = mrOK then
      FStoragePath := Path;
  finally
    Free;
  end;
end;

constructor TProjOptionSetsExpert.Create;
begin
  inherited Create;
  FStoragePath := ConfigInfo.ConfigPath;
  HasMenuItem := True;
  HasConfigOptions := True;

  if Assigned(PrjOptSetsExpert) then
  begin
    PrjOptSetsExpert.Free;
    PrjOptSetsExpert := nil;
  end;
  PrjOptSetsExpert := Self;
end;

destructor TProjOptionSetsExpert.Destroy;
begin
  fmProjOptionSets.Free;
  fmProjOptionSets := nil;
  PrjOptSetsExpert := nil;
  inherited Destroy;
end;

function TProjOptionSetsExpert.GetDisplayName: string;
resourcestring
  SProjOptionsDisplayName = 'Project Option Sets';
begin
  Result := SProjOptionsDisplayName;
end;

function TProjOptionSetsExpert.GetMenuCaption: string;
resourcestring
  SProjOptionsMenuCaption = 'Project &Option Sets';
begin
  Result := SProjOptionsMenuCaption;
end;

function TProjOptionSetsExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TProjOptionSetsExpert.GetMenuName: string;
begin
  Result := 'GX_ProjOptionSets';
end;

function TProjOptionSetsExpert.GetName: string;
begin
  Result := 'GXProjOptionSets_Expert';
end;

function TProjOptionSetsExpert.GetStorageFile: string;
begin
  Result := FStoragePath + ProjectOptionsSetsFileName;
end;

function TProjOptionSetsExpert.IconFileName: string;
begin
  Result := 'ProjOpts';
end;

procedure TProjOptionSetsExpert.LoadSettings;
begin
  inherited LoadSettings;
  // Load your expert-specific settings
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    FStoragePath := ReadString('ProjOptionSets', 'StoragePath', FStoragePath);
  finally
    Free;
  end;
  // This procedure is only called once so it is safe to register the form here.
  if Active then
    IdeDockManager.RegisterDockableForm(TfmProjOptionSets,
      fmProjOptionSets, 'fmProjOptionSets');
end;

procedure TProjOptionSetsExpert.SaveSettings;
begin
  inherited SaveSettings;
  // Save expert-specific settings here
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteString('ProjOptionSets', 'StoragePath', FStoragePath);
  finally
    Free;
  end;
end;

procedure TProjOptionSetsExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      fmProjOptionSets.Free;
      fmProjOptionSets := nil;
    end;
  end;
end;

{ TfmProjOptionSets }

constructor TfmProjOptionSets.Create(AOwner: TComponent);
resourcestring
  SModifyOptionValuesMenuCaption = '&Modify Option Values...';
var
  MainForm: TCustomForm;
  ProjectOptions: TComponent;
begin
  inherited Create(AOwner);

  ShowGxMessageBox(TShowBuggyOTAMessage);

  FLastLoadedSet := '';
  SetupListOptionsControls;
  UpdateButtonState;
  FillFilterBox;

  LoadPrjOptionList;
  LoadEnvOptionList;

  TCheckListBoxWithHints(lstPrjOptions).SortByString := True;
  TCheckListBoxWithHints(lstEnvOptions).SortByString := True;

  FStorage := TStructuredStorage.Create;
  FPrjSetOptions := TStringList.Create;
  FEnvSetOptions := TStringList.Create;
  {$IFDEF GX_VER130_up}
  splLeftRight.AutoSnap := False;
  {$ENDIF GX_VER130_up}
  FSplitterRatio := 0.50;
  LoadSettings;

  // Note: Optimally, we would go through IOTAOptions.EditOptions on our
  //   IOTAProjectOptions, but this fails when no project is loaded
  //   The TAction will work always, in particular for default project
  //   options, when there is no project loaded in the IDE.
  MainForm := GX_GenFunc.GetIdeMainForm;
  if Assigned(MainForm) then
  begin
    ProjectOptions := MainForm.FindComponent('ProjectOptionsCommand');
    if (ProjectOptions <> nil) and (ProjectOptions is TAction) then
    begin
      mniModifyPrjOptionValues.Action:= TAction(ProjectOptions);
      mniModifyPrjOptionValues.Caption := SModifyOptionValuesMenuCaption;
    end;
  end;
  if mniModifyPrjOptionValues.Action = nil then
    mniModifyPrjOptionValues.Enabled := False;

  // Load the saved list of project option sets
  LoadOptionSetList;
end;

destructor TfmProjOptionSets.Destroy;
begin
  SaveSettings;
  ClearPrjOptionList;

  // Before we free our structured storage object, try to defragment our file
  if Assigned(FStorage) then
    FStorage.DefragmentStorageFile(ProjOptsExpert.StorageFile);

  // Now free our objects
  FStorage.Free;
  FStorage := nil;

  FPrjSetOptions.Free;
  FPrjSetOptions := nil;

  FEnvSetOptions.Free;
  FEnvSetOptions := nil;

  fmProjOptionSets := nil;

  inherited Destroy;
end;

procedure TfmProjOptionSets.UpdateButtonState;
begin
  btnAdd.Enabled := True;
  btnDelete.Enabled := HaveSelectedSet;
  // Allow save even if the set has not changed (maybe the value
  // of an option has changed...)
  btnSave.Enabled := HaveSelectedSet;
  btnApply.Enabled := HaveSelectedSet and not FSetChanged;
  lstPrjOptions.Enabled := HaveSelectedSet;
  lstEnvOptions.Enabled := HaveSelectedSet;
  cbFilter.Enabled := HaveSelectedSet;
end;

procedure TfmProjOptionSets.lstSetsClick(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    if GetCurrentSetName <> FLastLoadedSet then
    begin
      // Load a options from the selected "file" in structured storage
      LoadSetOptions;
      // Store the item index
      FLastLoadedSet := GetCurrentSetName;
      FSetChanged := False;
    end;
  end;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.btnAddClick(Sender: TObject);
var
  NewSetName: string;
begin
  if InputQuery('Option Set Name', 'Option Set Name', NewSetName) then
  begin
    NewSetName := Trim(NewSetName);
    if NewSetName = '' then
      Exit;
    AddNewOptionSet(NewSetName);
  end;
end;

procedure TfmProjOptionSets.AddNewOptionSet(SetName: string);
var
  ItemPos: Integer;
begin
  ItemPos := lstSets.Items.IndexOf(SetName);
  if ItemPos = -1 then
  begin
    ItemPos := lstSets.Items.Add(SetName);
    // Update our set list
    UpdateSetListInStorage;
  end;
  lstSets.ItemIndex := ItemPos;
  lstSetsClick(lstSets);
  FSetChanged := True;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.btnDeleteClick(Sender: TObject);
resourcestring
  sDeleteMsg = 'Are you sure you want to delete this option set?';
begin
  if HaveSelectedSet then
  begin
    if MessageDlg(sDeleteMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Delete a file from our structured storage
      DeleteSetFromStorage;
      // Delete the item from our list
      lstSets.Items.Delete(lstSets.ItemIndex);
      FLastLoadedSet := '';
      // Now update our storage list of sets - this method
      // uses lstSets to re-write the set list in our storage
      UpdateSetListInStorage;
      // Clear checks from the option lists, since no set is selected
      SetAllChecks(lstPrjOptions, False);
      FPrjSetOptions.Clear;
      SetAllChecks(lstEnvOptions, False);
      FEnvSetOptions.Clear;
      UpdateButtonState;
    end;
  end;
end;

procedure TfmProjOptionSets.btnSaveClick(Sender: TObject);
begin
  if HaveSelectedSet then
  begin
    // Overwrite current set in the structured storage
    SaveSetOptions;
    // Now reload set options
    LoadSetOptions;
  end;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.btnApplyClick(Sender: TObject);
begin
  if HaveSelectedSet then
  try
    Screen.Cursor := crHourglass;
    // Apply the selected set to the Project Options file
    ApplySetOptions;
  finally
    Screen.Cursor := crDefault;
  end;
  UpdateButtonState;
end;

resourcestring
  SAllOptions = 'All options';

procedure TfmProjOptionSets.LoadPrjOptionList;
var
  j: Integer;
  OptionNames: TOTAOptionNameArray;
  tmpObj: TKindObject;
{$UNDEF CheckProjectOptionMap}
{$DEFINE CheckProjectOptionMap}
{$IFDEF CheckProjectOptionMap}
  i: Integer;
  MissingEntries: string;
{$ENDIF CheckProjectOptionMap}
begin
  OptionNames := nil;
  ClearPrjOptionList;
  FPrjOptions := GetProjectOptions;
  if Assigned(FPrjOptions) then
  try
      OptionNames := FPrjOptions.GetOptionNames;
      for j := Low(OptionNames) to High(OptionNames) do
      begin
      {$IFDEF CheckProjectOptionMap}
        i := High(GxOptionsMap);
        while i >= Low(GxOptionsMap) do
          begin
            if CompareText(GxOptionsMap[i].Name, OptionNames[j].Name) = 0 then
              Break;
            Dec(i);
          end;

        if i < Low(GxOptionsMap) then
        begin
          // Add missing project option to list
          MissingEntries := MissingEntries + OptionNames[j].Name;
          MissingEntries := MissingEntries + '(' + GetEnumName(TypeInfo(TTypeKind), Integer(OptionNames[j].Kind)) + '), ';
        end
        else
        begin
          // Sanity check: do we handle the IDE's option
          // with the right type ourselves?
          Assert(GxOptionsMap[i].AssumedTypeKind = OptionNames[j].Kind);
        end;
      {$ENDIF CheckProjectOptionMap}

        // Load options honoring filter selection
        if ((cbFilter.Text = sAllOptions) or
            (CategoryTextToCategory(cbFilter.Text) in GetOptionCategories(OptionNames[j].Name))
           ) and
           OptionIsAppropriateForIde(GetOptionCategories(OptionNames[j].Name)) then
        begin
          tmpObj := TKindObject.Create;
          try
            tmpObj.OptionKind := OptionNames[j].Kind;
            lstPrjOptions.Items.AddObject(OptionNames[j].Name, tmpObj);
          except
            on E: Exception do
            begin
              tmpObj.Free;
              raise;
            end;
          end;
        end;
        // Done loading option list
      end;

    {$IFDEF CheckProjectOptionMap}
      if MissingEntries <> '' then
      begin
        ClipBoard.AsText := MissingEntries;
        // Do not localize; this is just test code
        MessageDlg(MissingEntries, mtWarning, [mbOK], 0);
    end;
    {$ENDIF CheckProjectOptionMap}
  finally
    FPrjOptions := nil;
  end;
end;

procedure TfmProjOptionSets.LoadEnvOptionList;
var
  OptionNames: TOTAOptionNameArray;

    procedure AddEnvOptionItem(const OptionName: string);
    var
      i: Integer;
      tmpObj: TKindObject;
      EnvOption: TOTAOptionName;
    begin
      i := High(OptionNames);
      while i >= Low(OptionNames) do
      begin
        if OptionNames[i].Name = OptionName then  // Case-sensitive comparison
        begin
          EnvOption := OptionNames[i];
          Break;
        end;
        Dec(i);
      end;

      //  Assert(i >= Low(OptionNames), 'Could not find environment option ' + OptionName);
      if i >= Low(OptionNames) then
      begin
        tmpObj := TKindObject.Create;
        try
          tmpObj.OptionKind := EnvOption.Kind;
          lstEnvOptions.Items.AddObject(EnvOption.Name, tmpObj);
        except
          on E: Exception do
          begin
            tmpObj.Free;
            raise;
          end;
        end;
      end;
    end;

begin
  ClearEnvOptionList;

  FEnvOptions := GetEnvironmentOptions;
  if Assigned(FEnvOptions) then
  try
    OptionNames := FEnvOptions.GetOptionNames;
    Assert(OptionNames <> nil);

    // Note: we MANUALLY add to this list
    // as globally adding with all environment
    // options would pollute the list substantially
    // with irrelevant options and hence reduce
    // the usability of this feature.

    // The goal is to identify options that are
    // useful for individual project management.
    // Add when/where necessary

    AddEnvOptionItem('LibraryPath');
    AddEnvOptionItem('BrowsingPath');
    AddEnvOptionItem('RepositoryDir');

    AddEnvOptionItem('PackageSearchPath');
    AddEnvOptionItem('PackageDPLOutput');
    AddEnvOptionItem('PackageDCPOutput');

    AddEnvOptionItem('DeclarationInformation');
    AddEnvOptionItem('ScopeSort');

    AddEnvOptionItem('WarnOnPackageRebuild');

    AddEnvOptionItem('StepProgramBlock');

    AddEnvOptionItem('DFMAsText');
    AddEnvOptionItem('AutoCreateForms');
  finally
    FEnvOptions := nil;
  end;
end;

procedure TfmProjOptionSets.FormShow(Sender: TObject);
begin
  // Add a default, empty set if none exist
  if lstSets.Items.Count = 0 then
    AddNewOptionSet('Default');

  // Load project options in case they have not
  // been available yet; this takes care of the
  // case where the form is shown when no project
  // is open. Without these two lines, after doing
  // the above, it would be impossible to use the
  // expert, as no options would ever be re-retrieved.
  if not Assigned(FPrjOptions) then
    LoadPrjOptionList;

  if not Assigned(FEnvOptions) then
    LoadEnvOptionList;

  RefreshPrjCheckmarks;
  RefreshEnvCheckmarks;
  // Set item index in lstSets
  if lstSets.Items.Count > 0 then
    lstSets.ItemIndex := 0;
  lstSetsClick(lstSets);
  UpdateButtonState;

  // If we (un)dock, sort order is lost and we need to resort
  if Assigned(lstPrjOptions) then
    TCheckListBoxWithHints(lstPrjOptions).Resort;

  if Assigned(lstEnvOptions) then
    TCheckListBoxWithHints(lstEnvOptions).Resort;
end;


procedure TfmProjOptionSets.LoadOptionSetList;
var
  Success: Boolean;
  Stream: IStream;
  NumSets, i: Integer;
  SetName: string;
begin
  Assert(lstSets.Items.Count = 0, 'Set list already populated?');
  // Read structured storage (ProjOptsExpert.StorageFile) for list of "files"
  // and add them to lstSets
  Success := FStorage.OpenStorageFile(ProjOptsExpert.StorageFile,
    STGM_DIRECT or STGM_READ or STGM_SHARE_DENY_WRITE);
  if Success then
  try
    if FStorage.OpenStream(FStorage.StorageFile, 'SETS',
      STGM_DIRECT or STGM_READ or STGM_SHARE_EXCLUSIVE, Stream) then
    begin
      Success := FStorage.ReadInt(Stream, NumSets);
      Assert((NumSets >= 0) and (NumSets <= 500), 'Illegal number of option sets: ' + IntToStr(NumSets));
      if Success then
      begin
        for i := 0 to NumSets - 1 do
        begin
          Success := FStorage.ReadString(Stream, SetName);
          if Success then
            lstSets.Items.Add(SetName);
        end;
      end;
    end;
  finally
    Stream := nil; // Closes the stream ?
    FStorage.CloseStorageFile;
  end;
end;

procedure TfmProjOptionSets.ProcessListboxCheckClick(Listbox: TCheckListBox; Options: TStringList);
var
  Index: Integer;
  ItemString: string;
begin
  //{$IFOPT D+} SendDebug('Current Options: ' + Options.Text);  {$ENDIF}
  FSetChanged := True;
  Assert(Listbox.ItemIndex > -1, 'Check event with no selected item');
  ItemString := Listbox.Items[Listbox.ItemIndex];
  if Listbox.Checked[Listbox.ItemIndex] then
  begin // Add a new checked item to the stored list
    Index := Options.IndexOfName(ItemString);
    Assert(Index = -1, 'Check event for item in Options');
    ItemString := ItemString + '=['+SValUnknown+']';
    Options.Add(ItemString);
  end
  else
  begin // Remove an item from the stored list
    Index := Options.IndexOfName(ItemString);
    Assert(Index > -1, 'Uncheck event for item not in Options');
    Options.Delete(Index);
  end;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.lstEnvironmentOptClickCheck(Sender: TObject);
begin
  ProcessListboxCheckClick(lstEnvOptions, FEnvSetOptions);
end;

procedure TfmProjOptionSets.lstProjectOptClickCheck(Sender: TObject);
begin
  ProcessListboxCheckClick(lstPrjOptions, FPrjSetOptions);
end;

procedure TfmProjOptionSets.LoadSetOptions;
var
  Success: Boolean;
  Stream: IStream;
  NumOptions: Integer;
  i: Integer;
  Option: string;
begin
  FPrjSetOptions.Clear;
  FEnvSetOptions.Clear;
  Success := FStorage.OpenStorageFile(ProjOptsExpert.StorageFile,
    STGM_DIRECT or STGM_READ or STGM_SHARE_DENY_WRITE);
  if Success then
  try
    if FStorage.OpenStream(FStorage.StorageFile, GetCurrentSetName,
      STGM_DIRECT or STGM_READ or STGM_SHARE_EXCLUSIVE, Stream) then
    begin
      // First read project options
      Success := FStorage.ReadInt(Stream, NumOptions);
      Assert((NumOptions >= 0) and (NumOptions <= Length(GxOptionsMap)), 'Corrupt options storage file: ' + IntToStr(NumOptions));
      if Success then
      begin
        for i := 0 to NumOptions - 1 do
        begin
          Success := FStorage.ReadString(Stream, Option);
          if Success then
          begin
            Assert(Pos('=', Option) > 1, 'Bad project option string detected: ' + Option);
            FPrjSetOptions.Add(Option);
          end;
        end;
      end;

      // Now read environment options
      Success := FStorage.ReadInt(Stream, NumOptions);
      Assert((NumOptions >= 0) and (NumOptions <= Length(GxOptionsMap)), 'Corrupt options storage file: ' + IntToStr(NumOptions));
      if Success then
      begin
        for i := 0 to NumOptions - 1 do
        begin
          Success := FStorage.ReadString(Stream, Option);
          if Success then
          begin
            Assert(Pos('=', Option) > 1, 'Bad environment option string detected: ' + Option);
            FEnvSetOptions.Add(Option);
          end;
        end;
      end;
    end;
  finally
    Stream := nil; // Closes the stream ?
    FStorage.CloseStorageFile;
  end;
  RefreshPrjCheckmarks;
  RefreshEnvCheckmarks;
  FSetChanged := False;
end;

procedure TfmProjOptionSets.ApplySetOptions;
var
  j: Integer;
  OptionName: string;
begin
  // This method uses FPrjSetOptions and updates the ProjectOptions
  FPrjOptions := GetProjectOptions;
  try
    if Assigned(FPrjOptions) then
    begin
      for j := 0 to FPrjSetOptions.Count - 1 do
      begin
        OptionName := FPrjSetOptions.Names[j];
        Assert(Trim(OptionName) <> '', 'Empty option name in ApplySetOptions: ' + OptionName);
        SetPrjOptionValue(OptionName, FPrjSetOptions.Values[OptionName]);
      end;
    end;
  finally
    FPrjOptions := nil;
  end;

  // Now update the environment options also
  FEnvOptions := GetEnvironmentOptions;
  try
    if Assigned(FEnvOptions) then
    begin
      for j := 0 to FEnvSetOptions.Count - 1 do
      begin
        OptionName := FEnvSetOptions.Names[j];
        SetEnvOptionValue(OptionName, FEnvSetOptions.Values[OptionName]);
      end;
    end;
  finally
    FEnvOptions := nil;
  end;
end;

procedure TfmProjOptionSets.SaveSetOptions;
var
  Success: Boolean;
  NumOptions, i: Integer;
  OptionText: string;
  Stream: IStream;
begin
  if FileExists(ProjOptsExpert.StorageFile) then
    Success := FStorage.OpenStorageFile(ProjOptsExpert.StorageFile,
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE)
  else
    Success := FStorage.CreateStorageFile(ProjOptsExpert.StorageFile,
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE);
  if Success then
  try
    // Delete the existing "file"
    FStorage.DestroyElement(FStorage.StorageFile, GetCurrentSetName);
    // Create a new "file"
    Success := FStorage.CreateStream(FStorage.StorageFile, GetCurrentSetName,
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, Stream);
    if Success then
    begin
      // Write project options first
      FStorage.WriteInt(Stream, FPrjSetOptions.Count);
      FPrjOptions := GetProjectOptions;
      try
        if Assigned(FPrjOptions) then
        begin
          // Iterate over FPrjSetOptions due to filtering
          for i := 0 to FPrjSetOptions.Count - 1 do
          begin
            OptionText := FPrjSetOptions.Names[i];
            Assert(Trim(OptionText) <> '', 'Empty OptionText while saving project options: ' + FPrjSetOptions.Text);
            OptionText :=  OptionText + '=' + GetPrjOptionValue(OptionText);
            FStorage.WriteString(Stream, OptionText);
          end;
        end;
      finally
        FPrjOptions := nil;
      end;

      // Now write environment options
      NumOptions := GetCheckedCount(lstEnvOptions);
      FStorage.WriteInt(Stream, NumOptions);
      FEnvOptions := GetEnvironmentOptions;
      try
        if Assigned(FEnvOptions) then
        begin
          for i := 0 to lstEnvOptions.Items.Count - 1 do
          begin
            if lstEnvOptions.Checked[i] then
            begin
              OptionText := lstEnvOptions.Items[i] + '=' +
                GetEnvOptionValue(lstEnvOptions.Items[i]);
              FStorage.WriteString(Stream, OptionText);
            end;
          end;
        end;
      finally
        FEnvOptions := nil;
      end;
    end;
  finally
    Stream := nil;
    FStorage.CloseStorageFile;
  end;
end;

procedure TfmProjOptionSets.DeleteSetFromStorage;
var
  Success: Boolean;
begin
  Success := FStorage.OpenStorageFile(ProjOptsExpert.StorageFile,
    STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE);
  if Success then
  try
    FStorage.DestroyElement(FStorage.StorageFile, GetCurrentSetName);
  finally
    FStorage.CloseStorageFile;
  end;
end;

procedure TfmProjOptionSets.UpdateSetListInStorage;
var
  Success: Boolean;
  Stream: IStream;
  i: Integer;
begin
  if FileExists(ProjOptsExpert.StorageFile) then
    Success := FStorage.OpenStorageFile(ProjOptsExpert.StorageFile,
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE)
  else
    Success := FStorage.CreateStorageFile(ProjOptsExpert.StorageFile,
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE);
  if Success then
  try
    FStorage.DestroyElement(FStorage.StorageFile, 'SETS');
    Success := FStorage.CreateStream(FStorage.StorageFile, 'SETS',
      STGM_DIRECT or STGM_READWRITE or STGM_SHARE_EXCLUSIVE, Stream);
    if Success then
    begin
      FStorage.WriteInt(Stream, lstSets.Items.Count);
      for i := 0 to lstSets.Items.Count - 1 do
        FStorage.WriteString(Stream, lstSets.Items[i]);
    end;
  finally
    Stream := nil;
    FStorage.CloseStorageFile;
  end;
end;

function TfmProjOptionSets.GetCheckedCount(AList: TCheckListBox): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to AList.Items.Count - 1 do
  begin
    if AList.Checked[i] then
      Inc(Result);
  end;
end;

function TfmProjOptionSets.GetValueAsString(AValue: Variant; AKind: TTypeKind): string;
begin
  case AKind of
    tkUnknown,
    tkInteger,
    tkChar,
    tkEnumeration,
    tkFloat,
    tkString,
    tkSet,
    tkClass,
    tkMethod,
    tkWChar,
    tkLString,
    tkWString,
    tkVariant,
    tkArray,
    tkRecord,
    tkInterface,
    tkInt64,
    tkDynArray:  Result := VarAsType(AValue, varString);
  else
    Result := SValUnknown;
  end;
end;

procedure TfmProjOptionSets.GenericClearOptionList(AList: TCheckListBox);
var
  i: Integer;
begin
  if Assigned(AList) then
  begin
    for i := 0 to AList.Items.Count - 1 do
      AList.Items.Objects[i].Free;

    AList.Items.Clear;
  end;
end;

procedure TfmProjOptionSets.ClearPrjOptionList;
begin
  GenericClearOptionList(lstPrjOptions);
end;

procedure TfmProjOptionSets.ClearEnvOptionList;
begin
  GenericClearOptionList(lstEnvOptions);
end;

procedure TfmProjOptionSets.SetAllChecks(AList: TCheckListBox; ACheckFlag: Boolean);
var
  i: Integer;
begin
  for i := 0 to AList.Items.Count - 1 do
    AList.Checked[i] := ACheckFlag;
end;

procedure TfmProjOptionSets.mniPrjClearAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  SetAllChecks(lstPrjOptions, False);
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    Index := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    if Index > -1 then
      FPrjSetOptions.Delete(Index);
  end;
  FSetChanged := True;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.mniPrjCheckAllClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  SetAllChecks(lstPrjOptions, True);
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    Assert(Trim(lstPrjOptions.Items[i]) <> '', 'Empty option name in mniPrjCheckAllClick');
    Index := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    if Index = -1 then
      FPrjSetOptions.Add(lstPrjOptions.Items[i]+'=['+SValUnknown+']');
  end;
  FSetChanged := True;
  UpdateButtonState;
end;

procedure TfmProjOptionSets.SetupListOptionsControls;
begin
  lstPrjOptions := TCheckListBoxWithHints.Create(Self);
  lstPrjOptions.Align := alClient;
  lstPrjOptions.Parent := pnlCheckListHost;
  lstPrjOptions.PopupMenu := pmuPrjOptions;
  lstPrjOptions.OnClickCheck := lstProjectOptClickCheck;
  lstPrjOptions.ShowHint := True;
  TCheckListBoxWithHints(lstPrjOptions).OnGetHint := HandleOnGetHintPrj;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstPrjOptions).OnMouseMove := lstOptionsMouseMove;

  lstEnvOptions := TCheckListBoxWithHints.Create(Self);
  lstEnvOptions.Align := alClient;
  lstEnvOptions.Parent := tsEnvironmentSettings;
  lstEnvOptions.PopupMenu := pmuEnvOptions;
  lstEnvOptions.OnClickCheck := lstEnvironmentOptClickCheck;
  lstEnvOptions.ShowHint := True;
  TCheckListBoxWithHints(lstEnvOptions).OnGetHint := HandleOnGetHintEnv;
  // Activate this to update the hint as the mouse moves, without
  // having to select the listbox item under the cursor
  //TCheckListBoxWithHints(lstEnvOptions).OnMouseMove := lstOptionsMouseMove;
end;

procedure TfmProjOptionSets.FillFilterBox;
var
  i: TGxOptionCategory;
begin
  cbFilter.Items.Add(SAllOptions);
  cbFilter.ItemIndex := 0;

  for i := Low(TGxOptionCategory) to High(TGxOptionCategory) do
  begin
    if OptionCategoryIsAppropriateForIde(i) then
      cbFilter.Items.AddObject(GxOptionsCategoryText[i], Pointer(i));
  end;
end;

function TfmProjOptionSets.GetPrjOptionValue(const AOption: string): string;
var
  idx: Integer;
  tmpObj: TKindObject;
begin
  Result := '';
  if Assigned(FPrjOptions) then
  begin
    idx := lstPrjOptions.Items.IndexOf(AOption);
    if idx <> -1 then
    begin
      tmpObj := TKindObject(lstPrjOptions.Items.Objects[idx]);
      Result := GetValueAsString(FPrjOptions.Values[AOption], tmpObj.OptionKind);
    end;
  end;
end;

function TfmProjOptionSets.GetEnvOptionValue(const AOption: string): string;
var
  idx: Integer;
  tmpObj: TKindObject;
begin
  Result := '';
  if Assigned(FEnvOptions) then
  begin
    idx := lstEnvOptions.Items.IndexOf(AOption);
    if idx <> -1 then
    begin
      tmpObj := TKindObject(lstEnvOptions.Items.Objects[idx]);
      Result := GetValueAsString(FEnvOptions.Values[AOption], tmpObj.OptionKind);
    end;
  end;
end;

function TfmProjOptionSets.GetProjectOptions: IOTAProjectOptions;
var
  Modules: IOTAModuleServices;
  i: Integer;
  AModule: IOTAModule;
  AProject: IOTAProject;
begin
  Result := nil;
  Modules := (BorlandIDEServices as IOTAModuleServices);
  for i := 0 to Modules.ModuleCount - 1 do
  begin
    AModule := Modules.Modules[i];
    if (AProject = nil) and (AModule.QueryInterface(IOTAProject, AProject) = S_OK) then
    begin
      // Note: C++Builder 4.0 and 4.01 crash on AProject.ProjectOptions
      Result := AProject.ProjectOptions;
      Break;
    end;
  end;
end;

function TfmProjOptionSets.GetEnvironmentOptions: IOTAEnvironmentOptions;
var
  IServices: IOTAServices;
begin
  Result := nil;

  IServices := (BorlandIDEServices as IOTAServices);
  Result := IServices.GetEnvironmentOptions;
end;

procedure ProcessTranslatedValueForLineBreaks(var Value: string);
var
  SemicolonPos: Integer;
begin
  // If our value string does not contain any
  // line breaks, insert them manually.
  // We assume that we are using paths/defines
  // and that the semicolon is our preferred
  // delimiter - so break at each semicolon.
   if Pos(#13, Value) = 0 then
   begin
     SemicolonPos := Pos(';', Value);
     while SemicolonPos > 0 do
     begin
       Value[SemicolonPos] := #13;
       SemicolonPos := Pos(';', Value);
     end;
   end;
end;

procedure TfmProjOptionSets.HandleOnGetHintPrj(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstPrjOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstPrjOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstPrjOptions.ItemIndex;

  if idx <> -1 then
  begin
    OptionName := lstPrjOptions.Items[idx];
    HintStr := GetOptionDescription(OptionName);
    if HintStr <> '' then
      HintStr := HintStr + CRLF;
    HintStr := HintStr + OptionName + SOptValue;
    if lstPrjOptions.Checked[idx] and
      (FPrjSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + CRLF;
      // Get the saved value
      TranslatedValueString :=
        TranslatedValue(OptionName, FPrjSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;
    HintStr := HintStr + CRLF;
    // Get current value
    FPrjOptions := GetProjectOptions;
    try
      TranslatedValueString :=
        TranslatedValue(OptionName, GetPrjOptionValue(OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      FPrjOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.HandleOnGetHintEnv(Sender: TObject; const CursorPos: TPoint; var HintStr: string);
var
  idx: Integer;
  OptionName: string;
  TranslatedValueString: string;
begin
  HintStr := '';
  // HintStr := 'ItemIndex = ' + IntToStr(lstEnvOptions.ItemIndex);

  // By default display hint for that
  // item over which the cursor hovers.
  idx := lstEnvOptions.ItemAtPos(CursorPos, True);
  if not (idx >= 0) then
    idx := lstEnvOptions.ItemIndex;

  if idx <> -1 then
  begin
    OptionName := lstEnvOptions.Items[idx];

    //HintStr := GetEnvOptionDescription(OptionName);
    HintStr := OptionName;

    if HintStr <> '' then
      HintStr := HintStr + CRLF;

    HintStr := HintStr + OptionName + SOptValue;

    if lstEnvOptions.Checked[idx] and
       (FEnvSetOptions.IndexOfName(OptionName) <> -1) then
    begin
      HintStr := HintStr + CRLF;
      // Get the saved value.  Note that translation always fails, since
      // we don't have maps to translators for any environment options
      TranslatedValueString :=
        TranslatedValue(OptionName, FEnvSetOptions.Values[OptionName]);
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptSaved + TranslatedValueString;
    end;

    HintStr := HintStr + CRLF;

    // Get current value
    FEnvOptions := GetEnvironmentOptions;
    try
      TranslatedValueString := TranslatedValue(OptionName, GetEnvOptionValue(OptionName));
      ProcessTranslatedValueForLineBreaks(TranslatedValueString);
      HintStr := HintStr + SOptCurrent + TranslatedValueString;
    finally
      FEnvOptions := nil;
    end;
  end;
end;

procedure TfmProjOptionSets.SaveSettings;
begin
  // Do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteInteger('ProjOptionSets', 'Height', Height);
    WriteInteger('ProjOptionSets', 'Width', Width);
    //WriteInteger('ProjOptionSets', 'LeftWidth', pnlLeft.Width);
    WriteInteger('ProjOptionSets', 'SplitterRatio', Min(Max(Round(FSplitterRatio * 100), 10), 90));
    WriteString('ProjOptionSets', 'ActiveTab', pcSettings.ActivePage.Name);
  finally
    Free;
  end;
end;

procedure TfmProjOptionSets.LoadSettings;
var
  ActiveTabSheetName: string;
begin
  // Do not localize any of the below items
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    ActiveTabSheetName := ReadString('ProjOptionSets', 'ActiveTab', '');
    if ActiveTabSheetName = tsEnvironmentSettings.Name then
      pcSettings.ActivePage := tsEnvironmentSettings
    else
      pcSettings.ActivePage := tsProjectSettings;
    Width := ReadInteger('ProjOptionSets', 'Width', Width);
    Height := ReadInteger('ProjOptionSets', 'Height', Height);
    //pnlLeft.Width := ReadInteger('ProjOptionSets', 'LeftWidth', pnlLeft.Width);
    FSplitterRatio := ReadInteger('ProjOptionSets', 'SplitterRatio', 50) / 100;
    FormResize(Self);
  finally
    Free;
  end;
end;

procedure TfmProjOptionSets.mniPrjSortByCheckmarkClick(Sender: TObject);
begin
  TCheckListBoxWithHints(lstPrjOptions).SortByString := (Sender = mniPrjSortByName);
end;

procedure TfmProjOptionSets.mniPrjDescendingClick(Sender: TObject);
begin
  TCheckListBoxWithHints(lstPrjOptions).SortAscending := (Sender = mniPrjAscending);
end;

procedure TfmProjOptionSets.pmuPrjOptionsPopup(Sender: TObject);
begin
  if TCheckListBoxWithHints(lstPrjOptions).SortAscending then
    mniPrjAscending.Checked := True
  else
    mniPrjDescending.Checked := True;

  if TCheckListBoxWithHints(lstPrjOptions).SortByString then
    mniPrjSortByName.Checked := True
  else
    mniPrjSortByCheckmark.Checked := True;
end;

function TfmProjOptionSets.ProjOptsExpert: TProjOptionSetsExpert;
begin
  Result := PrjOptSetsExpert;
end;

procedure TfmProjOptionSets.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 27);
end;

procedure TfmProjOptionSets.cbFilterChange(Sender: TObject);
begin
  lstPrjOptions.Items.BeginUpdate;
  try
    LoadPrjOptionList;
    RefreshPrjCheckmarks;
  finally
    lstPrjOptions.Items.EndUpdate;
    UpdateButtonState;
  end;
end;

procedure TfmProjOptionSets.RefreshPrjCheckmarks;
var
  i, j: Integer;
begin
  if not Assigned(FPrjSetOptions) then
    Exit;
  // Now iterate over lstPrjOptions and check any items in FPrjSetOptions
  for i := 0 to lstPrjOptions.Items.Count - 1 do
  begin
    j := FPrjSetOptions.IndexOfName(lstPrjOptions.Items[i]);
    lstPrjOptions.Checked[i] := (j > -1);
  end;
  // Resort the list
  TCheckListBoxWithHints(lstPrjOptions).Resort;
end;

procedure TfmProjOptionSets.FormResize(Sender: TObject);
begin
  pnlLeft.Width := Trunc(FSplitterRatio * ClientWidth); //(pcSettings.Width + pnlLeft.Width));
end;

procedure TfmProjOptionSets.splLeftRightMoved(Sender: TObject);
begin
  FSplitterRatio := pnlLeft.Width / (pnlLeft.Width + pcSettings.Width);
  FormResize(Self);
end;

procedure TfmProjOptionSets.mniModifyEnvOptionValuesClick(Sender: TObject);
var
  IEnvironmentOptions: IOTAEnvironmentOptions;
begin
  IEnvironmentOptions := GetEnvironmentOptions;
  if Assigned(IEnvironmentOptions) then
    IEnvironmentOptions.EditOptions;
end;

procedure TfmProjOptionSets.splLeftRightCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
var
  SizingSplitter: TSplitter;
begin
  SizingSplitter := (Sender as TSplitter);
  if NewSize < SizingSplitter.MinSize then
    NewSize := SizingSplitter.MinSize;
end;

procedure TfmProjOptionSets.pnlOSInnerResize(Sender: TObject);
begin
  //lstSets.Width := pnlOSInner.Width - lstSets.Left - btnAdd.Width - 9;
  //lstSets.Height := pnlOSInner.ClientHeight - 10;
end;

procedure TfmProjOptionSets.RefreshEnvCheckmarks;
var
  i, j: Integer;
begin
  if not Assigned(FEnvSetOptions) then
    Exit;
  // Now iterate across FEnvSetOptions and check any items in lstEnvOptions that match
  for i := 0 to lstEnvOptions.Items.Count - 1 do
  begin
    j := FEnvSetOptions.IndexOfName(lstEnvOptions.Items[i]);
    lstEnvOptions.Checked[i] := (j > -1);
  end;
  // Resort the list
  TCheckListBoxWithHints(lstEnvOptions).Resort;
end;

procedure TfmProjOptionSets.SetEnvOptionValue(const AOption, AValue: string);
begin
  if Assigned(FEnvOptions) then
    FEnvOptions.Values[AOption] := AValue;
end;

procedure TfmProjOptionSets.SetPrjOptionValue(const AOption, AValue: string);
begin
  if Assigned(FPrjOptions) then
  begin
    //{$IFOPT D+} SendDebugFmt('Setting %s to %s (currently %s)', [AOption, AValue, FPrjOptions.Values[AOption]]);  {$ENDIF}
    try
      FPrjOptions.Values[AOption] := AValue;
    except on E: Exception do
      raise Exception.Create(Format('Error setting option %s to "%s" (%s).  IDE bug?', [AOption, AValue, E.Message]));
    end;
  end;
end;

procedure TfmProjOptionSets.FormCreate(Sender: TObject);
begin
  inherited;
  pcSettings.ActivePage := tsProjectSettings;
end;

function TfmProjOptionSets.GetCurrentSetName: string;
begin
  Assert(HaveSelectedSet, 'No selected set in GetCurrentSetName');
  Result := lstSets.Items[lstSets.ItemIndex];
end;

function TfmProjOptionSets.HaveSelectedSet: boolean;
begin
  Result := lstSets.ItemIndex <> -1;
end;

procedure TfmProjOptionSets.FormHide(Sender: TObject);
begin
  FLastLoadedSet := '';
end;


{ TShowBuggyOTAMessage }

function TShowBuggyOTAMessage.GetMessage: string;
begin
  Result := 'Your IDE version has a broken implementation of some of the '+
    'OpenTools project options interfaces.  You will find that most of the '+
    'settings in the Project Option Sets expert will be non-functional.  All versions of Delphi '+
    '4.0x and C++Builder 4.0x, as well as Delphi 5.00 suffer from this problem.  '+
    'There is no known workaround short of upgrading to Delphi 5.01 or greater.';
end;

function TShowBuggyOTAMessage.ShouldShow: Boolean;
begin
  // This is fixed in C++Builder 5.00+ and Delphi 5.01+
  Result := GetBorlandIdeVersion in [ideD300, ideD301, ideD302,
                                     ideD400, ideD401, ideD402, ideD403,
                                     ideD500,
                                     ideBCB300, ideBCB301,
                                     ideBCB400, ideBCB401, ideBCB402];
end;

initialization
  {$IFNDEF VER125}
    // StH: C++Builder 4.0x crashes on the call to GetProjectOptions.
    //      There is nothing that we can do about this.
    RegisterGX_Expert(TProjOptionSetsExpert);
  {$ENDIF VER125}

{$ELSE}
interface implementation // dummy unit
{$ENDIF GX_UseNativeToolsApi}

end.

