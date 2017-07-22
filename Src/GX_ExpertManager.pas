unit GX_ExpertManager;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics,
{$IFDEF GX_VER120_up}
  ImgList,
{$ENDIF GX_VER120_up}
  Controls, Forms, Dialogs,
  Menus, Buttons, StdCtrls, ExtCtrls, ComCtrls,
  DropTarget, DropSource, GX_Experts;

type
  TExpertManagerExpert = class;

  TfmExpertManager = class(TForm)
    pnToolBar: TPanel;
    lvExperts: TListView;
    sbEnabled: TSpeedButton;
    sbDisabled: TSpeedButton;
    sbAdd: TSpeedButton;
    sbRemove: TSpeedButton;
    mnuMain: TMainMenu;
    File1: TMenuItem;
    AddExpert1: TMenuItem;
    RemoveExpert1: TMenuItem;
    Experts1: TMenuItem;
    EnabledExpert1: TMenuItem;
    DisableExpert1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ilImages: TImageList;
    dlgAddExpert: TOpenDialog;
    sbHelp: TSpeedButton;
    Help2: TMenuItem;
    Contents1: TMenuItem;
    N2: TMenuItem;
    pmItems: TPopupMenu;
    EnablePopup: TMenuItem;
    DisablePopup: TMenuItem;
    N3: TMenuItem;
    RemovePopup: TMenuItem;
    StatusBar: TStatusBar;
    procedure DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint; var Effect: Longint);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure sbEnabledClick(Sender: TObject);
    procedure sbDisabledClick(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure sbHelpClick(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure lvExpertsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormResize(Sender: TObject);
  private
    FExpertManager: TExpertManagerExpert;
    FFileDrop: TDropFileTarget;
    procedure AddExperts(ExpertList: TStrings);
    procedure UpdateButtonState;
    procedure UpdateControlsState;
    procedure RefreshExpertListControl;
    function ConfirmIfGExperts(FileName: string): Boolean;
    procedure LoadUserInterfaceSettings;
    procedure SaveUserInterfaceSettings;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
    destructor Destroy; override;
  end;

  TExpertManagerExpert = class(TGX_EnhExpert)
  private
    FInitialExperts: TStrings;
    FInitialDisabledExperts: TStrings;

    function GetExpertsRegKeyName(IsEnabled: Boolean): string;
    procedure GetExperts(Experts: TStrings; IsEnabled: Boolean);

    function MoveExpertRegistryKey(ExpertName, FromKey, ToKey: string): Boolean;
    function AddExpertToRegistry(const ExpertName, FileName: string): Boolean;
    function EnableExpertInRegistry(ExpertName: string): Boolean;
    function DisableExpertInRegistry(ExpertName: string): Boolean;
    function RemoveExpertFromRegistry(ExpertName: string; IsEnabled: Boolean): Boolean;

    procedure ImportDisabledExpertsFromRegistry;

    procedure Execute;
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

    property InitialExperts: TStrings read FInitialExperts;
    property InitialDisabledExperts: TStrings read FInitialDisabledExperts;
  end;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
procedure CloseExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}

implementation

{$R *.DFM}
{$R ExpertManager.res}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  CommCtrl,
  Registry,
{$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
{$ENDIF GX_UseNativeToolsApi}
  ExptIntf, GX_GExperts, GX_GenFunc, GX_ConfigurationInfo, GX_MessageBox;

const
  // Do not localize any of the lines below
  UnknownExpertName = 'Unknown Expert';
  UnknownExpertFileName = 'Unknown';
  UnknownExpert = UnknownExpertName + ',' + UnknownExpertFileName + ',0';

type
  TGxExpertState = (gesCurrentlyEnabled, gesNextTimeEnabled,
                    gesCurrentlyDisabled, gesNextTimeDisabled,
                    gesInvalid);

  TShowDisableCurrentMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
    function GetButtons: TMsgDlgButtons; override;
    function GetDefaultButton: TMsgDlgBtn; override;
  end;

function GetListItemState(ListItem: TListItem): TGxExpertState;
begin
  if (ListItem <> nil) and (ListItem.StateIndex in [0..3]) then
    Result := TGxExpertState(ListItem.StateIndex)
  else
    Result := gesInvalid;
end;

constructor TfmExpertManager.Create(AOwner: TComponent);
begin
  Assert(False); // Call constructor CreateWithManger
end;

constructor TfmExpertManager.CreateWithManager(AOwner: TComponent; Manager: TExpertManagerExpert);
resourcestring
  SCouldNotLoadImages = 'Could not load expert DLL state images.';
begin
  FExpertManager := Manager;
  inherited Create(AOwner);

  ilImages.ResourceLoad(rtBitmap, 'EXPMGR_STATE', clOlive); // Do not localize

  FFileDrop := TDropFileTarget.Create(nil);
  FFileDrop.OnDrop := DropFiles;
  FFileDrop.Dragtypes := [dtCopy, dtMove, dtLink];
  FFileDrop.ShowImage := True;
  FFileDrop.Register(lvExperts);

  {$IFDEF GX_VER120_up}
  //lvExperts.Columns[1].AutoSize := True;
  {$ENDIF GX_VER120_up}
  LoadUserInterfaceSettings;

  if ilImages.Count = 0 then
    MessageDlg(SCouldNotLoadImages, mtInformation, [mbOK], 0);

  RefreshExpertListControl;
end;

destructor TfmExpertManager.Destroy;
begin
  FExpertManager := nil;
  SaveUserInterfaceSettings;

  FFileDrop.Unregister;
  FFileDrop.Free;
  FFileDrop := nil;

  inherited Destroy;
end;

procedure TfmExpertManager.About1Click(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TfmExpertManager.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfmExpertManager.RefreshExpertListControl;
var
  i: Integer;
  ListItem: TListItem;
  ExpertList: TStringList;
begin
  lvExperts.Items.BeginUpdate;
  try
    lvExperts.Items.Clear;
    ExpertList := TStringList.Create;
    try
      FExpertManager.GetExperts(ExpertList, True); // Get enabled experts
      for i := 0 to ExpertList.Count-1 do
      begin
        ListItem := lvExperts.Items.Add;
        ListItem.Caption := ExpertList.Names[i];
        ListItem.SubItems.Add(ExpertList.Values[ExpertList.Names[i]]);
        // Check if the expert was in registry when we started this expert
        if FExpertManager.InitialExperts.Values[ ExpertList.Names[i]] <> '' then
          ListItem.StateIndex := Ord(gesCurrentlyEnabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeEnabled);
      end;

      ExpertList.Clear;

      FExpertManager.GetExperts(ExpertList, False); // Get disabled experts
      for i := 0 to ExpertList.Count-1 do
      begin
        ListItem := lvExperts.Items.Add;
        ListItem.Caption := ExpertList.Names[i];
        ListItem.SubItems.Add(ExpertList.Values[ExpertList.Names[i]]);

        if FExpertManager.InitialDisabledExperts.Values[ExpertList.Names[i]] <> '' then
          ListItem.StateIndex := Ord(gesCurrentlyDisabled)
        else
          ListItem.StateIndex := Ord(gesNextTimeDisabled);
      end;
    finally
      ExpertList.Free;
    end;
  finally
    lvExperts.Items.EndUpdate;
  end;
end;

function TfmExpertManager.ConfirmIfGExperts(FileName: string): Boolean;
begin
  Result := True;
  if CompareText(FileName, GX_GenFunc.DllName) = 0 then
    Result := (ShowGxMessageBox(TShowDisableCurrentMessage) = mrYes);
end;

procedure TfmExpertManager.sbEnabledClick(Sender: TObject);
var
  GxExpertState: TGxExpertState;
begin
  if lvExperts.Selected <> nil then
  begin
    GxExpertState := GetListItemState(lvExperts.Selected);
    if (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]) then
      FExpertManager.DisableExpertInRegistry(lvExperts.Selected.Caption)
    else
      FExpertManager.EnableExpertInRegistry(lvExperts.Selected.Caption);
    UpdateControlsState;
  end;
end;

procedure TfmExpertManager.sbDisabledClick(Sender: TObject);
var
  GxExpertState: TGxExpertState;
resourcestring
  SCouldNotDisable = 'The expert could not be disabled.';
begin
  if lvExperts.Selected <> nil then
  begin
    GxExpertState := GetListItemState(lvExperts.Selected);
    if (GxExpertState in [gesCurrentlyDisabled, gesNextTimeDisabled]) then
      Exit;

    if not ConfirmIfGExperts(lvExperts.Selected.SubItems[0]) then
      Exit;

    if FExpertManager.DisableExpertInRegistry(lvExperts.Selected.Caption) then
      UpdateControlsState
    else
      MessageDlg(SCouldNotDisable, mtError, [mbOK], 0);
  end;
end;

procedure TfmExpertManager.lvExpertsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
    UpdateButtonState;
end;

procedure TfmExpertManager.UpdateButtonState;
resourcestring
  SCurrentlyDisabled = 'Expert DLL is currently disabled and not loaded';
  SCurrentlyEnabled  = 'Expert DLL is currently enabled and active';
  SNextTimeEnabled   = 'Expert DLL will be active next time IDE is started';
  SNextTimeDisabled  = 'Expert DLL will be inactive next time IDE is started';

    function GetDescriptiveState(State: TGxExpertState): string;
    begin
      case State of
        gesCurrentlyDisabled: Result := SCurrentlyDisabled;
        gesCurrentlyEnabled:  Result := SCurrentlyEnabled;
        gesNextTimeDisabled:  Result := SNextTimeDisabled;
        gesNextTimeEnabled:   Result := SNextTimeEnabled;
      else
        Result := '';
      end;
    end;

var
  CanRemoveExpert: Boolean;
  CanEnableExpert: Boolean;
  CanDisableExpert: Boolean;
  GxExpertState: TGxExpertState;
begin
  if lvExperts.Selected = nil then
  begin
    CanRemoveExpert := False;
    CanEnableExpert := False;
    CanDisableExpert := False;

    StatusBar.SimpleText := '';
  end
  else
  begin
    CanRemoveExpert := True;

    GxExpertState := GetListItemState(lvExperts.Selected);

    CanEnableExpert := (GxExpertState in [gesCurrentlyDisabled, gesNextTimeDisabled]);
    CanDisableExpert := (GxExpertState in [gesCurrentlyEnabled, gesNextTimeEnabled]);
    StatusBar.SimpleText := GetDescriptiveState(GxExpertState);
  end;

  sbRemove.Enabled := CanRemoveExpert;
  RemoveExpert1.Enabled := CanRemoveExpert;
  RemovePopup.Enabled := CanRemoveExpert;

  sbEnabled.Enabled := CanEnableExpert;
  EnabledExpert1.Enabled := CanEnableExpert;
  EnablePopup.Enabled := CanEnableExpert;

  sbDisabled.Enabled := CanDisableExpert;
  DisableExpert1.Enabled := CanDisableExpert;
  DisablePopup.Enabled := CanDisableExpert;
end;

procedure TfmExpertManager.UpdateControlsState;
var
  CurrentItemCaption: string;
  Item: TListItem;
begin
  CurrentItemCaption := '';
  if lvExperts.Selected <> nil then
    CurrentItemCaption := lvExperts.Selected.Caption;
  try
    RefreshExpertListControl;
  finally
    if CurrentItemCaption <> '' then
    begin
      Item := lvExperts.FindCaption(0, CurrentItemCaption, False, True, False);
      if Item <> nil then
        Item.Selected := True;
    end;
  end;
  UpdateButtonState;
end;

procedure TfmExpertManager.AddExperts(ExpertList: TStrings);
resourcestring
  SCouldNotAddExpert = '%s could not be added as an expert.'#13+
                       #13+
                       'The module you chose probably is not a valid expert DLL or is '+
                       'already loaded as an expert DLL.';
var
  i: Integer;
  ExpertName: string;
  ExpertEntry: string;
begin
  if ExpertList = nil then
    Exit;

  for i := 0 to ExpertList.Count -1 do
  begin
    ExpertEntry := ExpertList[i];

    ExpertName := ChangeFileExt(ExtractFileName(ExpertEntry), '');
    if FExpertManager.AddExpertToRegistry(ExpertName, ExpertEntry) then
      UpdateControlsState
    else
      MessageDlg(Format(SCouldNotAddExpert, [ExpertEntry]), mtError, [mbOK], 0);
  end;
end;

procedure TfmExpertManager.sbAddClick(Sender: TObject);
var
  ExpertList: TStringList;
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if not dlgAddExpert.Execute then
      Exit;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;

  ExpertList := TStringList.Create;
  try
    ExpertList.Add(dlgAddExpert.FileName);
    AddExperts(ExpertList);
  finally
    ExpertList.Free;
  end;
end;

procedure TfmExpertManager.sbRemoveClick(Sender: TObject);
resourcestring
  SConfirmRemoval = 'Are you sure you want to remove the selected expert?';
  SRemovalFailed = 'Could not remove the expert (registry error?).';
var
  ItemIsEnabled: Boolean;
begin
  if lvExperts.Selected = nil then
    Exit;

  if MessageDlg(SConfirmRemoval, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if not ConfirmIfGExperts(lvExperts.Selected.SubItems[0]) then
      Exit;

    ItemIsEnabled := GetListItemState(lvExperts.Selected) in [gesCurrentlyEnabled, gesNextTimeEnabled];
    if FExpertManager.RemoveExpertFromRegistry(lvExperts.Selected.Caption, ItemIsEnabled) then
      UpdateControlsState
    else
      MessageDlg(SRemovalFailed, mtError, [mbOK], 0);
  end;
end;

procedure TfmExpertManager.sbHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 7);
end;

procedure TfmExpertManager.Contents1Click(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTENTS, 0);
end;

procedure TfmExpertManager.DropFiles(Sender: TObject; ShiftState: TShiftState; Point: TPoint;
  var Effect: Longint);
begin
  AddExperts(FFileDrop.Files);
end;

procedure TfmExpertManager.LoadUserInterfaceSettings;
begin
  // Load window state, column widths??
end;

procedure TfmExpertManager.SaveUserInterfaceSettings;
begin
  // Save window state, column widths??
end;

procedure TfmExpertManager.FormResize(Sender: TObject);
begin
{.$IFNDEF GX_VER120_up}
  ListView_SetColumnWidth(lvExperts.Handle, 0, ColumnTextWidth);
  ListView_SetColumnWidth(lvExperts.Handle, 1, ColumnHeaderWidth);
{.$ENDIF GX_VER120_up}
end;


{ TExpertManagerExpert }

constructor TExpertManagerExpert.Create;
begin
  inherited Create;

  FInitialExperts := TStringList.Create;
  FInitialDisabledExperts := TStringList.Create;
  GetExperts(FInitialExperts, True);
  GetExperts(FInitialDisabledExperts, False);

  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TExpertManagerExpert.Destroy;
begin
  // Nothing has been added to the ancestor's SaveSettings
  //SaveSettings;

  FInitialExperts.Free;
  FInitialExperts := nil;

  FInitialDisabledExperts.Free;
  FInitialDisabledExperts := nil;

  inherited Destroy;
end;

procedure TExpertManagerExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here, Expert Manager has a modal form
    else
    begin
      // Nothing to free here, Expert Manager has a modal form
    end;
  end;
end;

function TExpertManagerExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Expert Manager...';
begin
  Result := SMenuCaption;
end;

function TExpertManagerExpert.GetMenuName: string;
begin
  Result := 'GX_ExpMgr';
end;

function TExpertManagerExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TExpertManagerExpert.GetName: string;
begin
  Result := 'Expert_Manager';
end;

function TExpertManagerExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Expert Manager';
begin
  Result := SDisplayName;
end;

procedure TExpertManagerExpert.Click(Sender: TObject);
begin
  Execute;
end;

procedure TExpertManagerExpert.Execute;
begin
  with TfmExpertManager.CreateWithManager(nil, Self) do
  try
    // Import from the old .97 registry format
    ImportDisabledExpertsFromRegistry;
    ShowModal;
  finally
    Free;
  end;
end;

function TExpertManagerExpert.RemoveExpertFromRegistry(ExpertName: string; IsEnabled: Boolean): Boolean;
begin
  Result := False;

  with TRegistry.Create do
  try
    if OpenKey(GetExpertsRegKeyName(IsEnabled), False) then
    begin
      Result := DeleteValue(ExpertName);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

// Returns true if FileName points to a valid expert DLL
// (valid wizard / expert entry point). False if not.
// Returns always true on Windows 9x, since Windows 9x does not
// support the DONT_RESOLVE_DLL_REFERENCES parameter for plain
// loading of the DLL as data.
{$DEFINE LoadAndCheckEntryPoint}
function IsValidExpertDll(const FileName: string): Boolean;
{$IFDEF LoadAndCheckEntryPoint}
var
  DllHandle: THandle;
{$ENDIF LoadAndCheckEntryPoint}
begin
  Result := True;

{$IFDEF LoadAndCheckEntryPoint}
  // Check that the DLL *really* is a valid expert or wizard DLL (Supported on NT only)
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    DllHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES {NT only!});
    try
      Result := (GetProcAddress(DllHandle, ExpertEntryPoint) <> nil);
    {$IFDEF GX_UseNativeToolsApi} // Note: This switch is slightly incorrect; do not fix
      // As of Delphi 4+ and C++Builder 4+ experts may be "wizards", too.
      if not Result then
        Result := (GetProcAddress(DllHandle, WizardEntryPoint) <> nil);
    {$ENDIF GX_UseNativeToolsApi}
    finally
      FreeLibrary(DllHandle);
    end;
  end; {NT Check}
{$ENDIF LoadAndCheckEntryPoint}
end;

function TExpertManagerExpert.AddExpertToRegistry(const ExpertName, FileName: string): Boolean;
var
  RegIni: TRegIniFile;
  ExpertList: TStringList;
  i: Integer;
begin
  {$IFOPT D+} SendDebug('Adding Expert: '+FileName); {$ENDIF}
  Result := True;

  // Make sure that this particular expert is not already loaded.
  ExpertList := TStringList.Create;
  try
    GetExperts(ExpertList, True); // Get enabled experts
    for i := 0 to ExpertList.Count - 1 do
    begin
      if ExpertList.Values[ExpertList.Names[i]] = FileName then
      begin
        Result := False;
        Break;
      end;
    end;
  finally
    ExpertList.Free;
  end;

  if not Result then
    Exit;

  Result := IsValidExpertDll(FileName);
  if not Result then
    Exit;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey);
  try
    RegIni.WriteString('Experts', ExpertName, FileName); // Do not localize.
  finally
    RegIni.Free;
  end;
end;

// Function to move a value from one registry key to another
function TExpertManagerExpert.MoveExpertRegistryKey(ExpertName, FromKey, ToKey: string): Boolean;
var
  RegFrom: TRegistry;
  RegTo: TRegistry;
begin
  Result := False;
  try

    RegTo := nil;
    RegFrom := TRegistry.Create;
    try
      RegTo := TRegistry.Create;

      if RegFrom.OpenKey(FromKey, False) then
      begin
        if RegTo.OpenKey(ToKey, True) then
        begin
          if RegFrom.ValueExists(ExpertName) then
          begin
            RegTo.WriteString(ExpertName, RegFrom.ReadString(ExpertName));
            RegFrom.DeleteValue(ExpertName);
            Result := True;
          end;
        end;
      end;
    finally
      RegTo.Free;
      RegFrom.Free;
    end;

  except
    on E: Exception do
    begin
      Result := False;
      // Swallow exception
    end;
  end;
end;

function TExpertManagerExpert.EnableExpertInRegistry(ExpertName: string): Boolean;
begin
  Result := MoveExpertRegistryKey(ExpertName, GetExpertsRegKeyName(False),
              GetExpertsRegKeyName(True));
end;

function TExpertManagerExpert.DisableExpertInRegistry(ExpertName: string): Boolean;
begin
  Result := MoveExpertRegistryKey(ExpertName, GetExpertsRegKeyName(True),
              GetExpertsRegKeyName(False));
end;

procedure TExpertManagerExpert.ImportDisabledExpertsFromRegistry;

  procedure AddDisabledExpert(Value: string);
  var
    ExpertName: string;
    DLLName: string;
    RegIni: TRegIniFile;
  begin
    ExpertName := Copy(Value, 1, Pos(',', Value) - 1);
    Delete(Value, 1, Pos(',', Value));
    DllName := Value;
    RegIni := TRegIniFile.Create(GetExpertsRegKeyName(False));
    try
      if not RegIni.ValueExists(ExpertName) then
        RegIni.WriteString('', ExpertName, DLLName);
    finally
      RegIni.Free;
    end;
  end;

var
  RegIni: TRegIniFile;
  i: Integer;
  FileNameValue: string;
  IsConverted: Boolean;
begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    IsConverted := RegIni.ReadBool('Expert Manager', 'Converted', False);
    if not IsConverted then
    begin
      for i := 0 to RegIni.ReadInteger('Expert Manager', 'Count', 0) - 1 do
      begin
        FileNameValue := RegIni.ReadString('Expert Manager', Format('Expert%d', [i]), UnknownExpert);
        if FileNameValue <> UnknownExpert then
          AddDisabledExpert(FileNameValue);
      end;
    end;
    RegIni.WriteBool('Expert Manager', 'Converted', True);
  finally
    RegIni.Free;
  end;
end;

function TExpertManagerExpert.IconFileName: string;
begin
  Result := 'ExpMgr';
end;

function TExpertManagerExpert.GetExpertsRegKeyName(IsEnabled: Boolean): string;
begin
  if IsEnabled then
    Result := ConfigInfo.RegKey + '\Experts'  // Do not localize
  else
    Result := ConfigInfo.RegKey + '\GExperts\Expert Manager\Disabled Experts';  // Do not localize
end;

procedure TExpertManagerExpert.GetExperts(Experts: TStrings; IsEnabled: Boolean);
begin
  with TRegIniFile.Create(GetExpertsRegKeyName(IsEnabled)) do
  try
    ReadSectionValues('', Experts);
  finally
    Free;
  end;
end;

// Code used by ExpMgr executable - DLL exports

var
  ExpMgr: TExpertManagerExpert = nil;

procedure ShowExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
begin
  {$IFOPT D+}SendDebug('Showing expert manager'); {$ENDIF}
  ExpMgr := TExpertManagerExpert.Create;
  ExpMgr.Execute;
end;

procedure CloseExpertManager; {$IFNDEF GX_BCB} export; {$ENDIF GX_BCB}
begin
  {$IFOPT D+}SendDebug('Closing expert manager'); {$ENDIF}
  ExpMgr.Free;
  ExpMgr := nil;
end;

{ TShowDisableCurrentMessage }

function TShowDisableCurrentMessage.GetButtons: TMsgDlgButtons;
begin
  Result := [mbYes, mbNo];
end;

function TShowDisableCurrentMessage.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbYes;
end;

function TShowDisableCurrentMessage.GetMessage: string;
resourcestring
  SConfirmGExpertsDisable = 'You are about to disable or remove the GExperts DLL '+
                            'that contains the expert you are currently using.'#13+
                            #13+
                            'If you proceed, you will not be able to use this Expert Manager '+
                            'the next time the IDE is started.'#13+
                            #13+
                            'Do you want to proceed?';
begin
  Result := SConfirmGExpertsDisable;
end;

initialization
  RegisterGX_Expert(TExpertManagerExpert);
end.

