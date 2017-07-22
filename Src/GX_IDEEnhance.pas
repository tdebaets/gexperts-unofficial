unit GX_IDEEnhance;

{$I GX_CondDefine.Inc}

interface

uses
  Windows, Messages, Classes, Graphics, ComCtrls, Forms, Menus, ExtCtrls,
  GX_MultiLinePalette, GX_DfmToTxt, GX_MultilineHost, // Errors here mean \GXSource\Src is not in your library path
  ExptIntf, ToolIntf;

type
  TIdeEnhancements = class(TObject)
  private
    FEnabled: Boolean;
    // Component palette
    FCPMultiLine: Boolean;
    FCPHotTracking: Boolean;
    FCPAsButtons: Boolean;
    FCPRaggedRight: Boolean;
    FCPScrollOpposite: Boolean;
    FCPFlatButtons: Boolean;
    FCPTabsInPopup: Boolean;
    FCPTabsInPopupAlphaSort: Boolean;
    FOldCPPopupEvent: TNotifyEvent;
    // Fonts
    FCPFontEnabled: Boolean;
    FCPFont: TFont;
    FOldCPFont: TFont;
    FOIFont: TFont;
    FOldOIFont: TFont;
    FOIFontEnabled: Boolean;
    // File saving
    FAutoSave: Boolean;
    FAutoSaveInterval: Integer;
    // Menus
    FShowWindowsMenu: Boolean;
    FDfm2TxtHelper: TDfm2TxtHelper;
{$IFDEF VER120} // Delphi 4 only needs this
    FShowAttachToMenuItem: Boolean;
{$ENDIF VER120}
{$IFDEF VER100}
    FShowCpuMenuItem: Boolean;
    FShowApiHelpMenuItem: Boolean;
    procedure SetShowCpuMenuItem(AShowCpuMenuItem: Boolean);
    procedure SetShowApiHelpMenuItem(AShowApiHelpMenuItem: Boolean);
{$ENDIF VER100}
{$IFDEF VER120}
    procedure SetShowAttachToMenuItem(AShowAttachToMenuItem: Boolean);
{$ENDIF VER120}
    procedure AddTabsToPopup(Sender: TObject);
    procedure DeleteCPPopupMenuItems(Popup: TPopupMenu);
    procedure SetActiveTab(Sender: TObject);
    procedure SetCPMultiLine(Value: Boolean);
    procedure SetCPAsButtons(Value: Boolean);
    procedure SetCPTabsInPopup(Value: Boolean);
    procedure SetCPTabsInPopupAlphaSort(Value: Boolean);
    procedure SetOIFont(Value: TFont);
    procedure SetOIFontEnabled(Value: Boolean);
    procedure OIFontChange(Sender: TObject);
    procedure SetCPFont(Value: TFont);
    procedure SetCPFontEnabled(Value: Boolean);
    procedure CPFontChange(Sender: TObject);
    procedure SetShowWindowsMenu(AShowWindowsMenu: Boolean);
    procedure SetCPFlatButtons(const Value: Boolean);
    procedure SetAutoSave(const Value: Boolean);
    procedure SetAutoSaveInterval(const Value: Integer);
    procedure SetSaveDfmAsTxt(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetCPRaggedRight(const Value: Boolean);
    procedure SetCPScrollOpposite(const Value: Boolean);
    function GetSaveDfmAsTxt: Boolean;
  private
    FWindowMenuItem: TIMenuItemIntf;
    FWindowMenuList: TList;
    FWindowList: TList;
    procedure InstallWindowMenu;
    procedure RemoveWindowMenu;
    procedure InsertWindowMenu(idx: Integer);
    procedure FreeWindowMenu;
    procedure ClearWindowItems;
    procedure WindowClick(Sender: TIMenuItemIntF);
    procedure GotoWindowClick(Sender: TIMenuItemIntf);
{$IFDEF VER100}
    // These menu items do not exist in any IDE but Delphi 3.0
    procedure InstallCpuMenuItem;
    procedure RemoveCpuMenuItem;
    procedure InstallWinApiMenuItem;
    procedure RemoveWinApiMenuItem;
    procedure ShowMenuItem(const MenuName: string; Visible: Boolean);
{$ENDIF VER100}
  private
    FMultiLineTabManager: TMultiLineTabManager;
    procedure InstallMultiLineComponentTabs;
    procedure RemoveMultiLineComponentTabs;
{$IFDEF GX_VER120_up}
  private
    FMultiLineTabDockHostManager: TGxMultiLineTabDockHostsManager;
    procedure InstallMultiLineHostTabs;
    procedure RemoveMultiLineHostTabs;
    function GetDefaultMultilineTabDockHost: Boolean;
    procedure SetDefaultMultilineTabDockHost(const Value: Boolean);
    function GetMultilineTabDockHost: Boolean;
    procedure SetMultilineTabDockHost(const Value: Boolean);
{$ENDIF GX_VER120_up}
  private
    procedure Install;
    procedure Remove;
  public
    constructor Create;
    destructor Destroy; override;

    property Enabled: Boolean read FEnabled write SetEnabled;

    procedure LoadSettings;
    procedure SaveSettings;

    // Menus
    property ShowWindowsMenu: Boolean read FShowWindowsMenu write SetShowWindowsMenu;
{$IFDEF VER100}
    property ShowCpuMenuItem: Boolean read FShowCpuMenuItem write SetShowCpuMenuItem;
    property ShowApiHelpMenuItem: Boolean read FShowApiHelpMenuItem write SetShowApiHelpMenuItem;
{$ENDIF VER100}
{$IFDEF VER120}
    property ShowAttachToMenuItem: Boolean read FShowAttachToMenuItem write SetShowAttachToMenuItem;
{$ENDIF VER120}
    // Component palette
    property CPMultiLine: Boolean read FCPMultiLine write SetCPMultiLine;
    property CPHotTracking: Boolean read FCPHotTracking write FCPHotTracking;
    property CPAsButtons: Boolean read FCPAsButtons write SetCPAsButtons;
    property CPFlatButtons: Boolean read FCPFlatButtons write SetCPFlatButtons;
    property CPScrollOpposite: Boolean read FCPScrollOpposite write SetCPScrollOpposite;
    property CPRaggedRight: Boolean read FCPRaggedRight write SetCPRaggedRight;
    property CPTabsInPopup: Boolean read FCPTabsInPopup write SetCPTabsInPopup;
    property CPTabsInPopupAlphaSort: Boolean read FCPTabsInPopupAlphaSort write SetCPTabsInPopupAlphaSort;
    // Fonts
    property OIFontEnabled: Boolean read FOIFontEnabled write SetOIFontEnabled;
    property CPFontEnabled: Boolean read FCPFontEnabled write SetCPFontEnabled;
    property OIFont: TFont read FOIFont;
    property CPFont: TFont read FCPFont;
    // File saving
    property SaveDfmAsTxt: Boolean read GetSaveDfmAsTxt write SetSaveDfmAsTxt;
    property AutoSave: Boolean read FAutoSave write SetAutoSave;
    property AutoSaveInterval: Integer read FAutoSaveInterval write SetAutoSaveInterval;
{$IFDEF GX_VER120_up}
    // Multi-line tab dock host
    property MultilineTabDockHost: Boolean read GetMultilineTabDockHost write SetMultilineTabDockHost;
    property DefaultMultilineTabDockHost: Boolean read GetDefaultMultilineTabDockHost write SetDefaultMultilineTabDockHost;
{$ENDIF GX_VER120_up}
  end;

function IdeEnhancements: TIdeEnhancements;
procedure FreeIdeEnhancements;

implementation

uses
{$IFDEF GX_VER120_up}
  TypInfo,
{$ENDIF GX_VER120_up}
  Registry, Controls, GX_GenFunc, SysUtils, GX_ConfigurationInfo,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  CommCtrl, Dialogs;

function GetObjInspForm: TCustomForm;
begin
  Result := GetIdeMainForm;
  if Result <> nil then
    Result := TCustomForm(Result.FindComponent('PropertyInspector'));
  {$IFOPT D+}if Result = nil then SendDebug('Unable to find object inspector!'); {$ENDIF}
end;

function GetCPPopupMenu: TPopupMenu;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := TPopupMenu(MainForm.FindComponent('PaletteMenu'));
  {$IFOPT D+}if Result = nil then SendDebug('Unable to find PaletteMenu!'); {$ENDIF}
end;


{ TIdeEnhancements }

constructor TIdeEnhancements.Create;
begin
  inherited Create;

  FOIFont := TFont.Create;
  FOIFont.OnChange := OIFontChange;
  FCPFont := TFont.Create;
  FCPFont.OnChange := CPFontChange;

  FWindowMenuList := TList.Create;
  FWindowList := TList.Create;
  FDfm2TxtHelper := TDfm2TxtHelper.Create;
end;

procedure TIdeEnhancements.Install;
begin
  if ToolServices = nil then
    Exit;
  Assert(Application.MainForm <> nil);

  {$IFOPT D+}SendDebug('Installing IDE Enhancements and loading settings'); {$ENDIF}
  LoadSettings;
  {$IFOPT D+}SendDebug('Loaded IDE Enhancement settings'); {$ENDIF}

  if CPMultiLine then
    InstallMultiLineComponentTabs;
end;

procedure TIdeEnhancements.Remove;
begin
  if ToolServices = nil then
    Exit;
  //SaveSettings;

  // Multiline component palette
  CPMultiLine := False;
  CPAsButtons := False;
  CPHotTracking := False;
  CPTabsInPopup := False;
  CPTabsInPopupAlphaSort := False;
  // Fonts
  CPFontEnabled := False;
  OIFontEnabled := False;
  // Menus
  ShowWindowsMenu := False;
{$IFDEF VER100}
  ShowApiHelpMenuItem := False;
  ShowCpuMenuItem := False;
{$ENDIF VER100}

  RemoveMultiLineComponentTabs;
{$IFDEF GX_VER120_up}
  RemoveMultiLineHostTabs;
{$ENDIF GX_VER120_up}
end;

procedure TIdeEnhancements.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if FEnabled then
    Install
  else
    Remove;
end;

destructor TIdeEnhancements.Destroy;
begin
  Remove;

  FDfm2TxtHelper.Free;
  FDfm2TxtHelper := nil;

  FOIFont.Free;
  FOIFont := nil;
  FCPFont.Free;
  FCPFont := nil;

  FWindowMenuList.Free;
  FWindowMenuList := nil;
  FWindowList.Free;
  FWindowList := nil;

  Assert(FWindowMenuItem = nil, 'Windows menu not freed');

  inherited Destroy;
end;

procedure TIdeEnhancements.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  Assert(ConfigInfo <> nil);

  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  with RegIni do
  try
    // Component palette
    CPMultiLine := ReadBool('IDEEnhancements', 'CPMultiLine', False);
    CPScrollOpposite := ReadBool('IDEEnhancements', 'CPScrollOpposite', False);
    CPRaggedRight := ReadBool('IDEEnhancements', 'CPRaggedRight', False);
{$IFDEF GX_VER120_up} // Delphi, BCB 4
    CPFlatButtons := ReadBool('IDEEnhancements', 'CPFlatButtons', False);
{$ENDIF GX_VER120_up} // Delphi, BCB 4
    CPAsButtons := ReadBool('IDEEnhancements', 'CPAsButtons', False);
    CPTabsInPopup := ReadBool('IDEEnhancements', 'CPTabsInPopup', False);
    CPTabsInPopupAlphaSort := ReadBool('IDEEnhancements', 'CPTabsInPopupAlphaSort', False);
    CPHotTracking := ReadBool('IDEEnhancements', 'CPHotTracking', False);

    // Menus
    ShowWindowsMenu := ReadBool('IDEEnhancements', 'ShowWindowsMenu', False);
{$IFDEF VER120}
    ShowAttachToMenuItem := ReadBool('IDEEnhancements', 'ShowAttachToMenu', False);
{$ENDIF VER120}
{$IFDEF VER100}
    ShowCpuMenuItem := ReadBool('IDEEnhancements', 'ShowCPUMenu', False);
    ShowApiHelpMenuItem := ReadBool('IDEEnhancements', 'ShowAPIMenu', False);
{$ENDIF VER100}
    // File saving
    SaveDfmAsTxt := ReadBool('IDEEnhancements', 'SaveDfmAsText', False);
    AutoSave := ReadBool('IDEEnhancements', 'AutoSave', False);
    AutoSaveInterval := ReadInteger('IDEEnhancements', 'AutoSaveInterval', 5);
    // Fonts
    LoadFont(RegIni, 'IDEEnhancements\CPFont', CPFont);
    LoadFont(RegIni, 'IDEEnhancements\OIFont', OIFont);
    OIFontEnabled := ReadBool('IDEEnhancements', 'EnableOIFont', False);
    CPFontEnabled := ReadBool('IDEEnhancements', 'EnableCPFont', False);
    // Multiline tab dock host
{$IFDEF GX_VER120_up}
    MultilineTabDockHost := ReadBool('IDEEnhancements', 'MultilineTabDockHost', False);
    DefaultMultilineTabDockHost := ReadBool('IDEEnhancements', 'DefaultMultilineTabDockHost', True);
{$ENDIF GX_VER120_up}
  finally
    RegIni.Free;
  end;
end;

procedure TIdeEnhancements.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  Assert(ConfigInfo <> nil);

  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  with RegIni do
  try
    // Menus
    WriteBool('IDEEnhancements', 'ShowWindowsMenu', ShowWindowsMenu);
{$IFDEF VER120}
    WriteBool('IDEEnhancements', 'ShowAttachToMenu', ShowAttachToMenuItem);
{$ENDIF VER120}
{$IFDEF VER100}
    WriteBool('IDEEnhancements', 'ShowAPIMenu', ShowApiHelpMenuItem);
    WriteBool('IDEEnhancements', 'ShowCPUMenu', ShowCpuMenuItem);
{$ENDIF VER100}
    // Component palette
    WriteBool('IDEEnhancements', 'CPMultiLine', CPMultiLine);
    WriteBool('IDEEnhancements', 'CPScrollOpposite', CPScrollOpposite);
    WriteBool('IDEEnhancements', 'CPRaggedRight', CPRaggedRight);
    WriteBool('IDEEnhancements', 'CPHotTracking', CPHotTracking);
    WriteBool('IDEEnhancements', 'CPAsButtons', CPAsButtons);
{$IFDEF GX_VER120_up} // Delphi, BCB 4
    WriteBool('IDEEnhancements', 'CPFlatButtons', CPFlatButtons);
{$ENDIF GX_VER120_up} // Delphi, BCB 4
    // File saving
    WriteBool('IDEEnhancements', 'SaveDfmAsText', SaveDfmAsTxt);
    WriteBool('IDEEnhancements', 'AutoSave', AutoSave);
    WriteInteger('IDEEnhancements', 'AutoSaveInterval', AutoSaveInterval);
    WriteBool('IDEEnhancements', 'CPTabsInPopup', CPTabsInPopup);
    WriteBool('IDEEnhancements', 'CPTabsInPopupAlphaSort', CPTabsInPopupAlphaSort);
    // Fonts
    WriteBool('IDEEnhancements', 'EnableOIFont', OIFontEnabled);
    WriteBool('IDEEnhancements', 'EnableCPFont', CPFontEnabled);
    SaveFont(RegIni, 'IDEEnhancements\CPFont', CPFont);
    SaveFont(RegIni, 'IDEEnhancements\OIFont', OIFont);
    // Multiline tab dock host
{$IFDEF GX_VER120_up}
    WriteBool('IDEEnhancements', 'MultilineTabDockHost', MultilineTabDockHost);
    WriteBool('IDEEnhancements', 'DefaultMultilineTabDockHost', DefaultMultilineTabDockHost);
{$ENDIF GX_VER120_up}
  finally
    RegIni.Free;
  end;
end;

procedure TIdeEnhancements.AddTabsToPopup(Sender: TObject);
var
  CPPopupMenu: TPopupMenu;
  TabControl: TTabControl;

  procedure AddPopupMenuItems;
  var
    StartInsertingAt: Integer;
    InsertionPoint: Integer;
    i: Integer;
    Menu: TMenuItem;
  begin
    Menu := TMenuItem.Create(nil);
    Menu.Caption := '-';
    Menu.Tag := -1;
    Menu.Name := 'GX_PopupSeparator';
    CPPopupMenu.Items.Add(Menu);

    StartInsertingAt := CPPopupMenu.Items.Count;

    TabControl := GetComponentPaletteTabControl;
    if TabControl <> nil then
      for i := 0 to TabControl.Tabs.Count - 1 do
      begin
        Menu := TMenuItem.Create(nil);
        Menu.Caption := TabControl.Tabs[i];
        Menu.Tag := -1;
        Menu.Name := 'GX_Palette' + IntToStr(i);
        Menu.RadioItem := True;
        Menu.GroupIndex := 99;
        Menu.Checked := (i = TabControl.TabIndex);
        Menu.OnClick := SetActiveTab;
        // This allows a max of 20 tabs per column.  Not perfect, but
        // still nicer than menu items disappearing off the screen.
        if ((StartInsertingAt + i - 1) mod 20 = 0) then
          Menu.Break := mbBarBreak;

        if not CPTabsInPopupAlphaSort then
          CPPopupMenu.Items.Add(Menu)
        else
        begin
          InsertionPoint := StartInsertingAt;
          while InsertionPoint < CPPopupMenu.Items.Count do
          begin
            if CompareText(CPPopupMenu.Items[InsertionPoint].Caption, Menu.Caption) > 0 then
              Break;
            Inc(InsertionPoint);
          end;
          CPPopupMenu.Items.Insert(InsertionPoint, Menu);
        end;
      end;
  end;

begin
  if (Sender = nil) or (not (Sender is TPopupMenu)) then Exit;
  CPPopupMenu := TPopupMenu(Sender);
  DeleteCPPopupMenuItems(CPPopupMenu);
  if Assigned(FOldCPPopupEvent) then
    FOldCPPopupEvent(Sender);
  AddPopupMenuItems;
end;

procedure TIdeEnhancements.DeleteCPPopupMenuItems(Popup: TPopupMenu);
var
  i: Integer;
  Menu: TMenuItem;
begin
  i := 0;
  while i <= Popup.Items.Count - 1 do
  begin
    if Popup.Items[i].Tag = -1 then
    begin
      Menu := Popup.Items[i];
      Popup.Items.Delete(i);
      Menu.Free;
    end
    else
      Inc(i);
  end;
end;

procedure TIdeEnhancements.SetActiveTab(Sender: TObject);
var
  TabControl: TTabControl;
  Tab: string;
  i: Integer;
begin
  TabControl := GetComponentPaletteTabControl;
  if TabControl <> nil then
  begin
    Tab := TMenuItem(Sender).Caption;
{$IFDEF GX_VER130_up}
    // Compensate for AutoHotKeys
    Tab := StringReplace(Tab, '&', '', [rfReplaceAll]);
{$ENDIF GX_VER130_up}
    for i := 0 to TabControl.Tabs.Count - 1 do
      if TabControl.Tabs[i] = Tab then
      begin
        TabControl.TabIndex := i;
        TabControl.OnChange(TabControl);
        Break;
      end;
  end;
end;

procedure TIdeEnhancements.SetAutoSave(const Value: Boolean);
begin
  // Do something here
  FAutoSave := Value;
end;

procedure TIdeEnhancements.SetAutoSaveInterval(const Value: Integer);
begin
  // Do something here
  FAutoSaveInterval := Value;
end;

procedure TIdeEnhancements.SetSaveDfmAsTxt(const Value: Boolean);
begin
  FDfm2TxtHelper.Active := Value;
end;

{ --- Window menu enhancement --- }

procedure TIdeEnhancements.InstallWindowMenu;
var
  Menu: TMenuItem;
  Idx: Integer;
  MainForm: TCustomForm;
begin
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
  begin
    Menu := TMenuItem(MainForm.FindComponent('HelpMenu'));
    if Menu <> nil then
      Idx := Menu.MenuIndex
    else
      Idx := 10;

    InsertWindowMenu(Idx);
  end;
end;

procedure TIdeEnhancements.RemoveWindowMenu;
begin
  FreeWindowMenu;
end;

{$IFDEF VER120}

procedure TIdeEnhancements.SetShowAttachToMenuItem(AShowAttachToMenuItem: Boolean);
begin
  if not (AShowAttachToMenuItem = FShowAttachToMenuItem) then
    with TRegistry.Create do
    try
      Assert(ConfigInfo <> nil);

      if OpenKey(ConfigInfo.RegKey + '\Debugging', True) then
      begin
        if AShowAttachToMenuItem then
          WriteString('Enable Attach Menu', '1')
        else
          WriteString('Enable Attach Menu', '0');
        FShowAttachToMenuItem := AShowAttachToMenuItem;
      end;
    finally
      Free;
    end;
end;
{$ENDIF VER120}

procedure TIdeEnhancements.InsertWindowMenu(idx: Integer);
resourcestring
  SWindowMenuCaption = 'Wi&ndows';
  SWindowMenuDescription = 'Open Windows';
var
  MainMenu: TIMainMenuIntf;
  Temp: TIMenuItemIntf;
begin
  MainMenu := ToolServices.GetMainMenu;
  try
    Temp := MainMenu.GetMenuItems;
    try
      FWindowMenuItem := Temp.InsertItem(idx, SWindowMenuCaption, 'GX_MyWindows',
        SWindowMenuDescription, 0, 0, 0, [mfVisible, mfEnabled], WindowClick);
    finally
      Temp.Free;
    end;
  finally
    MainMenu.Free;
  end;
end;

procedure TIdeEnhancements.ClearWindowItems;
var
  i: Integer;
begin
  Assert(FWindowMenuList <> nil, 'FWindowMenuList is nil in TIdeEnhancements.ClearWindowItems');

  for i := 0 to FWindowMenuList.Count - 1 do
    TIMenuItemIntf(FWindowMenuList.Items[i]).Free;
  FWindowMenuList.Clear;

  Assert(FWindowList <> nil);
  FWindowList.Clear;
end;

procedure TIdeEnhancements.FreeWindowMenu;
begin
  // Sometimes on shutdown in D4 the menu item is already freed
  // and freeing again causes bad AVs.  We need a better fix, but...
  if csDestroying in Application.ComponentState then
    FWindowMenuItem := nil
  else
  begin
    ClearWindowItems;
    FWindowMenuItem.Free;
    FWindowMenuItem := nil;
  end;
end;

{$IFNDEF GX_VER120_up}

procedure TIdeEnhancements.WindowClick(Sender: TIMenuItemIntF);
var
  Menu: TIMenuItemIntf;
  count: Integer;
  j: Integer;
  MainForm: TCustomForm;

  procedure LoadMenuItems;

    function IsAcceptableForm(AForm: TForm): Boolean;
    begin
      Assert(MainForm <> nil);

      Result := (AForm <> MainForm) and
        (AForm.Visible) and
        (AForm.Height > 0) and
        (AForm.Width > 0) and
        (Trim(AForm.Caption) <> '') and
        (IsWindowVisible(AForm.Handle));
    end;

  var
    i: Integer;
    TempForm: TForm;
  begin
    { Now add new options }
    for i := 0 to Screen.FormCount - 1 do
    begin
      TempForm := Screen.Forms[i];
      if IsAcceptableForm(TempForm) then
      begin
        if Count >= FWindowMenuList.Count then
        begin
          Menu := FWindowMenuItem.InsertItem(-1, TempForm.Caption, '', '',
            0, 0, 0,
            [mfVisible, mfEnabled], GotoWindowClick);
          if Menu <> nil then
            FWindowMenuList.Add(Menu);
        end
        else
        begin
          Menu := TIMenuItemIntf(FWindowMenuList.Items[Count]);
          Menu.SetCaption(Trim(TempForm.Caption));
          Menu.SetFlags([mfVisible], [mfVisible]);
        end;

        FWindowList.Add(TempForm);
        Inc(Count);
      end;
    end;
  end;

begin
  MainForm := GetIdeMainForm;
  if MainForm = nil then
    Exit;

  // Delphi 3 calls the OnClick of top-level menu items in a timer (why?)
  if not MainForm.Focused then
    Exit;

  FWindowList.Clear;
  Count := 0;
  LoadMenuItems;

  for j := Count to FWindowMenuList.Count - 1 do
  begin
    Menu := TIMenuItemIntf(FWindowMenuList.Items[j]);
    Menu.SetFlags([mfVisible], []);
  end;

  // Check the current form
  MainForm := Screen.ActiveForm;
  if (MainForm = Application.MainForm) and (Screen.FormCount > 1) then
    MainForm := Screen.Forms[1];

  for j := 0 to FWindowList.Count - 1 do
  begin
    if TCustomForm(FWindowList[j]) = MainForm then
    begin
      TIMenuItemIntf(FWindowMenuList[j]).SetFlags([mfChecked], [mfChecked]);
      Break;
    end;
  end;

  for j := 0 to Min(FWindowMenuItem.GetItemCount - 1, 15) do
  begin
    Menu := FWindowMenuItem.GetItem(j);
    try
      Menu.SetCaption(Format('&%x %s', [j, Menu.GetCaption]));
    finally
      Menu.Free;
    end;
  end;
end;

{$ELSE}

procedure TIdeEnhancements.WindowClick(Sender: TIMenuItemIntF);
var
  CurrentForm: TCustomForm;

  procedure LoadMenuItems;

    function IsAcceptableForm(AForm: TForm): Boolean;
    begin
      Assert(CurrentForm <> nil);

      Result := (AForm <> CurrentForm) and
        (AForm.Visible) and
        (AForm.Height > 0) and
        (AForm.Width > 0) and
        (Trim(AForm.Caption) <> '') and
  {$IFDEF GX_VER120_up}
      (AForm.Floating) and
  {$ENDIF GX_VER120_up}
      (IsWindowVisible(AForm.Handle));
    end;

  var
    i: Integer;
    TempForm: TForm;
    Menu: TIMenuItemIntf;
  begin
    for i := 0 to Screen.FormCount - 1 do
    begin
      TempForm := Screen.Forms[i];
      if IsAcceptableForm(TempForm) then
      begin
        Menu := FWindowMenuItem.InsertItem(-1, Format('&%x %s', [FWindowMenuList.Count + 1, TempForm.Caption]), '', '',
          0, 0, 0, [mfVisible, mfEnabled], GotoWindowClick);
        if Menu <> nil then
          FWindowMenuList.Add(Menu);

        FWindowList.Add(TempForm);
      end;
    end;
  end;

var
  j: Integer;
begin
  CurrentForm := GetIdeMainForm;
  if CurrentForm = nil then
    Exit;
  ClearWindowItems;
  LoadMenuItems;

  CurrentForm := Screen.ActiveForm;
  if (CurrentForm = Application.MainForm) and (Screen.FormCount > 1) then
    CurrentForm := Screen.Forms[1];

  for j := 0 to FWindowList.Count - 1 do
  begin
    if TCustomForm(FWindowList[j]) = CurrentForm then
    begin
      TIMenuItemIntf(FWindowMenuList[j]).SetFlags([mfChecked], [mfChecked]);
      Break;
    end;
  end;
end;
{$ENDIF GX_VER120_up}

procedure TIdeEnhancements.GotoWindowClick(Sender: TIMenuItemIntf);
var
  i: Integer;
  CaptionString: string;
  CurrentForm: TForm;
begin
  for i := 0 to FWindowList.Count - 1 do
  begin
    CaptionString := Sender.GetCaption;
    if Length(CaptionString) > 2 then
      if CaptionString[1] = '&' then
        CaptionString := Trim(Copy(CaptionString, 3, Length(CaptionString)));

    CurrentForm := TForm(FWindowList.Items[i]);
{$IFOPT D+}SendDebug('Comparing Window Captions: ' + CaptionString + ' and ' + CurrentForm.Caption + ' ' + IntToStr(i)); {$ENDIF}
    if CompareText(CurrentForm.Caption, CaptionString) = 0 then
    begin
{$IFOPT D+}SendDebug('Focusing IDE window: ' + CaptionString); {$ENDIF}
      CurrentForm.Show;

      if CurrentForm.WindowState = wsMinimized then
        CurrentForm.WindowState := wsNormal;

      CurrentForm.SetFocus;
      CurrentForm.BringToFront;
{$IFOPT D+}SendDebug('Focused IDE window: ' + CaptionString); {$ENDIF}
      Break;
    end;
  end;
end;

{$IFDEF VER100}

procedure TIdeEnhancements.ShowMenuItem(const MenuName: string; Visible: Boolean);
var
  MainForm: TCustomForm;
  MenuItem: TMenuItem;
begin
  MainForm := GetIdeMainForm;
  if MainForm = nil then
    Exit;

  MenuItem := MainForm.FindComponent(MenuName) as TMenuItem;
  if MenuItem <> nil then
    MenuItem.Visible := Visible;
end;
{$ENDIF VER100}

{$IFDEF VER100}

procedure TIdeEnhancements.InstallCpuMenuItem;
begin
  ShowMenuItem('ViewCPUItem', True);
end;
{$ENDIF VER100}

{$IFDEF VER100}

procedure TIdeEnhancements.RemoveCpuMenuItem;
begin
  ShowMenuItem('ViewCPUItem', False);
end;
{$ENDIF VER100}

{$IFDEF VER100}

procedure TIdeEnhancements.InstallWinApiMenuItem;
begin
  ShowMenuItem('HelpAPIItem', True);
end;
{$ENDIF VER100}

{$IFDEF VER100}

procedure TIdeEnhancements.RemoveWinApiMenuItem;
begin
  ShowMenuItem('HelpAPIItem', False);
end;
{$ENDIF VER100}

{$IFDEF GX_MultiLinePaletteSubclass}

procedure TIdeEnhancements.SetCPMultiLine(Value: Boolean);
begin
  // It is not safe to remove Delphi 3 multi-line tab support at runtime.
  // This is because WndProc sub-classing is extremely sensitive to
  // changes in (un-)hooking order.
  // Later we might want to add code that "dynamically" turns
  // off the multi-line feature by simply removing the calculations
  // and leaving in the WndProc sub-class.
  FCPMultiLine := Value;
end;
{$ELSE}

procedure TIdeEnhancements.SetCPMultiLine(Value: Boolean);
var
  CPTabControl: TTabControl;
begin
{$IFOPT D+}SendDebug('Setting multiline palette to ' + BooleanText(Value)); {$ENDIF}
  if FCPMultiLine <> Value then
  begin
    FCPMultiLine := Value;
    CPTabControl := GetComponentPaletteTabControl;
    if CPTabControl = nil then
    begin
{$IFOPT D+}SendDebug('Unable to reset OldCPResizeHandler (no tab control)'); {$ENDIF}
      Exit;
    end;
    if FCPMultiLine then
      InstallMultiLineComponentTabs
    else
      RemoveMultiLineComponentTabs;
  end;
end;
{$ENDIF GX_MultiLinePaletteSubclass}

procedure TIdeEnhancements.SetCPFlatButtons(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPFlatButtons <> Value then
  begin
    FCPFlatButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;
{$IFDEF GX_VER120_up} // Delphi, BCB 4
    if CPAsButtons then
    begin
      if FCPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end;
{$ENDIF GX_VER120_up}
  end;
end;

procedure TIdeEnhancements.SetCPAsButtons(Value: Boolean);
var
  TabControl: TTabControl;
{$IFNDEF GX_VER120_up}
  Style: Cardinal;
{$ENDIF  GX_VER120_up}
begin
  if FCPAsButtons <> Value then
  begin
    FCPAsButtons := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;
{$IFOPT D+}SendDebug('Removing CP Buttons'); {$ENDIF}
{$IFDEF GX_VER120_up} // Delphi, BCB 4
    if Value then
    begin
      if CPFlatButtons then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end
    else
      TabControl.Style := tsTabs;
{$ELSE}
    // --> Delphi 3, BCB3
    if TabControl.HandleAllocated then
    begin
      Style := GetWindowLong(TabControl.Handle, GWL_STYLE);
      if Value then
        Style := Style or TCS_BUTTONS
      else
        Style := Style and not TCS_BUTTONS;
      SetWindowLong(TabControl.Handle, GWL_STYLE, Style);
    end;
{$ENDIF GX_VER120_up}
  end;
end;

procedure TIdeEnhancements.SetCPRaggedRight(const Value: Boolean);
var
  TabControl: TTabControl;
  {$IFNDEF GX_VER120_up}
  Style: Integer;
  {$ENDIF}

begin
  if FCPRaggedRight <> Value then
  begin
    FCPRaggedRight := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    {$IFDEF GX_VER120_up} // Delphi, BCB 4
    TabControl.RaggedRight := FCPRaggedRight;
    {$ELSE}
    // --> Delphi 3, BCB3
    if TabControl.HandleAllocated then
    begin
      Style := GetWindowLong(TabControl.Handle, GWL_STYLE);
      if Value then
        Style := Style or TCS_RAGGEDRIGHT
      else
        Style := Style and not TCS_RAGGEDRIGHT;
      SetWindowLong(TabControl.Handle, GWL_STYLE, Style);
    end;
    {$ENDIF GX_VER120_up}
  end;
end;

procedure TIdeEnhancements.SetCPScrollOpposite(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if FCPScrollOpposite <> Value then
  begin
    FCPScrollOpposite := Value;
    TabControl := GetComponentPaletteTabControl;
    if TabControl = nil then
      Exit;

    TabControl.ScrollOpposite := FCPScrollOpposite;
  end;
end;

procedure TIdeEnhancements.SetCPTabsInPopupAlphaSort(Value: Boolean);
begin
  if FCPTabsInPopupAlphaSort <> Value then
    FCPTabsInPopupAlphaSort := Value;
end;

procedure TIdeEnhancements.SetCPTabsInPopup(Value: Boolean);
var
  CPPopupMenu: TPopupMenu;
begin
  if FCPTabsInPopup <> Value then
  begin
    FCPTabsInPopup := Value;
    CPPopupMenu := GetCPPopupMenu;
    if CPPopupMenu = nil then Exit;
    if Value then
    begin
      FOldCPPopupEvent := CPPopupMenu.OnPopup;
      CPPopupMenu.OnPopup := AddTabsToPopup;
    end
    else
      CPPopupMenu.OnPopup := FOldCPPopupEvent;
  end;
end;

procedure TIdeEnhancements.SetOIFont(Value: TFont);
var
  OIForm: TCustomForm;
begin
  OIForm := GetObjInspForm;
  if OIForm <> nil then
  begin
    if FOldOIFont = nil then
    begin
      FOldOIFont := TFont.Create;
      FOldOIFont.Assign(OIForm.Font);
    end;
    OIForm.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetOIFontEnabled(Value: Boolean);
begin
  if FOIFontEnabled <> Value then
  begin
    FOIFontEnabled := Value;
    if Value then
      SetOIFont(OIFont)
    else
      if FOldOIFont <> nil then
      begin
        SetOIFont(FOldOIFont);
        FOldOIFont.Free;
        FOldOIFont := nil;
      end;
  end;
end;

procedure TIdeEnhancements.OIFontChange(Sender: TObject);
begin
{$IFOPT D+}SendDebug('OI font changed'); {$ENDIF}
  if OIFontEnabled then
    SetOIFont(OIFont);
end;

procedure TIdeEnhancements.SetCPFont(Value: TFont);
var
  CPTabControl: TTabControl;
begin
  CPTabControl := GetComponentPaletteTabControl;
  if CPTabControl <> nil then
  begin
    if FOldCPFont = nil then
    begin
      FOldCPFont := TFont.Create;
      FOldCPFont.Assign(CPTabControl.Font);
    end;
    CPTabControl.Font.Assign(Value)
  end;
end;

procedure TIdeEnhancements.SetCPFontEnabled(Value: Boolean);
begin
  if FCPFontEnabled <> Value then
  begin
    FCPFontEnabled := Value;
    if Value then
      SetCPFont(CPFont)
    else
      if FOldCPFont <> nil then
      begin
        SetCPFont(FOldCPFont);
        FOldCPFont.Free;
        FOldCPFont := nil;
      end;
  end;
end;

procedure TIdeEnhancements.CPFontChange(Sender: TObject);
begin
{$IFOPT D+}SendDebug('CP font changed'); {$ENDIF}
  if CPFontEnabled then
    SetCPFont(CPFont);
end;

procedure TIdeEnhancements.SetShowWindowsMenu(AShowWindowsMenu: Boolean);
begin
  if FShowWindowsMenu <> AShowWindowsMenu then
  begin
    FShowWindowsMenu := AShowWindowsMenu;
    if FShowWindowsMenu then
      InstallWindowMenu
    else
      RemoveWindowMenu;
  end;
end;

{$IFDEF VER100}

procedure TIdeEnhancements.SetShowCpuMenuItem(AShowCpuMenuItem: Boolean);
begin
  if FShowCpuMenuItem <> AShowCpuMenuItem then
  begin
    FShowCpuMenuItem := AShowCpuMenuItem;
    if FShowCpuMenuItem then
      InstallCpuMenuItem
    else
      RemoveCpuMenuItem;
  end;
end;
{$ENDIF VER100}

{$IFDEF VER100}

procedure TIdeEnhancements.SetShowApiHelpMenuItem(AShowApiHelpMenuItem: Boolean);
begin
  if FShowApiHelpMenuItem <> AShowApiHelpMenuItem then
  begin
    FShowApiHelpMenuItem := AShowApiHelpMenuItem;
    if FShowApiHelpMenuItem then
      InstallWinApiMenuItem
    else
      RemoveWinApiMenuItem;
  end;
end;
{$ENDIF VER100}

procedure TIdeEnhancements.InstallMultiLineComponentTabs;
begin
  if GetComponentPaletteTabControl = nil then
    Exit;

  if FMultiLineTabManager = nil then
    FMultiLineTabManager := TMultiLineTabManager.Create(GetIdeMainForm);
end;

procedure TIdeEnhancements.RemoveMultiLineComponentTabs;
begin
  FMultiLineTabManager.Free;
  FMultiLineTabManager := nil;
end;

{$IFDEF GX_VER120_up}

procedure TIdeEnhancements.SetDefaultMultilineTabDockHost(
  const Value: Boolean);
begin
  GX_MultilineHost.DefaultToMultiLine := Value;
end;

function TIdeEnhancements.GetDefaultMultilineTabDockHost: Boolean;
begin
  Result := GX_MultilineHost.DefaultToMultiLine;
end;

function TIdeEnhancements.GetMultilineTabDockHost: Boolean;
begin
  Result := (FMultiLineTabDockHostManager <> nil);
end;

procedure TIdeEnhancements.SetMultilineTabDockHost(const Value: Boolean);
begin
  if Value then
    InstallMultiLineHostTabs
  else
    RemoveMultiLineHostTabs;
end;

procedure TIdeEnhancements.InstallMultiLineHostTabs;
begin
  if FMultiLineTabDockHostManager = nil then
    FMultiLineTabDockHostManager := TGxMultiLineTabDockHostsManager.Create;
end;

procedure TIdeEnhancements.RemoveMultiLineHostTabs;
begin
  FMultiLineTabDockHostManager.Free;
  FMultiLineTabDockHostManager := nil;
end;
{$ENDIF GX_VER120_up}

// Internal Singleton management code

var
  PrivateIdeEnhancements: TIdeEnhancements = nil;
  CanCreate: Boolean = True;

function IdeEnhancements: TIdeEnhancements;
begin
  Assert(CanCreate);

  if PrivateIdeEnhancements = nil then
    PrivateIdeEnhancements := TIdeEnhancements.Create;

  Result := PrivateIdeEnhancements;
end;

procedure FreeIdeEnhancements;
begin
  CanCreate := False;

  PrivateIdeEnhancements.Free;
  PrivateIdeEnhancements := nil;
end;

function TIdeEnhancements.GetSaveDfmAsTxt: Boolean;
begin
  Result := FDfm2TxtHelper.Active;
end;

initialization
  {$IFOPT D+} SendDebug('Initializing IDE enhancements unit'); {$ENDIF D+}

finalization
  FreeIdeEnhancements;
end.

