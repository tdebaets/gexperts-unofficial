unit GX_ToolBar;

{$I GX_CondDefine.Inc}

{$IFDEF GX_EII}

{$IFDEF VER110}
  {$D-}
{$ENDIF VER110}

interface

uses
  Classes, SysUtils, Windows, Controls,
  ExtCtrls, Menus,
  GX_EditorEnhancements;

const
  ButtonSize = 24;
  ButtonSpacing = 2;

type
  TPopupType = (popUnit, popForm);

  TGXToolBar = class(TPanel)
  private
    FUnitList: TStringList;
    FToolBarConfigPopup: TPopupMenu;
    FBtnPopup: TPopupMenu;
    FCompPopup: TPopupMenu;
    FEditorLocalSeparatorMenu: TMenuItem;
    FEditorLocalOptionMenu: TMenuItem;
    FEditMenus: array[0..3] of TMenuItem;

    procedure CreateToolBarConfigurationMenu;
    // OnClick handlers for two toolbar configuration
    // menu items we add
    procedure ConfigureToolBarButtonsClick(Sender: TObject);
    procedure GxEditorOptionsClick(Sender: TObject);

    procedure CreateEditorLocalMenuAdditions;
    procedure FreeEditorLocalMenuAdditions;

    procedure AddEditMenus;
    procedure RemoveEditMenus;

    procedure ApplyEditorTabControlStyles;
    procedure ClearEditorTabControlStyles;

    procedure ClearUnitList;
  private
    FEditMgr: TEditorEnhancements;
    procedure ExecuteKeyboardExpert(Sender: TObject);

    procedure ShowPopup(Sender: TObject; PopupType: TPopupType);
    procedure ShowCompPopup(Sender: TObject);

    procedure SpecialButtonClick(Sender: TObject);
    procedure GotoFileClick(Sender: TObject);
    procedure FocusInspectorClick(Sender: TObject);
    //procedure ButtonForGExpertsMenuClick(Sender: TObject);
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateManaged(AOwner: TComponent; EditorEnh: TEditorEnhancements);
    destructor Destroy; override;

    procedure CreateToolBarButtons;
    procedure FreeToolBarButtons;

    procedure SetEditorControls;
  end;


implementation

uses
  Buttons, Dialogs, CommCtrl, StdCtrls,
  Forms, Messages, ComCtrls,
  {$IFDEF GX_HasActionSupport} ActnList, {$ENDIF GX_HasActionSupport}
  {$IFDEF GX_UseNativeToolsApi} ToolsAPI, {$ENDIF GX_UseNativeToolsApi}
  ToolIntf, ExptIntf, EditIntf,
  EIManager, EIPanel, EINotifiers,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  GX_ToolBarButtons, GX_GenFunc, GX_EditReader, GX_Actions;

{$IFDEF GX_BCB}
var
  SelectComponent: procedure (Name: string); register;
{$ENDIF GX_BCB}

{ TGXToolBar }

constructor TGXToolBar.Create(AOwner: TComponent);
begin
  Assert(False);
end;

constructor TGXToolBar.CreateManaged(AOwner: TComponent; EditorEnh: TEditorEnhancements);
begin
  inherited Create(AOwner);

  FEditMgr := EditorEnh;

  Align := alTop;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Height := 30;

  FUnitList := TStringList.Create;

  FToolBarConfigPopup := TPopupMenu.Create(Self);
  CreateToolBarConfigurationMenu;

  FBtnPopup := TPopupMenu.Create(Self);
  FCompPopup := TPopupMenu.Create(Self);

  ShowHint := True;
  CreateToolBarButtons;
  CreateEditorLocalMenuAdditions;
end;

destructor TGXToolBar.Destroy;
begin
  {$IFOPT D+}SendDebug('ToolBar: Destroying'); {$ENDIF}
  FreeEditorLocalMenuAdditions;
  {$IFOPT D+}SendDebug('ToolBar: Freeing toolbar buttons'); {$ENDIF}
  FreeToolBarButtons;

  {$IFOPT D+}SendDebug('ToolBar: Clearing editor tab control styles'); {$ENDIF}
  ClearEditorTabControlStyles;

  FToolBarConfigPopup.Free;
  FToolBarConfigPopup := nil;

  FBtnPopup.Free;
  FBtnPopup := nil;

  FCompPopup.Free;
  FCompPopup := nil;

  {$IFOPT D+}SendDebug('ToolBar: Clearing unit list'); {$ENDIF}
  ClearUnitList;
  FUnitList.Free;
  FUnitList := nil;

  {$IFOPT D+}SendDebug('ToolBar: Calling inherited destructor'); {$ENDIF}
  inherited Destroy;
  {$IFOPT D+}SendDebug('ToolBar: Destroyed'); {$ENDIF}
end;

procedure TGXToolBar.Notification(Component: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  if Operation = opRemove then
  begin
    if Component = FEditorLocalSeparatorMenu then
      FEditorLocalSeparatorMenu := nil
    else
    if Component = FEditorLocalOptionMenu then
      FEditorLocalOptionMenu := nil
    else
    begin
      for i := Low(FEditMenus) to High(FEditMenus) do
        if Component = FEditMenus[i] then
          FEditMenus[i] := nil;
    end;
  end;

  inherited Notification(Component, Operation);
end;

procedure TGXToolBar.AddEditMenus;
var
  EditorLocalMenu: TPopupMenu;
  AppBuilder: TCustomForm;
{$IFDEF GX_HasActionSupport}
  Action: TAction;
{$ELSE}
  MenuItem: TMenuItem;
{$ENDIF GX_HasActionSupport}
  s: Integer;
resourcestring
  SGxCutMenu = 'Cu&t';
  SGxCopyMenu = '&Copy';
  SGxPasteMenu = '&Paste';
begin
  EditorLocalMenu := Owner.FindComponent('EditorLocalMenu') as TPopupMenu;
  if EditorLocalMenu = nil then
    Exit;

  AppBuilder := GetIdeMainForm;
  if AppBuilder = nil then
    Exit;

  Assert(FEditMenus[3] = nil);
  FEditMenus[3] := TMenuItem.Create(Self);
  FEditMenus[3].Caption := '-';
  FEditMenus[3].Name := 'GExperts_LocalSeparator';
  if FEditMgr.EditorContextMenusAtBottom then
  begin
    // Separator menu item
    EditorLocalMenu.Items.Add(FEditMenus[3]);
  end;

  s := 0;

  // Cut menu item
  Assert(FEditMenus[0] = nil);
  FEditMenus[0] := TMenuItem.Create(Self);
  FEditMenus[0].Name := 'GExperts_LocalCut';
{$IFDEF GX_HasActionSupport}
  Action := AppBuilder.FindComponent('EditCutCommand') as TAction;
  if Action <> nil then
    FEditMenus[0].Action := Action;
  FEditMenus[0].Action := nil;
  FEditMenus[0].Enabled := True;
{$ELSE}
  FEditMenus[0].Caption := SGxCutMenu;
  MenuItem := AppBuilder.FindComponent('EditCutItem') as TMenuItem;
  if MenuItem <> nil then
    FEditMenus[0].OnClick := MenuItem.OnClick;
{$ENDIF GX_HasActionSupport}

  if FEditMgr.EditorContextMenusAtBottom then
    EditorLocalMenu.Items.Add(FEditMenus[0])
  else
    EditorLocalMenu.Items.Insert(s, FEditMenus[0]);
  Inc(s);

  // Copy menu item
  Assert(FEditMenus[1] = nil);
  FEditMenus[1] := TMenuItem.Create(Self);
  FEditMenus[1].Name := 'GExperts_LocalCopy';

{$IFDEF GX_HasActionSupport}
  Action := AppBuilder.FindComponent('EditCopyCommand') as TAction;
  if Action <> nil then
    FEditMenus[1].Action := Action;
  FEditMenus[1].Action := nil;
  FEditMenus[1].Enabled := True;
{$ELSE}
  FEditMenus[1].Caption := SGxCopyMenu;
  MenuItem := AppBuilder.FindComponent('EditCopyItem') as TMenuItem;
  if MenuItem <> nil then
    FEditMenus[1].OnClick := MenuItem.OnClick;
{$ENDIF GX_HasActionSupport}

  if FEditMgr.EditorContextMenusAtBottom then
    EditorLocalMenu.Items.Add(FEditMenus[1])
  else
    EditorLocalMenu.Items.Insert(s, FEditMenus[1]);
  Inc(s);

  // Paste menu item
  Assert(FEditMenus[2] = nil);
  FEditMenus[2] := TMenuItem.Create(Self);
  FEditMenus[2].Name := 'GExperts_LocalPaste';

{$IFDEF GX_HasActionSupport}
  Action := AppBuilder.FindComponent('EditPasteCommand') as TAction;
  if Action <> nil then
    FEditMenus[2].Action := Action;
  FEditMenus[2].Action := nil;
  FEditMenus[2].Enabled := True;
{$ELSE}
  MenuItem := AppBuilder.FindComponent('EditPasteItem') as TMenuItem;
  FEditMenus[2].Caption := SGxPasteMenu;
  if MenuItem <> nil then
    FEditMenus[2].OnClick := MenuItem.OnClick;
{$ENDIF GX_HasActionSupport}

  if FEditMgr.EditorContextMenusAtBottom then
    EditorLocalMenu.Items.Add(FEditMenus[2])
  else
    EditorLocalMenu.Items.Insert(s, FEditMenus[2]);

  if not FEditMgr.EditorContextMenusAtBottom then
  begin
    Inc(s);
    EditorLocalMenu.Items.Insert(s, FEditMenus[3]);
  end;
end;

procedure TGXToolBar.RemoveEditMenus;
var
  EditorLocalMenu: TPopupMenu;
  i: Integer;
begin
  EditorLocalMenu := Owner.FindComponent('EditorLocalMenu') as TPopupMenu;
  if EditorLocalMenu = nil then
    Exit;

  for i := Low(FEditMenus) to High(FEditMenus) do
  begin
    if FEditMenus[i] <> nil then
    begin
      EditorLocalMenu.Items.Delete(FEditMenus[i].MenuIndex);
      FEditMenus[i].Free;
      FEditMenus[i] := nil;
    end;
  end;
end;

procedure TGXToolBar.CreateEditorLocalMenuAdditions;
resourcestring
  SGxLocalEditorCaption = '&GExperts Editor Options...';
var
  Menu: TMenuItem;
  Popup: TPopupMenu;
begin
  try
    Popup := Owner.FindComponent('EditorLocalMenu') as TPopupMenu;
    if Popup <> nil then
    begin
      Menu := TMenuItem.Create(Self);
      Menu.FreeNotification(Self);
      Popup.Items.Add(Menu);

      Menu.Caption := '-';
      Menu.Name := 'GX_Separator';
      FEditorLocalSeparatorMenu := Menu;

      Menu := TMenuItem.Create(Self);
      Menu.FreeNotification(Self);
      Popup.Items.Add(Menu);

      Menu.Caption := SGxLocalEditorCaption;
      Menu.Name := 'GX_EditOptions';
      Menu.OnClick := GxEditorOptionsClick;

      FEditorLocalOptionMenu := Menu;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TGXToolBar.FreeEditorLocalMenuAdditions;
var
  Popup: TPopupMenu;
begin
  try
    Popup := Owner.FindComponent('EditorLocalMenu') as TPopupMenu;
    if Popup <> nil then
    begin
      if FEditorLocalOptionMenu <> nil then
        Popup.Items.Delete(FEditorLocalOptionMenu.MenuIndex)
      else
      begin
        {$IFOPT D+}SendDebug('ToolBar: for some reason FEditorLocalOptionMenu = nil??');{$ENDIF D+}
      end;

      if FEditorLocalSeparatorMenu <> nil then
        Popup.Items.Delete(FEditorLocalSeparatorMenu.MenuIndex)
      else
      begin
        {$IFOPT D+}SendDebug('ToolBar: for some reason FEditorLocalSeparatorMenu = nil??');{$ENDIF D+}
      end;
    end;

    FEditorLocalSeparatorMenu.Free;
    FEditorLocalSeparatorMenu := nil;

    FEditorLocalOptionMenu.Free;
    FEditorLocalOptionMenu := nil;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TGXToolBar.CreateToolBarConfigurationMenu;
resourcestring
  SConfigureToolBar = 'Configure ToolBar...';
var
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem.Create(Self);
  FToolBarConfigPopup.Items.Add(MenuItem);
  MenuItem.Caption := SConfigureToolBar;
  MenuItem.OnClick := ConfigureToolBarButtonsClick;

  PopupMenu := FToolBarConfigPopup;
end;

procedure TGXToolBar.GxEditorOptionsClick(Sender: TObject);
begin
  if FEditMgr <> nil then
    FEditMgr.ShowConfigurationDialog('tshEditor');
end;

procedure TGXToolBar.ConfigureToolBarButtonsClick(Sender: TObject);
begin
  if FEditMgr <> nil then
    FEditMgr.ShowConfigurationDialog('tshToolBar');
end;

procedure TGXToolBar.SetEditorControls;
begin
  {$IFOPT D+}SendDebug('ToolBar: SetEditorControls'); {$ENDIF}

  // All these items need to be called from this
  // method, since they potentially cover multiple
  // editor windows.
  ApplyEditorTabControlStyles;

  if FEditMgr <> nil then
  begin
    if (FEditMenus[0] <> nil) then
      RemoveEditMenus;
    if (FEditMgr.EditorContextMenus and (FEditMenus[0] = nil)) then
      AddEditMenus;
  end;
end;

procedure TGXToolBar.ApplyEditorTabControlStyles;

  procedure SetStyle(TabControl: TTabControl; Enable: Boolean; NewStyle: SmallInt);
  var
    Style: LongInt;
  begin
    Style := GetWindowLong(TabControl.Handle, GWL_STYLE);
    if Enable then
      Style := Style or NewStyle
    else
      Style := Style and not NewStyle;
    SetWindowLong(TabControl.Handle, GWL_STYLE, Style);
  end;

var
  TabControl: TTabControl;
begin
  {$IFOPT D+}SendDebug('Applying editor tab control settings');{$ENDIF D+}
  if FEditMgr = nil then
    Exit;
  TabControl := Owner.FindComponent('TabControl') as TTabControl;
  if TabControl <> nil then
  begin
    SetStyle(TabControl, FEditMgr.MultiLine, TCS_MULTILINE);
    {$IFNDEF GX_VER120_up}
    SetStyle(TabControl, FEditMgr.HotTrack, TCS_HOTTRACK);
    {$ELSE}
    TabControl.HotTrack := FEditMgr.HotTrack;
    {$ENDIF GX_VER120_up}

    {$IFDEF GX_VER120_up}
    if FEditMgr.Buttons then
    begin
      if FEditMgr.ButtonsFlat then
        TabControl.Style := tsFlatButtons
      else
        TabControl.Style := tsButtons;
    end
    else
      TabControl.Style := tsTabs;
    {$ELSE}
    SetStyle(TabControl, FEditMgr.Buttons, TCS_BUTTONS);
    {$ENDIF GX_VER120_up}
  end;
  {$IFOPT D+}SendDebug('Done applying editor tab control settings');{$ENDIF D+}
end;

procedure TGXToolBar.ClearEditorTabControlStyles;
var
  TabControl: TTabControl;
  Style: Longint;
begin
  TabControl := TTabControl(Owner.FindComponent('TabControl'));
  if TabControl <> nil then
  begin
    TabControl.MultiLine := False;
    TabControl.HotTrack := False;
    if TabControl.HandleAllocated then
    begin
      Style := GetWindowLong(TabControl.Handle, GWL_STYLE);
      if Style = 0 then
        RaiseLastWin32Error;

      Style := Style and not TCS_BUTTONS;

      // Set desired window style
      if SetWindowLong(TabControl.Handle, GWL_STYLE, Style) = 0 then
        RaiseLastWin32Error;
    end;
  end;
end;

procedure TGXToolBar.FreeToolBarButtons;
var
  i: Integer;
  TempComponent: TComponent;
begin
  i := ComponentCount - 1;
  while i >= 0 do
  begin
    TempComponent := Components[i];
    if (TempComponent is TSpeedButton) or
       (TempComponent is TBevel) then
    begin
      TempComponent.Free
    end;

    Dec(i);
  end;
end;

procedure TGXToolBar.CreateToolBarButtons;
resourcestring
  SNotRecognized = ' (found component was not recognized)';
  SNotInIde = ' (action not available in this IDE)';
var
  ToolBarAlign: TAlign;
  XPos: Integer;
  AppBuilder: TCustomForm;

  procedure CreateSeparatorToolBarButton;
  begin
    with TBevel.Create(Self) do
    begin
      Parent := Self;
      if ToolBarAlign in [alTop, alBottom] then
      begin
        Shape := bsLeftLine;
        SetBounds(XPos + 5, ButtonSpacing, 4, ButtonSize);
      end
      else
      begin
        Shape := bsTopLine;
        SetBounds(ButtonSpacing, XPos + 5, ButtonSize, 4);
      end;
    end;
    Inc(XPos, 11);
  end;

  procedure CreateSpeedButton(ButtonIndex: Integer);
  var
    ObjectName: string;
    CmpItem: TComponent;
    tmpCmp: TComponent;
  begin
    {$IFOPT D+}SendDebug('Creating button #'+IntToStr(ButtonIndex));{$ENDIF D+}
    with TSpeedButton.Create(Self) do
    begin
      Parent := Self;
      if ToolBarAlign in [alTop, alBottom] then
        SetBounds(XPos, ButtonSpacing, ButtonSize, ButtonSize)
      else
        SetBounds(ButtonSpacing, XPos, ButtonSize, ButtonSize);
      Inc(XPos, 25);

      Glyph.Handle := LoadBitmap(hInstance, PChar(GxButtonInfo[ButtonIndex].ResourceName));
      NumGlyphs := 2;

      Hint := GxButtonInfo[ButtonIndex].ButtonName;
      Flat := True;
      PopupMenu := FToolBarConfigPopup;
      Tag := ButtonIndex;

      ObjectName := GxButtonInfo[ButtonIndex].ObjectName;
      if ObjectName = 'SPECIAL' then
        OnClick := SpecialButtonClick
      else
      begin
        // First try to find the item in the AppBuilder object
        // (on the main form)
        CmpItem := AppBuilder.FindComponent(ObjectName);
        if CmpItem = nil then
        begin
          // Next attempt is directed at our owner ????
          //ObjectName := GxButtonInfo[ButtonIndex].ObjectName;
          CmpItem := Self.Owner.FindComponent(ObjectName);
          if CmpItem = nil then
          begin
            // Finally check whether we ourselves own that
            // component
            tmpCmp := AppBuilder.FindComponent('GExperts');
            if tmpCmp <> nil then
              CmpItem := tmpCmp.FindComponent(ObjectName);
            if CmpItem = nil then
              CmpItem := GXActionManager.GExpertsMenu.FindComponent(ObjectName);
          end;
        end;

        if CmpItem <> nil then
        begin
          if CmpItem is TMenuItem then
            OnClick := TMenuItem(CmpItem).OnClick
          {$IFDEF GX_HasActionSupport}
          else
          if CmpItem is TAction then
            OnClick := TAction(CmpItem).OnExecute
          {$ENDIF GX_HasActionSupport}
          else
          begin
            Enabled := False;
            Hint := Hint + SNotRecognized;
          end;
        end
        else
        begin
          // CmpItem is nil
          Enabled := False;
          Hint := Hint + SNotInIde;
        end;
      end;
    end;
  end;

var
  i, n: Integer;
begin
  {$IFOPT D+}SendDebug('Creating toolbar buttons');{$ENDIF D+}

  {$IFOPT D+}SendDebug('Freeing existing toolbar buttons');{$ENDIF D+}
  // First free any existing buttons
  FreeToolBarButtons;

  if Application.Terminated then
    Exit;

  AppBuilder := GetIdeMainForm;
  if (FEditMgr = nil) or (AppBuilder = nil) then
    Exit;

  // Get alignment of this toolbar
  if Self.Parent is TEIPanel then
    ToolBarAlign := TEIPanel(Parent).Align
  else
    ToolBarAlign := alTop;

  // Start creating buttons at position 2
  XPos := ButtonSpacing;

  {$IFOPT D+}SendDebug('Creating new toolbar buttons');{$ENDIF D+}
  // Now create new buttons
  for i := 0 to FEditMgr.ToolBarButtonsList.Count - 1 do
  begin
    n := Integer(FEditMgr.ToolBarButtonsList.Items[i]);
    if n = SeparatorMenuItemMarker then
      CreateSeparatorToolBarButton
    else
    begin
      // EB: Check for invalid numeric button IDs and change them to 0
      if (n > High(GXButtonInfo)) or (n < Low(GXButtonInfo)) then
        FEditMgr.ToolBarButtonsList.Items[i] := Pointer(Low(GXButtonInfo));
      CreateSpeedButton(n);
    end;
  end;
end;

(*
procedure TGXToolBar.ButtonForGExpertsMenuClick(Sender: TObject);
var
  st: string;
  subItem, gxItem: TMenuItem;
  Main: TMainMenu;
  i: Integer;
{$IFNDEF GX_VER120_up}
  AppBuilderObj: TComponent;
{$ENDIF GX_VER120_up}
begin
  {$IFDEF GX_VER120_up}
    Main := (BorlandIDEServices as INTAServices).MainMenu;
  {$ELSE}
    AppBuilderObj := GetIdeMainForm;
    Assert(AppBuilderObj <> nil, 'IDE AppBuilder object not found');

    Main := AppBuilderObj.FindComponent('MainMenu1') as TMainMenu;
    Assert(Main <> nil, 'IDE main menu not found');
  {$ENDIF GX_VER120_up}

  gxItem := nil;
  for i := 0 to Main.Items.Count - 1 do
  begin
    if Main.Items[i].Name = 'GExperts' then
    begin
      gxItem := Main.Items[i];
      Break;
    end;
  end;

  if gxItem <> nil then
  begin
    st := GxButtonInfo[(Sender as TSpeedButton).Tag].ObjectName;
    subItem := nil;
    for i := 0 to gxItem.Count - 1 do
    begin
      if gxItem.Items[i].Name = st then
      begin
        subItem := gxItem.Items[i];
        Break;
      end;
    end;

    if subItem <> nil then
      subItem.OnClick(Sender);
  end
  else
    MessageBeep(MB_ICONHAND);
end;
*)

procedure TGXToolBar.SpecialButtonClick(Sender: TObject);

  procedure ExecuteToolsOptionsDialog;
  var
    App: TCustomForm;
    Menu: TMenuItem;
  begin
    App := GetIdeMainForm;
    if App <> nil then
    begin
      Menu := TMenuItem(App.FindComponent('ToolsOptionsItem'));
      if Menu <> nil then
        Menu.OnClick(Menu);
    end;
  end;

{$IFDEF GX_UseNativeToolsApi}
  procedure ToggleBreakOnException;
  var
    EnvironmentOptions: IOTAEnvironmentOptions;
    State: Boolean;
  begin
    //! StH: FIXME
    // Broken at least in Delphi 4.03...
    // Value changes
    EnvironmentOptions := (BorlandIDEServices as IOTAServices).GetEnvironmentOptions;
    State := EnvironmentOptions.Values['BreakOnException'];
    State := not State;
    EnvironmentOptions.Values['BreakOnException'] := State;
  end;
{$ELSE}
  procedure ToggleBreakOnException;
  var
    cbBreakOnException: TCheckBox;
    OKButton: TButton;
  begin
    if MessageDlg('This may crash! Continue?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

    FEditMgr.HookedEnvDialogInstance := nil;

    FEditMgr.DoHandleEnvDialog := True;
    Screen.Cursor := crHourglass;
    try
      ExecuteToolsOptionsDialog;
      with FEditMgr.HookedEnvDialogInstance do
      begin
        cbBreakOnException := TCheckBox(FindComponent('cbBreakOnException'));
        if cbBreakOnException <> nil then
          cbBreakOnException.Checked := not cbBreakOnException.Checked;
        OKButton := TButton(FindComponent('OKButton'));
        if OKButton <> nil then
          PostMessage(OKButton.Handle, BM_CLICK, 0, 0);
        PostMessage(FEditMgr.HookedEnvDialogInstance.Handle, WM_CHAR, Ord(#13), 0);

        Application.ProcessMessages;
      end;
    finally
      FEditMgr.DoHandleEnvDialog := False;
      Screen.Cursor := crDefault;
    end;
  end;
{$ENDIF GX_UseNativeToolsApi}

begin
  case (Sender as TSpeedButton).Tag of
    3:       PostMessage(GetIdeMainForm.Handle, WM_COMMAND, Ord(#13), 0);
    50:      ShowCompPopup(Sender);
    55:      ShowPopup(Sender, popUnit);
    56:      ShowPopup(Sender, popForm);
    57..62:  ExecuteKeyboardExpert(Sender);  // These indexes are hardcoded in the editor experts!
  else
    {$IFOPT D+} SendDebugEx('Unrecognized button tag: '+IntToStr((Sender as TSpeedButton).Tag), mtError); {$ENDIF}
  end;
end;

procedure TGXToolBar.ExecuteKeyboardExpert(Sender: TObject);
begin
  if FEditMgr <> nil then
    FEditMgr.ExecuteExpertByNo((Sender as TSpeedButton).Tag);
end;

procedure TGXToolBar.ClearUnitList;
var
  i: Integer;
begin
  if Assigned(FUnitList) then
  begin
    for i := 0 to FUnitList.Count-1 do
      FUnitList.Objects[i].Free;
    FUnitList.Clear;
  end;
end;

procedure TGXToolBar.FocusInspectorClick(Sender: TObject);
var
  App: TCustomForm;
  Obj: TCustomForm;
begin
  {$IFDEF GX_BCB}
    Assert(@SelectComponent <> nil);
  {$ENDIF GX_BCB}
  // Compensate for AutoHotKeys.  '&' is not valid in Name anyway
  {$IFDEF GX_VER130_up}
  SelectComponent(StringReplace((Sender as TMenuItem).Caption, '&', '', [rfReplaceAll]));
  {$ELSE}
  SelectComponent((Sender as TMenuItem).Caption);
  {$ENDIF}

  App := GetIdeMainForm;
  if App <> nil then
  begin
    Obj := App.FindComponent('PropertyInspector') as TCustomForm;
    if Obj <> nil then
    begin
      Obj.Show;
      Obj.SetFocus;
    end;
  end;
end;


  function ListSortCompare(Item1, Item2: Pointer): Integer;
  begin
    if (TCompInfo(Item1).CompType < TCompInfo(Item2).CompType) then
      Result := -1
    else
    if (TCompInfo(Item1).CompType > TCompInfo(Item2).CompType) then
      Result := 1
    else
    begin
      // The types are equal, so we compare the names
      if (TCompInfo(Item1).CompName < TCompInfo(Item2).CompName) then
        Result := -1
      else
      if (TCompInfo(Item1).CompName > TCompInfo(Item2).CompName) then
        Result := 1
      else
        Result := 0;
    end;
  end;


procedure TGXToolBar.ShowCompPopup(Sender: TObject);
var
  n: Integer;

  procedure AddComponentsToPopup(CompInfo: TCompInfo);
  var
    i: Integer;
    TempMenu1: TMenuItem;
    TempMenu2: TMenuItem;
  begin
    for i := 0 to FCompPopup.Items.Count - 1 do
    begin
      if CompareText(CompInfo.CompType, FCompPopup.Items[i].Caption) = 0 then
      begin
        TempMenu1 := TMenuItem.Create(Self);
        TempMenu1.Caption := CompInfo.CompName;
        TempMenu1.OnClick := FocusInspectorClick;

        if (FCompPopup.Items[i].Count > 0) and
           (FCompPopup.Items[i].Count mod n = 0) then
        begin
          TempMenu1.Break := mbBarBreak;
        end;

        FCompPopup.Items[i].Add(TempMenu1);

        Exit;
      end;
    end;

    TempMenu2 := TMenuItem.Create(Self);
    TempMenu2.Caption := CompInfo.CompType;
    FCompPopup.Items.Add(TempMenu2);

    TempMenu1 := TMenuItem.Create(Self);
    TempMenu1.Caption := CompInfo.Compname;
    TempMenu1.OnClick := FocusInspectorClick;

    TempMenu2.Add(TempMenu1);
  end;

var
  i, cy: Integer;
  MenuPopupPosition: TPoint;
  List: TList;
  StringList: TStringList;
  CompInfo: TCompInfo;
  TempMenu: TMenuItem;

  SendingButton: TSpeedButton;
begin
  while FCompPopup.Items.Count > 0 do
    FCompPopup.Items[0].Free;

  cy := GetSystemMetrics(SM_CYMENU);
  n := (Screen.Height - 10) div cy;
  SendingButton := Sender as TSpeedbutton;
  MenuPopupPosition.X := SendingButton.Left;
  MenuPopupPosition.Y := SendingButton.Top + SendingButton.Height + 1;
  MenuPopupPosition := ClientToScreen(MenuPopupPosition);

  List := TList.Create;
  try
    StringList := TStringList.Create;
  try
      GetCompInfoList(StringList);
      for i := 0 to StringList.Count - 1 do
        List.Add(StringList.Objects[i]);
      List.Sort(ListSortCompare);
    finally
      StringList.Free;
    end;

    for i := 0 to List.Count - 1 do
    begin
      CompInfo := TCompInfo(List.Items[i]);
      AddComponentsToPopup(CompInfo);
    end;

    if List.Count = 0 then
    begin
      TempMenu := TMenuItem.Create(Self);
      TempMenu.Caption := 'None';
      TempMenu.Enabled := False;
      FCompPopup.Items.Add(TempMenu);
    end;
  finally
    for i := 0 to List.Count-1 do
      TCompInfo(List.Items[i]).Free;
    List.Free;
  end;

  FCompPopup.Popup(MenuPopupPosition.X, MenuPopupPosition.Y);
end;

procedure TGXToolBar.ShowPopup(Sender: TObject; PopupType: TPopupType);
const
  MinMenuLines = 5;
var
  Menu: TMenuItem;
  i, cy, n: Integer;
  UnitInfo: TUnitInfo;
  FileExtension: string;
  IsBrowsableFile: Boolean;
  MenuPopupPosition: TPoint;
  List: TStringList;
  SendingButton: TSpeedButton;
  MaxMenuTop: Integer;
begin
  ClearUnitList;
  GetUnitInfoList(FUnitList);

  while FBtnPopup.Items.Count > 0 do
    FBtnPopup.Items[0].Free;

  cy := GetSystemMetrics(SM_CYMENU);
  SendingButton := Sender as TSpeedbutton;
  MenuPopupPosition.X := SendingButton.Left;
  MenuPopupPosition.Y := SendingButton.Top + SendingButton.Height + 1;

  MaxMenuTop := Screen.Height - (cy * MinMenuLines) ;
  if ClientToScreen(MenuPopupPosition).Y > MaxMenuTop then
    MenuPopupPosition.Y := ScreenToClient(Point(0, MaxMenuTop)).Y;
  MenuPopupPosition := Self.ClientToScreen(MenuPopupPosition);

  List := TStringList.Create;
  try
    for i := 0 to FUnitList.Count - 1 do
    begin
      UnitInfo := TUnitInfo(FUnitList.Objects[i]);
      if (Trim(UnitInfo.FormName) <> '') or (PopupType <> popForm) then
      begin
        if PopupType = popUnit then
        begin
          IsBrowsableFile := True;

          FileExtension := ExtractUpperFileExt(UnitInfo.FileName);
          {$IFDEF GX_BCB}
            // Filter out the MAKE files as browsable files.
            if FileExtension = '.BPK' then
              IsBrowsableFile := False
            else if FileExtension = '.BPR' then
              IsBrowsableFile := False
            else if FileExtension = '.BPF' then
              IsBrowsableFile := False;
          {$ENDIF GX_BCB}

          // Filter out DSK and RES as browsable files;
          // This applies to Delphi and C++Builder.
          if IsBrowsableFile then
          begin
            if FileExtension = '.DSK' then
              IsBrowsableFile := False
            else if FileExtension = '.RES' then
              IsBrowsableFile := False;
          end;

          if IsBrowsableFile then
          begin
            // Do not add duplicate entries to the list;
            // C++Builder, for instance, has the main
            // CPP file (i.e. YourProject.CPP) twice.
            if List.IndexOf(UnitInfo.UnitName) = -1 then
              List.AddObject(UnitInfo.UnitName, Pointer(i));
          end;
        end
        else
          List.AddObject(UnitInfo.FormName, Pointer(i));
      end;
    end;

    List.Sorted := True;
    cy := GetSystemMetrics(SM_CYMENU);
    n := (Screen.Height - MenuPopupPosition.Y - 10) div cy;
    n := Max(n, 1);

    for i := 0 to List.Count - 1 do
    begin
      Menu := TMenuItem.Create(Self);
      Menu.Tag := Integer(List.Objects[i]);
      Menu.Caption := List.Strings[i];

      if (i > 0) and (i mod n = 0) then
        Menu.Break := mbBarBreak;

      Menu.OnClick := GotoFileClick;
      FBtnPopup.Items.Add(Menu);
    end;
  finally
    List.Free;
  end;

  FBtnPopup.Popup(MenuPopupPosition.X, MenuPopupPosition.Y);
end;

procedure TGXToolBar.GotoFileClick(Sender: TObject);
var
  EditRead: TEditReader;
  UnitInfo: TUnitInfo;
  SendingMenu: TMenuItem;
begin
  SendingMenu := Sender as TMenuItem;

  UnitInfo := FUnitList.Objects[SendingMenu.Tag] as TUnitInfo;

  if not ToolServices.IsFileOpen(UnitInfo.FileName) then
    ToolServices.OpenFile(UnitInfo.FileName);

  // Since this edit reader is destroyed almost
  // immediately, do not call FreeFileData
  EditRead := TEditReader.Create(UnitInfo.FileName);
  try
    // Compensate for AutoHotKeys in D5+
    {$IFDEF GX_VER130_up}
    if StringReplace(SendingMenu.Caption, '&', '', [rfReplaceAll]) = UnitInfo.FormName then
    {$ELSE}
    if SendingMenu.Caption = UnitInfo.FormName then
    {$ENDIF}
      EditRead.ShowForm
    else
      EditRead.ShowSource;
  finally
    EditRead.Free;
  end;
end;

initialization
{$IFDEF GX_BCB}
  SelectComponent := GetProcAddress(HINSTANCE, '@SelectComponentImplementation$qqr17System@AnsiString');
  Assert(@SelectComponent <> nil);
{$ENDIF GX_BCB}

{$ELSE GX_EII}
interface implementation
{$ENDIF GX_EII}

end.

