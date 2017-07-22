unit GX_EditorEnhancements;

{$I GX_CondDefine.Inc}

{$IFNDEF GX_EII}
interface implementation
{$ELSE GX_EII}

interface

uses
  Classes, SysUtils, Windows, Forms, Messages, Controls,
  ExtCtrls, Menus, Buttons, Dialogs, StdCtrls,
  ToolIntf, ExptIntf,
  EIManager, EIPanel, EINotifiers;

type
  TEditorEnhancements = class(TComponent)
  private
    FToolBarAlign: TAlign;
    FIdeManager: TIdeManager;
    FFormChangeNotifier: TEIMessageNotifier;
    FDialogExit: TNotifyEvent;
    FToolBarButtonsList: TList;
    FToolBarList: TList;
    FToolBarNotifier: TIAddInNotifier;
    FNewEditWindow: TNotifyEvent;
    FButtons: Boolean;
    FButtonsFlat: Boolean;
    FMultiLine: Boolean;
    FHotTrack: Boolean;
    FToolBarVisible: Boolean;
    FFontControl: Boolean;
    FEditorContextMenus: Boolean;
    FEditorContextMenusAtBottom: Boolean;

    FDoHandleEnvDialog: Boolean;
    FHookedEnvDialogInstance: TForm;
    FEnabled: Boolean;

    FHookedDialogBounds: TRect;

    procedure LoadSettings;

    procedure NewEditWindowClick(Sender: TObject);
    procedure HookViewNewEditorItem(DoHook: Boolean);
    procedure LogFormChange(Sender: TObject);
    procedure DialogOnDestroyHook(Sender: TObject);
    procedure DialogOnResizeHook(Sender: TObject);
    procedure AddToolBar;
    procedure RemoveToolBar;
    procedure SetEnabled(const Value: Boolean);
    procedure Install;
    procedure Remove;
  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure SizeToolbar(EIPanel: TEIPanel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveSettings;
    procedure ExecuteExpertByNo(ButtonNo: Integer);
    // ToolBar code
    procedure ApplyToolBarSettings;
    property ToolBarButtonsList: TList read FToolBarButtonsList write FToolBarButtonsList;

    procedure ShowConfigurationDialog(ActiveTab: string);
    property HookedEnvDialogInstance: TForm read FHookedEnvDialogInstance write FHookedEnvDialogInstance;
    property DoHandleEnvDialog: Boolean read FDoHandleEnvDialog write FDoHandleEnvDialog;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property IdeManager: TIdeManager read FIdeManager;
    // Tab properties
    property MultiLine: Boolean read FMultiLine write FMultiLine;
    property HotTrack: Boolean read FHotTrack write FHotTrack;
    property Buttons: Boolean read FButtons write FButtons;
    property ButtonsFlat: Boolean read FButtonsFlat write FButtonsFlat;
    property FontControl: Boolean read FFontControl write FFontControl;
    // Context menu properties
    property EditorContextMenus: Boolean read FEditorContextMenus write FEditorContextMenus;
    property EditorContextMenusAtBottom: Boolean read FEditorContextMenusAtBottom write FEditorContextMenusAtBottom;
    // ToolBar properties
    property ToolBarAlign: TAlign read FToolBarAlign write FToolBarAlign;
    property ToolBarVisible: Boolean read FToolBarVisible write FToolBarVisible;
  end;

function EditorEnhancements: TEditorEnhancements;
procedure FreeEditorEnhancements;

implementation

uses
  Registry, ComCtrls,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
  GX_ConfigurationInfo, GX_Configure,
  GX_GExperts,
  GX_ToolBar, GX_ToolBarButtons,
  GX_GenFunc;

type
  TFormChangeNotifier = class(TEIMessageNotifier)
  private
    FEditManager: TEditorEnhancements;
  public
    constructor Create(EditManager: TEditorEnhancements);
    procedure BeforeEditorMessageProcess(Manager: TEIManager;
      OriginalMessage: TMessage; var Msg: TMessage); override;
    procedure OnActiveFormChanged(AForm: TCustomForm); override;
    procedure OnApplicationActivate(Activated: Boolean); override;
  end;

{ TFormChangeNotifier }

constructor TFormChangeNotifier.Create(EditManager: TEditorEnhancements);
begin
  inherited Create;

  Assert(EditManager <> nil);
  FEditManager := EditManager;
end;

procedure TFormChangeNotifier.BeforeEditorMessageProcess(Manager: TEIManager;
  OriginalMessage: TMessage; var Msg: TMessage);
begin
  case OriginalMessage.Msg of
    CN_KEYUP:
      begin
        if TWMKey(Msg).CharCode = 0 then
          Msg.Result := 1;
      end;
  end;
end;

procedure TFormChangeNotifier.OnActiveFormChanged(AForm: TCustomForm);
begin
  FEditManager.LogFormChange(AForm);
end;

procedure TFormChangeNotifier.OnApplicationActivate(Activated: Boolean);
begin
  // Nothing
end;

type
  TToolBarNotifier = class(TIAddInNotifier)
  private
    FEditManager: TEditorEnhancements;
  public
    constructor Create(EditManager: TEditorEnhancements);
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
  end;

{ ToolBarNotifier }

constructor TToolBarNotifier.Create(EditManager: TEditorEnhancements);
begin
  inherited Create;

  Assert(EditManager <> nil);
  FEditManager := EditManager;
end;

procedure TToolBarNotifier.EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  // nothing
end;

procedure TToolBarNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    fnFileOpened,
    fnProjectOpened,
    fnDefaultDesktopLoad,
    fnProjectDesktopLoad:  FEditManager.AddToolBar;
  else
    // nothing
  end;
end;

{ TEditorEnhancements }

constructor TEditorEnhancements.Create(AOwner: TComponent);
begin
  //! StH: There have been some rather inexplicable problems (AVs)
  // if AOwner <> nil; simply take no risks - we don't need an owner

  Assert(AOwner = nil);

  inherited Create(AOwner);

  Name := 'GX_EditEnhance';

  FToolBarAlign := alTop;
  FHookedEnvDialogInstance := nil;
  FEditorContextMenus := False;
  FFontControl := False;
  FMultiLine := False;
  FEditorContextMenusAtBottom := True;
  {$IFDEF GX_VER120_up}
  // Delphi 4+ default to hot tracking on
  FHotTrack := True;
  {$ELSE}
  FHotTrack := False;
  {$ENDIF GX_VER120_up}

  FButtons := False;
  FButtonsFlat := False;
  FToolBarVisible := True;

  FToolBarButtonsList := TList.Create;

  LoadSettings;
end;

destructor TEditorEnhancements.Destroy;
begin
  {$IFOPT D+}SendDebug('Destroying Editor Enhancements'); {$ENDIF}
  Remove;

  FToolBarButtonsList.Free;
  FToolBarButtonsList := nil;

  {$IFOPT D+}SendDebug('Editor Enhancements Destroyed'); {$ENDIF}

  inherited Destroy;
end;

procedure TEditorEnhancements.Install;
begin
  if ToolServices = nil then
    Exit;

  // Initialize the IDE Manager
  FIdeManager := InstallIDEIntegrationManager('GExperts');

  FToolBarList := TList.Create;

  // Notifiers
  FToolBarNotifier := TToolBarNotifier.Create(Self);
  ToolServices.AddNotifier(FToolBarNotifier);

  FFormChangeNotifier := TFormChangeNotifier.Create(Self);
  Assert(FIdeManager <> nil);
  FIdeManager.AddNotifier(FFormChangeNotifier);

  AddToolBar;
  HookViewNewEditorItem(True);
end;

procedure TEditorEnhancements.Remove;

  function IdeManagerExists: Boolean;
  begin
    Result := (FindIDEIntegrationManager <> nil);
  end;

begin
  if ToolServices = nil then
    Exit;

  HookViewNewEditorItem(False);
  Application.ProcessMessages;

  if IdeManagerExists and (FFormChangeNotifier <> nil) then
  begin
    {$IFOPT D+}SendDebug('Removing EII Notifier'); {$ENDIF}
    Assert(FIdeManager <> nil);
    FIdeManager.RemoveNotifier(FFormChangeNotifier);
  end;
  FFormChangeNotifier.Free;
  FFormChangeNotifier := nil;

  if FToolBarNotifier <> nil then
  begin
    ToolServices.RemoveNotifier(FToolBarNotifier);
    FToolBarNotifier.Free;
    FToolBarNotifier := nil;
  end;

  RemoveToolBar;

  {$IFOPT D+}SendDebug('Freeing toolbar list'); {$ENDIF}
  FToolBarList.Free;
  FToolBarList := nil;
end;

procedure TEditorEnhancements.LogFormChange(Sender: TObject);
var
  ActiveForm: TForm;

    procedure HandleTEditWindow;
    var
      Editor: TCustomControl;
    begin
      AddToolBar;
      Editor := ActiveForm.FindComponent('Editor') as TCustomControl;
      if Editor <> nil then
      try
        // Prevents problems when the core editor window is selected,
        // but the core editor itself isn't focusable/visible
        if Editor.CanFocus then
          Editor.SetFocus;
      except
      end;
    end;

    procedure HandleTEnvDialog;
    begin
      ActiveForm.SetBounds(-1000, -1000, 1, 1);
      FHookedEnvDialogInstance := ActiveForm;
      Application.ProcessMessages;
    end;

    procedure HandleTViewDialog;
    begin
      try
        ActiveForm.BorderStyle := bsSizeable;
      except
        on E: Exception do
        begin
          // Need to swallow visible warning
        end;
      end;
      ActiveForm.BoundsRect := FHookedDialogBounds;

      FDialogExit := ActiveForm.OnDestroy;
      ActiveForm.OnDestroy := DialogOnDestroyHook;
      ActiveForm.OnResize := DialogOnResizeHook;

      ShowWindow(ActiveForm.Handle, SW_SHOWNA);

      Application.ProcessMessages;
    end;

begin
  ActiveForm := Screen.ActiveForm;
  if ActiveForm = nil then
    Exit;

  {$IFOPT D+} SendDebug('FormChange Window Class:' + Screen.ActiveForm.ClassName + ' (' + Screen.ActiveForm.Name + ')'); {$ENDIF}

  if CompareText(Screen.ActiveForm.Classname, 'TEditWindow') = 0 then
    HandleTEditWindow
  else
  if (CompareText(Screen.ActiveForm.ClassName, 'TEnvDialog') = 0) and FDoHandleEnvDialog then
    HandleTEnvDialog
  else
  if (CompareText(Screen.ActiveForm.Classname, 'TViewDialog') = 0) and False {DExpert.Resize} then
    HandleTViewDialog;
end;

procedure TEditorEnhancements.DialogOnResizeHook(Sender: TObject);
var
  Form: TForm;
  ListBox: TListBox;
  HelpButton, CancelButton, OKButton: TButton;
  Edit: TEdit;
begin
  Form := Sender as TForm;
  if Form = nil then
    Exit;

  ListBox := Form.FindComponent('ListBox') as TListBox;
  HelpButton := Form.FindComponent('HelpButton') as TButton;
  CancelButton := Form.FindComponent('CancelButton') as TButton;
  OKButton := Form.FindComponent('OKButton') as TButton;
  Edit := Form.FindComponent('Edit') as TEdit;

  if (ListBox = nil) or (OKButton = nil) or (HelpButton = nil) or
     (CancelButton = nil) or (Edit = nil) then
  begin
    Exit;
  end;

  if Form.Width >= 120 then
  begin
    OKButton.Left := Form.ClientWidth - OKButton.Width - 6;
    HelpButton.Left := OKButton.Left;
    CancelButton.Left := OKButton.Left;
    ListBox.Width := OKButton.Left - 6 - ListBox.Left;
    ListBox.Height := Form.ClientHeight - ListBox.Top - 4;
    Edit.Width := ListBox.Width;
  end;
end;

procedure TEditorEnhancements.DialogOnDestroyHook(Sender: TObject);
var
  SendingForm: TForm;
begin
  if Sender is TForm then
  begin
    SendingForm := TForm(Sender);

    FHookedDialogBounds := SendingForm.BoundsRect;
    if Assigned(FDialogExit) then
    begin
      SendingForm.OnDestroy := FDialogExit;
      FDialogExit(Sender);
    end;
  end;
end;

procedure TEditorEnhancements.HookViewNewEditorItem(DoHook: Boolean);
var
  Menu: TMenuItem;
  App: TCustomForm;
begin
  App := GetIdeMainForm;
  if App = nil then
    Exit;

  Menu := App.FindComponent('ViewNewEditorItem') as TMenuItem;
  if Menu <> nil then
  begin
    if DoHook then
    begin
      FNewEditWindow := Menu.OnClick;
      Menu.OnClick := NewEditWindowClick;
    end
    else
    begin
      if Assigned(FNewEditWindow) then
        Menu.OnClick := FNewEditWindow;
      FNewEditWindow := nil;
    end;
  end;
end;

procedure TEditorEnhancements.NewEditWindowClick(Sender: TObject);
begin
  if Assigned(FNewEditWindow) then
    FNewEditWindow(Sender);

  AddToolBar;
end;

procedure TEditorEnhancements.AddToolBar;
var
  i: Integer;
  EditForm: TForm;
  ToolBar: TGXToolBar;
  EIManager: TEIManager;
  EIPanel: TEIPanel;
begin
  Assert(FIdeManager <> nil);
  for i := 0 to FIdeManager.EIManagerCount - 1 do
  begin
    EIManager := FIdeManager.EIManagers[i];
    EditForm := EIManager.Editor;
    if EditForm.FindComponent('GX_ToolBar') = nil then
    begin
      ToolBar := TGXToolBar.CreateManaged(EditForm, Self);
      FToolBarList.Add(ToolBar);
      ToolBar.FreeNotification(Self);

      EIPanel := FIdeManager.RequestIntegration(EIManager, ToolBar);
      // We do not want to have any buttons on
      // the integration panel - and we do not
      // want any caption on our panel
      EIPanel.TitleBarButtonVisible := [];
      EIPanel.TitleBarButtonEnabled := [];
      EIPanel.TitleBarVisible := False;

      //
      EIPanel.Align := FToolBarAlign;
      SizeToolbar(EIPanel);

      // Hide the splitter bar
      EIPanel.SplitterVisible := False;
      EIPanel.BevelInner := bvNone;
      EIPanel.BevelOuter := bvNone;

      ToolBar.Align := alClient;
      ToolBar.Name := 'GX_ToolBar';
      ToolBar.Caption := '';
      ToolBar.Parent := EIPanel;
      ToolBar.BringToFront;
      ToolBar.CreateToolBarButtons;

      // Finally make the EIPanel visible depending on settings
      EIPanel.Visible := ToolBarVisible;

      ToolBar.SetEditorControls;
    end;
  end;
end;

procedure TEditorEnhancements.RemoveToolBar;
var
  EIPanel: TEIPanel;
  GExpertsToolBar: TGXToolBar;
begin
  if Application.Terminated then
    Exit;

  if FToolBarList = nil then
    Exit;

  {$IFOPT D+}SendDebug(Format('Editor Enhancements: Removing %d toolbar(s)', [FToolBarList.Count])); {$ENDIF}
  while FToolBarList.Count > 0 do
  begin
    GExpertsToolBar := FToolBarList.Items[0];

    EIPanel := GExpertsToolBar.Parent as TEIPanel;
    // Note, notification removes the item from the FToolBarList
    GExpertsToolBar.Free;
    {$IFOPT D+}SendDebug('Editor Enhancements: Successfully freed toolbar'); {$ENDIF}
    { TODO -oStefan -cIssue: Should we Free the EIPanel, too? }
    EIPanel.Visible := False;
    {$IFOPT D+}SendDebug('Editor Enhancements: Set EIPanel.Visible to False'); {$ENDIF}
  end;
end;

procedure TEditorEnhancements.Notification(Component: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  if Operation = opRemove then
  begin
    i := FToolBarList.IndexOf(Component);
    if i >= 0 then
      FToolBarList.Delete(i);
  end;

  inherited Notification(Component, Operation);
end;

resourcestring
  SEditorExpertMissing = 'Editor expert %d not found.  Are they enabled?';

procedure TEditorEnhancements.ExecuteExpertByNo(ButtonNo: Integer);
var
  i: Integer;
begin
  for i := 0 to GExpertsInst.EditorExpertCount - 1 do
  begin
    if GExpertsInst.EditorExpertList[i].ButtonNo = ButtonNo then
    begin
      GExpertsInst.EditorExpertList[i].Execute;
      Exit;
    end;
  end;
  MessageDlg(Format(SEditorExpertMissing, [ButtonNo]), mtError, [mbOK], 0);
end;

procedure TEditorEnhancements.ShowConfigurationDialog(ActiveTab: string);
var
  Dlg: TfmConfiguration;
  TabComp: TComponent;
  i: Integer;
begin
  Dlg := TfmConfiguration.Create(nil);
  try
    with Dlg do
    begin
      TabComp := FindComponent(ActiveTab);
      if TabComp is TTabSheet then
        pcConfig.ActivePage := TTabSheet(TabComp);
      // Hide non-essential tab sheets
      for i := 0 to pcConfig.PageCount - 1 do
        pcConfig.Pages[i].TabVisible := (pcConfig.Pages[i].Name  = 'tshEditor') or
          (pcConfig.Pages[i].Name  = 'tshToolBar') or (pcConfig.Pages[i].Name  = 'tshEditorExperts');
    end;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TEditorEnhancements.ApplyToolBarSettings;
var
  i: Integer;
  EIPanel: TEIPanel;
  GExpertsToolBar: TGXToolBar;
begin
  if FToolBarList = nil then
    Exit;

  {$IFOPT D+}SendDebug(Format('Applying settings to %d toolbars', [FToolBarList.Count]));{$ENDIF D+}
  for i := 0 to FToolBarList.Count - 1 do
  begin
    GExpertsToolBar := FToolBarList.Items[i];

    EIPanel := GExpertsToolBar.Parent as TEIPanel;

    {$IFOPT D+}SendDebug('Hiding parent EIIPanel');{$ENDIF D+}
    EIPanel.Visible := False;
    {$IFOPT D+}SendDebug('Aligning parent EIIPanel');{$ENDIF D+}
    EIPanel.Align := FToolBarAlign;
    SizeToolbar(EIPanel);

    {$IFOPT D+}SendDebug('Setting ToolBar.Visible to ' + BooleanText(ToolBarVisible));{$ENDIF D+}
    EIPanel.Visible := ToolBarVisible;

    GExpertsToolBar.CreateToolBarButtons;
    GExpertsToolBar.SetEditorControls;
  end;
  {$IFOPT D+}SendDebug('Successfully applied toolbar settings');{$ENDIF D+}
end;

procedure TEditorEnhancements.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if FEnabled then
    Install
  else
    Remove;
end;

procedure TEditorEnhancements.SaveSettings;
var
  RegIni: TRegIniFile;
  i: Integer;
begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('ToolBar', 'Count', FToolBarButtonsList.Count);

    for i := 0 to FToolBarButtonsList.Count - 1 do
      RegIni.WriteInteger('ToolBar', 'Button' + IntToStr(i), Integer(FToolBarButtonsList.Items[i]));

    RegIni.WriteBool('EditEnhance', 'MultiLine', MultiLine);
    RegIni.WriteBool('EditEnhance', 'HotTrack', HotTrack);
    RegIni.WriteBool('EditEnhance', 'Buttons', Buttons);
    RegIni.WriteBool('EditEnhance', 'ButtonsFlat', ButtonsFlat);
    RegIni.WriteBool('EditEnhance', 'ToolBarVisible', ToolBarVisible);
    RegIni.WriteBool('EditEnhance', 'FontControl', FontControl);
    RegIni.WriteBool('EditEnhance', 'EditMenus', FEditorContextMenus);
    {$IFNDEF GX_VER110_up}
    RegIni.WriteBool('EditEnhance', 'BTMenus', FEditorContextMenusAtBottom);
    {$ENDIF GX_VER110_up}
    RegIni.WriteInteger('EditEnhance', 'ToolBarAlign', Ord(FToolBarAlign));
  finally
    RegIni.Free;
  end;
end;

procedure TEditorEnhancements.LoadSettings;
var
  RegIni: TRegIniFile;
  i: Integer;
  ToolBarButtonCount: Integer;
  ToolBarButton: Integer;
begin
  Assert(ConfigInfo <> nil);

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    ToolBarButtonCount := RegIni.ReadInteger('ToolBar', 'Count', 0);
    for i := 0 to ToolBarButtonCount-1 do
    begin
      ToolBarButton := RegIni.ReadInteger('ToolBar', 'Button' + IntToStr(i), SeparatorMenuItemMarker);
      if (ToolBarButton >= 0) and ((ToolBarButton < High(GxButtonInfo)) or (ToolBarButton = SeparatorMenuItemMarker)) then
        FToolBarButtonsList.Add(Pointer(ToolBarButton));
    end;

    if ToolBarButtonCount = 0 then
    begin
      // Add default buttons
      { TODO -oStefan -cIssue : Hard-coded toolbar button indices }
      for i := 8 to 12 do
        FToolBarButtonsList.Add(Pointer(i));
    end;

    MultiLine := RegIni.ReadBool('EditEnhance', 'MultiLine', MultiLine);
    HotTrack := RegIni.ReadBool('EditEnhance', 'HotTrack', HotTrack);
    Buttons := RegIni.ReadBool('EditEnhance', 'Buttons', Buttons);
    ButtonsFlat := RegIni.ReadBool('EditEnhance', 'ButtonsFlat', ButtonsFlat);
    ToolBarVisible := RegIni.ReadBool('EditEnhance', 'ToolBarVisible', ToolBarVisible);
    FontControl := RegIni.ReadBool('EditEnhance', 'FontControl', FontControl);
    {$IFNDEF GX_VER130_up}
    // Delphi 5+ provides these edit menu items by default
    FEditorContextMenus := RegIni.ReadBool('EditEnhance', 'EditMenus', FEditorContextMenus);
    {$ENDIF GX_VER130_up}
    {$IFNDEF GX_VER110_up}
    FEditorContextMenusAtBottom := RegIni.ReadBool('EditEnhance', 'BTMenus', FEditorContextMenusAtBottom);
    {$ENDIF GX_VER110_up}
    FToolBarAlign := TAlign(RegIni.ReadInteger('EditEnhance', 'ToolBarAlign', Ord(FToolBarAlign)));
  finally
    RegIni.Free;
  end;
end;

var
  PrivateEditorEnhancements: TEditorEnhancements;
  CanCreate: Boolean = True;

function EditorEnhancements: TEditorEnhancements;
begin
  Assert(CanCreate);

  if PrivateEditorEnhancements = nil then
    PrivateEditorEnhancements := TEditorEnhancements.Create(nil);

  Result := PrivateEditorEnhancements;
end;

procedure FreeEditorEnhancements;
begin
  CanCreate := False;

  PrivateEditorEnhancements.Free;
  PrivateEditorEnhancements := nil;
end;

// Special sizing to handle windows dcked into the code editor
// This keeps IDE splitters from resizing our toolbar
procedure TEditorEnhancements.SizeToolbar(EIPanel: TEIPanel);
var
  PanelSize: Integer;
begin
  PanelSize := ButtonSize + 2 * ButtonSpacing;
  {$IFOPT D+}SendDebug('Sizing parent EIIPanel');{$ENDIF D+}
  if FToolBarAlign in [alTop, alBottom] then
  begin
    EIPanel.Height := PanelSize;
    {$IFDEF GX_VER120_up}
    EIPanel.Constraints.MaxHeight := PanelSize;
    EIPanel.Constraints.MinHeight := PanelSize;
    EIPanel.Constraints.MaxWidth := 0;
    EIPanel.Constraints.MinWidth := 0;
    {$ENDIF GX_VER120_up}
  end
  else
  begin
    EIPanel.Width := PanelSize;
    {$IFDEF GX_VER120_up}
    // Partially fixes a resize bug when a toolbar and a window are docked right
    EIPanel.Constraints.MaxHeight := 0;
    EIPanel.Constraints.MinHeight := 0;
    EIPanel.Constraints.MaxWidth := PanelSize;
    EIPanel.Constraints.MinWidth := PanelSize;
    {$ENDIF GX_VER120_up}
  end;
end;

initialization

finalization
  FreeEditorEnhancements;

{$ENDIF GX_EII}

end.
