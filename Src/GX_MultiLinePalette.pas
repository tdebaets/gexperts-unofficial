unit GX_MultiLinePalette;

{$I GX_CondDefine.Inc}

interface

uses
  Windows, Messages, Classes, Graphics, ComCtrls, Menus, Forms,
  ExptIntf, ToolIntf;


type
{$IFDEF GX_MultiLinePaletteSubclass}
  TMultiLineTabManager = class;

  TMultiLineTabNotifier = class(TIAddInNotifier)
  private
    FTabManager: TMultiLineTabManager;
  public
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override; // unused - remove abstract warning
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    constructor Create(TabManager: TMultiLineTabManager);
  end;
{$ENDIF GX_MultiLinePaletteSubclass}

  TMultiLineTabManager = class(TComponent)
  private
    {$IFDEF GX_MultiLinePaletteSubclass}
    FMultiLineTabNotifier: TMultiLineTabNotifier;
    FMinTabSize: Integer;
    OldWndProc: TFarProc;
    NewWndProc: Pointer;
    Menu: TMenuItem;
    FToolsOptionsEvent: TNotifyEvent;
    procedure HookDelphi3MainForm;
    procedure UnHookDelphi3MainForm;
    procedure HookWndProc(var Message: TMessage);
    procedure DoToolsOptions(Sender: TObject);
    {$ENDIF GX_MultiLinePaletteSubclass}
    {$IFDEF GX_VER120_up}
    FOldCPResizeHandler: TNotifyEvent;
    procedure CPResizeHandler(Sender: TObject);
    {$ENDIF GX_VER120_up}
    {$IFDEF GX_MultiLinePaletteSubclass}
    function GetMainForm: TCustomForm;
    function IsSpeedPanelVisible: Boolean;
    function IsComponentPaletteVisible: Boolean;
    function GetMinTabSize: Integer;
    procedure SetTabHeight(Tab: TTabControl);
    procedure SetTabWidth;
    function GetAppHeight: Integer;
    procedure SetAppHeight(H: Integer);
    procedure DoResize;
    {$ENDIF GX_MultiLinePaletteSubclass}
  private
    FTabRaggedRight: Boolean;
    FTabScrollOpposite: Boolean;
    procedure SetTabRaggedRight(const Value: Boolean);
    procedure SetTabScrollOpposite(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TabRaggedRight: Boolean read FTabRaggedRight write SetTabRaggedRight;
    property TabScrollOpposite: Boolean read FTabScrollOpposite write SetTabScrollOpposite;
  end;


implementation

uses
  SysUtils,
{$IFDEF GX_VER120_up}
  TypInfo,
{$ENDIF GX_VER120_up}
  Controls, CommCtrl, ExtCtrls,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_GenFunc;

{$IFDEF GX_MultiLinePaletteSubclass}
procedure TMultiLineTabManager.HookDelphi3MainForm;
var
  MainForm: TCustomForm;
begin
  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  OldWndProc := TFarProc(GetWindowLong(MainForm.Handle, GWL_WNDPROC));
  if OldWndProc = nil then
    RaiseLastWin32Error;

  NewWndProc := MakeObjectInstance(HookWndProc);

  if SetWindowLong(MainForm.Handle, GWL_WNDPROC, Integer(NewWndProc)) = 0 then
    RaiseLastWin32Error;

  //! StH: Possibly sequeeze into the main form's OnResize handler
  // here. We might want to call "DoResize" from there, too:
  //! MainForm.OnResize := OurResizer; --> DoResize --> OldResizer

  Menu := MainForm.FindComponent('ToolsOptionsItem') as TMenuItem;
  if Menu <> nil then
  begin
    FToolsOptionsEvent := Menu.OnClick;
    Menu.OnClick := DoToolsOptions;
  end;
end;

procedure TMultiLineTabManager.UnHookDelphi3MainForm;
var
  MainForm: TCustomForm;
begin
  if OldWndProc = nil then
    Exit;

  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  if SetWindowLong(MainForm.Handle, GWL_WNDPROC, LongInt(OldWndProc)) = 0 then
    RaiseLastWin32Error;
  OldWndProc := nil;

  if NewWndProc <> nil then
  begin
    FreeObjectInstance(NewWndProc);
    NewWndProc := nil;
  end;
end;

procedure TMultiLineTabManager.HookWndProc(var Message: TMessage);
var
  AppHeight: Integer;
begin
  try
    if Message.Msg = WM_GETMINMAXINFO then
    begin
      AppHeight := GetAppHeight;

      with PMinMaxInfo(Message.LParam)^ do
      begin
        ptMaxSize := Point(Screen.Width, AppHeight);
        ptMinTrackSize := Point(640, AppHeight);
        ptMaxPosition := Point(0, 0);
        ptMaxTrackSize := Point(Screen.Width, AppHeight);
      end;
      Message.Result:=0;
    end
    else
    begin
      if Owner = nil then
        Exit;

      with Message do
      begin
        Result := CallWindowProc(OldWndProc, TCustomForm(Owner).Handle,
                                 Msg, WParam, LParam);
      end;
    end;

  except
    on E: Exception do
    begin
      ProcessException(E);
      {$IFOPT D+} SendDebug('Multi-line palette WndProc: ' + E.Message); {$ENDIF}
    end;
  end;
end;

procedure TMultiLineTabManager.DoToolsOptions(Sender: TObject);
var
  TabControl: TTabControl;
  MainForm: TCustomForm;
begin
  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  TabControl := GetMainForm.FindComponent('Tabcontrol') as TTabControl;

{$DEFINE UseLockWindowUpdate}
  { We are making a rather desperate attempt to prevent flicker in the
    following case:

      If the user selects Tools | Environment Options... and clicks
      OK in that dialog, the component palette tab control flickers
      HEAVILY.

    WM_SETREDRAW, the preferred method to get rid of such
    problems, does not work. Hence we need to resort to using
    LockWindowUpdate, but this results in the complete Desktop
    being redrawn.

    One might consider to make this flicker-fixer a user configuration
    option or not - it's just unsatisfactory either way. }

  {$IFDEF UseLockWindowUpdate}
  if TabControl <> nil then
    LockWindowUpdate(TabControl.Handle);
  try
  {$ENDIF UseLockWindowUpdate}

    FToolsOptionsEvent(Menu);

  {$IFDEF UseLockWindowUpdate}
  finally
    if TabControl <> nil then
      LockWindowUpdate(0);
  end;
  {$ENDIF UseLockWindowUpdate}
end;
{$ENDIF GX_MultiLinePaletteSubclass}

{$IFDEF GX_VER120_up}
procedure TMultiLineTabManager.CPResizeHandler(Sender: TObject);

    procedure SetConstraints(AnObject: TObject; const PropName: string; AHeight: Integer);
    var
      PropertyInfo: PPropInfo;
      ConstraintsInstance: TSizeConstraints;
    begin

      PropertyInfo := GetPropInfo(AnObject.ClassInfo, 'Constraints');
      if PropertyInfo = nil then
        Exit;

      ConstraintsInstance := TSizeConstraints(GetOrdProp(AnObject, PropertyInfo));
      if ConstraintsInstance = nil then Exit;

      PropertyInfo := GetPropInfo(ConstraintsInstance.ClassInfo, PropName);
      if PropertyInfo = nil then Exit;

      SetOrdProp(ConstraintsInstance, PropertyInfo, AHeight);
    end;

var
  AHeight: Integer;
  SenderControl: TTabControl;
begin
  {$IFOPT D+} SendDebug('In CPResizeHandler'); {$ENDIF}
  SenderControl := Sender as TTabControl;

  {$IFOPT D+} SendDebug('AHeight := Height ('+IntToStr(SenderControl.Height)+') - DisplayRect.Bottom ('+IntToStr(SenderControl.DisplayRect.Bottom)+ ') - DisplayRect.Top ('+IntToStr(SenderControl.DisplayRect.Top)+') + 29'); {$ENDIF}
  AHeight := SenderControl.Height -
             (SenderControl.DisplayRect.Bottom - SenderControl.DisplayRect.Top) + 29;

  // When compiled in 4.02, this is incompatible with Delphi 4.00/4.01
  SenderControl.Constraints.MinHeight := AHeight;
  SenderControl.Parent.Constraints.MaxHeight := AHeight;

  // This causes the D4 palette to grow in height each time a tab is
  // selected, because OnResize is called again ??
  //SetConstraints(SenderControl, 'MinHeight', AHeight);
  //SetConstraints(SenderControl.Parent, 'MaxHeight', AHeight);
end;

{$ENDIF GX_VER120_up}

procedure SetComCtlStyle(Ctl: TWinControl; Value: Integer; UseStyle: Boolean);
var
  Style: Integer;
begin
  if Ctl.HandleAllocated then
  begin
    Style := GetWindowLong(Ctl.Handle, GWL_STYLE);
    if not UseStyle then Style := Style and not Value
    else Style := Style or Value;
    SetWindowLong(Ctl.Handle, GWL_STYLE, Style);
  end;
end;


procedure TMultiLineTabManager.SetTabRaggedRight(const Value: Boolean);
var
  TabControl: TTabControl;
{$IFNDEF GX_VER120_up}
  Style: DWORD;
{$ENDIF GX_VER120_up}
begin
  if Value = FTabRaggedRight then
    Exit;

  FTabRaggedRight := Value;

  TabControl := GetComponentPaletteTabControl;
  if TabControl = nil then
    Exit;

{$IFDEF GX_VER120_up}
  TabControl.RaggedRight := FTabRaggedRight;
{$ELSE}
  if TabControl.HandleAllocated then
  begin
    Style := GetWindowLong(TabControl.Handle, GWL_STYLE);
    if FTabRaggedRight then
      Style := Style or TCS_RAGGEDRIGHT
    else
      Style := Style and not TCS_RAGGEDRIGHT;
    SetWindowLong(TabControl.Handle, GWL_STYLE, Style);
  end;
{$ENDIF GX_VER120_up}
end;

procedure TMultiLineTabManager.SetTabScrollOpposite(const Value: Boolean);
var
  TabControl: TTabControl;
begin
  if Value = FTabScrollOpposite then
    Exit;

  FTabScrollOpposite := Value;

  TabControl := GetComponentPaletteTabControl;
  if TabControl = nil then
    Exit;

  if FTabScrollOpposite then
  TabControl.ScrollOpposite := FTabScrollOpposite;
end;


{$IFDEF GX_MultiLinePaletteSubclass}
function TMultiLineTabManager.GetMinTabSize: Integer;
var
  TabControl: TTabControl;
  MainForm: TCustomForm;
begin
  Result := 0;

  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  TabControl := MainForm.FindComponent('Tabcontrol') as TTabControl;
  Assert(TabControl <> nil);
  Result := TabControl.Height;
end;

function TMultiLineTabManager.GetMainForm: TCustomForm;
begin
  {$IFDEF GX_MultiLinePaletteSubclass}
    Result := Owner as TCustomForm;
  {$ENDIF GX_MultiLinePaletteSubclass}
  {$IFDEF GX_VER120_up}
    Result := TIdeEnhancements.GetMainForm;
  {$ENDIF GX_VER120_up}
end;

function TMultiLineTabManager.IsSpeedPanelVisible: Boolean;
var
  Panel: TPanel;
  MainForm: TCustomForm;
begin
  Result := False;

  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  Panel := MainForm.FindComponent('SpeedPanel') as TPanel;
  if Panel <> nil then
    Result := Panel.Visible;
end;

function TMultiLineTabManager.IsComponentPaletteVisible: Boolean;
var
  TabControl: TTabControl;
  MainForm: TCustomForm;
begin
  Result := False;

  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  TabControl := MainForm.FindComponent('Tabcontrol') as TTabControl;
  if TabControl <> nil then
    Result := TabControl.Visible;
end;

function TMultiLineTabManager.GetAppHeight: Integer;
var
  MainForm: TCustomForm;
  TabControl: TTabControl;
  Panel: TPanel;
begin
  Result := GetSystemMetrics(SM_CYFRAME) * 2 +
            GetSystemMetrics(SM_CYMENU) +
            GetSystemMetrics(SM_CYCAPTION);

  if (not IsSpeedPanelVisible) and (not IsComponentPaletteVisible) then
    Exit;

  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  if (not IsComponentPaletteVisible) then
  begin
    Panel := TPanel(MainForm.FindComponent('SpeedPanel'));
    if Panel <> nil then
      Result := Result + Panel.Height;
    Exit;
  end;

  TabControl := TTabControl(MainForm.FindComponent('Tabcontrol'));
  if TabControl <> nil then
  begin
    SetTabHeight(TabControl);
    Result := Result + TabControl.Height + GetSystemMetrics(SM_CYFRAME) * 2;
  end;
end;

procedure TMultiLineTabManager.SetAppHeight(H: Integer);
var
  SpeedPanel: TPanel;
  MainForm: TCustomForm;
begin
  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  MainForm.Height := H;
  SpeedPanel := TPanel(MainForm.FindComponent('SpeedPanel'));
  if Speedpanel <> nil then
    SpeedPanel.Height := MainForm.ClientHeight;
end;

procedure TMultiLineTabManager.SetTabHeight(Tab: TTabControl);

  function GetHeight: Integer;
  begin
    Result := Tab.DisplayRect.Bottom - Tab.DisplayRect.Top;
  end;

  function GetTabHeight: Integer;
  var
    MainForm: TCustomForm;
  begin
    Result := Tab.TabHeight;
    if Result = 0 then
    begin
      MainForm := GetMainForm;
      if MainForm <> nil then
        Result := MainForm.Canvas.TextHeight('W') + 6;
    end;
  end;

var
  h: Integer;
begin
  h := GetHeight;
  if h <> 30 then
    Tab.Height := Tab.Height + (30 - h);

  if Tab.Height < FMinTabSize then
    Tab.Height := FMinTabSize;
end;

procedure TMultiLineTabManager.SetTabWidth;
var
  TabControl: TTabControl;
  MainForm: TCustomForm;
begin
  MainForm := GetMainForm;
  if MainForm = nil then
    Exit;

  TabControl := MainForm.FindComponent('Tabcontrol') as TTabControl;
  if TabControl <> nil then
    TabControl.Width := MainForm.ClientWidth - TabControl.Left;
end;

procedure TMultiLineTabManager.DoResize;
begin
  SetAppHeight(GetAppHeight);
  SetTabWidth;
end;
{$ENDIF GX_MultiLinePaletteSubclass}

constructor TMultiLineTabManager.Create(AOwner: TComponent);
var
  Tab: TTabControl;
{$IFNDEF GX_VER120_up}
  HasTabsAsButtons: Boolean;
  Style: DWORD;
{$ENDIF GX_VER120_up}
begin
  inherited Create(AOwner);

  Tab := GetComponentPaletteTabControl;

  if (AOwner = nil) or (Tab = nil) then
    Exit;

  {$IFDEF GX_MultiLinePaletteSubclass}
  FMinTabSize := GetMinTabSize;
  Assert(AOwner = GetIdeMainForm);
  HookDelphi3MainForm;
  FMultiLineTabNotifier := TMultiLineTabNotifier.Create(Self);
  ToolServices.AddNotifier(FMultiLineTabNotifier);
  {$ENDIF GX_MultiLinePaletteSubclass}

  {$IFDEF GX_VER120_up}
  FOldCPResizeHandler := Tab.OnResize;
  Tab.OnResize := CPResizeHandler;
  {$ENDIF GX_VER120_up}


  {$IFNDEF GX_VER120_up}
    HasTabsAsButtons := False;
    if Tab.HandleAllocated then
      HasTabsAsButtons := (GetWindowLong(Tab.Handle, GWL_STYLE) and TCS_BUTTONS <> 0);
  {$ENDIF GX_VER120_up}

  // This will re-create the tab control; since
  // D3,BCB3 do not have the Style property that
  // would store the "buttons" property, hence
  // save it, switch to multiline and restore
  Tab.MultiLine := True;

  {$IFNDEF GX_VER120_up}
  if HasTabsAsButtons then
  begin
    Style := GetWindowLong(Tab.Handle, GWL_STYLE);
    Style := Style or TCS_BUTTONS;
    SetWindowLong(Tab.Handle, GWL_STYLE, Style);
  end;
  {$ENDIF GX_VER120_up}
end;

destructor TMultiLineTabManager.Destroy;

    {$IFDEF GX_VER120_up}
    procedure RemoveDelphi4TabHook;
    var
      TabControl: TTabControl;
    begin
      TabControl := GetComponentPaletteTabControl;
      if TabControl = nil then
        Exit;

      with TabControl do
      begin
        {$IFOPT D+} SendDebug('Restored old CPResizeHandler'); {$ENDIF}
        OnResize := FOldCPResizeHandler;
        MultiLine := False;
      end;
    end;
    {$ENDIF GX_VER120_up}

begin
  try
    {$IFDEF GX_MultiLinePaletteSubclass}
    if FMultiLineTabNotifier <> nil then
    begin
      ToolServices.RemoveNotifier(FMultiLineTabNotifier);
      FMultiLineTabNotifier.Free;
      FMultiLineTabNotifier := nil;
    end;
    UnHookDelphi3MainForm;
    {$ENDIF GX_MultiLinePaletteSubclass}
    {$IFDEF GX_VER120_up}
    {$IFOPT D+} SendDebug('Removing Delphi 4 tab hooks'); {$ENDIF}
    RemoveDelphi4TabHook;
    {$IFOPT D+} SendDebug('Removed Delphi 4 tab hooks'); {$ENDIF}
    {$ENDIF GX_VER120_up}
  except
    on E: Exception do
    begin
      ProcessException(E);
      {$IFOPT D+} SendDebug('Multi-line Tab exception: ' + E.Message) {$ENDIF};
    end;
  end;
  inherited Destroy;
  {$IFOPT D+} SendDebug('Removed multi-line tabs'); {$ENDIF}
end;

{$IFDEF GX_MultiLinePaletteSubclass}
constructor TMultiLineTabNotifier.Create(TabManager: TMultiLineTabManager);
begin
  inherited Create;
  FTabManager := TabManager;
end;

procedure TMultiLineTabNotifier.EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  // nothing
end;

procedure TMultiLineTabNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // These notifiers do not do any good in D4 - hence they are not
  // activated; for Delphi 3 they make the tab control resize properly
  case NotifyCode of

    fnProjectDesktopLoad,
    fnFileOpened:
      FTabManager.DoResize;

    fnPackageInstalled,
    fnPackageUninstalled:
      FTabManager.DoResize;

  else
    // nothing
  end;
end;
{$ENDIF GX_MultiLinePaletteSubclass}

end.
