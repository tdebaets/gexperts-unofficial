unit GX_IdeDock;

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, IniFiles, Forms, Controls,
  MenuBar, Menus, Messages,
  GX_DummyIdeDock,
  // You must include DsnIdeXX.dcp or DsnIdeXX.bpi as a runtime
  // package to compile this unit.
  DockForm;

type
{$IFDEF EnableIdeDockingSupport}
  TDummyPopupMenu = class(TPopupMenu)
  private
    OwnerMenu: TMenu;
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
  end;
{$ENDIF EnableIdeDockingSupport}

{$UNDEF TrickTheIdeAncestorForm}  // this must always be undefined, so that
{$IFDEF TrickTheIdeAncestorForm}  // <--- this define is always false
  TfmIdeDockForm = class(TDummyIdeDockForm);
{$ELSE}
  TfmIdeDockForm = class(TDockableForm)
{$ENDIF TrickTheIdeAncestorForm}
  protected
    FMenuBar: TMenuBar;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF EnableIdeDockingSupport}
    procedure LoadWindowState(Desktop: TMemIniFile); override;
    procedure SaveWindowState(Desktop: TMemIniFile; IsProject: Boolean); override;
    {$ENDIF EnableIdeDockingSupport}
  end;

type
  TIdeDockFormClass = class of TfmIdeDockForm;

type
  EIdeDockError = class(Exception);

  TIdeDockManager = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);
       
    procedure ShowForm(Form: TForm);
  public
    // Override de/allocator functions to implement Singleton behaviour.
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

function IdeDockManager: TIdeDockManager;

implementation

uses
  {$IFOPT D+} GX_DbugIntf,{$ENDIF D+}
  DeskForm, DeskUtil,
  GX_VerDepConst, GX_GenFunc;

{$R *.DFM}

{ TIdeDockManager }

constructor TIdeDockManager.Create;
begin
  {$IFOPT D+} SendDebug('Creating TIdeDockManager'); {$ENDIF D+}
  inherited Create;
end;

destructor TIdeDockManager.Destroy;
begin
  inherited Destroy;
end;

procedure TIdeDockManager.ShowForm(Form: TForm);
begin
  {$IFDEF EnableIdeDockingSupport}
  with Form as TDockableForm do
  begin
    if not Floating then
    begin
      ForceShow;
      FocusWindow(Form);
    end
    else
      Show;
  end;
  {$ELSE}
    Form.Show;
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
begin
  {$IFDEF EnableIdeDockingSupport}
  if @RegisterFieldAddress <> nil then
    RegisterFieldAddress(IdeDockFormName, @IdeDockFormVar);

  RegisterDesktopFormClass(IdeDockFormClass, IdeDockFormName, IdeDockFormName);
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
{$IFDEF EnableIdeDockingSupport}
{$ENDIF EnableIdeDockingSupport}
begin
  {$IFDEF EnableIdeDockingSupport}
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@IdeDockFormVar);
  {$ENDIF EnableIdeDockingSupport}
end;

var
  PrivateIdeDockManager: TIdeDockManager = nil;
  CanFreeIdeDockManager: Boolean = False;

function IdeDockManager: TIdeDockManager;
begin
  Result := PrivateIdeDockManager;
end;

class function TIdeDockManager.NewInstance: TObject;
resourcestring
  SIdeDockManagerCannotBeCreatedManually = 'The IDE dock manager cannot be created manually.';
begin
  if PrivateIdeDockManager = nil then
    Result := inherited NewInstance
  else
    raise EIdeDockError.Create(SIdeDockManagerCannotBeCreatedManually);
end;

procedure TIdeDockManager.FreeInstance;
begin
  if CanFreeIdeDockManager then
    inherited FreeInstance;
end;

{ TfmIdeDockForm }

constructor TfmIdeDockForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //OutputDebugString(PChar(AnsiString(ClassName + ' - ' + Name + ' - ' + DeskSection)));
  {$IFDEF EnableIdeDockingSupport}
  if Menu <> nil then
  begin
    FMenuBar := TMenuBar.Create(Self);
    FMenuBar.Parent := Self;
    FMenuBar.Menu := Menu;
    FMenuBar.Height := GetSystemMetrics(SM_CYMENU) + 2;
    Menu := nil;
  end;
  if (PopupMenu = nil) and (FMenuBar <> nil) then
  begin
    PopupMenu := TDummyPopupMenu.Create(Self);
    TDummyPopupMenu(PopupMenu).OwnerMenu := FMenuBar.Menu;
  end;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
end;

destructor TfmIdeDockForm.Destroy;
begin
  {$IFDEF EnableIdeDockingSupport}
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
  inherited;
end;

procedure TfmIdeDockForm.Loaded;
begin
  inherited Loaded;
end;

{$IFDEF EnableIdeDockingSupport}
procedure TfmIdeDockForm.LoadWindowState(Desktop: TMemIniFile);
begin
  inherited LoadWindowState(Desktop);

  // OutputDebugString(PChar(AnsiString('Loading ' + ClassName + ' from ' + Desktop.FileName)));
end;

procedure TfmIdeDockForm.SaveWindowState(Desktop: TMemIniFile; IsProject: Boolean);
begin
  inherited SaveWindowState(Desktop, IsProject);

  // OutputDebugString(PChar(AnsiString('Saving ' + ClassName + ' from ' + Desktop.FileName)));
end;

{ TDummyPopupMenu }

function TDummyPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
begin
  // Call the form's IsShortCut so docked forms can use main menu shortcuts
  Result := (OwnerMenu <> nil) and OwnerMenu.IsShortCut(Message);
end;
{$ENDIF EnableIdeDockingSupport}

initialization
  PrivateIdeDockManager := TIdeDockManager.Create;

finalization
  CanFreeIdeDockManager := True;
  FreeAndNil(PrivateIdeDockManager);

end.
