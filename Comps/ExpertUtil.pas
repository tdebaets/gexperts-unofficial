unit ExpertUtil;

{ Hidden Paths of Delphi 3, by Ray Lischner.
  Informant Press, 1997.
  Copyright © 1997 Tempest Software, Inc.

  Expert utilities unit:
    notifier components:
      TRLProjectNotifier
      TModuleNotifier
    menu item convenience functions
      InsertMenuItem
      FindMenuItem
    editor interface helpers:
      TEditReaderStream
      TEditorStrings
      ReplaceSelection
}

{$I GX_CondDefine.inc}

{$UNDEF GX_WorkAroundFirstCharInLineSelectionBug}

{$IFDEF GX_VER120_up}
  {$DEFINE GX_WorkAroundFirstCharInLineSelectionBug}
{$ENDIF GX_VER120_up}

interface

uses
  Classes, SysUtils, EditIntf, ToolIntf, OwnerList;

{ The insert action dictates how InsertMenuItem interprets the
  target menu item. Use iaBefore to insert an item immediately
  before the target (i.e., at the same index). Use iaAfter to
  add a new item after the target (index + 1). Use iaChild to
  create a child menu item, where the target is the parent
  menu. }
type
  TInsertAction = (iaBefore, iaAfter, iaChild);

function FindMenuItem(const Name: string): TIMenuItemIntf;
function InsertMenuItem(Action: TInsertAction;
  TargetName, Caption, Name, Hint: string;
  ShortCut, Context, GroupIndex: Integer;
  Flags: TIMenuFlags;
  EventHandler: TIMenuClickEvent): TIMenuItemIntf;

procedure ReplaceSelection(Editor: TIEditorInterface;
  ViewNum: Integer; Text: string);
procedure SelectBlock(Editor: TIEditorInterface; Start, After: TCharPos);

type
  TEditReaderStream = class(TStream)
  private
    fSize: LongInt;
    fPosition: LongInt;
    fReader: TIEditReader;
    function GetSize: LongInt;
  public
    constructor Create(EditIntf: TIEditorInterface);
    destructor Destroy; override;
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function Seek(Offset: LongInt; Origin: Word): LongInt; override;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    property Size: LongInt read GetSize;
  end;

  TEditorStrings = class(TStrings)
  private
    fStrings: TStrings;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const Str: string); override;
    procedure PutObject(Index: Integer; Obj: TObject); override;
    function GetPosition(Index: Integer): LongInt; virtual;
    function GetCharPos(Index: Integer): TCharPos; virtual;
    property Strings: TStrings read fStrings;
  public
    constructor Create(Editor: TIEditorInterface);
    destructor Destroy; override;
    procedure LoadFromEditor(Editor: TIEditorInterface);
    procedure SaveToEditor(Editor: TIEditorInterface);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const Str: string); override;
    function PosToCharPos(Pos: LongInt): TCharPos;
    function CharPosToPos(CharPos: TCharPos): LongInt;
    property Position[Index: Integer]: LongInt read GetPosition;
    property CharPos[Index: Integer]: TCharPos read GetCharPos;
  end;

  TModuleNotifier = class;
  TAddInNotifier = class;
  TModNotifier = class;

  TProjectFilePreEvent    = procedure (Sender: TObject;
    Filename: string; var Cancel: Boolean) of object;
  TProjectFilePostEvent   = procedure (Sender: TObject;
    Filename: string) of object;
  TProjectNotifyPreEvent  = procedure (Sender: TObject;
    var Cancel: Boolean) of object;
  TProjectNotifyPostEvent = procedure (Sender: TObject;
    Successful: Boolean) of object;

  TRLProjectNotifier = class (TComponent)
  private
    fModuleNotifier: TModuleNotifier;
    fNotifier: TAddInNotifier;             { notifier object }
    { The following fields hold the event handlers. }
    fAfterCompile: TProjectNotifyPostEvent;
    fBeforeCompile: TProjectNotifyPreEvent;
    fOnFileOpening: TProjectFilePreEvent;
    fOnFileOpened: TProjectFilePostEvent;
    fOnFileClosing: TProjectFilePreEvent;
    fOnPackageInstalled: TProjectFilePostEvent;
    fOnPackageUninstalled: TProjectFilePostEvent;
    fOnProjectOpening: TProjectFilePreEvent;
    fOnProjectOpened: TProjectFilePostEvent;
    fOnProjectClosing: TProjectFilePreEvent;
    fOnAddedToProject: TProjectFilePostEvent;
    fOnRemovedFromProject: TProjectFilePostEvent;
    fOnDefaultDesktopSave: TProjectFilePostEvent;
    fOnDefaultDesktopLoad: TProjectFilePostEvent;
    fOnProjectDesktopSave: TProjectFilePostEvent;
    fOnProjectDesktopLoad: TProjectFilePostEvent;
  protected
    procedure AddNotifier(Filename: string); virtual;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    property Notifier: TAddInNotifier read fNotifier;
    { Each event has a method to call the event handler.
      Subclasses can override these methods to customize
      the behavior for one or more events. }
    procedure DoAfterCompile(Successful: Boolean); virtual;
    procedure DoBeforeCompile(var Cancel: Boolean); virtual;
    procedure FileOpening(Filename: string;
      var Cancel: Boolean); virtual;
    procedure FileOpened(Filename: string); virtual;
    procedure FileClosing(Filename: string;
      var Cancel: Boolean); virtual;
    procedure PackageInstalled(Filename: string); virtual;
    procedure PackageUninstalled(Filename: string); virtual;
    procedure ProjectOpening(Filename: string;
      var Cancel: Boolean); virtual;
    procedure ProjectOpened(Filename: string); virtual;
    procedure ProjectClosing(Filename: string;
      var Cancel: Boolean); virtual;
    procedure AddedToProject(Filename: string); virtual;
    procedure RemovedFromProject(Filename: string); virtual;
    procedure DefaultDesktopSave(Filename: string); virtual;
    procedure DefaultDesktopLoad(Filename: string); virtual;
    procedure ProjectDesktopSave(Filename: string); virtual;
    procedure ProjectDesktopLoad(Filename: string); virtual;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
  published
    property ModuleNotifier: TModuleNotifier
      read fModuleNotifier write fModuleNotifier;
    { Event handlers for event and file notifications }
    property AfterCompile: TProjectNotifyPostEvent
      read fAfterCompile write fAfterCompile;
    property BeforeCompile: TProjectNotifyPreEvent
      read fBeforeCompile write fBeforeCompile;
    property OnFileOpening: TProjectFilePreEvent
      read fOnFileOpening write fOnFileOpening;
    property OnFileOpened: TProjectFilePostEvent
      read fOnFileOpened write fOnFileOpened;
    property OnFileClosing: TProjectFilePreEvent
      read fOnFileClosing write fOnFileClosing;
    property OnPackageInstalled: TProjectFilePostEvent
      read fOnPackageInstalled write fOnPackageInstalled;
    property OnPackageUninstalled: TProjectFilePostEvent
      read fOnPackageUninstalled write fOnPackageUninstalled;
    property OnProjectOpening: TProjectFilePreEvent
      read fOnProjectOpening write fOnProjectOpening;
    property OnProjectOpened: TProjectFilePostEvent
      read fOnProjectOpened write fOnProjectOpened;
    property OnProjectClosing: TProjectFilePreEvent
      read fOnProjectClosing write fOnProjectClosing;
    property OnAddedToProject: TProjectFilePostEvent
      read fOnAddedToProject write fOnAddedToProject;
    property OnRemovedFromProject: TProjectFilePostEvent
      read fOnRemovedFromProject write fOnRemovedFromProject;
    property OnDefaultDesktopSave: TProjectFilePostEvent
      read fOnDefaultDesktopSave write fOnDefaultDesktopSave;
    property OnDefaultDesktopLoad: TProjectFilePostEvent
      read fOnDefaultDesktopLoad write fOnDefaultDesktopLoad;
    property OnProjectDesktopSave: TProjectFilePostEvent
      read fOnProjectDesktopSave write fOnProjectDesktopSave;
    property OnProjectDesktopLoad: TProjectFilePostEvent
      read fOnProjectDesktopLoad write fOnProjectDesktopLoad;
  end;

  TModuleNotifyEvent = procedure (Sender: TObject; ModuleInterface: TIModuleInterface) of object;
  TComponentRenamedEvent = procedure(Sender: TObject; ModuleInterface: TIModuleInterface;
                                     ComponentHandle: Pointer; const OldName, NewName: string) of object;

  { Manage a list of module notifiers. When a module notifier
    deletes itself, it also removes itself from the list. }
  TNotifierList = class(TOwnerList)
  private
    function GetItems(Index: Integer): TModNotifier;
    procedure SetItems(Index: Integer; Value: TModNotifier);
  public
    property Notifiers[Index: Integer]: TModNotifier read GetItems write SetItems; default;
  end;

  TModuleNotifier = class(TComponent)
  private
    fOnCreate: TModuleNotifyEvent;
    fOnModuleDeleted: TModuleNotifyEvent;
    fOnModuleRenamed: TModuleNotifyEvent;
    fOnEditorModified: TModuleNotifyEvent;
    fOnFormModified: TModuleNotifyEvent;
    fOnEditorSelected: TModuleNotifyEvent;
    fOnFormSelected: TModuleNotifyEvent;
    fOnBeforeSave: TModuleNotifyEvent;
    fOnAfterSave: TModuleNotifyEvent;
    fOnFormSaving: TModuleNotifyEvent;
    fOnProjResModified: TModuleNotifyEvent;
    fOnComponentRenamed: TComponentRenamedEvent;
    fNotifierList: TNotifierList;
  protected
    procedure PostCreate(ModuleInterface: TIModuleInterface); virtual;
    procedure ModuleDeleted(ModuleInterface: TIModuleInterface); virtual;
    procedure ModuleRenamed(ModuleInterface: TIModuleInterface); virtual;
    procedure EditorModified(ModuleInterface: TIModuleInterface); virtual;
    procedure FormModified(ModuleInterface: TIModuleInterface); virtual;
    procedure EditorSelected(ModuleInterface: TIModuleInterface); virtual;
    procedure FormSelected(ModuleInterface: TIModuleInterface); virtual;
    procedure BeforeSave(ModuleInterface: TIModuleInterface); virtual;
    procedure AfterSave(ModuleInterface: TIModuleInterface); virtual;
    procedure FormSaving(ModuleInterface: TIModuleInterface); virtual;
    procedure ProjResModified(ModuleInterface: TIModuleInterface); virtual;
    procedure ComponentRenamed(ModuleInterface: TIModuleInterface;
              ComponentHandle: Pointer; const OldName, NewName: string); virtual;
    property NotifierList: TNotifierList read fNotifierList;
  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure AddNotifier(Filename: string);
  published
    property OnCreate: TModuleNotifyEvent read fOnCreate write fOnCreate;
    property OnModuleDeleted: TModuleNotifyEvent read fOnModuleDeleted write fOnModuleDeleted;
    property OnModuleRenamed: TModuleNotifyEvent read fOnModuleRenamed write fOnModuleRenamed;
    property OnEditorModified: TModuleNotifyEvent read fOnEditorModified write fOnEditorModified;
    property OnFormModified: TModuleNotifyEvent read fOnFormModified write fOnFormModified;
    property OnEditorSelected: TModuleNotifyEvent read fOnEditorSelected write fOnEditorSelected;
    property OnFormSelected: TModuleNotifyEvent read fOnFormSelected write fOnFormSelected;
    property OnBeforeSave: TModuleNotifyEvent read fOnBeforeSave write fOnBeforeSave;
    property OnAfterSave: TModuleNotifyEvent read fOnAfterSave write fOnAfterSave;
    property OnFormSaving: TModuleNotifyEvent read fOnFormSaving write fOnFormSaving;
    property OnProjResModified: TModuleNotifyEvent read fOnProjResModified write fOnProjResModified;
    property OnComponentRenamed: TComponentRenamedEvent read fOnComponentRenamed write fOnComponentRenamed;
  end;

  TModNotifier = class(TIModuleNotifier)
  private
    fModIntf: TIModuleInterface;
    fOwner: TModuleNotifier;
  protected
    property Owner: TModuleNotifier read fOwner;
  public
    constructor Create(Intf: TIModuleInterface; Owner: TModuleNotifier);
    destructor Destroy; override;
    procedure Notify(NotifyCode: TNotifyCode); override;
    {$IFDEF GX_VER140_up}
    procedure ComponentRenamed(const AComponent: TComponent;
      const OldName, NewName: string); override;
    {$ELSE not GX_VER140_up}
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
    {$ENDIF}
    property ModIntf: TIModuleInterface read fModIntf;
  end;

  { Define a project notifer that creates a module notifier
    for every file that the user opens. The project notifier keeps
    the list of module notifiers, so all the modules notifiers can
    be freed when the project notifier is freed. }
  TAddInNotifier = class(TIAddInNotifier)
  private
    fOwner: TRLProjectNotifier;
  public
    constructor Create(Owner: TRLProjectNotifier);
    destructor Destroy; override;
    procedure EventNotification(NotifyCode: TEventNotification;
        var Cancel: Boolean); override;
    procedure FileNotification(NotifyCode: TFileNotification;
              const Filename: string; var Cancel: Boolean); override;
    property Owner: TRLProjectNotifier read fOwner;
  end;

implementation

uses ExptIntf, VirtIntf;

resourcestring
  sCannotGetMainMenu = 'Cannot get main menu';
  sCannotFindMenuItem = 'Cannot find menu item: %s';
  sNoMenuParent = 'No menu parent for %s';

procedure SelectBlock(Editor: TIEditorInterface; Start, After: TCharPos);
begin
  Editor.BlockVisible := False;
  try
    Editor.BlockType := btNonInclusive;
    Editor.BlockStart := Start;
    Editor.BlockAfter := After;
  finally
    Editor.BlockVisible := True;
  end;
end;

{ TRLProjectNotifier }

constructor TRLProjectNotifier.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  if not (csDesigning in ComponentState) then
    fNotifier := TAddInNotifier.Create(Self);
end;

destructor TRLProjectNotifier.Destroy;
begin
  fNotifier.Free;
  fNotifier := nil;

  inherited Destroy;
end;

procedure TRLProjectNotifier.AddNotifier(Filename: string);
begin
  if ModuleNotifier <> nil then
    ModuleNotifier.AddNotifier(Filename);
end;

procedure TRLProjectNotifier.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Operation = opInsert) and (ModuleNotifier = nil) and
     ([csDesigning, csLoading] * ComponentState = [csDesigning]) and
     (Component is TModuleNotifier) then
  begin
    fModuleNotifier := TModuleNotifier(Component);
  end
  else
  if (Operation = opRemove) and (Component = ModuleNotifier) then
    fModuleNotifier := nil
end;

procedure TRLProjectNotifier.FileOpening(Filename: string; var Cancel: Boolean);
begin
  if Assigned(fOnFileOpening) then
    fOnFileOpening(Self, Filename, Cancel);
end;

procedure TRLProjectNotifier.FileOpened(Filename: string);
begin
  if Assigned(fOnFileOpened) then
    fOnFileOpened(Self, Filename);
end;

procedure TRLProjectNotifier.FileClosing(Filename: string; var Cancel: Boolean);
begin
  if Assigned(fOnFileClosing) then
    fOnFileClosing(Self, Filename, Cancel);
end;

procedure TRLProjectNotifier.ProjectOpening(Filename: string; var Cancel: Boolean);
begin
  if Assigned(fOnProjectOpening) then
    fOnProjectOpening(Self, Filename, Cancel);
end;

procedure TRLProjectNotifier.ProjectOpened(Filename: string);
begin
  if Assigned(fOnProjectOpened) then
    fOnProjectOpened(Self, Filename);
end;

procedure TRLProjectNotifier.ProjectClosing(Filename: string; var Cancel: Boolean);
begin
  if Assigned(fOnProjectClosing) then
    fOnProjectClosing(Self, Filename, Cancel);
end;

procedure TRLProjectNotifier.AddedToProject(Filename: string);
begin
  if Assigned(fOnAddedToProject) then
    fOnAddedToProject(Self, Filename);
end;

procedure TRLProjectNotifier.RemovedFromProject(Filename: string);
begin
  if Assigned(fOnRemovedFromProject) then
    fOnRemovedFromProject(Self, Filename);
end;

procedure TRLProjectNotifier.DefaultDesktopSave(Filename: string);
begin
  if Assigned(fOnDefaultDesktopSave) then
    fOnDefaultDesktopSave(Self, Filename);
end;

procedure TRLProjectNotifier.DefaultDesktopLoad(Filename: string);
begin
  if Assigned(fOnDefaultDesktopLoad) then
    fOnDefaultDesktopLoad(Self, Filename);
end;

procedure TRLProjectNotifier.ProjectDesktopSave(Filename: string);
begin
  if Assigned(fOnProjectDesktopSave) then
    fOnProjectDesktopSave(Self, Filename);
end;

procedure TRLProjectNotifier.ProjectDesktopLoad(Filename: string);
begin
  if Assigned(fOnProjectDesktopLoad) then
    fOnProjectDesktopLoad(Self, Filename);
end;

procedure TRLProjectNotifier.DoAfterCompile(Successful: Boolean);
begin
  if Assigned(fAfterCompile) then
    fAfterCompile(Self, Successful);
end;

procedure TRLProjectNotifier.DoBeforeCompile(var Cancel: Boolean);
begin
  if Assigned(fBeforeCompile) then
    fBeforeCompile(Self, Cancel);
end;

procedure TRLProjectNotifier.PackageInstalled(Filename: string);
begin
  if Assigned(fOnPackageInstalled) then
    fOnPackageInstalled(Self, Filename);
end;

procedure TRLProjectNotifier.PackageUninstalled(Filename: string);
begin
  if Assigned(fOnPackageUninstalled) then
    fOnPackageUninstalled(Self, Filename);
end;


{ TModuleNotifier }
{ Add a module notifier for all open modules. }
function EnumFunc(ModNotifier: Pointer;
  const FileName, UnitName, FormName: string): Boolean; stdcall;
begin
  TModuleNotifier(ModNotifier).AddNotifier(Filename);
  Result := True;
end;

{ Create a module notifier component and its notifier list. }
constructor TModuleNotifier.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  fNotifierList := TNotifierList.Create;
  if not (csDesigning in ComponentState) then
  begin
    { Add module notifiers for all modules currently open. }
    ToolServices.EnumProjectUnits(EnumFunc, Self);
  end;
end;

{ Free the notifier list when destroying the component. The
  notifier list automatically unregisters and frees its
  notifiers. }
destructor TModuleNotifier.Destroy;
begin
  fNotifierList.Free;
  fNotifierList := nil;

  inherited Destroy;
end;

{ Add a notifier to the list: get the module interface for a
  file, create a notifier for that module, and add the notifier
  to the list. The notifier automatically registers itself. }
procedure TModuleNotifier.AddNotifier(Filename: string);
var
  ModIntf: TIModuleInterface;
begin
  ModIntf := ToolServices.GetModuleInterface(Filename);
  if ModIntf <> nil then
    try
      NotifierList.Add(TModNotifier.Create(ModIntf, Self));
    finally
      ModIntf.Free;
    end;
end;

procedure TModuleNotifier.PostCreate(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnCreate) then
    fOnCreate(Self, ModuleInterface);
end;

procedure TModuleNotifier.ModuleDeleted(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnModuleDeleted) then
    fOnModuleDeleted(Self, ModuleInterface);
end;

procedure TModuleNotifier.ModuleRenamed(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnModuleRenamed) then
    fOnModuleRenamed(Self, ModuleInterface);
end;

procedure TModuleNotifier.EditorModified(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnEditorModified) then
    fOnEditorModified(Self, ModuleInterface);
end;

procedure TModuleNotifier.FormModified(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnFormModified) then
    fOnFormModified(Self, ModuleInterface);
end;

procedure TModuleNotifier.EditorSelected(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnEditorSelected) then
    fOnEditorSelected(Self, ModuleInterface);
end;

procedure TModuleNotifier.FormSelected(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnFormSelected) then
    fOnFormSelected(Self, ModuleInterface);
end;

procedure TModuleNotifier.BeforeSave(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnBeforeSave) then
    fOnBeforeSave(Self, ModuleInterface);
end;

procedure TModuleNotifier.AfterSave(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnAfterSave) then
    fOnAfterSave(Self, ModuleInterface);
end;

procedure TModuleNotifier.FormSaving(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnFormSaving) then
    fOnFormSaving(Self, ModuleInterface);
end;

procedure TModuleNotifier.ProjResModified(ModuleInterface: TIModuleInterface);
begin
  if Assigned(fOnProjResModified) then
    fOnProjResModified(Self, ModuleInterface);
end;

procedure TModuleNotifier.ComponentRenamed(ModuleInterface: TIModuleInterface;
          ComponentHandle: Pointer; const OldName, NewName: string);
begin
  if Assigned(fOnComponentRenamed) then
    fOnComponentRenamed(Self, ModuleInterface, ComponentHandle, OldName, NewName);
end;


{ TNotifierList }

{ For convenience, access the list as TModNotifier references. }
function TNotifierList.GetItems(Index: Integer): TModNotifier;
begin
  Result := TModNotifier(Items[Index]);
end;

procedure TNotifierList.SetItems(Index: Integer; Value: TModNotifier);
begin
  Items[Index] := Value;
end;


{ Remember the module interface and owner. Register this
  notifier and notify the component that the notifier
  has been created. }
constructor TModNotifier.Create(Intf: TIModuleInterface;
  Owner: TModuleNotifier);
begin
  inherited Create;
  fOwner := Owner;
  fModIntf := Intf;
  fModIntf.AddRef;  { so the notifier can keep its reference
                      to the interface }
  fModIntf.AddNotifier(Self);
  Owner.PostCreate(ModIntf);
end;

{ When freeing the notifier, automatically unregister it,
  and then free the module interface. }
destructor TModNotifier.Destroy;
begin
  if fModIntf <> nil then
  begin
    fModIntf.RemoveNotifier(Self);
    fModIntf.Free;
    fModIntf := nil;
  end;

  inherited Destroy;
end;

{ Notify the expert interface for the interesting events. }
{$IFDEF GX_VER140_up}
procedure TModNotifier.ComponentRenamed(const AComponent: TComponent;
  const OldName, NewName: string);
begin
  Owner.ComponentRenamed(ModIntf, AComponent, OldName, NewName);
end;
{$ELSE not GX_VER140_up}
procedure TModNotifier.ComponentRenamed(ComponentHandle: Pointer;
  const OldName, NewName: string);
begin
  Owner.ComponentRenamed(ModIntf, ComponentHandle, OldName, NewName);
end;
{$ENDIF}


{ Note that when Delphi deletes the module, make sure the
  notifier is also deleted. Do this by removing the notifier
  from the list that contains it. The notifier list will then
  free the notifier. }
procedure TModNotifier.Notify(NotifyCode: TNotifyCode);
begin
  case NotifyCode of
    ncModuleDeleted:
      begin
        Owner.ModuleDeleted(ModIntf);
        Owner.NotifierList.Remove(Self);
      end;
    ncModuleRenamed:
      Owner.ModuleRenamed(ModIntf);
    ncEditorModified:
      Owner.EditorModified(ModIntf);
    ncFormModified:
      Owner.FormModified(ModIntf);
    ncEditorSelected:
      Owner.EditorSelected(ModIntf);
    ncFormSelected:
      Owner.FormSelected(ModIntf);
    ncBeforeSave:
      Owner.BeforeSave(ModIntf);
    ncAfterSave:
      Owner.AfterSave(ModIntf);
    ncFormSaving:
      Owner.FormSaving(ModIntf);
    ncProjResModified:
      Owner.ProjResModified(ModIntf);
  end;
end;



{ TAddInNotifier }
{ Create a project notifier. }
constructor TAddInNotifier.Create(Owner: TRLProjectNotifier);
begin
  inherited Create;
  fOwner := Owner;
  ToolServices.AddNotifierEx(Self);
end;

{ Free all the module notifiers and remove the project notifier. }
destructor TAddInNotifier.Destroy;
begin
  ToolServices.RemoveNotifier(Self);
  inherited Destroy;
end;

procedure TAddInNotifier.EventNotification(NotifyCode: TEventNotification;
var Cancel: Boolean);
begin
  case NotifyCode of
    enAfterCompile:
      Owner.DoAfterCompile(Cancel);
    enBeforeCompile:
      Owner.DoBeforeCompile(Cancel);
  else
    { Unknown notification is probably due to a newer
      version of Delphi. Ignore the event. }
  end;
end;

{ When a file is opened, create a module notifier for it. }
procedure TAddInNotifier.FileNotification(NotifyCode: TFileNotification; const Filename: string; var Cancel: Boolean);
begin
  case NotifyCode of
    fnFileOpening:
      Owner.FileOpening(Filename, Cancel);
    fnFileOpened:
      begin
        Owner.FileOpened(Filename);
        Owner.AddNotifier(Filename);
      end;
    fnFileClosing:
      Owner.FileClosing(Filename, Cancel);
    fnPackageInstalled:
      Owner.PackageInstalled(Filename);
    fnPackageUninstalled:
      Owner.PackageUninstalled(Filename);
    fnProjectOpening:
      Owner.ProjectOpening(Filename, Cancel);
    fnProjectOpened:
      begin
        Owner.ProjectOpened(Filename);
        Owner.AddNotifier(Filename);
      end;
    fnProjectClosing:
      Owner.ProjectClosing(Filename, Cancel);
    fnAddedToProject:
      Owner.AddedToProject(Filename);
    fnRemovedFromProject:
      Owner.RemovedFromProject(Filename);
    fnDefaultDesktopLoad:
      Owner.DefaultDesktopLoad(Filename);
    fnDefaultDesktopSave:
      Owner.DefaultDesktopSave(Filename);
    fnProjectDesktopLoad:
      Owner.ProjectDesktopLoad(Filename);
    fnProjectDesktopSave:
      Owner.ProjectDesktopSave(Filename);
  else
    { Unknown notification is probably due to a newer
      version of Delphi. Ignore the event. }
  end;
end;


{ Find a menu item, or raise an exception for any error. }
function FindMenuItem(const Name: string): TIMenuItemIntf;
var
  MainMenu: TIMainMenuIntf;
begin
  MainMenu := ToolServices.GetMainMenu;
  if MainMenu = nil then
    raise Exception.Create(sCannotGetMainMenu);
  try
    Result := MainMenu.FindMenuItem(Name);
    if Result = nil then
      raise Exception.CreateFmt(sCannotFindMenuItem, [Name]);
  finally
    MainMenu.Free;
  end;
end;


{ Insert a new menu item relative to the item named
  TargetName. Pass the Caption .. EventHandler arguments
  directly to InsertItem. }
function InsertMenuItem(Action: TInsertAction;
  TargetName, Caption, Name, Hint: string;
  ShortCut, Context, GroupIndex: Integer;
  Flags: TIMenuFlags;
  EventHandler: TIMenuClickEvent): TIMenuItemIntf;
var
  TargetItem: TIMenuItemIntf;
  ParentItem: TIMenuItemIntf;
  Index: Integer;
begin
  Result := nil;
  { Get the menu bar interface. }
  with ToolServices.GetMainMenu do
    try
      { Look up the target menu item. }
      TargetItem := FindMenuItem(TargetName);
      if TargetItem = nil then
        raise Exception.CreateFmt(sCannotFindMenuItem, [TargetName]);
      try
        if Action = iaChild then
        begin
          { Create a child of the target item. }
          ParentItem := TargetItem;
          ParentItem.AddRef; { avoid a double free }
          Index := 0;
        end
        else
        begin
          { To create a sibling of the target, get the
            target’s parent. }
          ParentItem := TargetItem.GetParent;
          Index := TargetItem.GetIndex;
          if Action = iaAfter then
            Inc(Index);
        end;
        if ParentItem = nil then
          raise Exception.CreateFmt(sNoMenuParent, [TargetName]);
        try
          Result := ParentItem.InsertItem(Index,
                        Caption, Name, Hint, ShortCut, Context,
                        GroupIndex, Flags, EventHandler);
        finally
          ParentItem.Free;
        end;
      finally
        TargetItem.Free;
      end;
    finally
      Free;
    end;
end;


{ TEditReaderStream }
{ Construct the stream from an editor interface. }
constructor TEditReaderStream.Create(EditIntf: TIEditorInterface);
begin
  inherited Create;
  fReader := EditIntf.CreateReader;
  fSize := -1; { size is unknown }
end;

{ Destroy the stream and free all the interfaces that the stream
  created. }
destructor TEditReaderStream.Destroy;
begin
  fReader.Free;
  fReader := nil;

  inherited Destroy;
end;

{ Read from the file stream or the editor. }
function TEditReaderStream.Read(var Buffer; Count: LongInt): LongInt;
const
  MaxCount = 31*1024;
var
  NRead: Integer;
  NRequest: Integer;
  BufPtr: PChar;
begin
  Assert(fReader <> nil, 'fReader is nil in TEditReaderStream.Read');
  { The initial release of D3 does not handle calls to GetText
    where Count >= 32K. It returns a result equal to Count
    without actually retrieving any text. To circumvent this
    problem, grab buffers of 31K at a time. }
  Result := 0;
  BufPtr := @Buffer;
  while Count > 0 do
  begin
    if Count > MaxCount then
      NRequest := MaxCount
    else
      NRequest := Count;
    NRead := fReader.GetText(fPosition, BufPtr, NRequest);
    Inc(fPosition, NRead);
    Inc(BufPtr, NRead);
    Inc(Result, NRead);
    Dec(Count, NRead);
    { Partially completed read means end-of-buffer, so remember
      the buffer size. If NRead = 0, the position might be past
      the end of file, so save the size only when NRead > 0. }
    if (fSize < 0) and (NRead > 0) and (NRead < NRequest) then
      fSize := fPosition;
  end;
end;

{ Seek to a new position. }
function TEditReaderStream.Seek(Offset: LongInt; Origin: Word): LongInt;
begin
  case Origin of
    soFromBeginning:    fPosition := Offset;
    soFromCurrent:      fPosition := fPosition + Offset;
    soFromEnd:          fPosition := Size + Offset;
  else
    raise Exception.CreateFmt('Invalid seek origin, %d', [Origin]);
  end;
  Result := fPosition;
end;

function TEditReaderStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  raise Exception.Create('Attempt to write to readonly stream!');
end;

{ If the stream user must seek relative to the end of the
  stream, then you need to know the size of the stream.
  There is no simple way to determine this. Instead, use
  a binary search to find a position where a single byte
  read is valid, and a read of the subsequent byte is invalid.
  Since this is such a pain, cache the size after the first call,
  and return the cached size for subsequent calls. }
function TEditReaderStream.GetSize: LongInt;
const
  BufSize = 16384;
var
//  Hi, Lo, Mid: LongInt;
//  Ch: Char;
  CurSize, BytesRead: Longint;
  Buf: array[0..BufSize] of Char;
begin
  Assert(fReader <> nil, 'fReader is nil in TEditReaderStream.GetSize');

  if fSize < 0 then
  begin
    // *VERY* slow but less crashy replacement by EB 2/2001
    // This works around an IDE bug calling GetText with large values
    CurSize := 0;
    repeat
      BytesRead := fReader.GetText(CurSize, Buf, BufSize);
      Inc(CurSize, BytesRead);
    until BytesRead < BufSize;
    fSize := CurSize;
  end;
  Result := fSize;

(*
  if fSize < 0 then
  begin
    Hi := High(LongInt);
    Lo := 0;
    while Lo <= Hi do
    begin
      Mid := (Hi + Lo) div 2;
      try
        if fReader.GetText(Mid, @Ch, 1) = 1 then  // The IDE crashes here too often
          Lo := Mid+1
        else
          Hi := Mid-1;
      except
        on E: Exception do
        begin
          Lo := 0;
          Break;
        end;
      end;
    end;
    fSize := Lo;
  end;
  Result := fSize;
  *)
end;


{ TEditorStrings }

{ Create an edit reader string list. }
constructor TEditorStrings.Create(Editor: TIEditorInterface);
begin
  inherited Create;
  fStrings := TStringList.Create;
  LoadFromEditor(Editor);
end;

destructor TEditorStrings.Destroy;
begin
  fStrings.Free;
  fStrings := nil;

  inherited Destroy;
end;

{ Load a string list from an editor interface. Read the edit
  reader as a stream. As each line is added to the string list,
  remember the position of that line in the stream. }
procedure TEditorStrings.LoadFromEditor(Editor: TIEditorInterface);
var
  ERStream: TEditReaderStream;
  StrStream: TStringStream;
  Str: PChar;
  Pos, I: Integer;
begin
  ERStream := TEditReaderStream.Create(Editor);
  try
    StrStream := TStringStream.Create('');
    try
      { Read the entire buffer into StrStream. }
      StrStream.CopyFrom(ERStream, 0);
      { Copy every line from StrStream to the string list. }
      Strings.Text := StrStream.DataString;

      { Scan the string to find the buffer position of each line. }
      Str := PChar(StrStream.DataString);
      Pos := 0;
      for I := 0 to Count-1 do
      begin
        Strings.Objects[I] := TObject(Pos);
        Inc(Pos, Length(Strings[I]));
        if Str[Pos] = #13 then
          Inc(Pos);
        if Str[Pos] = #10 then
          Inc(Pos);
      end;
    finally
      StrStream.Free;
    end;
  finally
    ERStream.Free;
  end;
end;

{ Save the string list to an editor interface. The string list
  does not keep track of specific changes, so replace the entire
  file with the text of the string list. }
procedure TEditorStrings.SaveToEditor(Editor: TIEditorInterface);
var
  Writer: TIEditWriter;
begin
  Writer := Editor.CreateUndoableWriter;
  try
    Writer.DeleteTo(High(LongInt));
    Writer.Insert(PChar(fStrings.Text));
  finally
    Writer.Free;
  end;
end;

{ Get a string. }
function TEditorStrings.Get(Index: Integer): string;
begin
  Result := Strings[Index];
end;

{ Get an object, which is really the string position. }
function TEditorStrings.GetObject(Index: Integer): TObject;
begin
  Result := Strings.Objects[Index];
end;

{ Set a string. }
procedure TEditorStrings.Put(Index: Integer; const Str: string);
begin
  Strings[Index] := Str;
end;

{ Set a string's position. }
procedure TEditorStrings.PutObject(Index: Integer; Obj: TObject);
begin
  Objects[Index] := Obj;
end;

{ Return the number of lines in the list. }
function TEditorStrings.GetCount: Integer;
begin
  Result := Strings.Count;
end;

procedure TEditorStrings.Clear;
begin
  Strings.Clear;
end;

procedure TEditorStrings.Delete(Index: Integer);
begin
  Strings.Delete(Index);
end;

procedure TEditorStrings.Insert(Index: Integer; const Str: string);
begin
  Strings.Insert(Index, Str);
end;

{ For convenience, return a position as an integer. }
function TEditorStrings.GetPosition(Index: Integer): LongInt;
begin
  Result := LongInt(Strings.Objects[Index]);
end;

{ Return a position as a character position. }
function TEditorStrings.GetCharPos(Index: Integer): TCharPos;
begin
  Result := PosToCharPos(GetPosition(Index));
end;

{ Get the buffer position given a character position.
  The character position position specifies a line of text.
  Retrieve the buffer position for the start of that line,
  and add the character index. If the character index is
  past the end of line, return the position of the line
  ending. }
function TEditorStrings.CharPosToPos(CharPos: TCharPos): LongInt;
var
  Text: string;
begin
  { CharPos.Line is 1-based; Strings list is 0-based. }
  Text := Strings[CharPos.Line-1];
  if CharPos.CharIndex > Length(Text) then
    Result := Position[CharPos.Line-1] + Length(Text)
  else
    Result := Position[CharPos.Line-1] + CharPos.CharIndex;
end;

{ Convert a buffer position to a character position.
  Search for the line such that Pos is between the start
  and end positions of the line. That specifies the line
  number. The char index is the offset within the line.
  If Pos lies within a line ending, return the character
  index of the end of the line.

  Line indices are 1-based, and string list indices are
  0-based, so add 1 to get the true line number.

  Use binary search to locate the desired line quickly. }
function TEditorStrings.PosToCharPos(Pos: LongInt): TCharPos;
var
  Lo, Mid, Hi: Integer;
begin
  Lo := 0;
  Hi := Strings.Count-1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    if Position[Mid] <= Pos then
      Lo := Mid+1
    else
      Hi := Mid-1
  end;

  Result.Line := Lo;
  if Pos >= Position[Lo-1]+Length(Strings[Lo-1]) then
    Result.CharIndex := Length(Strings[Lo-1])
  else
    Result.CharIndex := Pos - Position[Lo-1];
end;


{ ReplaceSelection }
{ Replace the current selection with Text, and select Text. The
  caller must specify the view number where the replacement text
  is scrolled into view. }
procedure ReplaceSelection(Editor: TIEditorInterface;
  ViewNum: Integer; Text: string);
var
  Start, After: TCharPos;
  Strings: TEditorStrings;
  View: TIEditView;
  TopPos: TEditPos;

  { Replace a columnar selection with a single string.
    Delete a portion of each line within the block, and insert
    the replacement text on the first line. The Start position
    is the upper left corner. The After position is the lower
    right corner. }
  procedure ReplaceColumns(Start, After: TEditPos; RplText: string);
  var
    EditPos: TEditPos;
    StartCharPos: TCharPos;
    AfterCharPos: TCharPos;
    Line: Integer;
    TmpText: string;
  begin
    { For each line in the block... }
    for Line := Start.Line to After.Line do
    begin
      { Determine the starting and ending character indices
        for the current line. }
      EditPos.Line := Line;
      EditPos.Col := Start.Col;
      View.ConvertPos(True, EditPos, StartCharPos);
      EditPos.Col := After.Col;
      View.ConvertPos(True, EditPos, AfterCharPos);
      { Delete the selected portion of the line and insert the
        replacement text so it starts at the same position.
        Lines numbers are 1-based, and the string list
        is 0-based, so subtract 1 for the line index. Strings
        indices are 1-based, but character position indices
        are 0-based, or add 1 for the character index. The After
        column is inclusive. }
      TmpText := Strings[Line-1];
      Delete(TmpText, StartCharPos.CharIndex+1,
        AfterCharPos.CharIndex-StartCharPos.CharIndex+1);
      Insert(RplText, TmpText, StartCharPos.CharIndex+1);
      Strings[Line-1] := TmpText;

      { Clear RplText so the replacement occurs only on the first line. }
      RplText := '';
    end;
  end;

  { Replace the text between Start and After, inclusive, with Text.
    IsInclusive determines whether the last selected character is part
    of the replaced block or not. }
  procedure ReplaceInclusiveExclusive(const IsInclusive: Boolean);
  var
    Writer: TIEditWriter;
    StartPos: Integer;
    AfterPos: Integer;
    DeleteToPos: Integer;
    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
      FirstCharInLineDeleted: Boolean;
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
  begin
    if not IsInclusive then
    {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
    begin
      FirstCharInLineDeleted := (After.CharIndex = 1);
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    end
    else
      FirstCharInLineDeleted := False;
    {$ELSE}
      if After.CharIndex > 0 then
        Dec(After.CharIndex);
    {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}

    StartPos := View.CharPosToPos(Start);
    Assert(StartPos >= 0, 'StartPos < 0');
    AfterPos := View.CharPosToPos(After);
    Assert(AfterPos >= 0, 'AfterPos < 0');
    Writer := Editor.CreateUndoableWriter;
    try
      { Copy the initial part of the file, up to the selection. }
      Writer.CopyTo(StartPos);
      { Delete the block if there is one to delete. }
      DeleteToPos := AfterPos;

      if (After.CharIndex = 0) and (After.Line - Start.Line = 1) then
      begin
        Dec(DeleteToPos, Length(#13#10));
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
        begin
          Inc(DeleteToPos, Length(#13#10));
          Inc(DeleteToPos);
        end;
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}
      end
      else
      begin
        {$IFDEF GX_WorkAroundFirstCharInLineSelectionBug}
        if FirstCharInLineDeleted then
        begin
          Inc(DeleteToPos);
        end
        else
        if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ELSE}
        if After.CharIndex > 0 then
          Inc(DeleteToPos);
        {$ENDIF GX_WorkAroundFirstCharInLineSelectionBug}

        //begin
        //! StH: IFF the block is selected up to a line's end AND
        //! if the block comprises of more than a single line, then
        //! we want to additionally Inc(DeleteToPos, Length(#13#10))
        //! Problem: How do we find out in a clean manner that we ARE at a line's end?
        //end
      end;
      if DeleteToPos > StartPos then
        Writer.DeleteTo(DeleteToPos);
      { Insert the replacement text. }
      Writer.Insert(PChar(Text));
      { Copy the rest of the file. }
      Writer.CopyTo(High(LongInt));
    finally
      Writer.Free;
    end;
  end;

begin
  Start := Editor.BlockStart;
  After := Editor.BlockAfter;

  Assert(Editor.GetViewCount > ViewNum, 'Editor.GetViewCount <= passed in ViewNum');
  View := Editor.GetView(ViewNum);
  TopPos := View.TopPos;
  if View <> nil then
  try
    Strings := TEditorStrings.Create(Editor);
    try
      case Editor.BlockType of

        btColumn:
          begin
            ReplaceColumns(TEditPos(Start), TEditPos(After), Text);
            Strings.SaveToEditor(Editor);
          end;

        btInclusive:
          begin
            ReplaceInclusiveExclusive(True);
          end;

        btNonInclusive:
          begin
            ReplaceInclusiveExclusive(False);
          end;

        btLine:
          begin
            Start.CharIndex := 0;              { start of line }
            //After.CharIndex := High(SmallInt); { entire line }
            // Edited by EB to prevent assertion failures when View.CharPosToPos(After)
            // returns -1 above on values over 1023 (last tested in D5)
            After.CharIndex := 1023;
            ReplaceInclusiveExclusive(True);
          end;
      else
        raise Exception.CreateFmt('Invalid block type, %d', [Ord(Editor.BlockType)]);
      end;
    finally
      Strings.Free;
    end;
  finally
    View.TopPos := TopPos;
    View.Free;
  end;
end;


end.
