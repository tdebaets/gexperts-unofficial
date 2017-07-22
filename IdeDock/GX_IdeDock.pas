unit GX_IdeDock;

// Original Author: Python <python@softhome.net>

{
  Making an IDE docking form:

  * A dockable form must have a global variable, which must be set to nil
    when the form is destroyed (Delphi uses this variable to get the form).
  * Delphi creates the form when it needs one using the default constructor
    (and assigns it to the above-mentioned variable), so you cannot make use
    of values which must be assigned by other code after your form is created.
  * When your form is docked it isn't a window so you can't rely on anything
    which expects your form to be a window (like a main menu).
  * Call inherited in your OnCreate and OnDestroy event handlers
  * To show a form use IdeDockManager.ShowForm(Form1) (this also shows the
    form when it is docked).
  * Register your form using
      IdeDockManager.RegisterDockableForm(TForm1, Form1, 'Form1')
    and unregister your form using
      IdeDockManager.UnregisterDockableForm('Form1').
    When your form is registered, Delphi can automatically create the form,
    so you usually register your form when your expert is activated.
}

{$I GX_CondDefine.inc}

interface

uses
  Windows, SysUtils, Classes, IniFiles, Forms, Controls,
  {$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
  MenuBar, Messages,
  {$ENDIF}{$ENDIF}
  GX_FakeIdeDock;

type
  TfmIdeDockForm = class(TfmFakeIdeDockForm)
  protected
    {$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
    FMenuBar: TMenuBar;
    {$ENDIF}{$ENDIF}
    procedure LoadDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); override;
    procedure LoadDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); override;
    procedure SaveDockClients(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); override;
    procedure SaveDockStream(MemIniFile: TMemIniFile; const AString: AnsiString; Control: TWinControl); override;
    procedure ZoomWindow; override;
    procedure EditAction; override;
    procedure GetEditState; override;
    procedure CreateDockParent(Message: TCMDockClient); override;
    procedure SetDockable(const Value: Boolean); override;
    procedure ForceShow; override;
    procedure SetName(const NewName: TComponentName); override;
  public
    procedure SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean); override;
    procedure LoadWindowState(MemIniFile: TMemIniFile); override;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    {$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
    function IsShortCut(var Message: TWMKey): Boolean; override;
    {$ENDIF}{$ENDIF}
  published
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

  TIdeDockFormClass = class of TfmIdeDockForm;

type
  EIdeDockError = class(Exception);

  TRegisterDesktopFormProc = procedure(DesktopFormClass: TFormClass;
                                       const String1, String2: string); register;
  TShowFormProc = procedure (Form: TForm); register;

  TIdeDockManager = class(TObject)
  private
    FIdeDockingEnable: Boolean;
    FDockFormClass: TFormClass;
    FDockForms: TStringList;
    FDesktopFormClassItems: TList;
    FRegisterDesktopFormProc: TRegisterDesktopFormProc;
    FShowFormProc: TShowFormProc;
    FCorIdeLib: HInst;
  protected
    function GetDockFormClass: TFormClass; virtual;
    function GetDockForms: TStringList; virtual;
    function GetDesktopFormClassItems: TList; virtual;
    function GetRegisterDesktopFormProc: TRegisterDesktopFormProc;
    function GetShowFormProc: TShowFormProc;
    property DockForms: TStringList read FDockForms;
    property DesktopFormClassItems: TList read FDesktopFormClassItems;
    property RegisterDesktopFormProc: TRegisterDesktopFormProc read FRegisterDesktopFormProc;
    property ShowFormProc: TShowFormProc read FShowFormProc;
    property CorIdeLib: HInst read FCorIdeLib;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string); virtual;
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string); virtual;
    procedure ShowForm(Form: TForm);

    property DockFormClass: TFormClass read FDockFormClass;
    property IdeDockingEnable: Boolean read FIdeDockingEnable;
  public
    // Override de/allocator functions to implement Singleton behaviour.
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

function IdeDockManager: TIdeDockManager;

implementation

uses
  GX_VerDepConst,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  Menus, VMTUtils;

{$R *.DFM}

const
  LoadedDesktopFormInstances = '@Desktop@LoadedDesktopFormInstances';
  // Desktop.LoadedDesktopFormInstances

  BorlandIDEName = '@Ideintf@BorlandIDE';
  // Ideintf.BorlandIDE

  FocusWindow = '@Uiutils@FocusWindow$qqrp11Forms@TForm';
  // Uiutils.FocusWindow(Form: TForm);

  TDockableFormName = 'TDockableForm';

{$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
type
  // PopupMenu to support shortcuts for the menu bar for docked forms.
  TDummyPopupMenu = class(TPopupMenu)
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
  end;
{$ENDIF}{$ENDIF}

var
  IdeDockFormClass: TClass = TfmIdeDockForm;

{$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
function TDummyPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
begin
  // Call the forms IsShortCut.  When a form is docked this normally isn't done
  Result := (Owner as TForm).IsShortCut(Message);
end;
{$ENDIF}{$ENDIF}


procedure TfmIdeDockForm.CreateParams(var Params: TCreateParams);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited CreateParams(Params);
end;
{$ELSE}
type
  TCreateParamsMethod = procedure(Self: TObject; var Params: TCreateParams); register;
begin
  TCreateParamsMethod(GetVirtualMethod(GetClassParent(TfmIdeDockForm), 37))(Self, Params);
end;
{$ENDIF EnableIdeDockingSupport}


constructor TfmIdeDockForm.Create(AOwner: TComponent);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited Create(AOwner);
end;
{$ELSE}
type
  TCreateMethod = procedure(Self: TObject; AllocMem: Boolean; AOwner: TComponent); register;
begin
  TCreateMethod(GetVirtualMethod(GetClassParent(TfmIdeDockForm), 11))(Self, False, AOwner);
end;
{$ENDIF EnableIdeDockingSupport}


type
  TNotifyMethod = procedure (Self, Sender: TObject); register;


procedure TfmIdeDockForm.FormCreate(Sender: TObject);
begin
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited FormCreate(Sender);
end;
{$ELSE}
  TNotifyMethod(GetClassParent(TfmIdeDockForm).MethodAddress('FormCreate'))(Self, Sender);
  if Menu <> nil then
  begin
    FMenuBar := TMenuBar.Create(Self);
    FMenuBar.Parent := Self;
    FMenuBar.Menu := Menu;
    FMenuBar.Height := GetSystemMetrics(SM_CYMENU) + 2;
    Menu := nil;
  end;
  if PopupMenu = nil then
    PopupMenu := TDummyPopupMenu.Create(Self);
{$ENDIF EnableIdeDockingSupport}
end;


{$IFDEF GX_VER120_up}{$IFDEF EnableIdeDockingSupport}
function TfmIdeDockForm.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := False;
  if Assigned(FMenuBar) and Assigned(FMenuBar.Menu) then
    Result := FMenuBar.Menu.IsShortCut(Message);
  if not Result then
    Result := inherited IsShortCut(Message);
end;
{$ENDIF}{$ENDIF}


procedure TfmIdeDockForm.FormDestroy(Sender: TObject);
begin
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited FormDestroy(Sender);
end;
{$ELSE}
  TNotifyMethod(GetClassParent(TfmIdeDockForm).MethodAddress('FormDestroy'))(Self, Sender);
{$ENDIF EnableIdeDockingSupport}
end;


procedure TfmIdeDockForm.SetName(const NewName: TComponentName);
begin
{$IFDEF EnableIdeDockingSupport}
  AnsiString(Pointer(Integer(Self) + 712)^) := NewName;
{$ENDIF EnableIdeDockingSupport}

  inherited SetName(NewName);
end;

//*******************************************************
//
// +++ Explanation of the following assembler code +++
//
// We exploit the fact that a virtual method call always
// looks like this:
//
//   mov eax, [eax]
//   call [eax]
//
// We always have the return address on the stack and
// just squeeze in an additional invisible call in order
// to get to IDE's virtual method.
// More explanation below.
//
//*******************************************************
procedure TfmIdeDockForm.LoadDockClients(MemIniFile: TMemIniFile; const AString: AnsiString;
                                         Control: TWinControl);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited LoadDockClients(MemIniFile, AString, Control);
end;
{$ELSE}
asm
        PUSH    EAX       // [1]  -- this can be any 32 bit value; SUB ESP, 4 would do, too.

        PUSH    EAX       // save eax = self
        PUSH    EDX       // save edx = MemIniFile
        PUSH    ECX       // save ecx = AString

        // inherited LoadDockClients
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent  // get the simulated class parent in the IDE
                                     //   - remember: we squeezed ourselves in!
        MOV     EDX, 53 + DockVmtOfs  // LoadDockClients is index 53 in the VMT (D4, BCB4)
        CALL    GetVirtualMethod     // get pointer to IDE's LoadDockClients

        //
        // Upon returning from GetVirtualMethod, EAX contains the
        // address of the IDE's virtual LoadDockClients methods.
        // Now we call this method, exploiting that during a call
        // the return address is always pushed on the stack.
        //
        // In [1] above we basically added an artificial return address
        // to the stack. At this stage, the return address is at ESP+12
        // since we pushed 3*4 bytes after the address.
        //
        // Upon exit of this assembler method, the RET instruction will
        // set EIP to "inherited LoadDockClients", so that we effectively
        // call "inherited LoadDockClients". When execution enters
        // the "inherited LoadDockClients" in the IDE the stack is set up
        // exactly the same way as if the virtual method call had gone
        // directly to the IDE's virtual method.
        //
        // This trick is only employed since due to register
        // calling convention there is no "free" register; if there
        // was a free register, we could easily JMP [free register]
        // which accomplishes exactly the same in the end.
        //
        MOV     [ESP+12], EAX    // patch "inherited LoadDockClients" as return address

        POP     ECX       // restore ecx = AString
        POP     EDX       // restore edx = MemIniFile
        POP     EAX       // restore eax = self
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.LoadDockStream(MemIniFile: TMemIniFile; const AString: AnsiString;
                                        Control: TWinControl);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited LoadDockStream(MemIniFile, AString, Control);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited LoadDockStream
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 54 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.SaveDockClients(MemIniFile: TMemIniFile; const AString: AnsiString;
                                         Control: TWinControl);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited SaveDockClients(MemIniFile, AString, Control);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited SaveDockClients
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 55 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.SaveDockStream(MemIniFile: TMemIniFile; const AString: AnsiString;
                                        Control: TWinControl);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited SaveDockStream(MemIniFile, AString, Control);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited SaveDockClients
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 56 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}

procedure TfmIdeDockForm.ZoomWindow;
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited ZoomWindow;
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited EditAction
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 57 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}

procedure TfmIdeDockForm.EditAction;
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited EditAction;
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited EditAction
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 58 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.GetEditState;
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited GetEditState;
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited GetEditState
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 59 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.SaveWindowState(MemIniFile: TMemIniFile; ABoolean: Boolean);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited SaveWindowState(MemIniFile, ABoolean);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited SaveWindowState
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 60 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.LoadWindowState(MemIniFile: TMemIniFile);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited LoadWindowState(MemIniFile);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited LoadWindowState
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 61 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.CreateDockParent(Message: TCMDockClient);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited CreateDockParent(Message);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited CreateDockParent
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 62 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.SetDockable(const Value: Boolean);
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited SetDockable(Value);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited SetDockable
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 63 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


procedure TfmIdeDockForm.ForceShow;
{$IFNDEF EnableIdeDockingSupport}
begin
  inherited ForceShow;
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX

        // inherited ForceShow
        MOV     EAX, IdeDockFormClass
        CALL    TObject.ClassParent
        MOV     EDX, 64 + DockVmtOfs
        CALL    GetVirtualMethod

        MOV     [ESP+12], EAX
        POP     ECX
        POP     EDX
        POP     EAX
end;
{$ENDIF EnableIdeDockingSupport}


{ TIdeDockManager }

constructor TIdeDockManager.Create;
resourcestring
  SCorIdeLoadingError = 'Cannot load IDE core %s';
  SIdeDockingNotSupported = 'IDE docking not supported';
{.$DEFINE ScanVirtualMethods}
{$IFDEF ScanVirtualMethods}
type
  PPointer = ^Pointer;
var
  VmtCount: Integer;
  VAddress: Pointer;
{$ENDIF ScanVirtualMethods}
begin
  inherited Create;

  {$IFDEF EnableIdeDockingSupport}
    // do get a reference to the core IDE module - if it is
    // not there, we are in an IDE that we do not support.
  try
    FCorIdeLib := GetModuleHandle(CorIdeLibName);
    if FCorIdeLib = 0 then
      raise EIdeDockError.CreateFmt(SCorIdeLoadingError, [CorIdeLibName]);

    FDockFormClass := GetDockFormClass;

    {$IFDEF ScanVirtualMethods}
    VmtCount := GetVirtualMethodCount(FDockFormClass);
    while VmtCount > 0 do
    begin
      VAddress := Pointer(GetVirtualMethod(FDockFormClass, VmtCount));
      Dec(VmtCount);

      OutputDebugString(PChar(Format('%.8x --> %d', [Integer(VAddress), VmtCount])));
    end;

    VmtCount := FDockFormClass.InstanceSize;
    OutputDebugString(PChar(Format('Added size: %d (%d DWORDs)', [VmtCount - TForm.InstanceSize, (VmtCount - TForm.InstanceSize) div 4])));
    asm int 3 end;
    {$ENDIF ScanVirtualMethods}
    {$UNDEF ScanVirtualMethods}

    FDockForms := GetDockForms;
    FDesktopFormClassItems := GetDesktopFormClassItems;
    FShowFormProc := GetShowFormProc();

    FRegisterDesktopFormProc := GetRegisterDesktopFormProc();
    FIdeDockingEnable := True;
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebug('Docking initialization error: ' + E.Message);{$ENDIF}
      FIdeDockingEnable := False;
    end;
  end;
  {$ELSE}
    //! StH: Rather than throwing an exception, we should
    // do nothing. Calls to docking features then result in
    // dummy operations. This simplifies code management for
    // Delphi 3.0 and C++Builder 3.0 dramatically, since the
    // only IFDEF'ed code is here. All other calls to docking
    // support can be left as-is.

    // raise EIdeDockError.Create(SIdeDockingNotSupported);
  FIdeDockingEnable := False;
  {$ENDIF EnableIdeDockingSupport}
end;


destructor TIdeDockManager.Destroy;
begin
  inherited Destroy;
end;


function TIdeDockManager.GetDockFormClass: TFormClass;
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SDockableFormClassNotFound = 'IDE dockable form class not found';
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Screen.FormCount -1 do
    if Screen.Forms[I].ClassParent.ClassName = TDockableFormName then
    begin
      Result := TFormClass(Screen.Forms[I].ClassParent);
      Break;
    end;

  if Result = nil then
    raise EIdeDockError.Create(SDockableFormClassNotFound);
end;
{$ELSE}
begin
  Result := TfmFakeIdeDockForm; // dummy return to kill compiler warning
end;
{$ENDIF EnableIdeDockingSupport}


function TIdeDockManager.GetDockForms: TStringList;
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SBorlandIdeObjectNotFound = 'BorlandIde object not found';
  SListOfFormsNotFound = 'List of forms not found in BorlandIde object';
  SErrorGettingFormList = 'Error "%s" while getting list of forms';
type
  PPointer = ^Pointer;
var
  p1: PPointer;
begin
  p1 := GetProcAddress(CorIdeLib, BorlandIDEName);
  if p1 = nil then
    raise EIdeDockError.Create(SBorlandIdeObjectNotFound);

      try
  {$UNDEF DockingSupportDefined}
  {$IFDEF VER120}
    {$DEFINE DockingSupportDefined}
  {$ENDIF VER120}

  {$IFDEF VER125}
    {$DEFINE DockingSupportDefined}
  {$ENDIF VER125}

  {$IFNDEF DockingSupportDefined}
    Unknown compiler
  {$ENDIF DockingSupportDefined}
  {$UNDEF DockingSupportDefined}

    Result := TStringList(PPointer(Integer(p1^) - $4)^);

    if Result.ClassType <> TStringList then
      raise EIdeDockError.Create(SListOfFormsNotFound);

  except
    on E: EAccessViolation do
      raise EIdeDockError.CreateFmt(SErrorGettingFormList, [E.Message]);
  end;
end;
{$ELSE}
begin
  Result := nil; // dummy return to kill compiler warning
end;
{$ENDIF EnableIdeDockingSupport}


function TIdeDockManager.GetDesktopFormClassItems: TList;
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SLoadedDesktopFormInstancesObjectNotFound = 'LoadedDesktopFormInstances object not found';
  DesktopFormClassItemsListNotFound = 'DesktopFormClassItems list not found';
  SErrorGettingDesktopFormClassItems = 'Error %s getting DesktopFormClassItems';
type
  PPointer = ^Pointer;
var
  p1: Pointer;
begin
  p1 := GetProcAddress(CorIdeLib, LoadedDesktopFormInstances);
  if p1 = nil then
    raise EIdeDockError.Create(SLoadedDesktopFormInstancesObjectNotFound);
  try
    Result := TList(PPointer(Integer(p1) + $8)^);
    {$IFOPT D+}SendDebug(Result.ClassName);{$ENDIF}
    if Result.ClassType <> TList then
      raise EIdeDockError.Create(DesktopFormClassItemsListNotFound);
  except
    on E: EAccessViolation do
      raise EIdeDockError.CreateFmt(SErrorGettingDesktopFormClassItems, [E.Message]);
  end;
end;
{$ELSE}
begin
  Result := nil; // dummy return to kill compiler warning
end;
{$ENDIF EnableIdeDockingSupport}


function TIdeDockManager.GetRegisterDesktopFormProc: TRegisterDesktopFormProc;
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SRegisterDesktopFormNotFound = 'RegisterDesktopForm procedure not found not in %s';
begin
  Result := GetProcAddress(CorIdeLib, RegisterDesktopFormClassName);
  if @Result = nil then
    raise EIdeDockError.CreateFmt(SRegisterDesktopFormNotFound, [corideLibName]);
end;
{$ELSE}
begin
  Result := nil; // dummy return to kill compiler warning
end;
{$ENDIF EnableIdeDockingSupport}

function TIdeDockManager.GetShowFormProc: TShowFormProc;
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SFocusWindowNotFound = 'FocusWindow procedure not found not in %s';
begin
  Result := GetProcAddress(CorIdeLib, FocusWindow);
  if @Result = nil then
    raise EIdeDockError.CreateFmt(SFocusWindowNotFound, [corideLibName]);
end;
{$ELSE}
begin
  Result := nil; // dummy return to kill compiler warning
end;
{$ENDIF EnableIdeDockingSupport}

procedure TIdeDockManager.ShowForm(Form: TForm);
begin
{$IFDEF EnableIdeDockingSupport}
  if IdeDockingEnable then
    ShowFormProc(Form)
  else
{$ENDIF EnableIdeDockingSupport}
    Form.Show;
end;

procedure TIdeDockManager.RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
begin
{$IFDEF EnableIdeDockingSupport}
  if not IdeDockingEnable then
    Exit;
  Assert(DockForms <> nil);

  DockForms.AddObject(IdeDockFormName, TObject(@IdeDockFormVar));
  RegisterDesktopFormProc(IdeDockFormClass, IdeDockFormName, IdeDockFormName);

  {$IFOPT D+}SendDebug('Form named ' + ideDockFormName + ' registered');{$ENDIF}
{$ENDIF EnableIdeDockingSupport}
end;


procedure TIdeDockManager.UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);
{$IFDEF EnableIdeDockingSupport}
resourcestring
  SIdeDockFormNotFound = 'Ide dock form "%s" not found.';
var
  ClassNamePtr: PString;
  I: Integer;
begin
  if not IdeDockingEnable then Exit;
  Assert(DockForms <> nil);

  I := DockForms.IndexOf(IdeDockFormName);

  if I = -1 then
    raise EIdeDockError.CreateFmt(SIdeDockFormNotFound, [IdeDockFormName]);

  DockForms.Delete(I);

  for I := 0 to DesktopFormClassItems.Count -1 do
  begin
    ClassNamePtr := PString(Integer(DesktopFormClassItems[I]) + 4);
    if ClassNamePtr^ = IdeDockFormName then // Note: case-sensitive comparison
    begin
      TObject(DesktopFormClassItems[I]).Free;
      Break;
    end;
  end;
end;
{$ELSE}
begin {empty} end;
{$ENDIF EnableIdeDockingSupport}


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

{.$DEFINE VerifyVmtCorrectness}
{$IFDEF VerifyVmtCorrectness}
var
  DummyMsg: TCMDockClient;
  Params: TCreateParams;
{$ENDIF VerifyVmtCorrectness}

initialization
  PrivateIdeDockManager := TIdeDockManager.Create;

{$IFDEF EnableIdeDockingSupport}
  // "squeeze" ourselves into the IDE's form hierarchy; this
  // fakes that we are descending from the IDE's dockable form
  // class while in fact we don't.
  if PrivateIdeDockManager.IdeDockingEnable then
  begin
    SetClassParent(TfmIdeDockForm, PrivateIdeDockManager.DockFormClass);

    {$IFDEF VerifyVmtCorrectness}
      with TfmIdeDockForm.Create(nil) do
      begin
        //+LoadDockClients(nil, '', nil);
        //+LoadDockStream(nil, '', nil);
        //+SaveDockClients(nil, '', nil);
        //+SaveDockStream(nil, '', nil);
        //+ZoomWindow;
        //+EditAction;
        //+GetEditState;
        //+CreateDockParent(DummyMsg);
        //+SetDockable(False);
        //+ForceShow;
        //+SetName('');

        //+SaveWindowState(nil, False);
        //+LoadWindowState(nil);
        //+Create(nil);
        //+CreateParams(Params);

        //+FormCreate(nil);
        //+FormDestroy(nil);
      end;
    {$ENDIF VerifyVmtCorrectness}
    {$UNDEF VerifyVmtCorrectness}
  end;

{$ENDIF EnableIdeDockingSupport}

finalization
  CanFreeIdeDockManager := True;
  PrivateIdeDockManager.Free;
  PrivateIdeDockManager := nil;

end.
