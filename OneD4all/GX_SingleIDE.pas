unit GX_SingleIDE;

// Delphi 4.0 and C++Builder 4.0 have their own single-instance
// handling, so this expert is no longer needed there. Consequently
// only "enable" it in Delphi 3 and C++Builder 3.0.

{$I GX_CondDefine.inc}

// By default disable this expert
{$UNDEF GX_UseIdeSingleInstanceExpert}

{$IFDEF VER100}  // Delphi 3.0
  {$DEFINE GX_UseIdeSingleInstanceExpert}
{$ENDIF VER100}

{$IFDEF VER110}  // C++Builder 3.0
  {$DEFINE GX_UseIdeSingleInstanceExpert}
{$ENDIF VER110}

{$IFNDEF GX_UseIdeSingleInstanceExpert}
interface implementation
{$ELSE GX_UseIdeSingleInstanceExpert}

{$IFDEF VER110}
  {$D-} // work around a bug in C++Builder 3.0
  
{$ENDIF VER110}


// This switch determines whether to use the "old" WM_COPYDATA
// trick with a stub application (incomplete) or the "new"
// implementation using the system's DDE connection; if defined
// we use DDE.
{$DEFINE DdeSingleInstance}


{ Note:

    We reset DdeMgr.AppName periodically if it is not the same
    as we expect and need ("DELPHI32" or BCB). Is that OK? Delphi 3 / BCB3
    apparently sets this to the current project, for what purpose?

  **********************************************************

  +++ The following notes apply to the Single IDE expert
      that made use of a STUB application

  In order to install ourselves correctly, we need to do
  the following
  - Install with *full* path name into registry
    --> this will pass the long file name to the stub,
        instead of the 8.3 representation (at least NT 4.0)

  - Make sure somehow that HKEY_Classes_Root\<item>\Shell\Open\ddeexec
    is **NOT** available, and, in particular, does not point to
    another version of Delphi (Delphi 4.0) or C++Builder (C++Builder 4.0)

    If this this is not done, then we will run into a rather strange
    situation where the stub is called properly, but the system (!)
    complains that

      <your source file> (or one of its components) could not
      be found. Make sure that path and file name information
      are correct and that all required libraries are available.

      [Translated from German NT 4.0 message]

    This one is a pain to fix, since Delphi 4.0 (and C++Builder 4.0)
    install themselves as DDE servers...
}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GX_Experts, ToolIntf, ExptIntf, DdeMan, ExtCtrls;

type
  TRegisterAction = (raRegister, raUnregister);

  TfmSingleIDE = class(TForm)
    btnOK: TButton;
    btnRegister: TButton;
    btnUnregister: TButton;
    System: TDdeServerConv;
    Open: TDdeServerItem;
    timDdeUpdate: TTimer;
    mRegistrationResults: TMemo;
    lRegistrationResults: TLabel;
    mPurpose: TMemo;
    procedure btnRegisterClick(Sender: TObject);
    procedure btnUnregisterClick(Sender: TObject);
    procedure SystemExecuteMacro(Sender: TObject; Msg: TStrings);
    procedure OpenPokeData(Sender: TObject);
    procedure timDdeUpdateTimer(Sender: TObject);
    procedure OpenChange(Sender: TObject);
    procedure SystemClose(Sender: TObject);
    procedure SystemOpen(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    procedure ModifyRegistry(RegisterAction: TRegisterAction);

  public
    { Public declarations }
  end;

  TSingleIDEExpert = class(TGX_Expert)
  private
    FSingleIdeForm: TfmSingleIDE;

  {$IFNDEF DdeSingleInstance}
    function WindowHook(var Message: TMessage): Boolean;
    function HandleWMCopyDataMessage(var Message: TWMCopyData): Boolean;
  {$ENDIF DdeSingleInstance}
  protected
    procedure SetActive(Value: Boolean); override;
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
  end;

implementation

{$R *.DFM}

uses
  ShellApi,
  Registry,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_GenFunc, GX_GExperts;

function LoadFileIntoIde(const FileName: string): Boolean;
var
  AppBuilderForm: TCustomForm;
begin
  Result := False;

  if FileExists(FileName) then
  begin
    Assert(Application <> nil);

    AppBuilderForm := GetIdeMainForm;
    if AppBuilderForm <> nil then
    begin
      if (AppBuilderForm.WindowState = wsMinimized) or IsIconic(Application.Handle) then
      begin
        Application.Restore;
        AppBuilderForm.WindowState := wsNormal;
      end;
    end;

    Result := ToolServices.OpenFile(FileName);
  end;
end;

{$IFNDEF DdeSingleInstance}
function TSingleIDEExpert.WindowHook(var Message: TMessage): Boolean;
begin
  // Return True if handled;
  if Message.Msg <> WM_COPYDATA then
    Result := False
  else
    Result := HandleWMCopyDataMessage(TWMCopyData(Message));
end;

function TSingleIDEExpert.HandleWMCopyDataMessage(var Message: TWMCopyData): Boolean;

  function GetMessage: string;
  var
    PassedData: PChar;
  begin
    with Message.CopyDataStruct^ do
      PassedData := lpData;
    Result := StrPas(PassedData);
  end;

begin
  if LoadFileIntoIde(GetMessage) then
  begin
    Message.Result := 1;
    Result := True;
  end
  else
  begin
    Message.Result := 0;
    Result := False;
  end;
end;
{$ENDIF DdeSingleInstance}

procedure TSingleIDEExpert.Click(Sender: TObject);
begin
  // Nothing
end;

procedure TSingleIDEExpert.Configure;
begin
  Assert(FSingleIdeForm <> nil);

  FSingleIdeForm.ShowModal;
end;

constructor TSingleIDEExpert.Create;
begin
  inherited Create;

  ShortCut := 0;
  HasConfigOptions := True;
  HasMenuItem := False;
  DefaultActive := False;

  FSingleIdeForm := TfmSingleIDE.Create(nil);
end;

destructor TSingleIDEExpert.Destroy;
begin
{$IFNDEF DdeSingleInstance}
  if Active then
    Application.UnhookMainWindow(WindowHook);
{$ENDIF DdeSingleInstance}

  FSingleIdeForm.Free;
  FSingleIdeForm := nil;

  inherited Destroy;
end;

procedure TSingleIDEExpert.SetActive(Value: Boolean);
begin
{$IFNDEF DdeSingleInstance}
  if Value <> Active then
  begin
    if Value = False then
      Application.UnhookMainWindow(WindowHook)
    else
      Application.HookMainWindow(WindowHook);
  end;
{$ENDIF DdeSingleInstance}

  inherited SetActive(Value);
end;


function TSingleIDEExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Single IDE Instance';
begin
  Result := SDisplayName;
end;

function TSingleIDEExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&IDE Single Instance';
begin
  Result := SMenuCaption;
end;

function TSingleIDEExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TSingleIDEExpert.GetMenuName: string;
begin
  Result := 'GX_SingleIDE';
end;

function TSingleIDEExpert.GetName: string;
begin
  Result := 'SingleIDE_Expert';
end;

procedure TSingleIDEExpert.LoadSettings;
begin
  inherited LoadSettings;
end;

procedure TSingleIDEExpert.SaveSettings;
begin
  inherited SaveSettings;
end;


{ TfmSingleIDE }

// NOTE: Do not localize any of the below lines


const
{$IFDEF GX_BCB}
  SoftwareInstallationKey = 'Software\Borland\C++Builder\3.0'; // HKEY_LOCAL_MACHINE (!)
  ApplicationValue = 'App';

  FormFile = 'BCBForm';
  PackageFile = 'BCBPackage';
  ProjectFile = 'BCBProject';
  ProjectFileFile = 'BCBProjectFile';
  ProjectFileGroup = 'BCBProjectGroup';
  UnitFile = 'BCBUnit';

type
  TSourceFile = (sfForm, sfPackage, sfProject,
                 sfProjectFile, sfProjectGroup, sfUnit);

const
  SourceFileClass: array[TSourceFile] of string =
    (FormFile, PackageFile, ProjectFile, ProjectFileFile, ProjectFileGroup, UnitFile);

{$ELSE}
  SoftwareInstallationKey = 'Software\Borland\Delphi\3.0'; // HKEY_LOCAL_MACHINE (!)
  ApplicationValue = 'Delphi 3';

  FormFile = 'DelphiForm';
  PackageFile = 'DelphiPackage';
  ProjectFile = 'DelphiProject';
  UnitFile = 'DelphiUnit';

type
  TSourceFile = (sfForm, sfPackage, sfProject, sfUnit);

const
  SourceFileClass: array[TSourceFile] of string =
    (FormFile, PackageFile, ProjectFile, UnitFile);
{$ENDIF GX_BCB}

{$IFDEF DdeSingleInstance}
const
  ddeexec = '\ddeexec';
  ddeexecApp = '\ddeexec\Application';
  ShellOpen = '\Shell\Open';
  ShellOpenCommand = ShellOpen + '\Command';
  {$IFDEF GX_BCB}
  const
    ServiceApplication = 'bcb';
  {$ELSE}
  const
    ServiceApplication = 'delphi32';
  {$ENDIF GX_BCB}


{$ELSE}
  // WM_COPYDATA technique
  const
    StubApplication = 'OneD4All.exe';
    StubParameter = ' "%1"';
{$ENDIF DdeSingleInstance}


var
  FullPathToApplication: string;

function IsRegisteredForApplication(const RegValue: string): Boolean;
var
  SubStringPos: Integer;
begin
  Result := False;

  if FullPathToApplication = '' then
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      {$IFDEF GX_VER125_up}
      if RegIni.OpenKeyReadOnly(SoftwareInstallationKey) then
      {$ELSE not GX_VER125_up}
      if OpenKey(SoftwareInstallationKey, False) then
      {$ENDIF not GX_VER125_up}
      begin
        FullPathToApplication := AnsiUpperCase(ReadString(ApplicationValue));
        CloseKey;
      end;
    finally
      Free;
    end;
  end;

  if FullPathToApplication = '' then
    Exit;

  SubStringPos := Pos(FullPathToApplication, AnsiUpperCase(RegValue));
  if (SubStringPos = 1) or         // No leading double quote
     (SubStringPos = 2) then       // Leading double quote
  begin
    Result := True;
  end;

{.$DEFINE DontCareToWhomRegistered}
{$IFDEF DontCareToWhomRegistered}
  Result := True;
{$ENDIF DontCareToWhomRegistered}
end;

{$IFNDEF DdeSingleInstance}
function IsRegisteredForSelf(const RegValue: string): Boolean;
begin
  // Result := (AnsiCompareText(RegValue, SelfClassesRootRegistryValue) = 0);
  Result := (Pos(StubApplication + StubParameter, RegValue) > 0);
end;
{$ENDIF DdeSingleInstance}

procedure TfmSingleIDE.ModifyRegistry(RegisterAction: TRegisterAction);
var
  ClassesRoot: TRegistry;
{$IFNDEF DdeSingleInstance}
  GExpertsStorage: TRegistry;
{$ENDIF DdeSingleInstance}
  Value: string;
  i: TSourceFile;

{$IFNDEF DdeSingleInstance}
      // simply delete the complete key specific
      // to OneD4All
      procedure ClearSavedRegistryEntries;
      begin
        GExpertsStorage.DeleteKey(ConfigInfo.RegKey + '\GExperts\OneD4All');
      end;
{$ENDIF DdeSingleInstance}


{.$DEFINE TestMode}
      procedure DoRegister;
      resourcestring
        SNotRegisteredForIDE = 'ERROR: File type "%s" is not registered to current IDE'#13#10+
                                   '   -> This expert only modifies entries of the current IDE.';
        SSuccessfullyRegistered = 'File type "%s" successfully registered';
      const
        ModuleBufferSize = 1024;
{$IFNDEF DdeSingleInstance}
      var
        ModuleLocation: AnsiString;
{$ENDIF DdeSingleInstance}
      begin
        // Only if the file is registered for this IDE do we
        // squeeze in ourselves here
        if IsRegisteredForApplication(Value) then
        begin
{$IFDEF DdeSingleInstance}
          with TRegistry.Create do
          try
            RootKey := HKEY_CLASSES_ROOT;

            {$IFOPT D+} SendDebug(SourceFileClass[i] + ShellOpen + ddeexec); {$ENDIF D+}
            if OpenKey(SourceFileClass[i] + ShellOpen + ddeexec, True) then
            begin
              WriteString('', '[open("%1")]'); // do not localize
              CloseKey;
            end
            else
              RaiseLastWin32Error;

            {$IFOPT D+} SendDebug(SourceFileClass[i] + ShellOpen + ddeexecApp); {$ENDIF D+}
            if OpenKey(SourceFileClass[i] + ShellOpen + ddeexecApp, True) then
            begin
              WriteString('', ServiceApplication);
              CloseKey;
            end
            else
              RaiseLastWin32Error;

            Assert(KeyExists(SourceFileClass[i] + ShellOpen + ddeexec));
            Assert(KeyExists(SourceFileClass[i] + ShellOpen + ddeexecApp));
          finally
            Free;
          end;

          mRegistrationResults.Lines.Add(
            Format(SSuccessfullyRegistered, [SourceFileClass[i]]));
{$ELSE}
          // Get our (GExperts DLL) own placement
          SetLength(ModuleLocation, ModuleBufferSize);
          GetModuleFileName(HINSTANCE, PChar(ModuleLocation), ModuleBufferSize);
          SetLength(ModuleLocation, StrLen(PChar(ModuleLocation)));

          // Build a path to OneD4All.Exe
          ModuleLocation := ExtractFilePath(ModuleLocation) + StubApplication;
          if not FileExists(ModuleLocation) then
            raise Exception.Create('Stub module application not found in the same directory as GExperts DLL');

          // save original data
          GExpertsStorage.WriteString(SourceFileClass[i], Value);

          // we do not write path information anywhere, since
          // the intention is to place OneD4All into a the system[32]
          // folder of the Windows installation
        {$IFNDEF TestMode}
          ClassesRoot.WriteString('', ModuleLocation + StubParameter);
        {$ENDIF TestMode}
          mRegistrationResults.Lines.Add(
            Format(SSuccessfullyRegistered, [SourceFileClass[i]])
           );
{$ENDIF DdeSingleInstance}
        end
        else
          mRegistrationResults.Lines.Add(Format(SNotRegisteredForIDE, [SourceFileClass[i]]));
      end;

      procedure DoUnregister;
      resourcestring
        SSuccessfullyUnregistered = 'File type "%s" successfully unregistered';
        SNotRegisteredToSelf = 'ERROR: File type "%s" not unregistered since'#13#10+
                               '  it was not registered to this expert';
        SGeneralError = 'General ERROR: %s';
{$IFNDEF DdeSingleInstance}
      var
        SavedData: string;
{$ENDIF DdeSingleInstance}
      begin
{$IFNDEF DdeSingleInstance}
        // Only if the file is registered for this IDE do we
        // squeeze in ourselves here
        if IsRegisteredForSelf(Value) then
        begin
          // read old data
          SavedData := GExpertsStorage.ReadString(SourceFileClass[i]);
          if SavedData <> '' then
          begin
            // we do not write path information anywhere, since
            // the intention is to place OneD4All into a the system[32]
            // folder of the Windows installation
          {$IFNDEF TestMode}
            ClassesRoot.WriteString('', SavedData); // Write to (Default) value
          {$ENDIF TestMode}

            mRegistrationResults.Lines.Add(Format(SSuccessfullyUnregistered, [SourceFileClass[i]]));
          end;
        end
        else
        begin
          mRegistrationResults.Lines.Add(Format(SNotRegisteredToSelf, [SourceFileClass[i]]));
        end;
{$ELSE}
        if not IsRegisteredForApplication(Value) then
        begin
          mRegistrationResults.Lines.Add(Format(SNotRegisteredToSelf, [SourceFileClass[i]]));
          Exit;
        end;

        try
          with TRegistry.Create do
          try
            if not DeleteKey(Value + ddeexecApp) then ;
            if not DeleteKey(Value + ddeexec) then ;
          finally
            Free;
          end;
          mRegistrationResults.Lines.Add(Format(SSuccessfullyUnregistered, [SourceFileClass[i]]));
        except
          on E: Exception do
          begin
            mRegistrationResults.Lines.Add(Format(SGeneralError, [E.Message]));
            { swallow exception }
          end;
        end;
{$ENDIF DdeSingleInstance}
      end;


resourcestring
  SUnspecificError = 'Unspecific ERROR: %s';
begin
  // It is slightly unsafe not to protect each allocation individually
  // in a try..finally block, but what the heck - we are going to terminate
  // that process in a few milliseconds anyway.
  ClassesRoot := TRegistry.Create;
{$IFNDEF DdeSingleInstance}
  GExpertsStorage := TRegistry.Create;
{$ENDIF DdeSingleInstance}
  try
    ClassesRoot.RootKey := HKEY_CLASSES_ROOT;
    // GExpertsStorage.RootKey := HKEY_CURRENT_USER; // Redundant

{$IFNDEF DdeSingleInstance}
    // Clear all existing saved entries before saving any new ones
    if RegisterAction = raRegister then
      ClearSavedRegistryEntries;

    if not GExpertsStorage.OpenKey(ConfigInfo.RegKey + '\GExperts\OneD4All', True) then
      raise Exception.Create('Could not open or create GExperts backup registry key');
{$ENDIF DdeSingleInstance}

    try

      for i := Low(TSourceFile) to High(TSourceFile) do
      begin

        try
          if ClassesRoot.OpenKey(SourceFileClass[i] + ShellOpenCommand, False) then
          try
            Value := ClassesRoot.ReadString(''); // Read (Default) value

            case RegisterAction of
              raRegister:   DoRegister;
              raUnregister: DoUnregister;
            else
              Assert(False, 'Whoops, internal error identifying RegisterAction');
            end;
          finally
            ClassesRoot.CloseKey;
          end;

        except
          on E: Exception do
          begin
            mRegistrationResults.Lines.Add(Format(SUnspecificError, [E.Message]));
            { swallow exception }
          end;
        end;
      end;

    finally
{$IFNDEF DdeSingleInstance}
      GExpertsStorage.CloseKey;
{$ENDIF DdeSingleInstance}
    end;

{$IFNDEF DdeSingleInstance}
    // Just clean up - we do not want to leave any traces
    if RegisterAction = raUnregister then
      ClearSavedRegistryEntries;
{$ENDIF DdeSingleInstance}

  finally
    ClassesRoot.Free;
{$IFNDEF DdeSingleInstance}
    GExpertsStorage.Free;
{$ENDIF DdeSingleInstance}
  end;
end;

procedure TfmSingleIDE.btnRegisterClick(Sender: TObject);
resourcestring
  SRegistrationResults = 'Registration Results:';
begin
  mRegistrationResults.Clear;
  lRegistrationResults.Caption := SRegistrationResults;
  try
    ModifyRegistry(raRegister);
  except
    on E: Exception do
    begin
      ProcessException(E);
    end;
  end;
end;

procedure TfmSingleIDE.btnUnregisterClick(Sender: TObject);
resourcestring
  SUnRegistrationResults = 'Unregistration Results:';
begin
  mRegistrationResults.Clear;
  lRegistrationResults.Caption := SUnRegistrationResults;
  try
    ModifyRegistry(raUnregister);
  except
    on E: Exception do
      ProcessException(E);
  end;
end;

function TSingleIDEExpert.IconFileName: string;
begin
  Result := 'SingleIDE';
end;

procedure TfmSingleIDE.SystemExecuteMacro(Sender: TObject; Msg: TStrings);
const
  DdeOpenLeft = '[OPEN("'; // do not localize
  DdeOpenRight = '")]';  // do not localize
var
  ExecLeft: string;
  ExecRight: string;
  CompleteExecCommand: string;
  i: Integer;
begin
  for i := 0 to Msg.Count-1 do
  begin
    try
      CompleteExecCommand := Msg[i];

      ExecLeft := Copy(CompleteExecCommand, 1, Length(DdeOpenLeft));
      ExecRight := Copy(CompleteExecCommand, Length(CompleteExecCommand) - Length(DdeOpenRight) + 1, Length(DdeOpenRight));

      {$IFOPT D+}SendDebug('Execute: ' + CompleteExecCommand + '/' + ExecLeft + '/' + ExecRight); {$ENDIF}

      // Only if the execute command matches our "Open" template do continue with
      // loading, otherwise ignore this exec command
      if (AnsiCompareText(ExecLeft, DdeOpenLeft) = 0) and
         (AnsiCompareText(ExecRight, DdeOpenRight) = 0) then
      begin
        // Strip off command strings
        Delete(CompleteExecCommand, Length(CompleteExecCommand) - Length(DdeOpenRight) + 1, Length(DdeOpenRight));
        Delete(CompleteExecCommand, 1, Length(DdeOpenLeft));

        {$IFOPT D+} SendDebug('Loading file ' + CompleteExecCommand); {$ENDIF}

        LoadFileIntoIde(CompleteExecCommand);
      end;
    except
      on E: Exception do
      begin
        // ignore
      end;
    end;
  end;
end;

procedure TfmSingleIDE.OpenPokeData(Sender: TObject);
begin
  // {$IFOPT D+} SendDebug('Open ' + Open.Text); {$ENDIF}
end;

procedure TfmSingleIDE.timDdeUpdateTimer(Sender: TObject);
begin
{$IFDEF DdeSingleInstance}
  if DdeMgr.AppName <> ServiceApplication then
    DdeMgr.AppName := ServiceApplication;
{$ENDIF DdeSingleInstance}
end;

procedure TfmSingleIDE.OpenChange(Sender: TObject);
begin
  // {$IFOPT D+} SendDebug('openchange') {$ENDIF}
end;

procedure TfmSingleIDE.SystemClose(Sender: TObject);
begin
  // {$IFOPT D+} SendDebug('systemclose') {$ENDIF}
end;

procedure TfmSingleIDE.SystemOpen(Sender: TObject);
begin
  // {$IFOPT D+} SendDebug('systemopen') {$ENDIF}
end;

procedure TfmSingleIDE.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfmSingleIDE.FormShow(Sender: TObject);
begin
  mRegistrationResults.Clear;
end;

initialization
  RegisterGX_Expert(TSingleIDEExpert);
{$ENDIF GX_UseIdeSingleInstanceExpert}
end.


