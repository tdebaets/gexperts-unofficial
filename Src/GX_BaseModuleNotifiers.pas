unit GX_BaseModuleNotifiers;

{$I GX_CondDefine.inc}

// Base class for project-wide module notifiers
// Currently used for "global" GExperts notifiers
// as well as for the Code Proofreader

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Classes, Dialogs, ToolIntf, EditIntf, ExptIntf;

type
  TBaseProjectNotifier = class;

  TBaseModuleNotifier = class(TIModuleNotifier)
  protected
    FFileName: string;
    FModuleInterface: TIModuleInterface;
    FProjectNotifier: TBaseProjectNotifier;
  protected
    procedure DoEditorModified; virtual;
  public
    constructor Create(ProjectNotifier: TBaseProjectNotifier; const ModuleFileName: string); virtual;
    destructor Destroy; override;
    procedure Notify(NotifyCode: TNotifyCode); override;
    {$IFDEF GX_VER140_up}
    procedure ComponentRenamed(const AComponent: TComponent;
      const OldName, NewName: string); override;
    {$ELSE not GX_VER140_up}
    procedure ComponentRenamed(ComponentHandle: Pointer;
      const OldName, NewName: string); override;
    {$ENDIF}
  public
    property FileName: string read FFileName;
  end;

  TBaseModuleNotifierClass = class of TBaseModuleNotifier;

  TBaseProjectNotifier = class(TIAddInNotifier)
  protected
    // Allow "duplicate" module notifiers (attached to the same file)
    // to be installed
    FAllowDuplicates: Boolean;
    // Type of module notifier created
    FModuleNotifierClass: TBaseModuleNotifierClass;
    // List of all module notifiers managed by this project notifier
    FNotifiers: TList;
    procedure InstallModuleNotifier(const FileName: string);
    procedure SetAllowDuplicates(const Value: Boolean);
  private
    function GetModuleNotifierCount: Integer;
    function GetModuleNotifier(const Index: Integer): TBaseModuleNotifier;
  protected
    procedure DoProjectOpened(const FileName: string; var Cancel: Boolean); virtual;
    procedure DoFileOpened(const FileName: string; var Cancel: Boolean); virtual;
    procedure DoFileClosing(const FileName: string; var Cancel: Boolean); virtual;
  public
    constructor Create(ModuleNotifierClass: TBaseModuleNotifierClass);
    destructor Destroy; override;

    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    procedure EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean); override;

    property AllowDuplicates: Boolean read FAllowDuplicates write SetAllowDuplicates;
    property ModuleNotifierCount: Integer read GetModuleNotifierCount;
    property ModuleNotifiers[const Index: Integer]: TBaseModuleNotifier read GetModuleNotifier;
  end;

implementation

uses
  Windows, Consts,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  SysUtils, TypInfo, GX_GenFunc;

{ TBaseProjectNotifier }

constructor TBaseProjectNotifier.Create(ModuleNotifierClass: TBaseModuleNotifierClass);
begin
  inherited Create;

  FModuleNotifierClass := ModuleNotifierClass;
  FNotifiers := TList.Create;
end;

destructor TBaseProjectNotifier.Destroy;
var
  BaseModuleNotifier: TBaseModuleNotifier;
begin
  if FNotifiers <> nil then
  begin
    // Clean up existing modifiers
    while FNotifiers.Count > 0 do
    begin
      BaseModuleNotifier := TBaseModuleNotifier(FNotifiers[0]);
      {$IFOPT D+}SendDebugEx('ERROR: TBaseProjectNotifier frees ' + BaseModuleNotifier.FileName, mtError);{$ENDIF}
      BaseModuleNotifier.Free;
    end;

    FNotifiers.Free;
    FNotifiers := nil;
  end;

  inherited Destroy;
end;

procedure TBaseProjectNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  {$IFOPT D+} SendDebug('Project: ' + GetEnumName(Typeinfo(TFileNotification), Ord(NotifyCode)) + ': ' + FileName); {$ENDIF}
  case NotifyCode of

    fnProjectOpened:
      begin
        DoProjectOpened(FileName, Cancel);
      end;

    fnFileOpened:
      begin
        DoFileOpened(FileName, Cancel);
      end;

    fnProjectClosing:
      begin
        DoFileClosing(FileName, Cancel);
      end;
  end;
end;

procedure TBaseProjectNotifier.DoFileClosing(const FileName: string; var Cancel: Boolean);
begin
  // Nothing
end;

procedure TBaseProjectNotifier.DoFileOpened(const FileName: string; var Cancel: Boolean);
begin
  {$IFOPT D+}SendDebug('File opened: ' + FileName);{$ENDIF}
  InstallModuleNotifier(FileName);
end;

procedure TBaseProjectNotifier.DoProjectOpened(const FileName: string; var Cancel: Boolean);
begin
  InstallModuleNotifier(FileName);
end;

procedure TBaseProjectNotifier.EventNotification(NotifyCode: TEventNotification; var Cancel: Boolean);
begin
  // Nothing
end;

function TBaseProjectNotifier.GetModuleNotifierCount: Integer;
begin
  Assert(FNotifiers <> nil);
  Result := FNotifiers.Count;
end;

function TBaseProjectNotifier.GetModuleNotifier(const Index: Integer): TBaseModuleNotifier;
begin
  Result := TObject(FNotifiers[Index]) as TBaseModuleNotifier;
end;

procedure TBaseProjectNotifier.InstallModuleNotifier(const FileName: string);
var
  AModuleNotifier: TBaseModuleNotifier;
  i: Integer;
begin
  // Delphi 4 sometimes passes in '' for new TLB files (and some packages?)
  if FileName = '' then
  begin
    {$IFOPT D+}SendDebugEx('!!! Warning: Got empty filename for module notifier installation', mtWarning);{$ENDIF}
    Exit;
  end;

(*
  if UpperCase(ExtractFileExt(FileName)) = '.BAT' then
  begin
    // Calling SetSyntaxHighlighter on bat files causes an AV, so we disable
    // modules notifiers for bat files.  This makes Code Proofreader happy.
    // This bug only applies to Delphi 5.00, and was fixed in 5.01
    {$IFOPT D+}SendDebugEx('!!! Warning: Do not install module notifiers for BAT files', mtWarning);{$ENDIF}
    Exit;
  end;
*)

  if UpperCase(ExtractFileExt(FileName)) = '.TLB' then
  begin
    // Calling GetModuleInterface on tlb files returns nil under D3
    {$IFOPT D+}SendDebugEx('!!! Warning: Do not install module notifiers for TLB files', mtWarning);{$ENDIF}
    Exit;
  end;

  if not FAllowDuplicates then
  begin
    for i := 0 to FNotifiers.Count - 1 do
    begin
      if CompareText(TBaseModuleNotifier(FNotifiers[i]).FileName,
                     FileName) = 0 then
      begin
        // Found a duplicate entry; do not install a module notifier
        Exit;
      end;
    end;
  end;

  {$IFOPT D+}SendDebug('Installing module notifier for '+FileName);{$ENDIF}
  AModuleNotifier := FModuleNotifierClass.Create(Self, FileName);
  FNotifiers.Add(AModuleNotifier);
end;

procedure TBaseProjectNotifier.SetAllowDuplicates(const Value: Boolean);

  procedure AssertNoDuplicatesInList;
  var
    i: Integer;
    j: Integer;
  begin
    for i := 0 to FNotifiers.Count-2 do
    begin
      for j := FNotifiers.Count-1 downto i+1 do
      begin
         if CompareText(TBaseModuleNotifier(FNotifiers[i]).FileName,
                        TBaseModuleNotifier(FNotifiers[j]).FileName) = 0 then
         begin
           raise Exception.Create('List of notifiers contains duplicates');
         end;
      end;
    end;
  end;

begin
  if Value <> FAllowDuplicates then
  begin
    FAllowDuplicates := Value;
    if not FAllowDuplicates then
      AssertNoDuplicatesInList;
  end;
end;

{ TBaseModuleNotifier }

constructor TBaseModuleNotifier.Create(ProjectNotifier: TBaseProjectNotifier; const ModuleFileName: string);
begin
  inherited Create;

  FProjectNotifier := ProjectNotifier;
  FModuleInterface := ToolServices.GetModuleInterface(ModuleFileName);
  if FModuleInterface <> nil then
    FModuleInterface.AddNotifier(Self)
  else // we no longer Assert here, because it is happening too often for odd file types
    MessageDlg('ModuleInterface for' + ModuleFileName + ' is nil', mtError, [mbOK], 0);
  FFileName := ModuleFileName;
end;

destructor TBaseModuleNotifier.Destroy;
begin
  {$IFOPT D+}SendDebug('Destroying Module Notifier: ' + ClassName);{$ENDIF}

  FProjectNotifier.FNotifiers.Remove(Self); // remove from "global" notifier list
  FModuleInterface.RemoveNotifier(Self); // unregister ourselves

  {$IFOPT D+}SendDebug('Removed Module Notifier');{$ENDIF}

  FModuleInterface.Free;
  FModuleInterface := nil;

  {$IFOPT D+}SendDebug('Freed FModuleInterface');{$ENDIF}

  inherited Destroy;

  {$IFOPT D+}SendDebug('Destroyed Module Notifier'){$ENDIF}
end;

procedure TBaseModuleNotifier.DoEditorModified;
begin
  // nothing
end;

procedure TBaseModuleNotifier.Notify(NotifyCode: TNotifyCode);
begin
  case NotifyCode of

    ncModuleRenamed:
      begin
        FFileName := ToolServices.GetCurrentFile;
      end;

    ncModuleDeleted:
      begin
        Self.Free;
      end;

    ncEditorModified:
      begin
        DoEditorModified;
      end;

  end;
end;

{$IFDEF GX_VER140_up}
procedure TBaseModuleNotifier.ComponentRenamed(const AComponent: TComponent;
  const OldName, NewName: string);
{$ELSE not GX_VER140_up}
procedure TBaseModuleNotifier.ComponentRenamed(ComponentHandle: Pointer;
  const OldName, NewName: string);
{$ENDIF}
begin
  // Nothing
end;

end.

