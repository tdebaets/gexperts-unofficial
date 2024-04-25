unit GX_GExperts;

{$I GX_CondDefine.Inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{$IFDEF VER110}
{$D-} //! StH: Work around a compiler bug in C++Builder 3.0
{$ENDIF VER110}

{$IFDEF VER125}
{$D-} //! StH: Work around a compiler bug in C++Builder 4.0
{$ENDIF VER110}

interface

uses
  Windows, Messages, SysUtils, Consts, Classes, Forms, ComCtrls,
  Dialogs, ClipBrd, Controls, ExtCtrls, Menus, StdCtrls, Graphics,
  EditIntf, ExptIntf, ToolIntf,
  // If you get errors here while compiling, see SourceCode.txt
  {$IFDEF GX_EII}EIManager, EINotifiers, GX_EditorExpert, {$ENDIF GX_EII}
  GX_Experts, GX_MenuNotifier,
  GX_ConfigurationInfo, GX_Configure,
  GX_IDEEnhance;

type
  TGExperts = class(TIExpert)
  private
    FExpertList: TList;
    {$IFDEF GX_EII}
    FEditorExpertList: TList;
    {$ENDIF GX_EII}
    FProjectNotifier: TProjectNotifier;
    procedure InstallAddIn;
    function GetExpert(const Index: Integer): TGX_Expert;
    function GetExpertCount: Integer;
    {$IFDEF GX_EII}
    function GetEditorExpert(const Index: Integer): TEditorExpert;
    function GetEditorExpertCount: Integer;
    {$ENDIF GX_EII}
  public
    {$IFDEF GX_EII}
    IDEManager: TIDEManager;
    KeyboardNotifier: TEIKeyboardNotifier;
    {$ENDIF GX_EII}
    constructor Create;
    destructor Destroy; override;
    function GetName: string; override;
    function GetStyle: TExpertStyle; override;
    function GetIDString: string; override;
    { dummy overrides }
    function GetAuthor: string; override;
    function GetComment: string; override;
    function GetPage: string; override;
    function GetGlyph: HICON; override;
    function GetState: TExpertState; override;
    function GetMenuText: string; override;
    procedure Execute; override;
    property ExpertList[const Index: Integer]: TGX_Expert read GetExpert;
    property ExpertCount: Integer read GetExpertCount;
    {$IFDEF GX_EII}
    property EditorExpertList[const Index: Integer]: TEditorExpert read GetEditorExpert;
    property EditorExpertCount: Integer read GetEditorExpertCount;
    procedure LoadEditorExperts;
    procedure FreeEditorExperts;
    {$ENDIF GX_EII}
    procedure ShowConfigurationForm;
    property ProjectNotifier: TProjectNotifier read FProjectNotifier;
  end;

  {$IFDEF GX_EII}
  TGXKeyboardNotifier = class(TEIKeyboardNotifier)
  private
    FWheelScrollChars: Cardinal;
    FWheelScrollLines: Cardinal;
  public
    constructor Create;
    procedure BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage;
      var Msg: TMessage); override;
  end;
  {$ENDIF GX_EII}


var
  GExpertsInst: TGExperts = nil;

//procedure Cleanup;
procedure ShowGXAboutForm;
procedure ShowGXConfigurationForm;

implementation

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  {$IFDEF GX_EII}
  EIMisc,
  {$ENDIF GX_EII}
  GX_GenFunc,
  Registry, GX_About, CommCtrl, GX_Actions;

procedure ShowGXAboutForm;
begin
  with TfmAbout.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure ShowGXConfigurationForm;
begin
  with TfmConfiguration.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

{$IFDEF GX_EII}
function TGExperts.GetEditorExpert(const Index: Integer): TEditorExpert;
begin
  if Index < GetEditorExpertCount then
    Result := TEditorExpert(FEditorExpertList[Index])
  else
    Result := nil;
end;

function TGExperts.GetEditorExpertCount: Integer;
begin
  Result := FEditorExpertList.Count;
end;
{$ENDIF GX_EII}

constructor TGExperts.Create;
resourcestring
  SInitError = 'Initialization Error:'#13#10;
begin
  // This mutex signals that at least one copy of GExperts is running
  // The installer uses this to determine if it should allow installation
  CreateMutex(nil, False, 'GExperts.Addin.For.Borland.IDEs');

  inherited Create;
  FExpertList := TList.Create;
  {$IFDEF GX_EII}
  FEditorExpertList := TList.Create;
  IDEManager := nil;
  {$ENDIF GX_EII}
  GExpertsInst := Self;
  // Create the action manager.
  {$IFOPT D+}SendDebug('Creating GXActionManager'); {$ENDIF}
  CreateGXActionManager;
  try
    if ToolServices <> nil then
    begin
      FProjectNotifier := TProjectNotifier.Create(TModuleNotifier);
      ToolServices.AddNotifier(FProjectNotifier);
    end;
    {$IFOPT D+}SendDebug('Installing AddIn'); {$ENDIF}
    InstallAddIn;
    {$IFOPT D+}SendDebug('Successfully installed AddIn'); {$ENDIF}
  except
    on E: Exception do
    begin
      ProcessException(E);
      MessageDlg(SInitError + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

destructor TGExperts.Destroy;

  (*
  procedure DumpExpertList;
  var
    j: Integer;
  begin
    {$IFOPT D+}SendDebug('Dumping Expert List'); {$ENDIF}
    {$IFOPT D+}SendDebug('There are X Experts: '+IntToStr(FExpertList.Count)); {$ENDIF}
    for j := 0 to FExpertList.Count - 1 do
      SendDebug(Format('Expert %d is %s', [j, TGX_Expert(FExpertList[j]).GetName]));
    {$IFOPT D+}SendDebug('Done dumping Expert List'); {$ENDIF}
  end;
  *)

resourcestring
  SFinitError = 'GExperts destruction error:'#13#10;
var
  i: Integer;
begin
  try
    {$IFOPT D+}SendDebug('Destroying GExperts'); {$ENDIF}

    {$IFOPT D+}SendDebug('Destroying Experts'); {$ENDIF}
    if FExpertList <> nil then
    begin
      //DumpExpertList;
      for i := 0 to FExpertList.Count - 1 do
      begin
        {$IFOPT D+}if ExpertList[i] <> nil then SendDebug('Destroying Expert: ' + ExpertList[i].GetName); {$ENDIF}
        ExpertList[i].Free;
      end;
      {$IFOPT D+}SendDebug('Done freeing experts'); {$ENDIF}
      FExpertList.Free;
      FExpertList := nil;
    end;

    {$IFOPT D+}SendDebug('Removing Notifiers'); {$ENDIF}
    if FProjectNotifier <> nil then
    begin
      ToolServices.RemoveNotifier(FProjectNotifier);
      FProjectNotifier.Free;
      FProjectNotifier := nil;
    end;
    {$IFOPT D+}SendDebug('Project Notifier Freed'); {$ENDIF}

    {$IFDEF GX_EII}
    FreeEditorExperts;
    FEditorExpertList.Free;
    FEditorExpertList := nil;
    {$ENDIF GX_EII}

    {$IFOPT D+}SendDebug('Freeing Action manager'); {$ENDIF}
    // Free the Action manager
    FreeGXActionManager;

    {$IFOPT D+}SendDebug('Freeing ConfigInfo'); {$ENDIF}
    // Free configuration info and all associated IDE and editor enhancements
    FreeConfigInfo;

    inherited Destroy;
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebug('GExpert Destroy Error ' + E.Message); {$ENDIF}
      ProcessExceptionMsg(E, SFinitError);
      raise;
    end;
  end;
end;

resourcestring
  SExpertCreationFailed = 'Expert "%s" could not be created.'#13'Reason: %s';

procedure TGExperts.InstallAddIn;
var
  Expert: TGX_Expert;
  ExpertClass: TGX_ExpertClass;
  i: Integer;
begin
  for i := 0 to GX_ExpertList.Count - 1 do
  begin
    ExpertClass := GetGX_ExpertClassByIndex(i);
    try
      Expert := ExpertClass.Create;
      Expert.ExpertIndex := i;
      Expert.LoadSettings;
      FExpertList.Add(Expert);
      if Expert.Active and Expert.HasMenuItem and
        not (Expert is TGX_EnhExpert) then
      begin
        GXActionManager.AddExpertAction(Expert);
      end;
    except
      on E: Exception do
      begin
        MessageDlg(Format(SExpertCreationFailed, [ExpertClass.ClassName, E.Message]), mtError, [mbOK], 0);
        // Eat the exception and load other experts (is this safe?)
        //raise;
      end;
    end;
  end;

  {$IFDEF GX_EII}
  if ConfigInfo.EditorExpertsEnabled then
    LoadEditorExperts;
  {$ENDIF GX_EII}
end;

{$IFDEF GX_EII}
constructor TGXKeyboardNotifier.Create;
begin
  inherited;
  FWheelScrollChars := DEFAULT_WHEELSCROLLCHARS;
  FWheelScrollLines := DEFAULT_WHEELSCROLLLINES;
  SystemParametersInfo(SPI_GETWHEELSCROLLCHARS, 0, @FWheelScrollChars, 0);
  SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0);
end;

procedure TGXKeyboardNotifier.BeforeEditorMessageProcess(Manager: TEIManager; OriginalMessage: TMessage; var Msg: TMessage);

procedure CheckKey(Sender: TObject; var Key: Word; Shift: TShiftState;
    KeyDown: Boolean);
  var
    SCut: TShortCut;
    i: Integer;
    Expert: TEditorExpert;
    Handled: Boolean;
  begin
    SCut := ShortCut(Key, Shift);
    for i := 0 to GExpertsInst.EditorExpertCount - 1 do
    begin
      Expert := GExpertsInst.EditorExpertList[i];
      if Expert.ShortCut = SCut then
      begin
        Handled := False;
        if KeyDown and (Expert is TEditorExpert2) then
          Handled := (Expert as TEditorExpert2).ExecuteKeyDown(Manager)
        else
          Expert.Execute;
        if not KeyDown or Handled then
          Key := 0;
        Break;
      end;
    end;
  end;

procedure HandleMouseWheel;
  var
    ScrollType: Cardinal;
    i: Integer;
  begin
    if not Assigned(Manager.EditControl) then
      Exit;
    // Note: GetScrollPos/SetScrollPos only changes the position of the scrollbar
    // and not of the edit control itself.
    if OriginalMessage.Msg = WM_MOUSEWHEEL then
    begin
      if TWMMouseWheel(OriginalMessage).WheelDelta > 0 then
        ScrollType := SB_LINEUP
      else
        ScrollType := SB_LINEDOWN;
      // Can't use Mouse.WheelScrollLines here because that's always 0 on newer
      // models of the Lenovo Thinkpad, because
      // GetSystemMetrics(SM_MOUSEWHEELPRESENT) returns FALSE there (see
      // TMouse.GetNativeData). We need to do the same for FWheelScrollChars
      // anyway (since there's no native Delphi support for that metric).
      for i := 1 to FWheelScrollLines do
        SendMessage(Manager.EditControl.Handle, WM_VSCROLL, ScrollType, 0);
      Msg.Msg := WM_NULL; // prevent the message from being handled again
    end
    else if OriginalMessage.Msg = WM_MOUSEHWHEEL then
    begin
      // TWMMouseWheel here isn't a typo, there's no such thing as a
      // TWMMouseHWheel and we can just use the 'vertical' struct here too.
      if TWMMouseWheel(OriginalMessage).WheelDelta > 0 then
        ScrollType := SB_LINERIGHT
      else
        ScrollType := SB_LINELEFT;
      for i := 1 to FWheelScrollChars do
        SendMessage(Manager.EditControl.Handle, WM_HSCROLL, ScrollType, 0);
      Msg.Msg := WM_NULL; // prevent the message from being handled again
    end;
  end;

begin
  case OriginalMessage.Msg of
    CN_KEYDOWN, CN_KEYUP:
      begin
        CheckKey(Manager, TWMKey(Msg).CharCode,
            KeyDataToShiftState(TWMkey(Msg).KeyData),
            (OriginalMessage.Msg = CN_KEYDOWN));
        if TWMKey(Msg).CharCode = 0 then
          Msg.Result := 1;
      end;
    WM_MOUSEWHEEL, WM_MOUSEHWHEEL:
      HandleMouseWheel;
  end;
end;
{$ENDIF GX_EII}

{$IFDEF GX_EII}
resourcestring
  SCodeRushWarning = 'The GExperts editor experts will not function with CodeRush installed.  '
    + #13 + #10 + 'To remove this message, disable the editor experts in the GExperts '
    + #13 + #10 + 'configuration window and restart Delphi or remove CodeRush from the'
    + #13 + #10 + '"Known Packages" list in the Delphi registry.';

procedure TGExperts.LoadEditorExperts;
var
  i: Integer;
  EditorExpert: TEditorExpert;
begin
  FreeEditorExperts;
  {$IFDEF VER100}
  // Make this a GXMessageBox warning
  if IsCodeRushInstalled then
  begin
    MessageDlg(SCodeRushWarning, mtWarning, [mbOK], 0);
    ConfigInfo.EditorExpertsEnabled := False;
    Exit;
  end;
  {$ENDIF VER100}
  ConfigInfo.EditorExpertsEnabled := True;
  for i := 0 to EditorExpertClassList.Count - 1 do
  begin
    EditorExpert := GetExpertClassByIndex(i).Create;
    EditorExpert.LoadSettings;
    FEditorExpertList.Add(EditorExpert);
  end;
  if IDEManager = nil then
  begin
    IDEManager := InstallIDEIntegrationManager('GExperts');
    KeyboardNotifier := TGXKeyboardNotifier.Create;
    IDEManager.AddNotifier(KeyboardNotifier);
  end;
end;

procedure TGExperts.FreeEditorExperts;

  function IDEManagerExists: Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := 0 to (Application.ComponentCount - 1) do
      if (CompareText(Application.Components[i].ClassName, TIDEManager.ClassName) = 0) then
      begin
        Result := True;
        Exit;
      end;
  end;

var
  I: Integer;
begin
  {$IFOPT D+}SendDebug('Removing the editor experts keyboard notifier');{$ENDIF D+}
  if (IDEManager <> nil) and (KeyboardNotifier <> nil) and IDEManagerExists then
    IDEManager.RemoveNotifier(KeyboardNotifier);
  KeyboardNotifier.Free;
  KeyboardNotifier := nil;

  {$IFOPT D+}SendDebug('Freeing the editor experts');{$ENDIF D+}
  if FEditorExpertList <> nil then
    for I := 0 to FEditorExpertList.Count - 1 do
      TEditorExpert(FEditorExpertList[I]).Free;

  { TODO -oStefan  -cIssue: investigate}
  FEditorExpertList.Clear;
end;
{$ENDIF GX_EII}

procedure TGExperts.ShowConfigurationForm;
var
  Config: TfmConfiguration;
  i: Integer;
begin
  Config := TfmConfiguration.Create(nil);
  try
    if Config.ShowModal = mrOK then
      for i := 0 to ExpertCount - 1 do
        ExpertList[i].AfterConfig;
  finally
    Config.Free;
  end;
end;

function TGExperts.GetExpert(const Index: Integer): TGX_Expert;
begin
  Result := TGX_Expert(FExpertList.Items[index]);
end;

function TGExperts.GetExpertCount: Integer;
begin
  Result := FExpertList.Count;
end;

{ Implement the expert interface. }

function TGExperts.GetName: string;
begin
  try
    Result := 'GExperts'; // do not localize
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TGExperts.GetStyle: TExpertStyle;
begin
  Result := esAddIn;
end;

function TGExperts.GetIDString: string;
begin
  try
    Result := 'GExperts.GExperts'; // do not localize
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

function TGExperts.GetAuthor: string;
begin
  Result := '';
end;

function TGExperts.GetComment: string;
begin
  Result := '';
end;

function TGExperts.GetPage: string;
begin
  Result := '';
end;

function TGExperts.GetGlyph: HICON;
begin
  Result := 0;
end;

function TGExperts.GetState: TExpertState;
begin
  Result := [];
end;

function TGExperts.GetMenuText: string;
begin
  Result := '';
end;

procedure TGExperts.Execute;
begin
  // nothing
end;

(*
procedure Cleanup; export;
resourcestring
  STerminationError = 'Termination Error: ';
begin
  try
    //Restore old WND_PROC handler on exit (Huh???)
    {$IFOPT D+}SendDebug('Successfully terminating'); {$ENDIF}
  except
    on E: Exception do
    begin
      {$IFOPT D+}SendDebug('GExperts Termination: ' + E.Message); {$ENDIF}
      ProcessExceptionMsg(E, STerminationError);
    end;
  end;
end;
*)

end.


