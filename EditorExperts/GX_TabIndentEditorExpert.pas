unit GX_TabIndentEditorExpert;

{$I GX_CondDefine.inc}

interface

uses
  GX_EditorExpert, Classes, Controls, Windows, EIManager;

type
  TGxAbstractTabEditorExpert = class(TEditorExpert2)
  private
    FUnindent: Boolean;
    function IsUnindent: Boolean; virtual; abstract;
  public
    constructor Create; override;
    procedure Execute; override;
    function ExecuteKeyDown(Manager: TEIManager): Boolean; override;
    procedure GetHelpString(List: TStrings); override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
  end;

  TGxTabIndentEditorExpert = class(TGxAbstractTabEditorExpert)
    function IsUnindent: Boolean; override; 
  end;

  TGxTabUnindentEditorExpert = class(TGxAbstractTabEditorExpert)
    function IsUnindent: Boolean; override; 
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  Menus, Registry, EditIntf;


{ TGxAbstractTabEditorExpert }

constructor TGxAbstractTabEditorExpert.Create;
resourcestring
  SDisplayNameIndent = 'Tab Indent Editor Expert';
  SDisplayNameUnindent = 'Tab Unindent Editor Expert';
var
  Shift: TShiftState;
begin
  inherited Create;
  FUnindent := IsUnindent;
  if FUnindent then begin
    FName := SDisplayNameUnindent;
    Shift := [ssShift];
  end
  else begin
    FName := SDisplayNameIndent;
    Shift := [];
  end;
  FHasConfigOptions := False;
  // The default shortcut key to activate the editor expert.
  ShortCut := Menus.ShortCut(VK_TAB, Shift);
  // LoadSettings is called automatically for the expert upon creation.
end;

procedure TGxAbstractTabEditorExpert.Execute;
begin
  // nothing here
end;

function TGxAbstractTabEditorExpert.ExecuteKeyDown(Manager: TEIManager): Boolean;
var
  ModIntf: TIModuleInterface;
  EditIntf: TIEditorInterface;
  OldKeyState, KeyState: TKeyboardState;
  Hotkey: Char;
begin
  {$IFOPT D+} SendDebug('Executing Tab Indent expert'); {$ENDIF}
  Result := False;
  GetInterfaces(ModIntf, EditIntf);
  try
    if not Assigned(EditIntf) then
      Exit;
    // bail out if there's no text selected
    if (EditIntf.BlockStart.CharIndex = EditIntf.BlockAfter.CharIndex)
        and (EditIntf.BlockStart.Line = EditIntf.BlockAfter.Line) then
      Exit;
  finally
    if Assigned(ModIntf) then
      ModIntf.Free;
    if Assigned(EditIntf) then
      EditIntf.Free;
  end;
  if FUnindent then
    Hotkey := 'U'
  else
    Hotkey := 'I';
  GetKeyboardState(KeyState);
  OldKeyState := KeyState;
  KeyState[VK_CONTROL] := KeyState[VK_CONTROL] or $80;
  KeyState[VK_SHIFT] := KeyState[VK_SHIFT] or $80;
  SetKeyboardState(KeyState);// control+shift down
  try
    SendMessage(Manager.EditControl.Handle, CN_KEYDOWN, Ord(Hotkey), 1);
    SendMessage(Manager.EditControl.Handle, CN_KEYUP, Ord(Hotkey), 1);
  finally
    SetKeyboardState(OldKeyState);
  end;
  Result := True; // key is handled
end;

procedure TGxAbstractTabEditorExpert.GetHelpString(List: TStrings);
resourcestring
  STabIndentEditorExpertHelp =
    'Catch the [TAB] key to provide different functionality.  ' +
    'Pressing [TAB] when a block of code is selected will Indent that block ' +
    'of code by the value specified in Editor Properties -> Block Indent.';
begin
  List.Text := STabIndentEditorExpertHelp;
end;

procedure TGxAbstractTabEditorExpert.LoadSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    // TODO: the value name should probably be different for indent/unindent
    ShortCut := ReadInteger('TabIndentEditorExpert', 'ShortCut', ShortCut);
  finally
    Free;
  end;
end;

procedure TGxAbstractTabEditorExpert.SaveSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    // TODO: the value name should probably be different for indent/unindent
    WriteInteger('TabIndentEditorExpert', 'ShortCut', ShortCut);
  finally
    Free;
  end;
end;

{ TGxTabIndentEditorExpert }

function TGxTabIndentEditorExpert.IsUnindent: Boolean;
begin
  Result := False;
end;

{ TGxTabUnindentEditorExpert }

function TGxTabUnindentEditorExpert.IsUnindent: Boolean;
begin
  Result := True;
end;

initialization
  RegisterEditorExpert(TGxTabIndentEditorExpert);
  RegisterEditorExpert(TGxTabUnindentEditorExpert);

end.
