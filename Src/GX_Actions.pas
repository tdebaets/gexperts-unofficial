unit GX_Actions;

{$I GX_CondDefine.inc}

interface

uses
  {$IFDEF GX_HasActionSupport}
  ActnList,
  {$ENDIF GX_HasActionSupport}
  Classes, Menus, GX_Experts, ExtCtrls;

type
  TGXActionManager = class(TObject)
  private
    {$IFDEF GX_VER120_up}
    FMenuTimer: TTimer;
    {$IFNDEF GX_VER130_up}
    FMenuInitCount: Integer;
    {$ENDIF not GX_VER130_up}
    {$ENDIF GX_VER120_up}
    FGExpertsMenu: TMenuItem;
    procedure ConfigClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure InitShortCuts(Sender: TObject);
    function GetAlphabetical: Boolean;
    procedure SetAlphabetical(Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property GExpertsMenu: TMenuItem read FGExpertsMenu;
    function CreateAction(ACaption, AName: string; OnExecute: TNotifyEvent;
      ImageIndex: Integer; ShortCut: TShortCut; Tag, Index: Integer): TGXAction;
    procedure AddExpertAction(Expert: TGX_Expert);
    procedure RemoveExpertAction(Expert: TGX_Expert);
    procedure UpdateShortCuts;
    property Alphabetical: Boolean read GetAlphabetical write SetAlphabetical;
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

function GXActionManager: TGXActionManager;

procedure CreateGXActionManager;
procedure FreeGXActionManager;

implementation

uses
  {$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
  {$ENDIF GX_UseNativeToolsApi}
  GX_ConfigurationInfo,
  {$IFOPT D+}GX_DbugIntf,{$ENDIF D+}
  Forms, GX_GExperts, SysUtils, GX_GenFunc;

// Hard-coded action captions
resourcestring
  SGxConfigMenu = 'GExperts-unofficial &Configuration...';
  SGxAboutMenu = 'About GE&xperts-unofficial...';

var
  PrivateGXActionManager: TGXActionManager;
  CanFree: Boolean;

function GXActionManager: TGXActionManager;
begin
  Assert(PrivateGXActionManager <> nil, 'PrivateGXActionManager is nil');
  Result := PrivateGXActionManager;
end;

procedure CreateGXActionManager;
begin
  Assert(PrivateGXActionManager = nil, 'PrivateGXActionManager is not nil');

  // TGXActionManager.NewInstance will assign
  // a meaningful value to PrivateGXActionManager
  PrivateGXActionManager := TGXActionManager.Create;
end;

procedure FreeGXActionManager;
begin
  // Anything else would be an error in assumptions;
  // nothing fatal, but not good.
  Assert(PrivateGXActionManager <> nil, 'PrivateGXActionManager is nil');

  CanFree := True;

  PrivateGXActionManager.Free;
  PrivateGXActionManager := nil;
end;

constructor TGXActionManager.Create;
var
  MainMenu: TMainMenu;
  Temp: TMenuItem;
begin
  inherited Create;

  // Create GExperts drop down menu.
  FGExpertsMenu := TMenuItem.Create(nil);
  FGExpertsMenu.Caption := 'GE&xperts'; // do not localize
  FGExpertsMenu.Name := 'GExperts'; // do not localize

  // Insert GExperts drop down menu.
  {$IFDEF GX_UseNativeToolsApi}
  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  {$ELSE}
  MainMenu := Application.MainForm.Menu;
  {$ENDIF GX_UseNativeToolsApi}
  Assert(Assigned(MainMenu), 'MainMenu component not found');
  MainMenu.Items.Insert(MainMenu.Items.Count - 2, FGExpertsMenu);

  // Insert separator
  Temp := TMenuItem.Create(FGExpertsMenu);
  Temp.Caption := '-'; // do not localize
  Temp.Name := 'GX_Sep1'; // do not localize
  FGExpertsMenu.Add(Temp);

  // Add hard-coded Actions
  // do not localize
  CreateAction(SGxConfigMenu, 'GX_CONFIG', ConfigClick, -1, 0, 0, 1);
  CreateAction(SGxAboutMenu, 'GX_ABOUT', AboutClick, -1, 0, 0, 2);

  // Kludge to work around VCL 4.0 menu shortcut bug
  {$IFDEF GX_VER120_up}
  FGExpertsMenu.OnClick := InitShortcuts;
  {$ENDIF GX_VER120_up}

  {$IFDEF GX_VER120_up} // We need this kludge for version 4/5 menu item restrictions
  FMenuTimer := TTimer.Create(nil);
  FMenuTimer.Interval := 10000;
  FMenuTimer.OnTimer := InitShortcuts;
  FMenuTimer.Enabled := True;
  {$ENDIF GX_VER120_up}
  UpdateShortCuts;
end;

destructor TGXActionManager.Destroy;
begin
  // If the timer still exists, free it
  {$IFDEF GX_VER120_up}
  if FMenuTimer <> nil then
  begin
    FMenuTimer.Enabled := False;
    FMenuTimer.Free;
    FMenuTimer := nil;
  end;
  {$ENDIF GX_VER120_up}

  // Free the top-level GExperts menu item (all other stuff is freed with it).
  FGExpertsMenu.Free;
  inherited Destroy;
end;

class function TGXActionManager.NewInstance: TObject;
begin
  Assert(PrivateGXActionManager = nil, ClassName + ' has already been created');

  Result := inherited NewInstance;
  TObject(PrivateGXActionManager) := Result;
end;

procedure TGXActionManager.FreeInstance;
begin
  Assert(CanFree, ClassName + ' cannot be freed at this time');
  Assert(PrivateGXActionManager <> nil, ClassName + ' has already been freed');

  inherited FreeInstance;
end;

// Create an action and add it to the GExperts menu.
function TGXActionManager.CreateAction(ACaption, AName: string; OnExecute: TNotifyEvent;
  ImageIndex: Integer; ShortCut: TShortCut; Tag, Index: Integer): TGXAction;
{$IFDEF GX_HasActionSupport}
var
  Temp: TMenuItem;
{$ENDIF GX_HasActionSupport}
begin
  {$IFDEF GX_HasActionSupport} // Action support
  // Create the Action.
  Result := TAction.Create(GetIdeMainForm);
  Result.Caption := ACaption;
  Result.Name := AName;
  // TODO -cImplement -oAnyone: Set the action Hint from the DisplayName of the Expert
  Result.OnExecute := OnExecute;
  Result.ImageIndex := ImageIndex;
  Result.ShortCut := ShortCut;
  Result.Category := 'GExperts'; // Do not localize
  // Register the action
  Result.ActionList := (BorlandIDEServices as INTAServices).ActionList;
  // Create the menu item and link it to the Action.
  Temp := TMenuItem.Create(Result);
  Temp.Action := Result;
  // Save the tag into the menu item for later insertion.
  Temp.Tag := Tag;
  // Insert the menu item.
  FGExpertsMenu.Insert(Index, Temp);
  {$ELSE} // No Action support
  // Create menu item
  Result := TMenuItem.Create(FGExpertsMenu);
  Result.Caption := ACaption;
  Result.Name := AName;
  Result.OnClick := OnExecute;
  Result.ShortCut := ShortCut;
  // Save the tag into the menu item for later insertion.
  Result.Tag := Tag;
  // Insert the menu item.
  FGExpertsMenu.Insert(Index, Result);
  {$ENDIF GX_HasActionSupport}
end;

procedure TGXActionManager.AddExpertAction(Expert: TGX_Expert);
var
  i: Integer;
  Index: Integer;
  AMenuCaption: string;
  S: string;
begin
  Assert(Expert <> nil, 'Invalid nil parameter for AddExpertAction');

  // Search the position to insert.
  Index := FGExpertsMenu.Count - 3; // FIXME!!! Hard-coded, undocumented offset
  if Alphabetical then
  begin
    // Remove ampersand ("&") from the caption
    AMenuCaption := Expert.GetMenuCaption;
    if Pos('&', AMenuCaption) <> -1 then
      Delete(AMenuCaption, Pos('&', AMenuCaption), 1);
    for i := 0 to FGExpertsMenu.Count - 4 do
    begin
      S := FGExpertsMenu[i].Caption;
      // Remove ampersand ("&") from the caption
      if Pos('&', S) <> -1 then
        Delete(S, Pos('&', S), 1);
      if CompareText(AMenuCaption, S) < 0 then
      begin
        Index := i;
        Break;
      end;
    end;
  end
  else
  begin
    for i := 0 to FGExpertsMenu.Count - 4 do
      if Expert.ExpertIndex < TGX_Expert(FGExpertsMenu[i].Tag).ExpertIndex then
      begin
        Index := i;
        Break;
      end;
  end;
  // Create the action
  with Expert do
  begin
    Action := CreateAction(GetMenuCaption, GetMenuName, Click, ImageIndex,
      ShortCut, Integer(Expert), Index);
  end;

  // Add action to the project notify list.
  if Trim(Expert.GetMenuMask) <> '' then
    GExpertsInst.ProjectNotifier.MenuItems.AddObject(Expert.GetMenuMask, Expert.Action);
end;

procedure TGXActionManager.RemoveExpertAction(Expert: TGX_Expert);
begin
  Assert(Expert <> nil, 'Invalid nil Expert parameter for AddExpertAction');

  // Remove the action form the project notify list
  if Expert.GetMenuMask <> '' then
  begin
    Assert(GExpertsInst <> nil, 'GExpertsInst is nil in RemoveExpertAction');
    // Assertions happen here when you have a menu mask on a TGX_EnhExpert
    // We need to rethink this logic sometime. Test case: Components to Code
    Assert(GExpertsInst.ProjectNotifier <> nil); //??
    Assert(GExpertsInst.ProjectNotifier.MenuItems <> nil); //??

    with GExpertsInst.ProjectNotifier.MenuItems do
      Delete(IndexOf(Expert.GetMenuMask));
  end;
  // Set OnExecute to nil to kill already existing links
  // Then, free the action and set it to nil
  {$IFDEF GX_HasActionSupport}
  Expert.Action.OnExecute := nil;
  {$ENDIF GX_HasActionSupport}
  Expert.Action.Free;
  Expert.Action := nil;
end;

procedure TGXActionManager.ConfigClick(Sender: TObject);
begin
  ShowGXConfigurationForm;
end;

procedure TGXActionManager.AboutClick(Sender: TObject);
begin
  ShowGXAboutForm;
end;

procedure TGXActionManager.InitShortcuts(Sender: TObject);
var
  i: Integer;
  Action: TGXAction;
begin
  // Part of kludge for menu initialization.  The IDE keybinding system resets
  // our menu item shortcuts on startup and after loading a project group in D5+.
  Assert(GExpertsInst <> nil, 'GExpertsInst is nil in InitShortcuts');

  {$IFNDEF GX_VER130_up}
  {$IFDEF GX_VER120_up}
  // Stop re-initializing after three tries under Delphi 4-, not under D5+
  Inc(FMenuInitCount);
  if FMenuInitCount > 3 then
  begin
    FMenuTimer.Free;
    FMenuTimer := nil;
  end;
  {$ENDIF GX_VER120_up}
  {$ENDIF not GX_VER130_up}

  for i := 0 to GExpertsInst.ExpertCount - 1 do
  begin
    Action := GExpertsInst.ExpertList[i].Action;
    if Action <> nil then
      Action.Shortcut := GExpertsInst.ExpertList[i].ShortCut;
  end;
end;

procedure TGXActionManager.UpdateShortCuts;
begin
  InitShortCuts(nil);
end;

function TGXActionManager.GetAlphabetical: Boolean;
begin
  Result := ConfigInfo.AlphabetizeMenu;
end;

procedure TGXActionManager.SetAlphabetical(Value: Boolean);
var
  i, j: Integer;
  AExpertIndex: Integer;
  AMenuCaption, S: string;
begin
  if Value <> Self.Alphabetical then
  begin
    ConfigInfo.AlphabetizeMenu := Value;
    if Value then
    begin
      // TODO -oStefan -cBug: Bug
      for i := 0 to FGExpertsMenu.Count - 4 do // FIXME! Hard-coded; also there is almost identical code above
      begin
        // Remove ampersand ("&") from the caption
        AMenuCaption := FGExpertsMenu[i].Caption;
        if Pos('&', AMenuCaption) <> -1 then
          Delete(AMenuCaption, Pos('&', AMenuCaption), 1);

        for j := 0 to i - 1 do
        begin
          S := FGExpertsMenu[j].Caption;
          // Remove ampersand ("&") from the caption
          if Pos('&', S) <> -1 then
            Delete(S, Pos('&', S), 1);

          if CompareText(AMenuCaption, S) < 0 then
          begin
            FGExpertsMenu[i].MenuIndex := j;
            Break;
          end;
        end;
      end;
    end
    else
    begin
      for i := 0 to FGExpertsMenu.Count - 4 do
      begin
        //SendDebug(Format('Sorting Expert #%d: %s, Position: %d', [TGX_Expert(FGExpertsMenu[i].Tag).ExpertIndex, TGX_Expert(FGExpertsMenu[i].Tag).GetMenuCaption, i);
        AExpertIndex := TGX_Expert(FGExpertsMenu[i].Tag).ExpertIndex;
        for j := 0 to i -1 do
        begin
          //SendDebug(Format('Comparing to Expert #%d: %s, Position: %d', [TGX_Expert(FGExpertsMenu[i].Tag).ExpertIndex, TGX_Expert(FGExpertsMenu[i].Tag).GetMenuCaption, i);
          if AExpertIndex < TGX_Expert(FGExpertsMenu[j].Tag).ExpertIndex then
          begin
            // Reinsert the expert at the new location
            FGExpertsMenu[i].MenuIndex := j;
            Break;
          end;
        end;
      end;
    end;
  end;
end;

initialization

finalization
  // When Delphi itself crashes just before shutdown (common in D4/D5 when
  // writing the DSK and DOF files), this assertion just compounds problems
  {$IFOPT D+}Assert(PrivateGXActionManager = nil, 'PrivateGXActionManager is not nil during finalization');{$ENDIF}

end.

