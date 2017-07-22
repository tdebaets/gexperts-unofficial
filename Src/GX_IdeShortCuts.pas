unit GX_IdeShortCuts;

{$I GX_CondDefine.inc}

// Unit originally by Stefan Hoffmeister
//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  ExptIntf, ToolIntf, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, ComCtrls, ExtCtrls, GX_Experts;

type
  TfmIdeShortCuts = class(TForm)
    MainMenu: TMainMenu;
    gbxMenuItem: TGroupBox;
    lblMenuStruc: TLabel;
    MenuStructure: TEdit;
    lblMenuItemName: TLabel;
    MenuItemName: TEdit;
    chkUseShortcut: TCheckBox;
    lblShortcut: TLabel;
    hkShortcut: THotKey;
    pnlButtons: TPanel;
    btOK: TButton;
    btCancel: TButton;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chkUseShortcutClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    FInternalUpdate: Boolean;
    procedure ChangeMenu;
    procedure ReadFromRegistryCFG;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure WMCommand(var Msg: TWMCommand); message wm_Command;
  end;

  TshShortCutXprt = class(TObject)
  private
    FIdeMainMenu: TMainMenu; // the IDE's main menu
    FOldShortcuts: TStringList; // saves original shortcut settings
    FShortcutNotifier: TIAddInNotifier; // notifies of package loadings
    FMenuTimer: TTimer;
    procedure ReadFromRegistryIDE;
    procedure ResetShortcuts;
    procedure InitializeShortcutExpert;
    procedure FinalizeShortcutExpert;
    procedure InitMenu(Sender: TObject);
  public
    procedure OnClick(Sender: TObject);
    constructor Create;
    destructor Destroy; override;
  end;

  TShortcutExpert = class(TGX_EnhExpert)
  private
    ShortCutExpert: TshShortCutXprt;
  protected
    procedure SetActive(New: Boolean); override;
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
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  Registry, GX_GExperts, GX_ConfigurationInfo, GX_GenFunc;

type
  TShortcutNotifier = class(TIAddInNotifier)
  private
    FShortcutExpert: TshShortCutXprt;
  public
    constructor Create(AOwner: TshShortCutXprt);
    procedure FileNotification(NotifyCode: TFileNotification;
      const FileName: string; var Cancel: Boolean); override;
    // Only needed to avoid the compiler's abstract warnings
    procedure EventNotification(NotifyCode: TEventNotification;
      var Cancel: Boolean); override;
  end;

constructor TShortcutNotifier.Create(AOwner: TshShortCutXprt);
begin
  inherited Create;
  FShortcutExpert := AOwner;
end;

procedure TShortcutNotifier.FileNotification(NotifyCode: TFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  // re-assign shortcuts if a package (which may contain
  // experts with hotkeys) has been loaded.
  // no need to detect unload - we simply are not interested in what's vanishing
  if NotifyCode = fnPackageInstalled then
    FShortcutExpert.ReadFromRegistryIDE;
end;

procedure TShortcutNotifier.EventNotification(NotifyCode: TEventNotification;
  var Cancel: Boolean);
begin
  // do nothing
end;

function FindMenu(AMenuItem: TMenuItem; const Name: string): TMenuItem;
var
  j: Integer;
  TempItem: TMenuItem;
begin
  Result := nil;
  for j := 0 to AMenuItem.Count - 1 do
  begin
    TempItem := AMenuItem.Items[j];
    //{$IFOPT D+}SendDebug('Looking at '+TempItem.Name);{$ENDIF}
    if TempItem.Name = Name then
      Result := TempItem
    else
      Result := FindMenu(TempItem, Name);
    if Result <> nil then
      Break;
  end;
end;

procedure TshShortCutXprt.InitializeShortcutExpert;
var
  AppBuilderObj: TComponent;
begin
  FOldShortcuts := TStringList.Create;
  with FOldShortcuts do
  begin
    Sorted := True;
    Duplicates := dupError;
  end;

  // Do not localize any of these items
  // initialize IDE environment settings
  AppBuilderObj := GetIdeMainForm;
  Assert(AppBuilderObj <> nil, 'IDE AppBuilder object not found');
  FIdeMainMenu := AppBuilderObj.FindComponent('MainMenu1') as TMainMenu;
  Assert(FIdeMainMenu <> nil, 'IDE main menu not found');
  // D4 hack for late create menu items
  FMenuTimer := TTimer.Create(nil);
  //FMenuTImer.Enabled := False;
  FMenuTimer.Interval := 8000;
  FMenuTimer.OnTimer := InitMenu;
  //ReadFromRegistryIDE;
end;

procedure TshShortCutXprt.InitMenu(Sender: TObject);
begin
  if FMenuTimer <> nil then
  begin
    FMenuTimer.Enabled := False;
    FMenuTimer.Free;
    FMenuTimer := nil;
  end;
  try
    ReadFromRegistryIDE;
  except
    on E: Exception do
    begin
      {$IFOPT D+} SendDebug('InitMenu Exception: '+E.Message); {$ENDIF}
    end;
  end;
end;

constructor TshShortCutXprt.Create;
begin
  inherited Create;
  // here we perform non-standard, internal stuff
  // you can disregard the rest of the constructor except for this line
  InitializeShortcutExpert;
end;

procedure TshShortCutXprt.ResetShortcuts;
var
  i: Integer;
  AMenuItem: TMenuItem;
begin
  if Assigned(FOldShortcuts) then
    with FOldShortcuts do
      for i := 0 to Count - 1 do
      begin
        AMenuItem := FindMenu(FIdeMainMenu.Items, Strings[i]);
        if AMenuItem <> nil then
          AMenuItem.Shortcut := TShortCut(Objects[i]);
      end;
end;

procedure TshShortCutXprt.FinalizeShortcutExpert;
begin
  if FShortcutNotifier <> nil then
  begin
    ToolServices.RemoveNotifier(FShortcutNotifier);
    FShortcutNotifier.Free;
    FShortcutNotifier := nil;
  end;
  // Restore old shortcut settings
  ResetShortcuts;
  FOldShortcuts.Free;
  FOldShortcuts := nil;
end;

destructor TshShortCutXprt.Destroy;
begin
  if FMenuTimer <> nil then
    FMenuTimer.Enabled := False;
  FinalizeShortcutExpert;
  inherited Destroy;
end;

procedure TshShortCutXprt.OnClick(Sender: TObject);
begin
  with TfmIdeShortCuts.Create(nil) do
  try
    ResetShortcuts; // restore old shortcuts - to show "original" settings
    ShowModal;
    ReadFromRegistryIDE; // apply (changed) shortcut settings; the form writes to the registry itself
  finally
    Free;
  end;
end;

{**********************************************************************}
{                                                                      }
{  Code for the configuration form itself                              }
{                                                                      }
{**********************************************************************}

{ Create the form and initialize the menu bar. }

procedure TfmIdeShortCuts.FormCreate(Sender: TObject);

var
  FAppBuilderObj: TComponent;
  MainMenu: TMainMenu;
  // Fill the form's menu bar with a facsimile of the IDE's menu bar
  // Partially extracted from Ray Lischner's Expert Toolkit (where it was part of
  // a property editor) with own modifications
  procedure FillMenuBar(Item: TMenuItem; Intf: TMenuItem);
  var
    ChildIntf: TMenuItem;
    ChildItem: TMenuItem;
    i: Integer;
  begin
    ChildItem := nil;
    for i := 0 to Intf.Count-1 do
    begin
      ChildIntf := Intf.Items[I];
      // only show visible entries
      if ChildIntf.Visible and (Length(Trim(ChildIntf.Name)) > 0)
        and not (ChildIntf.Name = 'GExperts') and not (ChildIntf.Name = 'GX_MyWindows')
        and not (ChildIntf.Name = 'GExpert_Menu') then
      begin
        try
          ChildItem := NewItem(ChildIntf.Caption, ChildIntf.ShortCut,
            False, True, MenuClick, 0, ChildIntf.Name);
        except
          on E: Exception do
          begin
            // exception handler works around a bug introduced
            // by CodeRush - CR appears to mess with IDE menu
            // items in an illegal manner 
            Continue;
          end;
        end;
        // store the corresponding IDE TMenuItem in the Tag property
        ChildItem.Tag := Longint(FAppBuilderObj.FindComponent(ChildItem.Name));
        Item.Add(ChildItem);
        FillMenuBar(ChildItem, ChildIntf);
      end;
    end;
  end;

begin
  LoadSettings;
  CenterForm(Self);
  FAppBuilderObj := GetIdeMainForm;
  MainMenu := TMainMenu(FAppBuilderObj.FindComponent('MainMenu1'));  // do not localize
  FillMenuBar(Self.MainMenu.Items, Mainmenu.Items);
end;

{ When the user chooses a menu item, show the item's name. }

procedure TfmIdeShortCuts.MenuClick(Sender: TObject);

  function MenuHierarchy(AMenuItem: TMenuItem): string;
  var
    AmperPos: Integer;
  begin
    Result := AMenuItem.Caption;
    // remove *all* ampersand characters
    //! StH: Note - this will fail for "&&"
    AmperPos := Pos('&', Result);
    while AmperPos <> 0 do
    begin
      Delete(Result, AmperPos, 1);
      AmperPos := Pos('&', Result);
    end;
    if (AMenuItem.Parent <> nil) and
       (AMenuItem.Parent <> MainMenu.Items) then
    begin
      Result := MenuHierarchy(AMenuItem.Parent) + ' | ' + Result;
    end;
  end;

var
  Enable: Boolean;
begin
  ChangeMenu;
  with Sender as TMenuItem do
  begin
    MenuItemName.Tag := Longint(Sender); // store sender in TEdit tag -> simplify access to it
    FInternalUpdate := True; // set a flag; this works around a strange GUI effect with the menus
    try
      // to assign a shortcut, the menu must have a name
      Enable := (Name <> '');
      chkUseShortCut.Checked := Checked and Enable;
      chkUseShortcut.Enabled := Enable;
      chkUseShortCutClick(Self);
      // fill informative edit controls
      MenuStructure.Text := MenuHierarchy(TMenuItem(Sender));
      MenuItemName.Text := Name;
      // and show data
      hkShortcut.Hotkey := Shortcut;
    finally
      FInternalUpdate := False;
    end;
  end;
end;

procedure TfmIdeShortCuts.FormResize(Sender: TObject);
const
  Margin = 4;
var
  TotalHeight: Integer;
begin
  TotalHeight := pnlButtons.BoundsRect.Bottom + Margin;
  if ClientHeight < TotalHeight then
    ClientHeight := TotalHeight;
  if Width > gbxMenuItem.Width then
    gbxMenuItem.Left := (Width - gbxMenuItem.Width) div 2;
  if Width > pnlButtons.Width then
    pnlButtons.Left := (Width - pnlButtons.Width) div 2;
end;

procedure TfmIdeShortCuts.ChangeMenu;
var
  OriginalShortcut: TShortcut;
begin
  // do not change menus if we are updating / creating things
  // this avoids some rather nasty GUI effects with the menus (inexplicable)
  if FInternalUpdate then Exit;
  with MenuItemName do
    if Tag <> 0 then // MenuItemName.Tag = reference to corresponding IDE TMenuItem
      with TObject(Tag) as TMenuItem do
      begin
        Checked := chkUseShortCut.Checked; // mark those menu item as checked where we add a shortcut
        if {TMenuItem.}Tag <> 0 then
          if Checked then
            Shortcut := hkShortcut.Hotkey // and add the shortcut
          else
          begin
            OriginalShortcut := (TObject(Tag) as TMenuItem).ShortCut;
            { TMenuItem(TMenuItem(MenuItemName.Tag).Tag).ShortCut }
            if ShortCut <> OriginalShortcut then
              Shortcut := OriginalShortcut; // the ORIGINAL shortcut
          end;
      end;
end;

procedure TfmIdeShortCuts.chkUseShortcutClick(Sender: TObject);
var
  ShortCutChecked: Boolean;
begin
  ShortCutChecked := chkUseShortCut.Checked;
  hkShortcut.Enabled := ShortCutChecked;
  lblShortcut.Enabled := ShortCutChecked;
  ChangeMenu;
end;

procedure TfmIdeShortCuts.WMCommand(var Msg: TWMCommand);
begin
  inherited;
  // HOTKEY common controls send a notification to their parent
  // if they change their value. NotifyCode = $300 has been
  // determined by inspection with WinSight32 (no docs found)
  with Msg do
    if (NotifyCode = $300) and
       (Ctl = hkShortcut.Handle) and
       (chkUseShortcut.Checked) then
    begin
      ChangeMenu;
    end;
end;

procedure TfmIdeShortCuts.FormShow(Sender: TObject);
begin
  MenuItemName.Text := '';
  ReadFromRegistryCFG;
end;

(*
{ compare function for TList.Sort }
function scCompare(Item1, Item2: Pointer): Integer;
begin
  with TMenuItem(Item1) do
  begin
    if ShortCut < TMenuItem(Item2).ShortCut then
      Result := -1
    else
    if ShortCut > TMenuItem(Item2).ShortCut then
      Result := 1
    else
      Result := 0;
  end;
end;
*)

procedure TfmIdeShortCuts.btOKClick(Sender: TObject);
var
  ARegistry: TRegistry;

  procedure WriteToRegistry(AMenuItem: TMenuItem);
  var
    j: Integer;
    ChildItem: TMenuItem;
  begin
    for j := 0 to AMenuItem.Count - 1 do
    begin
      ChildItem := AMenuItem.Items[j];
      with ChildItem do
        if Checked then
          ARegistry.WriteInteger(Name, ShortCut);
      WriteToRegistry(ChildItem);
    end;
  end;

(*
Due to the shortcuts in our own menu, there is no
chance to get a duplicate menu item.

This code nevertheless allows checking for duplicate
shortcuts.

var
  ShortCutList: TList; // used

    procedure AddToShortcuts(AMenuItem: TMenuItem);
    var
      j: Integer;
      ChildItem: TMenuItem;
    begin
      for j := 0 to AMenuItem.Count-1 do
      begin
        ChildItem := AMenuItem.Items[j];
        if ChildItem.Checked then
          ShortcutList.Add(ChildItem);

        AddToShortcuts(ChildItem);
      end;
    end;
*)

var
  RegValues: TStringList;

  i: Integer;
begin
(*
  // test whether there are duplicate shortcuts
  ShortCutList := TList.Create;
  try
    AddToShortcuts(MainMenu.Items);

    ShortcutList.Sort(scCompare);

    for i := 0 to ShortcutList.Count -1 -1 do
    begin
      if TMenuItem(ShortCutList[i]).Shortcut = TMenuItem(ShortCutList[i+1]).Shortcut then
        if MessageDlg('At least one duplicate shortcut detected:'#13+
                       Format('  %s <-> %s'#13, [ TMenuItem(ShortCutList[i]).Caption,
                                                  TMenuItem(ShortCutList[i+1]).Caption]) +
                       'Do you want to edit your settings?',
                       mtWarning, [mbYes, mbNo], 0) = mrYes then
          exit // finally is executed!
        else
          break;
    end;

  finally
    ShortCutList.Free;
  end; { try .. finally }
*)
  // store settings in registry
  ChangeMenu;
  ARegistry := TRegistry.Create;
  try
    RegValues := TStringList.Create;
    try
      with ARegistry do
        if OpenKey(ConfigInfo.RegKey + '\GExperts\IDEShortcuts', True) then // allow to create key
        try
          GetValueNames(RegValues);
          // remove all existing entries
          for i := 0 to RegValues.Count - 1 do
            DeleteValue(RegValues[i]);
          // write current entries
          WriteToRegistry(MainMenu.Items);
        finally
          CloseKey;
        end;
    finally
      RegValues.Free;
    end;
  finally
    ARegistry.Free;
  end;
  ModalResult := mrOK;
end;

procedure TfmIdeShortCuts.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('ShortCutWindow', 'Left', Left);
    RegIni.WriteInteger('ShortCutWindow', 'Top', Top);
    RegIni.WriteInteger('ShortCutWindow', 'Width', Width);
    RegIni.WriteInteger('ShortCutWindow', 'Height', Height);
  finally
    RegIni.Free;
  end;
end;

procedure TfmIdeShortCuts.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    Left := RegIni.ReadInteger('ShortCutWindow', 'Left', Left);
    Top := RegIni.ReadInteger('ShortCutWindow', 'Top', Top);
    Width := RegIni.ReadInteger('ShortCutWindow', 'Width', Width);
    Height := RegIni.ReadInteger('ShortCutWindow', 'Height', Height);
  finally
    RegIni.Free;
  end;
end;

// read shortcut settings from the registry
// into our local configuration menu
procedure TfmIdeShortCuts.ReadFromRegistryCFG;
var
  i: Integer;
  RegValues: TStringList;
  ARegistry: TRegistry;
  AMenuItem: TMenuItem;
begin
  RegValues := TStringList.Create;
  try
    ARegistry := TRegistry.Create;
    try
      with ARegistry do
        if OpenKey(ConfigInfo.RegKey + '\GExperts\IDEShortcuts', False) then
        try
          GetValueNames(RegValues);
          for i := 0 to RegValues.Count - 1 do
          begin
            AMenuItem := FindMenu(MainMenu.Items, RegValues[i]);
            if AMenuItem <> nil then
              with AMenuItem do
              begin
                Checked := True;
                ShortCut := ReadInteger(RegValues[i]);
              end;
          end;
        finally
          CloseKey;
        end;
    finally
      ARegistry.Free;
    end;
  finally
    RegValues.Free;
  end;
end;

// read shortcut settings from the registry
// into the IDE
procedure TshShortCutXprt.ReadFromRegistryIDE;
var
  i: Integer;
  RegValues: TStringList;
  ARegistry: TRegistry;
  AMenuItem: TMenuItem;
  OldShortcutIdx: Integer;
begin
  ARegistry := TRegistry.Create;
  try
    RegValues := TStringList.Create;
    try
      with ARegistry do
        if OpenKey(ConfigInfo.RegKey + '\GExperts\IDEShortcuts', False) then
        try
          GetValueNames(RegValues);
          {$IFOPT D+}SendDebug('Setting shortcuts for '+RegValues.Text);{$ENDIF}
          for i := 0 to RegValues.Count - 1 do
          begin
            {$IFOPT D+}SendDebug('Looking for '+RegValues[i]);{$ENDIF}
            AMenuItem := FindMenu(FIdeMainMenu.Items, RegValues[i]);
            if AMenuItem <> nil then
            begin
              {$IFOPT D+}SendDebug('Found '+RegValues[i]);{$ENDIF}
              // store old shortcut in list
              if not FOldShortcuts.Find(AMenuItem.Name, OldShortcutIdx) then // not in list?
                FOldShortcuts.AddObject(AMenuItem.Name, TObject(AMenuItem.ShortCut)); // store
              AMenuItem.ShortCut := ReadInteger(RegValues[i]);
              {$IFOPT D+}SendDebug('Set Shortcut to '+ShortcutToText(AMenuItem.ShortCut));{$ENDIF}
            end;
          end;
        finally
          CloseKey;
        end;
    finally
      RegValues.Free;
    end;
  finally
    ARegistry.Free;
  end;
end;

procedure TfmIdeShortCuts.FormDestroy(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfmIdeShortCuts.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 23);
end;

{ TShortcutExpert }

constructor TShortcutExpert.Create;
begin
  inherited Create;
  ShortCutExpert := TshShortCutXprt.Create;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

destructor TShortCutExpert.Destroy;
begin
  ShortCutExpert.Free;
  ShortCutExpert := nil;
  inherited Destroy;
end;

function TShortCutExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&IDE Menu Shortcuts...';
begin
  Result := SMenuCaption;
end;

function TShortCutExpert.GetMenuName: string;
begin
  Result := 'GX_Shortcut';
end;

function TShortCutExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TShortCutExpert.GetName: string;
begin
  Result := 'ShortCut_Expert';
end;

function TShortCutExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'IDE Menu Shortcuts';
begin
  Result := SDisplayName;
end;

procedure TShortCutExpert.Click(Sender: TObject);
begin
  if ShortCutExpert = nil then
    ShortCutExpert := TshShortCutXprt.Create;
  ShortCutExpert.OnClick(Sender);
end;

function TShortcutExpert.IconFileName: string;
begin
  Result := 'Keyboard';
end;

// Can someone re-verify this is all we need below?
procedure TShortcutExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
    begin
      if ShortCutExpert = nil then
        ShortCutExpert := TshShortCutXprt.Create;
      ShortCutExpert.ReadFromRegistryIDE;
    end
    else
    begin
      // IDE Shortcuts is modal, so the form will not exist here
      // Remove the modified shortcuts and restore the defaults
      ShortCutExpert.Free;
      ShortCutExpert := nil;
    end;
  end;
end;

initialization
{$IFNDEF GX_NotPackageBuilt}
  RegisterGX_Expert(TShortCutExpert);
{$ENDIF GX_NotPackageBuilt}
end.

