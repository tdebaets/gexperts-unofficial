{***************************************************************
 * Unit Name: GX_SampleExpert
 * Purpose  : Template for creation of new GExperts experts
 *          : Customize this when creating new experts
 * Authors  : Stefan Hoffmeister, Scott Mattes, Erik Berry
 ****************************************************************}

unit GX_SampleExpert;

{$I GX_CondDefine.inc}

interface

uses
  ToolIntf, ExptIntf,
  GX_Experts,
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls;

type
  TfmGxSampleExpertForm = class(TForm)
    btnOk: TButton;
    lblNote: TLabel;
    lblData: TLabel;
    edtData: TEdit;
    btnCancel: TButton;
  end;

  // Descend all new experts from TGX_EnhExpert
  TGxSampleExpert = class(TGX_EnhExpert)
  private
    FSomeData: string;
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function IconFileName: string; override;
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Click(Sender: TObject); override;
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  Registry, Menus,
  GX_GExperts, GX_ConfigurationInfo;

{ TGxSampleExpert }

//*********************************************************
//    Name: TGxSampleExpert.Click
// Purpose: The action taken when the menu item is clicked
//*********************************************************
procedure TGxSampleExpert.Click(Sender: TObject);
begin
  with TfmGxSampleExpertForm.Create(nil) do
  try
    edtData.Text := FSomeData;
    if ShowModal = mrOk then
    begin
      FSomeData := edtData.Text;
      SaveSettings;
     end;
  finally
    Release;
  end;
end;

//*********************************************************
//    Name: TGxSampleExpert.Configure
// Purpose: Action taken when user clicks the Configure
//          button on the Experts tab of menu item GExperts/
//          GExperts Configuration...
//*********************************************************
procedure TGxSampleExpert.Configure;
resourcestring
  SYouClickedConfigure = 'You clicked the Configuration button!';
begin
  MessageDlg(SYouClickedConfigure, mtInformation, [mbOK], 0);
end;

constructor TGxSampleExpert.Create;
begin
  inherited Create;

  // This expert should have a configure button in the configuration dialog
  HasConfigOptions := True;
  // This expert has a visible menu item in the GExperts top level menu
  HasMenuItem := True;

  // Assign a default value to your data
  FSomeData := 'string data';

  // If desired, assign a default menu item shortcut
  ShortCut := Menus.ShortCut(Word('Z'), [ssCtrl, ssShift, ssAlt]);

  // Saved settings are loaded automatically for you by the ancestor
  // via the virtual LoadSettings method
end;

destructor TGxSampleExpert.Destroy;
begin
  inherited Destroy;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetDisplayName
// Purpose: Returns the string displayed on the Experts tab
//          on menu item GExperts/GExperts Configuration...
//*********************************************************
function TGxSampleExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'GExperts Sample Expert';
begin
  Result := SDisplayName;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetMenuCaption
// Purpose: Returns the string displayed on the GExperts
//          menu item
//*********************************************************
function TGxSampleExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Sample Expert Menu Item...';
begin
  Result := SMenuCaption;
end;

//*********************************************************
//    Name: TGxSampleExpert.GetMenuMask
// Purpose: Specifies the file extensions for which the
//          menu item is available, an empty string means
//          always available. Ex: '*.PAS,*.DPR'
//          The values should be uppercase
//*********************************************************
function TGxSampleExpert.GetMenuMask: string;
begin
  //Result := '*.PAS;*.DPR;*.INC';
  Result := '';
end;

//*********************************************************
//    Name: TGxSampleExpert.GetMenuName
// Purpose: Returns the unique component name of the
// menu item.
//*********************************************************
function TGxSampleExpert.GetMenuName: string;
begin
  Result := 'GX_TheUniqueNameOfTheMenuItem';
end;

//*********************************************************
//    Name: TGxSampleExpert.GetName
// Purpose: Used to determine the unique keyword used to
// save the active state and shortcut into the registry
//*********************************************************
function TGxSampleExpert.GetName: string;
begin
  Result := 'TheUniqueNameOfTheExpert';
end;

//*********************************************************
//    Name: TGxSampleExpert.IconFileName
// Purpose: Specifies the icon used on the GExperts menu item
//    Note: Be sure the icon file is in the 'Icons' directory
//          located under the directory specified on the
//          File Locations tab of the menu item
//          GExperts/GExperts Configuration
//*********************************************************
function TGxSampleExpert.IconFileName: string;
begin
  Result := 'SampleExpert'; // No .ico extension
end;

//*********************************************************
//    Name: TGxSampleExpert.LoadSettings
// Purpose: Gets the expert settings from the registry
//*********************************************************
procedure TGxSampleExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // Read your persistent settings here, for example
    FSomeData := RegIni.ReadString('SampleExpert', 'TestSetting', FSomeData);
  finally
    RegIni.Free;
  end;

  inherited LoadSettings;
end;

//*********************************************************
//    Name: TGxSampleExpert.SaveSettings
// Purpose: Saves the expert settings to the registry
// You must call this manually in the destructor
//*********************************************************
procedure TGxSampleExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    // Write your persistent settings here, for example
    RegIni.WriteString('SampleExpert', 'TestSetting', FSomeData);
  finally
    RegIni.Free;
  end;
end;

//********************************************************************
//    Name: TGxSampleExpert.SetActive
// Purpose: Called to clean up the expert when it is disabled at runtime
// or destroyed on shutdown.  Free any forms and objects here.
// The ancestor removes the menu item for you.
//********************************************************************
procedure TGxSampleExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Initialize anything necessary here (generally empty)
    else
    begin
      // If we had a non-modal form, it would be destroyed here
      //FSampleForm.Free;
      //FSampleForm := nil;
    end;
  end;
end;

//*********************************************************
// Purpose: Lets GExperts know about this new expert
//*********************************************************
initialization
  RegisterGX_Expert(TGxSampleExpert);
end.

