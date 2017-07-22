/*
 ***************************************************************
 * Unit Name: GX_CppSampleExpert
 * Purpose  : Template for creation of new GExperts experts
 *          : Customize this when creating new experts
 * Authors  : Stefan Hoffmeister, Scott Mattes, Erik Berry
 ****************************************************************
*/
//---------------------------------------------------------------------------

#include "GX_PrecompiledHeader.h"
#pragma hdrstop

#include "GX_CppSampleExpert.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

//---------------------------------------------------------------------------
__fastcall TfmGxCppSampleExpertForm::TfmGxCppSampleExpertForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------

//*********************************************************
//    Name: TGxCppSampleExpert::Click
// Purpose: The action taken when the menu item is clicked
//*********************************************************

void __fastcall TGxCppSampleExpert::Click(TObject* Sender)
{
  TfmGxCppSampleExpertForm* GxSampleExpertForm;

  GxSampleExpertForm = new TfmGxCppSampleExpertForm(0);
  try
  {
    GxSampleExpertForm->edtData->Text = FSomeData;
    if (GxSampleExpertForm->ShowModal() == mrOk)
    {
      FSomeData = GxSampleExpertForm->edtData->Text;
      this->SaveSettings();
    }
  }
  __finally
  {
    GxSampleExpertForm->Release();
  }
}

//*********************************************************
//    Name: TGxCppSampleExpert::Configure
// Purpose: Action taken when user clicks the Configure
//          button on the Experts tab of menu item GExperts/
//          GExperts Configuration...
//*********************************************************
void __fastcall TGxCppSampleExpert::Configure(void)
{
  const String SYouClickedConfigure = "You clicked the Configuration button!";

  MessageDlg(SYouClickedConfigure, mtInformation, TMsgDlgButtons() << mbOK, 0);
}

__fastcall TGxCppSampleExpert::TGxCppSampleExpert(void)
  : inherited()
{
  // This expert should have a configure button in the configuration dialog
  HasConfigOptions = true;
  // This expert has a visible menu item in the GExperts top level menu
  HasMenuItem = true;

  // Assign a default value to your data
  FSomeData = "string data";

  // If desired, assign a default menu item shortcut
  ShortCut = Menus::ShortCut(Word('Z'), TShiftState() << ssCtrl << ssShift << ssAlt);

  // Saved settings are loaded automatically for you by the ancestor
  // via the virtual LoadSettings method
}

__fastcall TGxCppSampleExpert::~TGxCppSampleExpert(void)
{
  // Nothing to destroy.
}

//*********************************************************
//    Name: TGxCppSampleExpert::GetDisplayName
// Purpose: Returns the string displayed on the Experts tab
//          on menu item GExperts/GExperts Configuration...
//*********************************************************
String __fastcall TGxCppSampleExpert::GetDisplayName(void)
{
  const String SDisplayName = "GExperts C++ Sample Expert";

  return SDisplayName;
}

//*********************************************************
//    Name: TGxCppSampleExpert::GetMenuCaption
// Purpose: Returns the string displayed on the GExperts
//          menu item
//*********************************************************
String __fastcall TGxCppSampleExpert::GetMenuCaption(void)
{
  const String SMenuCaption = "C++ Sample Expert Menu Item...";

  return SMenuCaption;
}

//*********************************************************
//    Name: TGxCppSampleExpert::GetMenuMask
// Purpose: Specifies the file extensions for which the
//          menu item is available, an empty string means
//          always available. Ex: '*.PAS,*.DPR'
//          The values should be uppercase
//*********************************************************
String __fastcall TGxCppSampleExpert::GetMenuMask(void)
{
  //return "*.PAS;*.DPR;*.INC;*.CPP;*.HPP";
  return "";
}

//*********************************************************
//    Name: TGxCppSampleExpert::GetMenuName
// Purpose: Returns the unique component name of the
//          menu item.
//*********************************************************
String __fastcall TGxCppSampleExpert::GetMenuName(void)
{
  return "GX_TheUniqueNameOfTheMenuItemCpp";
}

//*********************************************************
//    Name: TGxCppSampleExpert::GetName
// Purpose: Used to determine the unique keyword used to
// save the active state and shortcut into the registry
//*********************************************************
String __fastcall TGxCppSampleExpert::GetName(void)
{
  return "TheUniqueNameOfTheExpertCpp";
}

//*********************************************************
//    Name: TGxCppSampleExpert::IconFileName
// Purpose: Specifies the icon used on the GExperts menu item
//    Note: Be sure the icon file is in the 'Icons' directory
//          located under the directory specified on the
//          File Locations tab of the menu item
//          GExperts/GExperts Configuration
//*********************************************************
String __fastcall TGxCppSampleExpert::IconFileName(void)
{
  return "SampleExpert"; // No .ico extension
}

//*********************************************************
//    Name: TGxCppSampleExpert::LoadSettings
// Purpose: Gets the expert settings from the registry
//*********************************************************
void __fastcall TGxCppSampleExpert::LoadSettings(void)
{
  TRegIniFile* RegIni;

  RegIni = new TRegIniFile(ConfigInfo()->RegKey + "\\GExperts");
  try
  {
    // Read your persistent settings here, for example
    FSomeData = RegIni->ReadString("SampleExpert", "TestSetting", FSomeData);
  }
  __finally
  {
    delete RegIni;
  }

  inherited::LoadSettings();
}

//*********************************************************
//    Name: TGxCppSampleExpert::SaveSettings
// Purpose: Saves the expert settings to the registry
// You must call this manually in the destructor
//*********************************************************
void __fastcall TGxCppSampleExpert::SaveSettings(void)
{
  TRegIniFile* RegIni;

  inherited::SaveSettings();

  RegIni = new TRegIniFile(ConfigInfo()->RegKey + "\\GExperts");
  try
  {
    // Write your persistent settings here, for example
    RegIni->WriteString("SampleExpert", "TestSetting", FSomeData);
  }
  __finally
  {
    delete RegIni;
  }
}

//********************************************************************
//    Name: TGxCppSampleExpert::SetActive
// Purpose: Called to clean up the expert when it is disabled at runtime
// or destroyed on shutdown.  Free any forms and objects here.
// The ancestor removes the menu item for you.
//********************************************************************
void __fastcall TGxCppSampleExpert::SetActive(bool New)
{
  if (New != Active)
  {
    inherited::SetActive(New);
    if (New)
    {
      // Initialize anything necessary here (generally empty)
    }
    else
    {
      // If we had a non-modal form, it would be destroyed here
      //delete FSampleForm;
      //FSampleForm = 0;
    }
  }
}

//*********************************************************
// Purpose: Lets GExperts know about this new expert
//*********************************************************
namespace // anonymous
{
  void ExpertRegistration(void)
  {
    #pragma startup ExpertRegistration
    RegisterGX_Expert(__classid(TGxCppSampleExpert));
  }
}
