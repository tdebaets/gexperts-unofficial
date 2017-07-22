//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("GDebug.res");
USEFORMNS("DebugWindow.pas", Debugwindow, fmDebug);
USEFORMNS("DebugOptions.pas", Debugoptions, fmDebugOptions);
USEUNIT("TrayIcon.pas");
//---------------------------------------------------------------------------
#include "DebugOptions.hpp"

//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
 try
 {
   Application->Initialize();
   Application->ShowMainForm = ConfigInfo->Start;
   Application->Title = "GExperts Debug Window";
                 Application->CreateForm(__classid(TfmDebug), &fmDebug);
                 Application->Run();
 }
 catch (Exception &exception)
 {
   Application->ShowException(&exception);
 }
 return 0;
}
//---------------------------------------------------------------------------
