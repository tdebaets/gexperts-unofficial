
#include <windows.h>
#pragma hdrstop
#include <condefs.h>

USERES("ExpMgr.res");
//---------------------------------------------------------------------------
#if (__BORLANDC__ == 0x0530) // C++Builder 3.0
  #define LinkedDll "GExpert3_bcb.dll"
#elif (__BORLANDC__ == 0x0540)  // C++Builder 4.0
  #define LinkedDll "GExpert4_bcb.dll"
#elif (__BORLANDC__ == 0x0550)  // C++Builder 5.0
  #define LinkedDll "GExpert5_bcb.dll"
#else
  Unknown version of C++Builder
#endif

void Error(const char * const ErrorMessage)
{
  MessageBox(0, ErrorMessage, "Error", MB_ICONERROR | MB_OK);
  throw(ErrorMessage);
}

//---------------------------------------------------------------------------
#pragma argsused
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  typedef  void __stdcall (*PlainProcedure)(void);  

  HANDLE GExpertsDllHandle;

  GExpertsDllHandle = LoadLibrary(LinkedDll);
  if (GExpertsDllHandle == 0)
  {
    Error( "Could not find required GExperts DLL: " \
           LinkedDll);
  }

  try
  {
    PlainProcedure ShowExpertManager;
    PlainProcedure CloseExpertManager;

    // Get entry point for ShowExpertManager
    ShowExpertManager = (PlainProcedure)GetProcAddress(GExpertsDllHandle, "ShowExpertManager");
    if (ShowExpertManager == NULL)
    {
      Error( "Could not find >ShowExpertManager< DLL export in " \
             LinkedDll);
    }

    // Get entry point for CloseExpertManager
    CloseExpertManager = (PlainProcedure)GetProcAddress(GExpertsDllHandle, "CloseExpertManager");
    if (ShowExpertManager == NULL)
    {
      Error( "Could not find >CloseExpertManager< DLL export in " \
             LinkedDll);
    }

    try
    {
      ShowExpertManager();
    }
    __finally
    {
      CloseExpertManager();
    }
  }
  __finally
  {
    FreeLibrary(GExpertsDllHandle);
  }

  return 0;
}
