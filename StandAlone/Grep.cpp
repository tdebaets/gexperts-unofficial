
#include <windows.h>
#pragma hdrstop
#include <condefs.h>

USERES("Grep.res");
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
    PlainProcedure ShowGrep;

    // Get entry point for ShowGrep
    ShowGrep = (PlainProcedure)GetProcAddress(GExpertsDllHandle, "ShowGrep");
    if (ShowGrep == NULL)
    {
      Error( "Could not find >ShowGrep< DLL export in " \
             LinkedDll);
    }

    // Show GREP dialog
    ShowGrep();

  }
  __finally
  {
    FreeLibrary(GExpertsDllHandle);
  }

  return 0;
}
