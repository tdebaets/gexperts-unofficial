//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("GxComponents4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("SpinIntEdit.pas");
USEUNIT("SortGrid.pas");
USEUNIT("FastSortTreeView.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
