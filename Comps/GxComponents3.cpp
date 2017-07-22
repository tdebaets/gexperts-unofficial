//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("GxComponents3.res");
USEPACKAGE("vclx35.bpi");
USEPACKAGE("VCL35.bpi");
USEUNIT("SortGrid.pas");
USEUNIT("SpinIntEdit.pas");
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
