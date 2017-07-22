//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("GxComponents5.res");
USEUNIT("FastSortTreeView.pas");
USEUNIT("SortGrid.pas");
USEUNIT("SpinIntEdit.pas");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldbx50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
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
