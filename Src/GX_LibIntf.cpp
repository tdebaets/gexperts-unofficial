//---------------------------------------------------------------------------

#include "GX_PrecompiledHeader.h"

#pragma hdrstop

#if defined(UseDevelopmentHeaders)
  #include "src\GX_GenFunc.hpp"
#endif

//---------------------------------------------------------------------------
#pragma package(smart_init)

__declspec(dllexport)
void __fastcall BCB_SwapSourceFormViewImplementation(void)
{
#if (__BCPLUSPLUS__ >= 0x0540) && defined(UsePackages)
  TIForm* CurrentForm;

  CurrentForm = CompLib->GetActiveForm();

  if (CurrentForm != NULL)
  {
    _di_IDesignerModule CurrentModule;

    CurrentModule = CurrentForm->GetModule();
    if (CurrentModule != NULL)
      CurrentModule->SwapSourceFormView();
  }
#endif
};

//---------------------------------------------------------------------------

__declspec(dllexport)
bool __fastcall BCB_HasFormImplementation(void)
{
#if defined(UsePackages)
  return (CompLib->GetActiveForm() != NULL);
#else
  return false;
#endif
}


//////////////////////////////////////////////

void __fastcall BCB_ShowForm(TIModuleInterface* ModIntf, TIComponentInterface* FormCompIntf)
{
  if (ModIntf != NULL)
  {
    ModIntf->ShowForm();
    if (FormCompIntf != NULL)
    {
      void* ComponentHandle = FormCompIntf->GetComponentHandle();
      TForm* Form = reinterpret_cast<TForm*>(ComponentHandle);
      Form->SendToBack();
    }
  }
}

__declspec(dllexport)
void __fastcall SelectComponentImplementation(AnsiString Name)
{
  TIComponentInterface* CompIntf;
  TIComponentInterface* FormCompIntf = NULL;
  TIModuleInterface* ModIntf = NULL;
  TIFormInterface* FormIntf = NULL;

  TIForm* Form;
  TControl* Control;

  AnsiString UnitName, FileName, FileExt;
  int i;

  FileName = UpperCase(ToolServices->GetCurrentFile());
  if (IsDfm(FileName))
  {
    // Get module interface
    FileExt = ExtractFileExt(FileName);
    i = FileName.AnsiPos(FileExt);
    if (i > 0)
      UnitName = FileName.SubString(1, i) + "PAS";
    else
      UnitName = FileName;
  }
  else
    UnitName = FileName;

  ModIntf = ToolServices->GetModuleInterface(UnitName);
  if (ModIntf == NULL)
    return;

  try
  {
    FormIntf = ModIntf->GetFormInterface();
    if (FormIntf == NULL)
      return;

    FormCompIntf = FormIntf->GetFormComponent();
    if (FormCompIntf != NULL)
    {
      for (i = 0; i < FormCompIntf->GetComponentCount(); ++i)
      {
        CompIntf = FormCompIntf->GetComponent(i);
        try
        {
          if (CompIntf != NULL)
          {
            Control = reinterpret_cast<TControl*>(CompIntf->GetComponentHandle());
            if (CompareText(Control->Name, Name) == 0)
            {
              Form = CompLib->GetActiveForm();
              if (Form == NULL)
                BCB_ShowForm(ModIntf, FormCompIntf);
              else
              {
                if (CompareText( Form->GetFormName(),
                                 (reinterpret_cast<TForm*>(FormCompIntf->GetComponentHandle()))->Name) != 0)
                  BCB_ShowForm(ModIntf, FormCompIntf);
              }

              CompIntf->Select();

              return;
            }
          }
        }
        __finally
        {
          CompIntf->Free();
        }
      }
    }
  }
  __finally
  {
    if ((FormCompIntf != NULL) && (FormCompIntf->GetComponentHandle() != NULL))
      FormCompIntf->Free();

    ModIntf->Free();
    FormIntf->Free();
  }
}

//---------------------------------------------------------------------------

