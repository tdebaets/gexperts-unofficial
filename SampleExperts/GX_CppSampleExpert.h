//---------------------------------------------------------------------------

#ifndef GX_CppSampleExpertH
#define GX_CppSampleExpertH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>

#include "GX_GExperts.Hpp"

//---------------------------------------------------------------------------
class TfmGxCppSampleExpertForm : public TForm
{
__published:	// IDE-managed Components
  TLabel *lblNote;
  TLabel *lblData;
  TButton *btnOk;
  TEdit *edtData;
  TButton *btnCancel;
private:	// User declarations
public:		// User declarations
  __fastcall TfmGxCppSampleExpertForm(TComponent* Owner);
};

  // Descend all new experts from TGX_EnhExpert
class TGxCppSampleExpert : public TGX_EnhExpert
{
  private:
    typedef TGX_EnhExpert inherited;
  private:
    String FSomeData;
  protected:
    virtual void __fastcall SetActive(bool New);
  public:
    virtual __fastcall TGxCppSampleExpert(void);
    virtual __fastcall ~TGxCppSampleExpert(void);

    virtual String __fastcall GetDisplayName(void);
    virtual String __fastcall GetMenuCaption(void);
    virtual String __fastcall GetMenuName(void);
    virtual String __fastcall GetMenuMask(void);
    virtual String __fastcall GetName(void);
    virtual String __fastcall IconFileName(void);

    virtual void __fastcall Configure(void);
    virtual void __fastcall LoadSettings(void);
    virtual void __fastcall SaveSettings(void);
    virtual void __fastcall Click(TObject* Sender);
};

#endif
