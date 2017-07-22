
// Standalone Expert Manager exports

namespace Gx_expertmanager
{
  extern
    void __fastcall ShowExpertManager(void);
  extern
    void __fastcall CloseExpertManager(void);
}

extern "C" __declspec(dllexport)
  void __stdcall ShowExpertManager(void)
{
  Gx_expertmanager::ShowExpertManager();
}

extern "C" __declspec(dllexport)
  void __stdcall CloseExpertManager(void)
{
  Gx_expertmanager::CloseExpertManager();
}

// Standalone GREP export

namespace Gx_grepresults
{
  extern
    void __fastcall ShowGrep(void);
}

extern "C" __declspec(dllexport)
  void __stdcall ShowGrep(void)
{
  Gx_grepresults::ShowGrep();
}

