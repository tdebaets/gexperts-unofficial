//---------------------------------------------------------------------------

#include "GX_PrecompiledHeader.h"

#pragma hdrstop

#if defined(UseDevelopmentHeaders)
  #include "src\GX_GenFunc.hpp"
  #include "src\GX_GExperts.hpp"
#endif

//---------------------------------------------------------------------------
/*
GExperts License Agreement

GExperts is copyright 1996-2001 by GExperts, Inc, Erik Berry, and several
other authors who have submitted their code for inclusion. This license
agreement only covers code written by GExperts, Inc. and Erik Berry. You
should contact the other authors concerning their respective copyrights
and conditions.

The rules governing the use of GExperts and the GExperts source code are
derived from the official Open Source Definition, available at
http://www.opensource.org/. The conditions and limitations are as follows:

You may not use the GExperts source code to develop a proprietary
commercial or shareware product; this includes the creation of
proprietary plugins and libraries for commercial products.  You may use
the GExperts source code in a non-proprietary (Open Source) project,
under the terms listed below.

You may not use the GExperts source code to create and distribute custom
versions of GExperts under the "GExperts" name.  If you do modify and
distribute custom versions of GExperts, the binary distribution must be
named differently and clearly marked so users can tell they are not using
the official GExperts distribution.  A visible and unmodified version of
this license must appear in any modified distribution of GExperts.

Custom distributions of GExperts must include all of the custom changes
as a patch file that can be appplied to the original source code.  This
restriction is in place to protect the integrity of the original author's
source code.  No support for modified versions of GExperts will be
provided by the original authors or on the GExperts mailing lists.

All works derived from GExperts must be distributed under a license
compatible with this license and the official Open Source Definition,
which can be obtained from http://www.opensource.org/.

Please note that GExperts, Inc. and the other contributing authors hereby
state that this package is provided "as is" and without any express or
implied warranties, including, but not without limitation, the implied
warranties of merchantability and fitness for a particular purpose. In
other words, we accept no liability for any damage that may result from
using GExperts or programs that use the GExperts source code.

If you have license questions, please email Erik Berry at eberry@gexperts.org.
*/
//---------------------------------------------------------------------------
USEUNIT("src\GX_GExperts.pas");
USEUNIT("src\GX_Experts.pas");
USERES("GExpert4_bcb.res");
USEUNIT("src\GX_LibIntf.cpp");
USEFORMNS("src\GX_ProcedureList.pas", Gx_procedurelist, fmProcedureList);
USEUNIT("src\GX_GenFunc.pas");
USEFORMNS("src\GX_About.pas", Gx_about, fmAbout);
USEFORMNS("src\GX_ExpertManager.pas", Gx_expertmanager, fmExpertManager);
USEFORMNS("src\GX_GrepResults.pas", Gx_grepresults, fmGrepResults);
USEUNIT("src\GX_Search.pas");
USEFORMNS("src\GX_GrepSearch.pas", Gx_grepsearch, fmGrepsearch);
USEFORMNS("src\GX_GrepOptions.pas", Gx_grepoptions, fmGrepOptions);
USEFORMNS("src\GX_messagedialog.pas", Gx_messagedialog, fmMessageDialog);
USEUNIT("src\GX_EditWriter.pas");
USEUNIT("src\GX_Backup.pas");
USEFORMNS("src\GX_BackupOptions.pas", Gx_backupoptions, fmBackupOptions);
USEFORMNS("src\GX_BackupConfig.pas", Gx_backupconfig, fmBackupConfig);
USEFORMNS("src\GX_TabOrder.pas", Gx_taborder, fmTabOrder);
USEFORMNS("src\GX_CleanDirectories.pas", Gx_cleandirectories, fmCleanDIrectories);
USEUNIT("src\GX_MenuNotifier.pas");
USEUNIT("src\GX_EditReader.pas");
USEFORMNS("src\GX_ClipboardHistory.pas", Gx_clipboardhistory, fmClipboardHistory);
USEFORMNS("FavFiles\GX_FavFiles.pas", Gx_favfiles, fmFavFiles);
USEFORMNS("FavFiles\GX_FavOptions.pas", Gx_favoptions, fmFavOptions);
USEFORMNS("FavFiles\GX_FavFileProp.pas", Gx_favfileprop, fmFavFileProp);
USEUNIT("ClassBrowser\GX_ClassBrowser.pas");
USEUNIT("ClassBrowser\GX_ClassOptions.pas");
USEUNIT("ClassBrowser\GX_ClassReport.pas");
USEFORMNS("ClassBrowser\GX_ClassMethProp.pas", Gx_classmethprop, fmClassMethodProp);
USEFORMNS("ClassBrowser\GX_ClassProp.pas", Gx_classprop, fmClassProp);
USEFORMNS("ClassBrowser\GX_ClassIdentify.pas", Gx_classidentify, fmClassIdentify);
USEFORMNS("ClassBrowser\GX_ClassParsing.pas", Gx_classparsing, fmClassParsing);
USEUNIT("src\GX_SourceExport.pas");
USEFORMNS("src\GX_CodeLib.pas", Gx_codelib, fmCodeLib);
USEFORMNS("src\GX_CodeNew.pas", Gx_codenew, fmCodeDBNew);
USEFORMNS("src\GX_CodeSrch.pas", Gx_codesrch, fmCodeSearch);
USEFORMNS("src\GX_CodeOpt.pas", Gx_codeopt, fmCodeOptions);
USEUNIT("src\GX_PrinterFunc.pas");
USEUNIT("src\GX_PrintPreview.pas");
USEFORMNS("src\GX_Progress.pas", Gx_progress, fmProgress);
USEFORMNS("src\GX_AsciiChart.pas", Gx_asciichart, fmAsciiChart);
USEFORMNS("src\GX_PeInformation.pas", Gx_peinformation, fmPeInformation);
USEUNIT("src\GX_PeInfo.pas");
USEFORMNS("src\GX_CompReplace.pas", Gx_compreplace, fmCompReplace);
USEFORMNS("src\GX_ComponentGrid.pas", Gx_componentgrid, fmComponentGrid);
USEFORMNS("src\GX_IdeShortCuts.pas", Gx_ideshortcuts, fmIdeShortCuts);
USEUNIT("src\GX_Substitute.pas");
USEUNIT("src\GX_ProjDepend.pas");
USEFORMNS("src\GX_ProjDependProp.pas", Gx_projdependprop, fmProjDependProp);
USEFORMNS("src\GX_PerfectLayout.pas", Gx_perfectlayout, fmPerfectLayout);
USEFORMNS("src\GX_ClipboardOptions.pas", Gx_clipboardoptions, fmClipboardOptions);
USEFORMNS("src\GX_Todo.pas", Gx_utodo, fmTodo);
USEFORMNS("src\GX_ToDoOptions.pas", Gx_utodooptions, fmToDoOptions);
USEUNIT("EditorExperts\GX_EditorExpert.pas");
USEFORMNS("EditorExperts\GX_eComment.pas", Gx_ecomment, fmCommentConfig);
USEFORMNS("EditorExperts\GX_eDate.pas", Gx_edate, fmDateFormat);
USEUNIT("EditorExperts\GX_eFindDelimiter.pas");
USEFORMNS("EditorExperts\GX_eHeader.pas", Gx_eheader, fmHeaderFormat);
USEFORMNS("src\GX_EditorShortcut.pas", Gx_editorsshortcut, fmEditorShortcut);
USEUNIT("src\GX_ProofReaderExpert.pas");
USEFORMNS("src\GX_ProofReaderDM.pas", Gx_proofreaderdm, dmProofReader); /* TDataModule: DesignClass */
USEFORMNS("src\GX_ProofReaderConfig.pas", Gx_proofreaderconfig, fmProofreaderConfig);
USEUNIT("src\GX_IDEEnhance.pas");
USEUNIT("src\GX_Toolbar.pas");
USEUNIT("src\Gx_ToolbarButtons.pas");
USEFORMNS("IdeDock\WithDock\GX_FakeIdeDock.pas", Gx_fakeidedock, fmFakeIdeDockForm);
USEFORMNS("IdeDock\GX_ideDock.pas", Gx_idedock, fmIdeDockForm);
USEFORMNS("src\GX_PriorityBoost.pas", Gx_priorityboost, fmGxPriorityBoostConfig);
USEUNIT("src\Gx_ConfigurationInfo.pas");
USEFORMNS("src\GX_Configure.pas", Gx_configure, fmConfigure);
USEUNIT("src\Gx_MultiLinePalette.pas");
USEUNIT("src\Gx_EditorEnhancements.pas");
USEFORMNS("ProjOptionSets\GX_ProjOptionSets.pas", Gx_projoptionsets, fmProjOptionSets);
USEUNIT("ProjOptionSets\GX_ProjOptMap.pas");
USEFORMNS("ProjOptionSets\GX_ProjOptOptions.pas", Gx_projoptoptions, fmProjOptOptions);
USEUNIT("src\GX_VerDepConst.pas");
USEUNIT("src\GX_MultilineHost.pas");
USEUNIT("src\GX_ClassMgr.pas");
USEUNIT("src\GX_DbugIntf.pas");
USEFORMNS("FavFiles\GX_FavNewFolder.pas", Gx_favnewfolder, fmFavNewFolder);
USEUNIT("Comps\RplWizInfo.pas");
USEUNIT("Comps\E_StgStr.pas");
USEUNIT("src\GX_StandaloneExports.cpp");
USEFORMNS("src\GX_CompsToCode.pas", Gx_compstocode, fmCompsToCode);
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
 return 1;
}

//---------------------------------------------------------------------------

extern "C" __declspec(dllexport) bool __stdcall INITEXPERT0017(TIToolServices* ToolServices,
                                                TExpertRegisterProc RegisterProc,
                                                TExpertTerminateProc &Terminate)
{

  bool Result = (ToolServices != NULL);

  if (Result && !BuiltWithPackages())
  {
    Result = false;
    ShowNoPackagesError();
  }

  if (Result)
  {
    Exptintf::ToolServices = ToolServices;
    Application->Handle = ToolServices->GetParentHandle();
    // Terminate = Gx_gexperts::Cleanup;

    RegisterProc(new TGExperts);
  }

  return Result;
}

