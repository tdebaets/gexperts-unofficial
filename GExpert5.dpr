library GExpert5;
{
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
}

uses
  Forms,
  ToolIntf,
  ExptIntf,
  GX_Experts in 'Src\GX_Experts.pas',
  GX_GExperts in 'Src\GX_GExperts.pas',
  GX_ProcedureList in 'Src\GX_ProcedureList.pas' {fmProcedureList},
  GX_GenFunc in 'Src\GX_GenFunc.pas',
  GX_About in 'Src\GX_About.pas' {fmAbout},
  GX_ExpertManager in 'Src\GX_ExpertManager.pas' {fmExpertManager},
  GX_GrepResults in 'Src\GX_GrepResults.pas' {fmGrepResults},
  GX_Search in 'Src\GX_Search.pas',
  GX_GrepSearch in 'Src\GX_GrepSearch.pas' {fmGrepSearch},
  GX_GrepOptions in 'Src\GX_GrepOptions.pas' {fmGrepOptions},
  GX_MessageDialog in 'Src\GX_MessageDialog.pas' {fmMessageDialog},
  GX_MessageOptions in 'Src\GX_MessageOptions.pas' {fmMessageOptions},
  GX_EditReader in 'Src\GX_EditReader.pas',
  GX_EditWriter in 'Src\GX_EditWriter.pas',
  GX_Backup in 'Src\GX_Backup.pas' {fmBackup},
  GX_BackupOptions in 'Src\GX_BackupOptions.pas' {fmBackupOptions},
  GX_BackupConfig in 'Src\GX_BackupConfig.pas' {fmBackupConfig},
  GX_TabOrder in 'Src\GX_TabOrder.pas' {fmTabOrder},
  GX_CleanDirectories in 'Src\GX_CleanDirectories.pas' {fmCleanDirectories},
  GX_MenuNotifier in 'Src\GX_MenuNotifier.pas',
  GX_ClipboardHistory in 'Src\GX_ClipboardHistory.pas' {fmClipboardHistory},
  GX_ClipboardOptions in 'Src\GX_ClipboardOptions.pas' {fmClipboardOptions},
  GX_FavFiles in 'FavFiles\GX_FavFiles.pas' {fmFavFiles},
  GX_FavOptions in 'FavFiles\GX_FavOptions.pas' {fmFavOptions},
  GX_FavUtil in 'FavFiles\GX_FavUtil.pas',
  GX_FavNewFolder in 'FavFiles\GX_FavNewFolder.pas' {fmFavNewFolder},
  GX_FavFileProp in 'FavFiles\GX_FavFileProp.pas' {fmFavFileProp},
  GX_ClassBrowser in 'ClassBrowser\GX_ClassBrowser.pas' {fmClassBrowser},
  GX_ClassOptions in 'ClassBrowser\GX_ClassOptions.pas' {fmClassOptions},
  GX_ClassReport in 'ClassBrowser\GX_ClassReport.pas' {fmClassReport},
  GX_ClassMethProp in 'ClassBrowser\GX_ClassMethProp.pas' {fmClassMethodProp},
  GX_ClassProp in 'ClassBrowser\GX_ClassProp.pas' {fmClassProp},
  GX_ClassIdentify in 'ClassBrowser\GX_ClassIdentify.pas' {fmClassIdentify},
  GX_ClassParsing in 'ClassBrowser\GX_ClassParsing.pas' {fmClassParsing},
  GX_SourceExport in 'Src\GX_SourceExport.pas' {fmSourceExport},
  GX_CodeLib in 'Src\GX_CodeLib.pas' {fmCodeLib},
  GX_CodeNew in 'Src\GX_CodeNew.pas' {fmCodeDBNew},
  GX_CodeSrch in 'Src\GX_CodeSrch.pas' {fmCodeSearch},
  GX_CodeOpt in 'Src\GX_CodeOpt.pas' {fmCodeOptions},
  GX_PrinterFunc in 'Src\GX_PrinterFunc.pas',
  GX_PrintPreview in 'Src\GX_PrintPreview.pas',
  GX_Progress in 'Src\GX_Progress.pas' {fmProgress},
  GX_AsciiChart in 'Src\GX_AsciiChart.pas' {fmAsciiChart},
  GX_PeInformation in 'Src\GX_PeInformation.pas' {fmPeInformation},
  GX_PeInfo in 'Src\GX_PeInfo.pas',
  GX_CompReplace in 'Src\GX_CompReplace.pas' {fmCompReplace},
  GX_ComponentGrid in 'Src\GX_ComponentGrid.pas' {fmComponentGrid},
  GX_IdeShortCuts in 'Src\GX_IdeShortCuts.pas' {fmIdeShortCuts},
  GX_Substitute in 'Src\GX_Substitute.pas',
  GX_ProjDepend in 'Src\GX_ProjDepend.pas' {fmProjDepend},
  GX_ProjDependProp in 'Src\GX_ProjDependProp.pas' {fmProjDependProp},
  GX_PerfectLayout in 'Src\GX_PerfectLayout.pas' {fmPerfectLayout},
  GX_ToDo in 'Src\GX_ToDo.pas' {fmToDo},
  GX_ToDoOptions in 'Src\GX_ToDoOptions.pas' {fmToDoOptions},
  GX_EditorExpert in 'EditorExperts\GX_EditorExpert.pas',
  GX_eComment in 'EditorExperts\GX_eComment.pas' {fmCommentConfig},
  GX_eDate in 'EditorExperts\GX_eDate.pas' {fmDateFormat},
  GX_eFindDelimiter in 'EditorExperts\GX_eFindDelimiter.pas',
  GX_eHeader in 'EditorExperts\GX_eHeader.pas' {fmHeaderFormat},
  GX_EditorShortcut in 'Src\GX_EditorShortcut.pas' {fmEditorShortcut},
  GX_ProofReaderExpert in 'Src\GX_ProofReaderExpert.pas',
  GX_ProofReaderDM in 'Src\GX_ProofReaderDM.pas' {dmProofReader: TDataModule},
  GX_ProofReaderConfig in 'Src\GX_ProofReaderConfig.pas' {fmProofReaderConfig},
  GX_IDEEnhance in 'Src\GX_IDEEnhance.pas',
  GX_HookDebugString in 'Src\GX_HookDebugString.pas',
  GX_Toolbar in 'Src\GX_Toolbar.pas',
  GX_ToolbarButtons in 'Src\GX_ToolbarButtons.pas',
  GX_DummyIdeDock in 'IdeDock\Native\GX_DummyIdeDock.pas' {fmDummyIdeDockForm},
  GX_IdeDock in 'IdeDock\Native\GX_IdeDock.pas' {fmIdeDockForm},
  GX_PriorityBoost in 'Src\GX_PriorityBoost.pas' {fmPriorityBoostConfig},
  GX_ConfigurationInfo in 'Src\GX_ConfigurationInfo.pas',
  GX_Configure in 'Src\GX_Configure.pas' {fmConfiguration},
  GX_MultiLinePalette in 'Src\GX_MultiLinePalette.pas',
  GX_EditorEnhancements in 'Src\GX_EditorEnhancements.pas',
  GX_ProjOptionSets in 'ProjOptionSets\GX_ProjOptionSets.pas' {fmProjOptionSets},
  GX_ProjOptMap in 'ProjOptionSets\GX_ProjOptMap.pas',
  GX_ProjOptOptions in 'ProjOptionSets\GX_ProjOptOptions.pas' {fmProjOptOptions},
  GX_MultilineHost in 'Src\GX_MultilineHost.pas',
  GX_VerDepConst in 'Src\GX_VerDepConst.pas',
  GX_BaseModuleNotifiers in 'Src\GX_BaseModuleNotifiers.pas',
  GX_CompsToCode in 'Src\GX_CompsToCode.pas' {fmCompsToCode},
  GX_MessageBox in 'Src\GX_MessageBox.pas' {fmGxMessageBox};

{$E dll}

{$R *.RES}

{ Register the expert }
function InitExpert(ToolServices: TIToolServices;  RegisterProc: TExpertRegisterProc;
  var Terminate: TExpertTerminateProc): Boolean; export; stdcall;
begin
  Result := (ToolServices <> nil);

  if Result and (not BuiltWithPackages) then
  begin
    Result := False;
    ShowNoPackagesError;
  end;

  if Result then
  begin
    ExptIntf.ToolServices := ToolServices;
    Application.Handle := ToolServices.GetParentHandle;
    //Terminate := GX_GExperts.Cleanup;
    RegisterProc(TGExperts.Create);
  end;
end;

exports
  InitExpert name ExpertEntryPoint resident,
  ShowExpertManager,
  CloseExpertManager,
  ShowGrep;

begin
end.

