unit GX_ToolBarButtons;

interface

{$R ToolBarButtons.res}

type
  TGXButtonClass = (gxbIDEMenuItem, gxbAction, gxbEditorExpert,
                    gxbDropDown, gxbExpert, gxbCustom);

  TGxButtonCategory = (bcFile, bcEdit, bcSearch, bcView, bcProject, bcRun,
                        bcDebug, bcGExperts);

  TGXButtonInfo = record
    ButtonName: string;
    ObjectName: string;
    //ActionName: string;
    ResourceName: string;
    Category: TGXButtonCategory;
    ButtonClass: TGXButtonClass;
  end;

 { TODO -oStefan -cIssue :
   We should reclassify the categories of some buttons;
   in particular "Run" should be dropped in favour of debugging etc }

 { TODO -oAnyone -cFeature :
   We need to fill in the ActionName for all of these
   so they can optionally be driven from actions in D4+ }

 { TODO -oAnyone -cFeature :
   We should match experts and editor experts to buttons
   by actual ClassName and not some magic number }

const
  SeparatorMenuItemMarker = 9999;

  GxButtonInfo: array[0..79] of TGxButtonInfo = (
  (
    ButtonName: 'Open file';
    ObjectName: 'FileOpenItem';
    ResourceName: 'GX_OPENFILE';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Save all';
    ObjectName: 'FileSaveAllItem';
    ResourceName: 'GX_SAVEALL';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Close all';
    //EB: This is disabled because it causes crashes
    ObjectName: 'FileCloseAllItemZZZ!!';
    ResourceName: 'GX_CLOSEALL';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'New form';
    ObjectName: 'FileNewFormItem';
    ResourceName: 'GX_NEWFORM';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'New Unit';
    ObjectName: 'FileNewUnitItem';
    ResourceName: 'GX_NEWUNIT';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Save file';
    ObjectName: 'FileSaveItem';
    ResourceName: 'GX_SAVEFILE';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Close File';
    ObjectName: 'FileCloseItem';
    ResourceName: 'GX_CLOSEFILE';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Print';
    ObjectName: 'FilePrintItem';
    ResourceName: 'GX_PRINT';
    Category: bcFile;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Cut';
    ObjectName: 'EditCutItem';
    ResourceName: 'GX_CUT';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Copy';
    ObjectName: 'EditCopyItem';
    ResourceName: 'GX_COPY';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  ( // 10
    ButtonName: 'Paste';
    ObjectName: 'EditPasteItem';
    ResourceName: 'GX_PASTE';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Undo';
    ObjectName: 'EditUndoItem';
    ResourceName: 'GX_UNDO';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Redo';
    ObjectName: 'EditRedoItem';
    ResourceName: 'GX_REDO';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Complete Class';
    ObjectName: 'ecCompleteClass';
    ResourceName: 'GX_COMPLETEC';
    Category: bcEdit;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Find';
    ObjectName: 'SearchFindItem';
    ResourceName: 'GX_FIND';
    Category: bcSearch;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Replace';
    ObjectName: 'SearchReplaceItem';
    ResourceName: 'GX_REPLACE';
    Category: bcSearch;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Find again';
    ObjectName: 'SearchAgainItem';
    ResourceName: 'GX_FINDAGAIN';
    Category: bcSearch;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Go to line number';
    ObjectName: 'SearchGoToItem';
    ResourceName: 'GX_FINDLINENO';
    Category: bcSearch;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Project manager';
    ObjectName: 'ViewPrjMgrItem';
    ResourceName: 'GX_PROJECTMANAGER';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Code Explorer';
    ObjectName: 'CodeExplorer';
    ResourceName: 'GX_CODEEXPLORER';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  ( // 20
    ButtonName: 'Call Stack';
    ObjectName: 'ViewCallStackItem';
    ResourceName: 'GX_VIEWCALLSTACK';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Watch window';
    {$IFNDEF GX_VER120_up}
    ObjectName: 'ViewWatchItem';
    {$ELSE}
    ObjectName: 'ViewWatchWindow';
    {$ENDIF GX_VER120_up}

    ResourceName: 'GX_VIEWWATCHES';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Units';
    ObjectName: 'ViewUnitItem';
    ResourceName: 'GX_VIEWUNITS';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Forms';
    ObjectName: 'ViewFormItem';
    ResourceName: 'GX_VIEWFORMS';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Toggle Form/Unit';
    ObjectName: 'ViewToggleFormItem';
    ResourceName: 'GX_VIEWFORM';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Swap Source Form';
    ObjectName: 'ViewSwapSourceFormItem';
    ResourceName: 'GX_SWAPSRCFORM';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Breakpoints';
    ObjectName: 'ViewBreakpointsItem';
    ResourceName: 'GX_VIEWBREAKS';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Threads';
    ObjectName: 'ViewThreadsItem';
    ResourceName: 'GX_VIEWTHREADS';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Modules';
    ObjectName: 'ViewModulesItem';
    ResourceName: 'GX_VIEWMODULES';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'CPU';
    ObjectName: 'ViewCPUItem';
    ResourceName: 'GX_VIEWCPU';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  ( // 30
    ButtonName: 'Event Log';
    ObjectName: 'ViewEventLogItem';
    ResourceName: 'GX_VIEWEVENTLOG';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'View Source';
    ObjectName: 'ViewPrjSourceItem';
    ResourceName: 'GX_VIEWSOURCE';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Add to project';
    ObjectName: 'ProjectAddItem';
    ResourceName: 'GX_ADDPROJECT';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Remove from project';
    ObjectName: 'ProjectRemoveItem';
    ResourceName: 'GX_REMOVEPROJECT';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Compile';
    ObjectName: 'ProjectCompileItem';
    ResourceName: 'GX_COMPILE';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Build All';
    ObjectName: 'ProjectBuildItem';
    ResourceName: 'GX_BUILDALL';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Syntax check';
    ObjectName: 'ProjectSyntaxItem';
    ResourceName: 'GX_SYNTAX';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Information';
    ObjectName: 'ProjectInformationItem';
    ResourceName: 'GX_INFORMATION';
    Category: bcProject;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Run';
    ObjectName: 'RunRunItem';
    ResourceName: 'GX_RUN';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Step over';
    ObjectName: 'RunStepOverItem';
    ResourceName: 'GX_STEPOVER';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  ( //40
    ButtonName: 'Trace into';
    ObjectName: 'RunTraceIntoItem';
    ResourceName: 'GX_TRACEINTO';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Trace to next source line';
    ObjectName: 'RunTraceToSourceItem';
    ResourceName: 'GX_TRACETONEXT';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Run to cursor';
    ObjectName: 'RunGotoCursorItem';
    ResourceName: 'GX_RUNTOCURSOR';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Show CSIP';
    ObjectName: 'RunShowCSIPItem';
    ResourceName: 'GX_RUNSHOWCSIP';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Add Source Breakpoint';
    ObjectName: 'RunAddSourceBreakpointItem';
    ResourceName: 'GX_RUNADDSBI';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Add Address Breakpoint';
    ObjectName: 'RunAddAddressBreakpointItem';
    ResourceName: 'GX_RUNADDABI';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Add Module Load Breakpoint';
    ObjectName: 'RunAddModuleLoadBreakpointItem';
    ResourceName: 'GX_RUNADDMLBI';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Add Data Breakpoint';
    ObjectName: 'RunAddDataBreakpointItem';
    ResourceName: 'GX_RUNADDDBI';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Inspect';
    ObjectName: 'RunInspectItem';
    ResourceName: 'GX_RUNINSPECT';
    Category: bcRun;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Toggle Breakpoint';
    ObjectName: 'ecToggleBreakpoint';
    ResourceName: 'GX_TOGGLEBREAKPOINT';
    Category: bcDebug;
    ButtonClass: gxbIDEMenuItem;
  ),

  ( // 50
    ButtonName: 'Component List';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_ADDTOINTERFACE';
    Category: bcGExperts;
    ButtonClass: gxbDropDown;
  ),

  (
    ButtonName: 'Procedure List';
    ObjectName: 'GX_ProcList';
    ResourceName: 'GX_PROCEDUREVIEWER';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Grep Search';
    ObjectName: 'GX_GrepDlg';
    ResourceName: 'GX_GREP';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'ASCII Chart';
    ObjectName: 'GX_ASCII';
    ResourceName: 'GX_ASCII';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Message Dialog';
    ObjectName: 'GX_MsgExp';
    ResourceName: 'GX_MESSAGEDLG';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Show Units';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_UNITLIST';
    Category: bcGExperts;
    ButtonClass: gxbDropDown;
  ),

  (
    ButtonName: 'Show Forms';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_FORMLIST';
    Category: bcGExperts;
    ButtonClass: gxbDropDown;
  ),

  (
    ButtonName: 'Comment Block';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_COMMENT';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  (
    ButtonName: 'Uncomment Block';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_UNCOMMENT';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  (
    ButtonName: 'Unit Header Comment';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_UNITHEADER';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  ( // 60
    ButtonName: 'Procedure Header Comment';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_PROCHEADER';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  (
    ButtonName: 'Locate Delimiter';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_LOCATEDELIM';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  (
    ButtonName: 'Move To Delimiter';
    ObjectName: 'SPECIAL';
    ResourceName: 'GX_MOVEDELIM';
    Category: bcGExperts;
    ButtonClass: gxbEditorExpert;
  ),

  (
    ButtonName: 'Local Variables';
    ObjectName: 'ViewLocalVariablesItem';
    ResourceName: 'GX_VIEWLOCVARS';
    Category: bcView;
    ButtonClass: gxbIDEMenuItem;
  ),

  (
    ButtonName: 'Expert Manager';
    ObjectName: 'GX_ExpMgr';
    ResourceName: 'GX_EXPMGR';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Grep Results';
    ObjectName: 'GX_GrepResults';
    ResourceName: 'GX_GREP';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Clean Directories';
    ObjectName: 'GX_Clean';
    ResourceName: 'GX_CLEAN';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Clipboard History';
    ObjectName: 'GX_Clip';
    ResourceName: 'GX_CLIP';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Favorite Files';
    ObjectName: 'GX_Filemgr';
    ResourceName: 'GX_FILEMGR';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Class Browser';
    ObjectName: 'GX_Class';
    ResourceName: 'GX_CLASS';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  ( // 70
    ButtonName: 'Code Librarian';
    ObjectName: 'GX_CodeLib';
    ResourceName: 'GX_CODELIB';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'PE Information';
    ObjectName: 'GX_PE';
    ResourceName: 'GX_PE';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Replace Components';
    ObjectName: 'GX_CompReplace';
    ResourceName: 'GX_COMPREPLACE';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'IDE Menu Shortcuts';
    ObjectName: 'GX_Shortcut';
    ResourceName: 'GX_SHORTCUT';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Project Dependencies';
    ObjectName: 'GX_Depend';
    ResourceName: 'GX_DEPEND';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Perfect Layout';
    ObjectName: 'GX_Layout';
    ResourceName: 'GX_LAYOUT';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'To Do List';
    ObjectName: 'GX_ToDo';
    ResourceName: 'GX_TODOMENU';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Code Proofreader';
    ObjectName: 'GX_APR';
    ResourceName: 'GX_APR';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  (
    ButtonName: 'Backup Project';
    ObjectName: 'GX_Backup';
    ResourceName: 'GX_BACKUP';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ),

  ( // 79
    ButtonName: 'Source To HTML';
    ObjectName: 'GX_HTML';
    ResourceName: 'GX_HTML';
    Category: bcGExperts;
    ButtonClass: gxbExpert;
  ));

  // EB: Add Project Option Sets button?

const
  GxButtonCategoryText: array[Low(TGxButtonCategory)..High(TGxButtonCategory)] of string = (
    'File',
    'Edit',
    'Search',
    'View',
    'Project',
    'Run',
    'Debug',
    'GExperts'
  );

implementation

end.
