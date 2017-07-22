unit GX_VerDepConst;

interface

// *****************************************************
//
//    Name of IDE package for some reasonably "dirty" hacks
//    (Delphi 4+)
//
// *****************************************************
  {$UNDEF CorIdeDefined}
const
  {$IFDEF VER100}
    dummyCorIdeLibName = '---void---';
    {$DEFINE CorIdeDefined}
  {$ENDIF VER100}

  {$IFDEF VER110}
    dummyCorIdeLibName = '---void---';
    {$DEFINE CorIdeDefined}
  {$ENDIF VER110}

  {$IFDEF VER120}
    CorIdeLibName = 'coride40.bpl';
    DphIdeLibName = 'dphide40.bpl';
    dccLibName = 'dcc40.dll';
    {$DEFINE CorIdeDefined}
  {$ENDIF VER120}

  {$IFDEF VER125}
    CorIdeLibName = 'coride40.bpl';
    DphIdeLibName = 'bcbide40.bpl';
    dccLibName = 'dcc40.dll';
    {$DEFINE CorIdeDefined}
  {$ENDIF VER125}

  {$IFDEF VER130}
    CorIdeLibName = 'coride50.bpl';
    {$IFDEF BCB}
    DphIdeLibName = 'bcbide50.bpl';
    {$ELSE not BCB}
    DphIdeLibName = 'dphide50.bpl';
    {$ENDIF BCB}
    dccLibName = 'dcc50.dll';
    {$DEFINE CorIdeDefined}
  {$ENDIF VER130}


  {$IFNDEF CorIdeDefined}
    CorIdeLibName not defined
  {$ENDIF CorIdeDefined}
  {$UNDEF CorIdeDefined}



// *****************************************************
//
//    Name of core IDE package for docking support
//    (Delphi 4+)
//
// *****************************************************
  {$UNDEF TTabDockHostFormClassContainerDefined}
const
  {$IFDEF VER100}
    dummyTTabDockHostFormClassContainer = '---void---';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER100}

  {$IFDEF VER110}
    dummyTTabDockHostFormClassContainer = '---void---';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER110}

  {$IFDEF VER120}
    TTabDockHostFormClassContainer = 'coride40.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER120}

  {$IFDEF VER125}
    TTabDockHostFormClassContainer = 'coride40.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER125}

  {$IFDEF VER130}
    TTabDockHostFormClassContainer = 'dsnide50.bpl';
    {$DEFINE TTabDockHostFormClassContainerDefined}
  {$ENDIF VER130}



  {$IFNDEF TTabDockHostFormClassContainerDefined}
    TTabDockHostFormClassContainer not defined
  {$ENDIF TTabDockHostFormClassContainerDefined}
  {$UNDEF TTabDockHostFormClassContainerDefined}


// *****************************************************
//
//    Major version number of IDE, e.g.
//      Delphi 4.0 -> '4'
//    (Delphi 3+)
//
// *****************************************************
  {$UNDEF MajorVersionNumber}
const
  {$IFDEF VER100}
    MajorVersionNumberChar = '3';  // Delphi 3.0
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER100}

  {$IFDEF VER110}
    MajorVersionNumberChar = '3';  // C++Builder 3.0
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER110}

  {$IFDEF VER120}
    MajorVersionNumberChar = '4';  // Delphi 4.0
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER120}

  {$IFDEF VER125}
    MajorVersionNumberChar = '4';  // C++Builder 4.0
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER125}

  {$IFDEF VER130}
    MajorVersionNumberChar = '5';  // Delphi 5.0, C++Builder 5.0
    {$DEFINE MajorVersionNumber}
  {$ENDIF VER130}


  {$IFNDEF MajorVersionNumber}
    MajorVersionNumberChar has not been defined
  {$ENDIF MajorVersionNumber}
  {$UNDEF MajorVersionNumber}

// *****************************************************
//
//    Base registry key for each compiler, in case
//    it is not available due to ToolServices = nil.
//    Do not localize these strings.
//    (Delphi 3+)
//
// *****************************************************
  {$UNDEF IdeBaseKey}
const
  {$IFDEF VER100}
    CompilerDefinedProductRegistryKey = 'Software\Borland\Delphi\3.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER100}

  {$IFDEF VER110}
    CompilerDefinedProductRegistryKey = 'Software\Borland\C++Builder\3.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER110}

  {$IFDEF VER120}
    CompilerDefinedProductRegistryKey = 'Software\Borland\Delphi\4.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER120}

  {$IFDEF VER125}
    CompilerDefinedProductRegistryKey = 'Software\Borland\C++Builder\4.0';
    {$DEFINE IdeBaseKey}
  {$ENDIF VER125}

  {$IFDEF VER130}
    {$IFNDEF BCB}
    CompilerDefinedProductRegistryKey = 'Software\Borland\Delphi\5.0';
    {$ELSE BCB}
    CompilerDefinedProductRegistryKey = 'Software\Borland\C++Builder\5.0';
    {$ENDIF BCB}
    {$DEFINE IdeBaseKey}
  {$ENDIF VER130}



  {$IFNDEF IdeBaseKey}
    CompilerDefinedProductRegistryKey not defined
  {$ENDIF IdeBaseKey}
  {$UNDEF IdeBaseKey}


// *****************************************************
//
//    Storage file for Project Options Sets (Delphi 4+)
//
// *****************************************************
  {$UNDEF OptionsFile}
const
  {$IFDEF VER100}
    dummyProjectOptionsSetsFileName = '---void---';
    {$DEFINE OptionsFile}
  {$ENDIF VER100}

  {$IFDEF VER110}
    dummyProjectOptionsSetsFileName = '---void---';
    {$DEFINE OptionsFile}
  {$ENDIF VER110}

  {$IFDEF VER120}
    ProjectOptionsSetsFileName = 'GXPoSets40.gxs';
    {$DEFINE OptionsFile}
  {$ENDIF VER120}

  {$IFDEF VER125}
    ProjectOptionsSetsFileName = 'GXPoSets40_BCB.gxs';
    {$DEFINE OptionsFile}
  {$ENDIF VER120}

  {$IFDEF VER130}
    {$IFNDEF BCB}
    ProjectOptionsSetsFileName = 'GXPoSets50.gxs';
    {$ELSE BCB}
    ProjectOptionsSetsFileName = 'GXPoSets50_BCB.gxs';
    {$ENDIF BCB}
    {$DEFINE OptionsFile}
  {$ENDIF VER130}



  {$IFNDEF OptionsFile}
    Project options set file not defined
  {$ENDIF OptionsFile}
  {$UNDEF OptionsFile}


// *****************************************************
//
//    Storage folders for the class browser (Delphi 3+)
//
// *****************************************************

  {$UNDEF ClassBrowserStorageDefined}
const
  {$IFDEF VER100}
    ClassBrowserStorageFolder = 'Storage3';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER100}

  {$IFDEF VER110}
    ClassBrowserStorageFolder = 'Storage3.BCB';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER110}

  {$IFDEF VER120}
    ClassBrowserStorageFolder = 'Storage4';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER120}

  {$IFDEF VER125}
    ClassBrowserStorageFolder = 'Storage4.BCB';
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER125}

  {$IFDEF VER130}
    {$IFNDEF BCB}
    ClassBrowserStorageFolder = 'Storage5';
    {$ELSE BCB}
    ClassBrowserStorageFolder = 'Storage5.BCB';
    {$ENDIF BCB}
    {$DEFINE ClassBrowserStorageDefined}
  {$ENDIF VER130}



  {$IFNDEF ClassBrowserStorageDefined}
    Storage folder for class browser not defined
  {$ENDIF ClassBrowserStorageDefined}
  {$UNDEF ClassBrowserStorageDefined}


// *****************************************************
//
//    Generic docking support (Delphi 4+)
//
// *****************************************************

//  LoadDeskFileName = '@Desktop@LoadDeskFile$qqrx17System@AnsiString4bool';
//  Desktop.LoadDeskFile(const AnsiString, Boolean)

  {$UNDEF RegisterDesktopFormClassName}
const
  {$IFDEF VER100}
    dummyRegisterDesktopFormClassName = '---void---';
    {$DEFINE RegisterDesktopFormClassName}
  {$ENDIF VER100}

  {$IFDEF VER110}
    dummyRegisterDesktopFormClassName = '---void---';
    {$DEFINE RegisterDesktopFormClassName}
  {$ENDIF VER110}

  {$IFDEF VER120}
    RegisterDesktopFormClassName = '@Desktop@RegisterDesktopFormClass$qqrp17System@TMetaClassx17System@AnsiStringxt2';
    // Desktop.RegisterDesktopFormClass(TMetaClass, const AnsiString, const AnsiString)
    {$DEFINE RegisterDesktopFormClassName}
  {$ENDIF VER120}

  {$IFDEF VER125}
    RegisterDesktopFormClassName = '@Desktop@RegisterDesktopFormClass$qqrp17System@TMetaClassx17System@AnsiStringxt2';
    // Desktop.RegisterDesktopFormClass(TMetaClass, const AnsiString, const AnsiString)
    {$DEFINE RegisterDesktopFormClassName}
  {$ENDIF VER125}

  {$IFDEF VER130}
    dummyRegisterDesktopFormClassName = '---void---';
    {$DEFINE RegisterDesktopFormClassName}
  {$ENDIF VER130}


  {$IFNDEF RegisterDesktopFormClassName}
    RegisterDesktopFormClassName is not defined
  {$ENDIF RegisterDesktopFormClassName}
  {$UNDEF RegisterDesktopFormClassName}


// *****************************************************
//
//    Generic docking support - VMT Offsets (Delphi 4+)
//
// *****************************************************
  {$UNDEF DockVmtOfsConstant}
const
  {$IFDEF VER100}
    dummyDockVmtOfs = 0;
    {$DEFINE DockVmtOfsConstant}
  {$ENDIF VER100}

  {$IFDEF VER110}
    dummyDockVmtOfs = 0;
    {$DEFINE DockVmtOfsConstant}
  {$ENDIF VER110}

  {$IFDEF VER120}
    DockVmtOfs = 0;
    {$DEFINE DockVmtOfsConstant}
  {$ENDIF VER120}

  {$IFDEF VER125}
    DockVmtOfs = 0;
    {$DEFINE DockVmtOfsConstant}
  {$ENDIF VER125}

  {$IFDEF VER130}
    dummyDockVmtOfs = 0;
    {$DEFINE DockVmtOfsConstant}
  {$ENDIF VER130}



  {$IFNDEF DockVmtOfsConstant}
    DockVmtOfs constant not defined
  {$ENDIF DockVmtOfsConstant}
  {$UNDEF DockVmtOfsConstant}

// *****************************************************
//
//    Packages required to compile GExperts
//
// *****************************************************

 {$UNDEF PackageListDefined}
 {$IFDEF VER130}
   GxRequiredPackageList = 'VCL50, VCLX50, VCLDB50, DSNIDE50, and VCLBDE50';
   {$DEFINE PackageListDefined}
 {$ENDIF VER130}
 {$IFDEF VER125}
   GxRequiredPackageList = 'VCL40, VCLX40, and VCLDB40';
   {$DEFINE PackageListDefined}
 {$ENDIF VER125}
 {$IFDEF VER120}
   GxRequiredPackageList = 'VCL40, VCLX40, and VCLDB40';
   {$DEFINE PackageListDefined}
 {$ENDIF VER120}
 {$IFDEF VER110}
   GxRequiredPackageList = 'VCL35, VCLX35, and VCLDB35';
   {$DEFINE PackageListDefined}
 {$ENDIF VER110}
 {$IFDEF VER100}
   GxRequiredPackageList = 'VCL30, VCLX30, and VCLDB30';
   {$DEFINE PackageListDefined}
 {$ENDIF VER100}
 {$IFNDEF PackageListDefined}
   Package list not specified
 {$ENDIF PackageListDefined}
 {$UNDEF PackageListDefined}


{$I GX_CondDefine.inc}

  {$IFDEF GX_VER120_up}
  // Defines the IDEs tabbed docking host class name
  TTabDockHostFormName = 'TTabDockHostForm';
  TEditorDockPanelName = 'TEditorDockPanel';
  {$ENDIF GX_VER120_up}

implementation

end.
