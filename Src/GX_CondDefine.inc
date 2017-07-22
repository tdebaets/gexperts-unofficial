{ GX_CondDefine.inc }

// Third-Party Product Defines
// ------------------------------------------------------------------------
// Define VCLZIP below if you own a copy of Kevin Boylan's VCLZip
// If not, the Project Backup expert will not be compiled into GExperts
// Download a trial version from: http://ourworld.compuserve.com/homepages/boylank/

// Define MWEDIT below if you have a copy of the mwEdit Open Source
// text editor.  Source Export requires this component.
// mwEdit will be replaced by SynEdit soon after GExperts version 1.0
// Download the source to mwEdit at: http://synedit.sourceforge.net/

// Define SYNTAXMEMO below if you own a copy of David Brock's TSyntaxMemo
// Download a trial version from: http://www.dbrocksoftware.com/
// NOTE: mwEdit is now the preferred choice for compiling into GExperts.

// Define ACEREPORTER below if you own a copy of SCT Associates
// ACE Reporter.  With this, you will get print previewing from
// the Grep and PE Information experts.
// Download a trial version from: http://www.sct-associates.com/

// Define DELPHIDEBUGGINGPACKAGE below if you own a copy of the
// Econos - Stefan Hoffmeister Delphi Debugging Package.
// This adds stack traces and source code mapping for debugging
// Download a trial version from: http://www.econos.de/software/borland/

// Switch off all third-party product defines
{$UNDEF SYNTAXMEMO}{$UNDEF ACEREPORTER}{$UNDEF VCLZIP}{$UNDEF GX_EII}{$UNDEF DELPHIDEBUGGINGPACKAGE}{$UNDEF MWEDIT}

// Then turn things on selectively.
{.$DEFINE MWEDIT}
{.$DEFINE SYNTAXMEMO}
{.$DEFINE ACEREPORTER}
{.$DEFINE VCLZIP}
{.$DEFINE DELPHIDEBUGGINGPACKAGE}

// We want to include support for EII: editor experts, toolbar, etc.
{$DEFINE GX_EII}

// We want to build a GExperts DLL (strongly recommended)
{$DEFINE DLL}

// We want IDE docking for those IDEs that we support
// For Delphi 5 and greater compile with the DsnIdeXX runtime package
{$DEFINE EnableIdeDockingSupport}

// Don't include the experts which use the BDE (Code Proofreader, Code Librarian)
{.$DEFINE GX_NOBDE}

// We may want build GExperts so that it does NOT require
// packages - only used currently for debugging C++Builder integration
{.$DEFINE GX_NotPackageBuilt}

// Misc GExperts Defines (do not change the items below!)
// ---------------------------------------------------------------

// GExperts does not support Kylix yet, so this is only
// here for someone in the future
{$IFDEF VER140}
  {$DEFINE GX_VER140_up}
  {$DEFINE GX_VER130_up}
  {$DEFINE GX_VER125_up}
  {$DEFINE GX_VER120_up}
  {$DEFINE GX_VER110_up}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}
  {$DEFINE GX_VCL35_up}
  {$DEFINE GX_VCL40_up}
  {$DEFINE GX_VCL50_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}
  {$DEFINE GX_DefaultParam}
  {$DEFINE GX_Overload}
  {$DEFINE GX_CppHeaderDefines}

  {$DEFINE GX_Delphi}
{$ENDIF VER140}


{ Borland Delphi 5.0 and Borland C++Builder 5.0 }
{$IFDEF VER130}
  {$DEFINE GX_VER130_up}
  {$DEFINE GX_VER125_up}
  {$DEFINE GX_VER120_up}
  {$DEFINE GX_VER110_up}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}
  {$DEFINE GX_VCL35_up}
  {$DEFINE GX_VCL40_up}
  {$DEFINE GX_VCL50_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}
  {$DEFINE GX_DefaultParam}
  {$DEFINE GX_Overload}
  {$DEFINE GX_CppHeaderDefines}

  {$IFDEF BCB}
    {$DEFINE GX_BCB}
  {$ELSE}
    {$DEFINE GX_Delphi}
  {$ENDIF BCB}
{$ENDIF VER130}

{ Borland C++Builder 4.0 }
{$IFDEF VER125}
  {$DEFINE GX_VER125_up}
  {$DEFINE GX_VER120_up}
  {$DEFINE GX_VER110_up}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}
  {$DEFINE GX_VCL35_up}
  {$DEFINE GX_VCL40_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}
  {$DEFINE GX_DefaultParam}
  {$DEFINE GX_Overload}
  {$DEFINE GX_CppHeaderDefines}

  {$DEFINE GX_BCB}
{$ENDIF VER125}

{ Borland Delphi 4.0 }
{$IFDEF VER120}
  {$DEFINE GX_VER120_up}
  {$DEFINE GX_VER110_up}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}
  {$DEFINE GX_VCL35_up}
  {$DEFINE GX_VCL40_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}
  {$DEFINE GX_DefaultParam}
  {$DEFINE GX_Overload}
  {$DEFINE GX_CppHeaderDefines}

  {$DEFINE GX_Delphi}
{$ENDIF VER120}

{ Borland C++Builder 3.0 }
{$IFDEF VER110}
  {$DEFINE GX_VER110_up}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}
  {$DEFINE GX_VCL35_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}
  {$DEFINE GX_DefaultParam}
  {$DEFINE GX_CppHeaderDefines}

  {$DEFINE GX_BCB}
{$ENDIF VER110}

{ Borland Delphi 3.0 }
{$IFDEF VER100}
  {$DEFINE GX_VER100_up}
  {$DEFINE GX_VER93_up}
  {$DEFINE GX_VER90_up}
  {$DEFINE GX_VER80_up}

  {$DEFINE GX_VCL10_up}
  {$DEFINE GX_VCL20_up}
  {$DEFINE GX_VCL30_up}

  {$DEFINE GX_Interfaces}
  {$DEFINE GX_Resourcestring}

  {$DEFINE GX_Delphi}
{$ENDIF VER100}


// ******************************************************************
// This item determines whether we support docking in the IDE

{$IFNDEF GX_VER120_up}
  // Only Delphi 4.0 and later allow docking in the IDE
  {$UNDEF EnableIdeDockingSupport}
{$ENDIF GX_VER120_up}

{$IFDEF GX_VER130_up}
  // Well, as of D5+, we essentially need to dock
  // in order to get a consistent user interface
  // thanks to "native" docking.
  {$DEFINE EnableIdeDockingSupport}
{$ENDIF GX_VER130_up}

// ******************************************************************
// These items determine whether we can build with
// the *native* Tools API (i.e. link with packages)

{$UNDEF GX_UseNativeToolsApi}

{$IFDEF GX_VER120_up}
  {$DEFINE GX_UseNativeToolsApi}
{$ENDIF GX_VER120_up}

{$IFDEF GX_NotPackageBuilt}
  {$UNDEF GX_UseNativeToolsApi}
{$ENDIF GX_NotPackageBuilt}

// ******************************************************************
// These items determine whether we can use TAction
// within the IDE.

{$UNDEF GX_HasActionSupport}

{$IFDEF GX_VER120_up}
  {$IFDEF GX_UseNativeToolsApi}
    {$DEFINE GX_HasActionSupport}
  {$ENDIF GX_UseNativeToolsApi}
{$ENDIF GX_VER120_up}


// ******************************************************************
// These items determine whether we want subclass the main window
// for multi-line tabs

{$UNDEF GX_MultiLinePaletteSubclass}

{$IFDEF VER100} // Delphi 3.0
  {$DEFINE GX_MultiLinePaletteSubclass}
{$ENDIF VER100}

{$IFDEF VER110} // C++ Builder 3.0
  {$DEFINE GX_MultiLinePaletteSubclass}
{$ENDIF VER110}

