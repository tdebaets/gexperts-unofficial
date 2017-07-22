
#ifndef GX_PrecompiledHeaderH
#define GX_PrecompiledHeaderH

// ***********************************************************

//
// The "UseDevelopmentHeaders" #define determines
// whether (pre-compiled) headers are optimised
// for one-time "fire and forget" builds or for
// builds that occur during development.
//
// This distinction is important as modifying
// the Object Pascal / Delphi source code during
// development results in *all* auto-generated
// header (.HPP) files to be updated. This in
// turn would enforce a complete rebuild of
// the pre-compiled header file if these auto-
// generated headers were included in this
// globally optimized pre-compiled header.
//
// The pre-compiled header optimized for
// development use should never #include any
// files where the Object Pascal source file
// is part of the project (see Project Manager)
//

#undef  UseDevelopmentHeaders
#define UseDevelopmentHeaders

// ***********************************************************

// Base VCL support

// vcl.h
// vcl0.h

#if !defined(_WINDOWS_)               // Don't optimize if WINDOWS.H has already been included
  #if !defined(NO_WIN32_LEAN_AND_MEAN)
    #define WIN32_LEAN_AND_MEAN           // Enable LEAN_AND_MEAN support
    #define  _VCL_LEAN_AND_MEAN           // BCB v1.0 compatible
  #endif                                  // NO_WIN32_LEAN_AND_MEAN
#endif                                // _WINDOWS_

#if !defined(COM_NO_WINDOWS_H)        // Make sure COM Headers don't include WINDOWS.H/OLE2.H
  #define COM_NO_WINDOWS_H
  #define UNDEF_COM_NO_WINDOWS_H
#endif

#if !defined(RPC_NO_WINDOWS_H)        // Make sure RPC Headers don't include WINDOWS.H
  #define RPC_NO_WINDOWS_H
  #define UNDEF_RPC_NO_WINDOWS_H
#endif

#include <System.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <ActiveX.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <Consts.hpp>	// Pascal unit
#include <SysConst.hpp>	// Pascal unit
#include <TypInfo.hpp>	// Pascal unit
#include <Imm.hpp>	// Pascal unit

#if (__BORLANDC__ == 0x0540)  // C++Builder 4.0 or later
  #include <MultiMon.hpp>	// Pascal unit
#endif

#include <CommDlg.hpp>	// Pascal unit
#include <RichEdit.hpp>	// Pascal unit
#include <Commctrl.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <IniFiles.hpp>	// Pascal unit
#include <Registry.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Clipbrd.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit

#if (__BORLANDC__ == 0x0540)  // C++Builder 4.0 or later
  #include <ActnList.hpp>	// Pascal unit
#endif

#include <Menus.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <ToolWin.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <DsgnIntf.hpp>	// Pascal unit

// Open Tools API support

#if (__BORLANDC__ == 0x0540)  // C++Builder 4.0 or later
  #include <ToolsAPI.hpp>	// Pascal unit -- note that we compile this unit, but use the *shipped* header
#endif

#include <VirtIntf.hpp>	// Pascal unit
#include <EditIntf.hpp>	// Pascal unit
#include <FileIntf.hpp>	// Pascal unit
#include <ToolIntf.hpp>	// Pascal unit
#include <Exptintf.hpp>	// Pascal unit

  // Usually we will build with packages
  #undef UsePackages
  #define UsePackages
  #ifdef UsePackages
    #include <LibIntf.hpp> // Pascal unit
  #endif UsePackages

#if defined(UNDEF_COM_NO_WINDOWS_H)   // Clean up MACRO to prevent inclusion of WINDOWS.H/OLE2.H
  #undef COM_NO_WINDOWS_H
  #undef UNDEF_COM_NO_WINDOWS_H
#endif

#if defined(UNDEF_RPC_NO_WINDOWS_H)   // Clean up MACRO to prevent inclusion of WINDOWS.H
  #undef RPC_NO_WINDOWS_H
  #undef UNDEF_RPC_NO_WINDOWS_H
#endif

#if !defined(UseDevelopmentHeaders)

// EII support

#include <EIExtension.hpp>	// Pascal unit
#include <EIEditWrapper.hpp>	// Pascal unit
#include <EIPanel.hpp>	// Pascal unit  --- static const Shortint

#include <EIManager.hpp>	// Pascal unit
#include <EINotifiers.hpp>	// Pascal unit

// Utility code for GExperts

#include <OwnerList.hpp>	// Pascal unit
#include <ExpertUtil.hpp>	// Pascal unit

// Components used by GExperts

#include <SpinIntEdit.hpp>	// Pascal unit
#include <HeadList.hpp>	// Pascal unit --- static const char

// GExperts files used by GX_GExperts and GX_uGenFunc

#include <Gx_uMultiLinePalette.hpp>	// Pascal unit
#include <GX_uExperts.hpp>	// Pascal unit
#include <GX_uMultilineHost.hpp>	// Pascal unit
#include <GX_Dfm2Txt.hpp>	// Pascal unit
#include <GX_IDEEnhance.hpp>	// Pascal unit
#include <GX_uConfigure.hpp>	// Pascal unit
#include <Gx_uConfigurationInfo.hpp>	// Pascal unit
#include <GX_uNotify.hpp>	// Pascal unit
#include <GX_EditorExpert.hpp>	// Pascal unit

// These are the files we need for building
// GExperts in C++Builder

#include "src\GX_GExperts.hpp"
#include "src\GX_uGenFunc.hpp"

#endif !defined(UseDevelopmentHeaders)

#endif GX_PrecompiledHeaderH
