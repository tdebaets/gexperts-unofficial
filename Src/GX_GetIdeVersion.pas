unit GX_GetIdeVersion;

// Original Authors: Stefan Hoffmeister and Erik Berry

interface

{$I GX_CondDefine.inc}

type
  TBorlandIdeVersion =
    (ideUndetected, ideUnknown,
     ideD300, ideD301, ideD302,
     ideD400, ideD401, ideD402, ideD403,
     ideD500, ideD501,
     ideBCB300, ideBCB301,
     ideBCB400, ideBCB401, ideBCB402,
     ideBCB500, ideBCB501
     );

// Returns the *exact* version of the product;
//
// Note that the IDE executable and hence the IDE's reported
// version number in the about box may have not been changed.
//
// We err on the safe side, i.e. until we do not
// detect a feature of a higher version, we do
// not increment the version number to something
// higher.

function GetBorlandIdeVersion: TBorlandIdeVersion;

implementation

uses
  Windows,
  {$IFDEF GX_UseNativeToolsApi}
    ToolsApi,
  {$ELSE not GX_UseNativeToolsApi}
    ExptIntf, ToolIntf,
  {$ENDIF GX_UseNativeToolsApi}
  SysUtils, FileCtrl, Dialogs,
  GX_GenFunc;

var
  DetectedVersion: TBorlandIdeVersion;

type
  TVersionNumber = packed record
    case Boolean of
      True:  ( dwFileVersionMS: DWORD;    { e.g. $00030075 = "3.75" }
               dwFileVersionLS: DWORD;    { e.g. $00000031 = "0.31" }
              );
      False: ( Minor: Word;
               Major: Word;
               Build: Word;
               Release: Word;
              );
    end;

type
  EVersionInfoNotFound = class(Exception);

function GetFileVersionNumber(const FileName: string): TVersionNumber;
var
  VersionInfoBufferSize: DWORD;
  dummyHandle: DWORD;
  VersionInfoBuffer: Pointer;
  FixedFileInfoPtr: PVSFixedFileInfo;
  VersionValueLength: UINT;
begin
  Assert(FileExists(FileName), FileName + ' does not exist to obtain VersionInfo');

  VersionInfoBufferSize := GetFileVersionInfoSize(PChar(FileName), dummyHandle);
  if VersionInfoBufferSize = 0 then
  begin
    raise EVersionInfoNotFound.Create(SysErrorMessage(GetLastError));
  end;

  GetMem(VersionInfoBuffer, VersionInfoBufferSize);
  try
    Win32Check(GetFileVersionInfo(PChar(FileName), dummyHandle,
                                  VersionInfoBufferSize, VersionInfoBuffer));

    // Retrieve root block / VS_FIXEDFILEINFO
    Win32Check(VerQueryValue(VersionInfoBuffer, '\',
                             Pointer(FixedFileInfoPtr), VersionValueLength));

    Result.dwFileVersionMS := FixedFileInfoPtr^.dwFileVersionMS;
    Result.dwFileVersionLS := FixedFileInfoPtr^.dwFileVersionLS;
  finally
    FreeMem(VersionInfoBuffer);
  end;
end;

// Result < 0 if V1 < V2
// Result = 0 if V1 = V2
// Result > 0 if V1 > V2
function CompareVersionNumber(const V1, V2: TVersionNumber): Integer;
begin
  Result := V1.Major - V2.Major;
  if Result <> 0 then
    Exit;

  Result := V1.Minor - V2.Minor;
  if Result <> 0 then
    Exit;

  Result := V1.Release - V2.Release;
  if Result <> 0 then
    Exit;

  Result := V1.Build - V2.Build;
end;

// *****************************************************

function GetBaseRegistryKey: string;
var
  {$IFDEF GX_UseNativeToolsApi}
    IServices: IOTAServices;
  {$ENDIF GX_UseNativeToolsApi}
begin
  {$IFDEF GX_UseNativeToolsApi}
    Assert(Assigned(BorlandIDEServices));
    IServices := BorlandIDEServices as IOTAServices;

    Assert(Assigned(IServices));
    Result := IServices.GetBaseRegistryKey;
  {$ELSE not GX_UseNativeToolsApi}
    Assert(Assigned(ToolServices));
    Result := ToolServices.GetBaseRegistryKey;
  {$ENDIF GX_UseNativeToolsApi}

  // Note that possibly a base key is returned with
  // a leading backslash which is something that
  // the Windows API does not like. Remove that.
  while (Length(Result) > 0) and (Result[1] = '\') do
    Delete(Result, 1, 1);
end;

// These >registry keys< are known to point (->) to the
// root directory >registry value name< of an installation:
//
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\C++Builder\5.0 -> RootDir
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\C++Builder\4.0 -> RootDir
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\C++Builder\3.0 -> RootDir
//
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\5.0 -> RootDir
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\4.0 -> RootDir
// HKEY_LOCAL_MACHINE\SOFTWARE\Borland\Delphi\3.0 -> RootDir

// Function returns root folder of installation
// with a backslash appended.
// Asserts that folder exists (Note: no exception, only assert).
function GetInstallationRootFolder: string;

  procedure RegistryError(const RegistryOperationResult: Longint);
  begin
    if RegistryOperationResult <> ERROR_SUCCESS then
      RaiseLastWin32Error;
  end;

const
  RootDirValueName = 'RootDir'; // Do not localize.
var
  RegistryHandle: HKEY;
  ParameterType: DWORD;
  BufferSize: DWORD;
  Buffer: Pointer;
begin
  RegistryError(RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(GetBaseRegistryKey()), 0, KEY_READ, RegistryHandle));
  try
    // Determine the amount of bytes we need to
    // allocate in order to retrieve the data.
    RegistryError(RegQueryValueEx(RegistryHandle, RootDirValueName, nil,
                                    @ParameterType, nil, @BufferSize));

    // We do expect a string. Anything else
    // and we don't know how to handle it.
    Assert((ParameterType and REG_SZ) = REG_SZ);

    GetMem(Buffer, BufferSize);
    try
      // Get data out of registry.
      RegistryError(RegQueryValueEx(RegistryHandle, RootDirValueName, nil,
                                    @ParameterType, Buffer, @BufferSize));

      Result := PChar(Buffer);
    finally
      FreeMem(Buffer);
    end;
  finally
    RegistryError(RegCloseKey(RegistryHandle));
  end;

  Assert(DirectoryExists(Result), 'The Borland IDE RootDir registry entry points to a non-existent directory');

  Result := AddSlash(Result);
end;

// *****************************************************
{
  Delphi 3.00:

  File          File Version  Size      Modified Time
  delphi32.exe  3.0.5.53      2027464   Monday, March 24, 1997 3:00:00 AM
  dcldb30.dpl   3.0.5.53      376320    Monday, March 24, 1997 3:00:00 AM
  dclstd30.dpl  3.0.5.53      198656    Monday, March 24, 1997 3:00:00 AM
  tlibimp.exe   [none]        276480    Monday, March 24, 1997 3:00:00 AM
  vcldb30.dcp   [none]        630696    Monday, March 24, 1997 3:00:00 AM
  vcl30.dcp     [none]        2284263   Monday, March 24, 1997 3:00:00 AM


  Delphi 3.01:

  File          File Version  Size      Modified Time
  delphi32.exe  3.0.5.83      2045888   Tuesday, August 05, 1997 3:01:00 AM
  dcldb30.dpl   3.0.5.83      381440    Tuesday, August 05, 1997 3:01:00 AM
  dclstd30.dpl  3.0.5.83      199168    Tuesday, August 05, 1997 3:01:00 AM
  tlibimp.exe   [none]        269312    Tuesday, August 05, 1997 3:01:00 AM
  scktsrvr.exe  [none]        314368    Tuesday, August 05, 1997 3:01:00 AM
  vcldb30.dcp   [none]        636049    Tuesday, August 05, 1997 3:01:00 AM
  vcl30.dcp     [none]        2288236   Tuesday, August 05, 1997 3:01:00 AM


  Delphi 3.02:

  File          File Version  Size      Modified Time
  delphi32.exe  3.0.5.83      2045888   Tuesday,  August  05, 1997 3:01:00 AM
  dcldb30.dpl   3.0.5.83      381952    Thursday, October 23, 1997 3:02:00 AM
  dclstd30.dpl  3.0.5.83      199168    Tuesday,  August  05, 1997 3:01:00 AM
  tlibimp.exe   [none]        269824    Thursday, October 23, 1997 3:02:00 AM
  scktsrvr.exe  [none]        314368    Thursday, October 23, 1997 3:02:00 AM
  vcldb30.dcp   [none]        636490    Thursday, October 23, 1997 3:02:00 AM
  vcl30.dcp     [none]        2288985   Thursday, October 23, 1997 3:02:00 AM
}
function GetDelphi3IdeVersion: TBorlandIdeVersion;
const
  Delphi32_EXE_D301: TVersionNumber =
    (Minor: 0; Major: 3; Build: 83; Release: 5);

  FileSizeDCLDB30_D302 = 381952;
var
  RootFolder: string;

  ReadFileSize: Integer;
  ReadFileVersion: TVersionNumber;
begin
  Result := ideD300;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\DCLDB30.DPL');
  if CompareVersionNumber(ReadFileVersion, Delphi32_EXE_D301) = 0 then
    Result := ideD301;

  // D3.02 can only be installed on top of D3.01
  if Result = ideD301 then
  begin
    // Try to detect D3.02; we choose to check the
    // file size of DCLDB30.DPL, because it is an
    // IDE design-time package which is handled
    // exclusively by the IDE.
    RootFolder := GetInstallationRootFolder;
    ReadFileSize := GetFileSize(RootFolder + 'Bin\DCLDB30.DPL');
    if ReadFileSize = FileSizeDCLDB30_D302 then
      Result := ideD302;
  end;
end;

{
  C++Builder 3.00:

  File          File Version  Size      Modified Time
  comp32p.dll   4.12.207.2    913498    Monday, February 9, 1998 3:00:00 AM
  ilink32.dll   3.0.0.0       266240    Monday, February 9, 1998 3:00:00 AM
  ilink32.exe   3.0.0.0       270226    Monday, February 9, 1998 3:00:00 AM


  C++Builder 3.01:

  File          File Version  Size      Modified Time
  comp32p.dll   4.12.210.2    913408    Monday, July 6, 1998 3:01:00 AM
  ilink32.dll   3.0.0.0       266752    Monday, July 6, 1998 3:01:00 AM
  ilink32.exe   3.0.0.0       270336    Monday, July 6, 1998 3:01:00 AM

  It appears as if comp32p.dll is the best detector 3.00 -> 3.01
}

function GetCppBuilder3IdeVersion: TBorlandIdeVersion;
const
  COMP32P_DLL_BCB301: TVersionNumber =
    (Minor: 12; Major: 4; Build: 2; Release: 210);
var
  ReadFileVersion: TVersionNumber;
begin
  Result := ideBCB300;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\comp32p.dll');
  if CompareVersionNumber(ReadFileVersion, COMP32P_DLL_BCB301) = 0 then
    Result := ideBCB301;
end;

{
  Delphi 4.00:

  File          File Version  Size      Modified Time
  delphi32.exe  4.0.5.37      343040    Wednesday, June 17, 1998 4:00:00 AM!
  coride40.bpl  4.0.5.25      3698176   Thursday, June 18, 1998 4:00:00 AM
  dcldb40.bpl   4.0.5.37      466944    Thursday, June 18, 1998 4:00:00 AM
  dclstd40.bpl  4.0.5.37      338944    Thursday, June 18, 1998 4:00:00 AM
  vclide40.bpl  4.0.5.25      847360    Thursday, June 18, 1998 4:00:00 AM
  dphide40.bpl  4.0.5.25      602112    Thursday, June 18, 1998 4:00:00 AM
  dbx40.bpl     4.0.0.0       847872    Thursday, June 18, 1998 4:00:00 AM
  dcc32.exe     [empty]       543232    Thursday, June 18, 1998 4:00:00 AM
  tlibimp.exe   [none]        304128    Thursday, June 18, 1998 4:00:00 AM
  scktsrvc.exe  [none]        474624    Thursday, June 18, 1998 4:00:00 AM
  sqlmon.exe    3.0           504320    Thursday, June 18, 1998 4:00:00 AM
  imagedit.exe  4.0.5.37      520192    Thursday, June 18, 1998 4:00:00 AM
  pce.exe       [none]        538624    Thursday, June 18, 1998 4:00:00 AM
  vcl40.dcp     [none]        3619257   Thursday, June 18, 1998 4:00:00 AM
  vcldb40.dcp   [none]        846752    Thursday, June 18, 1998 4:00:00 AM
  dbx40.dcp     [none]        650904    Thursday, June 18, 1998 4:00:00 AM


  Delphi 4.01 (Update #1):

  File          File Version  Size      Modified Time
  delphi32.exe  4.0.5.37      343040    Wednesday, June   17, 1998 4:00:00 AM!
  coride40.bpl  4.0.5.45      3698688   Tuesday,   August 11, 1998 4:00:00 AM
  dcldb40.bpl   4.0.5.45      466944    Tuesday,   August 11, 1998 4:00:00 AM
  dclstd40.bpl  4.0.5.37      338944    Thursday,  June   18, 1998 4:00:00 AM
  vclide40.bpl  4.0.5.25      847360    Thursday,  June   18, 1998 4:00:00 AM
  dphide40.bpl  4.0.5.25      605184    Tuesday,   August 11, 1998 4:00:00 AM
  dbx40.bpl     4.0.0.0       847872    Thursday,  June   18, 1998 4:00:00 AM
  dcc32.exe     [empty]       543232    Tuesday,   August 11, 1998 4:00:00 AM
  tlibimp.exe   [none]        304128    Tuesday,   August 11, 1998 4:00:00 AM
  scktsrvc.exe  [none]        474624    Thursday,  June   18, 1998 4:00:00 AM
  sqlmon.exe    3.0           504320    Thursday,  June   18, 1998 4:00:00 AM
  imagedit.exe  4.0.5.37      520192    Thursday,  June   18, 1998 4:00:00 AM
  pce.exe       [none]        538624    Thursday,  June   18, 1998 4:00:00 AM
  vcl40.dcp     [none]        3619304   Tuesday,   August 11, 1998 4:00:00 AM
  vcldb40.dcp   [none]        846944    Tuesday,   August 11, 1998 4:00:00 AM
  dbx40.dcp     [none]        650904    Thursday,  June   18, 1998 4:00:00 AM

  coride40.bpl looks to be the best determinant of 4.00 -> 4.01.


  Delphi 4.02 (Update #2):

  File          File Version  Size      Modified Time
  delphi32.exe  4.0.5.104     343552    Thursday, October 22, 1998 4:01:00 AM
  coride40.bpl  4.0.5.104     3706368   Thursday, October 22, 1998 4:01:00 AM
  dcldb40.bpl   4.0.5.104     466944    Thursday, October 22, 1998 4:01:00 AM
  dclstd40.bpl  4.0.5.104     339968    Thursday, October 22, 1998 4:01:00 AM
  vclide40.bpl  4.0.5.104     849408    Thursday, October 22, 1998 4:01:00 AM
  dphide40.bpl  4.0.5.104     604160    Thursday, October 22, 1998 4:01:00 AM
  dbx40.bpl     4.0.0.0       847872    Thursday, October 22, 1998 4:01:00 AM
  dcc32.exe     [empty]       545280    Thursday, October 22, 1998 4:01:00 AM
  tlibimp.exe   [none]        307200    Thursday, October 22, 1998 4:01:00 AM
  scktsrvc.exe  [none]        500736    Thursday, October 22, 1998 4:01:00 AM
  sqlmon.exe    3.0           505856    Thursday, October 22, 1998 4:01:00 AM
  imagedit.exe  4.0.5.104     521216    Thursday, October 22, 1998 4:01:00 AM
  pce.exe       [none]        540672    Thursday, October 22, 1998 4:01:00 AM
  vcl40.dcp     [none]        3622345   Thursday, October 22, 1998 4:01:00 AM
  vcldb40.dcp   [none]        847552    Thursday, October 22, 1998 4:01:00 AM
  dbx40.dcp     [none]        650915    Thursday, October 22, 1998 4:01:00 AM

  delphi32.exe looks to be the best determinant of 4.01 -> 4.02.

  NOTE: The update's registry entries we discussed in the newsgroups might not
  be changed when the second update was installed from an original 4.02 CD and
  not the Update Patch from the Internet.


  Delphi 4.03 (Update #3):

  File          File Version  Size      Modified Time
  delphi32.exe  4.0.5.108     385024    Tuesday, February 09, 1999 4:02:00 AM
  coride40.bpl  4.0.5.104     3706368   Thursday, October 22, 1998 4:01:00 AM
  dcldb40.bpl   4.0.5.104     466944    Thursday, October 22, 1998 4:01:00 AM
  dclstd40.bpl  4.0.5.108     339968    Tuesday, February 09, 1999 4:02:00 AM
  vclide40.bpl  4.0.5.104     849408    Thursday, October 22, 1998 4:01:00 AM
  dphide40.bpl  4.0.5.104     604160    Thursday, October 22, 1998 4:01:00 AM
  dbx40.bpl     4.0.0.0       847872    Thursday, October 22, 1998 4:01:00 AM
  dcc32.exe     [empty]       545280    Thursday, October 22, 1998 4:01:00 AM
  tlibimp.exe   [none]        307200    Thursday, October 22, 1998 4:01:00 AM
  scktsrvc.exe  [none]        500224    Wednesday,January 27, 1999 4:00:00 AM!
  sqlmon.exe    3.0           505856    Thursday, October 22, 1998 4:01:00 AM
  imagedit.exe  4.0.5.104     521216    Thursday, October 22, 1998 4:01:00 AM
  pce.exe       [none]        540672    Thursday, October 22, 1998 4:01:00 AM
  vcl40.dcp     [none]        3622200   Wednesday,February 17,1999 4:02:00 AM
  vcldb40.dcp   [none]        847552    Thursday, October 22, 1998 4:01:00 AM
  dbx40.dcp     [none]        650915    Thursday, October 22, 1998 4:01:00 AM

  delphi32.exe looks to be the best determinant of 4.02 -> 4.03.
}
function GetDelphi4IdeVersion: TBorlandIdeVersion;
const
  CORIDE40_BPL_D401: TVersionNumber =
    (Minor: 0; Major: 4; Build: 45; Release: 5);

  DELPHI32_EXE_D402: TVersionNumber =
    (Minor: 0; Major: 4;  Build: 104; Release: 5);

  DELPHI32_EXE_D403: TVersionNumber =
    (Minor: 0; Major: 4; Build: 108; Release: 5);
var
  ReadFileVersion: TVersionNumber;
begin
  Result := ideD400;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\CORIDE40.BPL');
  if CompareVersionNumber(ReadFileVersion, CORIDE40_BPL_D401) = 0 then
  begin
    Result := ideD401;
    Exit;
  end;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\DELPHI32.EXE');
  if CompareVersionNumber(ReadFileVersion, DELPHI32_EXE_D402) = 0 then
  begin
    Result := ideD402;
    Exit;
  end;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\DELPHI32.EXE');
  if CompareVersionNumber(ReadFileVersion, DELPHI32_EXE_D403) = 0 then
    Result := ideD403;
end;

{
  C++Builder 4.00:

  File          File Version  Size      Modified Time
  bcb.exe       4.0.14.4      508928    Wednesday, January 27, 1999 4:00:00 AM
  comp32p.dll   4.12.210.2    1044480   Wednesday, January 27, 1999 4:00:00 AM
  ilink32.dll   4.0.10.23     274432    Wednesday, January 27, 1999 4:00:00 AM


  C++Builder 4.01:

  File          File Version  Size      Modified Time
  bcb.exe       4.0.14.11     508928    Tuesday, June 1, 1999 4:01:00 AM
  comp32p.dll   4.12.210.2    1044480   Tuesday, June 1, 1999 4:01:00 AM
  ilink32.dll   4.0.10.26     274432    Tuesday, June 1, 1999 4:01:00 AM

  It appears as if bcb.exe is the best detector 4.00 -> 4.01


  C++Builder 4.02:

  File          File Version  Size      Modified Time
  bcb.exe       4.0.14.11     508928    Tuesday, June 1, 1999 4:01:00 AM
  comp32p.dll   4.12.210.2    1044480   Friday, October 15, 1999 4:02:00 AM
  ilink32.dll   4.0.10.26     274432    Tuesday, June 1, 1999 4:01:00 AM

  It appears as if the ONLY way to detect 4.02 is to have a look
  at the time-stamp of comp32p.dll (only bcc32.exe, comp32p.dll,
  cpp32.exe were changed)
}

function GetCppBuilder4IdeVersion: TBorlandIdeVersion;
const
  BCB_EXE_BCB401: TVersionNumber =
    (Minor: 0; Major: 4; Build: 14; Release: 11);
var
  ReadFileVersion: TVersionNumber;
  PatchedFileTimeStamp: TDateTime;
  Difference: Integer;
begin
  Result := ideBCB400;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\BCB.EXE');
  if CompareVersionNumber(ReadFileVersion, BCB_EXE_BCB401) = 0 then
    Result := ideBCB401;

  // BCB4.02 was not a "full patch", but incremental:
  if Result = ideBCB401 then
  begin
    PatchedFileTimeStamp := EncodeDate(1999, 10, 15);
    PatchedFileTimeStamp := PatchedFileTimeStamp + EncodeTime(04, 02, 0, 0); // 4:02:00 am

    Difference := FileAge(GetInstallationRootFolder + 'Bin\COMP32P.DLL') -
                  DateTimeToFileDate(PatchedFileTimeStamp);

    if Difference = 0 then
    begin
      Result := ideBCB402;
    end;
  end;
end;

{
  Delphi 5.00:

  File          File Version  Size      Modified Time
  coride40.bpl  5.0.5.62      3779072   Wednesday, August 11, 1999 5:00:00 AM
  Dsnide50.bpl  5.0.5.62      750592    Wednesday, August 11, 1999 5:00:00 AM
  Dphide50.bpl  5.0.5.62      409600    Wednesday, August 11, 1999 5:00:00 AM
  Vclide50.bpl  5.0.5.62      863232    Wednesday, August 11, 1999 5:00:00 AM

  Delphi 5.01:
  coride40.bpl  5.0.6.18      3780096   Monday, January 24, 2000 5:01:00 AM
  Dsnide50.bpl  5.0.6.18      760320    Monday, January 24, 2000 5:01:00 AM
  Dphide50.bpl  5.0.6.18      410112    Monday, January 24, 2000 5:01:00 AM
  Vclide50.bpl  5.0.6.18      863232    Monday, January 24, 2000 5:01:00 AM
  delphi32.exe  5.0.6.18      424960    Monday, January 24, 2000 5:01:00 AM
}

function GetDelphi5IdeVersion: TBorlandIdeVersion;
const
  Delphi32_EXE_D500: TVersionNumber =
    (Minor: 0; Major: 5; Build: 68; Release: 5);
var
  ReadFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideD500;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\DELPHI32.EXE');
  VersionNumber := CompareVersionNumber(ReadFileVersion, Delphi32_EXE_D500);
  if VersionNumber > 0 then
    Result := ideD501;
end;

{
  C++Builder 5.00:

  File          File Version  Size      Modified Time
  bcc32.exe     5.0.12.34     866304    Monday, January 31, 2000 5:00:00 AM
  bordbg51.exe  5.0.12.34     219136    Monday, January 31, 2000 5:00:00 AM
  cgconfig.exe  5.0.12.34     571392    Monday, January 31, 2000 5:00:00 AM
  dsnide50.bpl  5.0.12.34     760832    Monday, January 31, 2000 5:00:00 AM
  bcb.exe       5.0.12.34     954368    Monday, January 31, 2000 5:00:00 AM
  bcbide50.bpl  5.0.12.34     1121792   Monday, January 31, 2000 5:00:00 AM
  coride50.bpl  5.0.12.34     3883008   Monday, January 31, 2000 5:00:00 AM

  C++Builder 5.01:

  File          File Version  Size      Modified Time
  bcc32.exe     5.5.1.1       869376    Monday, August 07, 2000, 5:01:00 AM
  bordbg51.exe  5.0.12.34     219136    Monday, January 31, 2000 5:00:00 AM
  cgconfig.exe  5.0.12.34     571392    Monday, January 31, 2000 5:00:00 AM
  dsnide50.bpl  5.0.12.34     760832    Monday, January 31, 2000 5:00:00 AM
  bcb.exe       5.0.12.34     954368    Monday, January 31, 2000 5:00:00 AM
  bcbide50.bpl  5.0.12.34     1121792   Monday, January 31, 2000 5:00:00 AM
  coride50.bpl  5.0.12.34     3883008   Monday, January 31, 2000 5:00:00 AM
  tlibimp.exe   5.0.12.34(!)  23040     Monday, August 07, 2000, 5:01:00 AM
  tlib50.bpl    5.0.12.34(!)  589312    Monday, August 07, 2000, 5:01:00 AM
  ilink32.exe   5.0.1.1       326144    Monday, August 07, 2000, 5:01:00 AM
}

function GetCppBuilder5IdeVersion: TBorlandIdeVersion;
const
  BCC32_EXE_BCB500: TVersionNumber =
    (Minor: 0; Major: 5; Build: 34; Release: 12);
var
  ReadFileVersion: TVersionNumber;
  VersionNumber: Integer;
begin
  Result := ideBCB500;

  ReadFileVersion := GetFileVersionNumber(GetInstallationRootFolder + 'Bin\BCC32.EXE');
  VersionNumber := CompareVersionNumber(ReadFileVersion, BCC32_EXE_BCB500);
  if VersionNumber > 0 then
    Result := ideBCB501;
end;

function GetBorlandIdeVersion: TBorlandIdeVersion;
begin
  {TODO -oStefan -cNewVersion:
     Always check the conditional defines
     for each new minor or major version
     of Delphi or C++Builder being released. }

  // We only actually detect the version once per session
  // The previous result is cached in DetectedVersion
  if DetectedVersion <> ideUndetected then
  begin
    Result := DetectedVersion;
    Exit;
  end;

  {$IFDEF VER100} // Delphi 3.0
    Result := GetDelphi3IdeVersion;
    Assert(Result in [ideD300, ideD301, ide302]);
  {$ENDIF VER100}

  {$IFDEF VER110}  // C++Builder 3.0
    Result := GetCppBuilder3IdeVersion;
    Assert(Result in [ideBCB300, ideBCB301]);
  {$ENDIF VER110}

  {$IFDEF VER120}  // Delphi 4.0
    Result := GetDelphi4IdeVersion;
    Assert(Result in [ideD400, ideD401, ideD402, ideD403]);
  {$ENDIF VER120}

  {$IFDEF VER125}  // C++Builder 4.0
    Result := GetCppBuilder4IdeVersion;
    Assert(Result in [ideBCB400, ideBCB401]);
  {$ENDIF VER125}

  {$IFDEF VER130}  // Delphi 5.0 and C++Builder 5.0
    {$IFDEF GX_BCB}
      Result := GetCppBuilder5IdeVersion;
      Assert(Result in [ideBCB500, ideBCB501]);
    {$ELSE not GX_BCB}
      Result := GetDelphi5IdeVersion;
      Assert(Result in [ideD500, ideD501]);
    {$ENDIF VER130}
  {$ENDIF VER130}

  if Result = ideUnknown then
    MessageDlg('Unknown IDE major version detected.  Please update GX_GetIdeVersion.pas.', mtError, [mbOK], 0);

  DetectedVersion := Result;
end;

initialization
  DetectedVersion := ideUndetected;

end.
