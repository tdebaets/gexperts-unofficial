unit GX_OtaUtils;

// This file contains a subset of the code in GX_OtaUtils.pas from the latest
// GExperts source code.

{$I GX_CondDefine.inc}

interface

{$IFDEF GX_UseNativeToolsApi}

uses Classes, SysUtils, Windows, FileCtrl, ToolsApi, GX_GenFunc, GX_GenericUtils;

// Get an IDE services interface.  Succeeds or raises an exception.
function GxOtaGetIDEServices: IOTAServices;

// Returns reference to currently active project;
// returns nil if there is no (active) project.
function GxOtaGetCurrentProject: IOTAProject;

// Returns file name of the project; returns
// an empty string if there is no (active) project.
// Use NormalizeBdsProj to get the dpr instead of the bdsproj/dproj for Delphi projects
function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;
function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean = False): string;

// Returns the name of the currently active project; returns
// an empty string if there is no (active) project.
// The name of a project is the project's file name without
// a file extension and without path information.
function GxOtaGetCurrentProjectName: string;

// Returns reference to the IDE's project group;
// returns Nil if there is no project group.
function GxOtaGetProjectGroup: IOTAProjectGroup;

// Get the environment options interface.  Succeeds or raises an exception.
function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
// Return the IDE's environment string value that is
// associated with EnvironmentStringName
function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;

// Return the IDE's global library path
function GxOtaGetIdeLibraryPath: string;

///<summary>
/// Returns the project's platform, if any (and supported), or an empty string </summary>
function GxOtaGetProjectPlatform(Project: IOTAProject = nil): string;

///<summary>
/// Return project specific search path, with the directory containing the project
// file first.
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed. </summary>
procedure GxOtaGetProjectSourcePathStrings(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);
///<summary>
/// Return the global IDE library path (without the project specific paths).
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed. </summary>
procedure GxOtaGetIdeLibraryPathStrings(Paths: TStrings; DoProcessing: Boolean = true);
///<summary>
/// Return the effective library path, with the project specific paths
/// first and then the IDE's global library path.
/// @params if DoProcessing is true, the paths are macro expanded and non-existing
///                         paths removed. </summary>
procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);
// Retrieve a guess at all possible paths where files in the current project
// might be located by the compiler
procedure GxOtaGetAllPossiblePaths(Paths: TStrings);
// Locate a base file name on a list of paths
function GxOtaFindPathToFile(const FileName: string; Paths: TStrings = nil): string;
// Try to open a file located anywhere in GxOtaGetAllPossiblePaths

{$ENDIF GX_UseNativeToolsApi}

implementation

uses GX_dzFileUtils;

{$IFDEF GX_UseNativeToolsApi}

function GxOtaGetIDEServices: IOTAServices;
begin
  Result := BorlandIDEServices as IOTAServices;
  if not Assigned(Result) then
    raise Exception.Create('IOTAServices not implemented');
end;

function GxOtaGetProjectFileName(Project: IOTAProject; NormalizeBdsProj: Boolean = False): string;

  {$IFDEF GX_VER130_up}
  function SearchProjectSourceViaModule(var AProjectFileName: string): Boolean;
  var
    i: Integer;
    Module: IOTAModule;
    Editor: IOTAEditor;
  begin
    Result := False;
    Module := Project as IOTAModule;
    for i := 0 to Module.ModuleFileCount - 1 do
    begin
      Editor := Module.ModuleFileEditors[i];
      if IsProjectSource(Editor.FileName) then
      begin
        Result := True;
        AProjectFileName := Editor.FileName;
        Exit;
      end;
    end;
  end;

  function SearchProjectSourceViaFileExt(var AProjectFileName: string): Boolean;
  var
    PackageFileName: string;
  begin
    Result := GxOtaProjectIsEitherDelphi(Project);
    if Result then
    begin
      AProjectFileName := ChangeFileExt(AProjectFileName, '.dpr');
      if not GxOtaFileOrModuleExists(AProjectFileName) then
      begin
        PackageFileName := ChangeFileExt(AProjectFileName, '.dpk');
        if GxOtaFileOrModuleExists(PackageFileName) then
          AProjectFileName := PackageFileName
        else
          Result := False;
      end;
    end;
  end;
  {$ENDIF GX_VER130_up}

begin
  Result := '';
  if Assigned(Project) then begin
    Result := Project.FileName;
    {$IFDEF GX_VER130_up}
    if NormalizeBdsProj and IsBdsprojOrDproj(Result) then begin
      // Use a two-step search to get the right dpr/dpk/... file.
      // First search the IOTAProject's module list for a file with the correct
      // extension.  If this doesn't work, replace the bdsproj/dproj file
      // extension with dpr or dpk.  The second search can give wrong results
      // if the *proj file and the project source file don't have the same base
      // name (e.g. Demo.dpr and DemoD11.dproj).
      if not SearchProjectSourceViaModule(Result) then
        SearchProjectSourceViaFileExt(Result);
    end;
    {$ENDIF GX_VER130_up}
  end;
end;

function GxOtaGetCurrentProjectFileName(NormalizeBdsProj: Boolean): string;
begin
  Result := GxOtaGetProjectFileName(GxOtaGetCurrentProject, NormalizeBdsProj);
end;

function GxOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Assert(Assigned(BorlandIDEServices));

  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(Assigned(IModuleServices));

  Result := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    //if Supports(IModule, IOTAProjectGroup, Result) then
    if Succeeded(IModule.QueryInterface(IOTAProjectGroup, Result)) then
      Break;
  end;
end;

function GxOtaGetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := nil;

  IProjectGroup := GxOtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
  begin
    Assert(Assigned(BorlandIDEServices));
    IModuleServices := BorlandIDEServices as IOTAModuleServices;
    Assert(Assigned(IModuleServices));

    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      //if Supports(IModule, IOTAProject, Result) then
      if Succeeded(IModule.QueryInterface(IOTAProject, Result)) then
        Break;
    end;
  end;

  try
    // This raises exceptions in D5 with .bat projects active
    if Assigned(IProjectGroup) and (not Assigned(Result)) then
      Result := IProjectGroup.ActiveProject;
  except
    Result := nil;
  end;
end;

function GxOtaGetCurrentProjectName: string;
var
  IProject: IOTAProject;
begin
  Result := '';

  IProject := GxOtaGetCurrentProject;
  if Assigned(IProject) then
  begin
    Result := ExtractFileName(IProject.FileName);
    Result := ChangeFileExt(Result, '');
  end;
end;

function GxOtaGetEnvironmentOptions: IOTAEnvironmentOptions;
begin
  Result := GxOtaGetIDEServices.GetEnvironmentOptions;
  Assert(Assigned(Result));
end;

function GxOtaGetIdeEnvironmentString(const EnvironmentStringName: string): string;
begin
  Result := GxOtaGetEnvironmentOptions.Values[EnvironmentStringName];
end;

function GxOtaGetIdeLibraryPath: string;
begin
  {$IFDEF GX_VER130_up}
   // Do not localize.
  if RunningBDS2006OrGreater and GxOtaCurrentProjectIsDelphiDotNet then
    Result := GxOtaGetIdeEnvironmentString('DotNetLibraryPath')
  else if RunningBDS2006OrGreater and GxOtaCurrentProjectIsNativeCpp then
    Result := GxOtaGetIdeEnvironmentString('CppSearchPath')
  else
  {$ENDIF GX_VER130_up}
    Result := GxOtaGetIdeEnvironmentString('LibraryPath');
end;

function ReplaceMacro(const Str, OldValue, NewValue: string): string;
var
  ReplaceVal: string;
begin
  ReplaceVal := '$(' + OldValue + ')';
  Result := StringReplace(Str, ReplaceVal, NewValue, [rfReplaceAll, rfIgnoreCase]);
end;

procedure SplitIdePath(Strings: TStrings; IdePathString: string);
var
  PathItem: string;
  SemicolonPos: Integer;
begin
  SemicolonPos := Pos(';', IdePathString);
  while SemicolonPos > 0 do
  begin
    PathItem := Trim(Copy(IdePathString, 1, SemicolonPos - 1));
    if PathItem <> '' then
      EnsureStringInList(Strings, PathItem);
    Delete(IdePathString, 1, SemicolonPos);
    SemicolonPos := Pos(';', IdePathString);
  end;
  IdePathString := Trim(IdePathString);
  if IdePathString <> '' then
    EnsureStringInList(Strings, IdePathString);
end;

function GxOtaGetProjectPlatform(Project: IOTAProject = nil): string;
begin
  Result := '';
{$ifdef GX_VER230_up}
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Project <> nil then
    Result := Project.CurrentPlatform;
{$endif GX_VER230_up}
end;

function GetIdeRootDirectory: string;
begin
  Result := GxGetIdeInstallationDirectory;
end;

procedure ProcessPaths(Paths: TStrings; const Prefix: string; const PlatformName: string);
const
  IDEBaseMacros: array [0..2] of string = ('BDS', 'DELPHI', 'BCB');
var
  i: Integer;
  DirIdx: Integer;
  PathItem: string;
  BasePath: string;
  Environment: TStringList;
  EnvName: string;
  EnvValue: string;
begin
  BasePath := RemoveSlash(GetIdeRootDirectory);
  Environment := TStringList.Create;
  try
    GetEnvironmentVariables(Environment);
    for DirIdx := 0 to Paths.Count - 1 do begin
      PathItem := Paths[DirIdx];
      // Expand the IDE base folder names $([DELPHI,BCB,BDS])
      for i := Low(IDEBaseMacros) to High(IDEBaseMacros) do
        PathItem := ReplaceMacro(PathItem, IDEBaseMacros[i], BasePath);

      // Expand any environment variable macros
      for i := 0 to Environment.Count - 1 do begin
        EnvName := Environment.Names[i];
        EnvValue := Environment.Values[Environment.Names[i]];
        if (Trim(EnvName) <> '') and (Trim(EnvValue) <> '') then
          PathItem := ReplaceMacro(PathItem, EnvName, EnvValue);
      end;

      if PlatformName <> '' then
        PathItem := ReplaceMacro(PathItem, 'Platform', PlatformName);

      if not IsPathAbsolute(PathItem) then
      begin
        if Prefix <> '' then begin
          PathItem := TFileSystem.ExpandFileNameRelBaseDir(PathItem, Prefix);
        end;
      end;
      Paths[DirIdx] := PathItem;
    end;
  finally
    FreeAndNil(Environment);
  end;
end;

procedure GxOtaGetProjectSourcePathStrings(Paths: TStrings;
  Project: IOTAProject = nil; DoProcessing: Boolean = True);
var
  IdePathString: string;
  ProjectOptions: IOTAProjectOptions;
  ProjectDir: string;
  i: Integer;
  PlatformName: string;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  if Project = nil then
    Project := GxOtaGetCurrentProject;
  if Assigned(Project) then
  begin
    ProjectDir := ExtractFileDir(Project.FileName);
    // Add the current project directory first
    EnsureStringInList(Paths, ProjectDir);
    // Then the project search path
    ProjectOptions := Project.GetProjectOptions;
    if Assigned(ProjectOptions) then
    begin
      IdePathString := ProjectOptions.Values['SrcDir'];
      SplitIdePath(Paths, IdePathString);
    end;
  end;


  if DoProcessing then begin
    PlatformName := GxOtaGetProjectPlatform(Project);
    ProcessPaths(Paths, ProjectDir, PlatformName);
  end;
  for i := 0 to Paths.Count - 1 do begin
    Paths[i] := AddSlash(Paths[i]);
  end;
end;

procedure GxOtaGetIdeLibraryPathStrings(Paths: TStrings; DoProcessing: Boolean = true);
var
  IdePathString: string;
  i: Integer;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  IdePathString := GxOtaGetIdeLibraryPath;
  SplitIdePath(Paths, IdePathString);
  if DoProcessing then begin
    ProcessPaths(Paths, GetCurrentDir, '');
  end;
  for i := 0 to Paths.Count - 1 do begin
    Paths[i] := AddSlash(Paths[i]);
  end;
end;

procedure GxOtaGetEffectiveLibraryPath(Paths: TStrings;
  Project: IOTAProject; DoProcessing: Boolean);
var
  IdeLibraryPath: TStringList;
  i: Integer;
  PlatformName: string;
begin
  Assert(Assigned(Paths));
  Paths.Clear;
  GxOtaGetProjectSourcePathStrings(Paths, Project, DoProcessing);
  IdeLibraryPath := TStringList.Create;
  try
    GxOtaGetIdeLibraryPathStrings(IdeLibraryPath, DoProcessing);
    if DoProcessing then begin
      PlatformName := GxOtaGetProjectPlatform(Project);
      ProcessPaths(IdeLibraryPath, GetCurrentDir, PlatformName);
    end;
    for i := 0 to IdeLibraryPath.Count - 1 do begin
      EnsureStringInList(Paths, IdeLibraryPath[i]);
    end;
  finally
    FreeAndNil(IdeLibraryPath);
  end;
end;

procedure GxOtaGetAllPossiblePaths(Paths: TStrings);

  function GetPath(const FileName: string): string;
  begin
    Result := RemoveSlash(ExtractFilePath(FileName));
  end;

  procedure AddAPath(const APath: string);
  begin
    if DirectoryExists(APath) then
      EnsureStringInList(Paths, APath);
  end;

  procedure AddVCLPaths;
  var
    BasePath: string;
    DirFinder: TFileFindThread;
    i: Integer;
  begin
    BasePath := AddSlash(GetIdeRootDirectory) + 'Source' + PathDelim;
    DirFinder := TFileFindThread.Create;
    try
      DirFinder.FileMasks.Add(AllFilesWildCard);
      DirFinder.RecursiveSearchDirs.Add(BasePath);
      DirFinder.DirectoriesOnly := True;
      // TODO: Cache the IDE source dir list between calls
      DirFinder.StartFind;
      Sleep(1);
      while not DirFinder.Complete do
        Sleep(1);
      DirFinder.LockResults;
      try
        for i := 0 to DirFinder.Results.Count - 1 do
          AddAPath(DirFinder.Results[i])
      finally
        DirFinder.Terminate;
        DirFinder.ReleaseResults;
      end;
    finally
      FreeAndNil(DirFinder);
    end;
  end;

{var
  UnitList: TList;
  UnitInfo: TUnitInfo;
  i: Integer;}
begin
  Assert(Assigned(Paths));
  // Add path of the current project
  AddAPath(GetPath(GxOtaGetCurrentProjectFileName));
  // Add library search paths
  GxOtaGetEffectiveLibraryPath(Paths);
  // Not really needed, so the following was left out (for now)
  {// Add paths of all files included in the project
  UnitList := TList.Create;
  try
    GxOtaFillUnitInfoListForCurrentProject(UnitList);
    for i := 0 to UnitList.Count - 1 do
    begin
      UnitInfo := TUnitInfo(UnitList[i]);
      AddAPath(GetPath(UnitInfo.FileName));
    end;
  finally
    FreeAndNil(UnitList);
  end;
  // Add current file path
  AddAPath(GetPath(GxOtaGetFileNameOfCurrentModule));
  // Add path of the current source file (probably same as first one)
  AddAPath(GetPath(GxOtaGetCurrentSourceFile));
  // Add path of the project group
  AddAPath(GetPath(GxOtaGetProjectGroupFileName));}
  // Add paths to VCL source (since we are smart)
  AddVCLPaths;
end;

function GxOtaFindPathToFile(const FileName: string; Paths: TStrings): string;

  function MakeFilename(const Path, FileName: string): string;
  begin
    if Path = '' then
      Result := FileName
    else if Path[Length(Path)] = PathDelim then
      Result := Path + FileName
    else
      Result := Path + PathDelim + FileName;
  end;

var
  PathList: TStringList;
  i: Integer;
  NewFileName: string;
begin
  Result := FileName;
  if not (IsPathAbsolute(Result) and FileExists(Result)) then
  begin
    PathList := TStringList.Create;
    try
      if Assigned(Paths) then
        PathList.Assign(Paths)
      else
        GxOtaGetAllPossiblePaths(PathList);
      for i := 0 to PathList.Count - 1 do
      begin
        NewFileName := MakeFilename(PathList[i], FileName);
        if FileExists(NewFileName) then
        begin
          Result := NewFileName;
          Break;
        end;
      end;
    finally
      FreeAndNil(PathList);
    end;
  end;
end;

{$ENDIF GX_UseNativeToolsApi}

end.
