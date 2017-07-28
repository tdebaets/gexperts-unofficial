unit GX_GenericUtils;

// This file contains a subset of the code in GX_GenericUtils.pas from the
// latest GExperts source code. Additionally, some standard Delphi functions not
// present yet in Delphi 4 (e.g. FreeAndNil) were also added.

{$I GX_CondDefine.inc}

interface

uses SysUtils, Classes, Windows, SyncObjs;

const
  AllFilesWildCard = '*.*';

procedure FreeAndNil(var Obj);

// Ensure a string is in a list
procedure EnsureStringInList(List: TStrings; const Item: string);

const
  PathDelim = '\';

function IncludeTrailingPathDelimiter(const S: string): string;
function ExcludeTrailingPathDelimiter(const S: string): string;

// Adds a correct trailing slash to Directory if it is not present yet
function AddSlash(const Directory: string): string;

// Removes the trailing slash from Directory if it is present
function RemoveSlash(const Directory: string): string;

// Determine if a passed in path/file is absolute or relative
function IsPathAbsolute(const FileName: string): Boolean;

function LeftStr(InValue: String; Len: Integer): String;
function RightStr(InValue: String; Len: Integer): String;

// Return a complete file name with a path given a directory/file name
function BuildFileName(const Path, FileName: string): string;

procedure GetEnvironmentVariables(Strings: TStrings);

type
  TFileFindThread = class(TThread)
  private
    FFileMasks: TStringList;
    FResults: TStringList;
    FSearchDirs: TStringList;
    FRecursiveSearchDirs: TStringList;
    FFindComplete: TThreadMethod;
    FResultsLock: TCriticalSection;
    FDirectoriesOnly: Boolean;
    FComplete: Boolean;
    FDirsToIgnore: TStringList;
    procedure SetDirsToIgnore(const Value: TStringList);
  protected
    procedure Execute; override;
    procedure FindFilesInDir(const Dir: string; Recursive: Boolean);
    procedure AddResult(const FileName: string); virtual;
  public
    property DirsToIgnore: TStringList read FDirsToIgnore write SetDirsToIgnore;
    property Complete: Boolean read FComplete;
    property FileMasks: TStringList read FFileMasks;
    property SearchDirs: TStringList read FSearchDirs;
    property DirectoriesOnly: Boolean read FDirectoriesOnly write FDirectoriesOnly;
    property RecursiveSearchDirs: TStringList read FRecursiveSearchDirs;
    property OnFindComplete: TThreadMethod read FFindComplete write FFindComplete;
    property Results: TStringList read FResults;
    constructor Create;
    destructor Destroy; override;
    procedure AddDelphiDirsToIgnore;
    procedure AddSCMDirsToIgnore;
    procedure StartFind;
    procedure LockResults;
    procedure ReleaseResults;
  end;

implementation

procedure FreeAndNil(var Obj);
var
  P: TObject;
begin
  P := TObject(Obj);
  TObject(Obj) := nil;  // clear the reference before destroying the object
  P.Free;
end;

procedure EnsureStringInList(List: TStrings; const Item: string);
var
  Index: Integer;
begin
  Assert(Assigned(List));
  Index := List.IndexOf(Item);
  if Index = -1 then
    List.Add(Item);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if Length(S)>0 then begin
    if S[Length(S)] <> PathDelim then begin
      Result := S + PathDelim;
    end;
  end;
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if (S <> '') and (S[Length(S)] in ['/', '\']) then
    SetLength(Result, Length(Result) - 1);
end;

function AddSlash(const Directory: string): string;
begin
  Result := IncludeTrailingPathDelimiter(Directory);
end;

function RemoveSlash(const Directory: string): string;
begin
  Result := ExcludeTrailingPathDelimiter(Directory);
end;

function IsPathAbsolute(const FileName: string): Boolean;
begin
  Result := ExtractFileDrive(FileName) <> '';
end;

function LeftStr(InValue: String; Len: Integer): String;
begin
  Result := Copy(InValue, 1, Len);
end;

function RightStr(InValue: String; Len: Integer): String;
begin
  Result := Copy(InValue, Length(InValue)-(Len - 1), Len);
end;

function BuildFileName(const Path, FileName: string): string;
var
  FileNameHasDelimiter: Boolean;
  PathHasDelimiter: Boolean;
begin
  if Trim(Path) <> '' then
  begin
    PathHasDelimiter := RightStr(Path, 1) = PathDelim;
    FileNameHasDelimiter := LeftStr(FileName, 1) = PathDelim;
    if PathHasDelimiter and FileNameHasDelimiter then
      Result := RemoveSlash(Path) + FileName
    else if PathHasDelimiter or FileNameHasDelimiter then
      Result := Path + FileName
    else // neither
      Result := AddSlash(Path) + FileName;
  end
  else
    Result := FileName;
end;

procedure GetEnvironmentVariables(Strings: TStrings);
var
  EnvStart: Pointer;
  EnvPos: PChar;
begin
  Assert(Assigned(Strings));
  Strings.Clear;
  EnvStart := GetEnvironmentStrings;
  try
    EnvPos := EnvStart;
    while StrLen(EnvPos) > 0 do
    begin
      Strings.Add(EnvPos);
      EnvPos := StrEnd(EnvPos) + 1;
    end;
  finally
    FreeEnvironmentStrings(EnvStart);
  end;
end;

{ TFileFindThread }

procedure TFileFindThread.AddDelphiDirsToIgnore;
begin
  FDirsToIgnore.Add('__history');
  FDirsToIgnore.Add('__recovery');
end;

procedure TFileFindThread.AddResult(const FileName: string);
begin
  LockResults;
  try
    FResults.Add(FileName);
  finally
    ReleaseResults;
  end;
end;

procedure TFileFindThread.AddSCMDirsToIgnore;
begin
  FDirsToIgnore.Add('.svn');
  FDirsToIgnore.Add('.hg');
  FDirsToIgnore.Add('.git');
end;

constructor TFileFindThread.Create;
begin
  inherited Create(True);
  FFileMasks := TStringList.Create;
  FResults := TStringList.Create;
  FResultsLock := TCriticalSection.Create;
  FSearchDirs := TStringList.Create;
  FRecursiveSearchDirs := TStringList.Create;
  FResults.Duplicates := dupIgnore;
  FResults.Sorted := True;
  FDirsToIgnore := TStringList.Create;
  FDirsToIgnore.Duplicates := dupIgnore;
  FDirsToIgnore.Sorted := True;
  FDirsToIgnore.Add('.');
  FDirsToIgnore.Add('..');
end;

destructor TFileFindThread.Destroy;
begin
  FreeAndNil(FDirsToIgnore);
  FreeAndNil(FFileMasks);
  FreeAndNil(FResults);
  FreeAndNil(FResultsLock);
  FreeAndNil(FSearchDirs);
  FreeAndNil(FRecursiveSearchDirs);
  inherited;
end;

procedure TFileFindThread.Execute;
var
  i: Integer;
begin
  FComplete := False;
  try
    LockResults;
    try
      FResults.Clear;
    finally
      ReleaseResults;
    end;
    for i := 0 to FSearchDirs.Count - 1 do
    begin
      FSearchDirs[i] := AddSlash(FSearchDirs[i]);
      FindFilesInDir(FSearchDirs[i], False);
      if Terminated then
        Exit;
    end;
    if Terminated then
      Exit;
    for i := 0 to FRecursiveSearchDirs.Count - 1 do
    begin
      FRecursiveSearchDirs[i] := AddSlash(FRecursiveSearchDirs[i]);
      FindFilesInDir(FRecursiveSearchDirs[i], True);
      if Terminated then
        Exit;
    end;
    if Terminated then
      Exit;
    if Assigned(FFindComplete) then
      Synchronize(FFindComplete);
    FComplete := True;
  except
    on E: Exception do
      MessageBox(0, PChar(E.Message), 'File Search Thread', MB_OK + MB_ICONERROR + MB_APPLMODAL);
  end;
end;

procedure TFileFindThread.FindFilesInDir(const Dir: string; Recursive: Boolean);
var
  SearchRec: TSearchRec;
  i: Integer;
  Idx: Integer;
begin
  if Recursive then
  begin
    if FindFirst(Dir + AllFilesWildCard, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if not FDirsToIgnore.Find(SearchRec.Name, Idx) then
            FindFilesInDir(AddSlash(BuildFileName(Dir, SearchRec.Name)), Recursive);
        end;
      until (FindNext(SearchRec) <> 0) or Terminated;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;

  if Terminated then
    Exit;

  Assert(Assigned(FFileMasks));
  for i := 0 to FFileMasks.Count - 1 do
  begin
    if FindFirst(Dir + FileMasks[i], faAnyFile, SearchRec) = 0 then
    try
      repeat
        if DirectoriesOnly then
        begin
          if ((SearchRec.Attr and faDirectory) <> 0) and not FDirsToIgnore.Find(SearchRec.Name, Idx) then
            AddResult(BuildFileName(Dir, SearchRec.Name));
        end
        else
          AddResult(BuildFileName(Dir, SearchRec.Name));
      until (FindNext(SearchRec) <> 0) or Terminated;
    finally
      SysUtils.FindClose(SearchRec);
    end;
  end;
end;

procedure TFileFindThread.LockResults;
begin
  FResultsLock.Acquire;
end;

procedure TFileFindThread.ReleaseResults;
begin
  FResultsLock.Release;
end;

procedure TFileFindThread.SetDirsToIgnore(const Value: TStringList);
begin
  FDirsToIgnore.Assign(Value);
  FDirsToIgnore.Add('.');
  FDirsToIgnore.Add('..');
end;

procedure TFileFindThread.StartFind;
begin
  {$IFDEF GX_VER210_up}
  Start;
  {$ELSE}
  Resume;
  {$ENDIF}
end;

end.
