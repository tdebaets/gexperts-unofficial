/// <summary>
/// implements utility functions for file accesss
/// This is an extract from u_dzFileUtils in dzlib http://blog.dummzeuch.de/dzlib/ </summary>
unit GX_dzFileUtils;

{$I GX_CondDefine.inc}

{$IFDEF GX_VER200_up}
{$DEFINE SUPPORTS_UNICODE_STRING}
{$ENDIF}

interface

uses
  Windows,
  SysUtils,
  Classes;

const
  /// <summary>
  /// set of char constant containing all characters that are invalid in a filename </summary>
  INVALID_FILENAME_CHARS: set of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
type
  TFileSystem = class
  public
    class function ExpandFileNameRelBaseDir(const _FileName, _BaseDir: string): string;
    ///<summary>
    /// Replaces all invalid characters in the file name with the given character </summary>
    /// @param S is the input filename
    /// @param ReplaceChar is the character to use for replacing in valid characters
    ///                    defaults to '_'
    /// @param AllowPathChars determines whether the name may contain '\' characters and a single
    ///                       ':' as the second character, so a full path can be converted.
    /// @returns a valid filename </summary>
    class function MakeValidFilename(const _s: string; _ReplaceChar: Char = '_'; _AllowPathChars: Boolean = True): string;
  end;

type
  TFileAttributes = (
    dfaReadonly,
    dfaHidden, // Hidden files
    dfaSysFile, //	System files
    dfaDirectory, //	Directory files
    dfaArchive // Archive files
    );

  TFileAttributeSet = set of TFileAttributes;

const
  ALL_FILES_ATTRIB_SET = [dfaHidden, dfaSysFile, dfaDirectory, dfaArchive];

type
  TFileInfoRec = record
    Filename: string;
    Size: Int64;
    Timestamp: TDateTime;
  end;

type
  /// <summary>
  /// a simple wrapper around FindFirst/FindNext which allows to search for
  /// specified attributes only (e.g. only directories), it automatically
  /// ignores the special '.' and '..' directories. </summary>
  TSimpleDirEnumerator = class
  protected
    /// stores the search mask ('c:\windows\*.exe')
    FMask: string;
    /// set of attributes a file must match
    FMustHaveAttr: TFileAttributeSet;
    /// set of attributes a file may have
    FMayHaveAttr: TFileAttributeSet;
    /// internally used TSearchRec structure
    FSr: TSearchRec;
    /// true if FindFirst was called and returned no error code
    FActive: Boolean;
    /// number of matching files found
    FMatchCount: Integer;
  public
    /// <summary>
    /// Creates a TSimpleDirEnumerator, sets the Mask, MustHaveAttr and MayHaveAttr
    /// properties.
    /// MustHaveAttr is set to [] and MayHaveAttr is set to include all possible
    /// attributes, so calling FindNext will find any files or subdirectories,
    /// but the special '.' and '..' directories
    /// @param Mask is the file search mask and should include a path </summary>
    constructor Create(const _Mask: string; _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET);
    /// <summary>
    /// Destructor, will call FindClose if necessary </summary>
    destructor Destroy; override;
    /// <summary>
    /// creates a TSimpleDirEnumerator, calls its FindAll method and frees it
    /// @param IncludePath determines whether the List of filenames includes the full path or not </summary>
    class function Execute(const _Mask: string; _List: TStrings;
      _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET; _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
    class function EnumFilesOnly(const _Mask: string; _List: TStrings;
      _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
    class function EnumDirsOnly(const _Mask: string; _List: TStrings;
      _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
    /// <summary>
    /// Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
    /// calls.
    /// @param Filename is the name of the file found, if result is true, if you need
    ///       more information about it, use the SR property, note that it
    ///       does not include the path
    /// @param IncludePath determines whether the List of filenames includes the full path or not
    ///                    defaults to false
    /// @Returns true, if a matching file was found, false otherwise </summary>
    function FindNext(out _FileName: string; _IncludePath: Boolean = False): Boolean; overload;
    /// <summary>
    /// Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
    /// calls. If it returns true, use the SR property to get information about
    /// the file. See the overloaded @link(FindNext) version if you need only
    /// the filename.
    /// @Returns true, if a matching file was found, false otherwise </summary>
    function FindNext: Boolean; overload;
    /// <summary>
    /// Calls FindNext until it returns false, stores all filenames in List and
    /// returns the number of files found.
    /// @param List is a TStrings object which will be filled with the filenames
    ///        of matching files, may be nil.
    /// @param IncludePath determines whether the List of filenames includes the full path or not
    /// @returns the number of matching files </summary>
    function FindAll(_List: TStrings = nil; _IncludePath: Boolean = False): Integer;
    /// <summary>
    /// Calls FindClose so FindNext will start again. Reset does not change any
    /// properties (e.g. Mask, MustHaveAttr, MayHaveAttr) </summary>
    procedure Reset;
    /// <summary>
    /// Returns the number of matches so far, that is the number of successful
    /// calls to FindNext. </summary>
    property MatchCount: Integer read FMatchCount;
    /// <summary>
    /// Returns the search mask </summary>
    property Mask: string read FMask; // write fMask;
    /// <summary>
    /// the set of attributes a file must have to be found by FindNext </summary>
    property MustHaveAttr: TFileAttributeSet read FMustHaveAttr write FMustHaveAttr;
    /// <summary>
    /// the set of allowed attributes for a file to be found by FindNext </summary>
    property MayHaveAttr: TFileAttributeSet read FMayHaveAttr write FMayHaveAttr;
    /// <summary>
    /// the search rec containing additional information about the file </summary>
    property Sr: TSearchRec read FSr;
  end;

implementation

uses GX_GenericUtils;

{ TSimpleDirEnumerator }

constructor TSimpleDirEnumerator.Create(const _Mask: string;
  _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET);
begin
  FMask := _Mask;
  FMustHaveAttr := [];
  FMayHaveAttr := _MayHaveAttr;
end;

destructor TSimpleDirEnumerator.Destroy;
begin
  Reset;
  inherited;
end;

class function TSimpleDirEnumerator.EnumDirsOnly(const _Mask: string; _List: TStrings;
  _IncludePath, _Sort: Boolean): Integer;
var
  enum: TSimpleDirEnumerator;
  List: TStringList;
begin
  enum := TSimpleDirEnumerator.Create(_Mask, [dfaDirectory, dfaArchive]);
  try
    enum.MustHaveAttr := [dfaDirectory];
    List := TStringList.Create;
    try
      Result := enum.FindAll(List, _IncludePath);
      if _Sort then
        List.Sort;
      _List.AddStrings(List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(enum);
  end;
end;

class function TSimpleDirEnumerator.EnumFilesOnly(const _Mask: string; _List: TStrings;
  _IncludePath, _Sort: Boolean): Integer;
begin
  Result := Execute(_Mask, _List, [dfaArchive], _IncludePath, _Sort);
end;

class function TSimpleDirEnumerator.Execute(const _Mask: string; _List: TStrings;
  _MayHaveAttr: TFileAttributeSet = ALL_FILES_ATTRIB_SET;
  _IncludePath: Boolean = False; _Sort: Boolean = True): Integer;
var
  enum: TSimpleDirEnumerator;
  List: TStringList;
begin
  enum := TSimpleDirEnumerator.Create(_Mask, _MayHaveAttr);
  try
    List := TStringList.Create;
    try
      Result := enum.FindAll(List, _IncludePath);
      if _Sort then
        List.Sort;
      _List.AddStrings(List);
    finally
      FreeAndNil(List);
    end;
  finally
    FreeAndNil(enum);
  end;
end;

function TSimpleDirEnumerator.FindAll(_List: TStrings = nil; _IncludePath: Boolean = False): Integer;
var
  s: string;
  Path: string;
begin
  if _IncludePath then
    Path := ExtractFilePath(FMask)
  else
    Path := '';
  Result := 0;
  while FindNext(s) do begin
    Inc(Result);
    if Assigned(_List) then
      _List.Add(Path + s);
  end;
end;

function TSimpleDirEnumerator.FindNext(out _FileName: string; _IncludePath: Boolean = False): Boolean;
var
  Res: Integer;
  Attr: Integer;

  function AttrOk(_EnumAttr: TFileAttributes; _SysAttr: Integer): Boolean;
  begin
    Result := True;
    if _EnumAttr in FMustHaveAttr then
      if (Attr and _SysAttr) = 0 then
        Result := False;
  end;

  procedure CondAddAttr(_EnumAttr: TFileAttributes; _SysAttr: Integer);
  begin
    if _EnumAttr in FMayHaveAttr then
      Attr := Attr + _SysAttr;
  end;

var
  Path: string;
begin
  Path := ExtractFilePath(FMask);
  repeat
    if not FActive then begin
      FMatchCount := 0;
      Attr := 0;
      CondAddAttr(dfaReadonly, SysUtils.faReadOnly);
      CondAddAttr(dfaHidden, SysUtils.faHidden);
      CondAddAttr(dfaSysFile, SysUtils.faSysFile);
      CondAddAttr(dfaDirectory, SysUtils.faDirectory);
      CondAddAttr(dfaArchive, SysUtils.faArchive);
      Res := FindFirst(FMask, Attr, FSr);
      Result := (Res = 0);
      if Result then
        FActive := True;
    end else begin
      Res := SysUtils.FindNext(FSr);
      Result := (Res = 0);
    end;
    if not Result then
      Exit;
    if (Sr.Name = '.') or (Sr.Name = '..') then
      Continue;
    if FMustHaveAttr <> [] then begin
      Attr := FSr.Attr;
      if not AttrOk(dfaReadonly, SysUtils.faReadOnly) then
        Continue;
      if not AttrOk(dfaHidden, SysUtils.faHidden) then
        Continue;
      if not AttrOk(dfaSysFile, SysUtils.faSysFile) then
        Continue;
      if not AttrOk(dfaDirectory, SysUtils.faDirectory) then
        Continue;
      if not AttrOk(dfaArchive, SysUtils.faArchive) then
        Continue;
    end;
    Inc(FMatchCount);
    if _IncludePath then
      _FileName := Path + Sr.Name
    else
      _FileName := Sr.Name;
    Exit;
  until False;
end;

function TSimpleDirEnumerator.FindNext: Boolean;
var
  s: string;
begin
  Result := FindNext(s);
end;

procedure TSimpleDirEnumerator.Reset;
begin
  if FActive then
    FindClose(FSr);
  FActive := False;
end;

{ TFileSystem }

const
  shlwapi32 = 'shlwapi.dll';

function PathIsRelative(pszPath: PChar): BOOL; stdcall; external shlwapi32
{$IFDEF GX_VER200_up}
name 'PathIsRelativeW';
{$ELSE}
name 'PathIsRelativeA';
{$ENDIF}

function PathCanonicalize(pszBuf: PChar; pszPath: PChar): BOOL; stdcall; external shlwapi32
{$IFDEF GX_VER200_up}
name 'PathCanonicalizeW';
{$ELSE}
name 'PathCanonicalizeA';
{$ENDIF}

// taken from
// http://stackoverflow.com/a/5330691/49925

class function TFileSystem.ExpandFileNameRelBaseDir(const _FileName, _BaseDir: string): string;
var
  Buffer: array[0..MAX_PATH - 1] of Char;
begin
  if PathIsRelative(PChar(_FileName)) then begin
    Result := IncludeTrailingPathDelimiter(_BaseDir) + _FileName;
  end else begin
    Result := _FileName;
  end;
  if PathCanonicalize(@Buffer[0], PChar(Result)) then begin
    Result := Buffer;
  end;
end;

{$IFNDEF SUPPORTS_UNICODE_STRING}

function CharInSet(_c: Char; const _CharSet: TSysCharSet): Boolean;
begin
  Result := _c in _CharSet;
end;

{$ENDIF ~SUPPORTS_UNICODE_STRING}

class function TFileSystem.MakeValidFilename(const _s: string; _ReplaceChar: Char = '_';
  _AllowPathChars: Boolean = True): string;
var
  i: Integer;
  InvalidChars: set of AnsiChar;
begin
  Result := _s;
  InvalidChars := INVALID_FILENAME_CHARS;
  if _AllowPathChars then
    InvalidChars := InvalidChars - ['\'];
  for i := 1 to Length(Result) do begin
    if CharInSet(Result[i], InvalidChars) then begin
      if not _AllowPathChars or (i <> 2) or (Result[2] <> ':') or not CharInSet(UpCase(Result[1]), ['A'..'Z']) then
        Result[i] := _ReplaceChar;
    end;
  end;
end;

end.
