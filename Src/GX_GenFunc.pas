unit GX_GenFunc;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

// This allows C++Builder 4 to pass IOTA interfaces correctly
{$IFDEF VER125}
(*$HPPEMIT 'namespace Toolsapi {'*)
(*$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IOTAEditReader);'*)
(*$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IOTASourceEditor);'*)
(*$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IOTAProject);'*)
(*$HPPEMIT '  DECLARE_DINTERFACE_TYPE(IOTAProjectGroup);'*)
(*$HPPEMIT '};'*)
{$ENDIF VER125}

interface

uses
  SysUtils, Forms, Classes, Registry, Graphics,
  {$IFNDEF GX_BCB}
  {$IFNDEF GX_VER140_up}
  LibIntf,  // If you get errors here, please read SourceCode.txt
            // See GX_GrepResults for a way to use LibIntf in BCB
  {$ENDIF}
  {$ENDIF GX_BCB}
  {$IFDEF DELPHIDEBUGGINGPACKAGE}
  Debugger,
  {$ENDIF DELPHIDEBUGGINGPACKAGE}
  {$IFDEF SYNTAXMEMO}
  SyntaxEd, SynParse, // If you get errors here, edit GX_CondDefine.inc
  {$ENDIF SYNTAXMEMO}
  {$IFDEF MWEDIT}
  // If you get errors here, edit GX_CondDefine.inc
  mwCustomEdit, mwHighLighter, mwPasSyn, DcjCppSyn, hkhtmlsyn, wmSQLsyn, mwGeneralSyn,
  {$ENDIF MWEDIT}
  EditIntf, ToolIntf, ExptIntf,
  {$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
  {$ENDIF GX_UseNativeToolsApi}
  Controls, Windows, Dialogs, ComCtrls;

{$IFDEF VER100}
const // Not defined in Delphi 3
  TVS_NOTOOLTIPS = $80;
{$ENDIF VER100}

const // Not defined in Delphi 4
  WM_MOUSEHWHEEL = $20E;

const // Not defined in Delphi 4
  SPI_GETWHEELSCROLLCHARS = $6C;
  
const
  DEFAULT_WHEELSCROLLCHARS = 3;
  DEFAULT_WHEELSCROLLLINES = 3;

type
  TUnitInfo = class(TObject)
  public
    UnitName: string;
    FormName: string;
    FileName: string;
  end;

  TCompInfo = class(TObject)
  public
    CompName: string;
    CompType: string;
  end;

{$IFNDEF GX_BCB}
procedure SelectComponent(Name: string);
{$ENDIF GX_BCB}
procedure GetCompInfoList(List: TStringList);
procedure GetUnitInfoList(List: TStringList);
//procedure OutputComponentList(Form: TForm);
function GetFileSize(const FileName: string): Integer;
function Min(v1, v2: Integer): Integer;
function Max(v1, v2: Integer): Integer;
function CaseInsensitivePos(const SubString, S: string): Integer;
function GetFileName(FileName: string): string;
procedure CenterForm(Form: TCustomForm);
function StripWhiteSpace(const S: string): string;
function AddSlash(const Directory: string): string;
function IsStandAlone: Boolean;
function GXShellExecute(FName, FParam: string; RaiseException: Boolean): Boolean;

type
  TGXSyntaxParser = (gxpPlaceHolder, gxpNone, gxpPAS, gxpCPP, gxpHTML, gxpSQL);
{$IFDEF SYNTAXMEMO}
  GXSyntaxParsers = set of TGXSyntaxParser;

function GetSyntaxParser(SyntaxParser: TGXSyntaxParser): TSyntaxMemoParser;

{$R Parsers.res}
{$ENDIF SYNTAXMEMO}

{$IFDEF MWEDIT}
procedure SetmwEditHighLighter(mwEdit: TmwCustomEdit; HighLighter: TGXSyntaxParser);
{$ENDIF MWEDIT}

{$IFNDEF ACEREPORTER}
procedure MessageNoAceReporter;
{$ENDIF ACEREPORTER}

function GetPropAsString(CompIntf: TIComponentInterface; Index: Integer): string;
function GetPropIndex(CompIntf: TIComponentInterface; const Name: string): Integer;
procedure SetPropAsString(CompIntf: TIComponentInterface; Index: Integer; const Value: string);
function GetProjectDir: string;
function GetProjectName: string;
{$IFDEF GX_UseNativeToolsApi}
//function GetCurrentProject: IOTAProject;
function GetProjectGroup: IOTAProjectGroup;
function GetProjectGroupFileName: string;
{$ENDIF GX_UseNativeToolsApi}

//function IsCodeRushInstalled: Boolean;
function ExtractPureFilename(const FileName: string): string;
function GetDelphiBackgroundColor: TColor;
function GxGetIdeBaseRegistryKey: string;
function GxGetIdeInstallationDirectory: string;
function GetDir(ParentForm: TForm; var Dir: string): Boolean;
procedure SaveFont(RegIni: TRegIniFile; const Key: string; Font: TFont);
procedure LoadFont(RegIni: TRegIniFile; const Key: string; Font: TFont);
procedure ReadStringList(RegIni: TRegIniFile; List: TStrings; const Key, SubKey: string);
procedure WriteStringList(RegIni: TRegIniFile; List: TStrings; const Key, Subkey: string);
//procedure AssertIsDprOrPas(const FileName: string);
function IsDprOrPas(const FileName: string): Boolean;
function IsInc(const FileName: string): Boolean;
function IsDfm(const FileName: string): Boolean;
function IsPas(const FileName: string): Boolean;
function IsDpr(const FileName: string): Boolean;
function IsCpp(const FileName: string): Boolean;
function IsH(const FileName: string): Boolean;
function IsCppSourceModule(const FileName: string): Boolean;
function IsKnownSourceFile(const FileName: string): Boolean;
function IsCppBuilderIde: Boolean;
function ExtractUpperFileExt(const FileName: string): string;
{$IFOPT D+}
function BooleanText(B: Boolean): string;
{$ENDIF D+}
function DLLName: string;
function GetPathFromHandle(hObject: THandle; var Path: WideString): Boolean;
function GetCurrentEditWindowNumber(AFilename: string; var WindowCaption: string): Integer;
function GetCurrentEditView(EditorIntf: TIEditorInterface): Integer;

{$IFDEF GX_UseNativeToolsApi}
procedure SaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
function GetCurrentSourceEditor: IOTASourceEditor;
function GetGXHighLighterForCurrentSourceEditor: TGXSyntaxParser;
function GetFileNameOfCurrentModule: string;
function ReadSourceEditorBlockSelection(Stream: TStream): Boolean;
{$ELSE GX_UseNativeToolsApi}
function GetGXHighLighterForCurrentSourceEditor: TGXSyntaxParser;
function GetFileNameOfCurrentModule: string;
function ReadSourceEditorBlockSelection(Stream: TStream): Boolean;
{$ENDIF GX_UseNativeToolsApi}


function BuiltWithPackages: Boolean;
procedure ShowNoPackagesError;
//procedure PackTable(Table: TTable);
function GetIdeMainForm: TCustomForm;
function GetComponentPaletteTabControl: TTabControl;
procedure FocusEditWindow(NumWindows: Integer; AFilename: string);

// Shows the exception error message
procedure ShowExceptionErrorMessage(E: Exception);
// Use this function for silent processing
procedure ProcessException(E: Exception);
// This function will report the exception with the message (and E.Message)
procedure ProcessExceptionMsg(E: Exception; const Msg: string);

implementation

uses
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  DdeMan, BrowseDr, ShellApi, NativeApi, // Errors here mean \GXSource\Comps is not on your library path
  {$IFNDEF GX_NOBDE}
  DB, DBTables, BDE,
  {$ENDIF GX_NOBDE}
  GX_GExperts, GX_VerDepConst, GX_ConfigurationInfo, GX_EditReader;

const
  Unknown = 'Unknown Expert,Unknown,0'; // do not localize
  {$IFDEF SYNTAXMEMO}
var
  Parsers: array[TGXSyntaxParser] of TSyntaxMemoParser;
  {$ENDIF SYNTAXMEMO}

procedure ReadStringList(RegIni: TRegIniFile; List: TStrings; const Key, SubKey: string);
var
  i: Integer;
  ListString: string;
begin
  for i := 0 to RegIni.ReadInteger(Key, 'Count', 0) - 1 do // do not localize
  begin
    ListString := RegIni.ReadString(Key, SubKey + IntToStr(i), Unknown);
    if ListString <> Unknown then
      List.Add(ListString);
  end;
end;

procedure WriteStringList(RegIni: TRegIniFile; List: TStrings; const Key, SubKey: string);
var
  i: Integer;
  OldCount: Integer;
  DelReg: TRegistry;
begin
  with RegIni do
  begin
    OldCount := ReadInteger(Key, 'Count', 0);
    for i := 0 to List.Count - 1 do
      WriteString(Key, SubKey + IntToStr(i), List.Strings[i]);
    WriteInteger(Key, 'Count', List.Count); // do not localize

    // Delete any old entries when OldCount > List.Count
    if OldCount > List.Count then
    begin
      DelReg := TRegistry.Create;
      try
        if DelReg.OpenKey(RegIni.CurrentPath+'\'+Key, False) then
          for i := List.Count to OldCount - 1 do
            DelReg.DeleteValue(SubKey+ IntToStr(i));
      finally
        DelReg.Free;
      end;
    end;
  end;
end;

function GetDir(ParentForm: TForm; var Dir: string): Boolean;
resourcestring
  SSelDir = 'Select a directory';
var
  Dlg: TBrowseDirectoryDlg;
begin
  Dlg := TBrowseDirectoryDlg.Create(nil);
  try
    with Dlg do
    begin
      Parent := ParentForm;
      Root := idDesktopExpanded;
      Selection := Dir;
      Title := SSelDir;

      // Windows NT at least with IE 4.0 has problems
      // when demanding that the status bar be shown.
      // Simply turn off the status bar if that happens

      // If you get an error here, you must upgrade to Delphi 3.02 before compiling!
      if (Win32Platform <> VER_PLATFORM_WIN32_NT) and (GetComCtlVersion > ComCtlVersionIE4) then
      begin
        Options := Options + [bfDirectoriesOnly, bfEditBox];
        ShowSelectionInStatus := True;
        StatusText := SSelDir + '...'; // do not localize
      end
      else
      begin
        Options := Options + [bfDirectoriesOnly];
        ShowSelectionInStatus := False;
      end;

      Center := True;
      Caption := SSelDir;
      Result := Execute;
      if Result then
        Dir := Dlg.Selection;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure SaveFont(RegIni: TRegIniFile; const Key: string; Font: TFont);
begin
  with RegIni do
  begin
    // Do not localize any of the following strings
    WriteString(Key, 'Name', Font.Name);
    WriteInteger(key, 'Size', Font.Size);
    WriteBool(Key, 'Bold', (fsBold in Font.Style));
    WriteBool(Key, 'Italic', (fsItalic in Font.Style));
    WriteBool(Key, 'Underline', (fsUnderline in Font.Style));
  end;
end;

procedure LoadFont(RegIni: TRegIniFile; const Key: string; Font: TFont);
begin
  with RegIni do
  begin
    // Do not localize any of the following strings
    Font.Name := ReadString(Key, 'Name', Font.Name);
    Font.Size := ReadInteger(key, 'Size', Font.Size);
    if ReadBool(Key, 'Bold', (fsBold in Font.Style)) then
      Font.Style := Font.Style + [fsBold];
    if ReadBool(Key, 'Italic', (fsItalic in Font.Style)) then
      Font.Style := Font.Style + [fsItalic];
    if ReadBool(Key, 'Underline', (fsUnderline in Font.Style)) then
      Font.Style := Font.Style + [fsUnderLine];
  end;
end;

{$IFNDEF ACEREPORTER}
procedure MessageNoAceReporter;
resourcestring
  SMessageNoAceReporter = 'Feature not available - GExperts has been compiled without ACE Reporter support.';
begin
  MessageDlg(SMessageNoAceReporter, mtInformation, [mbOK], 0);
end;
{$ENDIF ACEREPORTER}


function GetDelphiBackgroundColor: TColor;
const
  {$IFNDEF GX_VER110_up}
  RegAddKey = '';
  {$ELSE}
  RegAddKey = '\Editor\Highlight'; // do not localize
  {$ENDIF GX_VER110_up}
var
  RegIniFile: TRegIniFile;
  {$IFNDEF GX_VER110_up}
  StringSetting: string;
  CommaPos: Integer;
  {$ENDIF GX_VER110_up}
begin
  Assert(ToolServices <> nil);
  RegIniFile := TRegIniFile.Create(ToolServices.GetBaseRegistryKey + RegAddKey);
  try
    try
      {$IFNDEF GX_VER110_up}
      StringSetting := RegIniFile.ReadString('Highlight', 'Whitespace', '16');
      // Extract numerical value for StringSetting
      CommaPos := Pos(',', StringSetting);
      if CommaPos > 0 then
      begin
        Delete(StringSetting, 1, CommaPos);
        CommaPos := Pos(',', StringSetting);
        if CommaPos > 0 then
          StringSetting := Copy(StringSetting, 1, CommaPos - 1);
      end;
      Result := TColor(StrToInt(StringSetting));
      {$ELSE}
      if RegIniFile.ReadInteger('Whitespace', 'Background Color', 15) = 0 then
        Result := clBlack
      else
        Result := TColor(RegIniFile.ReadInteger('Whitespace', 'Background Color', 15));
      {$ENDIF GX_VER110_up}
    except
      on E: Exception do
      begin
        Result := clWhite;
      end;
    end;
  finally
    RegIniFile.Free;
  end;
  {$IFOPT D+}SendDebug('Delphi Background Color is: ' + ColorToString(Result) + '  '); {$ENDIF}
end;

function GxGetIdeBaseRegistryKey: string;
begin
  if ToolServices = nil then
    Result := CompilerDefinedProductRegistryKey
  else
    Result := ToolServices.GetBaseRegistryKey;

  Assert(Length(Result) > 0, 'Bad IDE base registry key detected');

  // Windows 2000 is allergic to a leading backslash
  // in the registry key - NT4, for instance, is not.
  if Result[1] = '\' then
    Delete(Result, 1, 1);

  Assert(Result[Length(Result)] <> '\',  'Bad IDE base registry key detected');
end;

function GxGetIdeInstallationDirectory: string;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    // Under NT5+, we can't open HKLM in r/w mode (supported in D4+)
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    {$IFDEF GX_VER125_up}
    if Reg.OpenKeyReadOnly(GxGetIdeBaseRegistryKey) then
    {$ELSE not GX_VER125_up}
    if Reg.OpenKey(GxGetIdeBaseRegistryKey, False) then
    {$ENDIF not GX_VER125_up}
    begin
      Result := Reg.ReadString('RootDir'); // do not localize
      Assert(Length(Result) > 0, 'Bad IDE RootDir detected');
      if Result[Length(Result)] = '\' then
        Delete(Result, 1, 1)
    end
    else
      Result := '';
  finally
    Reg.Free;
  end;
end;

function ExtractPureFilename(const FileName: string): string;
var
  ExtPos: Integer;
begin
  Result := ExtractFileName(FileName);
  if Result = '' then Exit;
  ExtPos := CaseInsensitivePos(ExtractFileExt(Result), Result);
  if ExtPos > 0 then
    Delete(Result, ExtPos, Length(Result));
end;

function LoadUnitList(Param: Pointer; const FileName, Unitname,
  FormName: string): Boolean; stdcall;
var
  List: TStringList;
  UInfo: TUnitInfo;
begin
  Result := True;
  List := TStringList(Param);
  if UnitName <> '' then
  begin
    UInfo := TUnitInfo.Create;
    UInfo.UnitName := UnitName;
    UInfo.FileName := FileName;
    UInfo.FormName := FormName;
    List.AddObject(UnitName, UInfo);
  end;
end;

procedure GetUnitInfoList(List: TStringList);
begin
  ToolServices.EnumProjectUnits(LoadUnitList, List);
end;

procedure GetCompInfoList(List: TStringList);
var
  CompIntf, FormCompIntf: TIComponentInterface;
  ModIntf: TIModuleInterface;
  FormIntf: TIFormInterface;
  CompInfo: TCompInfo;
  Control: TControl;
  FileName: string;
  i: Integer;
begin
  ModIntf := nil;
  FormCompIntf := nil;
  FormIntf := nil;

  try
    // Get module interface
    FileName := UpperCase(ToolServices.GetCurrentFile);
    if not IsDfm(FileName) then
    begin
      ModIntf := ToolServices.GetModuleInterface(FileName);
    end
    else
    begin
      // Note we are using hard-coded file extensions here - not good.
      ModIntf := ToolServices.GetModuleInterface(ChangeFileExt(FileName, '.PAS'));

    {$IFDEF GX_BCB}
      if ModIntf = nil then
        ModIntf := ToolServices.GetModuleInterface(ChangeFileExt(FileName, '.CPP'));
    {$ENDIF GX_BCB}

      if ModIntf = nil then
        ModIntf := ToolServices.GetModuleInterface(FileName);
    end;

    if ModIntf = nil then
      Exit;

    FormIntf := ModIntf.GetFormInterface;
    if FormIntf = nil then
      Exit;

    FormCompIntf := FormIntf.GetFormComponent;
    if FormCompIntf <> nil then
      for i := 0 to FormCompIntf.GetComponentCount - 1 do
      begin
        CompIntf := FormCompIntf.GetComponent(i);
        try
          if CompIntF <> nil then
          begin
            CompInfo := TCompInfo.Create;
            Control := TControl(CompIntf.GetComponentHandle);
            CompInfo.CompName := Control.Name;
            CompInfo.CompType := Control.ClassName;
            List.AddObject(CompInfo.CompName, CompInfo);
          end;
        finally
          CompIntf.Free;
        end;
      end;
  finally
    if (FormCompIntf <> nil) and (FormCompIntf.GetComponentHandle <> nil) then
      FormCompIntf.Free;

    ModIntf.Free;
    FormIntf.Free;
  end;
end;

{$IFNDEF GX_BCB}
// This function is defined elsewhere for C++Builder
procedure SelectComponent(Name: string);
var
  CompIntf, FormCompIntf: TIComponentInterface;
  ModIntf: TIModuleInterface;
  FormIntf: TIFormInterface;
{$IFDEF GX_VER140_up}
{$ELSE not GX_VER140_up}
  Form: TIForm;
{$ENDIF}
  Control: TControl;
  UnitName, FileName, FileExt: string;
  i: Integer;

  procedure ShowForm;
  begin
    if ModIntf <> nil then
    begin
      ModIntf.ShowForm;
      if FormCompIntf <> nil then
        TForm(FormCompIntf.GetComponentHandle).SendToBack;
    end;
  end;

begin
  ModIntf := nil;
  FormCompIntf := nil;
  FormIntf := nil;

  FileName := UpperCase(ToolServices.GetCurrentFile);
  if IsDfm(FileName) then
  begin
    // Get module interface
    FileExt := ExtractFileExt(FileName);
    i := Pos(FileExt, FileName);
    if i > 0 then
      UnitName := Copy(FileName, 1, i) + 'PAS'
    else
      UnitName := FileName;
  end
  else
    UnitName := FileName;

  ModIntf := ToolServices.GetModuleInterface(UnitName);
  if ModIntf = nil then
    Exit;
  try
    FormIntf := ModIntf.GetFormInterface;
    if FormIntf = nil then
      Exit;

    FormCompIntf := FormIntf.GetFormComponent;
    if FormCompIntf <> nil then
      for i := 0 to FormCompIntf.GetComponentCount - 1 do
      begin
        CompIntf := FormCompIntf.GetComponent(i);
        try
          if CompIntF <> nil then
          begin
            Control := TControl(CompIntf.GetComponentHandle);
            if CompareText(Control.Name, Name) = 0 then
            begin
            {$IFDEF GX_VER140_up}
              // FIXME!!!
              ModIntf.ShowForm;
            {$ELSE not GX_VER140_up}
              Form := CompLib.GetActiveForm;
              if Form = nil then
                ShowForm
              else
              begin
                if CompareText(Form.GetFormName, TForm(FormCompIntf.GetComponentHandle).Name) <> 0 then
                  ShowForm;
              end;
            {$ENDIF}
              CompIntf.Select;
              Exit;
            end;
          end;
        finally
          CompIntf.Free;
        end;
      end;
  finally
    if (FormCompIntf <> nil) and (FormCompIntf.GetComponentHandle <> nil) then
      FormCompIntf.Free;

    ModIntf.Free;
    FormIntf.Free;
  end;
end;
{$ENDIF GX_BCB}

(*
function IsCodeRushInstalled: Boolean;
var
  Reg: TRegistry;
  List: TStringList;
  i: Integer;
begin
  IsCodeRushInstalled := False;
  List := TStringList.Create;
  Reg := TRegistry.Create;
  try
    if not Reg.OpenKey('Software\Borland\Delphi\4.0\Known Packages', False) then exit;
    Reg.GetValueNames(List);
    for i := 0 to List.Count - 1 do
      if uppercase(ExtractFileName(List.Strings[i])) = 'CODERUSH3.DPL' then
      begin
        Result := True;
        Break;
      end;
  finally
    Reg.Free;
    List.Free;
  end;
end;

procedure OutputComponentList(Form: TForm);
var
  i: Integer;
begin
  for i := 0 to Form.ComponentCount - 1 do
    SendDebugEx(Form.Components[i].Name, mtInformation);
end;
*)

function AddSlash(const Directory: string): string;
begin
  if Length(Trim(Directory)) = 0 then
    Result := ''
  else
    if Directory[Length(Directory)] <> '\' then
      Result := Directory + '\'
    else
      Result := Directory;
end;

function IsStandAlone: Boolean;
begin
  Result := (ToolServices = nil);
end;

function GXShellExecute(FName, FParam: string; RaiseException: Boolean): Boolean;
var
  ReturnVal: Integer;
begin
  ReturnVal := ShellExecute(Application.Handle, nil, PChar(FName), PChar(FParam),
                            PChar(ExtractFilePath(FName)), SW_SHOWNORMAL);
  Result := ReturnVal > 32;
  if (not Result) and RaiseException then
    raise Exception.Create(SysErrorMessage(GetLastError) + ' ('+FName+')');
end;

{$IFOPT D+}
function BooleanText(B: Boolean): string;
const
  BoolText: array[Boolean] of string =
    ('False', 'True'); // Do not localize
begin
  Result := BoolText[B];
end;
{$ENDIF}

{$IFDEF SYNTAXMEMO}
function GetSyntaxParser(SyntaxParser: TGXSyntaxParser): TSyntaxMemoParser;

  procedure CreateCompiledParser(AParser: TGXSyntaxParser; const ResourceName: string);
  var
    RS: TResourceStream;
  begin
    Parsers[AParser] := TSyntaxMemoParser.Create(nil);
    Parsers[AParser].UI_Styles.GutterWidth := 0;
    RS := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
    try
      Parsers[AParser].CompileFromStream(RS);
    finally
      RS.Free;
    end;
  end;

begin
  // First try to get a cached copy and use that one;
  // if none exists, create a parser instance
  Result := Parsers[SyntaxParser];
  if Result <> nil then
    Exit;

  case SyntaxParser of
    gxpPAS:
      begin
        CreateCompiledParser(gxpPAS, 'PASPARSER');

        {$IFNDEF GX_BCB}
        {$IFDEF GX_VER120_up}
          // Has the user customized the coloring? (TSyntaxMemo.SetToDelphi bug workaround)
          with TRegIniFile.Create(ConfigInfo.RegKey + '\Editor\Highlight\Assembler') do
            if ValueExists('Bold') then
              SetToDelphi(Parsers[gxpPAS], MajorVersionNumberChar);
        {$ENDIF GX_VER120_up}
        {$ELSE GX_BCB}
        //SetToBcb(Parsers[gxpPAS], MajorVersionNumberChar);
        {$ENDIF GX_BCB}

        {$IFDEF VER100}
        SetToDelphi(Parsers[gxpPAS], MajorVersionNumberChar);
        {$ENDIF VER100}
      end;

    gxpCPP:
      begin
        CreateCompiledParser(gxpCPP, 'CPARSER');

        {$IFDEF GX_DELPHI}
        SetToDelphi(Parsers[gxpCPP], MajorVersionNumberChar);
        {$ENDIF GX_DELPHI}

        {$IFDEF GX_BCB}
        SetToBcb(Parsers[gxpCPP], MajorVersionNumberChar);
        {$ENDIF GX_BCB}
      end;

    gxpHTML:
      begin
        CreateCompiledParser(gxpHTML, 'HTMLPARSER');
      end;

    gxpSQL:
      begin
        CreateCompiledParser(gxpSQL, 'SQLPARSER');
      end;

  else
    Assert(False, 'Internal error getting syntax parser');
  end;

  Result := Parsers[SyntaxParser];
  Assert(Result <> nil);
end;

procedure FreeSyntaxParsers;
var
  i: TGXSyntaxParser;
begin
  for i := Low(TGXSyntaxParser) to High(TGXSyntaxParser) do
  begin
    Parsers[i].Free;
    Parsers[i] := nil;
  end;
end;
{$ENDIF SYNTAXMEMO}

{$IFDEF MWEDIT}
procedure GetIDEHighLigherSettings(HighLighter: TmwCustomHighLighter);
var
  Elements: TStringList;
begin
  Elements := TStringList.Create;
  try
    HighLighter.EnumUserSettings(Elements);
    if Elements.Count > 0 then
      HighLighter.UseUserSettings(Elements.Count - 1);
  finally
    Elements.Free;
  end;
end;

procedure SetmwEditHighLighter(mwEdit: TmwCustomEdit; HighLighter: TGXSyntaxParser);
begin
  if mwEdit.HighLighter <> nil then
  begin
    mwEdit.HighLighter.Free;
    mwEdit.HighLighter := nil;
  end;
  case HighLighter of
    gxpPAS:  mwEdit.Highlighter := TmwPasSyn.Create(mwEdit);
    gxpCPP:  mwEdit.Highlighter := TDcjCPPSyn.Create(mwEdit);
    gxpHTML: mwEdit.Highlighter := ThkHTMLSyn.Create(mwEdit);
    gxpSQL:  mwEdit.Highlighter := TwmSQLSyn.Create(mwEdit);
  else
    mwEdit.Highlighter := TmwGeneralSyn.Create(mwEdit);
  end;
  if HighLighter in [gxpPAS, gxpCPP, gxpSQL] then
    GetIDEHighLigherSettings(mwEdit.Highlighter);
end;
{$ENDIF MWEDIT}

function GetFileSize(const FileName: string): Integer;
var
  F: file of Byte;
  OldFileMode: Byte;
begin
  AssignFile(F, FileName);
  try
    OldFileMode := FileMode;
    FileMode := 0; // read-only

    Reset(F);
    try
      Result := FileSize(F);
    finally
      CloseFile(F);
      FileMode := OldFileMode;
    end;
  except
    on E: Exception do
      Result := 0;
  end;
end;

type
  {$IFDEF GX_VER120_up}
  THinstance = LongWord;
  {$ELSE GX_VER120_up}
  THinstance = LongInt;
  {$ENDIF GX_VER120_up}

function BuiltWithPackages: Boolean;
{$IFNDEF GX_NotPackageBuilt}
var
  IsNotInInstance: Boolean;
{$ENDIF GX_NotPackageBuilt}
begin
  Result := True;

  {$IFNDEF GX_NotPackageBuilt}
  IsNotInInstance := (THinstance(FindClassHInstance(TForm)) <> HINSTANCE); // VCL
  Result := Result and IsNotInInstance;
  IsNotInInstance := (THinstance(FindClassHInstance(TDdeServerConv)) <> HINSTANCE); // VCLX
  Result := Result and IsNotInInstance;

  {$IFNDEF GX_NOBDE}
  IsNotInInstance := (THinstance(FindClassHInstance(TDataSet)) <> HINSTANCE); // VCL
  Result := Result and IsNotInInstance;
  IsNotInInstance := (THinstance(FindClassHInstance(TTable)) <> HINSTANCE); // VCLBDE/VCLDB
  Result := Result and IsNotInInstance;
  {$ENDIF GX_NOBDE}
  {$ENDIF GX_NotPackageBuilt}
end;

procedure ShowNoPackagesError;
resourcestring
  SGxBuildError = 'GExperts build error';
  SNotBuiltWithRequiredPackages =
 'GExperts has not been built with the required runtime packages: '#13+
 #13+'  '+
 GxRequiredPackageList +#13+
 #13+
 'Please add these packages to the list of used runtime packages, '+
 'check the "Build with runtime packages" box, and rebuild GExperts.'#13+
 #13+
 'GExperts will not be installed into the IDE until this has been done.';
begin
  Windows.MessageBox(0,
                     PChar(SNotBuiltWithRequiredPackages),
                     PChar(SGxBuildError),
                     MB_OK or MB_ICONERROR);
end;

function GetProjectDir: string;
(*
{$IFDEF GX_UseNativeToolsApi}
var
  IProject: IOTAProject;
{$ENDIF GX_UseNativeToolsApi}
*)
begin
  Result := '';
(*
  {$IFDEF GX_UseNativeToolsApi}
  IProject := GetCurrentProject;
  if IProject <> nil then
    Result := ExtractFilePath(IProject.FileName);
  {$ELSE not GX_UseNativeToolsApi}
*)
  Assert(ToolServices <> nil);
  Result := ExtractFilePath(ToolServices.GetProjectName);
  if Length(Result) = 0 then
    raise ERangeError.Create('String error');
  if Result[Length(Result)] = '\' then
    Delete(Result, Length(Result), 1);
//  {$ENDIF GX_UseNativeToolsApi}
end;

function GetProjectName: string;
begin
  Assert(ToolServices <> nil);
  Result := ExtractFileName(ToolServices.GetProjectName);
  Result := ChangeFileExt(Result, '');
end;

{$IFDEF GX_UseNativeToolsApi}

(*
// From Ray Lischner:
// The Open Tools API has a method that returns the current module, but
// not the current project. To find the currently active project, iterate
// through all modules. If a module is a project group (and Delphi should have
// only one project group open), call its ActiveProject method to learn the
// active project. Otherwise, if a module is a project, and there is only
// one such project, it must be the active project. Otherwise, return nil.
function GetCurrentProject: IOTAProject;
var
  Services: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  MultipleProjects: Boolean;
  I: Integer;
begin
  Result := nil;
  MultipleProjects := False;
  Services := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Services.ModuleCount-1 do
  begin
    Module := Services.Modules[I];
    if Module.QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK then
    begin
      Result := ProjectGroup.ActiveProject;
      Exit;
    end
    else if Module.QueryInterface(IOTAProject, Project) = S_OK then
    begin
      if Result = nil then
        // Found the first project, so save it.
        Result := Project
      else
        MultipleProjects := True;
        // It doesn't look good, but keep searching for a project group.
    end;
  end;
  if MultipleProjects then
    Result := nil;
end;
*)

function GetProjectGroupFileName: string;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  IProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := '';
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  IProjectGroup := nil;
  for i := 0 to IModuleServices.ModuleCount - 1 do
  begin
    IModule := IModuleServices.Modules[i];
    if IModule.QueryInterface(IOTAProjectGroup, IProjectGroup) = S_OK then
      Break;
  end;
  // Delphi 4/5 do not return the file path when querying IOTAProjectGroup directly
  if IProjectGroup <> nil then
    Result := IModule.FileName;
end;

function GetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  i: Integer;
begin
  Result := nil;
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to IModuleServices.ModuleCount - 1 do
    if IModuleServices.Modules[i].QueryInterface(IOTAProjectGroup, Result) = S_OK then
      Exit;
  Result := nil;
end;

{$ENDIF GX_UseNativeToolsApi}

// Replace with ChangeFileExt(FileName, ''); ??
function GetFileName(FileName: string): string;
var
  FileExt: string;
  ExtPos: Integer;
begin
  FileName := ExtractFileName(FileName);
  FileExt := ExtractFileExt(FileName);
  ExtPos := Pos(FileExt, FileName);
  if ExtPos > 0 then
    Result := Copy(FileName, 1, ExtPos - 1)
  else
    Result := FileName;
end;

procedure CenterForm(Form: TCustomForm);
var
  Rect: TRect;
begin
  if Form = nil then Exit;

  SystemParametersInfo(SPI_GETWORKAREA, 0, @Rect, 0);

  with Form do
  begin
    SetBounds((Rect.Right - Rect.Left - Width) div 2,
      (Rect.Bottom - Rect.Top - Height) div 2,
               {Form.} Width,
               {Form.} Height);
  end;
end;

function StripWhiteSpace(const S: string): string;
var
  i: Integer;
begin
  Result := S;

  i := Length(Result);
  while i > 0 do
  begin
    case Result[i] of

      #10, #13: Delete(Result, i, 1);

      #32:
        begin
          // this requires special casing
          // we only want to remove doubled blanks
          if (i > 1) and (Result[i - 1] = #32) then
            Delete(Result, i, 1);
        end;
    end;
    Dec(i);
  end;
end;

function CaseInsensitivePos(const SubString, S: string): Integer;
begin
  Result := Pos(UpperCase(SubString), UpperCase(S));
end;

function Min(v1, v2: Integer): Integer;
begin
  if v1 < v2 then
    Result := v1
  else
    Result := v2;
end;

function Max(v1, v2: Integer): Integer;
begin
  if v1 > v2 then
    Result := v1
  else
    Result := v2;
end;

{ Get a property value and return it as a string. }

function GetPropAsString(CompIntf: TIComponentInterface; Index: Integer): string;
resourcestring
  SMethodOutput = 'Method(%p)';
var
  VariantValue: Variant;
  OrdinalValue: LongInt;
  MethodValue: TMethod;
  FloatValue: Extended;
begin
  case CompIntf.GetPropType(Index) of
    ptString, ptLString, ptLWString:
      begin
        CompIntf.GetPropValue(Index, Result);
      end;
    ptInteger, ptChar, ptEnumeration, ptSet, ptClass, ptWChar:
      begin
        CompIntf.GetPropValue(Index, OrdinalValue);
        case CompIntf.GetPropType(Index) of
          ptInteger, ptEnumeration:
            Result := IntToStr(OrdinalValue);
          ptSet, ptClass:
{$IFDEF GX_BCB}          
            Result := '0x' + IntToHex(OrdinalValue, 8);
{$ELSE}
            Result := '$' + IntToHex(OrdinalValue, 8);
{$ENDIF GX_BCB}
          ptChar:
            Result := Char(OrdinalValue);
          ptWChar:
            Result := WideChar(OrdinalValue);
        end;
      end;
    ptFloat:
      begin
        CompIntf.GetPropValue(Index, FloatValue);
        Result := FloatToStr(FloatValue);
      end;
    ptMethod:
      begin
        CompIntf.GetPropValue(Index, MethodValue);
        Result := Format(SMethodOutput, [MethodValue.Code]);
      end;
    ptVariant:
      begin
        CompIntf.GetPropValue(Index, VariantValue);
        Result := VariantValue;
      end;
  else
    Result := '';
  end;
end;

{ Set a property value from a string }

procedure SetPropAsString(CompIntf: TIComponentInterface; Index: Integer; const Value: string);
var
  FloatValue: Extended;
  IntegerValue: Integer;
begin
  try
    case CompIntf.GetPropType(Index) of
      ptString, ptLString, ptLWString:
        begin
          CompIntf.SetProp(Index, Value);
        end;
      ptInteger, ptEnumeration:
        begin
          IntegerValue := StrToInt(Value);
          CompIntf.SetProp(Index, IntegerValue);
        end;
      ptFloat:
        begin
          FloatValue := StrToFloat(Value);
          CompIntf.SetProp(Index, FloatValue);
        end;
    end;
  except
    on E: Exception do
    begin
      // nothing - swallow each exception
    end;
  end;
end;

function GetPropIndex(CompIntf: TIComponentInterface; const Name: string): Integer;
begin
  Result := CompIntf.GetPropCount - 1;
  while Result >= 0 do
  begin
    if CompareText(CompIntf.GetPropname(Result), Name) = 0 then
      Break;
    Dec(Result);
  end;
end;

{
procedure AssertIsDprOrPas(const FileName: string);
resourcestring
  SExpertForPasOrDprOnly = 'This expert is for use in .PAS or .DPR files only';
begin
  if not IsDprOrPas(FileName) then
    raise Exception.Create(SExpertForPasOrDprOnly);
end;
}

function IsDprOrPas(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := ((FileExt = '.PAS') or (FileExt = '.DPR'));
end;

function IsInc(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := (FileExt = '.INC');
end;

function IsDpr(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := (FileExt = '.DPR');
end;

function IsCpp(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := (FileExt = '.CPP');
end;

function IsH(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := (FileExt = '.H');
end;

function IsDfm(const FileName: string): Boolean;
begin
  Result := (ExtractUpperFileExt(FileName) = '.DFM');
end;

function IsPas(const FileName: string): Boolean;
begin
  Result := (ExtractUpperFileExt(FileName) = '.PAS');
end;

function IsCppSourceModule(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := ExtractUpperFileExt(FileName);
  Result := ((FileExt = '.CPP') or (FileExt = '.C') or
             (FileExt = '.HPP') or (FileExt = '.H') or
             (FileExt = '.CXX') or
             (FileExt = '.HXX') or (FileExt = '.HH') or
             (FileExt = '.ASM'));
end;

function IsKnownSourceFile(const FileName: string): Boolean;
begin
  Result := IsDprOrPas(FileName) or IsCppSourceModule(FileName);
end;

function IsCppBuilderIde: Boolean;
begin
  Result := False;
  if ToolServices <> nil then
    Result := (Pos('C++BUILDER', UpperCase(ToolServices.GetBaseRegistryKey)) <> 0);
end;

function ExtractUpperFileExt(const FileName: string): string;
begin
  Result := UpperCase(ExtractFileExt(FileName));
end;

function DLLName: string;
var
  Buf: array[0..MAX_PATH + 1] of Char;
begin
  GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf) - 1);
  Result := StrPas(Buf);
end;

var
  xNtQueryObject: function(ObjectHandle: THandle;
      ObjectInformationClass: OBJECT_INFORMATION_CLASS;
      ObjectInformation: Pointer; ObjectInformationLength: ULONG;
      ReturnLength: PULONG): NTSTATUS; stdcall = nil;

function GetPathFromHandle(hObject: THandle; var Path: WideString): Boolean;
var
  Status: NTSTATUS;
  InitialBuffer: OBJECT_NAME_INFORMATION;
  NameInfo: POBJECT_NAME_INFORMATION;
  Size: Cardinal;
begin
  Result := False;
  Path := '';
  if not Assigned(@xNtQueryObject) then
    @xNtQueryObject := GetProcAddress(GetModuleHandle(ntdll), 'NtQueryObject');
  if not Assigned(@xNtQueryObject) then
    Exit;
  FillChar(InitialBuffer, SizeOf(InitialBuffer), 0);
  NameInfo := @InitialBuffer;
  Size := SizeOf(InitialBuffer);
  // Query the name information a first time to get the size of the name.
  Status := xNtQueryObject(hObject, ObjectNameInformation, NameInfo, Size, @Size);
  if Size > 0 then begin
    GetMem(NameInfo, Size);
    try
      FillChar(NameInfo^, Size, 0);
      // Query the name information a second time to get the name of the
      // object referenced by the handle.
      Status := xNtQueryObject(hObject, ObjectNameInformation, NameInfo,
          Size, @Size);
      if NT_SUCCESS(Status) then begin
        Path := NameInfo.ObjectName.Buffer;
        SetLength(Path, NameInfo.ObjectName.Length div SizeOf(WideChar));
      end;
    finally
      FreeMem(NameInfo);
    end;
  end;
  Result := NT_SUCCESS(Status);
end;

function GetCurrentEditWindowNumber(AFilename: string; var WindowCaption:string): Integer;
var
  WindowIterator: HWND;
  Buffer: array[0..255] of Char;
  APos: PChar;
begin
  WindowCaption := '';
  Result := -1;

  WindowIterator := GetWindow(GetDesktopWindow, GW_CHILD);
  Win32Check(WindowIterator <> 0);

  // Iterate over all windows whose owner is the Desktop.
  WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
  Win32Check(WindowIterator <> 0);

  AFilename := AnsiUpperCase(AFilename);

  // If we find a window with the class name "TEditWindow"
  // and the file names match, then this is the currently active
  // edit view.
  while (WindowIterator <> 0) and IsWindow(WindowIterator) do
  begin
    if GetClassName(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 then
      RaiseLastWin32Error;

    if StrPos(Buffer, 'TEditWindow') <> nil then
    begin
      if GetWindowText(WindowIterator, Buffer, SizeOf(Buffer) - 1) = 0 then
        RaiseLastWin32Error;

      WindowCaption := Buffer;
      AnsiStrUpper(Buffer);

      if AnsiStrPos(Buffer, PChar(AFilename)) <> nil then
      begin
        // In the case where there are multiple edit windows
        // open the first one you come to in the iteration process
        // should always be the top-most (or most recently active) edit
        // window - JCH
        // Scan window caption from the end; if we started at the
        // beginning, we might run into the colon of C:\MyFile
        APos := AnsiStrRScan(Buffer, ':');
        if APos <> nil then
        begin
          Inc(APos);
          Result := StrToIntDef(StrPas(APos), -1);

          // Subtract 1 since we need 0..GetViewCount-1 rather
          // than 1..GetViewCount.
          if Result <> -1 then
            Dec(Result);
        end
        else
        begin
          Result := 0;
        end;

        Break;
      end;
    end;
    WindowIterator := GetWindow(WindowIterator, GW_HWNDNEXT);
    // Do not test WindowIterator here, as the while
    // loop has the error criterion as the legitimate
    // termination condition.
  end;
end;

// There was a change in behaviour from D3/D4 to D5 for
// the view "array" (TIEditorInterface.GetView).
// In D3/D4, view:1 would be at position 0 of that array,
// view:2 would be at position 1 of that array, and so on.
// As of D5, the array mirrors the behaviour of the MDIChildren
// array: the top-most (current) view is at position 0, the
// view following in Z-order is at position 1, and so on.
{$IFDEF GX_VER130_UP}
function GetCurrentEditView(EditorIntf: TIEditorInterface): Integer;
begin
  Result := 0;
end;
{$ELSE}
function GetCurrentEditView(EditorIntf: TIEditorInterface): Integer;
var
  EditorFileName: string;
  TmpCaption: string;
begin
  Assert(EditorIntf <> nil);
  if EditorIntf.GetViewCount = 1 then
  begin
    // If there is only one edit view, then it is clear that
    // the first one is the current edit view.
    Result := 0;
  end
  else
  begin
    EditorFileName := ExtractFileName(EditorIntf.FileName);
    Result := GetCurrentEditWindowNumber(EditorFileName, TmpCaption);
  end;
end;
{$ENDIF GX_VER130_UP}

{$IFDEF GX_UseNativeToolsApi}

procedure SaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
const
  // Leave typed constant as is - needed for streaming code.
  TerminatingNulChar: Char = #0;
  BufferSize = 2048;
var
  Buffer: PChar;
  EditReaderPos: Integer;
  ReadDataSize: Integer;
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

  GetMem(Buffer, BufferSize);
  try
    EditReaderPos := 0;
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
    Inc(EditReaderPos, ReadDataSize);
    while ReadDataSize = BufferSize do
    begin
      Stream.Write(Buffer^, ReadDataSize);
      ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, BufferSize);
      Inc(EditReaderPos, ReadDataSize);
    end;
    Stream.Write(Buffer^, ReadDataSize);
    Stream.Write(TerminatingNulChar, SizeOf(TerminatingNulChar));
  finally
    FreeMem(Buffer);
  end;
end;

function GetCurrentSourceEditor: IOTASourceEditor;
var
  i: Integer;
  IModuleServices: IOTAModuleServices;
  ICurrentModule: IOTAModule;
  IEditor: IOTAEditor;
begin
  Result := nil;

  IModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Assert(IModuleServices <> nil);

  ICurrentModule := IModuleServices.CurrentModule;
  //Assert(ICurrentModule <> nil);
  if ICurrentModule = nil then
    Exit;

  for i := 0 to ICurrentModule.GetModuleFileCount-1 do
  begin
    IEditor := ICurrentModule.GetModuleFileEditor(i);
    if IEditor.QueryInterface(IOTASourceEditor, Result) = S_OK then
      Break;
  end;
end;

function GetGXHighLighterForCurrentSourceEditor: TGXSyntaxParser;
var
  ISourceEditor: IOTASourceEditor;
  Extension: string;
begin
  Result := gxpNone;

  ISourceEditor := GetCurrentSourceEditor;
  if ISourceEditor = nil then
    Exit;

  case ISourceEditor.SetSyntaxHighlighter(shQuery) of
    shPascal: Result := gxpPAS;
    shC:      Result := gxpCPP;
    shSQL:    Result := gxpSQL;
    //shIDL:  Result := ...
    else
    begin
      Extension := ExtractUpperFileExt(ISourceEditor.GetFileName);
      if (Extension = '.HTML') or (Extension = 'HTM') then
        Result := gxpHTML;
    end;
  end;
end;

function GetFileNameOfCurrentModule: string;
resourcestring
  SNoModuleServices = 'IOTAModuleServices is not available';
  SNoCurrentModule =  'There is no current IDE module available';
var
  IModuleServices: IOTAModuleServices;
  ICurrentModule: IOTAModule;
begin
  IModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Assert(IModuleServices <> nil, SNoModuleServices);

  ICurrentModule := IModuleServices.CurrentModule;
  Assert(ICurrentModule <> nil, SNoCurrentModule);

  Result := ICurrentModule.FileName;
end;

// Returns True if selected block was read;
// returns False if no block was selected and
// the complete editor buffer was copied into the stream.
function ReadSourceEditorBlockSelection(Stream: TStream): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
  IEditReader: IOTAEditReader;
  {$IFDEF GX_VER130_up}
  BlockSelText: string;
  {$ENDIF GX_VER130_up}
begin
  Assert(Stream <> nil);

  Result := False;

  ISourceEditor := GetCurrentSourceEditor;
  if ISourceEditor = nil then
    Exit;

  {$IFOPT D+}SendDebug('EditViewCount is ' + IntToStr(ISourceEditor.EditViewCount));{$ENDIF}
  if ISourceEditor.EditViewCount > 0 then
  begin
    // This is the hopefully is the top-most view;
    // Delphi 4 does not have the nicer IOTAEditBuffer.TopView of Delphi 5+.
    IEditView := ISourceEditor.EditViews[0];
    Assert(IEditView <> nil);

    // TODO  -cIssue -oAnyone : Delphi 4 does not support IOTAEditView.Block
    {$IFDEF GX_VER130_up}
    {$IFOPT D+}SendDebug('Block.Size is ' + IntToStr(IEditView.Block.Size));{$ENDIF}
    if IEditView.Block.Size > 0 then
    begin
      BlockSelText := IEditView.Block.Text;
      Stream.WriteBuffer(PChar(BlockSelText)^, Length(BlockSelText) + SizeOf(#0));
      Result := True;
    end
    else
    {$ENDIF GX_VER130_up}
    begin
      IEditReader := ISourceEditor.CreateReader;
      SaveReaderToStream(IEditReader, Stream);
      Result := False;
    end;
  end;
end;

{$ELSE not GX_UseNativeToolsApi}

function GetGXHighLighterForCurrentSourceEditor: TGXSyntaxParser;
var
  EditRead: TEditReader;
begin
  Result := gxpNone;

  Assert(ToolServices <> nil);
  EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
  if EditRead <> nil then
  try
    case EditRead.SyntaxHighlighter of
      shPascal:
        Result := gxpPAS;
      {$IFDEF GX_VER110_up}
      shC:
        Result := gxpCPP;
      {$ENDIF GX_VER110_up}
      shSQL:
        Result := gxpSQL;
    end;
  finally
    EditRead.Free;
  end;
end;

function GetFileNameOfCurrentModule: string;
begin
  Assert(ToolServices <> nil);
  Result := ToolServices.GetCurrentFile;
end;

function ReadSourceEditorBlockSelection(Stream: TStream): Boolean;
var
  EditRead: TEditReader;
  SelectedBlock: string;
begin
  Assert(ToolServices <> nil);
  EditRead := TEditReader.Create(ToolServices.GetCurrentFile);

  {$IFOPT D+}SendDebug('Getting Edit Block');{$ENDIF}
  SelectedBlock := EditRead.GetBlock;
  if Length(SelectedBlock) > 1 then
  begin
    Stream.WriteBuffer(PChar(SelectedBlock)^, Length(SelectedBlock) + SizeOf(#0));
    Result := True;
  end
  else
  begin
    // Get the complete editor text
    EditRead.SaveToStream(Stream);
    Result := False;
  end;
end;

{$ENDIF GX_UseNativeToolsApi}

function GetIdeMainForm: TCustomForm;
begin
  Result := Application.FindComponent('AppBuilder') as TCustomForm;
  {$IFOPT D+} if Result = nil then SendDebug('Unable to find AppBuilder!'); {$ENDIF}
end;

function GetComponentPaletteTabControl: TTabControl;
var
  MainForm: TCustomForm;
begin
  Result := nil;
  MainForm := GetIdeMainForm;
  if MainForm <> nil then
    Result := TTabControl(MainForm.FindComponent('TabControl'));
  {$IFOPT D+}if Result = nil then SendDebug('Unable to find TabControl!'); {$ENDIF}
end;

procedure FocusEditWindow(NumWindows: Integer; AFilename: string);
var
  EditForm: TCustomForm;
  i: Integer;
  WindowName: string;
  WindowCaption: string;
begin
  AFilename := ExtractFileName(AFilename);
  { IgnoreReturnData := } GetCurrentEditWindowNumber(AFilename, WindowCaption);
  for i := 0 to NumWindows - 1 do
  begin
    WindowName := 'EditWindow_' + IntToStr(i);
    EditForm := Application.FindComponent(WindowName) as TCustomForm;
    if Assigned(EditForm) then
    begin
      if EditForm.Caption = WindowCaption then
      begin
        if EditForm.CanFocus then
          EditForm.SetFocus;
        Break;
      end;
    end;
  end;
end;

procedure ShowExceptionErrorMessage(E: Exception);
begin
  ProcessException(E);
  MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure ProcessExceptionMsg(E: Exception; const Msg: string);
begin
  MessageDlg(Msg + E.Message, mtError, [mbOK], 0);
end;

procedure ProcessException(E: Exception);
begin
end;

initialization
  {$IFDEF SYNTAXMEMO}
  FillChar(Parsers, SizeOf(Parsers), 0);
finalization
  FreeSyntaxParsers;
  {$ENDIF SYNTAXMEMO}
end.

