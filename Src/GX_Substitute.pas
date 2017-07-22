// This unit is used to perform string substitution for string variables
// Function arguments, result, and username substitution by Ulrich Schulte and EB

unit GX_Substitute;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is compatible with C++Builder
//!      --> BUT there is only a Pascal parser available; hence do
//!          pass in DoParseSource = False if the current file is
//!          not Object Pascal source code

{$I GX_CondDefine.inc}

uses
  Classes, SysUtils, Windows,
  {$IFDEF GX_UseNativeToolsApi}
  ToolsApi,
  {$ENDIF GX_UseNativeToolsApi}
  ToolIntf, ExptIntf;

function ReplaceStrings(const RawString: string; DoParseSource: Boolean): string;

implementation

uses
  mPasLex,
  {$IFOPT D+}
  GX_DbugIntf,
  {$ENDIF D+}
  GX_ConfigurationInfo, GX_GenFunc, GX_EditReader;

resourcestring
  SUnknownNameResult = 'Not available';
  SNoneResult = 'None';

function GetUnitName: string;
var
  Parser: TmwPasLex;
  EditRead: TEditReader;
  MemStream: TMemoryStream;
begin
  Result := SUnknownNameResult;
  MemStream := TMemoryStream.Create;
  try
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
    try
      EditRead.SaveToStream(MemStream);
    finally
      EditRead.Free;
    end;
    Parser := TmwPasLex.Create;
    try
      Parser.Origin := MemStream.Memory;
      while (Parser.TokenID <> tkNull) and (Parser.TokenID <> tkUnit) do
        Parser.NextNoJunk;
      if Parser.TokenID = tkUnit then
      begin
        Parser.NextNoJunk;
        if Parser.TokenID = tkIdentifier then
          Result := Parser.Token;
      end;
    finally
      Parser.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

function GetCurrentUser: string;
var
  D: DWORD;
  S: array[0..256] of Char;
begin
  Result := SUnknownNameResult;
  D := SizeOf(S);
  if Windows.GetUserName(S, D) then
    Result := S;
end;

procedure GetNameArgsResult(var Name, Args, ResultType: string);
var
  Parser: TmwPasLex;
  EditRead: TEditReader;
  MemStream: TMemoryStream;
begin
  Name := SUnknownNameResult;
  Args := SNoneResult;
  ResultType := SNoneResult;

  MemStream := TMemoryStream.Create;
  try
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData
    EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
    try
      EditRead.SaveToStreamFromPos(MemStream);
    finally
      EditRead.Free;
    end;
    Parser := TmwPasLex.Create;
    try
      Parser.Origin := MemStream.Memory;
      while not (Parser.TokenID in [tkNull, tkProcedure, tkFunction, tkConstructor, tkDestructor]) do
        Parser.NextNoJunk;
      if Parser.TokenID in [tkProcedure, tkFunction, tkConstructor, tkDestructor] then
      begin
        Parser.NextNoJunk; // Get the proc/class identifier
        if Parser.TokenID = tkIdentifier then
          Name := Parser.Token;
        Parser.NextNoJunk; // Skip to the open paren or the '.'
        if Parser.TokenID = tkPoint then
        begin
          Parser.NextNoJunk; // Get the proc identifier
          Name := Name + '.' + Parser.Token;
          Parser.NextNoJunk; // skip past the procedure identifier
        end;

        if Parser.TokenID = tkRoundOpen then
        begin
          Parser.NextNoJunk;
          Args := '';
          while not (Parser.TokenID in [tkNull, tkRoundClose]) do
          begin
            if Parser.TokenID in [tkCRLF, tkCRLFCo, tkSlashesComment, tkBorComment, tkAnsiComment, tkSpace] then
              Args := Args + ' '
            else
              Args := Args + Parser.Token;
            Parser.Next;
          end;
          Args := StripWhiteSpace(Args);
          // Skip to the colon or semicolon after the ')'
          Parser.NextNoJunk;
        end;
        if Parser.TokenID in [tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo, tkSpace] then
          Parser.NextNoJunk;
        // If a colon is found, find the next token
        if Parser.TokenID = tkColon then
        begin
          Parser.NextNoJunk;
          ResultType := Parser.Token;
        end;
      end;
    finally
      Parser.Free;
    end;
  finally
    MemStream.Free;
  end;
end;


function GetReplaceString(StVar: string; DoParseSource: Boolean): string;
var
  ProcName, ProcArgs, ProcResult: string;
begin
  // do not localize any of the below items
  Result := '';
  ProcName := SUnknownNameResult;
  ProcArgs := SUnknownNameResult;
  ProcResult := SUnknownNameResult;
  StVar := UpperCase(StVar);
{$IFDEF GX_UseNativeToolsApi}
  if StVar = '%PROJECTGROUPNAME%' then
  begin
    Result := ExtractFileName(GetProjectGroupFileName);
    if Result = '' then
      Result := SUnknownNameResult;
    {$IFOPT D+}SendDebug('ProjectGroupName: '+Result+' ("'+GetProjectGroupFileName+'")');{$ENDIF}
  end;
  if StVar = '%PROJECTGROUPDIR%' then // Currently unsupported by all IDEs (blank)
  begin
    Result := ExtractFilePath(GetProjectGroupFileName);
    if Result = '' then
      Result := SUnknownNameResult;
    {$IFOPT D+}SendDebug('ProjectGroupDir: '+Result+' ("'+GetProjectGroupFileName+'")');{$ENDIF}
  end;
{$ENDIF GX_UseNativeToolsApi}
  if StVar = '%PROJECTDIR%' then
    Result := GetProjectDir
  else
  if StVar = '%PROJECTNAME%' then
    Result := GetProjectName
  else
  if StVar = '%DATETIME%' then
    Result := DateTimetoStr(Date + Time)
  else
  if StVar = '%HOUR%' then
    Result := FormatDateTime('hh', Time)
  else
  if StVar = '%MINUTE%' then
    Result := FormatDateTime('nn', Time)
  else
  if StVar = '%SECOND%' then
    Result := FormatDateTime('ss', Time)
  else
  if StVar = '%DATE%' then
    Result := DatetoStr(Date)
  else
  if StVar = '%YEAR%' then
    Result := FormatDateTime('yyyy', Date)  //! Y2K lesson <g>
  else
  if StVar = '%MONTH%' then
    Result := FormatDateTime('mm', Date)
  else
  if StVar = '%MONTHSHORTNAME%' then
    Result := FormatDateTime('mmm', Date)
  else
  if StVar = '%MONTHLONGNAME%' then
    Result := FormatDateTime('mmmm', Date)
  else
  if StVar = '%DAY%' then
    Result := FormatDateTime('dd', Date)
  else
  if StVar = '%DAYSHORTNAME%' then
    Result := FormatDateTime('ddd', Date)
  else
  if StVar = '%DAYLONGNAME%' then
    Result := FormatDateTime('dddd', Date)
  else
  if StVar = '%UNIT%' then
  begin
    if DoParseSource then
      Result := GetUnitName
    else
      Result := SUnknownNameResult;
  end
  else
  if stVar = '%USER%' then
    Result := GetCurrentUser
  else
  if StVar = '%PROCNAME%' then
  begin
    if DoParseSource then
    begin
      GetNameArgsResult(ProcName, ProcArgs, ProcResult);
      Result := ProcName;
    end
    else
      Result := SUnknownNameResult;
  end
  else
  if StVar = '%RESULT%' then
  begin
    if DoParseSource then
    begin
      GetNameArgsResult(ProcName, ProcArgs, ProcResult);
      Result := ProcResult;
    end
    else
      Result := SUnknownNameResult;
  end
  else
  if StVar = '%ARGUMENTS%' then
  begin
    if DoParseSource then
    begin
      GetNameArgsResult(ProcName, ProcArgs, ProcResult);
      Result := ProcArgs;
    end
    else
      Result := SUnknownNameResult;
  end;
end;

function ReplaceStrings(const RawString: string; DoParseSource: Boolean): string;
resourcestring
  SReplaceError = 'Error!';
var
  i: Integer;
  t: Integer;
begin
  i := 1;
  while i <= Length(RawString) do
  begin
    if RawString[i] <> '%' then
      Result := Result + RawString[i]
    else
    begin
      if i < Length(RawString) then
        if RawString[i + 1] = '%' then
        begin
          Result := Result + '%';
          Inc(i, 2);
          Continue;
        end;
      t := i + 1;
      while (t <= Length(RawString)) and (RawString[t] <> '%') do
        Inc(t);
      if t <= Length(RawString) then
        Result := Result + GetReplaceString(Copy(RawString, i, t - i + 1), DoParseSource)
      else
        Result := Result + SReplaceError;
      i := t;
    end;
    Inc(i);
  end;
end;

end.

