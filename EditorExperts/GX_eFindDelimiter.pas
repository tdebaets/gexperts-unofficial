unit GX_eFindDelimiter;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is *not* compatible with C++Builder
    // it exclusively scans Pascal source code

uses
  mwPasParser,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  Dialogs, ExptIntf, EditIntf, GX_EditorExpert, StdCtrls, Registry;

type
  TBaseDelimiterExpert = class(TEditorExpert)
  public
    procedure DoDelimiterAction(EditIntf: TIEditorInterface;
                                Parser: TmPasParser;
                                SChar, EChar: TCharPos); virtual; abstract;
    procedure Execute; override;
  end;

  TLocateDelimiter = class(TBaseDelimiterExpert)
  public
    constructor Create; override;
    procedure DoDelimiterAction(EditIntf: TIEditorInterface;
                                Parser: TmPasParser;
                                SChar, EChar: TCharPos); override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TMoveToDelimiter = class(TBaseDelimiterExpert)
  public
    constructor Create; override;
    procedure DoDelimiterAction(EditIntf: TIEditorInterface;
                                Parser: TmPasParser;
                                SChar, EChar: TCharPos); override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

implementation

uses
  ExpertUtil, GX_EditReader, GX_GenFunc;

resourcestring
  SDelimiterMoveToMessage =
    '  This expert enables you to quickly move to a matching '+
    'ending delimiter for the following tokens: begin, try, case, repeat, ''('', and ''[''.';

  SDelimiterLocateMessage =
    '  This expert enables you to quickly locate a matching '+
    'ending delimiter for the following tokens: begin, try, case, repeat, ''('', and ''[''.';

  SDelimiterUsage =
    #13#10+
    'The expert takes the following steps to match delimiters:'+#13#10+
    ' - Beginning from the current cursor position, the expert looks to the left '+
    'on the line for an opening token such as "try" or "begin".'+#13#10+
    ' - If an opening token is found, the expert scans for the associated '+
    'closing token, such as "end" in the above instance.' +#13#10+
    ' - If no opening token is found, as in the case "if|(i=0)" (where ''|'' represents the cursor), the '+
    'expert attempts to find an opening token to the right of the current '+
    'cursor position. In the mentioned example, this will identify the opening '+
    'parenthesis, and an attempt is made to locate the closing parenthesis.';

resourcestring
  SNotValidIdentifier = '"%s" is not a supported beginning delimiter.';
  SNoMatchingEndFound = 'No matching closing delimiter was found.';

{ TBaseDelimiterExpert }

{ TODO -oStefan -cCleanup:
    TBaseDelimiterExpert.Execute method is by far too long, too complex.
    It needs to be broken up into some more manageable chunks at
    some stage. }
procedure TBaseDelimiterExpert.Execute;
resourcestring
  SPasFilesOnly = 'This expert is for use in PAS, DPR, and INC files only';
  SCouldNotGetSourceBuffer = 'Could not get source code from editor buffer (is a special selection active?).';
var
  EditRead: TEditReader;
  MemStream: TMemoryStream;
  C: Integer;

  StartToken: TTokenKind;
  EndToken: TTokenKind;
  SPos: Integer;
  EPos: Integer;
  SChar: TCharPos;
  EChar: TCharPos;
  ModIntf: TIModuleInterface;
  EditIntf: TIEditorInterface;
  EList: TEditorStrings;
  Parser: TmPasParser;

  TextToken: string;
  EditorLine: string;
  FileName: string;
  ScanPos: Integer;
  SPosIncrement: Integer;
begin
  MemStream := TMemoryStream.Create;
  try
    FileName := GetFileNameOfCurrentModule;
    if not (IsDprOrPas(FileName) or IsInc(FileName)) then
      raise Exception.Create(SPasFilesOnly);
    // Since this edit reader is destroyed almost
    // immediately, do not call FreeFileData.
    EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
    try
      // We start trying to read a token from the left.
      // Tokens on the left of the current cursor
      // position always take precedence over other "opening"
      // tokens like brackets that are to the right of
      // the cursor.
      EditorLine := EditRead.GetPrecedingCharacters(256);
      ScanPos := Length(EditorLine);
      while ScanPos > 0 do
      begin
        // None of the "opening" tokens starts with a numeric
        // character, so we scan for alphabetic characters only.
        if not (EditorLine[ScanPos] in ['a'..'z', 'A'..'Z']) then
          Break;
        Dec(ScanPos);
      end;
      Delete(EditorLine, 1, ScanPos);
      MemStream.Write(PChar(EditorLine)^, Length(EditorLine));

      SPos := EditRead.GetCurrentBufferPos - Length(EditorLine);
      EditRead.SaveToStreamFromPos(MemStream);
    finally
      EditRead.Free;
    end;

    MemStream.Position := 0;
    SPosIncrement := 0;

    Parser := TmPasParser.Create;
    try
      Parser.Origin := MemStream.Memory;
      if Parser.Origin = nil then
      begin
        MessageDlg(SCouldNotGetSourceBuffer, mtError, [mbOK], 0);
        Exit;
      end;

      // Make a first attempt at identifying the opening
      // token to the LEFT if it existed; if there was
      // no token to the left, start looking to the right
      // of the cursor.
      Parser.NextToken;
      StartToken := Parser.Token.ID;
      case Parser.Token.ID of
        tkBegin, tkTry, tkCase,
        tkFinally, tkExcept:    EndToken := tkEnd;
        tkRoundOpen:            EndToken := tkRoundClose;
        tkSquareOpen:           EndToken := tkSquareClose;
        tkRepeat:               EndToken := tkUntil;
      else
        TextToken := Parser.Token.Data;

        // Alright, so there was a token to the left of
        // the cursor, but it did not match any of our
        // "opening" tokens - we may have run into
        //    procedure Test|(Some: Integer);
        // for instance.
        // If there was an identifier, keep checking to
        // the right of the cursor.
        if EditorLine <> '' then
        begin
          // Since we read an identifier and based our
          // position calculation on the assumption that
          // it was a "valid" opening token, we now
          // need to reverse the calculation of the
          // starting position here.
          SPosIncrement := Length(EditorLine);

          Parser.NextToken;
          StartToken := Parser.Token.ID;
          case Parser.Token.ID of
            tkBegin, tkTry, tkCase,
            tkFinally, tkExcept:    EndToken := tkEnd;
            tkRoundOpen:            EndToken := tkRoundClose;
            tkSquareOpen:           EndToken := tkSquareClose;
            tkRepeat:               EndToken := tkUntil;
          else
            MessageDlg(Format(SNotValidIdentifier, [TextToken]), mtError, [mbOK], 0);
            Exit;
          end;
        end
        else
        begin
          MessageDlg(Format(SNotValidIdentifier, [TextToken]), mtError, [mbOK], 0);
          Exit;
        end;
      end;  // case

      c := 1;

      while (c > 0) and (Parser.Token.ID <> tkNull) do
      begin
        Parser.NextToken;
        case StartToken of
          tkBegin, tkTry, tkCase, tkExcept, tkFinally:
            begin
              if Parser.Token.Id in [tkBegin, tkTry, tkCase] then
                Inc(c);
            end;

          tkRoundOpen, tkSquareOpen, tkRepeat:
            if Parser.Token.ID = StartToken then
              Inc(c);
        end;
        if Parser.Token.ID = EndToken then
          Dec(c);
      end;

      if c <> 0 then
      begin
        MessageDlg(SNoMatchingEndFound, mtInformation, [mbOK], 0);
        Exit;
      end;

      // Matching terminating delimiter found.

      // Now correct for any shift in the start position...
      Inc(SPos, SPosIncrement);

      EPos := SPos + Parser.RunPos;
      // ...and apply the same shift to the end position
      Dec(EPos, SPosIncrement);

      GetInterfaces(ModIntf, EditIntf);
      try
        if EditIntf <> nil then
        begin
          EList := TEditorStrings.Create(EditIntf);
          try
            SChar := EList.PosToCharPos(SPos);
            EChar := EList.PosToCharPos(EPos);

            DoDelimiterAction(EditIntf, Parser, SChar, EChar);
          finally
            EList.Free;
          end;
        end;
      finally
        EditIntf.Free;
        ModIntf.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

{ TMoveToDelimiter }

constructor TMoveToDelimiter.Create;
resourcestring
  SMoveToExpertName = 'Move to Matching Delimiter';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('V');
  FName := SMoveToExpertName;
  FButtonNo := 62;
  FHasConfigOptions := False;
end;

procedure TMoveToDelimiter.GetHelpString(List: TStrings);
begin
  List.Text := SDelimiterMoveToMessage + SDelimiterUsage;
end;

procedure TMoveToDelimiter.SaveSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    WriteInteger('MoveToDelimiter', 'ShortCut', ShortCut); // do not localize
  finally
    Free;
  end;
end;

procedure TMoveToDelimiter.LoadSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    ShortCut := ReadInteger('MoveToDelimiter', 'ShortCut', ShortCut);  // do not localize
  finally
    Free;
  end;
end;

procedure TMoveToDelimiter.DoDelimiterAction(EditIntf: TIEditorInterface;
  Parser: TmPasParser; SChar, EChar: TCharPos);
var
  CurrentView: Integer;
  EditView: TIEditView;
  EditPos: TEditPos;
  Line: Integer;
begin
  CurrentView := GetCurrentEditView(EditIntf);
  Assert((CurrentView >= 0) and (EditIntf.GetViewCount > 0));
  EditView := EditIntf.GetView(CurrentView);
  try
    EditView.ConvertPos(False, EditPos, EChar);
    EditPos.Col := EditPos.Col - Length(Parser.Token.Data);
    if EditPos.Col < 1 then EditPos.Col := 1;
    EditView.CursorPos := EditPos;
    if EditView.CursorPos.Line > EditView.TopPos.Line + EditView.ViewSize.cy then
    begin
      Line := EditView.CursorPos.Line - Editview.ViewSize.cy + 2;
      if Line <= 0 then Line := 1;
      EditPos.Line := Line;
      EditPos.Col := EditView.TopPos.Col;
      EditView.TopPos := EditPos;
    end;
  finally
    EditView.Free;
  end;
end;


{ TLocateDelimiter }

constructor TLocateDelimiter.Create;
resourcestring
  SLocateDelimiterName = 'Locate Matching Delimiter';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('O');
  FName := SLocateDelimiterName;
  FButtonNo := 61;
  FHasConfigOptions := False;
end;

procedure TLocateDelimiter.GetHelpString(List: TStrings);
begin
  List.Text := SDelimiterLocateMessage + SDelimiterUsage;
end;

procedure TLocateDelimiter.SaveSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    WriteInteger('LocateDelimiter', 'ShortCut', ShortCut);  // do not localize
  finally
    Free;
  end;
end;

procedure TLocateDelimiter.LoadSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    ShortCut := ReadInteger('LocateDelimiter', 'ShortCut', ShortCut);  // do not localize
  finally
    Free;
  end;
end;

procedure TLocateDelimiter.DoDelimiterAction(EditIntf: TIEditorInterface;
  Parser: TmPasParser; SChar, EChar: TCharPos);
var
  EditView: TIEditView;
  EditPos: TEditPos;
  CurrentView: Integer;
begin
  CurrentView := GetCurrentEditView(EditIntf);
  Assert((CurrentView >= 0) and (EditIntf.GetViewCount > 0));
  EditView := EditIntf.GetView(CurrentView);
  try
    EditView.ConvertPos(False, EditPos, SChar);
    EditView.CursorPos := EditPos;
  finally
    EditView.Free;
  end;
  SelectBlock(EditIntf, SChar, EChar);
end;

initialization
// Don't register these PAS only experts until we support BCB
{$IFNDEF GX_BCB}
  RegisterEditorExpert(TLocateDelimiter);
  RegisterEditorExpert(TMoveToDelimiter);
{$ENDIF GX_BCB}
end.

