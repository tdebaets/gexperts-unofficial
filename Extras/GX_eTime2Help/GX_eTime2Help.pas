{:This unit implements an editor expert that inserts
<strong>Time2Help (tm)</strong> (<a href="www.time2help.com">www.time2help.com</a>)
JavaDoc comments.
<p>These comments are processed to produced on-line documentation (WinHelp
or Html).
<p>As stated in T2H documentation :
<i><font Color="blue">
<p>Documentation written in the source code itself must be inside comments,
and in order for Time2HELP not to confuse what comments is meant for it,
and what is only meant for other programmers to read (and formatting comments
etc), Time2HELP looks for comments starting with a "magic" symbol.
We (Time2Help) allow ':' and '**' as "magic symbols" (the latter is taken
from JavaDoc).
<p>The comment must be located before the item in question.
</font></i>
<p>The comment is inserted with a sample description text.
<p>If the code after the insertion point is a routine header with arguments, the
corresponding <strong>@param</strong> tags will be generated, including the
name of the argument and a sample description text.
<p>If the code after the insertion point is a function,
the <strong>@returns</strong> tag will be generated, with a sample description
text.
<p><hr>
Warning :<p>
The default procedure header generates a "conflict", since it starts with
the sequence "//**", and following text included in generated help. If you
want to avoid this, just change this header, by inserting a space after the
slashes for instance. Maybe the default procedure comment could be changed
according to this ?
<p><hr>
@author Eric Pascual (eric_pascual@csi.com) - ANALIS S.A.
}
unit GX_eTime2Help;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, GX_EditorExpert;

type
  {:The Time2Help comments expert, that will be available as a
  GExperts editor expert.}
  TTime2HelpExpert = class (TEditorExpert)
  private
    FWithRaises : Boolean;
    procedure SetWithRaises (const Value : Boolean);
    function MakeTime2HelpComment : String;
  public
    constructor Create; override;
    destructor Destroy; override;
    {:Display and manages the expert configuration dialog.}
    procedure Configure; override;
    {:Does the job}
    procedure Execute; override;
    {:Saves settings to the registry}
    procedure SaveSettings; override;
    {:Loads settings from the registry}
    procedure LoadSettings; override;
    {:Returns expert description text
      @param List the return StringList for the text
    }
    procedure GetHelpString (List : TStrings); override;
    {:If True, a @raises item will be automatically inserted for
    routines.}
    property WithRaises : Boolean read FWithRaises write SetWithRaises;
  end;

type
  {:The expert configuration dialog box.}
  TfmT2HExpOptions = class (TForm)
    btnOK : TButton;
    btnCancel : TButton;
    chkRaises : TCheckBox;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

uses
  GX_EditRead,
  GX_EditWrt,
  GX_uGenFunc,
  EditIntf,
  ToolIntf,
  ExptIntf,
  mPasLex,
  Registry
  ;

resourcestring
  SHeaderMessage    =
    'This expert enables you to add a Time2Help inline documentation headers' +
    ' corresponding to the source code that is at the cursor position.' +
    #13#10;
  // Note : underscores have been used in place of spaces in following
  // strings so that a single double-click will select the whole to-be-replaced
  // text.
  SDescrHere        = 'short_description_text_here';
  SDescrVerbose     = 'more_verbose_description_here';
  SException        = 'exception_name';

const
  // do not localize any of the below items
  REG_ROOT_NAME     = 'Time2HelpExpert';
  REK_KEY_SHORTCUT  = 'ShortCut';
  REK_KEY_W_RAISES  = 'WithRaises';

  // ******************* TTime2HelpExpert.Create *************************
constructor TTime2HelpExpert.Create;
resourcestring
  SExpertName       = 'Insert Time2Help(tm) documentation comment';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord ('D');
  FName := SExpertName;
  FHasConfigOptions := True;
  WithRaises := True;
end;

// ******************* TTime2HelpExpert.Destroy *************************
destructor TTime2HelpExpert.Destroy;
begin
  inherited Destroy;
end;

// ******************* TTime2HelpExpert.GetHelpString *************************
procedure TTime2HelpExpert.GetHelpString (List : TStrings);
begin
  List.Text := SHeaderMessage;
end;

// ******************* TTime2HelpExpert.SaveSettings *************************
procedure TTime2HelpExpert.SaveSettings;
var
  RegIni            : TRegIniFile;
begin
  RegIni := TRegIniFile.Create (BaseRegistryKey);
  try
    RegIni.WriteInteger (REG_ROOT_NAME, REK_KEY_SHORTCUT, ShortCut);
    RegIni.WriteBool (REG_ROOT_NAME, REK_KEY_W_RAISES, FWithRaises);
  finally
    RegIni.Free;
  end;
end;

// ******************* TTime2HelpExpert.LoadSettings *************************
procedure TTime2HelpExpert.LoadSettings;
var
  RegIni            : TRegIniFile;
begin
  RegIni := TRegIniFile.Create (BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger (REG_ROOT_NAME, REK_KEY_SHORTCUT, ShortCut);
    WithRaises := RegIni.ReadBool (REG_ROOT_NAME, REK_KEY_W_RAISES,
      WithRaises);
  finally
    RegIni.Free;
  end;
end;

// ******************* TTime2HelpExpert.MakeTime2HelpComment *************************
function TTime2HelpExpert.MakeTime2HelpComment : String;
const
  ROUTINE_TOKENS    = [tkProcedure, tkFunction, tkConstructor, tkDestructor];
var
  Parser            : TmwPasLex;
  EditRead          : TEditReader;
  MemStream         : TMemoryStream;
  Indent            : Integer;
  IndentStr         : String;
  i                 : Integer;
  IsAFunction       : Boolean;
  HasArgs           : Boolean;
begin
  Result := '';
  MemStream := TMemoryStream.Create;
  try
    EditRead := TEditReader.Create (ToolServices.GetCurrentFile);
    try
      Indent := EditRead.GetCurrentPos.Col;
      IndentStr := '';
      for i := 1 to Indent - 1 do
        IndentStr := IndentStr + ' ';
      EditRead.SaveToStreamFromPos (MemStream);
    finally
      EditRead.Free;
    end;
    Parser := TmwPasLex.Create;
    try
      Parser.Origin := MemStream.Memory;
      // get Next significant token
      Parser.NextNoJunk;
      if Parser.TokenID in ROUTINE_TOKENS then
      begin
        IsAFunction := (Parser.TokenID = tkFunction);
        // no indentation insertion before start, since the cursor is
        // already at the right column
        Result := '{:' + SDescrHere + #13#10;
        // skip all stuff before Open paren, or colon, or semi-colon
        repeat
          Parser.NextNoJunk;
        until Parser.TokenId in [tkRoundOpen, tkColon, tkSemiColon];

        // see if an argument list is there
        HasArgs := (Parser.TokenID = tkRoundOpen);

        // if an arguments list exists, extract it
        if HasArgs then
        begin
          while Parser.TokenID <> tkRoundClose do
          begin
            // iterate on comma separated list of identifiers
            repeat
              // get next argument identifier
              Parser.NextNoJunk;
              if Parser.TokenId in [tkConst, tkVar] then Parser.NextNoJunk;
              Result := Result + IndentStr +
                Format ('  @param %s %s', [Parser.Token, SDescrHere]) +
                #13#10;
              Parser.NextNoJunk;
            until Parser.TokenId = tkColon;
            // now we are on the type delimiter
            // => skip its definition until Next semi-colon or Close paren
            repeat
              Parser.NextNoJunk;
            until Parser.TokenId in [tkSemiColon, tkRoundClose, tkNull];
          end;
        end;

        // if we are on a function, generate the @result
        if IsAFunction then
          Result := Result + IndentStr +
            Format ('  @returns %s', [SDescrHere]) + #13#10;

        // add the @raises if we are processing a routine, and if the option
        // is set
        if WithRaises then
          Result := Result + IndentStr +
            Format ('  @raises %s %s', [SException, SDescrHere]) + #13#10;

        // Close the comment now
        // If there is no argument and no result and no raises, remove the
        // linefeed that has been put after sample description text
        if not (HasArgs or IsAFunction or WithRaises) then
        begin
          Delete (Result, Length (Result) - 1, 2);
          Result := Result + '}';
        end
        else
          Result := Result + IndentStr + '}';
      end
      else
        if Parser.TokenId = tkUnit then
          Result :=
            '{:' + SDescrHere + #13#10 +
            IndentStr + '<p>' + SDescrVerbose + #13#10 +
            '}'
        else
          Result := '{:' + SDescrHere + '}';
    finally
      Parser.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

// ******************* TTime2HelpExpert.Execute *************************
procedure TTime2HelpExpert.Execute;
var
  EditWrite         : TEditWriter;
  InsertString      : String;
  DoParseSource     : Boolean;
begin
  try
    EditWrite := TEditWriter.Create (ToolServices.GetCurrentFile);
    try
      DoParseSource := (EditWrite.SyntaxHighlighter = shPascal);
      EditWrite.GotoCurrentPos;
      if DoParseSource then
        InsertString := MakeTime2HelpComment
      else
        InsertString := '';
      EditWrite.WriteAtCurrentPos (InsertString);
    finally
      EditWrite.Free;
    end;
  except
    on E : Exception do
      ShowExceptionErrorMessage (E);
  end;
end;

// ******************* TTime2HelpExpert.SetWithRaises *************************
procedure TTime2HelpExpert.SetWithRaises (const Value : Boolean);
begin
  FWithRaises := Value;
end;

// ******************* TTime2HelpExpert.Configure *************************
procedure TTime2HelpExpert.Configure;
begin
  with TfmT2HExpOptions.Create (nil) do
  try
    chkRaises.Checked := WithRaises;
    if ShowModal = mrOK then
      WithRaises := chkRaises.Checked;
  finally
    Release;
  end;
end;

initialization
  RegisterEditorExpert (TTime2HelpExpert);
end.

