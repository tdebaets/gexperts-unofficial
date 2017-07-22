unit GX_eComment;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GX_EditorExpert, StdCtrls;

type
  TCommentType = (ctSlash, ctC, ctPascal, ctCpp);

type
  TCommentExpert = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Configure; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TUnCommentExpert = class(TEditorExpert)
  public
    constructor Create; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

type
  TfmCommentConfig = class(TForm)
    GroupBox1: TGroupBox;
    rbSlash: TRadioButton;
    rbC: TRadioButton;
    rbPascal: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    rbCpp: TRadioButton;
    chkInsertSpace: TCheckBox;
  private
  end;

implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  ExptIntf, EditIntf, GX_EditReader, GX_GenFunc, ExpertUtil,
  Menus, Dialogs, Registry;

var
  // this is *local* and used by both the comment
  // and the uncomment expert...
  CommentType: TCommentType = ctSlash;
  InsertRemoveSpace: Boolean = False;

resourcestring
  SCommentSourceMismatch = 'The current source code appears not to be compatible '+
                           'with the chosen comment style "%s".'#13+
                           'Proceeding might introduce syntactical errors in your source code.'#13#13+
                           'Do you want to proceed?';

const
  CommentStyle: array[TCommentType] of string =
    ('//', '(* *)', '{ }', '/* */');


function CommentAndSourceCompatible(SyntaxHighlighter: TSyntaxHighlighter): Boolean;
var
  CommentStyleMatchesSource: Boolean;
begin
  Result := True;

  case SyntaxHighlighter of
  {$IFNDEF VER100}
    shC:
      CommentStyleMatchesSource := not (CommentType in [ctC, ctPascal]);
  {$ENDIF VER100}

    shPascal:
      CommentStyleMatchesSource := (CommentType <> ctCpp);
  else
    CommentStyleMatchesSource := False;
  end;

  if not CommentStyleMatchesSource then
  begin
    if MessageDlg(Format(SCommentSourceMismatch, [CommentStyle[CommentType]]), mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Result := False;
  end;
end;

constructor TCommentExpert.Create;
resourcestring
  SCommentName = 'Comment Code';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('C');
  FName := SCommentName;
  FButtonNo := 57;
end;

procedure TCommentExpert.GetHelpString(List: TStrings);
resourcestring
  SCommentHelp =
      '  This expert comments out a selected block of code. '+
      'To use it, select a block in the Delphi editor and '+
      'activate this expert.'+
      #13#10+
      '  You can configure this expert to use different comment styles.';
begin
  List.Text := SCommentHelp;
end;

procedure TCommentExpert.Configure;
var
  Dlg: TfmCommentConfig;
begin
  Dlg := TfmCommentConfig.Create(Application);
  try
    with Dlg do
    begin
      case CommentType of
        ctSlash:  rbSlash.Checked := True;
        ctC:      rbC.Checked := True;
        ctPascal: rbPascal.Checked := True;
        ctCpp:    rbCpp.Checked := True;
      end;
      chkInsertSpace.Checked := InsertRemoveSpace;

      if ShowModal = mrOK then
      begin
        if rbSlash.Checked then
          CommentType := ctSlash
        else
        if rbC.Checked then
          CommentType := ctC
        else
        if rbCpp.Checked then
          CommentType := ctCpp
        else
          CommentType := ctPascal;

        InsertRemoveSpace := chkInsertSpace.Checked;

        SaveSettings;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TCommentExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    RegIni.WriteInteger('Comment', 'ShortCut', ShortCut);
    RegIni.WriteBool('Comment', 'InsertRemoveSpace', InsertRemoveSpace);
    RegIni.WriteInteger('Comment', 'CommentType', Ord(CommentType));
  finally
    RegIni.Free;
  end;
end;

procedure TCommentExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger('Comment', 'ShortCut', ShortCut);
    InsertRemoveSpace := RegIni.ReadBool('Comment', 'InsertRemoveSpace', InsertRemoveSpace);
    CommentType := TCommentType(RegIni.ReadInteger('Comment', 'CommentType', 0));
  finally
    RegIni.Free;
  end;
end;

procedure TCommentExpert.Execute;
var
  CodeList: TStringList;

    procedure AddBracketing(const LeftBracket, RightBracket: string);
    begin
      if InsertRemoveSpace then
      begin
        CodeList[0] := LeftBracket + ' ' + CodeList[0];
        CodeList[CodeList.Count - 1] := CodeList[CodeList.Count - 1] + ' ' + RightBracket;
      end
      else
      begin
        CodeList[0] := LeftBracket + CodeList[0];
        CodeList[CodeList.Count - 1] := CodeList[CodeList.Count - 1] + RightBracket;
      end;
    end;

var
  ModIntf: TIModuleInterface;
  EditIntf: TIEditorInterface;
  EditRead: TEditReader;
  EditView: TIEditView;
  CharPos: TCharPos;
  EditPos: TEditPos;
  i, CurrentView: Integer;
begin
  {$IFOPT D+} SendDebug('Executing comment expert'); {$ENDIF}
  CodeList := TStringList.Create;
  try
    try
      // Since this edit reader is destroyed almost
      // immediately, do not call FreeFileData
      EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
      try
        if not CommentAndSourceCompatible(EditRead.SyntaxHighlighter) then
          Exit;
        {$IFOPT D+} SendDebug('Calling EditRead.GetBlock'); {$ENDIF}
        CodeList.Text := EditRead.GetBlock;
        {$IFOPT D+} SendDebug('Selected Text: '+CodeList.Text); {$ENDIF}
        if CodeList.Count = 0 then Exit;
        // do not localize any of the below lines
        case CommentType of
          ctSlash:
            begin
              if InsertRemoveSpace then
              begin
                for i := 0 to CodeList.Count-1 do
                  CodeList[i] := '// ' + CodeList[i];
              end
              else
              begin
                for i := 0 to CodeList.Count-1 do
                  CodeList[i] := '//' + CodeList[i];
              end;
            end;
          ctC:
            AddBracketing('(*', '*)');
          ctCpp:
            AddBracketing('/*', '*/');
          ctPascal:
            AddBracketing('{', '}');
        end;
      finally
        EditRead.Free;
      end;
      try
        GetInterfaces(ModIntf, EditIntf);
        if EditIntf <> nil then
        begin
          {$IFOPT D+} SendDebug('Calling ReplaceSelection'); {$ENDIF}
          if CodeList.Count = 1 then
            ReplaceSelection(EditIntf, 0, TrimRight(CodeList.Text))
          else
          begin
            // WARNING: This is a hack.
            // ReplaceSelection has a problem with appending an
            // unwanted CRLF in some specific situations - see
            // the comments in ExpertUtil.ReplaceSelection for more.
            // To work around this, we cut off the CRLF right here
            // and do not make the editor interface replace it.
            // The clean solution is just to call
            //    ReplaceSelection(EditIntf, 0, CodeList.Text);
            // and have the problem solved elsewhere
            // We don't remove the CRLF if the block ends in the first column,
            // because then we delete a necessary CRLF
            CurrentView := GetCurrentEditView(EditIntf);
            Assert((CurrentView >= 0) and (EditIntf.GetViewCount > 0));
            CharPos.Line := EditIntf.BlockAfter.Line;
            CharPos.CharIndex := EditIntf.BlockAfter.CharIndex;
            EditView := EditIntf.GetView(CurrentView);
            try
              EditView.ConvertPos(False, EditPos, CharPos);
              if (Length(CodeList.Text) > 1) and (EditPos.Col > 1) and (Copy(CodeList.Text, Length(CodeList.Text)-1, 2) = #13#10) then
                ReplaceSelection(EditIntf, 0, Copy(CodeList.Text, 1, Length(CodeList.Text)-2))
              else
                ReplaceSelection(EditIntf, 0, CodeList.Text);
            finally
              EditView.Free;
            end;
          end;
        end;
      finally
        EditIntf.Free;
        ModIntf.Free;
      end;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  finally
    CodeList.Free;
  end;
end;

constructor TUnCommentExpert.Create;
resourcestring
  SUncommentName = 'Uncomment Code';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('U');
  FName := SUncommentName;
  FButtonNo := 58;
  FHasConfigOptions := False;
end;

procedure TUnCommentExpert.GetHelpString(List: TStrings);
resourcestring
  SUncommentHelp = '  This expert uncomments a selected block of code.  '+
                   'To use it, select a block in the IDE code editor and ' +
                   'activate this expert.' +
                   #13#10+
                   '  Uncommenting is performed using the comment style that '+
                   'you selected for the Comment Code editor expert.';

begin
  List.Text := SUncommentHelp;
end;

procedure TUnCommentExpert.SaveSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    WriteInteger('UnComment', 'ShortCut', ShortCut); // do not localize
  finally
    Free;
  end;
end;

procedure TUnCommentExpert.LoadSettings;
begin
  with TRegIniFile.Create(BaseRegistryKey) do
  try
    ShortCut := ReadInteger('UnComment', 'ShortCut', ShortCut); // do not localize
  finally
    Free;
  end;
end;

procedure TUnCommentExpert.Execute;

  function RemoveFirstString(const SubString, InString: string): string;
  var
    Success: Boolean;

      function RemoveFirstInternal(const SubString, InString: string): string;
      var
        SubStringPos: Integer;
      begin
        if StrLComp(PChar(Trim(InString)), PChar(SubString), Length(SubString)) = 0 then
        begin
          SubStringPos := Pos(SubString, InString);
          if SubStringPos > 1 then
          begin
            Result := Copy(InString, 1, SubStringPos - 1) +
                      Copy(InString, SubStringPos + Length(SubString), Length(InString))
          end
          else
            Result := Copy(InString, Length(SubString) + 1, Length(InString));

          Success := True;
        end
        else
          Result := InString;
      end;

  begin
    Success := False;
    // if spaces are to be removed, try to first kill
    // WITH space, otherwise continue with default
    // comment removal
    if InsertRemoveSpace then
    begin
      Result := RemoveFirstInternal(SubString + ' ', InString);
      if Success then
        Exit;
    end;

    Result := RemoveFirstInternal(SubString, InString);
  end;


  function RemoveLastString(const SubString, InString: string): string;
  var
    Success: Boolean;

      function RemoveLastInternal(const SubString, InString: string): string;
      var
        SubStringStartPos: Integer;
        TempString: string;
      begin
        TempString := TrimRight(InString);

        SubStringStartPos := Length(TempString) - Length(SubString) + 1;

        if SubString = Copy(TempString, SubStringStartPos, Length(SubString)) then
        begin
          Result := Copy(TempString, 1, SubStringStartPos-1);
          Success := True;
        end
        else
          Result := InString;
      end;

  begin
    Success := False;
    // if spaces are to be removed, try to first kill
    // WITH space, otherwise continue with default
    // comment removal
    if InsertRemoveSpace then
    begin
      Result := RemoveLastInternal(' ' + SubString, InString);
      if Success then
        Exit;
    end;

    Result := RemoveLastInternal(SubString, InString);
  end;

var
  ModIntf: TIModuleInterface;
  EditIntf: TIEditorInterface;
  EditRead: TEditReader;
  CodeList: TStringList;
  EditView: TIEditView;
  CharPos: TCharPos;
  EditPos: TEditPos;
  i, CurrentView: Integer;
begin
  {$IFOPT D+} SendDebug('Executing uncomment expert'); {$ENDIF}
  CodeList := TStringList.Create;
  try
    try
      // Since this edit reader is destroyed almost
      // immediately, do not call FreeFileData
      EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
      try
        CodeList.Text := EditRead.GetBlock;
        {$IFOPT D+} SendDebug('Selected Text: '+CodeList.Text); {$ENDIF}
        if CodeList.Count = 0 then exit;
        case CommentType of
          ctSlash:
            for i := 0 to CodeList.Count-1 do
              CodeList[i] := RemoveFirstString('//', CodeList[i]);
          ctC:
            begin
              CodeList[0] := RemoveFirstString('(*', CodeList[0]);
              CodeList[CodeList.Count-1] := RemoveLastString('*)', CodeList[CodeList.Count-1]);
            end;
          ctCpp:
            begin
              CodeList[0] := RemoveFirstString('/*', CodeList[0]);
              CodeList[CodeList.Count-1] := RemoveLastString('*/', CodeList[CodeList.Count-1]);
            end;
          ctPascal:
            begin
              CodeList[0] := RemoveFirstString('{', CodeList[0]);
              CodeList[CodeList.Count-1] := RemoveLastString('}', CodeList[CodeList.Count-1]);
            end;
        end;
      finally
        EditRead.Free;
      end;
      try
        GetInterfaces(ModIntf, EditIntf);
        if EditIntf <> nil then
        begin
          if CodeList.Count = 1 then
            ReplaceSelection(EditIntf, 0, TrimRight(CodeList.Text))
          else
          begin
            // WARNING: This is a hack.
            // ReplaceSelection has a problem with appending an
            // unwanted CRLF in some specific situations - see
            // the comments in ExpertUtil.ReplaceSelection for more.
            // To work around this, we cut off the CRLF right here
            // and do not make the editor interface replace it.
            // The clean solution is just to call
            //    ReplaceSelection(EditIntf, 0, CodeList.Text);
            // and have the problem solved elsewhere
            CurrentView := GetCurrentEditView(EditIntf);
            Assert((CurrentView >= 0) and (EditIntf.GetViewCount > 0));
            CharPos.Line := EditIntf.BlockAfter.Line;
            CharPos.CharIndex := EditIntf.BlockAfter.CharIndex;
            EditView := EditIntf.GetView(CurrentView);
            try
              EditView.ConvertPos(False, EditPos, CharPos);
              if (Length(CodeList.Text) > 1) and (EditPos.Col > 1) and (Copy(CodeList.Text, Length(CodeList.Text)-1, 2) = #13#10) then
                ReplaceSelection(EditIntf, 0, Copy(CodeList.Text, 1, Length(CodeList.Text)-2))
              else
                ReplaceSelection(EditIntf, 0, CodeList.Text);
            finally
              EditView.Free;
            end;
          end;
        end;
      finally
        EditIntf.free;
        ModIntf.Free;
      end;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  finally
    CodeList.Free;
  end;
end;

initialization
  RegisterEditorExpert(TCommentExpert);
  RegisterEditorExpert(TUnCommentExpert);
end.

