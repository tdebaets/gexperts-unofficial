unit GX_eBlockTemplates;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++ Builder

uses
  SysUtils, Classes, GX_EditorExpert, GX_IDECodeTemplates;

type
  TBlockTemplateExpert = class(TEditorExpert)
  private
    FTemplatePath : string;
    FIdeTemplates : TIdeCodeTemplateList;
    function WrapBlockWithTemplate(const aCode, aTemplate : string) : string;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TFindDeclarationExpert = class(TEditorExpert)
  private
  public
    constructor Create; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
  end;

implementation

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
{$IFDEF GX_VER120_up} ActnList, {$ENDIF GX_VER120_up}
  Forms, Dialogs, ExptIntf, EditIntf, GX_EditRead, GX_uGenFunc,
  ExpertUtil, GX_EditWrt, Registry;

constructor TBlockTemplateExpert.Create;
resourcestring
  SBlockTemplateName = 'Insert Block Templates';
begin
  inherited Create;
  FIdeTemplates := TIdeCodeTemplateList.Create;
  if IsCppBuilderIde then
    FTemplatePath := ExtractFilePath(Application.ExeName) + 'bcb.dci'
  else
    FTemplatePath := ExtractFilePath(Application.ExeName) + 'delphi32.dci';
  ShortCut := scCtrl + scAlt + Ord('J');
  FName := SBlockTemplateName;
//  ButtonNo := 57;
  FHasConfigOptions := False;
end;

procedure TBlockTemplateExpert.GetHelpString(List: TStrings);
resourcestring
  SBlockTemplateHelp =
    '  This expert inserts a code template around the selected block of code. '+
    'To use it, select a block in the Delphi editor and activate this expert.';
begin
  List.Text := SBlockTemplateHelp;
end;

procedure TBlockTemplateExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    RegIni.WriteInteger('BlockTemplate', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TBlockTemplateExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger('BlockTemplate', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TBlockTemplateExpert.Execute;
resourcestring
  sBlockTemplateCaption = 'Block Templates';
  sBlockTemplatePrompt = 'Enter template name:';
var
  CodeBlock : String;
  ModIntf: TIModuleInterface;
  EditIntf: TIEditorInterface;
  EditRead: TEditReader;
//  EditView: TIEditView;
//  CharPos: TCharPos;
//  EditPos: TEditPos;
//  CurrentView: Integer;
  TemplateIdx : Integer;
  templateName : string;
  EditWrite: TEditWriter;
begin
  // reload the template list so it is always current
  FIdeTemplates.LoadFromFile(FTemplatePath);
  // now prompt the user for a template name
  if InputQuery(sBlockTemplateCaption,
                sBlockTemplatePrompt, templateName) then
  begin
    TemplateIdx := FIdeTemplates.IndexOf(templateName);
    if TemplateIdx = -1 then
      Exit;
  {$IFOPT D+} SendDebug('Executing block template expert'); {$ENDIF}
    try
      EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
      try
        {$IFOPT D+} SendDebug('Calling EditRead.GetBlock'); {$ENDIF}
        CodeBlock := EditRead.GetBlock;
        {$IFOPT D+} SendDebug('Selected Text: ' + CodeBlock); {$ENDIF}
      finally
        EditRead.Free;
      end;
      if CodeBlock = '' then
      begin
        // just insert the template
        EditWrite := TEditWriter.Create(ToolServices.GetCurrentFile);
        try
          EditWrite.WriteAtCurrentPos(FIdeTemplates.Templates[TemplateIdx].Code.Text);
        finally
          EditWrite.Free;
        end;
      end
      else
      begin
        // our codeblock is not empty so stick it into the template
        // at the '|' character
        CodeBlock := WrapBlockWithTemplate(CodeBlock, FIdeTemplates.Templates[TemplateIdx].Code.Text);
        try
          GetInterfaces(ModIntf, EditIntf);
          if EditIntf <> nil then
          begin
            {$IFOPT D+} SendDebug('Calling ReplaceSelection'); {$ENDIF}
            if Pos(#13, CodeBlock) = 0 then
              ReplaceSelection(EditIntf, 0, TrimRight(CodeBlock))
            else
            begin
              ReplaceSelection(EditIntf, 0, CodeBlock);
(*
              // WARNING: This is a hack.
              // ReplaceSelection has a problem with appending an
              // unwanted CRLF in some specific situations - see
              // the comments in ExpertUtil.ReplaceSelection for more.
              // To work around this, we cut off the CRLF right here
              // and do not make the editor interface replace it.
              // The clean solution is just to call
              //    ReplaceSelection(EditIntf, 0, CodeBlock);
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
              finally
                EditView.Free;
              end;
              if (Length(CodeBlock) > 1) and (EditPos.Col > 1) and
                 (Copy(CodeBlock, Length(CodeBlock)-1, 2) = #13#10) then
                ReplaceSelection(EditIntf, 0, Copy(CodeBlock, 1, Length(CodeBlock)-2))
              else
                ReplaceSelection(EditIntf, 0, CodeBlock);
*)
            end;
          end;
        finally
          EditIntf.Free;
          ModIntf.Free;
        end;
      end;
    except
      on E: Exception do
        ShowExceptionErrorMessage(E);
    end;
  end;
end;

destructor TBlockTemplateExpert.Destroy;
begin
  FIdeTemplates.Free;
  inherited Destroy;
end;

function TBlockTemplateExpert.WrapBlockWithTemplate(const aCode,
  aTemplate: string): string;
var
  UsePos, TildePos, BangPos : integer;
  function SkipChars : integer;
  begin
    result := 1;
    if (UsePos < Length(aTemplate)) and
       (aTemplate[UsePos+1] = #13) then
      inc(result, 2);
  end;
begin
  TildePos := Pos('~', aTemplate);
  BangPos  := Pos('|', aTemplate);
  if (TildePos = 0) and (BangPos = 0) then
    result := aCode + #13#10 + aTemplate
  else
  begin
    if TildePos > 0 then
      UsePos := TildePos
    else
      UsePos := BangPos;
    result := Copy(aTemplate, 1, UsePos - 1) +
              aCode +
              Copy(aTemplate, UsePos + SkipChars, Length(aTemplate));
  end;
end;

{ TFindDeclarationExpert }

constructor TFindDeclarationExpert.Create;
resourcestring
  SFindDeclarationName = 'Find Declaration';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('R');
  FName := SFindDeclarationName;
//  ButtonNo := 57;
  FHasConfigOptions := False;
end;

procedure TFindDeclarationExpert.Execute;
var
  tmpCmp : TComponent;
begin
// execute the Find Declaration action
  tmpCmp := Application.FindComponent('EditorActionLists');
  if assigned(tmpCmp) then
  begin
    tmpCmp := tmpCmp.FindComponent('FindDeclaration');
    if assigned(tmpCmp) and (tmpCmp is TAction) then
      TAction(tmpCmp).Execute;
  end;
end;

procedure TFindDeclarationExpert.GetHelpString(List: TStrings);
resourcestring
  SFindDeclarationHelp =
    '  This expert finds the declaration of the current identifier.';
begin
  List.Text := SFindDeclarationHelp;
end;

procedure TFindDeclarationExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger('FindDeclaration', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TFindDeclarationExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    RegIni.WriteInteger('FindDeclaration', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

initialization
  RegisterEditorExpert(TBlockTemplateExpert);
{$IFDEF GX_VER120_up}
  RegisterEditorExpert(TFindDeclarationExpert);
{$ENDIF GX_VER120_up}
end.

