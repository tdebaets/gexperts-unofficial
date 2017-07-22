unit GX_eHeader;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder
    // ... although the use of the word "procedure" is not that appropriate

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, GX_EditorExpert;

type
  TUnitHeaderExpert = class(TEditorExpert)
  private
    FHeader: TStrings;
    procedure SetHeader(New: TStrings);
    procedure FillDefault;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
    property Header: TStrings read FHeader write SetHeader;
  end;

type
  TProcedureHeaderExpert = class(TEditorExpert)
  private
    FHeader: TStrings;
    procedure SetHeader(New: TStrings);
    procedure FillDefault;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Configure; override;
    procedure Execute; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
    procedure GetHelpString(List: TStrings); override;
    property Header: TStrings read FHeader write SetHeader;
  end;

type
  TfmHeaderFormat = class(TForm)
    meHeader: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
  end;

implementation

{$R *.DFM}

uses
  EditIntf, ExptIntf, ToolIntf,
  Menus, Registry, Dialogs,
  GX_ConfigurationInfo, GX_GExperts, GX_Substitute, GX_GenFunc, GX_EditWriter;

resourcestring
  SHeaderMessage =
    '  This expert enables you to add custom headers to your %s.  '+
    'The header is completely configurable and various substitution macros are '+
    'available to insert useful information into the header.'+
    #13#10+
    'The macros available are:'+
    #13#10;

  SHeaderMessage2 =
    '  %PROJECTDIR% - Project directory'+#13#10+
    '  %PROJECTNAME% - Project name'+#13#10+
    {$IFDEF GX_VER120_up}
    '  %PROJECTGROUPDIR% - Project group directory'+#13#10+
    '  %PROJECTGROUPNAME% - Project group name'+#13#10+
    {$ENDIF GX_VER120_up}
    '  %UNIT% - Unit Name'+#13#10+
    '  %DATETIME% - Date and time'+#13#10+
    '  %DATE% - Date'+#13#10+
    '  %YEAR%, %MONTH%, %DAY% - Year, month, or day'+#13#10+
    '  %MONTHSHORTNAME%, %MONTHLONGNAME% - Month names'+#13#10+
    '  %DAYSHORTNAME%, %DAYLONGNAME% - Day names'+#13#10+
    '  %HOUR%, %MINUTE%, %SECOND% - Hour, minute, or second';

{*********************** TUnitHeaderExpert *******************}

constructor TUnitHeaderExpert.Create;
resourcestring
  SUnitHeaderName = 'Insert Unit Header';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('H');
  FName := SUnitHeaderName;
  FHeader := TStringList.Create;
  FButtonNo := 59;
end;

destructor TUnitHeaderExpert.Destroy;
begin
  FHeader.Free;
  FHeader := nil;
  inherited Destroy;
end;

procedure TUnitHeaderExpert.GetHelpString(List: TStrings);
resourcestring
  SUnits = 'units';
begin
  List.Text := Format(SHeaderMessage, [SUnits]) + SHeaderMessage2;
end;

procedure TUnitHeaderExpert.SetHeader(New: TStrings);
begin
  FHeader.Assign(New);
end;

procedure TUnitHeaderExpert.FillDefault;
resourcestring
  SDefaultHeader =
      '{***************************************************************'#13#10+
      ' *'#13#10+
      ' * Unit Name: %UNIT%'#13#10+
      ' * Purpose  :'#13#10+
      ' * Author   :'#13#10+
      ' * History  :'#13#10+
      ' *'#13#10+
      ' ****************************************************************}';
begin
  FHeader.Add(SDefaultHeader);
end;

procedure TUnitHeaderExpert.Configure;
begin
  with TfmHeaderFormat.Create(nil) do
  try
    meHeader.Lines.Assign(FHeader);
    if ShowModal = mrOK then
      FHeader.Assign(meHeader.Lines);
  finally
    Free;
  end;
end;

procedure TUnitHeaderExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    WriteStringList(RegIni, FHeader, 'HeaderExpert', 'Line');
    RegIni.WriteInteger('HeaderExpert', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TUnitHeaderExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger('HeaderExpert', 'ShortCut', ShortCut);
    ReadStringList(RegIni, FHeader, 'HeaderExpert', 'Line');
    if FHeader.Count = 0 then FillDefault;
  finally
    RegIni.Free;
  end;
end;

procedure TUnitHeaderExpert.Execute;
var
  EditWrite: TEditWriter;
  InsertString: string;
  DoParseSource: Boolean;
begin
  try
    EditWrite := TEditWriter.Create(ToolServices.GetCurrentFile);
    try
      DoParseSource := (EditWrite.SyntaxHighlighter = shPascal);
      InsertString := ReplaceStrings(FHeader.Text + #13#10, DoParseSource);
      EditWrite.Insert(InsertString);
      EditWrite.ScrollToTopOfFile;
    finally
      EditWrite.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

{*********************** TProcedureHeaderExpert *******************}

constructor TProcedureHeaderExpert.Create;
resourcestring
  SProcedureHeaderName = 'Insert Procedure Header';
begin
  inherited Create;
  ShortCut := scCtrl + scAlt + Ord('P');
  FName := SProcedureHeaderName;
  FHeader := TStringList.Create;
  FButtonNo := 60;
end;

destructor TProcedureHeaderExpert.Destroy;
begin
  FHeader.Free;
  FHeader := nil;
  inherited Destroy;
end;

procedure TProcedureHeaderExpert.GetHelpString(List: TStrings);
resourcestring
  SProcedures = 'procedures and functions';
  SProcedureHelpString = '  %PROCNAME% - Name of the next procedure after the cursor'+#13#10;
begin
  List.Text := Format(SHeaderMessage, [SProcedures]) + SProcedureHelpString +
               SHeaderMessage2;
end;

procedure TProcedureHeaderExpert.SetHeader(New: TStrings);
begin
  FHeader.Assign(New);
end;

procedure TProcedureHeaderExpert.FillDefault;
resourcestring
  SDefaultProcedureHeader = '//******************* %ProcName% *************************';
begin
  FHeader.Add(SDefaultProcedureHeader);
end;

procedure TProcedureHeaderExpert.Configure;
begin
  with TfmHeaderFormat.Create(nil) do
  try
    meHeader.Lines.Assign(FHeader);
    if ShowModal = mrOK then
      FHeader.Assign(meHeader.Lines);
  finally
    Free;
  end;
end;

procedure TProcedureHeaderExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    WriteStringList(RegIni, FHeader, 'ProcHeaderExpert', 'Line');
    RegIni.WriteInteger('ProcHeaderExpert', 'ShortCut', ShortCut);
  finally
    RegIni.Free;
  end;
end;

procedure TProcedureHeaderExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  // do not localize any of the below items
  RegIni := TRegIniFile.Create(BaseRegistryKey);
  try
    ShortCut := RegIni.ReadInteger('ProcHeaderExpert', 'ShortCut', ShortCut);
    ReadStringList(RegIni, FHeader, 'ProcHeaderExpert', 'Line');
    if FHeader.Count = 0 then FillDefault;
  finally
    RegIni.Free;
  end;
end;

procedure TProcedureHeaderExpert.Execute;
var
  EditWrite: TEditWriter;
  InsertString: string;
  DoParseSource: Boolean;
begin
  try
    EditWrite := TEditWriter.Create(ToolServices.GetCurrentFile);
    try
      DoParseSource := (EditWrite.SyntaxHighlighter = shPascal);
      EditWrite.GotoCurrentPos;
      InsertString := ReplaceStrings(FHeader.Text, DoParseSource);
      if Length(InsertString) > 0 then
      begin
        if InsertString[Length(InsertString) - 1] = #13 then
          Delete(InsertString, Length(InsertString) - 1, 2); // assumes #13#10 !!
      end;
      EditWrite.Insert(InsertString);
    finally
      EditWrite.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TfmHeaderFormat.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

initialization
  RegisterEditorExpert(TUnitHeaderExpert);
  RegisterEditorExpert(TProcedureHeaderExpert);
end.

