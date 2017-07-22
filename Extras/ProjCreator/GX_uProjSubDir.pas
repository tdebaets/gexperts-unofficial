unit GX_uProjSubDir;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++ Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ToolIntf, ExptIntf, FileCtrl, GX_GExperts,
  GX_uExperts, Registry, CheckLst, ToolsAPI,
  EditIntf;

type
  TProjSubDirExpert = class;

  TfmProjSubDir = class(TForm)
    gbxDirectories: TGroupBox;
    lbDirs: TListBox;
    btnSelectDir: TButton;
    btnCancel: TButton;
    btnCreate: TButton;
    btnAddSubDir: TButton;
    btnRemoveDir: TButton;
    btnHelp: TButton;
    gbxSubDir: TGroupBox;
    clbSubDir: TCheckListBox;
    gbxPersonal: TGroupBox;
    lblAuthor: TLabel;
    lblCompany: TLabel;
    lblCopyright: TLabel;
    edAuthor: TEdit;
    edCompany: TEdit;
    edCopyright: TEdit;
    btnRemoveSubDir: TButton;
    btnEditDir: TButton;
    btnSave: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clbSubDirClickCheck(Sender: TObject);
    procedure btnSelectDirClick(Sender: TObject);
    procedure btnRemoveDirClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure lbDirsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnHelpClick(Sender: TObject);
    procedure lbDirsClick(Sender: TObject);
    procedure btnAddSubDirClick(Sender: TObject);
    procedure btnRemoveSubDirClick(Sender: TObject);
    procedure lbDirsKeyPress(Sender: TObject; var Key: Char);
    procedure clbSubDirClick(Sender: TObject);
    procedure clbSubDirKeyPress(Sender: TObject; var Key: Char);
    procedure btnEditDirClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    MaxWidth: Integer;
    ProjSubDirExpert: TProjSubDirExpert;
    SelectedSubDir: TStringList;
  public
    constructor CreateParametrized(OwningExpert: TProjSubDirExpert);
  end;

  TProjSubDirExpert = class(TGX_Expert)
  private
    FSubDirectoryList: TStrings;
    FSelectedSubDirList: TStrings;
    FAuthor: string;
    FCompany: string;
    FCopyright: string;
    procedure SetAuthor(const Value: string);
    procedure SetCompany(const Value: string);
    procedure SetCopyright(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    property SubDirectoryList: TStrings read FSubDirectoryList;
    property SelectedSubDirList: TStrings read FSelectedSubDirList;
    property Author: string read FAuthor write SetAuthor;
    property Company: string read FCompany write SetCompany;
    property Copyright: string read FCopyright write SetCopyright;
  end;

  TmbPojectCreator = class(TIProjectCreator)
  public
    FileName, PrjName: string;
    function Existing: Boolean; override;
    function GetFileName: string; override;
    function GetFileSystem: string; override;
    function NewProjectSource(const ProjectName: string): string; override;
    procedure NewDefaultModule; override;
    procedure NewProjectResource(Module: TIModuleInterface); override;
  end;

var
  fmProjSubDir: TfmProjSubDir;

implementation

{$R *.DFM}

uses
  GX_uConfigurationInfo,
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_uGenFunc;

const
  LINEFEED = #10;
  CRLF = #13#10;
  CRLF2 = #13#10#13#10;

resourcestring
  sAuthor = 'Michael Beck';
  sComment = 'This expert will create a new Project w/ selected subdirectories';
  sIDString = 'mb.ProjectSubDirExpert';
  sName = 'Project w/SubDir';
  sEntryName = 'CompNameList';
  sEntryCaption = '&CompNameList Expert...';
  sMenuCaption = 'Pro&ject w/ SubDir';
  sMenuName = 'GX_ProjectSubDir';

//************* {TfmProjSubDir} ********************

constructor TfmProjSubDir.CreateParametrized(OwningExpert: TProjSubDirExpert);
begin
  ProjSubDirExpert := OwningExpert;

  inherited Create(nil);
end;

procedure TfmProjSubDir.FormCreate(Sender: TObject);
const
  CRLF1 = #13 + #10;
resourcestring
  SDefaultSubDirExts =
    'Batch' + CRLF1 + 'Bin' + CRLF1 + 'Classes' + CRLF1 + 'Data' + CRLF1 +
    'Docu' + CRLF1 + 'Glyphs' + CRLF1 + 'Help' + CRLF1 + 'Install' + CRLF1 +
    'Lib' + CRLF1 + 'Source';
var
  i, j: Integer;
begin
  ToolServices.CloseProject;

  MaxWidth := lbDirs.Width;

  SelectedSubDir := TStringList.Create;
  SelectedSubDir.Duplicates := dupIgnore;

  if ProjSubDirExpert.SubDirectoryList.Count > 0 then
    clbSubDir.Items.Assign(ProjSubDirExpert.SubDirectoryList)
  else
    clbSubDir.Items.Text := SDefaultSubDirExts;

  if ProjSubDirExpert.SelectedSubDirList.Count > 0 then
  begin
    SelectedSubDir.Assign(ProjSubDirExpert.SelectedSubDirList);
    for i := 0 to SelectedSubDir.Count - 1 do
    begin
      j := clbSubDir.Items.IndexOf(SelectedSubDir[i]);
      if j >= 0 then
        clbSubDir.Checked[j] := True;
    end;
  end;

  edAuthor.text := ProjSubDirExpert.Author;
  edCompany.text := ProjSubDirExpert.Company;
  edCopyright.text := ProjSubDirExpert.Copyright;
end;

procedure TfmProjSubDir.FormDestroy(Sender: TObject);
begin
  ProjSubDirExpert.SubDirectoryList.Assign(SelectedSubDir);
  ProjSubDirExpert.SubDirectoryList.Assign(clbSubDir.Items);

  SelectedSubDir.Free;
  SelectedSubDir := nil;
end;

procedure TfmProjSubDir.clbSubDirClickCheck(Sender: TObject);
begin
  with Sender as TCheckListBox do
  begin
    if Checked[ItemIndex] then
      SelectedSubDir.Add(Items[ItemIndex])
    else
      SelectedSubDir.Delete(SelectedSubDir.IndexOf(Items[ItemIndex]));
  end;
end;

procedure TfmProjSubDir.btnSelectDirClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := '';

  if lbDirs.Items.Count > 0 then
  begin
    Temp := lbDirs.Items[0];
    lbDirs.Items.Clear;
  end;

  GetDir(Self, Temp);

  if Temp <> '' then
  begin
    lbDirs.Items.Add(Temp);
    btnCreate.Enabled := True;
  end;
end;


procedure TfmProjSubDir.btnRemoveDirClick(Sender: TObject);
begin
  if lbDirs.Items.Count = 1 then
    lbDirs.Items.Delete(0);
  btnRemoveDir.Enabled := (lbDirs.ItemIndex > -1) and (lbDirs.Items.Count > 0);
  btnCreate.Enabled := lbDirs.Items.Count = 1;
  btnEditDir.Enabled := True;
end;

procedure TfmProjSubDir.btnAddSubDirClick(Sender: TObject);
resourcestring
  SAddNewSubDirectory = 'Add New Subdirectory';
  SAddNewText = 'Enter the name of the new Subdirectory:';
var
  NewDir: string;
  I, Idx: Integer;
begin
  if InputQuery(SAddNewSubdirectory, SAddNewText, NewDir) then
  begin

    for I := 0 to clbSubDir.Items.Count - 1 do
    begin
      if Pos(NewDir, clbSubDir.Items[I]) > 1 then
      begin
        clbSubDir.Checked[I] := True;
        Exit;
      end;
    end;

    Idx := clbSubDir.Items.Add(NewDir);

    clbSubDir.Checked[Idx] := True;
    SelectedSubDir.Add(NewDir);
  end;
end;

procedure TfmProjSubDir.btnRemoveSubDirClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := clbSubDir.ItemIndex;
  if i < 0 then
    Exit;

  j := SelectedSubDir.IndexOf(clbSubDir.Items[i]);
  if j >= 0 then SelectedSubDir.delete(j);

  clbSubDir.Items.Delete(i);

  btnRemoveDir.Enabled := (clbSubDir.ItemIndex > -1) and (clbSubDir.Items.Count > 0);
end;

procedure TfmProjSubDir.btnCreateClick(Sender: TObject);
var
  i, j: Integer;
  DirName, TempDir: string;
  mbPojectCreator: TmbPojectCreator;
begin
  // Create all required subdirectories
  if lbDirs.Items.Count > 0 then
  begin
    BtnSaveClick(self);

    DirName := lbDirs.Items[0];
    ForceDirectories(DirName);

    if DirectoryExists(DirName) then
    begin
      for j := 0 to SelectedSubDir.Count - 1 do
      begin
        TempDir := DirName + '\' + SelectedSubDir.Strings[j];
        ForceDirectories(TempDir);
      end;

      i := 1;
      while FileExists(TempDir + '\Project' + IntToStr(I) + '.dpr') do
        Inc(i);
      // Now create and open a project with the new name
      mbPojectCreator := TmbPojectCreator.Create;
      try
        mbPojectCreator.PrjName := 'Project' + IntToStr(i);
        mbPojectCreator.FileName := TempDir + '\Project' + IntToStr(i) + '.dpr';

        (ToolServices.ProjectCreate(mbPojectCreator, [cpCanShowSource])).Free;
      finally
        mbPojectCreator.Free;
      end;
    end;
    ModalResult := mrOK;
  end
  else
    ShowMessage('Sorry, but you have to select a directory for the project');
end;

procedure TfmProjSubDir.lbDirsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  TempString: string;
begin
  TempString := lbDirs.Items[Index];
  if lbDirs.Canvas.TextWidth(TempString) > MaxWidth then
  begin
    MaxWidth := lbDirs.Canvas.TextWidth(TempString);
    SendMessage(lbDirs.Handle, LB_SETHORIZONTALEXTENT, MaxWidth, 0);
    lbDirs.Invalidate;
  end;
  Rect.Right := Rect.Left + MaxWidth;
  lbDirs.Canvas.FillRect(Rect);
  // New style controls with DT_PATH_ELLIPSIS do not work out properly (buggy) - play it safe.
  DrawText(lbDirs.Canvas.Handle, PChar(TempString), -1, Rect, DT_LEFT);
end;

procedure TfmProjSubDir.btnHelpClick(Sender: TObject);
begin
  // WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 14);
end;

procedure TfmProjSubDir.lbDirsClick(Sender: TObject);
begin
  btnRemoveDir.Enabled := (lbDirs.ItemIndex > -1);
end;

procedure TfmProjSubDir.lbDirsKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemoveDir.Enabled := (lbDirs.ItemIndex > -1);
end;

procedure TfmProjSubDir.clbSubDirClick(Sender: TObject);
begin
  btnRemoveSubDir.Enabled := (clbSubDir.ItemIndex > -1);
end;

procedure TfmProjSubDir.clbSubDirKeyPress(Sender: TObject; var Key: Char);
begin
  btnRemoveSubDir.Enabled := (clbSubDir.ItemIndex > -1);
end;

//*************{TProjSubDirExpert}*************

constructor TProjSubDirExpert.Create;
begin
  inherited Create;

  FSelectedSubDirList := TStringList.Create;
  FSubDirectoryList := TStringList.Create;

  HasConfigOptions := False;
end;

destructor TProjSubDirExpert.Destroy;
begin
  FSelectedSubDirList.Free;
  FSelectedSubDirList := nil;

  FSubDirectoryList.Free;
  FSubDirectoryList := nil;

  inherited Destroy;
end;

function TProjSubDirExpert.GetMenuCaption: string;
begin
  Result := sMenuCaption;
end;

function TProjSubDirExpert.GetMenuName: string;
begin
  Result := sMenuName;
end;

function TProjSubDirExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TProjSubDirExpert.GetName: string;
begin
  Result := sName;
end;

function TProjSubDirExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Project w/ SubDir';
begin
  Result := SDisplayName;
end;

procedure TProjSubDirExpert.Click(Sender: TObject);
begin
  with TfmProjSubDir.CreateParametrized(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TProjSubDirExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited;
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    ReadStringList(RegIni, SelectedSubDirList, 'ProjectWithSubDir\Selected', 'CreateDir');
    ReadStringList(RegIni, SubDirectoryList, 'ProjectWithSubDir\Available', 'AvailableDir');

    FAuthor := RegIni.ReadString('ProjectWithSubDir', 'Author', 'Author');
    FCompany := RegIni.ReadString('ProjectWithSubDir', 'Company', 'Company');
    FCopyright := RegIni.ReadString('ProjectWithSubDir', 'Copyright', '1999,');
  finally
    RegIni.Free;
  end;
end;

procedure TProjSubDirExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited;
  // do not localize
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.EraseSection('ProjectWithSubDir\Available');
    WriteStringList(RegIni, SubDirectoryList, 'ProjectWithSubDir\Available', 'AvailableDir');
    RegIni.EraseSection('ProjectWithSubDir\Selected');
    WriteStringList(RegIni, SelectedSubDirList, 'ProjectWithSubDir\Selected', 'CreateDir');

    RegIni.WriteString('ProjectWithSubDir', 'Author', Author);
    RegIni.WriteString('ProjectWithSubDir', 'Company', Company);
    RegIni.WriteString('ProjectWithSubDir', 'Copyright', Copyright);
  finally
    RegIni.Free;
  end;
end;

function TProjSubDirExpert.IconFileName: string;
begin
  Result := 'SubDir';
end;

procedure TProjSubDirExpert.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TProjSubDirExpert.SetCompany(const Value: string);
begin
  FCompany := Value;
end;

procedure TProjSubDirExpert.SetCopyright(const Value: string);
begin
  FCopyright := Value;
end;

{ TmbPojectCreator }

function TmbPojectCreator.Existing: Boolean;
begin
  // the project file doesn't exist
  Result := False;
end;

function TmbPojectCreator.GetFileName: string;
begin
  // the file name defined by the Expert
  Result := FileName;
end;

function TmbPojectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TmbPojectCreator.NewProjectSource(const ProjectName: string): string;
var
  RegIni: TRegIniFile;
begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  Result := 'program ' + PrjName + ';' + CRLF2 +
    '{+--------------------------------------------------------------------------+' + CRLF +
    ' | Created:     ' + FormatDateTime('yyyy-mm-dd', date) + ' ' + TimeToStr(Time) + CRLF +
    ' | Author:      ' + RegIni.ReadString('ProjectWithSubDir', 'Author', 'Author') + CRLF +
    ' | Company:     ' + RegIni.ReadString('ProjectWithSubDir', 'Company', 'Company') + CRLF +
    ' | Copyright    ' + RegIni.ReadString('ProjectWithSubDir', 'Copyright', 'Copyright') + CRLF +
    ' | Description: ' + CRLF +
    ' | Version:     1.0' + CRLF +
    ' | Open Issues:' + CRLF +
    ' +--------------------------------------------------------------------------+}' + CRLF2 +

  'uses' + CRLF +
    '  Forms;' + CRLF2 +
    '{$R *.RES}' + CRLF2 +
    'begin' + CRLF +
    '  Application.Initialize;' + CRLF +
    '  Application.Run;' + CRLF +
    'end.' + CRLF;
  RegIni.Free;
end;

procedure TmbPojectCreator.NewDefaultModule;
begin
  ToolServices.CreateModule('', nil, nil, [cmAddToProject, cmNewForm, cmUnnamed, cmMarkModified]);
end;

procedure TmbPojectCreator.NewProjectResource(Module: TIModuleInterface);
begin
  Module.Free;
end;

procedure TfmProjSubDir.btnEditDirClick(Sender: TObject);
resourcestring
  SEditProjectDirectory = 'Edit Project Subdirectory';
  SEditDirName = 'Edit the name of the Project Directory:';
var
  NewDir: string;
begin
  if lbDirs.Items.Count > 0 then
    NewDir := lbDirs.Items[0]
  else
    NewDir := '';
  if InputQuery(SEditProjectDirectory, SEditDirName, NewDir) then
  begin
    if lbDirs.Items.Count > 0 then
      lbDirs.Items[0] := NewDir
    else
      lbDirs.Items.Add(NewDir);
  end;
end;

procedure TfmProjSubDir.btnSaveClick(Sender: TObject);
begin
  ProjSubDirExpert.SelectedSubDirList.Assign(SelectedSubDir);
  ProjSubDirExpert.SubDirectoryList.Assign(clbSubDir.Items);

  ProjSubDirExpert.Author := edAuthor.Text;
  ProjSubDirExpert.Company := edCompany.Text;
  ProjSubDirExpert.Copyright := edCopyright.Text;

  ProjSubDirExpert.SaveSettings;
end;

initialization
  RegisterGX_Expert(TProjSubDirExpert);
end.

