unit GX_CompReplace;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  {$IFDEF GX_VER140_up}
  DesignIntf,
  {$ELSE not GX_VER140_up}
  DsgnIntf,
  {$ENDIF}
  ToolIntf, ExptIntf, RplWizInfo, EditIntf, GX_Experts;

type
  TfmCompReplace = class(TForm)
    lblSeach: TLabel;
    cbSearch: TComboBox;
    lblReplace: TLabel;
    cbReplace: TComboBox;
    gbxScope: TGroupBox;
    rbCurrent: TRadioButton;
    rbAll: TRadioButton;
    rbSelected: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure cbSearchChange(Sender: TObject);
    procedure cbSearchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure LoadComponentList;
    procedure ReplaceComponents(OnlySelected: Boolean);
    procedure ReplaceAllComponents;
  public
    procedure ReplaceForForm(SourceFileName: string; OnlySelected: Boolean);
  end;

  TCompReplaceExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    function GetMenuCaption: string; override;
    function GetMenuName: string; override;
    function GetMenuMask: string; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
    function IconFileName: string; override;
    procedure Click(Sender: TObject); override;
  end;


implementation

{$R *.DFM}

uses
{$IFOPT D+}
  GX_DbugIntf,
{$ENDIF D+}
  GX_ConfigurationInfo, GX_GExperts, GX_GenFunc;

function ListFiles(Param: Pointer;
                   const FileName, Unitname, FormName: string): Boolean; stdcall;
begin
  Result := True;
  if FormName <> '' then
    TfmCompReplace(Param).ReplaceForForm(FileName, False);
end;

procedure TfmCompReplace.LoadComponentList;
var
  m: Integer;
  c: Integer;
begin
  for m := 0 to ToolServices.GetModuleCount-1 do
    for c := 0 to ToolServices.GetComponentCount(m)- 1 do
    begin
      cbSearch.Items.Add(ToolServices.GetComponentName(m, c));
      cbReplace.Items.Add(ToolServices.GetComponentName(m, c));
    end;
end;

procedure TfmCompReplace.FormCreate(Sender: TObject);
var
  FormIntf: TIFormInterface;
  ModIntf: TIModuleInterface;
  CompIntf: TIComponentInterface;
  i: Integer;
  CurrentFileName: string;
begin
  ModIntf := nil;
  FormIntf := nil;
  CompIntf := nil;
  LoadComponentList;
  // first assume that we have a Pascal source file
  CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.PAS'));
  try
    ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
    if ModIntf = nil then
    begin
      // try again with C++ source file
      CurrentFileName := UpperCase(ChangeFileExt(ToolServices.GetCurrentFile, '.CPP'));
      ModIntf := ToolServices.GetModuleInterface(CurrentFileName);
    end;
    if ModIntf <> nil then
    begin
      FormIntf := ModIntf.GetFormInterface;
      if FormIntf <> nil then
      begin
        if FormIntf.GetSelCount > 0 then
        begin
          CompIntf := FormIntf.GetSelComponent(0);
          if ((CompIntf <> nil) and
             (FormIntf.GetSelCount = 1) and
             ((TComponent(CompIntf.GetComponentHandle) is TCustomForm) or
             (TComponent(CompIntf.GetComponentHandle) is TDataModule))) then
          begin
            CompIntf.Free;
            CompIntf := nil;
          end;
          if CompIntf <> nil then
          begin
            i := cbSearch.Items.IndexOf(CompIntf.GetComponentType);
            if i > -1 then
            begin
              cbSearch.ItemIndex := i;
              ActiveControl := cbReplace;
            end;
          end;
        end;
      end;
    end;
    if CompIntf = nil then
    begin
      rbSelected.Enabled := False;
      rbCurrent.Checked := True;
      if FormIntf = nil then
      begin
        rbCurrent.Enabled := False;
        rbAll.Checked := True;
      end;
    end;
  finally
    ModIntf.Free;
    FormIntf.Free;
    CompIntf.Free;
  end;
end;

procedure TfmCompReplace.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 19);
end;

procedure TfmCompReplace.btnOKClick(Sender: TObject);
resourcestring
  SComponentNotSelectedForSearch = 'You have not selected a component to search for.';
  SComponentNotSelectedForReplace = 'You have not selected a component to replace.';
begin
  if Trim(cbSearch.Text) = '' then
  begin
    MessageDlg(SComponentNotSelectedForSearch, mtError, [mbOK], 0);
    Exit;
  end;

  if Trim(cbReplace.Text) = '' then
  begin
    MessageDlg(SComponentNotSelectedForReplace, mtError, [mbOK], 0);
    Exit;
  end;

  Screen.Cursor := crHourglass;
  try
    if rbSelected.Checked then
      ReplaceComponents(True)
    else
    if rbCurrent.Checked then
      ReplaceComponents(False)
    else
    if rbAll.Checked then
      ReplaceAllComponents;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmCompReplace.cbSearchChange(Sender: TObject);
{var
  nLoop: Integer;
  SavedText: string;
  Sel_Start: Integer;
  Sel_Length: Integer;
  ValLength: Integer;}
begin
  (*with TComboBox(Sender) do // Scott Mattes, 98/11/23, add this with
  begin
    SavedText := Text;
    ValLength := Length(SavedText);
    Sel_Start := 0;
    Sel_Length := ValLength;
    ItemIndex := -1;
    // try and complete the entry for the user
    for nLoop := 0 to (Items.Count - 1) do
    begin
      if Pos(UpperCase(SavedText), UpperCase(Items[nLoop])) <> 1 then
        Continue;
      ItemIndex := nLoop;
      Sel_Start := ValLength;
      Sel_Length := 99; // length(text) - ValLength;
      Break;
    end; // for
    SelStart := Sel_Start;
    SelLength := Sel_Length;
    // only enable the OK button if Search and Replace are filled in and they
    // are not set to the same type of component and they have valid entries
  end; // with
  *)
    btnOK.Enabled := (((Trim(cbSearch.Text) <> '') and
      (Trim(cbReplace.Text) <> '')) and
      (not (Trim(cbSearch.Text) = Trim(cbReplace.Text))))
      //and (ItemIndex <> -1); // Scott Mattes, 98/11/23, added this condition
end;

procedure TfmCompReplace.cbSearchKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_BACK then
  begin
    with Sender as TComboBox do
      Text := Copy(Text, 1, SelStart - 1);
    Key := 0;
    cbSearchChange(Sender);
  end;
end;

procedure TfmCompReplace.ReplaceForForm(SourceFileName: string; OnlySelected: Boolean);
var
  FormIntf: TIFormInterface;
  ModIntf: TIModuleInterface;
  FormInfo: TFormInfo;
  CompList: TStringList;
resourcestring
  SCouldNotOpenFile = 'Could not open file: ';
  SCouldNotGetFormInterface = 'Could not get form interface.';
begin
  ModIntf := nil;
  FormIntf := nil;
  CompList := TStringList.Create;
  try
    ModIntf := ToolServices.GetModuleInterface(SourceFileName);
    if ModIntf = nil then
    begin
      ToolServices.OpenFile(SourceFileName);
      ShowMessage(SourceFileName);
      ModIntf := ToolServices.GetModuleInterface(SourceFileName);
      if ModIntf = nil then
      begin
        MessageDlg(Format('%s %s.', [SCouldNotOpenFile, SourceFileName]), mtError, [mbOK], 0);
        Exit;
      end;
    end;
    FormIntf := ModIntf.GetFormInterface;
    if FormIntf = nil then
    begin
      MessageDlg(SCouldNotGetFormInterface, mtError, [mbOK], 0);
      Exit;
    end;
    FormInfo := TFormInfo.Create(FormIntf);
    try
      if OnlySelected then
        FormInfo.GetSelectedComponents(CompList, cbSearch.Text)
      else
        FormInfo.GetMatchingComponents(CompList, cbSearch.Text);
      FormInfo.ReplaceComponents(CompList, cbReplace.Text);
    finally
      FormInfo.Free;
    end;
  finally
    ModIntf.Free;
    FormIntf.Free;
    CompList.Free;
  end;
end;

procedure TfmCompReplace.ReplaceComponents(OnlySelected: Boolean);
var
  i: Integer;
  CurrentFile: string;
  FileName: string;
begin
  CurrentFile := ToolServices.GetCurrentFile;
  {$IFOPT D+}SendDebug('Current:' + CurrentFile);{$ENDIF D+}

  if not IsDfm(CurrentFile) then
    ReplaceForForm(CurrentFile, OnlySelected)
  else
  begin
    // Search for unit that corresponds to DFM
    // This may be either a Pascal or C++ source file
    for i := 0 to ToolServices.GetUnitCount - 1 do
    begin
      FileName := ToolServices.GetUnitName(i);
      if CompareText(CurrentFile, ChangeFileExt(FileName, '.DFM')) = 0 then
      begin
        ReplaceForForm(FileName, OnlySelected);
        Break;
      end;
      if (i = ToolServices.GetUnitCount - 1) then
      begin
        // The file is not in the current project, try again in non-project files
        {$IFDEF GX_Delphi}
        ReplaceForForm(ChangeFileExt(CurrentFile, '.PAS'), OnlySelected);
        {$ELSE not GX_Delphi}
        ReplaceForForm(ChangeFileExt(CurrentFile, '.CPP'), OnlySelected);
        {$ENDIF not GX_Delphi}
      end;
    end;
  end;
end;

procedure TfmCompReplace.ReplaceAllComponents;
begin
  ToolServices.EnumProjectUnits(ListFiles, Pointer(Self));
end;

//************************ TCompReplaceExpert ********************

constructor TCompReplaceExpert.Create;
begin
  inherited Create;
  HasConfigOptions := False;
  HasMenuItem := True;
end;

function TCompReplaceExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Replace &Components...';
begin
  Result := SMenuCaption;
end;

function TCompReplaceExpert.GetMenuName: string;
begin
  Result := 'GX_CompReplace';
end;

function TCompReplaceExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TCompReplaceExpert.GetName: string;
begin
  Result := 'Component_Replace';
end;

function TCompReplaceExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Replace Components';
begin
  Result := SDisplayName;
end;

procedure TCompReplaceExpert.Click(Sender: TObject);
begin
  with TfmCompReplace.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TCompReplaceExpert.IconFileName: string;
begin
  Result := 'Replace';
end;

procedure TCompReplaceExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Replace Components is a modal expert
    end;
  end;
end;

initialization
  RegisterGX_Expert(TCompReplaceExpert);
end.

