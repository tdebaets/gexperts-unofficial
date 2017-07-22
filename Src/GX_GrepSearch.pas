unit GX_GrepSearch;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ToolIntf, ExptIntf, GX_Experts, Registry, GX_GrepResults;

type
  TfmGrepSearch = class(TForm)
    lblFind: TLabel;
    cbText: TComboBox;
    gbxOptions: TGroupBox;
    cbNoCase: TCheckBox;
    cbNoComments: TCheckBox;
    cbDFM: TCheckBox;
    gbxWhere: TGroupBox;
    rbAllFiles: TRadioButton;
    rbOpenFiles: TRadioButton;
    rbDirectories: TRadioButton;
    gbxDirectories: TGroupBox;
    cbDirectory: TComboBox;
    lblDirectory: TLabel;
    lblMasks: TLabel;
    cbMasks: TComboBox;
    cbInclude: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnBrowse: TSpeedButton;
    cbWholeWord: TCheckBox;
    rbCurrentOnly: TRadioButton;
    btnHelp: TButton;
    sbVCL: TSpeedButton;
    sbRTL: TSpeedButton;
    cbRegEx: TCheckBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbDirectoriesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure sbVCLClick(Sender: TObject);
    procedure sbRTLClick(Sender: TObject);
    procedure cbDirectoryDropDown(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FGrepExpert: TGrepExpert;
    procedure DirEnable(New: Boolean);
  public
    property GrepExpert: TGrepExpert read FGrepExpert;
  end;

  TGrepDlgExpert = class(TGX_EnhExpert)
  protected
    procedure SetActive(New: Boolean); override;
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
    procedure Configure; override;
    procedure LoadSettings; override;
    procedure SaveSettings; override;
  end;

implementation

{$R *.DFM}

uses
{$IFNDEF STANDALONE}
  GX_EditReader, GX_GenFunc, GX_GExperts, GX_ConfigurationInfo,
  {$IFOPT D+} GX_DbugIntf, {$ENDIF D+}
{$ENDIF STANDALONE}
  Menus, FileCtrl;

resourcestring
  SGrepResultsNotActive = 'The Grep Results window is not active';

procedure TfmGrepSearch.btnBrowseClick(Sender: TObject);
{$IFNDEF STANDALONE}
var
  Temp: string;
{$ENDIF STANDALONE}
begin
{$IFNDEF STANDALONE}
  Temp := cbDirectory.Text;
  if GetDir(Self, Temp) then
    cbDirectory.Text := Temp;
{$ENDIF STANDALONE}
end;

procedure TfmGrepSearch.FormCreate(Sender: TObject);
{$IFNDEF STANDALONE}
var
  EditRead: TEditReader;
  Temp: string;
  i: Integer;
{$ENDIF STANDALONE}
begin
{$IFNDEF STANDALONE}
  if fmGrepResults = nil then
    raise Exception.Create(SGrepResultsNotActive);
  FGrepExpert := fmGrepResults.GrepExpert;
  cbText.Items.Assign(FGrepExpert.SearchList);
  cbDirectory.Items.Assign(FGrepExpert.DirList);
  cbMasks.Items.Assign(FGrepExpert.MaskList);
  if FGrepExpert.GrepSave then
  begin
    cbNoCase.Checked := FGrepExpert.GrepNoCase;
    cbNoComments.Checked := FGrepExpert.GrepNoComments;
    cbDFM.Checked := FGrepExpert.GrepDFM;
    cbInclude.Checked := FGrepExpert.GrepSub;
    cbWholeWord.Checked := FGrepExpert.GrepWholeWord;
    cbRegEx.Checked := FGrepExpert.GrepRegEx;
    case FGrepExpert.GrepSearch of
      0: rbCurrentOnly.Checked := True;
      1: rbAllFiles.Checked := True;
      2: rbOpenFiles.Checked := True;
      3: rbDirectories.Checked := True;
    else
      rbAllFiles.Checked := True;
    end;
    if cbText.Items.Count > 0 then
      cbText.Text := cbText.Items[0];
    if cbDirectory.Items.Count > 0 then
      cbDirectory.Text := cbDirectory.Items[0];
    if cbMasks.Items.Count > 0 then
      cbMasks.Text := cbMasks.Items[0];
  end;

  if not StandAlone and (not IsDfm(ToolServices.GetCurrentFile)) then
  begin
    try
      // Since this edit reader is destroyed almost
      // immediately, do not call FreeFileData

      // This can raise an exception for unsaved files
      EditRead := TEditReader.Create(ToolServices.GetCurrentFile);
      try
        EditRead.NoComments := not IsDprOrPas(ToolServices.GetCurrentFile);
        Temp := EditRead.GetBlock;
        if (Length(Trim(Temp)) > 0) and (Length(Trim(Temp)) < 30) then
        begin
          i := Min(Pos(#13, Temp), Pos(#10, Temp));
          if i > 0 then
            Temp := Copy(Temp, 0, i - 1);
          Temp := Temp;
          cbText.Text := Temp;
          cbText.SelStart := 0;
          cbText.SelLength := Length(Temp);
        end;
      finally
        EditRead.Free;
      end;
    except
      on E: Exception do
      begin
        // ignore
      end;
    end;
  end;
{$ENDIF STANDALONE}

  if ToolServices <> nil then
  begin
    if ToolServices.GetProjectName = '' then
    begin
      rbCurrentOnly.Enabled := False;
      rbAllFiles.Enabled := False;
      rbOpenFiles.Enabled := False;
      rbDirectories.Checked := True;
    end;
  end
  else
  begin
    rbCurrentOnly.Enabled := False;
    rbAllFiles.Enabled := False;
    rbOpenFiles.Enabled := False;
    rbDirectories.Checked := True;
  end;
  DirEnable(rbDirectories.Checked);
end;

procedure TfmGrepSearch.DirEnable(New: Boolean);
begin
  cbDirectory.Enabled := New;
  cbMasks.Enabled := New;
  cbInclude.Enabled := New;
  btnBrowse.Enabled := New;
  sbVCL.Enabled := New;
  sbRTL.Enabled := New;
  if not New then
  begin
    cbDirectory.Color := clBtnface;
    cbMasks.Color := clBtnface;
  end
  else
  begin
    cbDirectory.Color := clWindow;
    cbMasks.Color := clWindow;
  end
end;

procedure TfmGrepSearch.rbDirectoriesClick(Sender: TObject);
begin
  DirEnable(rbDirectories.Checked);
end;

procedure TfmGrepSearch.FormDestroy(Sender: TObject);

  procedure AddString(Text: string; List: TStrings; DeleteSlash: Boolean);
  begin
    if Trim(Text) = '' then Exit;
    if Length(Text) > 100 then Exit;
    if DeleteSlash and (Text[Length(Text)] = '\') then
      Delete(Text, Length(Text), 1);
    if List.IndexOf(Text) >= 0 then
      List.Delete(List.IndexOf(Text));

    if List.Count = 0 then
      List.Add(Text)
    else
      List.Insert(0, Text);

    if List.Count > 20 then
      List.Delete(List.Count - 1);
  end;

begin
{$IFNDEF STANDALONE}
  AddString(cbText.Text, FGrepExpert.SearchList, False);
  AddString(cbDirectory.Text, FGrepExpert.DirList, True);
  AddString(cbMasks.Text, FGrepExpert.MaskList, False);
  FGrepExpert.GrepNoCase := cbNoCase.Checked;
  FGrepExpert.GrepNoComments := cbNoComments.Checked;
  FGrepExpert.GrepDFM := cbDFM.Checked;
  FGrepExpert.GrepSub := cbInclude.Checked;
  FGrepExpert.GrepWholeWord := cbWholeWord.Checked;
  FGrepExpert.GrepRegEx := cbRegEx.Checked;
  if rbCurrentOnly.Checked then
    FGrepExpert.GrepSearch := 0
  else
  if rbAllFiles.Checked then
    FGrepExpert.GrepSearch := 1
  else
  if rbOpenFiles.Checked then
    FGrepExpert.GrepSearch := 2
  else
  if rbDirectories.Checked then
    FGrepExpert.GrepSearch := 3;
{$ENDIF STANDALONE}
end;

procedure TfmGrepSearch.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 1);
end;

procedure TfmGrepSearch.sbVCLClick(Sender: TObject);
begin
  cbDirectory.Text := ConfigInfo.VCLPath;
end;

procedure TfmGrepSearch.sbRTLClick(Sender: TObject);
var
  Path: string;
  SubPos: Integer;
begin
  Path := ConfigInfo.VCLPath;
  SubPos := Pos('VCL', Path);
  if SubPos > 0 then
  begin
    Delete(Path, SubPos, 3);
    Insert('RTL', Path, SubPos);
    cbDirectory.Text := Path;
  end;
end;

procedure TfmGrepSearch.cbDirectoryDropDown(Sender: TObject);
var
  i: Integer;
  MaxWidth: Integer;
  Bitmap: TBitmap;
begin
  MaxWidth := cbDirectory.Width;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(cbDirectory.Font);
    for i := 0 to cbDirectory.Items.Count - 1 do
      MaxWidth := Max(MaxWidth, Bitmap.Canvas.TextWidth(cbDirectory.Items[i]) + 10);
  finally;
    Bitmap.Free;
  end;
  if cbDirectory.Items.Count > cbDirectory.DropDownCount then
    Inc(MaxWidth,  GetSystemMetrics(SM_CXVSCROLL));
  if MaxWidth > cbDirectory.Width then
    SendMessage(cbDirectory.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0)
  else
    SendMessage(cbDirectory.Handle, CB_SETDROPPEDWIDTH, 0, 0)
end;

procedure TfmGrepSearch.btnOKClick(Sender: TObject);
begin
  if rbDirectories.Checked then
  begin
    if Trim(cbDirectory.Text) = '' then
      cbDirectory.Text := GetCurrentDir;
    cbDirectory.Text := ExpandFileName(cbDirectory.Text);
    if not DirectoryExists(cbDirectory.Text) then
    begin
      ModalResult := mrNone;
      cbDirectory.SetFocus;
      raise Exception.Create('The specified search directory does not exist');
    end;
  end;
end;

{ TGrepDlgExpert }

constructor TGrepDlgExpert.Create;
begin
  inherited Create;
  ShortCut := Menus.ShortCut(Word('S'), [ssCtrl, ssShift]);
  HasConfigOptions := False;
end;

destructor TGrepDlgExpert.Destroy;
begin
  inherited Destroy;
end;

function TGrepDlgExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Grep Search...';
begin
  Result := SMenuCaption;
end;

function TGrepDlgExpert.GetMenuName: string;
begin
  Result := 'GX_GrepDlg';
end;

function TGrepDlgExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TGrepDlgExpert.GetName: string;
begin
  Result := 'Grep_Dlg';
end;

function TGrepDlgExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Grep Search';
begin
  Result := SDisplayName;
end;

procedure TGrepDlgExpert.Click(Sender: TObject);
begin
  if fmGrepResults <> nil then
    fmGrepResults.Execute(False)
  else
    raise Exception.Create(SGrepResultsNotActive);
end;

procedure TGrepDlgExpert.Configure;
resourcestring
  SUseGrepExpert = 'Configure Grep search options from the Grep expert.';
begin
  MessageDlg(SUseGrepExpert, mtInformation, [mbOK], 0);
end;

procedure TGrepDlgExpert.SaveSettings;
begin
  inherited SaveSettings;
end;

procedure TGrepDlgExpert.LoadSettings;
begin
  inherited LoadSettings;
end;

function TGrepDlgExpert.IconFileName: string;
begin
   Result := 'GrepResults';
end;

procedure TGrepDlgExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Grep Search is a modal expert
    end;
  end;
end;

initialization
  RegisterGX_Expert(TGrepDlgExpert);
end.

