unit GX_ToDoOptions;
// Created 12/21/98 by AJ Banck, ajbanck@davilex.nl
// ToDo - Make the list not sorted, but have a list-order.
// That way you can have a 'ToDo' after a '#ToDo'

interface

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmToDoOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxTokens: TGroupBox;
    gbxOptions: TGroupBox;
    cbShowTokens: TCheckBox;
    cbAddMessage: TCheckBox;
    cbHideOnGoto: TCheckBox;
    lblPriority: TLabel;
    lblToken: TLabel;
    lstTokens: TListBox;
    btnInsert: TButton;
    btnApply: TButton;
    btnRemove: TButton;
    edToken: TEdit;
    cboPriority: TComboBox;
    gbxSearchFiles: TGroupBox;
    btnBrowse: TSpeedButton;
    chkInclude: TCheckBox;
    cboDirectory: TComboBox;
    radScanProj: TRadioButton;
    radScanOpen: TRadioButton;
    radScanDir: TRadioButton;
    chkInclProjUses: TCheckBox;
    procedure btnInsertClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure edTokenChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lstTokensClick(Sender: TObject);
    procedure cboPriorityChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure radScanDirClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
  private
    procedure UpdateButtonState;
    procedure ProjEnable(New: Boolean);
    procedure DirEnable(New: Boolean);
  end;

// #ToDo:4 test2

implementation

{$R *.DFM}

uses
  GX_GenFunc, GX_Todo;

procedure TfmToDoOptions.UpdateButtonState;
var
  HasTokenText: Boolean;
  TokenTextInList: Boolean;
  IsListItemSelected: Boolean;
  TextIsCurrentListItem: Boolean;
begin
  HasTokenText := (edToken.Text <> '');
  TokenTextInList := (lstTokens.Items.IndexOf(edToken.Text) > -1);
  IsListItemSelected := (lstTokens.ItemIndex > -1);

  with lstTokens do
    TextIsCurrentListItem := IsListItemSelected and (edToken.Text = Items[ItemIndex]);

  btnInsert.Enabled := HasTokenText and not TokenTextInList;
  btnRemove.Enabled := IsListItemSelected;
  btnApply.Enabled := HasTokenText and IsListItemSelected and TokenTextInList;

  // #ToDo2 rewrite this to make it readable!!!
  if TextIsCurrentListItem then
    with lstTokens do
    begin
      if (cboPriority.ItemIndex = Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority)) then
        btnApply.Enabled := False;
    end;
end;

procedure TfmToDoOptions.btnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := cboDirectory.Text;
  if GetDir(Self, Temp) then
    cboDirectory.Text := Temp;
end;

procedure TfmToDoOptions.ProjEnable(New: Boolean);
begin
  chkInclProjUses.Enabled := New;
end;

procedure TfmToDoOptions.DirEnable(New: Boolean);
begin
  cboDirectory.Enabled := New;
  chkInclude.Enabled   := New;
  btnBrowse.Enabled    := New;
  if not New then
    cboDirectory.Color := clBtnFace
  else
    cboDirectory.Color := clWindow;
end;

procedure TfmToDoOptions.btnInsertClick(Sender: TObject);
resourcestring
  SLeadingDollarNotAllowed = 'A leading "$" character is not allowed in tokens, '+
                             'as this can conflict with Object Pascal compiler options.'#13+
                             #13+
                             'Please choose a different token.';

  SEmptyTokenTextError = 'You cannot insert a token that only consists of white space.';
var
  TokenInfo: TTokenInfo;
  TokenString: string;
begin
  TokenString := Trim(edToken.Text);
  if TokenString <> '' then
  begin
    if TokenString[1] = '$' then
    begin
      MessageDlg(SLeadingDollarNotAllowed, mtError, [mbOk], 0);
      Exit;
    end;

    TokenInfo := TTokenInfo.Create;
    TokenInfo.Token := TokenString;
    TokenInfo.Priority := TTodoPriority(cboPriority.ItemIndex);
    lstTokens.Items.AddObject(TokenInfo.Token, TokenInfo);
  end
  else
  begin
    // Warning message that an empty token is inserted
    MessageDlg(SEmptyTokenTextError, mtError, [mbOK], 0);
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnRemoveClick(Sender: TObject);
begin
  with lstTokens do
    if ItemIndex <> -1 then
    begin
      Items.Objects[ItemIndex].Free;
      Items.Delete(ItemIndex);
    end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.btnApplyClick(Sender: TObject);
var
  TokenText: string;
begin
  with lstTokens do
  begin
    TokenText := edToken.Text;

    if (ItemIndex > -1) and (TokenText <> '') and
       Assigned(Items.Objects[ItemIndex]) then
    begin
      Items[ItemIndex] := TokenText;
      with TTokeninfo(Items.Objects[ItemIndex]) do
      begin
        Token := TokenText;
        Priority := TToDoPriority(cboPriority.ItemIndex);
      end;
    end;
  end;
  UpdateButtonState;
end;

procedure TfmToDoOptions.edTokenChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormShow(Sender: TObject);
begin
  cboPriority.ItemIndex := 1;
  UpdateButtonState;
  {$IFNDEF GX_VER120_up}
  // Writing to the message view is only
  // possible in Delphi 4.0+
  cbAddMessage.Enabled := False;
  {$ENDIF GX_VER120_up}
end;

procedure TfmToDoOptions.lstTokensClick(Sender: TObject);
begin
  UpdateButtonState;
  if lstTokens.ItemIndex > -1 then
  begin
    with lstTokens do
    begin
      cboPriority.ItemIndex := Ord(TTokenInfo(Items.Objects[ItemIndex]).Priority);
      edToken.Text := Items[ItemIndex]
    end;
  end;
end;

procedure TfmToDoOptions.cboPriorityChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmToDoOptions.FormCreate(Sender: TObject);
begin
  ProjEnable(radScanProj.Checked);
  DirEnable(radScanDir.Checked);
end;

procedure TfmToDoOptions.radScanDirClick(Sender: TObject);
begin
  ProjEnable(radScanProj.Checked);
  DirEnable(radScanDir.Checked);
end;

end.
