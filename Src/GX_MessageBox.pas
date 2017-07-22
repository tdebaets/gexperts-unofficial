unit GX_MessageBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type

  TGxMsgBoxAdaptor = class(TObject)
  protected
    function GetMessage: string; virtual; abstract;
    function GetCaption: string; virtual;
    function GetButtons: TMsgDlgButtons; virtual;
    function GetDefaultButton: TMsgDlgBtn; virtual;
    function GetUniqueIdentifier: string; virtual;
    procedure DoPermanentlySuppress; virtual;
    function IsPermanentlySuppressed: Boolean; virtual;
    function ShouldShow: Boolean; virtual;
  end;

  TfmGxMessageBox = class(TForm)
    chkNeverShowAgain: TCheckBox;
    bvlFrame: TBevel;
    mmoMessage: TMemo;
  end;

  TGxMsgBoxAdaptorClass = class of TGxMsgBoxAdaptor;

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass): TModalResult;

implementation

{$R *.DFM}

uses
  Registry,
  GX_ConfigurationInfo;

const
  MsgDlgResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);
  // These are not currently localized appropriately
  MsgDlgButtonCaptions: array[TMsgDlgBtn] of string = (
   '&Yes', '&No', '&OK', '&Cancel', '&Abort', '&Retry', '&Ignore',
   '&All', 'N&o to All', 'Ye&s to All', '&Help');

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass): TModalResult;
var
  Adaptor: TGxMsgBoxAdaptor;
  Dlg: TfmGxMessageBox;

  procedure CreateButtons;
  const
    SingleButtonWidth = 75;
    SingleButtonHeight = 25;
    ButtonHorizSpacing = 8;
    ButtonYPos = 208;
  var
    BtnType, DefaultBtn, CancelBtn: TMsgDlgBtn;
    DialogButtons: TMsgDlgButtons;
    ButtonRowWidth, NextButonXPos: Integer;
    CurrentButton: TButton;
  begin
    // Calculate the width of all buttons together
    ButtonRowWidth := 0;
    DialogButtons := Adaptor.GetButtons;
    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if BtnType in DialogButtons then
        Inc(ButtonRowWidth, SingleButtonWidth + ButtonHorizSpacing);
    Dec(ButtonRowWidth, ButtonHorizSpacing);
    if ButtonRowWidth > Dlg.ClientWidth then
      Dlg.ClientWidth := ButtonRowWidth;

    DefaultBtn := Adaptor.GetDefaultButton;

    if mbCancel in DialogButtons then
      CancelBtn := mbCancel
    else if mbNo in DialogButtons then
      CancelBtn := mbNo
    else
      CancelBtn := mbOK;

    NextButonXPos := Round((Dlg.ClientWidth - ButtonRowWidth)/2);

    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if BtnType in DialogButtons then
      begin
        CurrentButton := TButton.Create(Dlg);
        with CurrentButton do
        begin
          Caption := MsgDlgButtonCaptions[BtnType];
          ModalResult := MsgDlgResults[BtnType];
          Parent := Dlg;
          TabOrder := 999;
          SetBounds(NextButonXPos, ButtonYPos, SingleButtonWidth, SingleButtonHeight);
          if BtnType = DefaultBtn then
          begin
            Default := True;
            Dlg.ActiveControl := CurrentButton;
          end;
          if BtnType = CancelBtn then
            Cancel := True;
        end;
        Inc(NextButonXPos, SingleButtonWidth + ButtonHorizSpacing);
      end;
    end;
  end;

begin
  Adaptor := AdaptorClass.Create;
  try
    Result := MsgDlgResults[Adaptor.GetDefaultButton];
    if Adaptor.IsPermanentlySuppressed then
      Exit;
    if Adaptor.ShouldShow then
    begin
      Dlg := TfmGxMessageBox.Create(nil);
      try
        Dlg.Caption := Adaptor.GetCaption;
        Dlg.mmoMessage.Lines.Text := Adaptor.GetMessage;
        CreateButtons;
        Result := Dlg.ShowModal;
        if Dlg.chkNeverShowAgain.Checked then
          Adaptor.DoPermanentlySuppress;
      finally
        Dlg.Release;
      end;
    end;
  finally
    Adaptor.Free;
  end;
end;

{ TGxMsgBoxAdaptor }

function TGxMsgBoxAdaptor.GetCaption: string;
begin
  Result := 'GExperts Message';
end;

function TGxMsgBoxAdaptor.GetUniqueIdentifier: string;
begin
  Result := Self.ClassName;
end;

const
  RegistryKeyName = 'SuppressedMessages';

procedure TGxMsgBoxAdaptor.DoPermanentlySuppress;
begin
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteBool(RegistryKeyName, Self.ClassName, True); // Do not localize
  finally
    Free;
  end;
end;

function TGxMsgBoxAdaptor.IsPermanentlySuppressed: Boolean;
begin
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    Result := ReadBool(RegistryKeyName, Self.ClassName, False); // Do not localize
  finally
    Free;
  end;
end;

function TGxMsgBoxAdaptor.ShouldShow: Boolean;
begin
  Result := True;
end;

function TGxMsgBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbOK];
end;

function TGxMsgBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
begin
  Result := mbOK;
end;

end.
