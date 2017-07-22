unit GX_MessageDialog;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{$I GX_CondDefine.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  {$IFDEF GX_VER120_up}
  ImgList,
  {$ENDIF GX_VER120_up}
  Forms, Dialogs,
  StdCtrls, ExtCtrls, Registry, GX_Experts, EditIntf, ExptIntf, ToolIntf,
  SpinIntEdit;

type
  TSourceType = (stPascal, stCpp);

type
  TfmMessageDialog = class(TForm)
    lblMessage: TLabel;
    gbButtons: TGroupBox;
    cbYes: TCheckBox;
    cbNo: TCheckBox;
    cbOK: TCheckBox;
    cbCancel: TCheckBox;
    cbAbort: TCheckBox;
    cbretry: TCheckBox;
    cbIgnore: TCheckBox;
    cbAll: TCheckBox;
    lblHelpContext: TLabel;
    gbTypes: TGroupBox;
    rbWarning: TRadioButton;
    rbInformation: TRadioButton;
    rbConfirmation: TRadioButton;
    rbError: TRadioButton;
    rbCustom: TRadioButton;
    ilImages: TImageList;
    imWarning: TImage;
    imInformation: TImage;
    meCaption: TMemo;
    imError: TImage;
    imConfirmation: TImage;
    btnOK: TButton;
    btnCancel: TButton;
    btnDefaults: TButton;
    btnHelp: TButton;
    cbNoToAll: TCheckBox;
    cbYesToAll: TCheckBox;
    btnTest: TButton;
    cbHelp: TCheckBox;
    edHelpContext: TSpinIntEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure meCaptionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnTestClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FCode: string;
    FSourceType: TSourceType;
    procedure BuildSourceCode;
    procedure LoadSettings;
    procedure SaveSettings;
  public
    property Code: string read FCode;
    property SourceType: TSourceType read FSourceType write FSourceType;
  end;

  TMsgExpExpert = class(TGX_EnhExpert)
  private
    FMsgString: string;
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
    procedure LoadSettings; override;
    procedure SaveSettings; override;
    procedure Configure; override;
    property MsgString: string read FMsgString write FMsgString;
  end;

var
  MsgExpExpert: TMsgExpExpert = nil;

implementation

{$R *.DFM}
{$R MessageDialog.res}

uses
  TypInfo, Menus,
  GX_ConfigurationInfo, GX_GExperts, GX_EditWriter, GX_GenFunc, GX_MessageOptions;

// Note: this special routine is never buggy for a double
// ampersand, i.e. a caption that reads "This && That"
// because we will never pass in && and because the
// processed string is directly used as an identifier
function RemoveAmpersand(const Caption: string): string;
var
  AmperPos: Cardinal;
begin
  Result := Caption;

  AmperPos := Pos('&', Result);
  while AmperPos > 0 do
  begin
    Delete(Result, AmperPos, 1);
    AmperPos := Pos('&', Result);
  end;
end;

procedure TfmMessageDialog.FormCreate(Sender: TObject);

  procedure DrawImage(Canvas: TCanvas; Img: Integer);
  begin
    Canvas.Brush.Color := clBtnface;
    Canvas.FillRect(Rect(0, 0, 32, 32));
    ilImages.Draw(Canvas, 0, 0, Img);
  end;

resourcestring
  SCouldNotLoadImages = 'Could not load images';
begin
  FCode := '';
  ilImages.ResourceLoad(rtBitmap, 'MESSAGE_ICONS', clOlive);  // do not localize
  if ilImages.Count = 0 then
    raise Exception.Create(SCouldNotLoadImages);

  DrawImage(imWarning.Canvas, 2);
  DrawImage(imError.Canvas, 3);
  DrawImage(imInformation.Canvas, 0);
  DrawImage(imConfirmation.Canvas, 1);

  LoadSettings;
end;

procedure TfmMessageDialog.BuildSourceCode;
var
  DialogTypeCodeText: string;
  ButtonCodeText: string;
  MessageDlgCaption: string;

  // Build Pascal source code for MessageDlg
  function BuildPascalSourceCode: string;
  var
  i, j: Integer;
  begin
    ButtonCodeText := '[' + ButtonCodeText + ']';

    Result := 'MessageDlg(' + #39;  // do not localize
    for i := 0 to meCaption.Lines.Count - 1 do
    begin
      MessageDlgCaption := '';
      for j := 1 to Length(meCaption.Lines[i]) do
        if meCaption.Lines[i][j] = #39 then
        begin
          MessageDlgCaption := MessageDlgCaption + #39 + '+#39';
          if i < Length(meCaption.Lines[i]) then
            MessageDlgCaption := MessageDlgCaption + '+' + #39;
        end
        else
          MessageDlgCaption := MessageDlgCaption + meCaption.Lines[i][j];
      if i > 0 then
        Result := Result + #39 + MsgExpExpert.MsgString + #39 + MessageDlgCaption
      else
        Result := Result + MessageDlgCaption;
    end;
    Result := Result + #39 + ', ' + DialogTypeCodeText + ', ' + ButtonCodeText + ', ' + edHelpContext.Text + ');';
  end;

  // Build C++ source code for MessageDlg
  // MessageDlg("Msg\nNext", DlgType, TMsgDlgButtons() << mbOK << mbYes << mbNo, 0)
  function BuildCppSourceCode: string;
  var
    i, j: Integer;
  begin
    Result := 'MessageDlg("';  // do not localize
    MessageDlgCaption := '';
    for i := 0 to meCaption.Lines.Count-1 do
    begin
      for j := 1 to Length(meCaption.Lines[i]) do
        if meCaption.Lines[i][j] = '"' then
          MessageDlgCaption := MessageDlgCaption + '\"'
        else
        if meCaption.Lines[i][j] = '\' then
          MessageDlgCaption := MessageDlgCaption + '\\'
        else
          MessageDlgCaption := MessageDlgCaption + meCaption.Lines[i][j];

      // append only up to last line
      if i < meCaption.Lines.Count-1 then
        MessageDlgCaption := MessageDlgCaption + '\n';
    end;
    MessageDlgCaption := MessageDlgCaption + '"';

    Result := Result + MessageDlgCaption + ', ' +
              DialogTypeCodeText + ', ' +
              ButtonCodeText + ', ' +
              edHelpContext.Text + ');';
  end;

var
  i: Integer;
  ButtonCheckBox: TCheckBox;
begin
  for i := 0 to gbTypes.ControlCount - 1 do
    if gbTypes.Controls[i] is TRadioButton then
    begin
      if TRadioButton(gbTypes.Controls[i]).Checked then
      begin
        DialogTypeCodeText := RemoveAmpersand(TRadioButton(gbTypes.Controls[i]).Caption);
        Break;
      end;
    end;

  case FSourceType of
    stCpp:    ButtonCodeText := 'TMsgDlgButtons()';
    stPascal: ButtonCodeText := '';
  end;

  for i := 0 to gbButtons.ControlCount - 1 do
    if gbButtons.Controls[i] is TCheckBox then
    begin
      ButtonCheckBox := TCheckBox(gbButtons.Controls[i]);
      if ButtonCheckBox.Checked then
      begin
        if ButtonCodeText <> '' then
        begin
          case FSourceType of
            stPascal: ButtonCodeText := ButtonCodeText + ', ';    // do not localize
            stCpp:    ButtonCodeText := ButtonCodeText + ' << ';  // do not localize
          end;
        end;
        ButtonCodeText := ButtonCodeText + ButtonCheckBox.Caption;
      end;
    end;

  case Self.SourceType of
    stPascal: FCode := BuildPascalSourceCode;
    stCpp   : FCode := BuildCppSourceCode;
  else
    raise Exception.Create('Invalid source code type');
  end;
end;


procedure TfmMessageDialog.btnOKClick(Sender: TObject);
begin
  try
    BuildSourceCode;
  except
    on E: Exception do
    begin
      // ignore
    end;
  end;

  ModalResult := mrOK;
end;

procedure TfmMessageDialog.SaveSettings;
var
  DialogTypeCodeText: string;
  ButtonCodeText: string;
  i: Integer;
begin
  for i := 0 to gbTypes.ControlCount - 1 do
    if gbTypes.Controls[i] is TRadioButton then
      if TRadioButton(gbTypes.Controls[i]).Checked then
      begin
        DialogTypeCodeText := TRadioButton(gbTypes.Controls[i]).Caption;
        Break;
      end;

  ButtonCodeText := '';
  for i := 0 to gbButtons.ControlCount - 1 do
    if gbButtons.Controls[i] is TCheckBox then
      if TCheckBox(gbButtons.Controls[i]).Checked then
        ButtonCodeText := ButtonCodeText + TCheckBox(gbButtons.Controls[i]).Caption + ',';

  // do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteString('MsgExp', 'MsgType', DialogTypeCodeText);
    WriteString('MsgExp', 'Buttons', ButtonCodeText);
  finally
    Free;
  end;
end;

procedure TfmMessageDialog.LoadSettings;
var
  DialogTypeCodeText: string;
  ButtonCodeText: string;
  i: Integer;
begin
  // do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    DialogTypeCodeText := ReadString('MsgExp', 'MsgType', 'mtWarning');
    ButtonCodeText := ReadString('MsgExp', 'Buttons', '');
  finally
    Free;
  end;

  for i := 0 to gbTypes.ControlCount - 1 do
    if gbTypes.Controls[i] is TRadioButton then
      if TRadioButton(gbTypes.Controls[i]).Caption = DialogTypeCodeText then
      begin
        TRadioButton(gbTypes.Controls[i]).Checked := True;
        Break;
      end;

  for i := 0 to gbButtons.ControlCount - 1 do
    if gbButtons.Controls[i] is TCheckBox then
      if Pos(TCheckBox(gbButtons.Controls[i]).Caption, ButtonCodeText) <> 0 then
        TCheckBox(gbButtons.Controls[i]).Checked := True;
end;


procedure TfmMessageDialog.btnDefaultsClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TfmMessageDialog.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 9);
end;

procedure TfmMessageDialog.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #27: begin
           Close;
           Key := #0;
         end;
  end;
end;

{ TMsgExpExpert }

constructor TMsgExpExpert.Create;
begin
  inherited Create;
  MsgExpExpert := Self;
  ShortCut := Menus.ShortCut(Word('D'), [ssCtrl]);
  HasConfigOptions := True;
  HasMenuItem := True;
  FMsgString := '+#13+#10+';   // do not localize
end;

destructor TMsgExpExpert.Destroy;
begin
  MsgExpExpert := nil;
  inherited Destroy;
end;

procedure TMsgExpExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here, MessageDialog is a modal form
    else
    begin
      // Nothing to free here, MessageDialog is a modal form
    end;
  end;
end;

function TMsgExpExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = '&Message Dialog...';
begin
  Result := SMenuCaption;
end;

function TMsgExpExpert.GetMenuName: string;
begin
  Result := 'GX_MsgExp';
end;

function TMsgExpExpert.GetMenuMask: string;
begin
  Result := '*.PAS;*.DPR;*.INC;*.CPP';
end;

function TMsgExpExpert.GetName: string;
begin
  Result := 'Message_Expert';
end;

function TMsgExpExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Message Dialog';
begin
  Result := SDisplayName;
end;

procedure TMsgExpExpert.Click(Sender: TObject);
resourcestring
  SWrongFileType = 'This expert is for use in pas, dpr, inc, and cpp files only.';
var
  Dlg: TfmMessageDialog;
  FileName: string;
  EditWrite: TEditWriter;
  SourceType: TSourceType;
begin
  FileName := ToolServices.GetCurrentFile;
  if IsCpp(FileName) then
    SourceType := stCpp
  else if (IsDprOrPas(FileName) or IsInc(FileName)) then
    SourceType := stPascal
  else
    raise Exception.Create(SWrongFileType);

  try
    Dlg := TfmMessageDialog.Create(nil);
    try
      Dlg.SourceType := SourceType;
      Dlg.ShowModal;
      if Dlg.Code <> '' then
      begin
        Screen.Cursor := crHourGlass;
        EditWrite := TEditWriter.Create(ToolServices.GetCurrentFile);
        try
          EditWrite.WriteAtCurrentPos(Dlg.Code);
        finally
          EditWrite.Free;
          Screen.Cursor := crDefault;
        end;
      end;
    finally
      Dlg.Free;
    end;
  except
    on E: Exception do
      ShowExceptionErrorMessage(E);
  end;
end;

procedure TMsgExpExpert.LoadSettings;
begin
  inherited LoadSettings;

  // do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    MsgString := ReadString('MsgDlg', 'Concate', FMsgString);
  finally
    Free;
  end;
end;

procedure TMsgExpExpert.SaveSettings;
begin
  inherited SaveSettings;

  // do not localize
  with TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts') do
  try
    WriteString('MsgDlg', 'Concate', FMsgString);
  finally
    Free;
  end;
end;

procedure TMsgExpExpert.Configure;
begin
  with TfmMessageOptions.Create(nil) do
  try
    edtMsgString.Text := MsgString;
    if ShowModal = mrOK then
      FMsgString := edtMsgString.Text;
  finally
    Free;
  end;
end;

procedure TfmMessageDialog.meCaptionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    Key := 0;
    btnOK.Click;
  end;
end;

function TMsgExpExpert.IconFileName: string;
begin
  Result := 'Message';
end;

procedure TfmMessageDialog.btnTestClick(Sender: TObject);

  function GetMsgDlgType: TMsgDlgType;
  var
    i: Integer;
    ARadioButton: TRadioButton;
    ValueName: string;
  begin
    Result := mtCustom; // Remove compiler warning

    for i := 0 to gbTypes.ControlCount - 1 do
      if gbTypes.Controls[i] is TRadioButton then
      begin
        ARadioButton := TRadioButton(gbTypes.Controls[i]);
        if ARadioButton.Checked then
        begin
          ValueName := RemoveAmpersand(ARadioButton.Caption);
          Result := TMsgDlgType(GetEnumValue(TypeInfo(TMsgDlgType), ValueName));
          Assert(Result in [Low(TMsgDlgType)..High(TMsgDlgType)]);
          Break;
        end;
      end;
  end;

  function GetMsgDlgButtons: TMsgDlgButtons;
  var
    i: Integer;
    ACheckBox: TCheckBox;
    ValueName: string;
    SelectedMsgDlgButton: TMsgDlgBtn;
  begin
    Result := [];

    for i := 0 to gbButtons.ControlCount - 1 do
      if gbButtons.Controls[i] is TCheckBox then
      begin
        ACheckBox := TCheckBox(gbButtons.Controls[i]);
        if ACheckBox.Checked then
        begin
          ValueName := RemoveAmpersand(ACheckBox.Caption);
          SelectedMsgDlgButton := TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), ValueName));
          Assert(SelectedMsgDlgButton in [Low(TMsgDlgBtn)..High(TMsgDlgBtn)]);

          Result := Result + [SelectedMsgDlgButton];
        end;
      end;
  end;

  function GetHelpContext: Integer;
  begin
    Result := StrToIntDef(edHelpContext.Text, 0);
  end;

begin
  MessageDlg(meCaption.Text, GetMsgDlgType, GetMsgDlgButtons, GetHelpContext);
end;

initialization
  RegisterGX_Expert(TMsgExpExpert);
end.

 
