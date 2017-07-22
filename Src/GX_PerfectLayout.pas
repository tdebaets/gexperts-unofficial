unit GX_PerfectLayout;

{$I GX_CondDefine.inc}

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, GX_Experts, ToolIntf, ExptIntf, Registry;

type
  TfmPerfectLayout = class(TForm)
    gbxLayout: TGroupBox;
    pnlLayout1: TPanel;
    shpMain1: TShape;
    shpOI1: TShape;
    shpEditor1: TShape;
    shpWatch1: TShape;
    lblWatch1: TLabel;
    lblMain1: TLabel;
    lblOI1: TLabel;
    lblEditor1: TLabel;
    pnlLayout2: TPanel;
    shpMain2: TShape;
    shpOI2: TShape;
    shpEditor2: TShape;
    shpWatch2: TShape;
    lblWatch2: TLabel;
    lblMain2: TLabel;
    lblOI2: TLabel;
    lblEditor2: TLabel;
    rbnLayout1: TRadioButton;
    rbnLayout2: TRadioButton;
    rbnCustom: TRadioButton;
    btnCustom: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure btnCustomClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
  end;

  TLayoutType = (ltLayout1, ltLayout2, ltCustom);

  TLayoutExpert = class(TGX_EnhExpert)
  private
    FLayoutType: TLayoutType;
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
    property LayoutType: TLayoutType read FLayoutType write FLayoutType;
  end;

implementation

{$R *.DFM}

uses
  Menus, GX_GenFunc, GX_GExperts, GX_ConfigurationInfo, GX_VerDepConst;

function IsValidMoveableForm(Form: TCustomForm; AllowParented: Boolean): Boolean;
begin
  Result := False;
  if (Form = nil) then
    Exit;
  // The OI can be repositioned when parented to a docking host in D4+
  if (not AllowParented) and (Form.Parent <> nil) then
    Exit;
  // Don't save the state of the two GExperts configuration dialogs
  if (Form.Name = 'fmConfiguration') or (Form.Name = 'fmPerfectLayout') then
    Exit;
  // Don't save project forms or invisible forms
  if (Form.Designer <> nil) or (not Form.Visible) then
    Exit;
  Result := True;
end;

procedure TfmPerfectLayout.btnCustomClick(Sender: TObject);
var
  RegIni: TRegIniFile;
  i: Integer;

  procedure SaveWindow(Form: TForm);
  begin
    RegIni.WriteInteger(Form.Name, 'Left', Form.Left);
    RegIni.WriteInteger(Form.Name, 'Top', Form.Top);
    RegIni.WriteInteger(Form.Name, 'Height', Form.Height);
    RegIni.WriteInteger(Form.Name, 'Width', Form.Width);
  end;

begin
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts\Layout');
  try
    TRegistry(RegIni).DeleteKey(ConfigInfo.RegKey + '\GExperts\Layout');
    // Not necessary - causes relative registry key opening, anyway
    //RegIni.OpenKey(ConfigInfo.RegKey + '\GExperts\Layout', True);
    for i := 0 to Screen.FormCount - 1 do
      if IsValidMoveableForm(Screen.Forms[i], False) then
        SaveWindow(TForm(Screen.Forms[i]));
  finally
    RegIni.Free;
  end;
end;

procedure TfmPerfectLayout.btnHelpClick(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 25);
end;

{ TLayoutExpert }

constructor TLayoutExpert.Create;
begin
  inherited Create;
  {$IFDEF GX_VER130_up}
  DefaultActive := False; // Delphi 5+ provide more powerful saved desktops
  {$ENDIF GX_VER130_up}
  ShortCut := Menus.ShortCut(Word('L'), [ssCtrl, ssShift]);
  HasConfigOptions := True;
  HasMenuItem := True;
end;

destructor TLayoutExpert.Destroy;
begin
  inherited Destroy;
end;

function TLayoutExpert.GetMenuCaption: string;
resourcestring
  SMenuCaption = 'Perfect La&yout';
begin
  Result := SMenuCaption;
end;

function TLayoutExpert.GetMenuName: string;
begin
  Result := 'GX_Layout';  // do not localize
end;

function TLayoutExpert.GetMenuMask: string;
begin
  Result := '';
end;

function TLayoutExpert.GetName: string;
begin
  Result := 'Layout_Expert';  // do not localize
end;

function TLayoutExpert.GetDisplayName: string;
resourcestring
  SDisplayName = 'Perfect Layout';
begin
  Result := SDisplayName;
end;

procedure TLayoutExpert.Click(Sender: TObject);

  procedure LoadCustomLayout;
  var
    RegIni: TRegIniFile;
    i: Integer;

    procedure LoadWindow(Form: TForm);
    begin
      Form.WindowState := wsNormal;
      // do not localize any of the below
      Form.Left := RegIni.ReadInteger(Form.Name, 'Left', Form.Left);
      Form.Top := RegIni.ReadInteger(Form.Name, 'Top', Form.Top);
      Form.Height := RegIni.ReadInteger(Form.Name, 'Height', Form.Height);
      Form.Width := RegIni.ReadInteger(Form.Name, 'Width', Form.Width);
    end;

  begin
    RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts\Layout');
    try
      for i := 0 to Screen.FormCount - 1 do
        if IsValidMoveableForm(Screen.Forms[i], False) then
          LoadWindow(Screen.Forms[i]);
    finally
      RegIni.Free;
    end;
  end;

  function FindForm(Name: string; AllowParented: Boolean): TForm;
  var
    App: TCustomForm;
  begin
    Result := Application.FindComponent(Name) as TForm;
    if Result = nil then
    begin
      App := GetIdeMainForm;
      if App <> nil then
        Result := App.FindComponent(Name) as TForm;
    end;
    if not IsValidMoveableForm(Result, AllowParented) then
      Result := nil;
  end;

resourcestring
  SCouldNotFindAppBuilder = 'Could not find IDE AppBuilder window.';
var
  OI, Watch, Editor: TCustomForm;
  App: TCustomForm;
  Left, Width, Top, Bottom: Integer;
  R: TRect;
begin
  if LayoutType = ltCustom then
  begin
    LoadCustomLayout;
    Exit;
  end;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);

  App := GetIdeMainForm;
  if App = nil then
  begin
    MessageDlg(SCouldNotFindAppBuilder, mtError, [mbOK], 0);
    Exit;
  end;
  App.WindowState := wsNormal;
  App.SetBounds(R.Left, R.Top, R.Right-R.Left, App.Height);
  Top := R.Top + App.Height;
  Bottom := R.Bottom - R.Top;
  Watch := FindForm('WatchWindow', False);  // do not localize
  if Watch <> nil then
  begin
    if Watch.Visible {$IFDEF GX_VER120_up} and Watch.Floating {$ENDIF} then
    begin
      Watch.WindowState := wsNormal;
      Watch.SetBounds(R.Left, R.Bottom - Watch.Height, R.Right - R.Left, Watch.Height);
      Bottom := Watch.Top;
    end;
  end;
  OI := FindForm('PropertyInspector', True); // do not localize
  if OI <> nil then
  begin
    // In case the OI is docked:
    if GetParentForm(OI) <> nil then
      OI := GetParentForm(OI);
    OI.Top := Top;
    OI.Height := Bottom - Top;
    if LayoutType = ltLayout1 then
    begin
      OI.Left := R.Left;
      Left := OI.Left + OI.Width;
    end
    else
    begin
      OI.Left := R.Right - R.Left - OI.Width;
      Left := R.Left;
    end;
    Width := R.Right - R.Left - OI.Width;
  end
  else
  begin
    Left := R.Left;
    Width := R.Right - R.Left;
  end;
  Editor := FindForm('EditWindow_0', False); // do not localize
  if Editor <> nil then
  begin
    Editor.WindowState := wsNormal;
    Editor.SetBounds(Left, Top, Width, Bottom - Top);
  end;
end;

procedure TLayoutExpert.LoadSettings;
var
  RegIni: TRegIniFile;
begin
  inherited LoadSettings;

  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    FLayoutType := TLayoutType(RegIni.ReadInteger('Misc', 'Layout', 0));
  finally
    RegIni.Free;
  end;
end;

procedure TLayoutExpert.SaveSettings;
var
  RegIni: TRegIniFile;
begin
  inherited SaveSettings;

  // do not localize any of the below items
  RegIni := TRegIniFile.Create(ConfigInfo.RegKey + '\GExperts');
  try
    RegIni.WriteInteger('Misc', 'Layout', Ord(FLayoutType));
  finally
    RegIni.Free;
  end;
end;

procedure TLayoutExpert.Configure;
var
  Dlg: TfmPerfectLayout;
begin
  Dlg := TfmPerfectLayout.Create(nil);
  try
    Dlg.rbnLayout1.Checked := (FLayoutType = ltLayout1);
    Dlg.rbnLayout2.Checked := (FLayoutType = ltLayout2);
    Dlg.rbnCustom.Checked := (FLayoutType = ltCustom);
    if Dlg.ShowModal = mrOK then
    begin
      if Dlg.rbnLayout1.Checked then
        FLayoutType := ltLayout1
      else
      if Dlg.rbnLayout2.Checked then
        FLayoutType := ltLayout2
      else
      if Dlg.rbnCustom.Checked then
        FLayoutType := ltCustom;
    end;
  finally
    Dlg.Free;
  end;
end;

function TLayoutExpert.IconFileName: string;
begin
  Result := 'Layout';
end;

procedure TLayoutExpert.SetActive(New: Boolean);
begin
  if New <> Active then
  begin
    inherited SetActive(New);
    if New then
      // Nothing to initialize here
    else
    begin
      // Nothing to free here as Perfect Layout is a modal form
    end;
  end;
end;

initialization
  RegisterGX_Expert(TLayoutExpert);

end.

