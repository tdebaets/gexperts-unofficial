unit GX_BackupConfig;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{$IFNDEF VCLZIP}
interface implementation
{$ELSE VCLZIP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TfmBackupConfig = class(TForm)
    gbxOptions: TGroupBox;
    imgIcon: TImage;
    cbBackupInc: TCheckBox;
    cbIncludeDir: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblDirectives: TLabel;
    rgDefaultScope: TRadioGroup;
    gbBackupTarget: TGroupBox;
    rbBackupAskForFile: TRadioButton;
    rbBackupToDirectory: TRadioButton;
    lBackupDir: TLabel;
    edBackupDir: TEdit;
    sbBackupDir: TSpeedButton;
    cbSearchOnLibraryPath: TCheckBox;
    Button1: TButton;
    procedure sbBackupDirClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbGenericBackupTargetClick(Sender: TObject);
    procedure cbBackupIncClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  public
    procedure SetBackupTargetDirectoryEnabled(const Enable: Boolean);
  end;

implementation

{$R *.DFM}

uses
  GX_GenFunc, GX_ConfigurationInfo;

procedure TfmBackupConfig.sbBackupDirClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edBackupDir.Text;
  if GetDir(Self, Temp) then
    edBackupDir.Text := Temp;
end;

procedure TfmBackupConfig.FormShow(Sender: TObject);
begin
  {$IFNDEF GX_VER120_up}
    rgDefaultScope.Enabled := False;
  {$ENDIF GX_VER120_up}
  cbBackupIncClick(nil);
end;

procedure TfmBackupConfig.SetBackupTargetDirectoryEnabled(const Enable: Boolean);
begin
  lBackupDir.Enabled := Enable;
  edBackupDir.Enabled := Enable;
  sbBackupDir.Enabled := Enable;
end;

procedure TfmBackupConfig.rbGenericBackupTargetClick(Sender: TObject);
begin
  SetBackupTargetDirectoryEnabled(rbBackupToDirectory.Checked);
end;

procedure TfmBackupConfig.cbBackupIncClick(Sender: TObject);
begin
  cbSearchOnLibraryPath.Enabled := cbBackupInc.Checked;
end;

procedure TfmBackupConfig.Button1Click(Sender: TObject);
begin
  WinHelp(Self.Handle, PChar(ConfigInfo.HelpFile), HELP_CONTEXT, 10);
end;

{$ENDIF VCLZIP}

end.

