unit GX_BackupOptions;

{$I GX_CondDefine.inc}

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

{$IFNDEF VCLZIP}
interface implementation
{$ELSE VCLZIP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmBackupOptions = class(TForm)
    GroupBox1: TGroupBox;
    cbIncludeDir: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbPassword: TCheckBox;
    Label1: TLabel;
    edPassword: TEdit;
    rgScope: TRadioGroup;
    cbSearchLibraryPath: TCheckBox;
    procedure cbPasswordClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  end;

implementation

{$R *.DFM}

procedure TfmBackupOptions.cbPasswordClick(Sender: TObject);
begin
  edPassword.Enabled := cbPassword.Checked;
  if edPassword.Enabled then
  begin
    edPassword.Color := clWindow;
    try
      if edPassword.CanFocus and Visible then
        edPassword.SetFocus;
    except
    end;
  end
  else
    edPassword.Color := clBtnface;
  edPassword.Refresh;
end;

procedure TfmBackupOptions.FormShow(Sender: TObject);
begin
  {$IFNDEF GX_VER120_up}
    rgScope.Enabled := False;
  {$ENDIF GX_VER120_up}
end;

{$ENDIF VCLZIP}

end.
