unit GX_FavOptions;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmFavOptions = class(TForm)
    gbxFavOptions: TGroupBox;
    chkConfirmFolderDelete: TCheckBox;
    chkExpandAllOnLoad: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblSaveFile: TLabel;
    edtSaveFile: TEdit;
    dlgSaveFile: TSaveDialog;
    sbnSaveFile: TSpeedButton;
    chkHideOnExecute: TCheckBox;
    chkShowPreview: TCheckBox;
    procedure sbnSaveFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfmFavOptions.sbnSaveFileClick(Sender: TObject);
var
  CurrentIdeFolder: string;
begin
  CurrentIdeFolder := GetCurrentDir;
  try
    if dlgSaveFile.Execute then
      edtSaveFile.Text := dlgSaveFile.FileName;
  finally
    SetCurrentDir(CurrentIdeFolder);
  end;
end;

end.
