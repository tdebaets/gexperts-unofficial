unit GX_FavFileProp;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons;

type
  TfmFavFileProp = class(TForm)
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    lblFile: TLabel;
    lblName: TLabel;
    lblDescription: TLabel;
    btnCancel: TButton;
    btnOK: TButton;
    edtName: TEdit;
    edtDescription: TEdit;
    imlIcon: TImage;
    lblIcon: TLabel;
    lblExecuteType: TLabel;
    cbxExecuteType: TComboBox;
    edtFilename: TEdit;
    sbnFile: TSpeedButton;
    lblExecuteUsing: TLabel;
    edtExecuteUsing: TEdit;
    sbnExecute: TSpeedButton;
    procedure sbnFileClick(Sender: TObject);
    procedure edtFilenameExit(Sender: TObject);
    procedure sbnExecuteClick(Sender: TObject);
    procedure cbxExecuteTypeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
    FFavoriteFilesForm: TForm;
  public
    { Public declarations }
    property FavoriteFilesForm: TForm write FFavoriteFilesForm;
  end;

implementation

{$R *.DFM}

uses
  GX_FavFiles;

procedure TfmFavFileProp.sbnFileClick(Sender: TObject);
begin
  (FFavoriteFilesForm as TfmFavFiles).SetFilter;

  if FileExists(edtFileName.Text) then
    TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.FileName := edtFileName.Text;

  if TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.Execute then
  begin
    edtFilename.Text := TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.FileName;
    TfmFavFiles(FFavoriteFilesForm).GetIcon(edtFileName.Text, imlIcon);
  end;
end;

procedure TfmFavFileProp.edtFilenameExit(Sender: TObject);
begin
  TfmFavFiles(FFavoriteFilesForm).GetIcon(edtFileName.Text, imlIcon);
end;

procedure TfmFavFileProp.sbnExecuteClick(Sender: TObject);
begin
  (FFavoriteFilesForm as TfmFavFiles).dlgGetFiles.FilterIndex := 7;
  if FileExists(edtExecuteUsing.Text) then
    TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.FileName := edtExecuteUsing.Text;
  if TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.Execute then
    edtExecuteUsing.Text := TfmFavFiles(FFavoriteFilesForm).dlgGetFiles.FileName;
end;

procedure TfmFavFileProp.cbxExecuteTypeClick(Sender: TObject);
var
  ItemEnabled: Boolean;
begin
  ItemEnabled := (cbxExecuteType.ItemIndex = 2);

  sbnExecute.Enabled := ItemEnabled;
  edtExecuteUsing.Enabled := ItemEnabled;
  if ItemEnabled then
    edtExecuteUsing.Color := clWindow
  else
    edtExecuteUsing.Color := clBtnFace;
end;

procedure TfmFavFileProp.FormActivate(Sender: TObject);
begin
  cbxExecuteTypeClick(cbxExecuteType);
end;

end.

