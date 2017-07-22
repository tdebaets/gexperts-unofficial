unit GX_FavNewFolder;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfmFavNewFolder = class(TForm)
    gbxNewFolder: TGroupBox;
    lblFolderName: TLabel;
    edtFolderName: TEdit;
    lblFolderType: TLabel;
    cbxFolderType: TComboBox;
    btnCancel: TButton;
    btnOK: TButton;
    procedure edtFolderNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbxFolderTypeMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
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
  GX_FavUtil, GX_FavFiles;

procedure TfmFavNewFolder.edtFolderNameChange(Sender: TObject);
begin
  btnOK.Enabled := (Length(edtFolderName.Text) > 0);
end;

procedure TfmFavNewFolder.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to MaxFolders-1 do
    cbxFolderType.Items.AddObject(FolderNames[i], Pointer(i));
  cbxFolderType.ItemIndex := 0;
end;

procedure TfmFavNewFolder.cbxFolderTypeDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  try
    with cbxFolderType.Canvas do
    begin
      if odSelected in State then
        Brush.Color := clHighlight
      else
        Brush.Color := clWindow;
      FillRect(Rect);
      (FFavoriteFilesForm as TfmFavFiles).imlFolders.Draw(cbxFolderType.Canvas, Rect.Left+3, Rect.Top+1, Index*2);
      TextOut(Rect.Left+22, Rect.Top+3, cbxFolderType.Items[Index]);
    end;
  except
    on E: Exception do
    begin
      // ignore
    end;
  end;
end;

procedure TfmFavNewFolder.cbxFolderTypeMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  Height := 18; //cbType.Canvas.TextHeight('W'); //! StH: This is not really good practice
end;

end.
