unit GX_ClassReport;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, StdCtrls, SpinIntEdit, Controls, Classes, Forms;

type
  TfmClassReport = class(TForm)
    gbxPageSettings: TGroupBox;
    lblFont: TLabel;
    lblFontSize: TLabel;
    lblBoxSize: TLabel;
    lblInCharacters: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    lblBoxSpacing: TLabel;
    lblInPixels: TLabel;
    cbxFont: TComboBox;
    spnFontSize: TSpinIntEdit;
    spnBoxSize: TSpinIntEdit;
    spnBoxSpacing: TSpinIntEdit;
    procedure FormCreate(Sender: TObject);
  private
  end;

implementation

{$R *.DFM}

procedure TfmClassReport.FormCreate(Sender: TObject);
begin
  cbxFont.Items.Assign(Screen.Fonts);
end;

end.
