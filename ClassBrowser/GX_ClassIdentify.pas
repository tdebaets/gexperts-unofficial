unit GX_ClassIdentify;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, StdCtrls, Controls, Classes, Forms;

type
  TfmClassIdentify = class(TForm)
    gbxIdentifier: TGroupBox;
    lblNotes: TLabel;
    lblIdentifier: TLabel;
    edtID: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
  private
  end;

implementation

{$R *.DFM}

end.
