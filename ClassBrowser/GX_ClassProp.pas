unit GX_ClassProp;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, StdCtrls, Controls, ComCtrls, Classes, Forms;

type
  TfmClassProp = class(TForm)
    pgeProperties: TPageControl;
    tabProps: TTabSheet;
    lblClassName: TLabel;
    edtClassName: TEdit;
    lblDerivedFrom: TLabel;
    edtDerivedFrom: TEdit;
    lblFileName: TLabel;
    lblLineNumber: TLabel;
    edtLineNo: TEdit;
    btnOK: TButton;
    mmoFileName: TMemo;
    lblUnit: TLabel;
    edtUnit: TEdit;
  private
  end;


implementation

{$R *.DFM}

end.
