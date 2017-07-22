unit GX_ClassMethProp;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, StdCtrls, ExtCtrls, Graphics, Controls, Classes, Forms;

type
  TfmClassMethProp = class(TForm)
    gbxProperties: TGroupBox;
    imgProperties: TImage;
    lblMethodC: TLabel;
    lblInheritedFromC: TLabel;
    lblMethod: TLabel;
    lblInheritedFrom: TLabel;
    lblOtherInfoC: TLabel;
    btnOK: TButton;
    pnlDisabledChecks: TPanel;
    chkOverride: TCheckBox;
    chkAbstract: TCheckBox;
    chkMessageHandler: TCheckBox;
    chkVirtual: TCheckBox;
  private
  end;

implementation

{$R *.DFM}

end.
