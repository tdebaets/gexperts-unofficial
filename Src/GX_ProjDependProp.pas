unit GX_ProjDependProp;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfmProjDependProp = class(TForm)
    pgeProperties: TPageControl;
    tabProperties: TTabSheet;
    btnOK: TButton;
    lblFileName: TLabel;
    laFileName: TStaticText;
    lbxSource: TListBox;
    lblSource: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
