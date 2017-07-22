unit GX_GrepOptions;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmGrepOptions = class(TForm)
    gbxOptions: TGroupBox;
    imgLogo: TImage;
    chkGrepSave: TCheckBox;
    chkGrepMiddle: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkGrepANSI: TCheckBox;
  private
  end;

implementation

{$R *.DFM}

end.
