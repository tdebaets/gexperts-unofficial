unit GX_ClipboardOptions;

interface

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmClipboardOptions = class(TForm)
    gbxClipboardOptions: TGroupBox;
    imgLogo: TImage;
    lblMaxEntries: TLabel;
    edtMaxClip: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    chkAutoStart: TCheckBox;
    chkAutoClose: TCheckBox;
  private
  end;

implementation

{$R *.DFM}

end.
