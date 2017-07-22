unit GX_CodeNew;

//! StH: This unit has been completely prepared for localization
//! StH: This unit is fully compatible with C++Builder

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmCodeDBNew = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbxObject: TGroupBox;
    rbFolder: TRadioButton;
    rbCode: TRadioButton;
    edDesc: TEdit;
    lblDescription: TLabel;
    lblObjType: TLabel;
    rbRoot: TRadioButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TfmCodeDBNew.btnOKClick(Sender: TObject);
resourcestring
  SDescriptionRequired = 'You must enter a description';
begin
  if Length(edDesc.Text) <> 0 then
  begin
    ModalResult := mrOK;
  end
  else
  begin
    MessageDlg(SDescriptionRequired, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

end.
