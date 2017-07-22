unit GX_CPULabelAdd;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TfmAddCPULabel = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtAddress: TEdit;
    edtLabel: TEdit;
    lblAddress: TLabel;
    lblLabel: TLabel;
    bvlLabel: TBevel;
  private
    function GetAddress: Pointer;
  public
    property Address: Pointer read GetAddress;
  end;

var
  fmAddCPULabel: TfmAddCPULabel;

implementation

{$R *.DFM}

function TfmAddCPULabel.GetAddress: Pointer;
begin
  Result := Pointer(StrToInt(edtAddress.Text));
end;

end.
