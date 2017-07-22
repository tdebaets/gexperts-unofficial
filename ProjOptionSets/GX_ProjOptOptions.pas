unit GX_ProjOptOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfmProjOptOptions = class(TForm)
    lblStoragePath: TLabel;
    edtPath: TEdit;
    sbnBrowse: TSpeedButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure sbnBrowseClick(Sender: TObject);
  private
    function GetPath: string;
    procedure SetPath(const Value: string);
  public
    property Path: string read GetPath write SetPath;
  end;

implementation

{$R *.DFM}

uses
  GX_GenFunc;

function TfmProjOptOptions.GetPath: string;
begin
  Result := AddSlash(edtPath.Text);
end;

procedure TfmProjOptOptions.sbnBrowseClick(Sender: TObject);
var
  Temp: string;
begin
  Temp := edtPath.Text;
  if GetDir(Self, Temp) then
    edtPath.Text := AddSlash(Temp);
end;

procedure TfmProjOptOptions.SetPath(const Value: string);
begin
  edtPath.Text := Value;
end;

end.
