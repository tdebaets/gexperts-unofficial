unit EIReg;

interface

uses
  EIPanel;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
  RegisterComponents('EII',[TEIPanel]);
end;

end.
