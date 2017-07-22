program Grep;

uses
  Windows;

{$R *.RES}

const
{$IFDEF VER140}
  GExpertsDll = 'GExpert6.dll';
{$ENDIF VER140}
{$IFDEF VER130}
  GExpertsDll = 'GExpert5.dll';
{$ENDIF VER130}
{$IFDEF VER120}
  GExpertsDll = 'GExpert4.dll';
{$ENDIF VER120}
{$IFDEF VER100}
  GExpertsDll = 'GExpert3.dll';
{$ENDIF VER100}

procedure ShowGrep; external GExpertsDll;

begin
  ShowGrep;
end.
