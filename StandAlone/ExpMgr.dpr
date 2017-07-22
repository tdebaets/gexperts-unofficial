program ExpMgr;

uses
  Windows;

{$R *.RES}

const
{$IFDEF VER100}
  GExpertsDll = 'GExpert3.dll';
{$ENDIF}
{$IFDEF VER120}
  GExpertsDll = 'GExpert4.dll';
{$ENDIF VER120}
{$IFDEF VER130}
  GExpertsDll = 'GExpert5.dll';
{$ENDIF VER130}
{$IFDEF VER140}
  GExpertsDll = 'GExpert6.dll';
{$ENDIF VER140}


procedure ShowExpertManager; external GExpertsDll;
procedure CloseExpertManager; external GExpertsDll;

begin
  try
    ShowExpertManager;
  finally
    CloseExpertManager;
  end;
end.
