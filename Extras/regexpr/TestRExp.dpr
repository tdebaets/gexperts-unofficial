program TestRExp;

uses
  Forms,
  TestRE in 'TestRE.pas' {fmTestRE},
  RegExpr in 'RegExpr.pas',
  PCode in 'PCode.pas' {fmPseudoCodeViewer};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfmTestRE, fmTestRE);
  Application.Run;
end.
