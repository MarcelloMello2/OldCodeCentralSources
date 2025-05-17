program DimensionalCalc;

uses
  Forms,
  fMain in 'fMain.pas' {fmCalculator};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmCalculator, fmCalculator);
  Application.Run;
end.
