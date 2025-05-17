program NumPadTest;

uses
  Forms,
  NumPadTest1 in 'NumPadTest1.pas' {Form1},
  Numpad in 'Numpad.pas' {NumericKeypad};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TNumericKeypad, NumericKeypad);
  Application.Run;
end.
