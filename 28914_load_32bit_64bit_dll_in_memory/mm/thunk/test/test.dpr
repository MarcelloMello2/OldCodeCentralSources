program test;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MemoryModule in '..\src\MemoryModule.pas';

begin
  Application.Initialize;
  //Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
