program SampleHost;

uses
  Forms,
  SHostU in 'SHostU.pas' {Form1},
  InterfU in 'BPL\InterfU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
