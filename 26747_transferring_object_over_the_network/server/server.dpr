program server;

uses
  Forms,
  servermain in 'servermain.pas' {Form1},
  customer in '..\common\customer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
