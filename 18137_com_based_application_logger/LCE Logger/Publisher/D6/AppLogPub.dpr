program AppLogPub;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  AppLogEvents_TLB in '..\..\Event\AppLogEvents_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
