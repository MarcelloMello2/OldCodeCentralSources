program WCSample;

uses
  Forms,
  WCSample1 in 'WCSample1.pas' {Form1},
  WCIntf in 'WCIntf.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
