program FmtNumb;

uses
  Forms,
  MainNF in 'MainNF.pas' {Form1},
  NumbFmt in 'NumbFmt.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
