{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program TopFormml;

uses
  Forms,
  TopFormwbu in 'TopFormwbu.pas' { TopFormC } ;
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TopForm Application Dialog';
  Application.CreateForm(TTopFormC, TopFormC);
  TopFormC.ADOConnection1.ConnectionString :=
    'Provider=MSDASQL.1;Persist Security Info=False;Data Source=bcswebtools';
  TopFormC.ADOTable1.TableName := 'categoreis';
  TopFormC.DBEdit1.DataField := 'categories';
  TopFormC.ADOConnection1.Connected := True;
  TopFormC.ADOTable1.Open;
  Application.Run;

end.
