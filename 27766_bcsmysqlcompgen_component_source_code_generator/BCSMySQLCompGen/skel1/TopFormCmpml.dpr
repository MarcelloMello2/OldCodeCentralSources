{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program TopFormCmpml;

uses
  Forms,
  TopFormCmpwbu in 'TopFormCmpwbu.pas' { TopFormCmpC } ;
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'TopFormCmp Application Dialog';
  Application.CreateForm(TTopFormCmpC, TopFormCmp);
  TopFormCmp.RConStr :=
    'Provider=MSDASQL.1;Persist Security Info=False;Data Source=eagle';
  TopFormCmp.RDEName := 'sir_name';
  TopFormCmp.RTableName := 'eag01';
  TopFormCmp.ADOTable1.Open;
  TopFormCmp.ADOTable1.Last;
  TopFormCmp.ADOTable1.First;
  TopFormCmp.RBookMark := TopFormCmp.ADOTable1.GetBookmark;
  Application.Run;

end.
