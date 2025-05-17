{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSMySQLCompGenml;

uses
  Forms,
  BCSMySQLCompGenwbu in 'BCSMySQLCompGenwbu.pas' { BCSMySQLCompGenC } ;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSMySQLCompGen Application Dialog';
  Application.CreateForm(TBCSMySQLCompGenC, BCSMySQLCompGenC);
  Application.Run;
end.
