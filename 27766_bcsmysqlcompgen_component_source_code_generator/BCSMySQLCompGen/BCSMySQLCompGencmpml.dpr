{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSMySQLCompGenCmpml;

uses
  Forms,
  BCSMySQLCompGenCmpwbu in 'BCSMySQLCompGenCmpwbu.pas' { BCSMySQLCompGenCmpC } ;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSMySQLCompGenCmp Application Dialog';
  Application.CreateForm(TBCSMySQLCompGenCmpC, BCSMySQLCompGenCmp);
  Application.Run;
end.
