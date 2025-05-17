{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSSelDirCmpml;

uses
  Forms,
  BCSSelDirCmpwbu in 'BCSSelDirCmpwbu.pas' { BCSSelDirCmpC } ;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSSelDirCmp Application Dialog';
  Application.CreateForm(TBCSSelDirCmpC, BCSSelDirCmp);
  Application.Run;
end.
