{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSSelDirml;

uses
  Forms,
  BCSSelDirwbu in 'BCSSelDirwbu.pas' { BCSSelDirC } ;
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSSelDir Application Dialog';
  Application.CreateForm(TBCSSelDirC, BCSSelDirC);
  BCSSelDirC.RCaption := 'Select A Directory Now!';
  BCSSelDirC.RSelDir := '';
  Application.Run;

end.
