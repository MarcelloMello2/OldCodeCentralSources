{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSDirSizeml;

uses
  Forms,
  BCSDirSizewbu in 'BCSDirSizewbu.pas' { BCSDirSizeC } ;
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSDirSize Application Dialog';
  Application.CreateForm(TBCSDirSizeC, BCSDirSizeC);
  BCSDirSizeC.RColor := $00AAE3FF;
  BCSDirSizeC.RDirectory := 'C:\users\archman\downloads\';
  // BCSDirSizeC.RDirectoryStrings;
  BCSDirSizeC.RShowSize := True;
  BCSDirSizeC.RShowSize := False;
  Application.Run;

end.
