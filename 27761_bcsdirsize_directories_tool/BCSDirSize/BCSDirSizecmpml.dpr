{ *-----------------------------------------------------------------------------
  Application Main Module

  This is the Main Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

program BCSDirSizeCmpml;

uses
  Forms,
  BCSDirSizeCmpwbu in 'BCSDirSizeCmpwbu.pas' { BCSDirSizeCmpC } ;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSDirSizeCmp Application Dialog';
  Application.CreateForm(TBCSDirSizeCmpC, BCSDirSizeCmp);
  Application.Run;
end.
