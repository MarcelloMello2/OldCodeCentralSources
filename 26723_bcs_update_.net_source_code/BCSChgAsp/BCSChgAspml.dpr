{*-----------------------------------------------------------------------------
Local And Remote Domain Designator

  This is the Controlling Application module.

  @Author Mr. Arch Brooks
  @Version 1.0

-------------------------------------------------------------------------------}

program BCSChgAspml;

uses
  Forms,
  BCSChgAspwbu in 'BCSChgAspwbu.pas' {BCSChgAspC};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSChgAsp Application Dialog';
  Application.CreateForm(TBCSChgAspC, BCSChgAsp);
  Application.Run;
end.
