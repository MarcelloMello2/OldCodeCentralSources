{*-----------------------------------------------------------------------------
 Application Main Module

 This is the Main Application module.

 @author Mr. Arch Brooks
 @version 1.0

 -------------------------------------------------------------------------------}

program BCSPwbml;

uses
  Forms,
  BCSPwbwbu in 'BCSPwbwbu.pas'{BCSPwbC},
  BCSPwbdmu in 'BCSPwbdmu.pas'{BCSPwbD: TDataModule};
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSPwb Application Dialog';
  Application.CreateForm(TBCSPwbC, BCSPwbC);
  Application.CreateForm(TBCSPwbD, BCSPwbD);
  BCSPwbD.RConnStr :=
    'Provider=MSDASQL.1;Persist Security Info=False;Data Source=bcspwb';
  BCSPwbD.RTabName := 'cats';
  BCSPwbD.OpenRDS;
  BCSPwbC.RColor := $00AAE3FF;
  Application.Run;
  BCSPwbD.CloseRDS;

end.
