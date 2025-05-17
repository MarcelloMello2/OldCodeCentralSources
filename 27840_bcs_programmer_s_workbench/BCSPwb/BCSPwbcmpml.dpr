{*-----------------------------------------------------------------------------
 Brooks Computing Systems Programmer's Workbench Component Tester
 <br>
 This component testing tool is designed to allow the user to put the
 Brooks Computing Systems Programmer's Workbench Component through all
 it's available fucntionality.

 @Author Mr. Arch Brooks
 @Version 1.0

 -------------------------------------------------------------------------------}

program BCSPwbcmpml;

uses
  Forms,
  BCSPwbcmpwbu in 'BCSPwbcmpwbu.pas'{BCSPwbcmpC};
{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BCSPwbcmp Application Dialog';
  Application.CreateForm(TBCSPwbcmpC, BCSPwbcmpC);
  BCSPwbcmpC.RColor := $00AAE3FF;
  Application.Run;

end.
