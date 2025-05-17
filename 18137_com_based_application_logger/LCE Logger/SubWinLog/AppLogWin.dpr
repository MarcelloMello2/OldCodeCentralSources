library AppLogWin;

uses
  ComServ,
  AppLogWin_TLB in 'AppLogWin_TLB.pas',
  AppLogEvents_TLB in 'AppLogEvents_TLB.pas',
  uIAppLog in 'uIAppLog.pas' {WinLog: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
