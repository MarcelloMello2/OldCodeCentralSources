library AppLogEvents;

uses
  ComServ,
  AppLogEvents_TLB in 'AppLogEvents_TLB.pas',
  uIAppLog in 'uIAppLog.pas' {AppLog: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
