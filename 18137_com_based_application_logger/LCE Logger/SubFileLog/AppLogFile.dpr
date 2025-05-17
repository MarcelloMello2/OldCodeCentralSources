library AppLogFile;

uses
  ComServ,
  AppLogFile_TLB in 'AppLogFile_TLB.pas',
  AppLogEvents_TLB in 'AppLogEvents_TLB.pas',
  uIAppLog in 'uIAppLog.pas' {FileLog: CoClass};
  //COMSVCSLib_TLB in 'COMSVCSLib_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
