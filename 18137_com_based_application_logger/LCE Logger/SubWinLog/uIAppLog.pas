unit uIAppLog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AppLogWin_TLB, StdVcl, AppLogEvents_TLB;

type
  TWinLog = class(TAutoObject, IAppLog)
  protected
    procedure Log(const App, Msg: WideString; Kind: eMsgKind); safecall;
    { Protected declarations }
  end;

implementation

uses
  ComServ,
  windows,
  WinLog;


{ TWinLog }

procedure TWinLog.Log(const App, Msg: WideString; Kind: eMsgKind);
begin
  WinLog.LogEvent(App, Msg, Kind);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TWinLog, Class_WinLog,
    ciMultiInstance, tmNeutral);
end.
