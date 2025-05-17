unit WinLog;

interface

uses
  ActiveX;

procedure LogEvent(const aApp, aMsg: string; eMsgKind: TOleEnum);

implementation

uses
  uEventLog; // TEventLog

const
  cSourcePrefix   = 'AppLog.';

// core functions

procedure Log(const aSource, aMsg: string; const aType: TEventLogType);
var
  vEventLog: TEventLog;
begin
  vEventLog := TEventLog.Create(aSource);
  try
    vEventLog.Log(aMsg, aType, 0, 0{vThreadID});
  finally
    vEventLog.Free;
  end;
end;

procedure LogEvent(const aApp, aMsg: string; eMsgKind: TOleEnum);
begin
  Log(cSourcePrefix + aApp, aMsg, TEventLogType(eMsgKind));
end;

end.
