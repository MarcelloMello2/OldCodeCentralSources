unit FileLog;

interface

uses ActiveX;

procedure LogEvent(const aApp, aMsg: string; eMsgKind: TOleEnum);
procedure SetLogFileName(const aFileName: string);

implementation

uses
  Windows,
  SysUtils;

type
  TEventLogType = (eltError, eltWarning, eltInfo, eltAuditOk, eltAuditErr);

const
  cLogType: array [TEventLogType] of string =
    ('Error', 'Warning', 'Info', 'AuditOk', 'AuditErr');

var
  LogFileName: string;

{* Support ********************************************************************}

function DebugTimeToStr(const aInterval: TDateTime): string;
var H, M, S, MS: WORD;
begin
  DecodeTime(aInterval, H, M, S, MS);
  Result := Format('%.2d:%.2d:%.2d %.3dms',[H, M, S, MS]);
end;

{* Public *********************************************************************}

procedure Log(const str: string);
var
  f: system.text;
  vNow: TDateTime;
begin
  vNow := Now;
  system.assign(f, LogFileName);
  if FileExists(LogFileName) then append(f) else system.rewrite(f);
  writeln(f,                                  // file
          FormatDateTime('mm/dd/yyyy', vNow), // date
          ', ',                               // divider
          DebugTimeToStr(vNow),               // time
          ', ',                               // divider
          str);                               // message
  close(f);
end;

procedure LogEvent(const aApp, aMsg: string; eMsgKind: TOleEnum);
begin
  Log(aApp + ', ' + aMsg + ', ' + cLogType[TEventLogType(eMsgKind)])
end;

procedure SetLogFileName(const aFileName: string);
begin
  LogFileName := aFileName;
end;

initialization
  SetLength(LogFileName, MAX_PATH);
  GetModuleFileName(HInstance, PChar(LogFileName), MAX_PATH);
  LogFileName := ChangeFileExt(PChar(LogFileName),'.LOG');

end.
