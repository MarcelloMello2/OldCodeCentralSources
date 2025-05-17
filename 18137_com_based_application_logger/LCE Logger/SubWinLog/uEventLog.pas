//
// This code is based on code from SvcMgr VCL unit
//

unit uEventLog;

{$X+}

interface

uses
  Windows;

type
  TEventLogType = (eltError, eltWarning, eltInfo, eltAuditOk, eltAuditErr);

  { TEventLog }

  TEventLog = class(TObject)
  private
    FName: string;
    FEventLog: Integer;
  public
    constructor Create(aName: string);
    destructor Destroy; override;
    procedure Log(aMessage: string; aType: TEventLogType = eltError;
      aCategory: Word = 0; aID: DWord = 0); overload;
    procedure Log(aMessage, aData: string; aType: TEventLogType = eltError;
      aCategory: Word = 0; aID: DWord = 0); overload;
    procedure LogMessage(Message: String; EventType: DWord; Category,
      ID: Integer; const AID: string = 'Interact');
  end;

implementation

const
  cLogTypeArr: array [TEventLogType] of DWord =
    (
      EVENTLOG_ERROR_TYPE,
      EVENTLOG_WARNING_TYPE,
      EVENTLOG_INFORMATION_TYPE,
      EVENTLOG_AUDIT_SUCCESS,
      EVENTLOG_AUDIT_FAILURE
    );

{ TEventLog }

constructor TEventLog.Create(aName: String);
begin
  FName := aName;
  FEventLog := 0;
end;

destructor TEventLog.Destroy;
begin
  if FEventLog <> 0 then
    DeregisterEventSource(FEventLog);
  inherited Destroy;
end;

procedure TEventLog.Log(aMessage: String; aType: TEventLogType;
  aCategory: Word; aID: DWord);
var
  P: Pointer;
begin
  P := PChar(aMessage);
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, cLogTypeArr[aType], aCategory, aID, nil, 1, 0, @P, nil);
end;

procedure TEventLog.Log(aMessage, aData: string; aType: TEventLogType = eltError;
  aCategory: Word = 0; aID: DWord = 0);
var
  P, D: Pointer;
  l: DWord;
begin
  P := PChar(aMessage);
  D := PChar(aData);
  l := Length(aData) + 1;
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, cLogTypeArr[aType], aCategory, aID, nil, 1, l, @P, D);
end;

procedure TEventLog.LogMessage(Message: string; EventType: DWord;
  Category, ID: Integer; const AID: string = 'Interact');
var
  P: Pointer;
begin
  P := PChar(Message);
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, EventType, Category, ID, nil, 1, 0, @P, nil);
end;

end.
