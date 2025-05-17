unit uEventLog;

interface

uses
  Windows;

type
  TEventLog = class
  private
    FHandle: THandle;
    FName: String;
    procedure Open;
    procedure Close;
  public
    type TLogType =
      (
        ltError = EVENTLOG_ERROR_TYPE,
        ltWarning = EVENTLOG_WARNING_TYPE,
        ltInformation = EVENTLOG_INFORMATION_TYPE,
        ltAuditSuccess = EVENTLOG_AUDIT_SUCCESS,
        ltAuditFailure = EVENTLOG_AUDIT_FAILURE
      );
    // Constructor & Destructor
    constructor Create(const iName: String); reintroduce;
    destructor Destroy; override;
    // Methods
    procedure E(const iMsg: String);
    procedure W(const iMsg: String);
    procedure I(const iMsg: String);
    procedure Add(
      const iLogType: TLogType;
      const iCategory, iEventId: Integer;
      const iMsg: String;
      const iDataLength: Integer;
      const iData: Pointer);
  end;

implementation

uses
  SysUtils;

{ TEventLog }

procedure TEventLog.Add(
  const iLogType: TLogType;
  const iCategory, iEventId: Integer;
  const iMsg: String;
  const iDataLength: Integer;
  const iData: Pointer);
var
  Ptr: Pointer;
begin
  Ptr := PChar(iMsg);

  Open;
  try
    ReportEvent(
      FHandle,
      Ord(iLogType), // Event Type
      iCategory,     // Category
      iEventId,      // Event Id
      nil,           // User Sid
      1,             // 書き込む文字列の数
      iDataLength,   // 書き込むバイナリデータのバイト数
      @Ptr,          // 書き込む文字列
      iData);        // バイナリデータ
  finally
    Close;
  end;
end;

procedure TEventLog.Close;
begin
  if (FHandle <> 0) then begin
    DeregisterEventSource(FHandle);
    FHandle := 0;
  end;
end;

constructor TEventLog.Create(const iName: String);
begin
  inherited Create;

  FName := iName;
end;

destructor TEventLog.Destroy;
begin
  Close;

  inherited;
end;

procedure TEventLog.E(const iMsg: String);
begin
  Add(ltError, 0, 0, iMsg, 0, nil);
end;

procedure TEventLog.I(const iMsg: String);
begin
  Add(ltInformation, 0, 0, iMsg, 0, nil);
end;

procedure TEventLog.Open;
begin
  if (FHandle = 0) then
    FHandle := RegisterEventSource(nil, PChar(FName));
end;

procedure TEventLog.W(const iMsg: String);
begin
  Add(ltWarning, 0, 0, iMsg, 0, nil);
end;

end.
