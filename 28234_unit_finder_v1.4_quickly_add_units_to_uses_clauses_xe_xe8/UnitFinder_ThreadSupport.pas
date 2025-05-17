unit UnitFinder_ThreadSupport;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, SyncObjs;

type
  TffThread = class(TThread)
  private
    FReallyTerminated:LongBool;
    FSleepEvent:TEvent;
    FSleeping:boolean;
    FStarted:LongBool;
  protected
    procedure DoTerminate; override;
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    procedure Execute; override;
    function GetErrorInfo: string; virtual;
    procedure WrappedExecute; virtual; abstract;
    procedure ThreadSleep(MSec:Cardinal);
    procedure AbortSleep;
    procedure WaitForSleep(ProcessMessages:boolean);
    procedure TerminateAndWait;
    procedure Terminate; reintroduce;
    procedure Resume; reintroduce;
  end;


implementation

{** ffSystemSupport **}

function Suffix(const s,Sfx:string):string;
begin
  if s = '' then
    result := ''
  else
    result := s + Sfx;
end;

{*********************}


{
********************************** TffThread ***********************************
}
constructor TffThread.Create;
begin
  inherited Create(True);
  FSleepEvent := TEvent.Create(nil, False, False, '');
end;

destructor TffThread.Destroy;
begin
  if (FStarted) and (not Terminated) then
    raise Exception.Create('TffThread.Destroy: Thread not terminated.');
  FSleepEvent.Free;
  inherited;
end;

procedure TffThread.DoTerminate;
begin
  inherited;
  FReallyTerminated := True;
  FStarted := False;
end;

procedure TffThread.Execute;
begin
  try
    WrappedExecute;
    Terminate;
  except
    on E:Exception do begin
      Terminate;    
      E.Message :=  E.Message + #13#10#13#10+ 'Class: '+ClassName+#13#10+ Suffix(GetErrorInfo,#13#10)+#13#10;
      raise;
    end;
  end;
end;

function TffThread.GetErrorInfo: string;
begin
  result := '';
end;


procedure TffThread.Resume;
begin
  FStarted := True;
  inherited Resume;
end;

procedure TffThread.Terminate;
begin
  inherited Terminate;
  AbortSleep;
end;

procedure TffThread.TerminateAndWait;
var
  Hold_FreeOnTerminate:boolean;
begin
  if not FStarted then
    exit;
  if FReallyTerminated then
    exit;
  FReallyTerminated := False;
  Hold_FreeOnTerminate := FreeOnTerminate;
  FreeOnTerminate := False;
  Terminate;
  Application.ProcessMessages;
  while not FReallyTerminated do begin
    Application.ProcessMessages;
  end;
  if Hold_FreeOnTerminate then
    Free;
end;

procedure TffThread.ThreadSleep(MSec: Cardinal);
begin
  FSleeping := True;
  FSleepEvent.WaitFor(MSec);
end;

procedure TffThread.WaitForSleep(ProcessMessages:boolean);
begin
  while (not FSleeping) and (not Terminated) do begin
    if ProcessMessages then
      Application.ProcessMessages;
  end;
end;

procedure TffThread.AbortSleep;
begin
  FSleeping := False;
  FSleepEvent.SetEvent;
end;

end.
