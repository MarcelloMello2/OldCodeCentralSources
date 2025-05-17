{ Thread Controller
  Desenvolvimento iniciado em 30/07/2007 por
  Walter Frederico Bauchspiess }

unit ThreadControl;

interface

uses
  Classes, SysUtils, Windows;

type
  TControlledThread = class(TThread)
  protected
    procedure Terminate; virtual;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

  TThreadController = class
  private
    fThreads: TList;
    fLock: TMultiReadExclusiveWriteSynchronizer;
    procedure Add(const pThread: TControlledThread);
    procedure Remove(const pThread: TControlledThread);
  public
    constructor Create;
    destructor Destroy; override;

    function Count: integer;
    procedure TerminateThreads;
  end;

function ThreadController: TThreadController;

implementation

{$define UseDLLProc} // Define the termination method used on DLLs (DLLProc or Finalization)

var
  fThreadController: TThreadController;
  {$ifdef UseDLLProc}
  fLastDLLProc: TDLLProc;
  {$endif}

function ThreadController: TThreadController;
begin
  Result := fThreadController;
end;

procedure FinishThreadController;
begin
  fThreadController.Free;
  fThreadController := nil;
  // can't be FreeAndNil
end;

{$ifdef UseDLLProc}
procedure LibraryProc(Reason: Integer);
begin
  if Reason in [DLL_PROCESS_DETACH {, DLL_THREAD_DETACH}] then
    FinishThreadController;
  if Assigned(fLastDLLProc) then
    fLastDLLProc(Reason);
end;
{$endif}

{ TThreadController }

procedure TThreadController.Add(const pThread: TControlledThread);
begin
  fLock.BeginWrite;
  try
    fThreads.Add(pThread);
  finally
    fLock.EndWrite;
  end;
end;

function TThreadController.Count: integer;
begin
  fLock.BeginRead;
  try
    Result := fThreads.Count;
  finally
    fLock.EndRead;
  end;
end;

constructor TThreadController.Create;
begin
  inherited;
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
  fThreads := TList.Create;
end;

destructor TThreadController.Destroy;
var
  lFirst: boolean;
begin
  lFirst := True;
  while Count > 0 do
  begin
    TerminateThreads; // In the loop, because new threads could have been created
    if not lFirst then
      Sleep(17)
    else lFirst := False;
  end;
  fLock.Free;
  fThreads.Free;
  inherited;
end;

procedure TThreadController.Remove(const pThread: TControlledThread);
begin
  fLock.BeginWrite;
  try
    fThreads.Remove(pThread);
  finally
    fLock.EndWrite;
  end;
end;

procedure TThreadController.TerminateThreads;
var
  I: integer;
begin
  fLock.BeginRead;
  try
    for I := fThreads.Count - 1 downto 0 do
      with TControlledThread(fThreads[I]) do
        if not Terminated then
          Terminate;
  finally
    fLock.EndRead;
  end;
end;

{ TControlledThread }

procedure TControlledThread.AfterConstruction;
begin
  inherited;
  fThreadController.Add(Self);
end;

destructor TControlledThread.Destroy;
begin
  fThreadController.Remove(Self);
  inherited;
end;

procedure TControlledThread.Terminate;
begin
  inherited Terminate;
end;

initialization
  fThreadController := TThreadController.Create;
  {$ifdef UseDLLProc}
  if IsLibrary then
  begin
    fLastDLLProc := DLLProc;
    DLLProc := LibraryProc;
  end;
  {$endif}
finalization
  {$ifdef UseDLLProc} if not IsLibrary then {$endif}
    FinishThreadController;
end.
