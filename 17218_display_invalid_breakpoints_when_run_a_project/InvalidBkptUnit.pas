unit InvalidBkptUnit;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils, ToolsAPI;

const
  UM_CHECKBREAKPOINTS = WM_USER + $1000;

type
  TInvalidBkptsExpert = class(TNotifierObject, IOTAWizard)
  private
    FDebuggerServices: IOTADebuggerServices;
    FInvalidBreakpoints: TStringList;
    FNotifierIndex: Integer;
    FWnd: HWND;
    procedure CheckInvalidBreakpoints;
    procedure WndProc(var Message: TMessage);
  protected
    procedure Execute;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDebuggerNotifier = class(TNotifierObject, IOTADebuggerNotifier)
  private
    FExpert: TInvalidBkptsExpert;
  protected
    procedure BreakpointAdded(Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(Breakpoint: IOTABreakpoint);
    procedure ProcessCreated(Process: IOTAProcess);
    procedure ProcessDestroyed(Process: IOTAProcess);
  public
    constructor Create(AExpert: TInvalidBkptsExpert);
  end;

procedure Register;

implementation

{$IFNDEF VER140}
const
  sLineBreak = #13#10;
{$ENDIF}  

resourcestring
  RsInvalidBreakpointLine = '%s Line: %d, FileName: %s' + sLineBreak;
  RsInvalidBreakpointsCaption = 'Invalid breakpoints';

procedure Register;
begin
  RegisterPackageWizard(TInvalidBkptsExpert.Create);
end;

function SortInvalidBreakpoints(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List[Index1], List[Index2]);
  if Result = 0 then
    Result := Integer(List.Objects[Index1]) - Integer(List.Objects[Index2]);
end;

{ TInvalidBkptsExpert }

procedure TInvalidBkptsExpert.CheckInvalidBreakpoints;
var
  I: Integer;
  Breakpoint: IOTASourceBreakpoint;
  S: string;
begin
  FInvalidBreakpoints.Clear;
  for I := 0 to FDebuggerServices.SourceBkptCount - 1 do
  begin
    Breakpoint := FDebuggerServices.SourceBkpts[I];
    if Breakpoint.Enabled and not Breakpoint.ValidInCurrentProcess then
      FInvalidBreakpoints.AddObject(Breakpoint.FileName, Pointer(Breakpoint.LineNumber));
  end;
  if FInvalidBreakpoints.Count > 0 then
  begin
    FInvalidBreakpoints.CustomSort(SortInvalidBreakpoints);
    S := '';
    for I := 0 to FInvalidBreakpoints.Count - 1 do
      S := Format(RsInvalidBreakpointLine, [S, Integer(FInvalidBreakpoints.Objects[I]), FInvalidBreakpoints[I]]);
    Application.MessageBox(PChar(S), PChar(RsInvalidBreakpointsCaption), MB_OK or MB_ICONWARNING);
  end;
end;

constructor TInvalidBkptsExpert.Create;
begin
  inherited;
  FDebuggerServices := BorlandIDEServices as IOTADebuggerServices;
  FNotifierIndex := FDebuggerServices.AddNotifier(TDebuggerNotifier.Create(Self));
  FInvalidBreakpoints := TStringList.Create;
  FWnd := AllocateHWnd(WndProc);
end;

destructor TInvalidBkptsExpert.Destroy;
begin
  if FNotifierIndex <> -1 then
    FDebuggerServices.RemoveNotifier(FNotifierIndex);
  FreeAndNil(FInvalidBreakpoints);
  DeallocateHWnd(FWnd);
  inherited;
end;

procedure TInvalidBkptsExpert.Execute;
begin
end;

function TInvalidBkptsExpert.GetIDString: string;
begin
  Result := 'PetrV.InvalidBkpt';
end;

function TInvalidBkptsExpert.GetName: string;
begin
  Result := ClassName;
end;

function TInvalidBkptsExpert.GetState: TWizardState;
begin
  Result := [];
end;

procedure TInvalidBkptsExpert.WndProc(var Message: TMessage);
begin
  with Message do
    case Msg of
      UM_CHECKBREAKPOINTS:
        begin
          CheckInvalidBreakpoints;
          Result := 1;
        end;
    else
      Result := DefWindowProc(FWnd, Msg, WParam, LParam);
    end;
end;

{ TDebuggerNotifier }

procedure TDebuggerNotifier.BreakpointAdded(Breakpoint: IOTABreakpoint);
begin
end;

procedure TDebuggerNotifier.BreakpointDeleted(Breakpoint: IOTABreakpoint);
begin
end;

constructor TDebuggerNotifier.Create(AExpert: TInvalidBkptsExpert);
begin
  FExpert := AExpert;
end;

procedure TDebuggerNotifier.ProcessCreated(Process: IOTAProcess);
begin
  PostMessage(FExpert.FWnd, UM_CHECKBREAKPOINTS, 0, 0);
end;

procedure TDebuggerNotifier.ProcessDestroyed(Process: IOTAProcess);
begin
end;

end.

