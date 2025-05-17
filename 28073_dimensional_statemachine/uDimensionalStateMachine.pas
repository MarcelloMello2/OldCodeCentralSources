// Copyright, Atle Smelvær
// Free under GNU GPL License
// http://www.smelvaer.com
unit uDimensionalStateMachine;

interface

uses
  Generics.Collections, SysUtils, Classes;

type
  TDimensionalStateMachine<T> = class;
  TStateDimension<T> = class;

  TStateEvent<T> = procedure(const Data: T) of object;

  TStateItem<T> = class
  private
    FEvent: TStateEvent<T>;
    FNext: TStateDimension<T>;
  public
    constructor Create(AEvent: TStateEvent<T>; ANext: TStateDimension<T>);
    procedure Define(AEvent: TStateEvent<T>; ANext: TStateDimension<T>);
    property Event: TStateEvent<T> read FEvent write FEvent;
    property Next: TStateDimension<T> read FNext write FNext;
  end;

  EStateDimension = class(Exception);
  EStateDimensionMissing = class(EAbort);

  TStateDimension<T> = class
  private
    FName: string;
    FOwner: TDimensionalStateMachine<T>;
    FStates: TObjectList<TStateItem<T>>;
  protected
    property States: TObjectList<TStateItem<T>> read FStates;
  public
    constructor Create(AOwner: TDimensionalStateMachine<T>; const AName: string);
    destructor Destroy; override;
    procedure Run(AState: integer);
    function Copy(const ADimension: string): TStateDimension<T>;
    function Fix(AState: integer; const ANextDimension: string): TStateDimension<T>;
    function Del(AState: integer): TStateDimension<T>;
    function Add(AState: integer; AEvent: TStateEvent<T>; const ANextDimension: string): TStateDimension<T>;
    property Owner: TDimensionalStateMachine<T> read FOwner;
    property Name: string read FName;
  end;

  EDimensionalStateMachine = class(Exception);

  TStateLogEvent<T> = procedure(const State: string; const Dimension: string; Valid: boolean; const Data: T) of object;
  TStateExceptionEvent<T> = procedure(E: Exception; const State: string; const Dimension: string; const Data: T; var Handled: boolean) of object;

  TDimensionalStateMachine<T> = class
  private
    FOnLog: TStateLogEvent<T>;
    FOnException: TStateExceptionEvent<T>;
    FStateNames: array of string;
    FStateCount: integer;
    FCurrent: TStateDimension<T>;
    FInitialDimension: TStateDimension<T>;
    FDimensions: TObjectDictionary<string,TStateDimension<T>>;
  protected
    FData: T;
    procedure PrepareState(var State: integer; const StateName: string);
    procedure PrepareStates(AStates: array of PInteger; AStateNames: array of string);
    function Dimension(const AName: string): TStateDimension<T>;
    function InitialDimension(const AName: string): TStateDimension<T>;
    property StateCount: integer read FStateCount;
    procedure PrepareDimensions; virtual; abstract;
    procedure DoLog(State: integer; const Dimension: string; Valid: boolean; const Data: T);
    procedure DoException(E: Exception; State: integer; const Dimension: string; const Data: T; var Handled: boolean); virtual;
    property Current: TStateDimension<T> read FCurrent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure Run(AState: integer);
    property Data: T read FData write FData;
    property OnLog: TStateLogEvent<T> read FOnLog write FOnLog;
    property OnException: TStateExceptionEvent<T> read FOnException write FOnException;
  end;

resourcestring
  SEStateNotAllowed = 'State not allowed in this dimension';
  SEStateInvalid = 'State is invalid';
  SEStateNotPrepared = 'State has not been prepared';
  SEFixStateMissing = 'State missing, cannot adjust';
  SEMissingDimensionRegistration = 'Engine must have registered dimensions';
  SEDimensionsOutOfSync = 'Dimensions out of sync';

implementation

{ TDimensionalStateMachine<T> }

constructor TDimensionalStateMachine<T>.Create;
begin
  FCurrent := nil;
  FInitialDimension := nil;
  FDimensions := TObjectDictionary<string,TStateDimension<T>>.Create();
  PrepareDimensions;
end;

destructor TDimensionalStateMachine<T>.Destroy;
begin
  FDimensions.Free;
  inherited;
end;

function TDimensionalStateMachine<T>.Dimension(const AName: string): TStateDimension<T>;
begin
  if not FDimensions.TryGetValue(AName,result) then
  begin
    result := TStateDimension<T>.Create(self,AName);
    FDimensions.Add(AName,result);
  end;
end;

procedure TDimensionalStateMachine<T>.DoException(E: Exception; State: integer; const Dimension: string; const Data: T; var Handled: boolean);
begin
  if Assigned(FOnException) then
    FOnException(E,FStateNames[State],Dimension,Data,Handled);
  DoLog(State,Dimension,not (E is EStateDimensionMissing),Data);
  FCurrent := FInitialDimension;
end;

procedure TDimensionalStateMachine<T>.DoLog(State: integer; const Dimension: string; Valid: boolean; const Data: T);
begin
  if Assigned(FOnLog) then
    FOnLog(FStateNames[State],Dimension,Valid,Data);
end;

function TDimensionalStateMachine<T>.InitialDimension(const AName: string): TStateDimension<T>;
begin
  result := Dimension(AName);
  FInitialDimension := result;
  FCurrent := FInitialDimension;
end;

procedure TDimensionalStateMachine<T>.PrepareStates(AStates: array of PInteger; AStateNames: array of string);
var
  n: integer;
begin
  for n := low(AStates) to high(AStates) do
    AStates[n]^ := n;
  SetLength(FStateNames,length(AStateNames));
  for n := low(AStateNames) to high(AStateNames) do
    FStateNames[n] := AStateNames[n];
  FStateCount := length(AStates);
end;

procedure TDimensionalStateMachine<T>.Reset;
begin
  if FDimensions.Count > 0 then
    FCurrent := FInitialDimension
  else
    raise EDimensionalStateMachine.Create(SEMissingDimensionRegistration);
end;

procedure TDimensionalStateMachine<T>.Run(AState: integer);
var
  Handled: boolean;
begin
  Assert(FCurrent<>nil);
  try
    FCurrent.Run(AState);
  except
    on E: Exception do
    begin
      Handled := false;
      DoException(E,AState,FCurrent.Name,Data,Handled);
      if not Handled then
        raise;
    end;
  end;
end;

procedure TDimensionalStateMachine<T>.PrepareState(var State: integer; const StateName: string);
var
  n: integer;
begin
  n := length(FStateNames);
  SetLength(FStateNames,n+1);
  State := n;
  FStateNames[n] := StateName;
end;

{ TStateDimension<T> }

function TStateDimension<T>.Add(AState: integer; AEvent: TStateEvent<T>; const ANextDimension: string): TStateDimension<T>;
begin
  result := self;
  if AState < 0 then
    raise EStateDimension.Create(SEStateInvalid);
  if AState >= Owner.StateCount then
    raise EStateDimension.Create(SEStateNotPrepared);
  if Assigned(FStates[AState]) then
    FStates[AState].Define(AEvent,Owner.Dimension(ANextDimension))
  else
    FStates[AState] := TStateItem<T>.Create(AEvent,Owner.Dimension(ANextDimension));
end;

function TStateDimension<T>.Copy(const ADimension: string): TStateDimension<T>;
var
  d: TStateDimension<T>;
  s: TStateItem<T>;
  n: integer;
begin
  result := self;
  d := Owner.Dimension(ADimension);
  if States.Count <> d.States.Count then
    raise EStateDimension.Create(SEDimensionsOutOfSync);
  for n := 0 to States.Count-1 do
  begin
    FStates[n].Free;
    s := d.States[n];
    if Assigned(s) then
      FStates[n] := TStateItem<T>.Create(s.Event,s.Next)
    else
      FStates[n] := nil;
  end;
end;

constructor TStateDimension<T>.Create(AOwner: TDimensionalStateMachine<T>; const AName: string);
var
  n: integer;
begin
  FName := AName;
  FOwner := AOwner;
  FStates := TObjectList<TStateItem<T>>.Create;
  for n := 0 to Owner.StateCount-1 do
    FStates.Add(nil);
end;

function TStateDimension<T>.Del(AState: integer): TStateDimension<T>;
var
  s: TStateItem<T>;
begin
  result := self;
  if AState < 0 then
    raise EStateDimension.Create(SEStateInvalid);
  if AState >= Owner.StateCount then
    raise EStateDimension.Create(SEStateNotPrepared);
  if Assigned(FStates[AState]) then
    FStates[AState] := nil;
end;

destructor TStateDimension<T>.Destroy;
begin
  FStates.Free;
  inherited;
end;

function TStateDimension<T>.Fix(AState: integer; const ANextDimension: string): TStateDimension<T>;
begin
  result := self;
  if AState < 0 then
    raise EStateDimension.Create(SEStateInvalid);
  if AState >= Owner.StateCount then
    raise EStateDimension.Create(SEStateNotPrepared);
  if Assigned(FStates[AState]) then
    FStates[AState].Next := Owner.Dimension(ANextDimension)
  else
    raise EStateDimension.Create(SEFixStateMissing);
end;

procedure TStateDimension<T>.Run(AState: integer);
var
  State: TStateItem<T>;
begin
  Assert(AState>=0);
  Assert(AState<FStates.Count);
  State := FStates[AState];
  if Assigned(State) then
  begin
    State.Event(Owner.Data);
    Owner.DoLog(AState,Name,true,Owner.Data);
    Owner.FCurrent := State.Next;
  end
  else
    raise EStateDimensionMissing.Create(SEStateNotAllowed);
end;

{ TStateItem<T> }

procedure TStateItem<T>.Define(AEvent: TStateEvent<T>; ANext: TStateDimension<T>);
begin
  FEvent := AEvent;
  FNext := ANext;
end;

constructor TStateItem<T>.Create(AEvent: TStateEvent<T>; ANext: TStateDimension<T>);
begin
  Define(AEvent,ANext);
end;

end.
