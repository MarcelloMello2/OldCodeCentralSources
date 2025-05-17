{
  Interfaces + Classes is greater than the sum

  Written by Wayne Niddery, February 2001
  This sample unit may be freely copied, used, and distributed.

  First, two simple interfaces and corresponding implementation
  classes are defined: An IClock interface that simply returns
  the current time on request and an ICalculator interface that
  implements addition and subtraction. These two classes can be
  used independently by applications.

  A third interface, IElapsedTime, is defined to provide timing
  facilities, it can start, stop, and report elapsed seconds

  A class, TElapsedTime is defined that implements all three
  interfaces, each in a different way. The IClock implementation is
  inherited from the TClock class and allows it to query time values,
  the IElapsedTime interface is implemented directly, and the
  ICalculator interface is implemented by delegation using the
  'implenents' feature of properties in order to provide calculation
  facilities for the elapsed time. Note that, in addition to the
  IElapsedTime interface, both the IClock and ICalculator interfaces
  are available to the user of the TElapsedTime class by using the
  'as' operator.
}

unit WCIntf;

interface

uses Windows, SysUtils;

type
  // Simple clock interface to get current time
  IClock = interface
    ['{E3E07520-0551-11D5-9B8F-006097D40B37}']
    function GetCurrentTime: TDateTime;
    property CurrentTime: TDateTime read GetCurrentTime;
  end;

  // Implements clock interface.
  // Descending from TInterfacedObject provides us with an
  // implementation of IUnknown that provides reference counting
  TClock = class(TInterfacedObject, IClock)
  private
    function GetCurrentTime: TDateTime;
  public
    property CurrentTime: TDateTime read GetCurrentTime;
  end;

  // Simple 2 function calculator interface.
  ICalculator = interface
    ['{E3E07521-0551-11D5-9B8F-006097D40B37}']
    function Add(const a: integer): integer;
    function Subtract(const a: integer): integer;
    function GetValue: integer;
    procedure SetValue(const Value: integer);
    property Value: integer read GetValue write SetValue;
  end;

  // Implements calculator interface
  TCalculator = class(TInterfacedObject, ICalculator)
  private
    FValue: integer;
    function GetValue: integer;
    procedure SetValue(const Value: integer);
  public
    constructor Create;
    function Add(const a: integer): integer;
    function Subtract(const a: integer): integer;
    property Value: integer read GetValue write SetValue;
  end;

  // interface to calculate elapsed time
  IElapsedTime = interface
    ['{67314271-0557-11D5-9B8F-006097D40B37}']
    function Start: TDateTime;
    function Stop: TDateTime;
    function ElapsedSeconds: integer;
  end;

  // this class is both a clock and a calculator
  // and uses both in order to produce elapsed time
  TElapsedTime = class(TClock, IClock, IElapsedTime, ICalculator)
  private
    FCalculator : ICalculator;
    FStartTime: TDateTime;
    FStopTime: TDateTime;
  public
    constructor Create;
    function Start: TDateTime;
    function Stop: TDateTime;
    function ElapsedSeconds: integer;
    property Calculator: ICalculator read FCalculator
        implements ICalculator;
  end;

implementation

{ TClock }

function TClock.GetCurrentTime: TDateTime;
begin
  Result := Time;
end;

{ TCalculator }

constructor TCalculator.Create;
begin
  inherited Create;
  FValue := 0;
end;

function TCalculator.Add(const a: integer): integer;
begin
  Inc(FValue, a);
  Result := FValue;
end;

function TCalculator.Subtract(const a: integer): integer;
begin
  Dec(FValue, a);
  Result := FValue;
end;

function TCalculator.GetValue: integer;
begin
  Result := FValue;
end;

procedure TCalculator.SetValue(const Value: integer);
begin
  FValue := Value;
end;

{ TElapsedTime }

constructor TElapsedTime.Create;
begin
  inherited Create;
  // we must create an instance of a class that
  // implements the calculator interface
  FCalculator := TCalculator.Create;
end;

function TElapsedTime.Start: TDateTime;
begin
  FStartTime := CurrentTime;
  Result := FStartTime;
end;

function TElapsedTime.Stop: TDateTime;
begin
  FStopTime := CurrentTime;
  Result := FStopTime;
end;

function TElapsedTime.ElapsedSeconds: integer;
var hh, mm, ss, ms: word;
begin
  // get the current time from the clock interface
  DecodeTime(FStopTime, hh, mm, ss, ms);
  // use the calculator interface to get elapsed time
  with Calculator do begin
    Value := 0;
    Add(hh * 3600 + mm * 60 + ss);
    DecodeTime(FStartTime, hh, mm, ss, ms);
    Subtract(hh * 3600 + mm * 60 + ss);
    Result := Value;
  end;
end;

end.
