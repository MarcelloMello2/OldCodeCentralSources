unit [!UnitName];

// Created with USysWare Templates add-in ([!TimeStamp])

interface

uses
  System.Threading;

type
  T[!ClassName] = class
  private
    class var FLockObj: System.Object;
    class var FInstance: Integer;
    class var FSingleton: T[!ClassName];

    class constructor Create;

    constructor Create;
  protected
    procedure Initialize; virtual;
  public
    class function GetSingleton: T[!ClassName];

    class property Instance: Integer read FInstance;
  end;

implementation

{ T[!ClassName] }

class constructor T[!ClassName].Create;
begin
  FLockObj := System.Object.Create;
end;

constructor T[!ClassName].Create;
begin
  inherited Create;

  Initialize;
end;

procedure T[!ClassName].Initialize;
begin
  //ToDo: add your code here
end;

class function T[!ClassName].GetSingleton: T[!ClassName];
begin
  Monitor.Enter(FLockObj);

  if FSingleton = nil then
  begin
    FSingleton := T[!ClassName].Create;

    Inc(FInstance);
  end;

  Monitor.Exit(FLockObj);

  Result := FSingleton;
end;

end.