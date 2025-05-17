unit [!UnitName];

// Created with USysWare Templates add-in ([!TimeStamp])

interface

type
  T[!ClassName] = class
  private
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
  if FSingleton = nil then
  begin
    FSingleton := T[!ClassName].Create;
    
    Inc(FInstance);
  end;

  Result := FSingleton;
end;

end.