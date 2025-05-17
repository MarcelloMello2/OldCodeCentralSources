unit [!UnitName];

// Created with USysWare Templates add-in ([!TimeStamp])

interface

uses
  [!InterfaceUnitName];

type
  T[!ClientClassName] = class(System.Object, I[!ObserverInterfaceName])
  public
    constructor Create;

    // I[!ObserverInterfaceName] implementation
    procedure Notify(Obj: System.Object); virtual;
  end;

implementation

{ T[!ClientClassName] }

constructor T[!ClientClassName].Create;
begin
  inherited Create;

  //ToDo: add your code here
end;

procedure T[!ClientClassName].Notify(Obj: System.Object);
begin
  //ToDo: add your code here
end;

end.