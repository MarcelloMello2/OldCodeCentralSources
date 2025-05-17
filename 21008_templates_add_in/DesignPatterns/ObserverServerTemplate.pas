unit [!UnitName];

// Created with USysWare Templates add-in ([!TimeStamp])

interface

uses
  System.Collections,
  [!InterfaceUnitName];

type
  T[!ServerClassName] = class(System.Object, I[!NotifierInterfaceName])
  private
    FHastTable: Hashtable;
  public
    constructor Create;

    // I[!NotifierInterfaceName] implementation
    procedure RegisterObserver(Obj: I[!ObserverInterfaceName]); virtual;
    procedure UnregisterObserver(Obj: I[!ObserverInterfaceName]); virtual;
    procedure NotifyAll(Obj: System.Object); virtual;
  end;

implementation

{ T[!ServerClassName] }

constructor T[!ServerClassName].Create;
begin
  inherited Create;

  FHastTable := Hashtable.Create

  //ToDo: add your code here
end;

procedure T[!ServerClassName].RegisterObserver(Obj: I[!ObserverInterfaceName]);
begin
  FHastTable.Add(Obj, Obj);

  //ToDo: add your code here
end;

procedure T[!ServerClassName].UnregisterObserver(Obj: I[!ObserverInterfaceName]);
begin
  FHastTable.Remove(Obj);

  //ToDo: add your code here
end;

procedure T[!ServerClassName].NotifyAll(Obj: System.Object);
var
  Enumerator: IDictionaryEnumerator;
  Observer: I[!ObserverInterfaceName];
begin
  Enumerator := FHastTable.GetEnumerator;

  while Enumerator.MoveNext do
  begin
    Observer := Enumerator.Key as IObserver;

    Observer.Notify(Obj);
  end;
end;

end.