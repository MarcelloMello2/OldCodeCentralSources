unit [!UnitName];

// Created with USysWare Templates add-in ([!TimeStamp])

interface

type
  I[!ObserverInterfaceName] = interface
    procedure Notify(Obj: System.Object);
  end;

  I[!NotifierInterfaceName] = interface
    procedure RegisterObserver(Obj: I[!ObserverInterfaceName]);
    procedure UnregisterObserver(Obj: I[!ObserverInterfaceName]);
    procedure NotifyAll(Obj: System.Object);
  end;

implementation

end.