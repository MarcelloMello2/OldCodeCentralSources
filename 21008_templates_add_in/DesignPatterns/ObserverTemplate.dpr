program [!ProgramName];

// Created with USysWare Templates add-in ([!TimeStamp])

{$APPTYPE CONSOLE}

type
  TMyObserverServer = class(T[!ServerClassName])
  public
    procedure RegisterObserver(Obj: I[!ObserverInterfaceName]); override;
    procedure UnregisterObserver(Obj: I[!ObserverInterfaceName]); override;
    procedure NotifyAll(Obj: System.Object); override;
  end;

  TMyObserverClient = class(T[!ClientClassName])
  private
    FClientName: string;
  public
    constructor Create(const ClientName: string);

    procedure Notify(Obj: System.Object); override;
  end;

var
  ObserverServer: TMyObserverServer;
  ObserverClient1, ObserverClient2: TMyObserverClient;

{ TMyObserverServer }

procedure TMyObserverServer.RegisterObserver(Obj: I[!ObserverInterfaceName]);
begin
  Console.WriteLine('Register an observer');
  Console.WriteLine;

  inherited;
end;

procedure TMyObserverServer.UnregisterObserver(Obj: I[!ObserverInterfaceName]);
begin
  Console.WriteLine('Unregister an observer');
  Console.WriteLine;

  inherited;
end;

procedure TMyObserverServer.NotifyAll(Obj: System.Object);
begin
  Console.WriteLine('Notify all clients...');

  inherited;

  Console.WriteLine;
end;

{ TMyObserverClient }

constructor TMyObserverClient.Create(const ClientName: string);
begin
  inherited Create;

  FClientName := ClientName;
end;

procedure TMyObserverClient.Notify(Obj: System.Object);
begin
  Console.WriteLine(FClientName + ' received a notification');

  inherited;
end;

begin
  ObserverServer := TMyObserverServer.Create;
  ObserverClient1 := TMyObserverClient.Create('Client1');
  ObserverClient2 := TMyObserverClient.Create('Client2');

  ObserverServer.RegisterObserver(ObserverClient1);
  ObserverServer.RegisterObserver(ObserverClient2);

  ObserverServer.NotifyAll(nil);

  ObserverServer.UnregisterObserver(ObserverClient1);

  ObserverServer.NotifyAll(nil);

  Console.WriteLine('Press Enter to end...');
  Console.ReadLine;
end.