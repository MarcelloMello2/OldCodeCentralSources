unit [!UnitName];

// Created with USysWare Templates add-in

interface

uses
  System.ServiceProcess;

type
  T[!ClassName] = class(System.ServiceProcess.ServiceBase)
  strict protected
    procedure OnStart(args: array of string); override;
    procedure OnPause; override;
    procedure OnStop; override;
    procedure OnContinue; override;
    procedure OnShutdown; override;
  public
    constructor Create;
  end;

implementation

uses
  [!DependantUnitName];

{ T[!ClassName] }

constructor T[!ClassName].Create;
begin
  inherited Create;

  ServiceName := DefaultServiceName;

  CanHandlePowerEvent := False;
  CanPauseAndContinue := True;
  CanShutdown := True;
  CanStop := True;

  EventLog.Source := DefaultServiceName;
  EventLog.Log := ''; //ToDo: set your custom log or leave it blank for defaults

  AutoLog := True;
end;

procedure T[!ClassName].OnStart(args: array of string);
begin
  inherited;

  //ToDo: add your code here
end;

procedure T[!ClassName].OnPause;
begin
  inherited;

  //ToDo: add your code here
end;

procedure T[!ClassName].OnStop;
begin
  inherited;

  //ToDo: add your code here
end;

procedure T[!ClassName].OnContinue;
begin
  inherited;

  //ToDo: add your code here
end;

procedure T[!ClassName].OnShutdown;
begin
  inherited;

  //ToDo: add your code here
end;

end.