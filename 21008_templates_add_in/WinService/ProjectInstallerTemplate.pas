unit [!UnitName];

// Created with USysWare Templates add-in

interface

uses
  System.ServiceProcess,
  System.Configuration.Install,
  System.ComponentModel,
  System.Collections,
  Microsoft.Win32;

const
  DefaultServiceName = '[!ServiceName]';
  DefaultServiceDisplayName = '[!ServiceDisplayName]';
  DefaultServiceDescription = '[!ServiceDescription]';

type
  [RunInstaller(True)]
  T[!ClassName] = class(System.Configuration.Install.Installer)
  private
    FServicesDependedOn: array of string;
    FServiceProcessInstaller: System.ServiceProcess.ServiceProcessInstaller;
    FServiceInstaller: System.ServiceProcess.ServiceInstaller;

    procedure AddServiceDescription(ServiceDesc: string);
  strict protected
    procedure OnBeforeInstall(savedState: IDictionary); override;
    procedure OnAfterInstall(savedState: IDictionary); override;
    procedure OnBeforeUninstall(savedState: IDictionary); override;
    procedure OnAfterUninstall(savedState: IDictionary); override;
  public
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(T[!ClassName]))]

implementation

{ T[!ClassName] }

constructor T[!ClassName].Create;
begin
  inherited Create;

  //ToDo: add more service dependencies here
  SetLength(FServicesDependedOn, 1);
  FServicesDependedOn[0] := 'Event Log';

  FServiceProcessInstaller :=
    System.ServiceProcess.ServiceProcessInstaller.Create;
  FServiceProcessInstaller.Account :=
    System.ServiceProcess.ServiceAccount.LocalSystem;
  FServiceProcessInstaller.Password := '';
  FServiceProcessInstaller.Username := '';

  FServiceInstaller := System.ServiceProcess.ServiceInstaller.Create;
  FServiceInstaller.ServiceName := DefaultServiceName;
  FServiceInstaller.DisplayName := DefaultServiceDisplayName;
  //ToDo: adjust service type to Manual if necessary
  FServiceInstaller.StartType :=
    System.ServiceProcess.ServiceStartMode.Automatic;
  FServiceInstaller.ServicesDependedOn := FServicesDependedOn;

  Installers.Add(FServiceProcessInstaller);
  Installers.Add(FServiceInstaller);
end;

// Note that Description will be automatically deleted along with the
// service registry key when the service's uninstalled
procedure T[!ClassName].AddServiceDescription(ServiceDesc: string);
var
  ServiceKey: RegistryKey;
begin
  try
    ServiceKey := Microsoft.Win32.Registry.LocalMachine.OpenSubKey(
      'SYSTEM\CurrentControlSet\Services\' + DefaultServiceName, True);
  except
    ServiceKey := nil;
  end;

  if (ServiceKey <> nil) then
  begin
    try
      ServiceKey.SetValue('Description', ServiceDesc);
    except
    end;

    ServiceKey.Close;
  end;
end;

procedure T[!ClassName].OnBeforeInstall(savedState: IDictionary);
begin
  //ToDo: add your code here

  inherited;
end;

procedure T[!ClassName].OnAfterInstall(savedState: IDictionary);
begin
  inherited;

  AddServiceDescription(DefaultServiceDescription);
  
  //ToDo: add your code here
end;

procedure T[!ClassName].OnBeforeUninstall(savedState: IDictionary);
begin
  //ToDo: add your code here

  inherited;
end;

procedure T[!ClassName].OnAfterUninstall(savedState: IDictionary);
begin
  inherited;

  //ToDo: add your code here
end;

end.