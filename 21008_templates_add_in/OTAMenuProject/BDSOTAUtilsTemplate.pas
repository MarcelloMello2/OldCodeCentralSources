unit [!UnitName];

// Created with USysWare Templates add-in

interface

uses
  Borland.Studio.ToolsAPI;

function GetAboutBoxService: IOTAAboutBoxService;
function GetSplashScreenService: IOTASplashScreenService;
function GetMainMenuService: IOTAMainMenuService;

implementation

function GetAboutBoxService: IOTAAboutBoxService;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAAboutBoxService)) as
    IOTAAboutBoxService);
end;

function GetSplashScreenService: IOTASplashScreenService;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTASplashScreenService)) as
    IOTASplashScreenService);
end;

function GetMainMenuService: IOTAMainMenuService;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAMainMenuService)) as
    IOTAMainMenuService);
end;

end.