unit BDSOTAUtils;
//------------------------------------------------------------------------------
//  File name:      BDSOTAUtils.pas
//  Last updated:   11/24/03
//  Author:         Sergey Mishkovskiy
//  Company:        USysWare, Inc.
//  Contact info:   usysware@comcast.net
//
//  Compatibility:  Borland Delphi for .NET
//
//  Description:    BDS OTA helper functions.
//------------------------------------------------------------------------------

interface

uses
  Borland.Studio.ToolsAPI;

function GetProjectPath: string;

function GetAboutBoxService: IOTAAboutBoxService;
function GetSplashScreenService: IOTASplashScreenService;
function GetWizardService: IOTAWizardService;
function GetModuleServices: IOTAModuleServices;
function GetGalleryCategoryManager: IOTAGalleryCategoryManager;
function GetDotNetProject(IProject: IOTAProject): IOTADotNetProject;
function GetService: IOTAService;
function GetActiveProject: IOTAProject;

implementation

uses
  System.IO,  // Path
  BDSConfig;

function GetProjectPath: string;
var
  IModuleServices: IOTAModuleServices;
  Index: Integer;
begin
  Result := '';
  IModuleServices := GetModuleServices;

  if IModuleServices <> nil then
    for Index := 0 to IModuleServices.ModuleCount - 1 do
      if IModuleServices.GetModule(Index) is IOTAProjectGroup then
      begin
        Result := AddBackSlash(Path.GetDirectoryName(
          IModuleServices.GetModule(Index).FileName));

        Break;
      end;
end;

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

function GetWizardService: IOTAWizardService;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAWizardService)) as
    IOTAWizardService);
end;

function GetModuleServices: IOTAModuleServices;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAModuleServices)) as
    IOTAModuleServices);
end;

function GetGalleryCategoryManager: IOTAGalleryCategoryManager;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAGalleryCategoryManager)) as
    IOTAGalleryCategoryManager);
end;

function GetDotNetProject(IProject: IOTAProject): IOTADotNetProject;
begin
  if IProject = nil then
    Result := nil
  else
    Result := (IProject.GetService(TypeOf(IOTADotNetProject)) as
      IOTADotNetProject);
end;

function GetService: IOTAService;
begin
  Result := (BorlandIDE.GetService(TypeOf(IOTAService)) as IOTAService);
end;

function GetActiveProject: IOTAProject;
var
  IModuleServices: IOTAModuleServices;
  IProjectGroup: IOTAProjectGroup;
begin
  Result := nil;

  IModuleServices := GetModuleServices;
  if IModuleServices = nil then
    Exit;

  IProjectGroup := IModuleServices.MainProjectGroup;
  if IProjectGroup = nil then
    Exit;

  Result := IProjectGroup.ActiveProject;
end;

end.
