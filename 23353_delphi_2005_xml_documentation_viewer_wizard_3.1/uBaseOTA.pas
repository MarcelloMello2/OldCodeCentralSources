{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uBaseOTA;

interface

uses
  Borland.Studio.ToolsAPI;

type
  TBaseOTA = class
  public
    class function GetOTAService: IOTAService; static;
    class function GetOTAWizardService: IOTAWizardService; static;
    class function GetOTAModuleServices: IOTAModuleServices; static;
    class function GetOTAAboutBoxService: IOTAAboutBoxService; static;
  end;

implementation

{ TBaseOTA }

class function TBaseOTA.GetOTAWizardService: IOTAWizardService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAWizardService)) as IOTAWizardService;
end;

class function TBaseOTA.GetOTAService: IOTAService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
end;

class function TBaseOTA.GetOTAModuleServices: IOTAModuleServices;
begin
  Result := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
end;

class function TBaseOTA.GetOTAAboutBoxService: IOTAAboutBoxService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAAboutBoxService)) as IOTAAboutBoxService;
end;

end.
