unit ProjectDesktop;

interface

uses
  Classes, SysUtils, ToolsAPI;

type
  TProjectDesktopNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    procedure LoadProjectDesktop(const FileName: string);
    procedure SaveProjectDesktop(const FileName: string);
  protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  end;

procedure Register;

implementation

uses
  Windows, IniFiles;

const
  SIniActiveProjectSection = 'ActiveProject';
  SIniActiveProjectIdent = 'ActiveProject';
  SDesktopExt = '.dsk';
  SDesktopTempExt = '.$$$';

var
  NotifierIndex: Integer = -1;

function FindModuleInterface(AInterface: TGUID): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as IOTAModuleServices do
    for I := 0 to ModuleCount - 1 do
      if (Modules[I].QueryInterface(AInterface, Result) = S_OK) then
         Break;
end;

{ TProjectDesktopNotifier private }

procedure TProjectDesktopNotifier.LoadProjectDesktop(const FileName: string);
var
  ActiveProjectIndex: Integer;
  ProjectGroup: IOTAProjectGroup;
  ProjectGroupModule: IOTAModule;
  ProjectGroupFileName: string;
begin
  ProjectGroup := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
  if not Assigned(ProjectGroup) then
    Exit;

  ProjectGroupFileName := '';
  ProjectGroupModule := ProjectGroup as IOTAModule;
  if Assigned(ProjectGroupModule) then
    ProjectGroupFileName := ProjectGroupModule.FileName;

  if AnsiCompareText(FileName, ChangeFileExt(ProjectGroupFileName,
    SDesktopExt)) = 0 then
  begin
    with TIniFile.Create(FileName) do
      try
        ActiveProjectIndex := ReadInteger(SIniActiveProjectSection,
          SIniActiveProjectIdent, -1);
      finally
        Free;
      end;
      
    with ProjectGroup do
      if (ActiveProjectIndex >= 0) and (ActiveProjectIndex < ProjectCount) then
        ActiveProject := Projects[ActiveProjectIndex];
  end;
end;

procedure TProjectDesktopNotifier.SaveProjectDesktop(const FileName: string);
var
  ProjectGroup: IOTAProjectGroup;
  ProjectGroupModule: IOTAModule;
  ProjectGroupFileName: string;
  ActiveProjectIndex: Integer;
  I: Integer;
begin
  ProjectGroup := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
  if not Assigned(ProjectGroup) then
    Exit;

  ActiveProjectIndex := -1;
  with ProjectGroup do
    for I := 0 to ProjectCount - 1 do
      if Projects[I] = ActiveProject then
      begin
        ActiveProjectIndex := I;
        Break;
      end;

  ProjectGroupFileName := '';
  ProjectGroupModule := ProjectGroup as IOTAModule;
  if Assigned(ProjectGroupModule) then
    ProjectGroupFileName := ProjectGroupModule.FileName;

  if AnsiCompareText(FileName, ChangeFileExt(ProjectGroupFileName,
    SDesktopTempExt)) = 0 then
  begin
    with TIniFile.Create(FileName) do
      try
        WriteInteger(SIniActiveProjectSection, SIniActiveProjectIdent,
          ActiveProjectIndex);
      finally
        Free;
      end;
  end;
end;

{ TProjectDesktopNotifier protected }

procedure TProjectDesktopNotifier.AfterCompile(Succeeded: Boolean);
begin
  // do nothing
end;

procedure TProjectDesktopNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  // do nothing
end;

procedure TProjectDesktopNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  case NotifyCode of
    ofnProjectDesktopLoad:
      LoadProjectDesktop(FileName);
    ofnProjectDesktopSave:
      SaveProjectDesktop(FileName);
  end;
end;

procedure Register;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
    NotifierIndex := Services.AddNotifier(TProjectDesktopNotifier.Create);
end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
    Services.RemoveNotifier(NotifierIndex);
end;

initialization

finalization
  RemoveNotifier;

end.
