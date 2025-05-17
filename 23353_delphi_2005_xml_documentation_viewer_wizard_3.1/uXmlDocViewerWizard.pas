{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uXmlDocViewerWizard;

interface

uses
  uBaseOTAMenuWizard, Borland.Studio.ToolsAPI, uWinFormXmlXsls;

type
  TXmlDocViewerWizard = class(TBaseOTAMenuWizard)
  private
    procedure FileEvent(Sender: TObject; args: FileNotificationEventArgs);
    procedure AfterCompileEvent(Sender: TObject; args: AfterCompileArgs);
    procedure RefreshViewer;
  private
    FXmlFileName: string;
    FXmlDocViewer: TWinFormXmlXsls;
    procedure DisplayProjXmlDoc(const aProject: IOTAProject);
    function get_XmlDocViewer: TWinFormXmlXsls;
    property XmlDocViewer: TWinFormXmlXsls read get_XmlDocViewer;
  public
    class procedure IDERegister; static;

    constructor Create(const aName, aIDString: string; aMenuText: string;
      aChecked, aEnabled: boolean);

    procedure Destroyed; override;
    procedure Execute; override;
  end;

implementation

uses
  System.IO;

{ TXmlDocViewerWizard }

class procedure TXmlDocViewerWizard.IDERegister;
begin
  TXmlDocViewerWizard.Create('pgXmlDocViewer30', 'pgXmlDocViewer30',
    'XML Documentation', False, True);
end;

constructor TXmlDocViewerWizard.Create(const aName, aIDString: string;
  aMenuText: string; aChecked, aEnabled: boolean);
begin
  inherited Create(aName, aIDString, aMenuText, aChecked, aEnabled);
  GetOTAWizardService.AddWizard(self);
  Include(GetOTAService.FileNotification, self.FileEvent);
  Include(GetOTAService.AfterCompile, self.AfterCompileEvent);

  GetOTAAboutBoxService.AddPluginInfo(
    'XML Documentation Viewer Wizard',
    'XML Documentation Viewer Wizard - Version 3.1',
    IntPtr.Create(0), false, '', '');

end;

function TXmlDocViewerWizard.get_XmlDocViewer: TWinFormXmlXsls;
begin
  if FXmlDocViewer = nil
    then FXmlDocViewer := TWinFormXmlXsls.Create;

  Result := FXmlDocViewer;
end;

procedure TXmlDocViewerWizard.Execute;
begin
  RefreshViewer;
end;

procedure TXmlDocViewerWizard.Destroyed;
begin
  Exclude(GetOTAService.FileNotification, FileEvent);
  Exclude(GetOTAService.AfterCompile, AfterCompileEvent);
end;

procedure TXmlDocViewerWizard.FileEvent(sender: TObject;
  args: FileNotificationEventArgs);
var aProjectGroup: IOTAProjectGroup; aProject: IOTAProject;
begin
  if (args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged) then
  begin
    aProjectGroup := GetOTAModuleServices.MainProjectGroup;
    if aProjectGroup <> nil then aProject := aProjectGroup.ActiveProject;
    DisplayProjXmlDoc(aProject);
  end;
end;

procedure TXmlDocViewerWizard.RefreshViewer;
begin
  with XmlDocViewer do
  begin
    Do_Refresh;
    Show;
    BringToFront;
  end;
end;

procedure TXmlDocViewerWizard.AfterCompileEvent(Sender: TObject;
  args: AfterCompileArgs);
begin
  if args.Succeeded then DisplayProjXmlDoc(args.Project);
end;

procedure TXmlDocViewerWizard.DisplayProjXmlDoc(const aProject: IOTAProject);
var s: string;
begin
  if aProject <> nil then
  begin
    s := Path.ChangeExtension(aProject.FileName, 'xml');
    if &File.Exists(s)
    then FXmlFileName := s
    else FXmlFileName := '';
  end;

  XmlDocViewer.XmlFilename := FXmlFileName;
end;

end.
