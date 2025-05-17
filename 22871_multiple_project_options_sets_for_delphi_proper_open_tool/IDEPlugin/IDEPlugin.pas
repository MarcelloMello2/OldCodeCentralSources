unit IDEPlugin;

interface

uses
  Borland.Studio.ToolsAPI,
  Commons.Settings,
  Options,
  SetsDialog,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms,
  TrayAreaInjector;

type
  TIDEPlugin = class(System.ComponentModel.Component)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    ImageList: System.Windows.Forms.ImageList;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private type
    TShutdownWizard = class(System.Object, IOTAWizard)
    strict private
      WizardIndex: Integer;
      Service: IOTAWizardService;
      FIDEShutdown: EventHandler;
    public
      function get_IDString: string;
      function get_Name: string;
    public
      property IDString: string read get_IDString;
      property Name: string read get_Name;
      property IDEShutdown: EventHandler add FIDEShutdown remove FIDEShutdown;
      procedure Execute;
      procedure Destroyed;
      constructor Create;
      destructor Destroy; override;
    end;
  strict private
    Options: TOptions;
    ShutdownWizard: TShutdownWizard;
    OTAService: IOTAService;
    IdleNotifier: IOTAIdleNotifier;
    ModuleServices: IOTAModuleServices;
    MessageService: IOTAMessageService;
    Project: IOTAProject;
    TrayArea: ITrayArea;
    TrayAreaPersonalityPanel,TrayAreaPanel: Integer;
    Idled: Boolean;
    CurrentProjectPath: string;
    MainMenuService: IOTAMainMenuService;
    MenuItemOptionsSets: IOTAMenuItem;
  strict private
    procedure SetsDialogMessageHandler(Sender: TObject; Args: TSetsDialogMessageEventArgs);
    procedure OTAProjectSync(AProject: IOTAProject; SetsDialog: TSetsDialog);
    procedure TrayAreaUpdate(SetName: string; Personality: string);
    procedure IDEShutdownHandler(Sender: TObject; Args: EventArgs);
    procedure IdleHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectClosedHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectBeforeSaveHandler(Sender: TObject; Args: EventArgs);
    procedure FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
    procedure OnMenuExecutedOptionsSets(Sender: TObject; Args: EventArgs);
  public
    class procedure IDERegister; static;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIDEPlugin))]

implementation

uses
  Commons.Utils.InteropServices,
  ProjectOptionsSets,
  System.Diagnostics,
  System.Runtime.InteropServices;

{$R 'IDEPlugin\IDEPlugin.TIDEPlugin.resources' 'IDEPlugin\IDEPlugin.resx'}

const
  PluginCategory = 'Project';
	AfterToReferenceItem0 = 'ProjectOptionsItem';
	AfterToReferenceItem1 = 'ProjectDependenciesItem';
  OptionsSetsItem = 'ProjectOptionsSetsItem';

{ TIDEPlugin.TShutdownWizard }

constructor TIDEPlugin.TShutdownWizard.Create;
begin
  inherited Create;
  Service := BorlandIDE.GetService(TypeOf(IOTAWizardService)) as IOTAWizardService;
  WizardIndex := Service.AddWizard(Self);
end;

destructor TIDEPlugin.TShutdownWizard.Destroy;
begin
  Service.RemoveWizard(WizardIndex);
  inherited Destroy;
end;

procedure TIDEPlugin.TShutdownWizard.Destroyed;
begin
  if Assigned(FIDEShutdown) then
    FIDEShutdown(Self,EventArgs.Create);
end;

procedure TIDEPlugin.TShutdownWizard.Execute;
begin
end;

function TIDEPlugin.TShutdownWizard.get_IDString: string;
begin
  Result := 'TIDEPlugin.OptionsSets.TShutdownWizard';
end;

function TIDEPlugin.TShutdownWizard.get_Name: string;
begin
  Result := 'IDEPlugin OptionsSets ShutdownWizard';
end;

{ TIDEPlugin }

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TIDEPlugin.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TIDEPlugin));
  Self.ImageList := System.Windows.Forms.ImageList.Create(Self.components);
  //
  // ImageList
  //
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('I' +
    'mageList.ImageStream')));
  Self.ImageList.TransparentColor := System.Drawing.Color.Transparent;
end;
{$ENDREGION}

class procedure TIDEPlugin.IDERegister;
begin
  TIDEPlugin.Create;
end;

procedure TIDEPlugin.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TIDEPlugin.Create;
begin
  inherited Create;
  InitializeComponent;
  TOptions.PreloadSerializer('OptionsSerializer.dll','Options.TOptionsSerializer');
  TProjectOptionsSets.PreloadSerializer('ProjectOptionsSerializer.dll','ProjectOptions.TProjectOptionsSetsSerializer');
  Options := TOptions.Load;
  MainMenuService := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
  MenuItemOptionsSets := MainMenuService.AddMenuItem(AfterToReferenceItem0,OTAMenuItemLocation.otamlAfter,
                                                     OptionsSetsItem,'&Options Sets...[]',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  if not Assigned(MenuItemOptionsSets) then
    MenuItemOptionsSets := MainMenuService.AddMenuItem(AfterToReferenceItem1,OTAMenuItemLocation.otamlAfter,
                                                       OptionsSetsItem,'&Options Sets...[]',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  MenuItemOptionsSets.Category := PluginCategory;
  ShutdownWizard := TIDEPlugin.TShutdownWizard.Create;
  OTAService := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
  IdleNotifier := BorlandIDE.GetService(typeof(IOTAIdleNotifier)) as IOTAIdleNotifier;
  ModuleServices := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
  MessageService := BorlandIDE.GetService(typeof(IOTAMessageService)) as IOTAMessageService;
  Include(ShutdownWizard.IDEShutdown,IDEShutdownHandler);
  Include(IdleNotifier.Idle,IdleHandler);
  Include(OTAService.FileNotification,FileNotificationHandler);
  Include(MenuItemOptionsSets.Executed, OnMenuExecutedOptionsSets);
  OTAProjectSync(ModuleServices.ActiveProject,nil);
  TrayAreaPersonalityPanel := -1;
  TrayAreaPanel := -1;
  Idled := false;
end;

procedure TIDEPlugin.IDEShutdownHandler(Sender: TObject; Args: EventArgs);
begin
  TrayArea.RemovePanel(TrayAreaPanel);
  Marshal.ReleaseComObject(TrayArea);
  MainMenuService.RemoveMenuItem(OptionsSetsItem);
  Exclude(OTAService.FileNotification,FileNotificationHandler);
  Exclude(IdleNotifier.Idle,IdleHandler);
  ShutdownWizard.Free;
end;

procedure TIDEPlugin.IdleHandler(Sender: TObject; Args: EventArgs);
var
  S: array of string;
begin
  if not Idled then begin
    Exclude(IdleNotifier.Idle,IdleHandler);
    Idled := true;
    S := MenuItemOptionsSets.Text.Split(['[',']']);
    S[0] := '';
    if Length(S) > 1 then
      S[0] := S[1];
    if Assigned(Project) then
      TrayAreaUpdate(S[0],Project.Personality)
    else
      TrayAreaUpdate(S[0],'');
  end;
end;

procedure TIDEPlugin.OTAProjectSync(AProject: IOTAProject; SetsDialog: TSetsDialog);
begin
  if Assigned(Project) and Assigned(AProject) then
    if System.string.Compare(Project.FileName,AProject.FileName,true) = 0 then
      Exit;
  if Assigned(Project) then begin
    Exclude(Project.Closed,ProjectClosedHandler);
    Exclude(Project.Renamed,ProjectRenamedHandler);
    Exclude(Project.BeforeSave,ProjectBeforeSaveHandler);
  end;
  CurrentProjectPath := '';
  MenuItemOptionsSets.Visible := false;
  Project := AProject;
  if Assigned(Project) then begin
    CurrentProjectPath := Project.FileName;
    Include(Project.Closed,ProjectClosedHandler);
    Include(Project.Renamed,ProjectRenamedHandler);
    Include(Project.BeforeSave,ProjectBeforeSaveHandler);
    MenuItemOptionsSets.Visible := true;
    MenuItemOptionsSets.Enabled := (Project.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                                   (Project.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality);
    if not MenuItemOptionsSets.Enabled then begin
      MenuItemOptionsSets.Text := '&Options Sets...[]';
      TrayAreaUpdate('',Project.Personality);
    end
    else begin
      if Assigned(SetsDialog) then begin
        MenuItemOptionsSets.Text := System.string.Format('&Options Sets...[{0}]',[SetsDialog.CurrentSetName]);
        TrayAreaUpdate(SetsDialog.CurrentSetName,Project.Personality);
      end
      else with TSetsDialog.Create(Project.FileName,Project.ProjectOptions,SetsDialogMessageHandler,Options) do begin
        MenuItemOptionsSets.Text := System.string.Format('&Options Sets...[{0}]',[CurrentSetName]);
        TrayAreaUpdate(CurrentSetName,Project.Personality);
        Free;
      end;
    end;
  end
  else begin
    MenuItemOptionsSets.Text := '&Options Sets...[]';
    TrayAreaUpdate('','');
  end;
end;

procedure TIDEPlugin.SetsDialogMessageHandler(Sender: TObject; Args: TSetsDialogMessageEventArgs);
begin
  MessageService.AddToolMessage('',Args.&Message,'Options Sets',0,0);
  MessageService.ShowMessageView(nil);
end;

procedure TIDEPlugin.TrayAreaUpdate(SetName: string; Personality: string);
begin
  if (TrayAreaPersonalityPanel < 0) and Idled and Options.TrayAreaInjector then try
    TrayArea := TrayAreaClass.Create;
    TrayArea.AttachTrayArea(Process.GetCurrentProcess.Id,'TAppBuilder');
    TrayAreaPersonalityPanel := TrayArea.AddPanel('Delphi 2005','Delphi 2005',0,0);
  except
    TrayAreaPersonalityPanel := -1;
  end;
  if (TrayAreaPersonalityPanel >= 0) and Idled and Options.TrayAreaInjector then begin
    if Personality = OTAIDEPersonalities.sDelphiDotNetPersonality then
      TrayArea.PanelImageIndex[TrayAreaPersonalityPanel] := 1
    else if Personality = OTAIDEPersonalities.sDelphiPersonality then
      TrayArea.PanelImageIndex[TrayAreaPersonalityPanel] := 2
    else if Personality = OTAIDEPersonalities.sCSharpPersonality then
      TrayArea.PanelImageIndex[TrayAreaPersonalityPanel] := 3
    else
      TrayArea.PanelImageIndex[TrayAreaPersonalityPanel] := 0;
    if (Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) or
       (Personality = OTAIDEPersonalities.sDelphiPersonality) then begin
      if TrayAreaPanel < 0 then
        TrayAreaPanel := TrayArea.AddPanel('OptionSets','Options:[Debug]',-1,0);
      TrayArea.PanelText[TrayAreaPanel] := System.string.Format('Options:[{0}]',[SetName]);
    end
    else begin
      if TrayAreaPanel >= 0 then begin
        TrayArea.RemovePanel(TrayAreaPanel);
        TrayAreaPanel := -1;
      end;
    end;
  end;
end;

procedure TIDEPlugin.FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
begin
  if (Args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged) then
    OTAProjectSync(ModuleServices.ActiveProject,nil);
end;

procedure TIDEPlugin.ProjectClosedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(nil,nil);
end;

procedure TIDEPlugin.ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
begin
  if Assigned(Project) and (CurrentProjectPath <> '') then begin
    with TSetsDialog.Create(Project.FileName,nil,SetsDialogMessageHandler,Options) do begin
      Rename(CurrentProjectPath);
      Free;
    end;
    CurrentProjectPath := Project.FileName;
  end;
end;

procedure TIDEPlugin.ProjectBeforeSaveHandler(Sender: TObject; Args: EventArgs);
begin
  if not Assigned(Project) then
    OTAProjectSync(ModuleServices.ActiveProject,nil);
  if Assigned(Project) then begin
    if Project.ProjectOptions.ModifiedState and MenuItemOptionsSets.Enabled then begin
      with TSetsDialog.Create(Project.FileName,Project.ProjectOptions,SetsDialogMessageHandler,Options) do begin
        Syncronize;
        Free;
      end;
    end;
  end;
end;

procedure TIDEPlugin.OnMenuExecutedOptionsSets(Sender: TObject; Args: EventArgs);
begin
  if not Assigned(Project) then
    OTAProjectSync(ModuleServices.ActiveProject,nil);
  if Assigned(Project) then begin
    if Project.ProjectOptions.ModifiedState then begin
      ModuleServices.SaveAll;
      Application.DoEvents;
    end;
    with TSetsDialog.Create(Project.FileName,Project.ProjectOptions,SetsDialogMessageHandler,Options) do begin
      if ShowDialog = System.Windows.Forms.DialogResult.Ok then
        ModuleServices.SaveAll;
      MenuItemOptionsSets.Text := System.string.Format('&Options Sets...[{0}]',[CurrentSetName]);
      TrayAreaUpdate(CurrentSetName,Project.Personality);
      Free;
    end;
  end
  else begin
    with TSetsDialog.Create('<No Open Project>',nil,SetsDialogMessageHandler,Options) do begin
      ShowDialog;
      Free;
    end;
  end;
end;

end.
