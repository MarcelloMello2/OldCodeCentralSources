unit IDEPlugin;

interface

uses
  Borland.Studio.ToolsAPI,
  Options,
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
    ModuleServices: IOTAModuleServices;
    IdleNotifier: IOTAIdleNotifier;
    TrayArea: ITrayArea;
    TrayAreaPanel: Integer;
    Idled: Boolean;
    MainMenuService: IOTAMainMenuService;
    MainMenuItem,MenuItemDynamicEnabled,MenuItemCheck: IOTAMenuItem;
  strict private
    procedure TrayAreaUpdate(EION: Boolean);
    procedure IDEShutdownHandler(Sender: TObject; Args: EventArgs);
    procedure IdleHandler(Sender: TObject; Args: EventArgs);
    procedure FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
    procedure AfterCompileHandler(Sender: TObject; Args: AfterCompileArgs);
    procedure OnMenuExecutedDynamicEnabled(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedCheck(Sender: TObject; Args: EventArgs);
  public
    class procedure IDERegister; static;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIDEPlugin))]

implementation

uses
  Commons.Utils.InteropServices,
  System.Diagnostics,
  System.IO,
  System.Runtime.InteropServices,
  System.Text;

const
  PluginCategory = 'Project';
	BeforeToReferenceItem = 'ProjectCompileItem';
  DEInsightItem = 'DEInsightItem';
  DEInsightDynamicEnableItem = 'DEInsightDynamicEnableItem';
  DEInsightCheckItem = 'DEInsightCheckItem';
  ErrorInsightOptionName = 'EditorOptionsErrorInsight';

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
  Result := 'TIDEPlugin.DEInsight.TShutdownWizard';
end;

function TIDEPlugin.TShutdownWizard.get_Name: string;
begin
  Result := 'IDEPlugin DEInsight ShutdownWizard';
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
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('ImageList.ImageStream')));
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
var
  OTAOptions: IOTAEnvironmentOptions;
begin
  inherited Create;
  InitializeComponent;
  Options := TOptions.Load;
  MainMenuService := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
  MainMenuItem := MainMenuService.AddMenuItem(BeforeToReferenceItem,OTAMenuItemLocation.otamlBefore,DEInsightItem,'&Dynamic Error Insight');
  MainMenuItem.Category := PluginCategory;
  MenuItemDynamicEnabled := MainMenuService.AddMenuItem(DEInsightItem,OTAMenuItemLocation.otamlChild,DEInsightDynamicEnableItem,'&Dynamic Mode Enable');
  MenuItemDynamicEnabled.Category := PluginCategory;
  MenuItemDynamicEnabled.Checked := Options.DynamicEnabledDefault;
  MenuItemCheck := MainMenuService.AddMenuItem(DEInsightItem,OTAMenuItemLocation.otamlChild,DEInsightCheckItem,'&Check',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  MenuItemCheck.Category := PluginCategory;
  ShutdownWizard := TIDEPlugin.TShutdownWizard.Create;
  OTAService := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
  OTAOptions := OTAService.EnvironmentOptions;
  MenuItemCheck.Checked := Boolean(OTAOptions.GetOptionValue(ErrorInsightOptionName));
  ModuleServices := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
  IdleNotifier := BorlandIDE.GetService(typeof(IOTAIdleNotifier)) as IOTAIdleNotifier;
  Include(ShutdownWizard.IDEShutdown,IDEShutdownHandler);
  Include(IdleNotifier.Idle,IdleHandler);
  Include(OTAService.FileNotification,FileNotificationHandler);
  Include(OTAService.AfterCompile,AfterCompileHandler);
  Include(MenuItemDynamicEnabled.Executed, OnMenuExecutedDynamicEnabled);
  Include(MenuItemCheck.Executed, OnMenuExecutedCheck);
  TrayAreaPanel := -1;
  Idled := false;
end;

procedure TIDEPlugin.IDEShutdownHandler(Sender: TObject; Args: EventArgs);
begin
  TrayArea.RemovePanel(TrayAreaPanel);
  Marshal.ReleaseComObject(TrayArea);
  MainMenuService.RemoveMenuItem(DEInsightCheckItem);
  MainMenuService.RemoveMenuItem(DEInsightDynamicEnableItem);
  MainMenuService.RemoveMenuItem(DEInsightItem);
  Exclude(OTAService.AfterCompile,AfterCompileHandler);
  Exclude(OTAService.FileNotification,FileNotificationHandler);
  Exclude(IdleNotifier.Idle,IdleHandler);
  ShutdownWizard.Free;
end;

procedure TIDEPlugin.IdleHandler(Sender: TObject; Args: EventArgs);
begin
  if not Idled then begin
    Exclude(IdleNotifier.Idle,IdleHandler);
    Idled := true;
    TrayAreaUpdate(MenuItemCheck.Checked);
  end;
end;

procedure TIDEPlugin.FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
var
  OTAOptions: IOTAEnvironmentOptions;

  function ConverShortCut(ShortCut: Keys): Integer;
  const
    KeyShift = $2000;
    KeyControl = $4000;
    KeyAlt = $8000;
  begin
    Result := 0;
    if (Keys.Control and ShortCut) <> Keys.None then
      Result := Result or KeyControl;
    if (Keys.Shift and ShortCut) <> Keys.None then
      Result := Result or KeyShift;
    if (Keys.Alt and ShortCut) <> Keys.None then
      Result := Result or KeyAlt;
    ShortCut := ShortCut and Keys.KeyCode;
    Result := Result or Convert.ToInt32(ShortCut);
  end;

begin
  if ((Args.NotifyCode = OTAFileNotification.ofnPackageInstalled) or
			(Args.NotifyCode = OTAFileNotification.ofnPackageUninstalled)  or
			(Args.NotifyCode = OTAFileNotification.ofnFileOpened)) then begin
    if Options.ItemCheckShortcut <> Keys.None then
      MenuItemCheck.Shortcut := ConverShortCut(Options.ItemCheckShortcut);
  end
  else if Args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged then begin
    if MenuItemDynamicEnabled.Checked and MenuItemCheck.Checked then begin
      MenuItemCheck.Checked := false;
      OTAOptions := OTAService.EnvironmentOptions;
      OTAOptions.SetOptionValue(ErrorInsightOptionName,TObject(false));
      TrayAreaUpdate(MenuItemCheck.Checked);
    end;
  end;
end;

procedure TIDEPlugin.AfterCompileHandler(Sender: TObject; Args: AfterCompileArgs);
var
  OTAOptions: IOTAEnvironmentOptions;
begin
  try
    if MenuItemDynamicEnabled.Checked then begin
      if MenuItemCheck.Checked and Args.Succeeded then begin
        MenuItemCheck.Checked := false;
        OTAOptions := OTAService.EnvironmentOptions;
        OTAOptions.SetOptionValue(ErrorInsightOptionName,TObject(false));
      end
      else if not MenuItemCheck.Checked and not Args.IsCodeInsight and not Args.Succeeded then begin
        MenuItemCheck.Checked := true;
        OTAOptions := OTAService.EnvironmentOptions;
        OTAOptions.SetOptionValue(ErrorInsightOptionName,TObject(true));
      end;
      TrayAreaUpdate(MenuItemCheck.Checked);
    end;
  except
    ;
  end;
end;

procedure TIDEPlugin.TrayAreaUpdate(EION: Boolean);
begin
  if (TrayAreaPanel < 0) and Idled and Options.TrayAreaInjector then try
    TrayArea := TrayAreaClass.Create;
    TrayArea.AttachTrayArea(Process.GetCurrentProcess.Id,'TAppBuilder');
    TrayArea.AddPanel('Delphi 2005','Delphi 2005',0,0);
    TrayAreaPanel := TrayArea.AddPanel('DEInsigh','Error Insight: off',-1,0);
  except
    TrayAreaPanel := -1;
  end;
  if (TrayAreaPanel >= 0) and Idled and Options.TrayAreaInjector then begin
    if EION then begin
      TrayArea.PanelText[TrayAreaPanel] := 'Error Insight: on';
      TrayArea.PanelImageIndex[TrayAreaPanel] := 9;
    end
    else begin
      TrayArea.PanelText[TrayAreaPanel] := 'Error Insight: off';
      TrayArea.PanelImageIndex[TrayAreaPanel] := -1;
    end;
  end;
end;

procedure TIDEPlugin.OnMenuExecutedCheck(Sender: TObject; Args: EventArgs);
var
  OTAOptions: IOTAEnvironmentOptions;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  I: Integer;
begin
  MenuItemCheck.Checked := not MenuItemCheck.Checked;
  OTAOptions := OTAService.EnvironmentOptions;
  OTAOptions.SetOptionValue(ErrorInsightOptionName,TObject(MenuItemCheck.Checked));
  TrayAreaUpdate(MenuItemCheck.Checked);
  Module := ModuleServices.CurrentModule;
  if Assigned(Module) then begin
    for I := 0 to Pred(Module.ModuleFileCount) do begin
      Editor := Module.ModuleFileEditors(I);
      if Editor is IOTASourceEditor then begin
        SourceEditor := IOTASourceEditor(Editor);
        (SourceEditor as IOTAEditor).MarkModified;
        Break;
      end;
    end;
  end;
end;

procedure TIDEPlugin.OnMenuExecutedDynamicEnabled(Sender: TObject; Args: EventArgs);
begin
  MenuItemDynamicEnabled.Checked := not MenuItemDynamicEnabled.Checked;
end;


end.
