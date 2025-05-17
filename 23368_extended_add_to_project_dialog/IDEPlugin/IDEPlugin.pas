unit IDEPlugin;

interface

uses
  Borland.Studio.ToolsAPI,
  Options,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms;

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
    MessageService: IOTAMessageService;
    ActiveProject: IOTAProject;
    MainMenuService: IOTAMainMenuService;
    MenuItemAddMany,MenuItemAddProject,MenuItemOpenFiles: IOTAMenuItem;
    ApplicationSts: TApplicationSts;
  public
    procedure Add(LoadProject: Boolean);
    procedure Open;
  strict private
    procedure ShowToolMessage(&Message: string);
    procedure IDEShutdownHandler(Sender: TObject; Args: EventArgs);
    procedure FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
    procedure ProjectClosedHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
    procedure OTAProjectSync(AProject: IOTAProject);
    procedure OnMenuExecutedAddMany(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedAddProject(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedOpenFiles(Sender: TObject; Args: EventArgs);
  public
    class procedure IDERegister; static;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIDEPlugin))]

implementation

uses
  AddNewDialog,
  BorlandProjectSettings,
  OpenDialog,
  System.Collections.Specialized,
  System.IO,
  System.Text;

{$R 'IDEPlugin\IDEPlugin.TIDEPlugin.resources' 'IDEPlugin\IDEPlugin.resx'}

const
  PluginCategory = 'Project';
	AfterToReferenceItem0 = 'ProjectAddItem';
  AddManyItem = 'ProjectAddManyItem';
  AddProjectItem = 'ProjectAddProjectItem';
  OpenFilesItem = 'ProjectOpenFilesItem';

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
  Result := 'TIDEPlugin.AddMany.TShutdownWizard';
end;

function TIDEPlugin.TShutdownWizard.get_Name: string;
begin
  Result := 'IDEPlugin AddMany ShutdownWizard';
end;

{ TIDEPlugin }

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TIDEPlugin.InitializeComponent;
begin
  Self.components := System.ComponentModel.Container.Create;
  Self.ImageList := System.Windows.Forms.ImageList.Create(Self.components);
  //
  // ImageList
  //
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
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

procedure TIDEPlugin.ShowToolMessage(&Message: string);
begin
  MessageService.AddToolMessage('',&Message,'Add Many to Project',0,0);
  MessageService.ShowMessageView(nil);
end;

constructor TIDEPlugin.Create;
begin
  inherited Create;
  InitializeComponent;
  TOptions.PreloadSerializer('OptionsSerializer.dll','Options.TOptionsSerializer');
  TApplicationSts.PreloadSerializer('ApplicationStsSerializer.dll','ApplicationSts.TApplicationStsSerializer');
  TBorlandProject.PreloadSerializer('BorlandProjectSerializer.dll','BorlandProjectSettings.TBorlandProjectSerializer');
  Options := TOptions.Load;
  ActiveProject := nil;
  MainMenuService := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
  MenuItemOpenFiles := MainMenuService.AddMenuItem(AfterToReferenceItem0,OTAMenuItemLocation.otamlAfter,
                                                   OpenFilesItem,'Open Project &Files...');
  MenuItemOpenFiles.Category := PluginCategory;
  MenuItemAddProject := MainMenuService.AddMenuItem(AfterToReferenceItem0,OTAMenuItemLocation.otamlAfter,
                                                    AddProjectItem,'Add &Project Files to Project...');
  MenuItemAddProject.Category := PluginCategory;
  MenuItemAddMany := MainMenuService.AddMenuItem(AfterToReferenceItem0,OTAMenuItemLocation.otamlAfter,
                                                 AddManyItem,'Add &Many to Project...');
  MenuItemAddMany.Category := PluginCategory;
  ShutdownWizard := TIDEPlugin.TShutdownWizard.Create;
  OTAService := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
  ModuleServices := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
  MessageService := BorlandIDE.GetService(typeof(IOTAMessageService)) as IOTAMessageService;
  Include(ShutdownWizard.IDEShutdown,IDEShutdownHandler);
  Include(OTAService.FileNotification,FileNotificationHandler);
  Include(MenuItemAddMany.Executed, OnMenuExecutedAddMany);
  Include(MenuItemAddProject.Executed, OnMenuExecutedAddProject);
  Include(MenuItemOpenFiles.Executed, OnMenuExecutedOpenFiles);
  OTAProjectSync(ModuleServices.ActiveProject);
  ApplicationSts := TApplicationSts.Load;
end;

procedure TIDEPlugin.IDEShutdownHandler(Sender: TObject; Args: EventArgs);
begin
  ApplicationSts.Store;
  MainMenuService.RemoveMenuItem(AddManyItem);
  MainMenuService.RemoveMenuItem(AddProjectItem);
  MainMenuService.RemoveMenuItem(OpenFilesItem);
  Exclude(OTAService.FileNotification,FileNotificationHandler);
  ShutdownWizard.Free;
end;

procedure TIDEPlugin.OTAProjectSync(AProject: IOTAProject);
begin
  if Assigned(ActiveProject) then begin
    Exclude(ActiveProject.Closed,ProjectClosedHandler);
    Exclude(ActiveProject.Renamed,ProjectRenamedHandler);
  end;
  ActiveProject := AProject;
  if Assigned(ActiveProject) then begin
    Include(ActiveProject.Closed,ProjectClosedHandler);
    Include(ActiveProject.Renamed,ProjectRenamedHandler);
    MenuItemAddMany.Enabled := (ActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                               (ActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) or
                               (ActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality);
  end
  else
    MenuItemAddMany.Enabled := false;
  MenuItemAddProject.Enabled := MenuItemAddMany.Enabled;
  MenuItemOpenFiles.Enabled := MenuItemAddMany.Enabled;
end;

procedure TIDEPlugin.FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);

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
    if Options.OpenFilesShortcut <> Keys.None then
      MenuItemOpenFiles.Shortcut := ConverShortCut(Options.OpenFilesShortcut);
  end
  else if (Args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged) then
    OTAProjectSync(ModuleServices.ActiveProject);
end;

procedure TIDEPlugin.ProjectClosedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(nil);
end;

procedure TIDEPlugin.ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(ModuleServices.ActiveProject);
end;

procedure TIDEPlugin.OnMenuExecutedAddMany(Sender: TObject; Args: EventArgs);
begin
  Add(false);
end;

procedure TIDEPlugin.OnMenuExecutedAddProject(Sender: TObject; Args: EventArgs);
begin
  Add(true);
end;

procedure TIDEPlugin.OnMenuExecutedOpenFiles(Sender: TObject; Args: EventArgs);
begin
  Open;
end;

procedure TIDEPlugin.Add(LoadProject: Boolean);
var
  FilePath,ChildFilePath,ProjectName: string;
  SearchPatterns: array of string;
  ResPaths: array of string;
  ImageIndex,I: Integer;
  Module: IOTAModuleInfo;
begin
  ProjectName := Path.GetFileNameWithoutExtension(ActiveProject.FileName);
  SearchPatterns := New(array [1] of string);
  if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) then begin
    ResPaths := New(array [1] of string);
    SearchPatterns[0] := '*.pas';
    ImageIndex := 0;
  end
  else if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) then begin
    ResPaths := New(array [2] of string);
    SearchPatterns[0] := '*.pas';
    ImageIndex := 0;
  end
  else if (ActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality) then begin
    ResPaths := New(array [1] of string);
    SearchPatterns[0] := '*.cs';
    ImageIndex := 1;
  end
  else begin
    ShowToolMessage('Current Personality is not supported');
    Exit;
  end;
  with TAddNewDialog.Create(ApplicationSts,ProjectName,SearchPatterns,ImageIndex,LoadProject) do begin
    if ShowDialog = System.Windows.Forms.DialogResult.OK then begin
      for FilePath in FilePaths do begin
        try
          if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) then
            ResPaths[0] := Path.ChangeExtension(FilePath,'.dfm')
          else if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) then begin
            ResPaths[0] := Path.ChangeExtension(FilePath,'.resx');
            ResPaths[1] := Path.ChangeExtension(FilePath,'.nfm');
          end
          else if (ActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality) then
            ResPaths[0] := Path.ChangeExtension(FilePath,'.resx');
          if ActiveProject.FileInProject(FilePath) then
            ShowToolMessage(System.string.Format('Project {0} already contains file {1}',[ProjectName,Path.GetFileName(FilePath)]))
          else begin
            if (Length(ResPaths) > 1) and &File.Exists(ResPaths[1]) then begin
              ActiveProject.AddFile(FilePath);
              ChildFilePath := ResPaths[1];
            end
            else if &File.Exists(ResPaths[0]) then begin
              ActiveProject.AddFile(FilePath);
              ChildFilePath := ResPaths[0];
            end
            else begin
              ActiveProject.AddFile(FilePath);
              ChildFilePath := '';
            end;
            ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(FilePath),ProjectName]));
          end;
        except
          on E: Exception do begin
            ShowToolMessage(System.string.Format('Error ({2}) on adding file {0} to project {1}',[FilePath,ProjectName,E.Message]));
            Continue;
          end;
        end;
        if OpenToo or (ChildFilePath <> '') then begin
          for I := 0 to Pred(ActiveProject.ModuleCount) do begin
            Module := ActiveProject.GetModuleInfo(I);
            if System.string.Compare(Module.FileName, FilePath, true) = 0 then begin
              Module.OpenModule.ShowFileName(FilePath);
              if ChildFilePath <> '' then begin
                try
                  ActiveProject.AddFile(ChildFilePath,FilePath);
                  ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(ChildFilePath),ProjectName]));
                except
                  on E: Exception do
                    ShowToolMessage(System.string.Format('Error ({2}) on adding file {0} to project {1}',[ChildFilePath,ProjectName,E.Message]));
                end;
              end;
              Break;
            end;
          end;
        end;
      end;
    end;
    Free;
  end;
end;

procedure TIDEPlugin.Open;
var
  ProjectName: string;
  Extensions: array of string;
  ImageIndex,I: Integer;
  Modules: IOTAModuleInfoArray;
  Module: IOTAModuleInfo;
begin
  ProjectName := Path.GetFileNameWithoutExtension(ActiveProject.FileName);
  Extensions := New(array [1] of string);
  if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) then begin
    Extensions[0] := '.pas';
    ImageIndex := 0;
  end
  else if (ActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) then begin
    Extensions[0] := '.pas';
    ImageIndex := 0;
  end
  else if (ActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality) then begin
    Extensions[0] := '.cs';
    ImageIndex := 1;
  end
  else begin
    ShowToolMessage('Current Personality is not supported');
    Exit;
  end;
  Modules := New(IOTAModuleInfoArray,ActiveProject.ModuleCount);
  for I := 0 to Pred(ActiveProject.ModuleCount) do
    Modules[I] := ActiveProject.GetModuleInfo(I);
  with TOpenDialog.Create(ApplicationSts,ProjectName,Extensions,Modules,ImageIndex) do begin
    if ShowDialog = System.Windows.Forms.DialogResult.OK then begin
      for Module in SelectedModules do begin
        try
          Module.OpenModule.ShowFileName(Module.FileName);
        except
          ShowToolMessage(System.string.Format('Error on opening file {0} of project {1}',[Module.Name,ProjectName]));
        end;
      end;
    end;
    Free;
  end;
end;

end.
