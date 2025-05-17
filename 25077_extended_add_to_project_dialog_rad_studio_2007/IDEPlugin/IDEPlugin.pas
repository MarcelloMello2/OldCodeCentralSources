unit IDEPlugin;

interface

uses
  AddNewDialog,
  Borland.Studio.ToolsAPI,
  Options,
  System.ComponentModel,
  System.Drawing,
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
    IdleNotifier: IOTAIdleNotifier;
    OTAService: IOTAService;
    ModuleServices: IOTAModuleServices;
    ModuleCreator: IOTAModuleCreator;
    MessageService: IOTAMessageService;
    OTAActiveProject: IOTAProject;
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
  OpenDialog,
  RADStudioProject,
  System.CodeDom,
  System.Collections.Generic,
  System.IO,
  System.Text;

const
  PluginCategory = 'Project';
	AfterToReferenceItem0 = 'ProjectAddItem';
  AddManyItem = 'ProjectAddManyItem';
  AddProjectItem = 'ProjectAddProjectItem';
  OpenFilesItem = 'ProjectOpenFilesItem';

type
  TProjectSource = class
  strict private
    Source: List<string>;
    SourceLength: Integer;
    SourceEditor: IOTASourceEditor;
  public
    procedure TweakResourceLine(ResxPath,ClassName: string; var Error: Boolean);
    procedure TweakSourceLine(FilePath,ClassName,DesignClassName: string; var Error: Boolean);
    class function Load(Project: IOTAProject): TProjectSource; static;
    procedure Store(Project: IOTAProject);
    class function ContainsResource(Project: IOTAProject; ParentFileName,ResourceFileName: string): Boolean; static;
  end;

{$region 'TProjectSource'}

class function  TProjectSource.ContainsResource(Project: IOTAProject; ParentFileName,ResourceFileName: string): Boolean;
var
  ProjectSource: TProjectSource;
  I: Integer;
  Editor: IOTAEditor;
  Reader: IOTAFileReader;
  Builder: StringBuilder;
  Buffer: array of Byte;
  S: string;
begin
  Result := false;
  ParentFileName := ParentFileName.ToLower;
  ResourceFileName := ResourceFileName.ToLower;
  ProjectSource := TProjectSource.Create;
  with ProjectSource do for I := 0 to Pred(Project.ModuleFileCount) do begin
    Editor := Project.ModuleFileEditors(I);
    if Editor is IOTASourceEditor then begin
      SourceEditor := IOTASourceEditor(Editor);
      Source := List<string>.Create;
      Builder := StringBuilder.Create;
      Reader := SourceEditor.CreateReader;
      try
        repeat
          Buffer := Reader.Read(1024,0);
          if Assigned(Buffer) and (Length(Buffer) > 0) then begin
            Builder.Append(Buffer);
            S := Builder.ToString.ToLower;
            if S.Contains(ResourceFileName) then begin
              Result := true;
              Break;
            end
            else if S.Contains(ParentFileName) then
              Break
            else if Buffer[High(Buffer)] = 0 then
              Break;
          end
          else
            Break;
        until false;
      finally
        Reader.Close;
      end;
    end;
  end;
end;

class function TProjectSource.Load(Project: IOTAProject): TProjectSource;
var
  I: Integer;
  Editor: IOTAEditor;
  Reader: IOTAFileReader;
  Builder: StringBuilder;
  Buffer: array of Byte;
  Line: string;
begin
  Result := TProjectSource.Create;
  with Result do for I := 0 to Pred(Project.ModuleFileCount) do begin
    Editor := Project.ModuleFileEditors(I);
    if Editor is IOTASourceEditor then begin
      SourceEditor := IOTASourceEditor(Editor);
      Source := List<string>.Create;
      Builder := StringBuilder.Create;
      Reader := SourceEditor.CreateReader;
      try
        repeat
          Buffer := Reader.Read(1024,0);
          if Assigned(Buffer) and (Length(Buffer) > 0) then begin
            Builder.Append(Buffer);
            if Buffer[High(Buffer)] = 0 then
              Break;
          end
          else
            Break;
        until false;
      finally
        Reader.Close;
      end;
      SourceLength := Builder.Length;
      with StringReader.Create(Builder.ToString) do repeat
        Line := ReadLine;
        if not Assigned(Line) then Break;
        Source.Add(Line);
      until false;
      Break;
    end;
  end;
end;

procedure TProjectSource.Store(Project: IOTAProject);
var
  Writer: IOTAFileWriter;
  Builder: StringBuilder;
  Line: string;
begin
  if Assigned(SourceEditor) then begin
    Builder := StringBuilder.Create;
    for Line in Source do
      Builder.AppendLine(Line);
    Writer := SourceEditor.CreateWriter;
    try
      Writer.CopyTo(0);
      Writer.DeleteTo(SourceLength);
      Writer.Insert(Builder.ToString);
    finally
      Writer.Close;
    end;
  end;
end;

procedure TProjectSource.TweakResourceLine(ResxPath, ClassName: string; var Error: Boolean);
var
  I,P,Q: Integer;
  Target,S: string;
  Tokens: array of string;
begin
  Tokens := ClassName.Split(['.']);
  ClassName := Tokens[High(Tokens)];
  Target := Path.GetFileName(ResxPath).ToLower;
  for I := 0 to Pred(Source.Count) do begin
    S := Source[I].ToLower;
    if S.Contains(Target) then begin
      P := S.IndexOf('.resources''');
      if Length(Tokens) <= 2 then
        Source[I] := System.string.Format('{0}.{1}{2}',[Source[I].Substring(0,P),ClassName,Source[I].Substring(P)])
      else begin
        S := S.Substring(0,P);
        Q := S.LastIndexOf('.');
        Source[I] := System.string.Format('{0}.{1}{2}',[Source[I].Substring(0,Q),ClassName,Source[I].Substring(P)])
      end;
      Exit;
    end;
  end;
  Error := true;
end;

procedure TProjectSource.TweakSourceLine(FilePath, ClassName, DesignClassName: string; var Error: Boolean);
var
  I: Integer;
  Target,S: string;
begin
  Target := Path.GetFileName(FilePath).ToLower;
  for I := 0 to Pred(Source.Count) do begin
    S := Source[I];
    if S.ToLower.Contains(Target) then begin
      Source[I] := System.string.Format('{0} {{{1}: {2}}}{3}',[Source[I].Substring(0,Length(Source[I]) - 1),ClassName,DesignClassName,Source[I].Substring(Length(Source[I]) - 1)]);
      Exit;
    end;
  end;
  Error := true;
end;

{$endregion}

{$region 'TIDEPlugin.TShutdownWizard'}

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

{$endregion}

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

{$region 'TIDEPlugin'}

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
  TOptions.PreloadSerializer('XmlSerializers.Options.dll','XmlSerializers.Options.TOptionsSerializer');
  TApplicationSts.PreloadSerializer('XmlSerializers.ApplicationSts.dll','XmlSerializers.ApplicationSts.TApplicationStsSerializer');
  TRADStudioProject.PreloadSerializer('XmlSerializers.RADStudio.dll','XmlSerializers.RADStudio.TRADStudioProjectSerializer');
  Options := TOptions.Load;
  OTAActiveProject := nil;
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
  ModuleCreator := BorlandIDE.GetService(typeof(IOTAModuleCreator)) as IOTAModuleCreator;
  IdleNotifier := BorlandIDE.GetService(typeof(IOTAIdleNotifier)) as IOTAIdleNotifier;
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
  if Assigned(OTAActiveProject) then begin
    Exclude(OTAActiveProject.Closed,ProjectClosedHandler);
    Exclude(OTAActiveProject.Renamed,ProjectRenamedHandler);
  end;
  OTAActiveProject := AProject;
  if Assigned(OTAActiveProject) then begin
    Include(OTAActiveProject.Closed,ProjectClosedHandler);
    Include(OTAActiveProject.Renamed,ProjectRenamedHandler);
    MenuItemAddMany.Enabled := (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                               (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality);
    MenuItemAddMany.Visible := MenuItemAddMany.Enabled;
    MenuItemAddProject.Enabled := MenuItemAddMany.Enabled;
    MenuItemAddProject.Visible := MenuItemAddProject.Enabled;
    MenuItemOpenFiles.Enabled := (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                                 (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) or
                                 (OTAActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality);
    MenuItemOpenFiles.Visible := MenuItemOpenFiles.Enabled;
  end
  else begin
    MenuItemAddMany.Enabled := false;
    MenuItemAddMany.Visible := false;
    MenuItemAddProject.Enabled := false;
    MenuItemAddProject.Visible := false;
    MenuItemOpenFiles.Enabled := false;
    MenuItemOpenFiles.Visible := false;
  end;
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

  procedure GetDependentInfo(Module: IOTAModule; var Item: TAddItem);
  var
    I,J,K: Integer;
    CodeDomProvider: IOTACodeDomProvider;
    CodeCompileUnit: System.CodeDom.CodeCompileUnit;
    CodeNamespace: System.CodeDom.CodeNamespace;
    CodeTypeDeclaration: System.CodeDom.CodeTypeDeclaration;
    CodeTypeMember: System.CodeDom.CodeTypeMember;
  begin
		CodeDomProvider := Module.GetService(TypeOf(IOTACodeDomProvider)) as IOTACodeDomProvider;
    CodeCompileUnit := System.CodeDom.CodeCompileUnit(CodeDomProvider.CodeDomFile.GetDom);
    for I := 0 to CodeCompileUnit.Namespaces.Count - 1 do begin
      CodeNamespace := CodeCompileUnit.Namespaces[I];
      for J := 0 to CodeNamespace.Types.Count - 1 do begin
        CodeTypeDeclaration := CodeNamespace.Types[J];
        if CodeTypeDeclaration.IsClass then begin
          for K := 0 to CodeTypeDeclaration.Members.Count - 1 do begin
            CodeTypeMember := CodeTypeDeclaration.Members[K];
            if System.string.Compare(CodeTypeMember.Name,'InitializeComponent',true) = 0 then begin
              try
                Item.DesignClassName := CodeTypeDeclaration.BaseTypes.Item[0].BaseType;
                Item.ClassName := CodeNamespace.Name + '.' + CodeTypeDeclaration.Name;
              except
              end;
              Exit;
            end;
          end;
          if (J = 0) and (J = 0) then begin
            Item.ClassName := CodeNamespace.Name + '.' + CodeTypeDeclaration.Name;
            try
              Item.DesignClassName := CodeTypeDeclaration.BaseTypes.Item[0].BaseType;
            except
              Item.DesignClassName := '';
            end;
          end;
        end;
      end;
    end;
  end;

  function ModuleInfoFromFilePath(Project: IOTAProject; Filepath: string): IOTAModuleInfo;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Pred(Project.ModuleCount) do
      if System.string.Compare(Project.GetModuleFileName(I), FilePath, true) = 0 then begin
        Result := Project.GetModuleInfo(I);
        Break;
      end;
  end;

var
  Item,TweakItem: TAddItem;
  ProjectName,ProjectFilter,Ext: string;
  SearchPatterns: array of string;
  ImageIndexes: array of Integer;
  Module: IOTAModule;
  ModuleInfo: IOTAModuleInfo;
  Detach: TDependentDetach;
  TweakList: TAddItemCollection;
  ProjectSource: TProjectSource;
  Error: Boolean;
begin
  ProjectName := Path.GetFileNameWithoutExtension(OTAActiveProject.FileName);
  if OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality then begin
    Detach := TDependentDetach.Forbidden;
    SearchPatterns := New(array [2] of string);
    ImageIndexes := New(array [2] of Integer);
    SearchPatterns[0] := '*.pas';
    SearchPatterns[1] := '*.dfm';
    ImageIndexes[0] := ItemPasSource;
    ImageIndexes[1] := ItemDelphiRes;
    ProjectFilter := 'RAD Studio Delphi Project File (*.dproj)|*.dproj';
  end
  else if OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality then begin
    Detach := TDependentDetach.AllowedDefaultNo;
    SearchPatterns := New(array [4] of string);
    ImageIndexes := New(array [4] of Integer);
    SearchPatterns[0] := '*.pas';
    SearchPatterns[1] := '*.resx';
    SearchPatterns[2] := '*.nfm';
    SearchPatterns[3] := '*.aspx';
    ImageIndexes[0] := ItemPasSource;
    ImageIndexes[1] := ItemRes;
    ImageIndexes[2] := ItemDelphiRes;
    ImageIndexes[3] := ItemAspx;
    ProjectFilter := 'RAD Studio Delphi Project File (*.dproj)|*.dproj';
  end
  else begin
    ShowToolMessage('Current Personality is not supported');
    Exit;
  end;
  with AddNewDialog.TAddNewDialog.Create(ApplicationSts,ProjectName,ProjectFilter,SearchPatterns,ImageIndexes,Detach,LoadProject) do begin
    if ShowDialog = System.Windows.Forms.DialogResult.OK then begin
      TweakList := TAddItemCollection.Create;
      for Item in Items do begin
        Item.Project := OTAActiveProject;
        try
          if OTAActiveProject.FileInProject(Item.FilePath) then
            ShowToolMessage(System.string.Format('Project {0} already contains file {1}',[ProjectName,Path.GetFileName(Item.FilePath)]))
          else begin
            if not Assigned(Item.DependentFilePaths) then begin
              OTAActiveProject.AddFile(Item.FilePath);
              ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.FilePath),ProjectName]));
              if OpenToo then begin
                ModuleInfo := ModuleInfoFromFilePath(OTAActiveProject,Item.FilePath);
                if Assigned(ModuleInfo) then
                  ModuleInfo.OpenModule.Show;
              end;
            end
            else begin
              Ext := Path.GetExtension(Item.DependentFilePaths[0]).ToLower;
              if (Ext = '.dfm') or (Ext = '.nfm') then begin
                Module := ModuleServices.CreateModule(Item);
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.FilePath),ProjectName]));
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.DependentFilePaths[0]),ProjectName]));
                if OpenToo then
                  Module.Show;
              end
              else if Ext = '.aspx' then begin
                OTAActiveProject.AddFile(Item.DependentFilePaths[0]);
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.DependentFilePaths[0]),ProjectName]));
                OTAActiveProject.AddFile(Item.FilePath,Item.DependentFilePaths[0]);
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.FilePath),ProjectName]));
                TweakItem := Item;
                TweakItem.TweakResource := false;
                try
                  ModuleInfo := ModuleInfoFromFilePath(OTAActiveProject,Item.FilePath);
                  Module := ModuleInfo.OpenModule;
                  GetDependentInfo(Module,TweakItem);
                  TweakList.Add(TweakItem);
                except
                  raise Exception.Create('CodeDom error, project files might require manual editing');
                end;
                if OpenToo and Assigned(Module) then
                  Module.ShowFileName(ModuleInfo.FileName);
              end
              else begin
                Module := ModuleServices.CreateModule(Item);
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.FilePath),ProjectName]));
                if not TProjectSource.ContainsResource(OTAActiveProject,Path.GetFileName(Item.FilePath),Path.GetFileName(Item.DependentFilePaths[0])) then
                  OTAActiveProject.AddFile(Item.DependentFilePaths[0],Item.FilePath);
                ShowToolMessage(System.string.Format('File {0} added to project {1}',[Path.GetFileName(Item.DependentFilePaths[0]),ProjectName]));
                if OpenToo then
                  Module.Show;
                TweakItem := Item;
                TweakItem.TweakResource := true;
                try
                  GetDependentInfo(Module,TweakItem);
                  TweakList.Add(TweakItem);
                except
                  raise Exception.Create('CodeDom error, project files might require manual editing');
                end;
              end;
            end;
          end;
        except
          on E: Exception do begin
            ShowToolMessage(System.string.Format('Error ({2}) on adding file {0} to project {1}',[Item.FilePath,ProjectName,E.Message]));
            Continue;
          end;
        end;
      end;
      if TweakList.Count > 0 then begin
        Error := false;
        try
          ProjectSource := TProjectSource.Load(OTAActiveProject);
          for TweakItem in TweakList do begin
            if TweakItem.TweakResource then
              ProjectSource.TweakResourceLine(TweakItem.DependentFilePaths[0],TweakItem.ClassName,Error);
            ProjectSource.TweakSourceLine(TweakItem.FilePath,TweakItem.ClassName,TweakItem.DesignClassName,Error);
          end;
          ProjectSource.Store(OTAActiveProject);
        except
          Error := true;
        end;
        if Error then
          ShowToolMessage(System.string.Format('Error during project {0} file tweaking, manual editing might be required',[ProjectName]))
        else
          ShowToolMessage(System.string.Format('Project {0} file was successfully tweaked',[ProjectName]));
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
  ModuleInfos: IOTAModuleInfoArray;
  ModuleInfo: IOTAModuleInfo;
begin
  ProjectName := Path.GetFileNameWithoutExtension(OTAActiveProject.FileName);
  Extensions := New(array [1] of string);
  if (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiPersonality) then begin
    Extensions[0] := '.pas';
    ImageIndex := ItemPasSource;
  end
  else if (OTAActiveProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) then begin
    Extensions[0] := '.pas';
    ImageIndex := ItemPasSource;
  end
  else if (OTAActiveProject.Personality = OTAIDEPersonalities.sCSharpPersonality) then begin
    Extensions[0] := '.cs';
    ImageIndex := ItemCsSource;
  end
  else begin
    ShowToolMessage('Current Personality is not supported');
    Exit;
  end;
  ModuleInfos := New(IOTAModuleInfoArray,OTAActiveProject.ModuleCount);
  for I := 0 to Pred(OTAActiveProject.ModuleCount) do
    ModuleInfos[I] := OTAActiveProject.GetModuleInfo(I);
  with TOpenDialog.Create(ApplicationSts,ProjectName,Extensions,ModuleInfos,ImageIndex) do begin
    if ShowDialog = System.Windows.Forms.DialogResult.OK then begin
      for ModuleInfo in SelectedModules do begin
        try
          ModuleInfo.OpenModule.ShowFileName(ModuleInfo.FileName);
        except
          ShowToolMessage(System.string.Format('Error on opening file {0} of project {1}',[ModuleInfo.Name,ProjectName]));
        end;
      end;
    end;
    Free;
  end;
end;

{$endregion}

end.
