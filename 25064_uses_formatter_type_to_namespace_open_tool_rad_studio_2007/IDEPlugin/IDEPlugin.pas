unit IDEPlugin;

interface

uses
  Borland.Studio.ToolsAPI,
  Commons.Settings,
  Options,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms,
  TrayAreaInjector,
  TypeList;

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
    TypeList: TTypeList;
    ShutdownWizard: TShutdownWizard;
    OTAService: IOTAService;
    IdleNotifier: IOTAIdleNotifier;
    ModuleServices: IOTAModuleServices;
    MessageService: IOTAMessageService;
    Project: IOTAProject;
    HashProjects: HashTable;
    TrayArea: ITrayArea;
    TrayAreaPanelPersonality,TrayAreaPanelPrefix: Integer;
    Idled: Boolean;
    MainMenuService: IOTAMainMenuService;
    MenuItemUses,MenuItemReformat,MenuItemInterface,MenuItemImplementation,MenuItemFindNamespaces,
    MenuItemSeparator,MenuItemRemoveNonTOption,MenuItemTOption,MenuItemReload: IOTAMenuItem;
  strict private
    procedure ShowToolMessage(&Message: string);
    function CanGetSelectedText(var SourceEditor: IOTASourceEditor; var View: IOTAEditView): Boolean;
    procedure AdjustSelection(AddT,RemoveNonT,AddSystemT: Boolean);
    procedure TrayAreaUpdate(PrefixInsertion: Boolean; Personality: string);
    procedure OTAProjectSync(AProject: IOTAProject);
    procedure IDEShutdownHandler(Sender: TObject; Args: EventArgs);
    procedure IdleHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectClosedHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
    procedure FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
    procedure OnMenuExecutedInterface(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedImplementation(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedFindNamespaces(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedTOption(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedRemoveNonTOption(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedReload(Sender: TObject; Args: EventArgs);
  public
    class procedure IDERegister; static;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIDEPlugin))]

implementation

uses
  Commons.Utils.InteropServices,
  NamespacesDialog,
  System.Diagnostics,
  System.IO,
  System.Text,
  System.Runtime.InteropServices;

const
  PluginCategory = 'Edit';
	AfterToReferenceItem = 'HTMLTidySubmenuItem';
  PUsesItem = 'PUsesItem';
	PUsesReloadOptionsItem = 'PUsesReloadOptionsItem';
	PUsesNonTUnitsRemovalItem = 'PUsesNonTUnitsRemovalItem';
  PUsesTUnitsInsertionItem = 'PUsesTUnitsInsertionItem';
  PUsesFindNamespacesItem = 'PUsesFindNamespacesItem';
  PUsesReformatImplUsesItem = 'PUsesReformatImplUsesItem';
  PUsesReformatIntUsesItem = 'PUsesReformatIntUsesItem';
  PUsesReformatUsesItem = 'PUsesReformatUsesItem';

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
  Result := 'TIDEPlugin.PUses.TShutdownWizard';
end;

function TIDEPlugin.TShutdownWizard.get_Name: string;
begin
  Result := 'IDEPlugin PUses ShutdownWizard';
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
    HashProjects.Clear;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

procedure TIDEPlugin.ShowToolMessage(&Message: string);
begin
  MessageService.AddToolMessage('',&Message,'Uses Reformat',0,0);
  MessageService.ShowMessageView(nil);
end;

function TIDEPlugin.CanGetSelectedText(var SourceEditor: IOTASourceEditor; var View: IOTAEditView): Boolean;
var
  Module: IOTAModule;
  Editor: IOTAEditor;
  I: Integer;
begin
  Result := false;
  Module := ModuleServices.CurrentModule;
  if not Assigned(Module) then begin
    ShowToolMessage('No Module found');
    Exit;
  end;
  for I := 0 to Pred(Module.ModuleFileCount) do begin
    Editor := Module.ModuleFileEditors(I);
    if Editor is IOTASourceEditor then begin
      SourceEditor := IOTASourceEditor(Editor);
      Break;
    end;
  end;
  if not Assigned(SourceEditor) then begin
    ShowToolMessage('No Source Editor found');
    Exit;
  end;
  if SourceEditor.EditViewCount > 1 then begin
    ShowToolMessage('Too many views open, please close all but one');
    Exit;
  end;
  if SourceEditor.EditViewCount < 1 then begin
    ShowToolMessage('No Source Editor View found');
    Exit;
  end;
  View := SourceEditor.GetEditView(0);
  if SourceEditor.BlockType = OTABlockType.btColumn then begin
    ShowToolMessage('Wrong Selection mode');
    Exit;
  end;
  Result := true;
end;

constructor TIDEPlugin.Create;
begin
  inherited Create;
  InitializeComponent;
  TOptions.PreloadSerializer('XmlSerializers.Options.dll','XmlSerializers.Options.TOptionsSerializer');
  TTypeList.PreloadSerializer('XmlSerializers.TypeList.dll','XmlSerializers.TypeList.TTypeListSerializer');
  Options := TOptions.Load;
  TypeList := TTypeList.Load(Options.TypeListPath);
  HashProjects := HashTable.Create;
  MainMenuService := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
  MenuItemUses := MainMenuService.AddMenuItem(AfterToReferenceItem,OTAMenuItemLocation.otamlAfter,PUsesItem,'Uses Reformat');
  MenuItemUses.Category := PluginCategory;
  MenuItemReload := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesReloadOptionsItem,'Reload &Options');
  MenuItemReload.Category := PluginCategory;
  MenuItemRemoveNonTOption := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesNonTUnitsRemovalItem,'&Non Prefix Units Removal',BitMap(ImageList.Images.Item[2]).GetHBitMap);
  MenuItemRemoveNonTOption.Category := PluginCategory;
  MenuItemTOption := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesTUnitsInsertionItem,'&Prefix Units Insertion');
  MenuItemTOption.Category := PluginCategory;
  MenuItemSeparator := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,'','-',0);
  MenuItemSeparator.Category := PluginCategory;
  MenuItemFindNamespaces := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesFindNamespacesItem,'&Find Namespaces');
  MenuItemFindNamespaces.Category := PluginCategory;
  MenuItemImplementation := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesReformatImplUsesItem,'Reformat Im&plementation Uses',BitMap(ImageList.Images.Item[1]).GetHBitMap);
  MenuItemImplementation.Category := PluginCategory;
  MenuItemInterface := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesReformatIntUsesItem,'Reformat &Interface Uses',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  MenuItemInterface.Category := PluginCategory;
  MenuItemReformat := MainMenuService.AddMenuItem(PUsesItem,OTAMenuItemLocation.otamlChild,PUsesReformatUsesItem,'&Reformat Uses',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  MenuItemReformat.Category := PluginCategory;
  MenuItemRemoveNonTOption.Checked := Options.RemoveUnprefixedDefault;
  MenuItemTOption.Enabled := (Options.KnownNamespaceHash.Count > 0);
  MenuItemTOption.Checked := Options.AddPrefixDefault and MenuItemTOption.Enabled;
  MenuItemFindNamespaces.Enabled := (TypeList.KnownTypeHash.Count > 0);
  MenuItemRemoveNonTOption.Visible := MenuItemTOption.Checked;
  MenuItemReformat.Visible := not MenuItemTOption.Checked;
  MenuItemInterface.Visible := not MenuItemReformat.Visible;
  MenuItemImplementation.Visible := not MenuItemReformat.Visible;
  ShutdownWizard := TIDEPlugin.TShutdownWizard.Create;
  OTAService := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
  IdleNotifier := BorlandIDE.GetService(typeof(IOTAIdleNotifier)) as IOTAIdleNotifier;
  ModuleServices := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
  MessageService := BorlandIDE.GetService(typeof(IOTAMessageService)) as IOTAMessageService;
  Include(ShutdownWizard.IDEShutdown,IDEShutdownHandler);
  Include(IdleNotifier.Idle,IdleHandler);
  Include(OTAService.FileNotification,FileNotificationHandler);
  Include(MenuItemReload.Executed, OnMenuExecutedReload);
  Include(MenuItemTOption.Executed, OnMenuExecutedTOption);
  Include(MenuItemRemoveNonTOption.Executed, OnMenuExecutedRemoveNonTOption);
  Include(MenuItemFindNamespaces.Executed, OnMenuExecutedFindNamespaces);
  Include(MenuItemInterface.Executed, OnMenuExecutedInterface);
  Include(MenuItemImplementation.Executed, OnMenuExecutedImplementation);
  Include(MenuItemReformat.Executed, OnMenuExecutedImplementation);
  OTAProjectSync(ModuleServices.ActiveProject);
  TrayAreaPanelPersonality := -1;
  TrayAreaPanelPrefix := -1;
  Idled := false;
end;

procedure TIDEPlugin.IDEShutdownHandler(Sender: TObject; Args: EventArgs);
begin
  if (TrayAreaPanelPrefix >= 0) or (TrayAreaPanelPrefix >= 0) then begin
    if TrayAreaPanelPrefix >= 0 then
      TrayArea.RemovePanel(TrayAreaPanelPrefix);
    if TrayAreaPanelPersonality >= 0 then
      TrayArea.RemovePanel(TrayAreaPanelPersonality);
    Marshal.ReleaseComObject(TrayArea);
  end;
  MainMenuService.RemoveMenuItem(PUsesReformatUsesItem);
  MainMenuService.RemoveMenuItem(PUsesReformatIntUsesItem);
  MainMenuService.RemoveMenuItem(PUsesFindNamespacesItem);
  MainMenuService.RemoveMenuItem(PUsesReformatImplUsesItem);
  MainMenuService.RemoveMenuItem(PUsesTUnitsInsertionItem);
  MainMenuService.RemoveMenuItem(PUsesNonTUnitsRemovalItem);
  MainMenuService.RemoveMenuItem(PUsesReloadOptionsItem);
  MainMenuService.RemoveMenuItem(PUsesItem);
  Exclude(OTAService.FileNotification,FileNotificationHandler);
  Exclude(IdleNotifier.Idle,IdleHandler);
  ShutdownWizard.Free;
end;

procedure TIDEPlugin.IdleHandler(Sender: TObject; Args: EventArgs);
var
  Project: IOTAProject;
begin
  if not Idled then begin
    Exclude(IdleNotifier.Idle,IdleHandler);
    Idled := true;
    Project := ModuleServices.ActiveProject;
    if Assigned(Project) then
      TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, Project.Personality)
    else
      TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, '');
  end;
end;

procedure TIDEPlugin.OTAProjectSync(AProject: IOTAProject);
var
  UnitDirPaths: string;
begin
  if Assigned(Project) then begin
    Exclude(Project.Closed,ProjectClosedHandler);
    Exclude(Project.Renamed,ProjectRenamedHandler);
    if HashProjects.ContainsKey(Project.FileName.ToUpper) then
      HashProjects[Project.FileName.ToUpper] := TObject(Boolean(MenuItemTOption.Checked));
  end;
  MenuItemUses.Enabled := false;
  Project := AProject;
  if Assigned(Project) then begin
    Include(Project.Closed,ProjectClosedHandler);
    Include(Project.Renamed,ProjectRenamedHandler);
    MenuItemUses.Enabled := (Project.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                            (Project.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality);
    if Project.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality then begin
      try
        UnitDirPaths := string((Project.ProjectOptions as IOTAProjectOptions).GetOptionValue('UnitDir'));
      except
        UnitDirPaths := '';
      end;
      MenuItemTOption.Enabled := (Options.KnownNamespaceHash.Count > 0);
      MenuItemFindNamespaces.Enabled := (TypeList.KnownTypeHash.Count > 0);
      if HashProjects.ContainsKey(Project.FileName.ToUpper) then
        MenuItemTOption.Checked := Boolean(HashProjects[Project.FileName.ToUpper]) and MenuItemTOption.Enabled
      else begin
        if UnitDirPaths <> '' then {The use of ToUpper requires that UnitDirPaths is meaningful}
          MenuItemTOption.Checked := Options.AddPrefixDefault and MenuItemTOption.Enabled and
                                     (UnitDirPaths.ToUpper.IndexOf(Options.PrefixUnitPath.ToUpper) >= 0)
        else
          MenuItemTOption.Checked := false;
        HashProjects.Add(Project.FileName.ToUpper,TObject(Boolean(MenuItemTOption.Checked)));
      end;
    end
    else begin
      MenuItemFindNamespaces.Enabled := false;
      MenuItemTOption.Checked := false;
      MenuItemTOption.Enabled := false;
    end;
    MenuItemRemoveNonTOption.Visible := MenuItemTOption.Checked;
    MenuItemReformat.Visible := not MenuItemTOption.Checked;
    MenuItemInterface.Visible := not MenuItemReformat.Visible;
    MenuItemImplementation.Visible := not MenuItemReformat.Visible;
    TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, Project.Personality);
  end
  else
    TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, '');
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
    if Options.ItemImplementationShortcut <> Keys.None then
      MenuItemImplementation.ShortCut := ConverShortCut(Options.ItemImplementationShortcut);
    if Options.ItemInterfaceShortcut <> Keys.None then
      MenuItemInterface.ShortCut := ConverShortCut(Options.ItemInterfaceShortcut);
    if Options.ItemImplementationShortcut <> Keys.None then
      MenuItemReformat.ShortCut := ConverShortCut(Options.ItemImplementationShortcut);
    if Options.ItemRemoveNonPrefixShortCut <> Keys.None then
      MenuItemRemoveNonTOption.ShortCut := ConverShortCut(Options.ItemRemoveNonPrefixShortCut);
    if Options.ItemFindNamespacesShortCut <> Keys.None then
      MenuItemFindNamespaces.ShortCut := ConverShortCut(Options.ItemFindNamespacesShortCut);
  end;
  if (Args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged) then
    OTAProjectSync(ModuleServices.ActiveProject);
end;

procedure TIDEPlugin.TrayAreaUpdate(PrefixInsertion: Boolean; Personality: string);
begin
  if (TrayAreaPanelPersonality < 0) and Idled and Options.TrayAreaInjector then try
    TrayArea := TrayAreaClass.Create;
    TrayArea.AttachTrayArea(Process.GetCurrentProcess.Id,'TAppBuilder');
    TrayAreaPanelPersonality := TrayArea.AddPanel('RADStudio','RAD Studio',0,0);
  except
    TrayAreaPanelPersonality := -1;
  end;
  if (TrayAreaPanelPersonality >= 0) and Idled and Options.TrayAreaInjector then begin
    if Options.TrayAreaPersonality then begin
      if Personality = OTAIDEPersonalities.sDelphiDotNetPersonality then
        TrayArea.PanelImageIndex[TrayAreaPanelPersonality] := 1
      else if Personality = OTAIDEPersonalities.sDelphiPersonality then
        TrayArea.PanelImageIndex[TrayAreaPanelPersonality] := 2
      else if Personality = OTAIDEPersonalities.sCSharpPersonality then
        TrayArea.PanelImageIndex[TrayAreaPanelPersonality] := 3
      else
        TrayArea.PanelImageIndex[TrayAreaPanelPersonality] := 0;
    end;
    if (Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) or
       (Personality = OTAIDEPersonalities.sDelphiPersonality) then begin
      if TrayAreaPanelPrefix < 0 then
        TrayAreaPanelPrefix := TrayArea.AddPanel('PUses','Prefix Units',-1,0);
      if PrefixInsertion then
        TrayArea.PanelImageIndex[TrayAreaPanelPrefix] := 10
      else
        TrayArea.PanelImageIndex[TrayAreaPanelPrefix] := -1;
    end
    else begin
      if TrayAreaPanelPrefix >= 0 then begin
        TrayArea.RemovePanel(TrayAreaPanelPrefix);
        TrayAreaPanelPrefix := -1;
      end;
    end;
  end;
end;

procedure TIDEPlugin.AdjustSelection(AddT,RemoveNonT,AddSystemT: Boolean);
var
  SourceEditor: IOTASourceEditor;
  View: IOTAEditView;
  Reader: IOTAFileReader;
  Writer: IOTAFileWriter;
  BlockStart: OTACharPos;
  BlockAfter: OTACharPos;
  StartPos, EndPos: LongInt;
  S: string;

  procedure AdjustSelectedText(var SelectedText: string);
  var
    UsesItems: array of string;
    UsesItemList,UpperUsesItemList: ArrayList;
    SB: StringBuilder;
    I: Integer;
    S: string;
    UsesSelected: Boolean;
  begin
    UsesItemList := ArrayList.Create;
    UpperUsesItemList := ArrayList.Create;
    SB := StringBuilder.Create;
    UsesSelected := false;
    UsesItems := SelectedText.Split([' ',',',';']);
    for I := 0 to Pred(Length(UsesItems)) do begin
      S := UsesItems[I].Trim;
      if S <> '' then begin
        if S.ToUpper = 'USES' then
          UsesSelected := true
        else begin
          UsesItemList.Add(S);
          UpperUsesItemList.Add(S.ToUpper);
        end;
      end;
    end;
    if AddT then begin
      for I := 0 to Pred(UpperUsesItemList.Count) do begin
        S := string(UpperUsesItemList[I]);
        if not S.EndsWith(Options.UnitPostfix) then begin
          if Options.KnownNamespaceHash.ContainsKey(S) then begin
            if not UpperUsesItemList.Contains(S + Options.UnitPostfix) then
              UsesItemList.Add(string(UsesItemList[I]) + Options.UnitPostfix);
            if RemoveNonT then
              UsesItemList[I] := '';
          end;
        end
        else begin
          S := S.Substring(0,S.Length - Options.UnitPostfix.Length);
          if Options.KnownNamespaceHash.ContainsKey(S) then begin
            if not UpperUsesItemList.Contains(S) and not RemoveNonT then
              UsesItemList.Add(string(UsesItemList[I]).Substring(0,S.Length));
          end;
        end;
      end;
      if AddSystemT then begin
        if not UpperUsesItemList.Contains('SYSTEM' + Options.UnitPostfix) then
          UsesItemList.Add('System' + Options.UnitPostfix);
      end
      else begin
        I := UpperUsesItemList.IndexOf('SYSTEM' + Options.UnitPostfix);
        if I >= 0 then
          UsesItemList.RemoveAt(I);
      end;
    end
    else begin
      for I := Pred(UpperUsesItemList.Count) downto 0 do begin
        S := string(UpperUsesItemList[I]);
        if S.EndsWith(Options.UnitPostfix) then begin
          S := S.Substring(0,S.Length - Options.UnitPostfix.Length);
          if UpperUsesItemList.Contains(S) or (S = 'SYSTEM') then begin
            UpperUsesItemList.RemoveAt(I);
            UsesItemList.RemoveAt(I);
          end
          else begin
            UpperUsesItemList[I] := S;
            S := string(UsesItemList[I]);
            UsesItemList[I] := S.Substring(0,S.Length - Options.UnitPostfix.Length);
          end;
        end;
      end;
    end;
    UsesItemList.Sort;
    if UsesSelected then begin
      SB.Append('uses');
      SB.Append(Environment.NewLine);
    end;
    for I := 0 to Pred(UsesItemList.Count) do begin
      if string(UsesItemList[I]) <> '' then begin
        SB.Append(Options.UsesIndent);
        SB.Append(UsesItemList[I]);
        SB.Append(',');
        SB.Append(Environment.NewLine);
      end;
    end;
    SB.Chars[SB.Length - 1 - Environment.NewLine.Length] := ';';
    SelectedText := SB.ToString;
    UsesItemList.Clear;
    UpperUsesItemList.Clear;
    SetLength(UsesItems,0);
  end;

begin
  SourceEditor := nil;
  if CanGetSelectedText(SourceEditor,View) then try
    BlockStart := SourceEditor.BlockStart;
    BlockAfter := SourceEditor.BlockAfter;
    BlockStart.CharIndex := 0;
    if BlockAfter.CharIndex > 0 then begin
      BlockAfter.CharIndex := 0;
      Inc(BlockAfter.Line);
    end;
    StartPos := View.CharPosToPos(BlockStart);
    EndPos := View.CharPosToPos(BlockAfter);
    if (EndPos - StartPos - 1) > 0 then begin
      Reader := SourceEditor.CreateReader;
      Reader.Read(StartPos,0);
      S := System.Text.Encoding.UTF8.GetString(Reader.Read(EndPos - StartPos - 1,0));
      Reader.Close;
      AdjustSelectedText(S);
      Writer := SourceEditor.CreateWriter;
      Writer.CopyTo(StartPos);
      Writer.DeleteTo(EndPos);
      Writer.Insert(S);
      Writer.Close;
      (SourceEditor as IOTAEditor).MarkModified;
    end;
  except
    ShowToolMessage('Unable to reformat the selected text');
  end;
  if Assigned(SourceEditor) then
    (SourceEditor as IOTAEditor).Show;
end;

procedure TIDEPlugin.ProjectClosedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(nil);
end;

procedure TIDEPlugin.ProjectRenamedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(ModuleServices.ActiveProject);
end;

procedure TIDEPlugin.OnMenuExecutedInterface(Sender: TObject; Args: EventArgs);
begin
  AdjustSelection(MenuItemTOption.Checked,MenuItemRemoveNonTOption.Checked,true);
end;

procedure TIDEPlugin.OnMenuExecutedImplementation(Sender: TObject; Args: EventArgs);
begin
  AdjustSelection(MenuItemTOption.Checked,MenuItemRemoveNonTOption.Checked,false);
end;

procedure TIDEPlugin.OnMenuExecutedFindNamespaces(Sender: TObject; Args: EventArgs);
var
  SourceEditor: IOTASourceEditor;
  View: IOTAEditView;
  Reader: IOTAFileReader;
  BlockStart: OTACharPos;
  BlockAfter: OTACharPos;
  StartPos, EndPos: LongInt;
  S: string;
begin
  SourceEditor := nil;
  if CanGetSelectedText(SourceEditor,View) then try
    BlockStart := SourceEditor.BlockStart;
    BlockAfter := SourceEditor.BlockAfter;
    StartPos := View.CharPosToPos(BlockStart);
    EndPos := View.CharPosToPos(BlockAfter);
    if (EndPos - StartPos) > 0 then begin
      Reader := SourceEditor.CreateReader;
      Reader.Read(StartPos,0);
      S := System.Text.Encoding.UTF8.GetString(Reader.Read(EndPos - StartPos,0));
      Reader.Close;
    end
    else
      S := '';
    with TNamespacesDialog.Create(TypeList) do begin
      TypeName := S;
      ShowDialog;
      Free;
    end;
  except
    ShowToolMessage('Unable to get the selected text');
  end;
  if Assigned(SourceEditor) then
    (SourceEditor as IOTAEditor).Show;
end;

procedure TIDEPlugin.OnMenuExecutedTOption(Sender: TObject; Args: EventArgs);
begin
  MenuItemTOption.Checked := not MenuItemTOption.Checked;
  MenuItemRemoveNonTOption.Visible := MenuItemTOption.Checked;
  MenuItemReformat.Visible := not MenuItemTOption.Checked;
  MenuItemInterface.Visible := not MenuItemReformat.Visible;
  MenuItemImplementation.Visible := not MenuItemReformat.Visible;
  if Assigned(Project) then
    TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, Project.Personality)
  else
    TrayAreaUpdate(MenuItemTOption.Checked and MenuItemUses.Enabled, '');
end;

procedure TIDEPlugin.OnMenuExecutedRemoveNonTOption(Sender: TObject; Args: EventArgs);
begin
  MenuItemRemoveNonTOption.Checked := not MenuItemRemoveNonTOption.Checked;
end;

procedure TIDEPlugin.OnMenuExecutedReload(Sender: TObject; Args: EventArgs);
begin
  Cursor.Current := Cursors.WaitCursor;
  try
    TypeList.Free;
    Options.Free;
    Options := TOptions.Load;
    TypeList := TTypeList.Load(Options.TypeListPath);
    OTAProjectSync(ModuleServices.ActiveProject);
  finally
    Cursor.Current := Cursors.Default;
  end;
end;


end.
