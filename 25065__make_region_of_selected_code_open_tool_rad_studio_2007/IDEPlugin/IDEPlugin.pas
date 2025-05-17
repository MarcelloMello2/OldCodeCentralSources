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
    ShutdownWizard: TShutdownWizard;
    OTAService: IOTAService;
    OTAModuleServices: IOTAModuleServices;
    OTAMessageService: IOTAMessageService;
    OTAProject: IOTAProject;
    MainMenuService: IOTAMainMenuService;
    MenuItemRegionalize,MenuItemAdd,MenuItemRemove: IOTAMenuItem;
    Options: TOptions;
  strict private
    procedure OTAProjectSync(Project: IOTAProject);
    procedure IDEShutdownHandler(Sender: TObject; Args: EventArgs);
    procedure ProjectClosedHandler(Sender: TObject; Args: EventArgs);
    procedure FileNotificationHandler(Sender: TObject; Args: FileNotificationEventArgs);
    procedure AdjustSelection(Add: Boolean; Text: string);
    procedure OnMenuExecutedAdd(Sender: TObject; Args: EventArgs);
    procedure OnMenuExecutedRemove(Sender: TObject; Args: EventArgs);
  public
    class procedure IDERegister; static;
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TIDEPlugin))]

implementation

uses
  System.IO,
  System.Text;

const
  PluginCategory = 'Edit';
	AfterToReferenceItem = 'HTMLTidySubmenuItem';
	RegionalizeItem = 'RegionalizeItem';
	RegionalizeRemoveRegionItem = 'RegionalizeRemoveRegionItem';
	RegionalizeMakeRegionItem = 'RegionalizeMakeRegionItem';

type
  ERegionFormat = class(Exception);

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
  Result := 'TIDEPlugin.Regionalize.TShutdownWizard';
end;

function TIDEPlugin.TShutdownWizard.get_Name: string;
begin
  Result := 'IDEPlugin Regionalize ShutdownWizard';
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
  TOptions.PreloadSerializer('XmlSerializers.Options.dll','XmlSerializers.Options.TOptionsSerializer');
  Options := TOptions.Load;
  MainMenuService := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
  MenuItemRegionalize := MainMenuService.AddMenuItem(AfterToReferenceItem,OTAMenuItemLocation.otamlAfter,RegionalizeItem,'&Regionalize');
  MenuItemRegionalize.Category := PluginCategory;
  MenuItemRemove := MainMenuService.AddMenuItem(RegionalizeItem,OTAMenuItemLocation.otamlChild,
                                                RegionalizeRemoveRegionItem,'&Remove Region around Selection');
  MenuItemRemove.Category := PluginCategory;
  MenuItemAdd := MainMenuService.AddMenuItem(RegionalizeItem,OTAMenuItemLocation.otamlChild,
                                             RegionalizeMakeRegionItem,'&Make Region of Selection',BitMap(ImageList.Images.Item[0]).GetHBitMap);
  MenuItemAdd.Category := PluginCategory;
  Include(MenuItemAdd.Executed, OnMenuExecutedAdd);
  Include(MenuItemRemove.Executed, OnMenuExecutedRemove);
  OTAMessageService := BorlandIDE.GetService(typeof(IOTAMessageService)) as IOTAMessageService;
  ShutdownWizard := TIDEPlugin.TShutdownWizard.Create;
  OTAService := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
  Include(OTAService.FileNotification,FileNotificationHandler);
  Include(ShutdownWizard.IDEShutdown,IDEShutdownHandler);
  OTAModuleServices := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
  OTAProjectSync(OTAModuleServices.ActiveProject);
end;

procedure TIDEPlugin.IDEShutdownHandler(Sender: TObject; Args: EventArgs);
begin
  Exclude(OTAService.FileNotification,FileNotificationHandler);
  MainMenuService.RemoveMenuItem(RegionalizeMakeRegionItem);
  MainMenuService.RemoveMenuItem(RegionalizeRemoveRegionItem);
  MainMenuService.RemoveMenuItem(RegionalizeItem);
  ShutdownWizard.Free;
end;

procedure TIDEPlugin.OTAProjectSync(Project: IOTAProject);
begin
  if Assigned(OTAProject) then
    Exclude(OTAProject.Closed,ProjectClosedHandler);
  MenuItemRegionalize.Enabled := false;
  OTAProject := Project;
  if Assigned(OTAProject) then begin
    Include(OTAProject.Closed,ProjectClosedHandler);
    MenuItemRegionalize.Enabled := true;
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
    if Options.ItemRemoveShortcut <> Keys.None then
      MenuItemRemove.ShortCut := ConverShortCut(Options.ItemRemoveShortcut);
    if Options.ItemAddShortcut <> Keys.None then
      MenuItemAdd.ShortCut := ConverShortCut(Options.ItemAddShortcut);
  end;
  if (Args.NotifyCode = OTAFileNotification.ofnActiveProjectChanged) then
    OTAProjectSync(OTAModuleServices.ActiveProject);
end;

procedure TIDEPlugin.AdjustSelection(Add: Boolean; Text: string);
var
  Project: IOTAProject;
  Module: IOTAModule;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
  View: IOTAEditView;
  Reader: IOTAFileReader;
  Writer: IOTAFileWriter;
  BlockStart: OTACharPos;
  BlockAfter: OTACharPos;
  StartPos, EndPos: LongInt;
  IsDelphi: Boolean;
  I: Integer;
  SelectedText: string;

  procedure AdjustSelectedText(var SelectedText: string; Add: Boolean; IsDelphi: Boolean);
  var
    Lines: ArrayList;
    Reader: StringReader;
    SB: StringBuilder;
    I: Integer;
    DRegionSearch,DRegion,DTitledRegion,DEndRegionSearch,DEndRegion,Padding,S1,S2: string;
		IsTitled,HasRegionHeader,HasRegionFooter,IsComment1,IsComment2: Boolean;
  begin
    if IsDelphi then begin
      DRegionSearch := '{$region';
      DRegion := '{$region}';
      DTitledRegion := '{{$region ''{0}''}}';
      DEndRegionSearch := '{$endregion';
      DEndRegion := '{$endregion}';
    end
    else begin
      DRegionSearch := '#region';
      DRegion := DRegionSearch;
      DTitledRegion := '#region {0}';
      DEndRegionSearch := '#endregion';
      DEndRegion := DEndRegionSearch;
    end;
    Lines := ArrayList.Create;
    Reader := StringReader.Create(SelectedText);
    try
      S1 := Reader.ReadLine;
      while Assigned(S1) do begin
        Lines.Add(S1);
        S1 := Reader.ReadLine;
      end;
      if Lines.Count < 2 then
        raise ERegionFormat.Create('The selected text contains less then 2 lines')
      else begin
        S1 := string(Lines[0]).TrimStart([' ',Chr(9)]);
        Padding := string(Lines[0]).Substring(0,string(Lines[0]).Length - S1.Length);
        SB := StringBuilder.Create;
        if Add then begin
          IsTitled := false;
          S2 := S1.TrimEnd([' ',Chr(9)]);
          if Length(S2) > 2 then begin
            IsTitled := S2.Substring(0,2) = '//';
            if IsTitled then
              S1 := S2.Substring(2).Trim([' ',Chr(9)])
            else if IsDelphi and not IsTitled then begin
              IsTitled := (S2[1] = '{') and not (S2[2] = '$') and (S2[Length(S2)] = '}');
              if IsTitled then
                S1 := S2.Substring(1,Length(S2) - 2).Trim([' ',Chr(9)]);
            end;
          end;
          HasRegionHeader := (not IsTitled and (string(Lines[0]).ToLower.IndexOf(DRegionSearch) >= 0)) or
                             (    IsTitled and (string(Lines[1]).ToLower.IndexOf(DRegionSearch) >= 0));
          if IsDelphi and not HasRegionHeader and not IsTitled then begin
            IsComment1 := ((Length(S1) >= 1) and (S1.Substring(0,1) = '{'));
            IsComment2 := ((Length(S1) >= 2) and (S1.Substring(0,2) = '(*'));
            if IsComment1 or IsComment2 then begin
              S2 := string(Lines[Pred(Lines.Count)]).TrimEnd([' ',Chr(9)]);
              IsComment1 := IsComment1 and S2.EndsWith('}');
              IsComment2 := IsComment2 and S2.EndsWith('*)');
              if not IsComment1 and not IsComment2 then
                raise ERegionFormat.Create('The selected text is a badly formatted comment');
            end;
          end
          else begin
            IsComment1 := false;
            IsComment2 := false;
          end;
          if HasRegionHeader then
            raise ERegionFormat.Create('The selected text starts with a region directive')
          else begin
            if IsComment1 or IsComment2 then begin
              if IsComment2 then
                S2 := S1.Substring(2).Trim
              else
                S2 := S1.Substring(1).Trim;
              if S2.Trim <> '' then
                SB.Append(Padding + System.string.Format(DTitledRegion,[S2]))
              else
                SB := SB.Append(Padding + DRegion);
              if IsComment1 then
                SB.Append('{')
              else
                SB.Append('(*');
              SB.Append(Environment.NewLine);
            end
            else if IsTitled then begin
              SB.Append(Padding + System.string.Format(DTitledRegion,[S1]));
              SB.Append(Environment.NewLine);
            end
            else begin
              SB := SB.Append(Padding + DRegion);
              SB.Append(Environment.NewLine);
              SB.Append(Lines[0]);
              SB.Append(Environment.NewLine);
            end;
            for I := 1 to Pred(Lines.Count) - 1 do begin
              SB.Append(Lines[I]);
              SB.Append(Environment.NewLine);
            end;
            if IsComment1 or IsComment2 then begin
              SB := SB.Append(string(Lines[Pred(Lines.Count)]) + DEndRegion);
              SB.Append(Environment.NewLine);
            end
            else begin
              SB.Append(Lines[Pred(Lines.Count)]);
              SB.Append(Environment.NewLine);
              SB := SB.Append(Padding + DEndRegion);
              SB.Append(Environment.NewLine);
            end;
            SelectedText := SB.ToString;
          end;
        end
        else if not Add then begin
          HasRegionHeader := (string(Lines[0]).ToLower.IndexOf(DRegionSearch) >= 0);
          HasRegionFooter := (string(Lines[Pred(Lines.Count)]).ToLower.IndexOf(DEndRegionSearch) >= 0);
          if IsDelphi and HasRegionHeader then begin
            S2 := S1.Replace(DRegionSearch,'');
            IsComment2 := (S2.IndexOf('(*') >= 0);
            IsComment1 := not IsComment2 and (S2.IndexOf('{') >= 0);
          end
          else begin
            IsComment1 := false;
            IsComment2 := false;
          end;
          IsTitled := HasRegionHeader and (S1.Substring(DRegion.Length) <> '');
          if not HasRegionHeader then
            raise ERegionFormat.Create('The selected text does not start with a region directive')
          else if not HasRegionFooter then
            raise ERegionFormat.Create('The selected text does not end with a endregion directive')
          else begin
            if IsComment1 or IsComment2 then begin
              S1 := S1.Replace(DRegionSearch + ' ','').Replace(DRegionSearch,'');
              S1 := S1.Replace('}','').Replace('''','').Replace('{','').Replace('(*','');
              if IsComment1 then
                S1 := Padding + '{' + S1
              else
                S1 := Padding + '(*' + S1;
              SB.Append(S1);
              SB.Append(Environment.NewLine);
            end
            else if IsTitled then begin
              if IsDelphi then begin
                S1 := S1.Replace(DRegionSearch + ' ','').Replace('}','').Replace('''','');
                S1 := Padding + '{ ' + S1 + ' }';
              end
              else begin
                S1 := S1.Replace(DRegionSearch + ' ','');
                S1 := Padding + '//' + S1;
              end;
              SB.Append(S1);
              SB.Append(Environment.NewLine);
            end;
            for I := 1 to Pred(Lines.Count) - 1 do begin
              SB.Append(Lines[I]);
              SB.Append(Environment.NewLine);
            end;
            if IsComment1 or IsComment2 then begin
              S2 := string(Lines[Pred(Lines.Count)]);
              SB.Append(S2.Replace(DEndRegion,''));
              SB.Append(Environment.NewLine);
            end;
            SelectedText := SB.ToString;
          end;
        end;
      end;
    finally
      Lines.Clear;
      Reader.Close;
    end;
  end;

  procedure ShowToolMessage(&Message: string);
  begin
    OTAMessageService.AddToolMessage('',&Message,'Regionalize',0,0);
    OTAMessageService.ShowMessageView(nil);
    if Assigned(SourceEditor) then
      (SourceEditor as IOTAEditor).Show;
  end;

begin
  Module := OTAModuleServices.CurrentModule;
  if not Assigned(Module) then begin
    ShowToolMessage('No Module found');
    Exit;
  end;
  Project := OTAModuleServices.ActiveProject;
  if not Assigned(Project) then
    IsDelphi := true
  else
    IsDelphi := (Project.Personality = OTAIDEPersonalities.sDelphiPersonality) or
                (Project.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality);
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
  try
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
      SelectedText := System.Text.Encoding.UTF8.GetString(Reader.Read(EndPos - StartPos - 1,0));
      Reader.Close;
      AdjustSelectedText(SelectedText,Add,IsDelphi);
      Writer := SourceEditor.CreateWriter;
      Writer.CopyTo(StartPos);
      Writer.DeleteTo(EndPos);
      Writer.Insert(SelectedText);
      Writer.Close;
      (SourceEditor as IOTAEditor).MarkModified;
    end
    else
      ShowToolMessage('Unreformattable text selection');
  except
    on E: ERegionFormat do
      ShowToolMessage(E.Message)
    else
      ShowToolMessage('Unable to reformat the selected text');
  end;
  (SourceEditor as IOTAEditor).Show;
end;

procedure TIDEPlugin.ProjectClosedHandler(Sender: TObject; Args: EventArgs);
begin
  OTAProjectSync(nil);
end;

procedure TIDEPlugin.OnMenuExecutedAdd(Sender: TObject; Args: EventArgs);
begin
  AdjustSelection(true,'');
end;

procedure TIDEPlugin.OnMenuExecutedRemove(Sender: TObject; Args: EventArgs);
begin
  AdjustSelection(false,'');
end;


end.
