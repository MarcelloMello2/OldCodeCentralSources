unit PetrVones.Utils.OpenTools;

// Reference:
// http://bdn.borland.com/article/0,1410,30194,00.html

interface

uses
  Borland.Studio.ToolsAPI;

type
  TOTAUtils = class
  private
    class function GetAboutBoxService: IOTAAboutBoxService; static;
    class function GetCurrentModule: IOTAModule; static;
    class function GetActiveProject: IOTAProject; static;
    class function GetActiveProjectGroup: IOTAProjectGroup; static;
    class function GetIdleNotifier: IOTAIdleNotifier; static;
    class function GetMainMenuService: IOTAMainMenuService; static;
    class function GetMessageService: IOTAMessageService; static;
    class function GetModuleServices: IOTAModuleServices; static;
    class function GetOTAService: IOTAService; static;
    class function GetWizardService: IOTAWizardService; static;
  private
    type
      TOTAUtilsDestroyedDelegate = procedure (Sender: TObject; Args: EventArgs) of object;
      TOtaDestroyedWizard = class(TObject, IOTAWizard)
      public
        function get_IDString: string;
        function get_Name: string;
        procedure Destroyed;
        procedure Execute;
      end;
    class var
      FOtaDestroyedWizard: TOtaDestroyedWizard;
      FDestroyed: TOTAUtilsDestroyedDelegate;
  public
    class procedure add_Destroyed(Value: TOTAUtilsDestroyedDelegate); static;
  public
    class function GetSourceEditorForModule(Module: IOTAModule): IOTASourceEditor;
    class function GetSourceEditorText(SourceEditor: IOTASourceEditor): string;
    class function GetSourceEditorData(SourceEditor: IOTASourceEditor): TBytes;
    class procedure SetSourceEditorText(SourceEditor: IOTASourceEditor; const Text: string);
    class function UniqueModuleName(const FileName: string): string;
    class property AboutBoxService: IOTAAboutBoxService read GetAboutBoxService;
    class property CurrentModule: IOTAModule read GetCurrentModule;
    class property ActiveProject: IOTAProject read GetActiveProject;
    class property ActiveProjectGroup: IOTAProjectGroup read GetActiveProjectGroup;
    class property IdleNotifier: IOTAIdleNotifier read GetIdleNotifier;
    class property MainMenuService: IOTAMainMenuService read GetMainMenuService;
    class property MessageService: IOTAMessageService read GetMessageService;
    class property ModuleServices: IOTAModuleServices read GetModuleServices;
    class property OTAService: IOTAService read GetOTAService;
    class property WizardService: IOTAWizardService read GetWizardService;
    class property Destroyed: TOTAUtilsDestroyedDelegate add add_Destroyed remove FDestroyed;
  end;

  TOTASourceModuleCreator = class(TObject, IOTACreator, IOTAModuleCreator)
  private
    FCreatorType: string;
    FFileName: string;
    FSource: string;
  protected
    // IOTACreator
    function get_CreatorType: string;
    function get_Existing: Boolean;
    function get_FileSystem: string;
    function get_Owner: IOTAModule;
    function get_Unnamed: Boolean;
    // IOTAModuleCreator
    function get_AncestorName: string;
    function get_ImplFileName: string;
    function get_IntfFileName: string;
    function get_FormName: string;
    function get_MainForm: Boolean;
    function get_ShowForm: Boolean;
    function get_ShowSource: Boolean;
    function NewFormFile(FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
  public
    constructor Create(const ASource, ACreatorType, AFileName: string);
  end;

implementation

uses
  System.Text, System.IO;

{ TOTAUtils }

class function TOTAUtils.GetAboutBoxService: IOTAAboutBoxService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAAboutBoxService)) as IOTAAboutBoxService;
end;

class function TOTAUtils.GetCurrentModule: IOTAModule;
begin
  Result := ModuleServices.CurrentModule;
end;

class function TOTAUtils.GetActiveProject: IOTAProject;
var
  Group: IOTAProjectGroup;
begin
  Group := ModuleServices.MainProjectGroup;
  if Assigned(Group) then
    Result := Group.ActiveProject
  else
    Result := nil;
end;

class function TOTAUtils.GetActiveProjectGroup: IOTAProjectGroup;
begin
  Result := ModuleServices.MainProjectGroup;
end;

class function TOTAUtils.GetIdleNotifier: IOTAIdleNotifier;
begin
  Result := BorlandIDE.GetService(typeof(IOTAIdleNotifier)) as IOTAIdleNotifier;
end;

class function TOTAUtils.GetMainMenuService: IOTAMainMenuService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAMainMenuService)) as IOTAMainMenuService;
end;

class function TOTAUtils.GetMessageService: IOTAMessageService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAMessageService)) as IOTAMessageService;
end;

class function TOTAUtils.GetModuleServices: IOTAModuleServices;
begin
  Result := BorlandIDE.GetService(typeof(IOTAModuleServices)) as IOTAModuleServices;
end;

class function TOTAUtils.GetOTAService: IOTAService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAService)) as IOTAService;
end;

class function TOTAUtils.GetSourceEditorData(SourceEditor: IOTASourceEditor): TBytes;
var
  Reader: IOTAFileReader;
  Ms: MemoryStream;
  Buffer: TBytes;
  L: Integer;
begin
  Reader := SourceEditor.CreateReader;
  Ms := nil;
  try
    Ms := MemoryStream.Create;
    repeat
      Buffer := Reader.Read(32768, 0); // Reading more than 64k at once does not work
      L := Length(Buffer);
      Ms.Write(Buffer, 0, L);
    until L = 0;
    Result := Ms.ToArray;  
  finally
    Reader.Close;
    Ms.Close;
  end;
end;

class function TOTAUtils.GetSourceEditorForModule(Module: IOTAModule): IOTASourceEditor;
var
  I: Integer;
  E: IOTAEditor;
begin
  Result := nil;
  if Module <> nil then
    for I := 0 to Module.ModuleFileCount - 1 do
    begin
      E := Module.ModuleFileEditors(I);
      if E is IOTASourceEditor then
      begin
        Result := IOTASourceEditor(E);
        Break;
      end;
    end;
end;

class function TOTAUtils.GetSourceEditorText(SourceEditor: IOTASourceEditor): string;
begin
  Result := System.Text.Encoding.UTF8.GetString(GetSourceEditorData(SourceEditor));
end;

class function TOTAUtils.GetWizardService: IOTAWizardService;
begin
  Result := BorlandIDE.GetService(typeof(IOTAWizardService)) as IOTAWizardService;
end;

class procedure TOTAUtils.SetSourceEditorText(SourceEditor: IOTASourceEditor; const Text: string);
var
  Writer: IOTAFileWriter;
begin
  Writer := SourceEditor.CreateWriter;
  try
    Writer.CopyTo(0);
    Writer.DeleteTo(MaxInt);
    Writer.Insert(Text);
  finally
    Writer.Close;
  end;
end;

class function TOTAUtils.UniqueModuleName(const FileName: string): string;
var
  Name, Ext: string;
  Number: Integer;
  M: IOTAModuleServices;
begin
  Result := FileName;
  M := ModuleServices;
  Name := Path.GetFileNameWithoutExtension(FileName);
  Ext := Path.GetExtension(FileName);
  Number := 1;
  while M.FindModule(Result) <> nil do
  begin
    Result := System.String.Format('{0}{1}{2}', [Name, Number, Ext]);
    Inc(Number);
  end;
end;

class procedure TOTAUtils.add_Destroyed(Value: TOTAUtilsDestroyedDelegate);
begin
  if not Assigned(FOtaDestroyedWizard) then
  begin
    FOtaDestroyedWizard := TOtaDestroyedWizard.Create;
    WizardService.AddWizard(FOtaDestroyedWizard);
  end;  
  FDestroyed := TOTAUtilsDestroyedDelegate(Delegate.Combine(Delegate(@FDestroyed), Delegate(@Value)));
end;

{ TOTAUtils.TOtaDestroyedWizard }

function TOTAUtils.TOtaDestroyedWizard.get_IDString: string;
begin
  Result := 'PetrVones.OtaUtils';
end;

function TOTAUtils.TOtaDestroyedWizard.get_Name: string;
begin
  Result := 'TOtaDestroyedWizard';
end;

procedure TOTAUtils.TOtaDestroyedWizard.Destroyed;
begin
  if Assigned(FDestroyed) then
    FDestroyed(Self, EventArgs.Empty);
end;

procedure TOTAUtils.TOtaDestroyedWizard.Execute;
begin
end;

{ TOTASourceModuleCreator }

constructor TOTASourceModuleCreator.Create(const ASource, ACreatorType, AFileName: string);
begin
  inherited Create;
  FCreatorType := ACreatorType;
  FFileName := AFileName;
  FSource := ASource;
end;

function TOTASourceModuleCreator.get_AncestorName: string;
begin
  Result := '';
end;

function TOTASourceModuleCreator.get_CreatorType: string;
begin
  Result := FCreatorType;
end;

function TOTASourceModuleCreator.get_Existing: Boolean;
begin
  Result := False;
end;

function TOTASourceModuleCreator.get_FileSystem: string;
begin
  Result := '';
end;

function TOTASourceModuleCreator.get_FormName: string;
begin
  Result := '';
end;

function TOTASourceModuleCreator.get_ImplFileName: string;
begin
  Result := FFileName;
end;

function TOTASourceModuleCreator.get_IntfFileName: string;
begin
  Result := '';
end;

function TOTASourceModuleCreator.get_MainForm: Boolean;
begin
  Result := False;
end;

function TOTASourceModuleCreator.get_Owner: IOTAModule;
begin
  Result := nil;
end;

function TOTASourceModuleCreator.get_ShowForm: Boolean;
begin
  Result := False;
end;

function TOTASourceModuleCreator.get_ShowSource: Boolean;
begin
  Result := True;
end;

function TOTASourceModuleCreator.get_Unnamed: Boolean;
begin
  Result := True;
end;

function TOTASourceModuleCreator.NewFormFile(FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TOTASourceModuleCreator.NewImplSource(ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := OTAFile.Create(FSource);
end;

function TOTASourceModuleCreator.NewIntfSource(ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
