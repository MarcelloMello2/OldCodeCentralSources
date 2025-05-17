unit BDSTemplatesMain;
//------------------------------------------------------------------------------
//  File name:      BDSTemplatesMain.pas
//  Last updated:   1/6/04
//  Author:         Sergey Mishkovskiy
//  Company:        USysWare, Inc.
//  Contact info:   usysware@comcast.net
//
//  Compatibility:  Borland Delphi for .NET
//
//  Description:    Implementation of all BDS IDE interfaces necessary
//                  to create either project or unit level add-in.
//------------------------------------------------------------------------------

interface

uses
  Borland.Studio.ToolsAPI,
  BDSConfig;

type
  TBDSProjectWizard = class(System.Object, IOTAProjectWizard)
  private
    FConfigAddin: TConfigAddin;
  public
    constructor Create(ConfigAddin: TConfigAddin);

    class procedure IDERegister; static;
    class function GetAddinBitmap: IntPtr;

    procedure Execute;
    procedure Destroyed;
    function get_IDString: string;
    function get_Name: string;
    function get_Author: string;
    function get_Comment: string;
    function get_Glyph: IntPtr;
    function get_Designer: string;
    function get_GalleryCategory: IOTAGalleryCategory;
    function get_Personality: string;
  end;

  TBDSProjectGroupCreator = class(System.Object, IOTAProjectGroupCreator)
  public
    function get_CreatorType: string;
    function get_FileSystem: string;
    function get_Existing: Boolean;
    function get_Owner: IOTAModule;
    function get_Unnamed: Boolean;
    function get_FileName: string;
    function get_ShowSource: Boolean;
  end;

  TBDSProjectCreator = class(System.Object, IOTAProjectCreator)
  private
    FConfigAddin: TConfigAddin;
    FUnitIndex: Integer;
  public
    constructor Create(ConfigAddin: TConfigAddin; UnitIndex: Integer);

    function get_CreatorType: string;
    function get_FileSystem: string;
    function get_Existing: Boolean;
    function get_Owner: IOTAModule;
    function get_Unnamed: Boolean;
    function get_FileName: string;
    function get_ShowSource: Boolean;
    function get_ProjectPersonality: string;
    function NewProjectSource(projectName: string): IOTAFile;
    procedure NewProjectResource(project: IOTAProject);
    procedure NewDefaultProjectModule(project: IOTAProject);
  end;

  TBDSUnitCreator = class(System.Object, IOTAModuleCreator)
  private
    FConfigAddin: TConfigAddin;
    FUnitIndex: Integer;
    FNameSuffix: string;
  public
    constructor Create(ConfigAddin: TConfigAddin; UnitIndex: Integer);

    function get_CreatorType: string;
    function get_FileSystem: string;
    function get_Existing: Boolean;
    function get_Owner: IOTAModule;
    function get_Unnamed: Boolean;
    function get_AncestorName: string;
    function get_ImplFileName: string;
    function get_IntfFileName: string;
    function get_FormName: string;
    function get_MainForm: Boolean;
    function get_ShowForm: Boolean;
    function get_ShowSource: Boolean;
    function NewFormFile(formIdent: string; ancestorIdent: string): IOTAFile;
    function NewImplSource(moduleIdent: string; formIdent: string;
      ancestorIdent: string): IOTAFile;
    function NewIntfSource(moduleIdent: string; formIdent: string;
      ancestorIdent: string): IOTAFile;
  end;

implementation

{$R BDSTemplates.bmp}

uses
  System.IO,                            
  System.Windows.Forms,  // MessageBox.Show
  System.Drawing,        // Icon
  System.Threading,      // Mutex
  System.Reflection,     // Assembly
  Microsoft.Win32,       // Registry
  BDSOTAUtils,
  BDSDefaultsForm;

const
  AddInCompany = 'USysWare';
  AddInName = 'BDSTemplates';
  AddInVersion = '1.0';
  AddInTemplates = 'Templates';
  AddInRegKey = 'Software\' + AddInCompany + '\' + AddInName + '\' +
    AddInVersion + '\' + AddInTemplates;
  AddInMutexName = AddInCompany + AddInName + AddInVersion;
  AddInLongName = 'Templates add-in';
  AddInCopyright = 'Copyright (c) 2003-2004 USysWare, Inc.' + #13#10 +
    'All Rights Reserved';
  AddInVerPrefix = ' v';
  AddInBitmapResource = 'BDSTemplates.bmp'; // 24x24

var
  AddInInitialized: Boolean = False;
  AddInMutex: Mutex = nil;  // used by Inno Setup installer

{ TBDSProjectWizard }

class procedure TBDSProjectWizard.IDERegister;
var
  IAboutBox: IOTAAboutBoxService;
  ISplashScreen: IOTASplashScreenService;
  IWizardService: IOTAWizardService;
  TemplatesKey: RegistryKey;
  ConfigFiles: array of string;
  Config: TConfig;
  Index, Addin: Integer;
  TemplateFolder: string;
begin
  AddInMutex := Mutex.Create(False, AddInMutexName);

  if AddInInitialized then
    raise ApplicationException.Create(AddInName + ' must be setup just once.');
  AddInInitialized := True;

  ISplashScreen := GetSplashScreenService;
  ISplashScreen.StatusMessage('Loading ' + AddInLongName);
  ISplashScreen.AddPluginBitmap(AddInLongName, GetAddinBitmap, False, '', '');

  IAboutBox := GetAboutBoxService;
  IAboutBox.AddPluginInfo(AddInLongName +
    AddInVerPrefix + Assembly.GetExecutingAssembly.GetName.Version.ToString,
    AddInCopyright, GetAddinBitmap, False, '', '');

  // get templates

  try
    TemplatesKey := Registry.CurrentUser.OpenSubKey(AddInRegKey);
  except
    TemplatesKey := nil;
  end;

  if TemplatesKey <> nil then
  begin
    IWizardService := GetWizardService;

    ConfigFiles := TemplatesKey.GetValueNames;

    for Index := 0 to Length(ConfigFiles) - 1 do
    begin
      TemplateFolder := TemplatesKey.GetValue(ConfigFiles[Index]).ToString;

      if TemplateFolder = '' then
        MessageBox.Show('Template folder is not set for configuration file ' +
          ConfigFiles[Index] + ' .', AddInLongName)
      else
        if not System.IO.File.Exists(ConfigFiles[Index]) then
          MessageBox.Show('Configuration file ' + ConfigFiles[Index] +
            ' does not exist.', AddInLongName)
        else
          try
            // One template XML file could have one or more add-ins defined
            Config := TConfig.Create(ConfigFiles[Index], TemplateFolder);

            for Addin := 0 to Config.AddinCount - 1 do
              IWizardService.AddWizard(
                TBDSProjectWizard.Create(Config.Addins[Addin]));
          except
            on E: Exception do
              MessageBox.Show(E.Message, AddInLongName);
          end;
    end;

    TemplatesKey.Close;
  end;
end;

// This method has to be called once per each bitmap request
class function TBDSProjectWizard.GetAddinBitmap: IntPtr;
var
  AddinBitmapStream: System.IO.Stream;
  AddinBitmap: Bitmap;
begin
  Result := IntPtr.Zero;

  AddinBitmapStream := Assembly.GetExecutingAssembly.GetManifestResourceStream(
    AddInBitmapResource);

  if AddinBitmapStream <> nil then
  begin
    AddinBitmap := Bitmap.Create(AddinBitmapStream);
    if AddinBitmap <> nil then
      Result := AddinBitmap.GetHbitmap;
  end;
end;

constructor TBDSProjectWizard.Create(ConfigAddin: TConfigAddin);
begin
   inherited Create;

   FConfigAddin := ConfigAddin;
end;

procedure TBDSProjectWizard.Execute;
var
  Dialog: TBDSDefaultsForm;
  IModuleServices: IOTAModuleServices;
  IProjectGroup: IOTAProjectGroup;
  IDotNetProject: IOTADotNetProject;
  Index: Integer;
begin
  if not Directory.Exists(FConfigAddin.Config.TemplatesPath) then
    raise ApplicationException.Create(
      FConfigAddin.Name + ' error. Template folder ' +
      FConfigAddin.Config.TemplatesPath +
      ' does not exist. Please reinstall the add-in.');

  FConfigAddin.ParseDynamicData;

  // check if all templates are in place
  for Index := 0 to FConfigAddin.UnitCount - 1 do
    if not System.IO.File.Exists(FConfigAddin.Units[Index].TemplateFile) then
      raise ApplicationException.Create(
        FConfigAddin.Name + ' error. Template ' +
        FConfigAddin.Units[Index].TemplateFile + ' is missing.');

  // configuration dialog
  if (FConfigAddin.Dialog <> nil) and FConfigAddin.Dialog.Enabled then
  begin
    Dialog := TBDSDefaultsForm.Create;
    Dialog.ConfigAddin := FConfigAddin;

    if Dialog.ShowDialog = DialogResult.Cancel then
      Exit;
  end;

  IModuleServices := GetModuleServices;

  if (IModuleServices = nil) then
    raise ApplicationException.Create(
      FConfigAddin.Name + ' error. Failed to execute add-in.');

  // create project group and project source for project level add-ins
  if FConfigAddin.AddinType = atProject then
  begin
    if IModuleServices.MainProjectGroup = nil then
    begin
      // created project group
      IModuleServices.CreateModule(TBDSProjectGroupCreator.Create);

      if IModuleServices.MainProjectGroup = nil then
        raise ApplicationException.Create(FConfigAddin.Name +
          ' error. Failed to create a default project group.');
    end;

    // create project source
    with FConfigAddin.Units[FConfigAddin.UnitProject] do
      IModuleServices.CreateModule(
        TBDSProjectCreator.Create(FConfigAddin, FConfigAddin.UnitProject));
  end;

  IProjectGroup := IModuleServices.MainProjectGroup;
  if IProjectGroup <> nil then
    IDotNetProject := GetDotNetProject(IProjectGroup.ActiveProject)
  else
    IDotNetProject := nil;

  // add all assembly references
  if IDotNetProject <> nil then
    for Index := 0 to FConfigAddin.ReferenceCount - 1 do
      try
        IDotNetProject.References.AddReference(
          FConfigAddin.References[Index].Assembly);
      except
        on E: Exception do
          raise ApplicationException.Create(
            FConfigAddin.Name + ' error. Failed to add a reference to ' +
            FConfigAddin.References[Index].Assembly + ' assembly.');
      end;

  // create all units except project one since it's already been created
  for Index := 0 to FConfigAddin.UnitCount - 1 do
    if not FConfigAddin.Units[Index].IsProject then
      with FConfigAddin.Units[Index] do
        IModuleServices.CreateModule(
          TBDSUnitCreator.Create(FConfigAddin, Index));
end;

procedure TBDSProjectWizard.Destroyed;
begin
  if FConfigAddin <> nil then
    FConfigAddin.Free;
end;

// Must be unique
function TBDSProjectWizard.get_IDString: string;
begin
  Result := FConfigAddin.AddInId;
end;

function TBDSProjectWizard.get_Name: string;
begin
  Result := FConfigAddin.Name;
end;

function TBDSProjectWizard.get_Author: string;
begin
  Result := FConfigAddin.CompanyLong;
end;

function TBDSProjectWizard.get_Comment: string;
begin
  Result := FConfigAddin.Name;
end;

function TBDSProjectWizard.get_Glyph: IntPtr;
var
  ImageFile: Icon;
begin
  if (FConfigAddin.ImageFile <> '') and
     (System.IO.File.Exists(FConfigAddin.ImageFile)) then
  begin
    try
      ImageFile := Icon.Create(FConfigAddin.ImageFile);
    except
      ImageFile := nil;
    end;

    if ImageFile <> nil then
      Result := ImageFile.Handle
    else
      Result := nil; 
  end
  else
    Result := nil;
end;

function TBDSProjectWizard.get_Designer: string;
begin
  Result := OTADesignerTypes.dAny;
end;

function TBDSProjectWizard.get_GalleryCategory: IOTAGalleryCategory;
const
  CategorySep = '\';
var
  GalleryCategoryManager: IOTAGalleryCategoryManager;
  Categories: array of string;
  Index: Integer;
  Category: IOTAGalleryCategory;
  CategoryId: string;
begin
  GalleryCategoryManager := GetGalleryCategoryManager;

  if GalleryCategoryManager = nil then
    Result := nil
  else
  begin
    Categories := FConfigAddin.MenuCategory.Split([CategorySep]);
    CategoryId := '';

    for Index := 0 to Length(Categories) - 1 do
    begin
      CategoryId := CategoryId + Categories[Index];
      Category := GalleryCategoryManager.FindCategory(CategoryId);

      if Category = nil then
      begin
        if Result = nil then
          Result := GalleryCategoryManager.AddCategory(
            CategoryId, Categories[Index], nil)
        else
          Result := GalleryCategoryManager.AddCategory(
            Result, CategoryId, Categories[Index], nil);
      end
      else
        Result := GalleryCategoryManager.AddCategory(
          Category, CategoryId, Categories[Index], nil);

      // If everything fails then add the add-in to Other category
      if Result = nil then
      begin
        Result := GalleryCategoryManager.FindCategory(
          OTAGalleryCategories.sCategoryGalileoOther);
        Break;
      end;
    end; // for
  end;
end;

function TBDSProjectWizard.get_Personality: string;
begin
  Result := OTAIDEPersonalities.sDelphiDotNetPersonality;
end;

{ TBDSProjectGroupCreator }

function TBDSProjectGroupCreator.get_CreatorType: string;
begin
  Result := OTACreatorTypes.sPackage;
end;

function TBDSProjectGroupCreator.get_FileSystem: string;
begin
  Result := '';
end;

function TBDSProjectGroupCreator.get_Existing: Boolean;
begin
  Result := False;
end;

function TBDSProjectGroupCreator.get_Owner: IOTAModule;
begin
  Result := nil;
end;

function TBDSProjectGroupCreator.get_Unnamed: Boolean;
begin
  Result := True;
end;

function TBDSProjectGroupCreator.get_FileName: string;
begin
  Result := '';
end;

function TBDSProjectGroupCreator.get_ShowSource: Boolean;
begin
  Result := False;
end;

{ TBDSProjectCreator }

constructor TBDSProjectCreator.Create(ConfigAddin: TConfigAddin;
  UnitIndex: Integer);
begin
  inherited Create;

  FConfigAddin := ConfigAddin;
  FUnitIndex := UnitIndex;
end;

function TBDSProjectCreator.get_CreatorType: string;
begin
  Result := FConfigAddin.Units[FUnitIndex].UnitType;
end;

function TBDSProjectCreator.get_FileSystem: string;
begin
  Result := '';
end;

function TBDSProjectCreator.get_Existing: Boolean;
begin
  Result := False;
end;

function TBDSProjectCreator.get_Owner: IOTAModule;
begin
  //ToDo: this should be replaced with GetModuleServices.MainProjectGroup;
  //      when it works again.
  Result := nil;
end;

function TBDSProjectCreator.get_Unnamed: Boolean;
begin
  Result := True;
end;

function TBDSProjectCreator.get_FileName: string;
var
  CurrentUnit: TConfigUnit;
begin
  CurrentUnit := FConfigAddin.Units[FUnitIndex];

  Result := GetProjectPath + CurrentUnit.Name +
    Path.GetExtension(CurrentUnit.TemplateFile);
end;

function TBDSProjectCreator.get_ShowSource: Boolean;
begin
  Result := True;
end;

function TBDSProjectCreator.get_ProjectPersonality: string;
begin
  Result := OTAIDEPersonalities.sDelphiDotNetPersonality;
end;

function TBDSProjectCreator.NewProjectSource(projectName: string): IOTAFile;
begin
  Result := OTAFile.Create(FConfigAddin.Units[FUnitIndex].GetTemplateSource);
end;

procedure TBDSProjectCreator.NewProjectResource(project: IOTAProject);
begin
end;

procedure TBDSProjectCreator.NewDefaultProjectModule(project: IOTAProject);
begin
end;

{ TBDSUnitCreator }

constructor TBDSUnitCreator.Create(ConfigAddin: TConfigAddin;
  UnitIndex: Integer);
begin
  inherited Create;

  FConfigAddin := ConfigAddin;
  FUnitIndex := UnitIndex;
end;

function TBDSUnitCreator.get_CreatorType: string;
begin
  Result := FConfigAddin.Units[FUnitIndex].UnitType;
end;

function TBDSUnitCreator.get_FileSystem: string;
begin
  Result := '';
end;

function TBDSUnitCreator.get_Existing: Boolean;
begin
  Result := False;
end;

function TBDSUnitCreator.get_Owner: IOTAModule;
var
  IProjectGroup: IOTAProjectGroup;
begin
  IProjectGroup := GetModuleServices.MainProjectGroup;
  if IProjectGroup <> nil then
    Result := IProjectGroup.ActiveProject
  else
    Result := nil;
end;

function TBDSUnitCreator.get_Unnamed: Boolean;
begin
  Result := True;
end;

function TBDSUnitCreator.get_AncestorName: string;
begin
  Result := '';
end;

function TBDSUnitCreator.get_ImplFileName: string;
var
  IActiveProject: IOTAProject;
  CurrentUnit: TConfigUnit;
  Index: Integer;
  FileExt: string;
begin
  IActiveProject := GetActiveProject;

  CurrentUnit := FConfigAddin.Units[FUnitIndex];
  Index := 0;

  if CurrentUnit.UnitType.ToLower.Equals(OTACreatorTypes.sText.ToLower) then
    FileExt := Path.GetExtension(CurrentUnit.TemplateFile)
  else
    FileExt := '.pas';

  while True do
  begin
    Result := GetProjectPath + CurrentUnit.Name + FNameSuffix + FileExt;

    if (IActiveProject = nil) or (not IActiveProject.FileInProject(Result)) then
      Exit;

    Inc(Index);
    FNameSuffix := Index.ToString;
  end;
end;

function TBDSUnitCreator.get_IntfFileName: string;
begin
  Result := '';
end;

function TBDSUnitCreator.get_FormName: string;
begin
  Result := '';
end;

function TBDSUnitCreator.get_MainForm: Boolean;
begin
  Result := False;
end;

function TBDSUnitCreator.get_ShowForm: Boolean;
begin
  Result := False;
end;

function TBDSUnitCreator.get_ShowSource: Boolean;
begin
  Result := True;
end;

function TBDSUnitCreator.NewFormFile(formIdent: string;
  ancestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TBDSUnitCreator.NewImplSource(moduleIdent: string; formIdent: string;
  ancestorIdent: string): IOTAFile;
var
  TempUnitName: string;
begin
  TempUnitName := FConfigAddin.Units[FUnitIndex].Name;

  if FNameSuffix <> '' then
    FConfigAddin.Units[FUnitIndex].Name :=
      FConfigAddin.Units[FUnitIndex].Name + FNameSuffix;

  try
    Result := OTAFile.Create(FConfigAddin.Units[FUnitIndex].GetTemplateSource);
  finally
    if FNameSuffix <> '' then
    begin
      FConfigAddin.Units[FUnitIndex].Name := TempUnitName;
      
      FNameSuffix := '';
    end;
  end;
end;

function TBDSUnitCreator.NewIntfSource(moduleIdent: string; formIdent: string;
  ancestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
