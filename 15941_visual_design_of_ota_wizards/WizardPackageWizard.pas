unit WizardPackageWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI,
  DMRepositoryWizard, DMWizard;

type
  TPackageWizardRepository = class(TRepositoryWizardModule, IOTAProjectWizard)
    ModuleCreatorAddIn: TModuleCreator;
    ModuleCreatorDMAddIn: TModuleCreator;
    ModuleCreatorDMForm: TModuleCreator;
    ModuleCreatorDMMenu: TModuleCreator;
    ModuleCreatorDMProject: TModuleCreator;
    ModuleCreatorForm: TModuleCreator;
    ModuleCreatorMenu: TModuleCreator;
    ModuleCreatorProject: TModuleCreator;
    ProjectCreator: TProjectCreator;

    procedure ModuleCreatorGetOwner(Sender: TObject; var OwnerModule: IOTAModule);
    procedure ModuleCreatorGetSource(Sender: TObject; SourceType: TModuleSourceType;
      var Source: String);
    procedure ProjectCreatorGetOwner(Sender: TObject; var OwnerModule: IOTAModule);
    procedure ProjectCreatorNewDefaultModule(Sender: TObject; const Project: IOTAProject);
  private
    FClassName: string;
    FDataModule: Boolean;
    FNewProject: IOTAProject;
    FWizardType: Integer;
  protected
    procedure Execute; override;
  public
  end;

procedure Register;

implementation

uses
  FormNewWizardPackage,
  WizardUtils;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

function PosEx(Substr: string; S: string; StartPos: Integer): Integer;

var
  P: PChar;

begin
  Result := 0;                         
  if StartPos > Length(S) then
    Exit;
  P := StrPos(PChar(S) + StartPos - 1, PChar(Substr));
  if P = nil then
    Exit;
  Result := P - PChar(S) + 1;
end;

//----------------------------------------------------------------------------------------------------------------------

function Replace(const Find, Replace: string; StartPos: Integer; var S: string): Integer;

begin
  Result := PosEx(Find, S, StartPos);
  if Result = 0 then
    Exit;
  if Length(Replace) = Length(Find) then
    Move(Replace[1], S[Result], Length(Replace))
  else
  begin
    Delete(S, Result, Length(Find));
    Insert(Replace, S, Result);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ReplaceAll(const SFind, SReplace: string; var S: string);

var
  I: Integer;

begin
  I := 1; 
  while I <> 0 do
    I := Replace(SFind, SReplace, I, S);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterPackageWizard(TPackageWizardRepository.Create(nil));
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule1 protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TPackageWizardRepository.Execute;

var
  Result: TModalResult;
  Form: TNewWizardPackageForm;

begin
  inherited;
  FWizardType := -1;
  Form := TNewWizardPackageForm.Create(nil);
  try
    Result := Form.ShowModal;
    if Result = mrOK then
    begin
      FClassName := Form.EditClassName.Text;
      FDataModule := Form.CheckBoxDataModule.Checked;
      FWizardType := Form.RadioWizardType.ItemIndex;
    end;
  finally
    Form.Free;
  end;
  if Result = mrOK then
    (BorlandIDEServices as IOTAModuleServices).CreateModule(ProjectCreator);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule1 event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TPackageWizardRepository.ModuleCreatorGetOwner(Sender: TObject; var OwnerModule: IOTAModule);

begin
  OwnerModule := FNewProject;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPackageWizardRepository.ModuleCreatorGetSource(Sender: TObject; SourceType: TModuleSourceType;
  var Source: String);

begin
  ReplaceAll('%WizardClassName%', FClassName, Source);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPackageWizardRepository.ProjectCreatorGetOwner(Sender: TObject; var OwnerModule: IOTAModule);

begin
  OwnerModule := GetCurrentProjectGroup;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TPackageWizardRepository.ProjectCreatorNewDefaultModule(Sender: TObject; const Project: IOTAProject);

var
  ModuleCreator: TModuleCreator;

begin
  FNewProject := Project;
  try
    if FDataModule then
    begin
      case FWizardType of
        0:
          ModuleCreator := ModuleCreatorDMAddIn;
        1:
          ModuleCreator := ModuleCreatorDMMenu;
        2:
          ModuleCreator := ModuleCreatorDMForm;
        3:
          ModuleCreator := ModuleCreatorDMProject;
        else
          raise Exception.Create('Not supported');
      end;
      // remove 'T' prefix
      ModuleCreator.FormName := Copy(FClassName, 2, Length(FClassName));
      Project.AddFile('WizardWizards.dcp', False);
    end
    else
    begin
      case FWizardType of
        0:
          ModuleCreator := ModuleCreatorAddIn;
        1:
          ModuleCreator := ModuleCreatorMenu;
        2:
          ModuleCreator := ModuleCreatorForm;
        3:
          ModuleCreator := ModuleCreatorProject;
        else
          raise Exception.Create('Not supported');
      end;
    end;

    (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);

    Project.ProjectOptions.SetOptionValue('DesignTimeOnly', True);
    Project.ProjectOptions.SetOptionValue('ExeDescription', Format('%s wizard package', [FClassName]));
  finally
    // release reference
    FNewProject := nil;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
