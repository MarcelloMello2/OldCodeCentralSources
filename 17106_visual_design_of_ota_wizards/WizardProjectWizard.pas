unit WizardProjectWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI,
  DMRepositoryWizard, DMWizard, DMNotifier;

type
  TProjectWizardRepository = class(TRepositoryWizardModule, IOTAFormWizard)
    ModuleCreator: TModuleCreator;
  private
  protected
    procedure Execute; override;
  public
  end;

procedure Register;

implementation

uses
  WizardUtils;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterPackageWizard(TProjectWizardRepository.CreateInterfaced);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TProjectWizardRepository protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TProjectWizardRepository.Execute;

begin
  inherited Execute;
  (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
