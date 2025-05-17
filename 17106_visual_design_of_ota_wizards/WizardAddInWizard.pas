unit WizardAddInWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI,
  DMRepositoryWizard, DMWizard, DMNotifier;

type
  TAddInWizardRepository = class(TRepositoryWizardModule, IOTAFormWizard)
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
  RegisterPackageWizard(TAddInWizardRepository.CreateInterfaced);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TBlankWizardRepository protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TAddInWizardRepository.Execute;

begin
  inherited Execute;
  (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
