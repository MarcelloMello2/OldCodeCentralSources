unit WizardMenuWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, DMRepositoryWizard, DMNotifier, DMWizard;

type
  TMenuWizardRepository = class(TRepositoryWizardModule, IOTAFormWizard)
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
  RegisterPackageWizard(TMenuWizardRepository.CreateInterfaced);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TMenuWizardRepository protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TMenuWizardRepository.Execute;

begin
  inherited Execute;
  (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
