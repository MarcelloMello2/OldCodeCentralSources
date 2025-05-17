unit WizardFormWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, DMRepositoryWizard, DMNotifier, DMWizard;

type
  TFormWizardRepository = class(TRepositoryWizardModule, IOTAFormWizard)
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
  RegisterPackageWizard(TFormWizardRepository.Create(nil));
end;

//----------------------------------------------------------------------------------------------------------------------

{ TFormWizardRepository protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TFormWizardRepository.Execute;

begin
  inherited Execute;
  (BorlandIDEServices as IOTAModuleServices).CreateModule(ModuleCreator);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
