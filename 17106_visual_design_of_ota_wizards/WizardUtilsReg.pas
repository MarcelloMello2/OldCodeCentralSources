unit WizardUtilsReg;

interface

procedure Register;

implementation

uses
  Classes, SysUtils,
  DMNotifier,
  DMWizard,
  DMMenuWizard,
  DMRepositoryWizard,
  Designer, DesignIntf, DMForm;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterCustomModule(TNotifierModule, TDataModuleCustomModule);
  RegisterCustomModule(TWizardModule, TDataModuleCustomModule);
  RegisterCustomModule(TMenuWizardModule, TDataModuleCustomModule);
  RegisterCustomModule(TRepositoryWizardModule, TDataModuleCustomModule);

  RegisterComponents('Wizards', [TModuleCreator, TProjectCreator]);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
