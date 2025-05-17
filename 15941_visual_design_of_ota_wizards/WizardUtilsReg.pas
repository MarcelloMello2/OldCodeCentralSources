unit WizardUtilsReg;

interface

procedure Register;

implementation

uses
  Classes, SysUtils,
  DMNotifier, DMWizard, DMMenuWizard, DMRepositoryWizard,
  DMDesigner, DsgnIntf;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterCustomModule(TNotifierModule, TDataModuleDesignerCustomModule);
  RegisterCustomModule(TWizardModule, TDataModuleDesignerCustomModule);
  RegisterCustomModule(TMenuWizardModule, TDataModuleDesignerCustomModule);
  RegisterCustomModule(TRepositoryWizardModule, TDataModuleDesignerCustomModule);

  RegisterComponents('Wizards', [TModuleCreator, TProjectCreator]);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
