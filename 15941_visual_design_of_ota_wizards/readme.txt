Code Central Submission:
ID: 15941
Title: Visual design of OTA wizards
Product: Delphi
Category: OpenTools
Copyright: No significant restrictions
Version: 4 to 5
URL:
Short Description: A set of classes to provide visual design of OTA wizards within the IDE. Wizard interfaces implemented as data modules. Creator interfaces implemented as components.

Long Description:
A set of classes to provide visual design and default .dfm streaming of OTA wizards within the IDE:
IOTAWizard, IOTAMenuWizard, IOTARepositoryWizard, IOTAFormWizard, IOTAProjectWizard interfaces implemented as data modules. IOTAModuleCreator, IOTAProjectCreator interfaces implemented as components.

1. WizardUtil.dpk

WizardUtils.pas:
- utility routines

WizardUtilsReg.pas:
- registers components and custom data modules

DMNotifier.pas:
- TNotifierModule - TDataModule descendant, implements IOTANotifier

DMWizard.pas:
- TWizardModule - TNotifierModule descendant, implements IOTAWizard
- TModuleFile - TInterfacedObject descendant, implements IOTAFile
- TCreator - abstract TComponent descendant, implements IOTACreator
- TModuleCreator - TCreator descendant, implements IOTAModuleCreator
- TProjectCreator - TCreator descendant, implements IOTAProjectCreator, IOTAProjectCreator50

DMMenuWizard.pas:
- TMenuWizardModule - TWizardModule descendant, implements IOTAMenuWizard

DMRepositoryWizard.pas:
- TRepositoryWizardModule - TWizardModule descendant, implements IOTARepositoryWizard

2. WizardWizards.dpk

WizardAddInWizard.pas:
- new Add-in wizard creator wizard

WizardMenuWizard.pas:
- new Menu wizard creator wizard

WizardFormWizard.pas:
- new Form wizard creator wizard

WizardProjectWizard.pas:
- new Project wizard creator wizard

WizardPackageWizard.pas:
- new wizard package creator wizard.

3. Installation
- install WizardUtil.dpk
- install WizardWizards.dpk
You should now have TModuleCreator and TProjectCreator registered into the component palette, Wizards page added to your File\New... dialog, and the custom data modules registered with the IDE.
The default wizard author can be specified in the registry using a string value named "DefaultAuthor" under [DelphiBaseRegKey]\DMWizards (typically, HKCU\Software\Borland\Delphi\5.0\DMWizards)

Enjoy. Any comments and ideas welcome.
TOndrej (tondrej@programmer.net)