object FormWizardRepository: TFormWizardRepository
  OldCreateOrder = False
  IDString = 'TOndrej.FormWizardRepository'
  WizardName = 'Form Wizard'
  Author = 'TOndrej'
  Comment = 'Unit/Form wizard'
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000444400000000
    0000000000000000000044440000000000000000000000000044444400000777
    7777777777777777774444447777000000000000000000004444440000070F77
    77777777777777774444447777070F8F8F8F8F8F8F8F8F4444448F8F87070FF8
    F8F8F8F8F8F8F8444444F8F8F7070F8F8F8F8F8F8F8F8444448F8F8F87070FF8
    F8F8F8F8F8F8444444F8F8F8F7070F8F8F8F8F8F8F8444448F8F8F8F87070FF8
    F8F8F8F8F8444444F8F8F8F8F7070F8F8F8F8F8F8444448F8F8F8F8F87070FF8
    F8F8F8F8444444F8F8F8F8F8F7070F8F8F8F8F4444448F8F8F8F8F8F87070FF8
    F8F8F8444444F8F8F8F8F8F8F7070F8F8F8F80BB448F8F8F8F8F8F8F87070FF8
    F8F80BBB44F8F878F8F7F8F8F7070F8F8F80BBB08F8F87978F797F8F87070FF8
    F80BBB08F8F8F878F8F7F8F8F7070F8F8F77B08F8F8F8F8F8F8F8F8F87070FF8
    F87708F8F7F8F878F8F8F8F8F7070F8F8F8F8F8F797F87978F8F8F8F87070FFF
    FFFFFFFFF7FFFF7FFFFFFFFFFF07000000000000000000000000000000070CCC
    CCCCCCCCCCCCCCCCCCCC08080807000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFF0FFFFFFF0FFFFFFC0F800000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  Page = 'Wizards'
  Left = 200
  Top = 100
  Height = 200
  Width = 300
  object ModuleCreator: TModuleCreator
    AncestorName = 'RepositoryWizardModule'
    SourceForm.Strings = (
      'object %1:s: T%1:s'
      '  OldCreateOrder = False'
      '  object ModuleCreator: TModuleCreator'
      '    CreatorType = ctForm'
      '    Existing = False'
      '    AncestorName = '#39'RepositoryWizardModule'#39
      '    MainForm = False'
      '    Left = 48'
      '    Top = 16'
      '  end'
      'end')
    SourceImpl.Strings = (
      'unit %0:s;'
      ''
      'interface'
      ''
      'uses'
      
        '  Windows, Messages, SysUtils, Classes, Graphics, Controls, Form' +
        's, Dialogs,'
      '  ToolsAPI,'
      '  DMRepositoryWizard, DMWizard;'
      ''
      'type'
      '  T%1:s = class(TRepositoryWizardModule, IOTAFormWizard)'
      '    ModuleCreator: TModuleCreator;'
      '  private'
      '  protected'
      '    procedure Execute; override;'
      '  public'
      '  end;'
      ''
      'procedure Register;'
      ''
      'implementation'
      ''
      'uses'
      '  WizardUtils;'
      ''
      '{$R *.DFM}'
      ''
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      ''
      'procedure Register;'
      ''
      'begin'
      '  RegisterPackageWizard(T%1:s.CreateInterfaced);'
      'end;'
      ''
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      ''
      '{ T%1:s protected }'
      ''
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      ''
      'procedure T%1:s.Execute;'
      ''
      'begin'
      '  inherited Execute;'
      
        '  (BorlandIDEServices as IOTAModuleServices).CreateModule(Module' +
        'Creator);'
      'end;'
      ''
      
        '//--------------------------------------------------------------' +
        '--------------------------------------------------------'
      ''
      'end.')
    Left = 48
    Top = 16
  end
end
