object NewWizardPackageForm: TNewWizardPackageForm
  Left = 192
  Top = 112
  Width = 372
  Height = 226
  Caption = 'New Wizard'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelClassName: TLabel
    Left = 16
    Top = 152
    Width = 57
    Height = 13
    Caption = '&Class name:'
    FocusControl = EditClassName
  end
  object RadioWizardType: TRadioGroup
    Left = 8
    Top = 8
    Width = 263
    Height = 111
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Select wizard &Type'
    ItemIndex = 0
    Items.Strings = (
      '&Add-in Wizard'
      '&Menu Wizard'
      '&Form Wizard'
      '&Project Wizard')
    TabOrder = 0
  end
  object ButtonOK: TButton
    Left = 278
    Top = 16
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object ButtonCancel: TButton
    Left = 278
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object CheckBoxDataModule: TCheckBox
    Left = 16
    Top = 125
    Width = 257
    Height = 17
    Anchors = [akLeft]
    Caption = 'With &Data module'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object EditClassName: TEdit
    Left = 16
    Top = 168
    Width = 263
    Height = 21
    TabOrder = 4
    Text = 'TTestWizard'
    OnChange = EditClassNameChange
  end
end
