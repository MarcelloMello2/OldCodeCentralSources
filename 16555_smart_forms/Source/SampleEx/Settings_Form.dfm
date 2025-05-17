object SettingsForm: TSettingsForm
  Left = 253
  Top = 107
  Width = 696
  Height = 480
  Caption = 'SettingsForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  SavedComponents = <
    item
      Component = PageControl1
      Properties.Strings = (
        'ActivePage')
      SaveName = 'PageControl1'
    end
    item
      Component = CheckBox1
      Properties.Strings = (
        'Checked')
      SaveName = 'CheckBox1'
    end
    item
      Component = CheckBox2
      Properties.Strings = (
        'Checked')
      SaveName = 'CheckBox2'
    end
    item
      Component = Image1
      Properties.Strings = (
        'Picture')
      SaveName = 'Image1'
    end
    item
      Component = DataModule2.Table1
      Properties.Strings = (
        'Active')
      SaveName = 'Table1'
    end
    item
      Component = Memo1
      Properties.Strings = (
        'Lines')
      SaveName = 'Memo1'
    end
    item
      Component = DataSource1
      Properties.Strings = (
        'DataSet')
      SaveName = 'DataSource1'
    end
    item
      Component = RadioButton1
      Properties.Strings = (
        'Checked')
      SaveName = 'RadioButton1'
    end
    item
      Component = RadioButton2
      Properties.Strings = (
        'Checked')
      SaveName = 'RadioButton2'
    end>
  SavedProperties.Strings = (
    'ActiveControl'
    'Height'
    'Left'
    'Top'
    'Width')
  SaveName = 'SettingsForm'
  Storage.Path = '[APPPATH][APPNAME].ini'
  Storage.ParsedPath = 'D:\Program Files\Borland\Delphi5\Bin\delphi32.ini'
  Storage.Enabled = True
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 688
    Height = 412
    ActivePage = TabSheet6
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Image1: TImage
        Left = 8
        Top = 20
        Width = 105
        Height = 105
      end
      object Label1: TLabel
        Left = 128
        Top = 24
        Width = 351
        Height = 13
        Caption = 
          '<- Note that no image defined in the IDE. It'#39's being loaded from' +
          ' settings file'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 16
        Top = 40
        Width = 329
        Height = 321
        Lines.Strings = (
          'Type here something')
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TabSheet3'
      ImageIndex = 2
      object CheckBox1: TCheckBox
        Left = 32
        Top = 32
        Width = 97
        Height = 17
        Caption = 'CheckBox1'
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 32
        Top = 64
        Width = 97
        Height = 17
        Caption = 'CheckBox2'
        TabOrder = 1
      end
      object GroupBox1: TGroupBox
        Left = 24
        Top = 112
        Width = 177
        Height = 121
        Caption = 'GroupBox1'
        TabOrder = 2
        object RadioButton1: TRadioButton
          Left = 24
          Top = 32
          Width = 113
          Height = 17
          Caption = 'RadioButton1'
          TabOrder = 0
        end
        object RadioButton2: TRadioButton
          Left = 24
          Top = 64
          Width = 113
          Height = 17
          Caption = 'RadioButton2'
          TabOrder = 1
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'TabSheet4'
      ImageIndex = 3
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 680
        Height = 337
        Align = alTop
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'TabSheet5'
      ImageIndex = 4
      object Label2: TLabel
        Left = 32
        Top = 48
        Width = 117
        Height = 13
        Caption = 'Leave me Active, please'
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'TabSheet6'
      ImageIndex = 5
    end
    object TabSheet7: TTabSheet
      Caption = 'TabSheet7'
      ImageIndex = 6
    end
    object TabSheet8: TTabSheet
      Caption = 'TabSheet8'
      ImageIndex = 7
    end
    object TabSheet9: TTabSheet
      Caption = 'TabSheet9'
      ImageIndex = 8
    end
    object TabSheet10: TTabSheet
      Caption = 'TabSheet10'
      ImageIndex = 9
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 412
    Width = 688
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 504
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 592
      Top = 8
      Width = 75
      Height = 25
      TabOrder = 1
      Kind = bkCancel
    end
  end
  object DataSource1: TDataSource
    DataSet = DataModule2.Table1
    Left = 28
    Top = 416
  end
end
