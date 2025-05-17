object Form2: TForm2
  Left = 250
  Top = 106
  BorderStyle = bsDialog
  Caption = 'Form2'
  ClientHeight = 281
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  SavedComponents = <
    item
      Component = PageControl1
      Properties.Strings = (
        'ActivePage')
      SaveName = 'PageControl1'
    end
    item
      Component = Memo1
      Properties.Strings = (
        'Lines')
      SaveName = 'Memo1'
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
    end>
  SavedProperties.Strings = (
    'ActiveControl')
  SaveName = 'Form2'
  Storage.Path = '[APPPATH][APPNAME].ini'
  Storage.ParsedPath = 'E:\Borland\Delphi5\Bin\delphi32.ini'
  Storage.Enabled = True
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 439
    Height = 240
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 431
        Height = 212
        Align = alClient
        Lines.Strings = (
          'Type here something'
          '')
        TabOrder = 0
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object CheckBox1: TCheckBox
        Left = 32
        Top = 32
        Width = 97
        Height = 17
        Caption = 'Toggle Me '
        TabOrder = 0
      end
      object CheckBox2: TCheckBox
        Left = 32
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Toggle Me Too'
        TabOrder = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 240
    Width = 439
    Height = 41
    Align = alBottom
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 247
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 0
      Kind = bkOK
    end
    object BitBtn2: TBitBtn
      Left = 343
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      TabOrder = 1
      Kind = bkCancel
    end
  end
end
