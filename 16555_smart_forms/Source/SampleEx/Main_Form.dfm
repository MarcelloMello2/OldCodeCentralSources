object MainForm: TMainForm
  Left = 248
  Top = 106
  Width = 696
  Height = 480
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  SavedComponents = <
    item
      Component = about1
      Properties.Strings = (
        'Caption')
      SaveName = 'about1'
    end
    item
      Component = Memo1
      Properties.Strings = (
        'Width')
      SaveName = 'Memo1'
    end
    item
      Component = Memo2
      Properties.Strings = (
        'Width')
      SaveName = 'Memo2'
    end>
  SavedProperties.Strings = (
    'Height'
    'Left'
    'Top'
    'Width')
  SaveName = 'MainForm'
  Storage.Path = '[APPPATH][APPNAME].ini'
  Storage.ParsedPath = 'D:\Program Files\Borland\Delphi5\Bin\delphi32.ini'
  Storage.Enabled = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 185
    Top = 0
    Width = 4
    Height = 434
    Cursor = crHSplit
    Beveled = True
  end
  object Splitter2: TSplitter
    Left = 442
    Top = 0
    Width = 4
    Height = 434
    Cursor = crHSplit
    Beveled = True
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 185
    Height = 434
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 189
    Top = 0
    Width = 253
    Height = 434
    Align = alLeft
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 446
    Top = 0
    Width = 242
    Height = 434
    Align = alClient
    Constraints.MinWidth = 120
    TabOrder = 2
    object BitBtn1: TBitBtn
      Left = 16
      Top = 16
      Width = 97
      Height = 25
      Caption = 'Edit Settings'
      TabOrder = 0
      OnClick = BitBtn1Click
    end
  end
  object MainMenu1: TMainMenu
    Left = 320
    object file1: TMenuItem
      Caption = 'file'
      object new1: TMenuItem
        Caption = 'new'
      end
    end
    object edit1: TMenuItem
      Caption = 'edit'
      object copy1: TMenuItem
        Caption = 'copy'
      end
    end
    object help1: TMenuItem
      Caption = 'help'
      object about1: TMenuItem
        Caption = 'about'
      end
    end
  end
end
