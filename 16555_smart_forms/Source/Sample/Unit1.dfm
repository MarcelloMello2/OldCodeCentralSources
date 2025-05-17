object Form1: TForm1
  Left = 250
  Top = 106
  Width = 696
  Height = 218
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  SavedComponents = <>
  SavedProperties.Strings = (
    'Left'
    'Top'
    'Width')
  SaveName = 'Form1'
  Storage.Path = '[APPPATH][APPNAME].ini'
  Storage.ParsedPath = 'E:\Borland\Delphi5\Bin\delphi32.ini'
  Storage.Enabled = True
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 24
    Width = 241
    Height = 25
    Caption = 'Edit Settings'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 24
    Top = 64
    Width = 241
    Height = 25
    Caption = 'Delete Current Form Settings'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 24
    Top = 104
    Width = 241
    Height = 25
    Caption = 'Delete All Settings'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Memo1: TMemo
    Left = 376
    Top = 24
    Width = 297
    Height = 145
    TabOrder = 3
  end
  object Button4: TButton
    Left = 24
    Top = 144
    Width = 241
    Height = 25
    Caption = 'Show Storage Path'
    TabOrder = 4
    OnClick = Button4Click
  end
end
