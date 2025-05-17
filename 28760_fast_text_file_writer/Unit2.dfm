object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 319
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 40
    Top = 32
    Width = 75
    Height = 25
    Caption = 'ANSI test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 40
    Top = 136
    Width = 209
    Height = 145
    TabOrder = 1
  end
  object Button2: TButton
    Left = 40
    Top = 80
    Width = 75
    Height = 25
    Caption = 'UTF8 test'
    TabOrder = 2
    OnClick = Button2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 150
    Top = 32
    Width = 99
    Height = 73
    ItemIndex = 0
    Items.Strings = (
      'Short strings'
      'Long strings')
    TabOrder = 3
  end
end
