object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Form1'
  ClientHeight = 349
  ClientWidth = 648
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 88
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Create files'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 16
    Top = 72
    Width = 185
    Height = 265
    Lines.Strings = (
      'Buffered Readln')
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 216
    Top = 72
    Width = 185
    Height = 265
    Lines.Strings = (
      'TStringList with capacity set')
    TabOrder = 2
  end
  object Memo3: TMemo
    Left = 416
    Top = 72
    Width = 185
    Height = 265
    Lines.Strings = (
      'FileStream, search for #13')
    TabOrder = 3
  end
  object Button2: TButton
    Left = 272
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 4
    OnClick = Button2Click
  end
end
