object Form1: TForm1
  Left = 371
  Top = 213
  Width = 236
  Height = 146
  Caption = 'Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 100
    Height = 25
    Caption = 'Create Object'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 100
    Height = 25
    Caption = 'Free Object'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 44
    Width = 100
    Height = 25
    Caption = 'Show Classname'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 120
    Top = 44
    Width = 100
    Height = 25
    Caption = 'Check Existance'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 76
    Top = 84
    Width = 75
    Height = 25
    Caption = '&Close'
    Default = True
    TabOrder = 4
    OnClick = Button5Click
  end
end
