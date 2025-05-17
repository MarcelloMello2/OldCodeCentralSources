object Form1: TForm1
  Left = 239
  Top = 148
  Width = 253
  Height = 473
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 120
  TextHeight = 17
  object SpeedButton1: TSpeedButton
    Left = 100
    Top = 8
    Width = 85
    Height = 25
    Caption = 'Clear'
    OnClick = SpeedButton1Click
  end
  object Button1: TButton
    Left = 12
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Keypad'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 12
    Top = 124
    Width = 217
    Height = 296
    ScrollBars = ssVertical
    TabOrder = 1
    WantReturns = False
  end
  object Edit1: TEdit
    Left = 12
    Top = 48
    Width = 217
    Height = 25
    TabOrder = 2
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 12
    Top = 84
    Width = 217
    Height = 25
    TabOrder = 3
    Text = 'Edit2'
  end
end
