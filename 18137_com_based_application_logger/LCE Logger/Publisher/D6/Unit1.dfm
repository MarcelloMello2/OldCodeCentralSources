object Form1: TForm1
  Left = 192
  Top = 107
  Width = 314
  Height = 127
  Caption = 'App Log'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnLogInfo: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'Log Info'
    TabOrder = 0
    OnClick = BtnLogInfoClick
  end
  object BtnLogError: TButton
    Left = 8
    Top = 38
    Width = 113
    Height = 25
    Caption = 'Log Error'
    TabOrder = 1
    OnClick = BtnLogErrorClick
  end
  object BtnLogWarning: TButton
    Left = 8
    Top = 68
    Width = 113
    Height = 25
    Caption = 'Log Warning'
    TabOrder = 2
    OnClick = BtnLogWarningClick
  end
  object EditInfo: TEdit
    Left = 128
    Top = 10
    Width = 170
    Height = 21
    TabOrder = 3
    Text = 'Hello'
  end
  object EditError: TEdit
    Left = 128
    Top = 41
    Width = 170
    Height = 21
    TabOrder = 4
    Text = 'Stop, you have a problem here'
  end
  object EditWarning: TEdit
    Left = 128
    Top = 71
    Width = 170
    Height = 21
    TabOrder = 5
    Text = 'You work too much'
  end
end
