object Form1: TForm1
  Left = 411
  Top = 156
  Width = 353
  Height = 166
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object labTime: TLabel
    Left = 100
    Top = 20
    Width = 6
    Height = 13
    Caption = '0'
  end
  object labTotal: TLabel
    Left = 20
    Top = 88
    Width = 6
    Height = 13
    Caption = '0'
  end
  object labStop: TLabel
    Left = 272
    Top = 48
    Width = 6
    Height = 13
    Caption = '0'
  end
  object labElapsed10: TLabel
    Left = 272
    Top = 104
    Width = 6
    Height = 13
    Caption = '0'
  end
  object labStart: TLabel
    Left = 272
    Top = 20
    Width = 6
    Height = 13
    Caption = '0'
  end
  object labElapsed: TLabel
    Left = 272
    Top = 76
    Width = 6
    Height = 13
    Caption = '0'
  end
  object btnTime: TButton
    Left = 16
    Top = 14
    Width = 75
    Height = 25
    Caption = 'Time'
    TabOrder = 0
    TabStop = False
    OnClick = btnTimeClick
  end
  object edNum: TEdit
    Left = 16
    Top = 56
    Width = 73
    Height = 21
    TabOrder = 1
    Text = '0'
  end
  object btnAdd: TButton
    Left = 96
    Top = 56
    Width = 61
    Height = 25
    Caption = 'Add'
    TabOrder = 2
    TabStop = False
    OnClick = btnAddClick
  end
  object btnSubtract: TButton
    Left = 96
    Top = 84
    Width = 61
    Height = 25
    Caption = 'Subtract'
    TabOrder = 3
    TabStop = False
    OnClick = btnSubtractClick
  end
  object btnStart: TButton
    Left = 176
    Top = 14
    Width = 61
    Height = 25
    Caption = 'Start Time'
    TabOrder = 4
    OnClick = btnStartClick
  end
  object btnAdd10: TButton
    Left = 176
    Top = 98
    Width = 89
    Height = 25
    Caption = 'Add 10 seconds'
    TabOrder = 5
    OnClick = btnAdd10Click
  end
  object btnStop: TButton
    Left = 176
    Top = 42
    Width = 61
    Height = 25
    Caption = 'End Time'
    TabOrder = 6
    OnClick = btnStopClick
  end
end
