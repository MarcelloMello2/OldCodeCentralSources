object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 151
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 96
    Width = 70
    Height = 13
    Caption = 'Enter Number:'
  end
  object Label2: TLabel
    Left = 8
    Top = 68
    Width = 65
    Height = 13
    Caption = 'Set precision:'
  end
  object Edit1: TEdit
    Left = 96
    Top = 93
    Width = 185
    Height = 21
    TabOrder = 0
    Text = '3.14159265359E-2'
  end
  object StaticText1: TStaticText
    Left = 96
    Top = 120
    Width = 82
    Height = 23
    BevelInner = bvLowered
    BevelKind = bkSoft
    Caption = 'StaticText1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object SpinEdit1: TSpinEdit
    Left = 96
    Top = 65
    Width = 73
    Height = 22
    MaxValue = 20
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object Button1: TButton
    Left = 96
    Top = 31
    Width = 177
    Height = 25
    Caption = 'Format number from given string'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 0
    Width = 249
    Height = 25
    Caption = 'Display 3.14159265359E-4 in selected format'
    TabOrder = 4
    OnClick = Button2Click
  end
  object RadioGroup1: TRadioGroup
    Left = 352
    Top = 31
    Width = 105
    Height = 105
    Caption = 'Number Format'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'Fixed'
      'Engineering'
      'Scientific')
    TabOrder = 5
  end
end
