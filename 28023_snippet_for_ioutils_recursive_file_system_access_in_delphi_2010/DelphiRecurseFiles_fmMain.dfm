object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 338
  ClientWidth = 520
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 520
    Height = 105
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 167
      Top = 54
      Width = 330
      Height = 29
      Caption = 'Recurse Files'
      TabOrder = 0
      OnClick = Button1Click
    end
    object edtFilter: TEdit
      Left = 24
      Top = 59
      Width = 121
      Height = 24
      TabOrder = 1
      Text = '*.pas'
    end
    object edtPath: TEdit
      Left = 24
      Top = 24
      Width = 473
      Height = 24
      TabOrder = 2
      Text = 'c:\temp\'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 105
    Width = 520
    Height = 233
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object ListBox1: TListBox
      Left = 1
      Top = 1
      Width = 518
      Height = 231
      Align = alClient
      TabOrder = 0
    end
  end
end
