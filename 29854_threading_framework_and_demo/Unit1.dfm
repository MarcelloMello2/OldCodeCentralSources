object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Threading Demo'
  ClientHeight = 293
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 6
  Padding.Top = 6
  Padding.Right = 6
  Padding.Bottom = 6
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 21
  object Label1: TLabel
    Left = 6
    Top = 6
    Width = 621
    Height = 21
    Align = alTop
    Caption = 'Log'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 26
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 9
    Top = 30
    Width = 615
    Height = 229
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitTop = 27
  end
  object Button1: TButton
    Left = 6
    Top = 262
    Width = 621
    Height = 25
    Align = alBottom
    Caption = 'Launch tasks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button1Click
    ExplicitLeft = 288
    ExplicitTop = 156
    ExplicitWidth = 75
  end
end
