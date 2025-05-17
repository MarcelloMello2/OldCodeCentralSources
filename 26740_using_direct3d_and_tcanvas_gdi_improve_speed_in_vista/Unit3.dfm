object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 604
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    728
    604)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 8
    Top = 32
    Width = 337
    Height = 305
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object Button1: TButton
    Left = 8
    Top = 1
    Width = 75
    Height = 25
    Caption = 'GDI'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 360
    Top = 32
    Width = 353
    Height = 313
    Caption = 'Panel1'
    FullRepaint = False
    ParentColor = True
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 32
    Top = 384
    Width = 673
    Height = 193
    Caption = 'Panel2'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 112
    Top = 1
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
end
