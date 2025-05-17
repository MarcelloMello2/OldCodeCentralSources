object SmartFormAboutDlg: TSmartFormAboutDlg
  Left = 250
  Top = 106
  BorderStyle = bsNone
  Caption = 'SmartFormAboutDlg'
  ClientHeight = 79
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 326
    Height = 79
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 17
      Top = 5
      Width = 292
      Height = 24
      Alignment = taCenter
      Caption = 'Smart Forms have a memory ...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -19
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 16
      Top = 40
      Width = 192
      Height = 13
      Caption = 'Copyright (c) 2001 by Alexander Rodygin'
    end
    object Label3: TLabel
      Left = 16
      Top = 56
      Width = 94
      Height = 13
      Cursor = crHandPoint
      Caption = 'rodigin@yahoo.com'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label3Click
    end
    object BitBtn1: TBitBtn
      Left = 232
      Top = 43
      Width = 75
      Height = 25
      TabOrder = 0
      Kind = bkYes
    end
  end
end
