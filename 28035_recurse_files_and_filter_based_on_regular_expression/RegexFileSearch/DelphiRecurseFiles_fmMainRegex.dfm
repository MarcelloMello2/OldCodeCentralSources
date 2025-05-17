object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 415
  ClientWidth = 597
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 597
    Height = 137
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Lucida Sans Unicode'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Button1: TButton
      Left = 24
      Top = 95
      Width = 545
      Height = 29
      Caption = 'Recurse Files N&ow'
      TabOrder = 3
      OnClick = Button1Click
    end
    object edtFilter: TEdit
      Left = 24
      Top = 59
      Width = 321
      Height = 30
      TabOrder = 1
      Text = '*.pas'
    end
    object edtPath: TEdit
      Left = 24
      Top = 23
      Width = 545
      Height = 30
      TabOrder = 0
      Text = 'c:\temp\'
    end
    object CheckBoxUseRegEx: TCheckBox
      Left = 368
      Top = 59
      Width = 129
      Height = 30
      Caption = 'Use Reg Ex'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 137
    Width = 597
    Height = 278
    Align = alClient
    Caption = 'Panel2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -18
    Font.Name = 'Lucida Sans Unicode'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 595
      Height = 276
      Align = alClient
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 312
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Presets1: TMenuItem
      Caption = '&Presets'
      object N0windowsBPL1: TMenuItem
        Caption = '&0 - windows BPL'
        OnClick = N0windowsBPL1Click
      end
      object N1Passource1: TMenuItem
        Caption = '&1 - Pas source'
        OnClick = N1Passource1Click
      end
      object N2ProjectLogs1: TMenuItem
        Caption = '&2 - Project Logs'
        OnClick = N2ProjectLogs1Click
      end
      object N3aa1: TMenuItem
        Caption = '&3 - aa'
        OnClick = N3aa1Click
      end
      object N4aaregex1: TMenuItem
        Caption = '&4 - aa regex'
        OnClick = N4aaregex1Click
      end
    end
  end
end
