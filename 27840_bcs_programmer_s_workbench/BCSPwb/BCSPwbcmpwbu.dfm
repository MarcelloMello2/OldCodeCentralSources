object BCSPwbcmpC: TBCSPwbcmpC
  Left = 0
  Top = 0
  Caption = 'Brooks Computing Systems Programmer'#39's Workbench Tester'
  ClientHeight = 87
  ClientWidth = 480
  Color = 11199487
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF009999
    99999999999999999999999999999CC00000000000000000000000000CC99CC0
    00000000B00000000B0000000CC99CC00000000040000800040008080CC99CC0
    0000000030000000030000000CC99CC008080000C00000000C0008080CC99CC0
    00000000C00008080C0000000CC99CC008080800C00000000C0000000CC99CC0
    00000000C0080C00CC0000000CC99CCCC00C0000C0000C00CC0000000CC99CCC
    C00C0000C0000C00CC00000CCCC99CCCC00C0000C0000CCCCC00000CCCC99CCC
    CCCC0000C0030CCCCC00000CCCC99CCCCCCC0300C0000CCCCC00000CCCC99CCC
    CC9C0000CC00CCCCCC00000CCCC99CCCCCCC0300CCCCCCCCCC00000CCCC99CCC
    CCCC0000CCCFCCCCCC00080CCCC99CCCFCCC0808CCCCCCCCCCCC000CCCC99CCC
    CCCC0000CCCCCCCCFCCC080CCCC99CCCCCCC0808CCCCCCCCCCCC000CCFC99CCC
    CCCC0000CCCCCCCCCCCCC0CCCCC99CCCCCCC0808CCCCCCCCCCCCC0CCCCC99CCC
    CCCC0000CCCCFCCCCCCCC0CCCCC99CCCCCCCC00CCCCCCCCCCCCCCCCCCCC99CCC
    FCCCC00CCCCCCCCCFCCCCCCCCCC99CCCCCCCCCCCCCCCCCCCCCCCCC9CCCC99CCC
    CCCCCCCCCCCCCFCCCCCCCCCCCCC99CCCCCCCCCCCCCCCCCCCCFCCCCCCCCC99CCC
    CCCC6FCCCCCCCCCCCCCCCCCCFCC99CCCCCCCCCCCCCCCCCCCCCCCCCCCCCC99CCC
    CCCCCCCCCCCCCCCCCCCCCCCCCCC9999999999999999999999999999999990000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvStatusBar1: TJvStatusBar
    Left = 0
    Top = 68
    Width = 480
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 175
      end
      item
        Width = 50
      end>
    object RClockDisplay: TLabel
      Left = 405
      Top = 0
      Width = 75
      Height = 19
      Align = alRight
      Caption = 'RClockDisplay'
      ExplicitHeight = 15
    end
  end
  object OvcNotebook1: TOvcNotebook
    Left = 0
    Top = 0
    Width = 480
    Height = 68
    ActiveTabFont.Charset = DEFAULT_CHARSET
    ActiveTabFont.Color = clWindowText
    ActiveTabFont.Height = -11
    ActiveTabFont.Name = 'Tahoma'
    ActiveTabFont.Style = []
    PageUsesTabColor = False
    TabOrientation = toBottom
    Align = alClient
    Color = 11199487
    TabOrder = 1
    object Page1: TOvcTabPage
      Caption = 'Programmer'#39's Workbench Test Panel'
      TabColor = 11199487
    end
  end
  object MainMenu1: TMainMenu
    Left = 85
    Top = 3
    object PrimaryOptoins1: TMenuItem
      Caption = 'Primary Options'
      object TestComponent1: TMenuItem
        Caption = 'Test Component'
        OnClick = TestComponent1Click
      end
      object ChangeColors1: TMenuItem
        Caption = 'Change Colors'
        OnClick = ChangeColors1Click
      end
      object Exit2: TMenuItem
        Caption = 'Exit'
        OnClick = Exit2Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
    end
    object Exit1: TMenuItem
      Caption = 'Exit'
      OnClick = Exit1Click
    end
  end
  object JvColorDialog1: TJvColorDialog
    Options = [cdFullOpen]
    Left = 158
    Top = 3
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 227
    Top = 3
  end
  object BCSPwbdp1: TBCSPwbdp
    RColor = clBlack
    Left = 24
    Top = 3
  end
end
