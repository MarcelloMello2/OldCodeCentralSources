object frmMain: TfrmMain
  Left = 192
  Top = 116
  Width = 623
  Height = 331
  Caption = 'Garbage Collection Simulation'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnStart: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 8
    Top = 70
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = btnStopClick
  end
  object btnNewThread: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'New Thread'
    Enabled = False
    TabOrder = 2
    OnClick = btnNewThreadClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 278
    Width = 615
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Panels = <
      item
        Width = 100
      end
      item
        Width = 250
      end
      item
        Width = 120
      end
      item
        Width = 100
      end>
    SimplePanel = False
    UseSystemFont = False
  end
  object rgSweepMode: TRadioGroup
    Left = 96
    Top = 8
    Width = 185
    Height = 65
    Caption = 'Sweep Mode'
    ItemIndex = 1
    Items.Strings = (
      'Incremental'
      'Stop the world')
    TabOrder = 4
  end
  object chbNewThreads: TCheckBox
    Left = 296
    Top = 12
    Width = 249
    Height = 17
    Caption = 'Launch new threads automatically'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object rgBlockGranularity: TRadioGroup
    Left = 96
    Top = 79
    Width = 185
    Height = 105
    Hint = 'Set to 8 bytes if caching is disabled'
    Caption = 'Block granularity'
    ItemIndex = 0
    Items.Strings = (
      '8 bytes'
      '16 bytes'
      '32 bytes'
      '64 bytes')
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
  end
  object rgCacheCapacity: TRadioGroup
    Left = 96
    Top = 190
    Width = 185
    Height = 79
    Caption = 'Instance Cache Capacity'
    ItemIndex = 0
    Items.Strings = (
      '0 (disabled)'
      '100,000'
      '200,000')
    TabOrder = 7
  end
  object gbSampleClasses: TGroupBox
    Left = 296
    Top = 35
    Width = 185
    Height = 105
    Caption = 'Sample Classes'
    TabOrder = 8
    object chbStatic: TCheckBox
      Left = 16
      Top = 21
      Width = 97
      Height = 17
      Caption = 'Static'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chbDynamic: TCheckBox
      Left = 16
      Top = 44
      Width = 97
      Height = 17
      Caption = 'Dynamic'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chbConvoluted: TCheckBox
      Left = 16
      Top = 67
      Width = 97
      Height = 17
      Caption = 'Convoluted'
      TabOrder = 2
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 504
    Top = 8
  end
end
