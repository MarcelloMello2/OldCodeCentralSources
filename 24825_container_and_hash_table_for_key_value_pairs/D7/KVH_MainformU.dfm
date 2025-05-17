object Mainform: TMainform
  Left = 41
  Top = 58
  Width = 632
  Height = 598
  ActiveControl = KeyEdit
  Caption = 'Mainform'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 21
  object StatusBar: TStatusBar
    Left = 0
    Top = 540
    Width = 624
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 474
    Height = 540
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object DisplayMemo: TMemo
      Left = 1
      Top = 75
      Width = 472
      Height = 464
      Align = alClient
      Lines.Strings = (
        'DisplayMemo')
      TabOrder = 2
    end
    object Panel5: TPanel
      Left = 1
      Top = 38
      Width = 472
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel5'
      TabOrder = 1
      object ValueLabel: TLabel
        Left = 0
        Top = 0
        Width = 50
        Height = 37
        Align = alLeft
        AutoSize = False
        Caption = '&Value:'
        FocusControl = ValueEdit
        Layout = tlCenter
      end
      object ValueEdit: TEdit
        Left = 54
        Top = 0
        Width = 402
        Height = 29
        TabOrder = 0
        Text = 'ValueEdit'
      end
    end
    object Panel6: TPanel
      Left = 1
      Top = 1
      Width = 472
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel5'
      TabOrder = 0
      object KeyLabel: TLabel
        Left = 0
        Top = 0
        Width = 50
        Height = 37
        Align = alLeft
        AutoSize = False
        Caption = '&Key:'
        FocusControl = KeyEdit
        Layout = tlCenter
      end
      object KeyEdit: TEdit
        Left = 54
        Top = 0
        Width = 402
        Height = 29
        TabOrder = 0
        Text = 'Edit1'
      end
    end
  end
  object Panel3: TPanel
    Left = 474
    Top = 0
    Width = 150
    Height = 540
    Align = alRight
    Caption = 'Panel3'
    TabOrder = 2
    object EnumerateButton: TButton
      Left = 9
      Top = 108
      Width = 132
      Height = 25
      Action = EnumerateAction
      TabOrder = 3
    end
    object DeleteButton: TButton
      Left = 9
      Top = 75
      Width = 132
      Height = 25
      Action = DeleteAction
      TabOrder = 2
    end
    object FindButton: TButton
      Left = 9
      Top = 42
      Width = 132
      Height = 25
      Action = FindAction
      TabOrder = 1
    end
    object AddButton: TButton
      Left = 9
      Top = 9
      Width = 132
      Height = 25
      Action = AddAction
      Default = True
      TabOrder = 0
    end
    object RestoreButton: TButton
      Left = 9
      Top = 207
      Width = 132
      Height = 25
      Action = RestoreAction
      TabOrder = 4
    end
    object SaveButton: TButton
      Left = 9
      Top = 141
      Width = 132
      Height = 25
      Action = SaveAction
      TabOrder = 5
    end
    object ClearButton: TButton
      Left = 9
      Top = 174
      Width = 132
      Height = 25
      Action = ClearAction
      TabOrder = 6
    end
  end
  object Messagetimer: TTimer
    Enabled = False
    OnTimer = MessagetimerTimer
    Left = 24
    Top = 168
  end
  object ActionList1: TActionList
    Left = 32
    Top = 132
    object AddAction: TAction
      Caption = '&Add'
      OnExecute = AddActionExecute
      OnUpdate = AddActionUpdate
    end
    object FindAction: TAction
      Caption = '&Find'
      OnExecute = FindActionExecute
      OnUpdate = FindActionUpdate
    end
    object DeleteAction: TAction
      Caption = '&Delete'
      OnExecute = DeleteActionExecute
      OnUpdate = FindActionUpdate
    end
    object EnumerateAction: TAction
      Caption = '&Enumerate'
      OnExecute = EnumerateActionExecute
    end
    object SaveAction: TAction
      Caption = '&Save'
      OnExecute = SaveActionExecute
    end
    object RestoreAction: TAction
      Caption = '&Restore'
      OnExecute = RestoreActionExecute
      OnUpdate = RestoreActionUpdate
    end
    object ClearAction: TAction
      Caption = '&Clear'
      OnExecute = ClearActionExecute
    end
  end
end
