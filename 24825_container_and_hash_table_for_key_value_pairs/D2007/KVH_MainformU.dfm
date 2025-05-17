object Mainform: TMainform
  Left = 0
  Top = 0
  ActiveControl = KeyEdit
  Caption = 'Mainform'
  ClientHeight = 559
  ClientWidth = 624
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
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 1
    object DisplayMemo: TMemo
      Left = 9
      Top = 83
      Width = 456
      Height = 448
      Align = alClient
      Lines.Strings = (
        'DisplayMemo')
      TabOrder = 2
    end
    object Panel5: TPanel
      Left = 9
      Top = 46
      Width = 456
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel5'
      Padding.Bottom = 8
      TabOrder = 1
      object ValueLabel: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 50
        Height = 29
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
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
        Align = alClient
        TabOrder = 0
        Text = 'ValueEdit'
      end
    end
    object Panel6: TPanel
      Left = 9
      Top = 9
      Width = 456
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Panel5'
      Padding.Bottom = 8
      TabOrder = 0
      object KeyLabel: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 50
        Height = 29
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 4
        Margins.Bottom = 0
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
        Align = alClient
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
    Padding.Left = 8
    Padding.Top = 8
    Padding.Right = 8
    Padding.Bottom = 8
    TabOrder = 2
    object EnumerateButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 108
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = EnumerateAction
      Align = alTop
      TabOrder = 3
    end
    object DeleteButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 75
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = DeleteAction
      Align = alTop
      TabOrder = 2
    end
    object FindButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 42
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = FindAction
      Align = alTop
      TabOrder = 1
    end
    object AddButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 9
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = AddAction
      Align = alTop
      Default = True
      TabOrder = 0
    end
    object RestoreButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 207
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = RestoreAction
      Align = alTop
      TabOrder = 4
    end
    object SaveButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 141
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = SaveAction
      Align = alTop
      TabOrder = 5
    end
    object ClearButton: TButton
      AlignWithMargins = True
      Left = 9
      Top = 174
      Width = 132
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 8
      Action = ClearAction
      Align = alTop
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
