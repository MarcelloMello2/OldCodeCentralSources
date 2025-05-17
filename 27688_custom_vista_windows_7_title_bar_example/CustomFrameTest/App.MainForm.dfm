object frmMain: TfrmMain
  Left = 428
  Top = 189
  Caption = 'Custom Title Bar Test'
  ClientHeight = 393
  ClientWidth = 493
  Color = clBtnFace
  Constraints.MinHeight = 438
  Constraints.MinWidth = 511
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  DesignSize = (
    493
    393)
  PixelsPerInch = 120
  TextHeight = 20
  GlassFrame.Enabled = True
  object btnClose: TSpeedButton
    Left = 185
    Top = 352
    Width = 100
    Height = 33
    Anchors = [akBottom]
    Caption = '&Close'
    OnClick = btnCloseClick
  end
  inline ClientFrame: TfraMain
    Left = 0
    Top = 0
    Width = 493
    Height = 332
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
    ExplicitWidth = 493
    ExplicitHeight = 332
    inherited chkEnableCustomFrame: TCheckBox
      Left = 11
      Top = 11
      Width = 470
      Height = 45
      ExplicitLeft = 11
      ExplicitTop = 11
      ExplicitWidth = 470
      ExplicitHeight = 45
    end
    inherited grpBorderIcons: TGroupBox
      Left = 11
      Top = 125
      Width = 150
      Height = 119
      ExplicitLeft = 11
      ExplicitTop = 125
      ExplicitWidth = 150
      ExplicitHeight = 119
      inherited chkSysMenu: TCheckBox
        Left = 11
        Top = 24
        Width = 129
        Height = 23
        ExplicitLeft = 11
        ExplicitTop = 24
        ExplicitWidth = 129
        ExplicitHeight = 23
      end
      inherited chkMinimize: TCheckBox
        Left = 11
        Top = 55
        Width = 129
        Height = 22
        ExplicitLeft = 11
        ExplicitTop = 55
        ExplicitWidth = 129
        ExplicitHeight = 22
      end
      inherited chkMaximize: TCheckBox
        Left = 11
        Top = 85
        Width = 129
        Height = 23
        ExplicitLeft = 11
        ExplicitTop = 85
        ExplicitWidth = 129
        ExplicitHeight = 23
      end
    end
    inherited grpBorderStyle: TGroupBox
      Left = 175
      Top = 125
      Width = 306
      Height = 119
      ExplicitLeft = 175
      ExplicitTop = 125
      ExplicitWidth = 306
      ExplicitHeight = 119
      inherited rdoSingle: TRadioButton
        Left = 11
        Top = 24
        Width = 102
        Height = 23
        ExplicitLeft = 11
        ExplicitTop = 24
        ExplicitWidth = 102
        ExplicitHeight = 23
      end
      inherited rdoSizeable: TRadioButton
        Left = 11
        Top = 55
        Width = 102
        Height = 22
        ExplicitLeft = 11
        ExplicitTop = 55
        ExplicitWidth = 102
        ExplicitHeight = 22
      end
      inherited rdoDialog: TRadioButton
        Left = 11
        Top = 85
        Width = 92
        Height = 23
        ExplicitLeft = 11
        ExplicitTop = 85
        ExplicitWidth = 92
        ExplicitHeight = 23
      end
      inherited rdoToolWindow: TRadioButton
        Left = 120
        Top = 24
        Width = 127
        Height = 23
        ExplicitLeft = 120
        ExplicitTop = 24
        ExplicitWidth = 127
        ExplicitHeight = 23
      end
      inherited rdoSizeableToolWindow: TRadioButton
        Left = 120
        Top = 55
        Width = 181
        Height = 22
        ExplicitLeft = 120
        ExplicitTop = 55
        ExplicitWidth = 181
        ExplicitHeight = 22
      end
    end
    inherited chkGlassFooter: TCheckBox
      Left = 11
      Top = 68
      Width = 465
      Height = 45
      ExplicitLeft = 11
      ExplicitTop = 68
      ExplicitWidth = 465
      ExplicitHeight = 45
    end
    inherited grpIcon: TGroupBox
      Left = 251
      Top = 255
      Width = 230
      Height = 74
      ExplicitLeft = 251
      ExplicitTop = 255
      ExplicitWidth = 230
      ExplicitHeight = 74
      inherited btnResetIcon: TButton
        Left = 11
        Top = 27
        Width = 100
        Height = 33
        ExplicitLeft = 11
        ExplicitTop = 27
        ExplicitWidth = 100
        ExplicitHeight = 33
      end
      inherited btnLoadIcon: TButton
        Left = 119
        Top = 27
        Width = 100
        Height = 33
        ExplicitLeft = 119
        ExplicitTop = 27
        ExplicitWidth = 100
        ExplicitHeight = 33
      end
    end
    inherited grpCaption: TGroupBox
      Left = 11
      Top = 255
      Width = 225
      Height = 74
      ExplicitLeft = 11
      ExplicitTop = 255
      ExplicitWidth = 225
      ExplicitHeight = 74
      inherited edtCaption: TEdit
        Left = 11
        Top = 28
        Width = 204
        ExplicitLeft = 11
        ExplicitTop = 28
        ExplicitWidth = 204
      end
    end
  end
end
