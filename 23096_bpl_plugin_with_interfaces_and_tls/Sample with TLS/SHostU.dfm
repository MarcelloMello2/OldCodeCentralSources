object Form1: TForm1
  Left = 239
  Top = 151
  Width = 787
  Height = 540
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 144
  TextHeight = 20
  object GroupBox1: TGroupBox
    Left = 40
    Top = 120
    Width = 705
    Height = 201
    BiDiMode = bdLeftToRight
    Caption = 
      'Dynamic Linking at RuningTime        Control ower Interface     ' +
      '                          Plugin Off '
    ParentBiDiMode = False
    TabOrder = 0
    object Button1: TButton
      Left = 32
      Top = 72
      Width = 177
      Height = 65
      Caption = 'LoadPackage'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 240
      Top = 72
      Width = 201
      Height = 65
      Caption = 'Package'#39's Form'
      Enabled = False
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 472
      Top = 72
      Width = 201
      Height = 65
      Caption = 'UnLoadPackage'
      Enabled = False
      TabOrder = 2
      OnClick = Button3Click
    end
  end
end
