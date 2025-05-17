object Form2: TForm2
  Left = 433
  Top = 202
  Width = 530
  Height = 380
  Caption = 'Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblIP: TLabel
    Left = 31
    Top = 24
    Width = 49
    Height = 13
    Caption = 'Server IP:'
  end
  object lblPort: TLabel
    Left = 56
    Top = 48
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object lbl1: TLabel
    Left = 32
    Top = 120
    Width = 69
    Height = 13
    Caption = 'Customer'#39's list'
  end
  object edtIP: TEdit
    Left = 88
    Top = 20
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'localhost'
  end
  object edtPort: TEdit
    Left = 88
    Top = 44
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '85'
  end
  object btnConnect: TButton
    Left = 88
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 2
    OnClick = btnConnectClick
  end
  object lvCustomers: TListView
    Left = 32
    Top = 144
    Width = 441
    Height = 150
    Columns = <
      item
        Caption = 'Last name'
        Width = 150
      end
      item
        Caption = 'First Name'
        Width = 150
      end
      item
        Caption = 'Born Date'
        Width = 100
      end>
    GridLines = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
  object IdTCPClient1: TIdTCPClient
    MaxLineAction = maException
    ReadTimeout = 0
    Port = 0
    Left = 280
    Top = 40
  end
end
