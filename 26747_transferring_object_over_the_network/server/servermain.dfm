object Form1: TForm1
  Left = 429
  Top = 283
  Width = 258
  Height = 141
  Caption = 'Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPort: TLabel
    Left = 24
    Top = 28
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object edtPort: TEdit
    Left = 56
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
    Text = '85'
  end
  object btnStart: TButton
    Left = 56
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Start listening'
    TabOrder = 1
    OnClick = btnStartClick
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 0
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnExecute = IdTCPServer1Execute
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 200
    Top = 32
  end
end
