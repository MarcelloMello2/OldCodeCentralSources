object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 527
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MukaLegendaGC1: TMukaLegendaGC
    Left = 24
    Top = 24
    Width = 153
    Height = 201
  end
  object MukaLegendaCC1: TMukaLegendaCC
    Left = 385
    Top = 48
    Width = 136
    Height = 41
    TabStop = True
  end
  object EditConteiner1: TEditConteiner
    Left = 336
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'EditConteiner1'
  end
  object EditConteiner2: TEditConteiner
    Left = 336
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'EditConteiner2'
    object Button1: TButton
      Left = 72
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Button1'
      TabOrder = 0
    end
  end
  object MukaEdit1: TMukaEdit
    Left = 479
    Top = 159
    Width = 121
    Height = 21
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 3
    Text = 'MukaEdit1'
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 508
    Width = 634
    Height = 19
    Panels = <>
  end
  object MukaLegendaSW1: TMukaLegendaSW
    Left = 44
    Top = 258
    Width = 133
    Height = 103
    VertScrollBar.Range = 78
    Legendas = <
      item
        Color = clBlack
        Caption = 'Teste 1'
      end
      item
        Color = clNavy
        Caption = 'Teste 2'
      end
      item
        Color = clBlack
        Caption = 'Teste 3'
      end
      item
        Color = clBlack
        Caption = 'teste 4'
      end>
    Color = clFuchsia
  end
  object CompLegendaSW1: TCompLegendaSW
    Left = 440
    Top = 217
    Width = 65
    Height = 41
    Color = clWhite
    Legendas = <>
    AutoScroll = True
  end
  object MukaLegendaSW2: TMukaLegendaSW
    Left = 336
    Top = 289
    Width = 225
    Height = 112
    VertScrollBar.Range = 96
    Legendas = <
      item
        Color = clBlack
      end
      item
        Color = clBlack
      end
      item
        Color = clBlack
      end
      item
        Color = clBlack
      end
      item
        Color = clBlack
      end>
    Color = clBlue
  end
end
