object NumericKeypad: TNumericKeypad
  Left = 496
  Top = 109
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'NumericKeypad'
  ClientHeight = 213
  ClientWidth = 170
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Numpad: TDrawGrid
    Left = 0
    Top = 0
    Width = 170
    Height = 213
    Align = alClient
    ColCount = 4
    DefaultColWidth = 40
    DefaultRowHeight = 40
    DefaultDrawing = False
    FixedCols = 0
    FixedRows = 0
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -20
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
    ParentFont = False
    TabOrder = 0
    OnClick = NumpadClick
    OnDrawCell = NumpadDrawCell
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 60
    Top = 20
  end
end
