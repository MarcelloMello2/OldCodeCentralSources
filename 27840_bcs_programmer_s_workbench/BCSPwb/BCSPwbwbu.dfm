object BCSPwbC: TBCSPwbC
  Left = 0
  Top = 0
  Caption = 'BCSPwb Primary Dialog'
  ClientHeight = 347
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
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object JvStatusBar1: TJvStatusBar
    Left = 0
    Top = 328
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
    Height = 328
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
      Caption = 'Categories'
      TabColor = 11199487
      object JvDBGrid1: TJvDBGrid
        Left = 0
        Top = 25
        Width = 477
        Height = 281
        Align = alClient
        DataSource = BCSPwbD.DataSource1
        ParentColor = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnCellClick = JvDBGrid1CellClick
        SelectColumnsDialogStrings.Caption = 'Select columns'
        SelectColumnsDialogStrings.OK = '&OK'
        SelectColumnsDialogStrings.NoSelectionWarning = 'At least one column must be visible!'
        EditControls = <>
        RowsHeight = 17
        TitleRowHeight = 17
        Columns = <
          item
            Expanded = False
            FieldName = 'Category'
            Width = 450
            Visible = True
          end>
      end
      object DBNavigator1: TDBNavigator
        Left = 0
        Top = 0
        Width = 477
        Height = 25
        DataSource = BCSPwbD.DataSource1
        Align = alTop
        TabOrder = 1
      end
    end
    object Page2: TOvcTabPage
      Caption = 'Programs'
      TabColor = 11199487
      object DBText2: TDBText
        Left = 0
        Top = 0
        Width = 477
        Height = 17
        Align = alTop
        Alignment = taCenter
        DataField = 'category'
        DataSource = BCSPwbD.DataSource1
        ExplicitLeft = 208
        ExplicitTop = 8
        ExplicitWidth = 65
      end
      object DBNavigator2: TDBNavigator
        Left = 0
        Top = 17
        Width = 477
        Height = 25
        DataSource = BCSPwbD.DataSource2
        Align = alTop
        TabOrder = 0
      end
      object OvcNotebook2: TOvcNotebook
        Left = 0
        Top = 42
        Width = 477
        Height = 264
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
        object Page3: TOvcTabPage
          Caption = 'Programs'
          TabColor = 11199487
          object DBGrid1: TDBGrid
            Left = 0
            Top = 0
            Width = 474
            Height = 242
            Align = alClient
            DataSource = BCSPwbD.DataSource2
            ParentColor = True
            PopupMenu = PopupMenu1
            TabOrder = 0
            TitleFont.Charset = DEFAULT_CHARSET
            TitleFont.Color = clWindowText
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = []
            OnTitleClick = DBGrid1TitleClick
            Columns = <
              item
                Expanded = False
                FieldName = 'prog_desc'
                Title.Caption = 'Description'
                Width = 219
                Visible = True
              end
              item
                Expanded = False
                FieldName = 'xqt_cmd'
                Title.Caption = 'Command Line'
                Width = 218
                Visible = True
              end>
          end
        end
        object Page4: TOvcTabPage
          Caption = 'Documentation'
          TabColor = 11199487
          object DBText3: TDBText
            Left = 0
            Top = 0
            Width = 474
            Height = 17
            Align = alTop
            Alignment = taCenter
            DataField = 'prog_desc'
            DataSource = BCSPwbD.DataSource2
          end
          object OvcNotebook3: TOvcNotebook
            Left = 0
            Top = 17
            Width = 474
            Height = 225
            ActiveTabFont.Charset = DEFAULT_CHARSET
            ActiveTabFont.Color = clWindowText
            ActiveTabFont.Height = -11
            ActiveTabFont.Name = 'Tahoma'
            ActiveTabFont.Style = []
            PageUsesTabColor = False
            TabOrientation = toBottom
            Align = alClient
            Color = 11199487
            TabOrder = 0
            object Page5: TOvcTabPage
              Caption = 'Description'
              TabColor = 11199487
              object DBMemo1: TDBMemo
                Left = 0
                Top = 0
                Width = 471
                Height = 203
                Align = alClient
                DataField = 'prog_note'
                DataSource = BCSPwbD.DataSource2
                ParentColor = True
                ScrollBars = ssVertical
                TabOrder = 0
              end
            end
            object Page6: TOvcTabPage
              Caption = 'Other Info'
              TabColor = 11199487
              object JvLabel1: TJvLabel
                Left = 56
                Top = 76
                Width = 137
                Height = 13
                Alignment = taRightJustify
                AutoSize = False
                Caption = 'Command Line Prarmeters : '
                Transparent = True
                HotTrackFont.Charset = DEFAULT_CHARSET
                HotTrackFont.Color = clWindowText
                HotTrackFont.Height = -11
                HotTrackFont.Name = 'Tahoma'
                HotTrackFont.Style = []
              end
              object JvLabel2: TJvLabel
                Left = 56
                Top = 100
                Width = 137
                Height = 13
                Alignment = taRightJustify
                AutoSize = False
                Caption = 'Initial Directory : '
                Transparent = True
                HotTrackFont.Charset = DEFAULT_CHARSET
                HotTrackFont.Color = clWindowText
                HotTrackFont.Height = -11
                HotTrackFont.Name = 'Tahoma'
                HotTrackFont.Style = []
              end
              object DBEdit1: TDBEdit
                Left = 199
                Top = 76
                Width = 215
                Height = 21
                DataField = 'xqt_params'
                DataSource = BCSPwbD.DataSource2
                ParentColor = True
                TabOrder = 0
              end
              object DBEdit2: TDBEdit
                Left = 199
                Top = 100
                Width = 215
                Height = 21
                DataField = 'start_dir'
                DataSource = BCSPwbD.DataSource2
                ParentColor = True
                TabOrder = 1
              end
            end
          end
        end
      end
    end
    object Page7: TOvcTabPage
      Caption = 'Utilities'
      TabColor = 11199487
      object JvLabel3: TJvLabel
        Left = 199
        Top = 113
        Width = 79
        Height = 13
        Caption = 'Default Browser'
        Transparent = True
        HotTrackFont.Charset = DEFAULT_CHARSET
        HotTrackFont.Color = clWindowText
        HotTrackFont.Height = -11
        HotTrackFont.Name = 'Tahoma'
        HotTrackFont.Style = []
      end
      object SpeedButton1: TSpeedButton
        Left = 191
        Top = 159
        Width = 98
        Height = 22
        Caption = 'Change Browser'
        OnClick = SpeedButton1Click
      end
      object JvEdit1: TJvEdit
        Left = 34
        Top = 132
        Width = 409
        Height = 21
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
        Text = 'C:\Program Files\Internet Explorer\iexplore.exe'
      end
      object JvEdit2: TJvEdit
        Left = 34
        Top = 86
        Width = 409
        Height = 21
        TabOrder = 1
        Visible = False
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 424
    Top = 8
    object PrimaryOptoins1: TMenuItem
      Caption = 'Primary Options'
      object Reports1: TMenuItem
        Caption = 'Reports'
        object ListAllExecutables1: TMenuItem
          Caption = 'List All Executables'
          OnClick = ListAllExecutables1Click
        end
      end
      object OpenFileInNotepad1: TMenuItem
        Caption = 'Open File In Notepad'
        OnClick = OpenFileInNotepad1Click
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
    Left = 424
    Top = 56
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 424
    Top = 104
  end
  object PopupMenu1: TPopupMenu
    Left = 248
    object ExecuteThisOne1: TMenuItem
      Caption = 'Execute This One'
      OnClick = ExecuteThisOne1Click
    end
    object LoadCommand1: TMenuItem
      Caption = 'Load Command'
      OnClick = LoadCommand1Click
    end
    object PasteFromClipboard1: TMenuItem
      Caption = 'Paste From Clipboard'
      OnClick = PasteFromClipboard1Click
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'Executables'
        FileMask = '*.exe'
      end
      item
        DisplayName = 'Batch Files'
        FileMask = '*.bat'
      end
      item
        DisplayName = 'Com Executables'
        FileMask = '*.com'
      end>
    Options = []
    Title = 'Select Executable File Now!'
    Left = 336
    Top = 8
  end
  object RvSystem1: TRvSystem
    TitleSetup = 'Output Options'
    TitleStatus = 'Report Status'
    TitlePreview = 'Report Preview'
    SystemFiler.StatusFormat = 'Generating page %p'
    SystemPreview.ZoomFactor = 100.000000000000000000
    SystemPrinter.ScaleX = 100.000000000000000000
    SystemPrinter.ScaleY = 100.000000000000000000
    SystemPrinter.StatusFormat = 'Printing page %p'
    SystemPrinter.Title = 'Rave Report'
    SystemPrinter.UnitsFactor = 1.000000000000000000
    Left = 104
    Top = 32
  end
  object RvProject1: TRvProject
    Engine = RvSystem1
    ProjectFile = 'C:\dev\w32\2010\BCSPwb\Project1.rav'
    Left = 176
    Top = 8
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Left = 80
    Top = 88
  end
end
