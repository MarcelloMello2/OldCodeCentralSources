object DFMSearchForm: TDFMSearchForm
  Left = 264
  Top = 190
  Width = 800
  Height = 600
  Caption = 'DFM Search'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    792
    566)
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 38
    Top = 15
    Width = 29
    Height = 14
    Alignment = taRightJustify
    Caption = 'Path'
  end
  object Label2: TLabel
    Left = 48
    Top = 100
    Width = 70
    Height = 14
    Alignment = taRightJustify
    Caption = 'ClassName'
  end
  object Label3: TLabel
    Left = 82
    Top = 160
    Width = 35
    Height = 14
    Alignment = taRightJustify
    Caption = 'Value'
  end
  object FileLabel: TLabel
    Left = 5
    Top = 544
    Width = 4
    Height = 14
    Anchors = [akLeft, akBottom]
  end
  object Label5: TLabel
    Left = 52
    Top = 130
    Width = 65
    Height = 14
    Alignment = taRightJustify
    Caption = 'PropName'
  end
  object Label6: TLabel
    Left = 9
    Top = 70
    Width = 109
    Height = 14
    Alignment = taRightJustify
    Caption = 'ComponentName'
  end
  object SearchButton: TButton
    Left = 628
    Top = 531
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Search'
    TabOrder = 6
    OnClick = SearchButtonClick
  end
  object CancelButton: TButton
    Left = 708
    Top = 531
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 7
    OnClick = CancelButtonClick
  end
  object PathEdit: TEdit
    Left = 75
    Top = 10
    Width = 321
    Height = 22
    TabOrder = 0
    Text = 'C:\'
  end
  object SubDirsCheckBox: TCheckBox
    Left = 75
    Top = 40
    Width = 176
    Height = 17
    Caption = 'Include Sub-Directories'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object ClassNameEdit: TEdit
    Left = 275
    Top = 95
    Width = 246
    Height = 22
    TabOrder = 3
  end
  object ValueEdit: TEdit
    Left = 275
    Top = 155
    Width = 246
    Height = 22
    TabOrder = 5
  end
  object PropNameEdit: TEdit
    Left = 275
    Top = 125
    Width = 246
    Height = 22
    TabOrder = 4
  end
  object ComponentNameEdit: TEdit
    Left = 275
    Top = 65
    Width = 246
    Height = 22
    TabOrder = 2
  end
  object ComponentNameCompareBox: TComboBox
    Left = 125
    Top = 65
    Width = 145
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 8
  end
  object ClassNameCompareBox: TComboBox
    Left = 125
    Top = 95
    Width = 145
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 9
  end
  object PropNameCompareBox: TComboBox
    Left = 125
    Top = 125
    Width = 145
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 10
  end
  object ValueCompareBox: TComboBox
    Left = 125
    Top = 155
    Width = 145
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 11
  end
  object DBGrid1: TDBGrid
    Left = 5
    Top = 190
    Width = 782
    Height = 335
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
    TabOrder = 12
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = 'Verdana'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'File'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ComponentName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'ComponentClassName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'PropName'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'Value'
        Visible = True
      end>
  end
  object SearchFiles: TJvSearchFiles
    FileParams.SearchTypes = [stFileMask]
    FileParams.FileMasks.Strings = (
      '*.dfm')
    Left = 5
    Top = 5
  end
  object ResultDataSet: TJvMemoryData
    Active = True
    FieldDefs = <
      item
        Name = 'File'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'ComponentName'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'ComponentClassName'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'PropName'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'Value'
        DataType = ftString
        Size = 255
      end>
    Left = 5
    Top = 35
    object ResultDataSetFile: TStringField
      DisplayWidth = 30
      FieldName = 'File'
      Size = 255
    end
    object ResultDataSetComponentName: TStringField
      DisplayWidth = 30
      FieldName = 'ComponentName'
      Size = 255
    end
    object ResultDataSetComponentClassName: TStringField
      DisplayWidth = 30
      FieldName = 'ComponentClassName'
      Size = 255
    end
    object ResultDataSetPropName: TStringField
      DisplayWidth = 30
      FieldName = 'PropName'
      Size = 255
    end
    object ResultDataSetValue: TStringField
      DisplayWidth = 30
      FieldName = 'Value'
      Size = 255
    end
  end
  object DataSource1: TDataSource
    DataSet = ResultDataSet
    Left = 35
    Top = 35
  end
end
