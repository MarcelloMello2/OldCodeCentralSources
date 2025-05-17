object BCSPwbD: TBCSPwbD
  OldCreateOrder = False
  Height = 238
  Width = 411
  object ADOConnection1: TADOConnection
    LoginPrompt = False
    Left = 40
    Top = 16
  end
  object ADOTable1: TADOTable
    Connection = ADOConnection1
    IndexFieldNames = 'category'
    Left = 40
    Top = 120
  end
  object DataSource1: TDataSource
    DataSet = ADOTable1
    Left = 40
    Top = 64
  end
  object ADOTable2: TADOTable
    Connection = ADOConnection1
    IndexFieldNames = 'category; prog_desc'
    MasterFields = 'category'
    MasterSource = DataSource1
    TableName = 'prog_info'
    Left = 120
    Top = 120
  end
  object DataSource2: TDataSource
    DataSet = ADOTable2
    Left = 120
    Top = 64
  end
  object RvTableConnection1: TRvTableConnection
    RuntimeVisibility = rtDeveloper
    Table = Table1
    Left = 240
    Top = 24
  end
  object RvTableConnection2: TRvTableConnection
    RuntimeVisibility = rtDeveloper
    Table = Table2
    Left = 240
    Top = 80
  end
  object Table1: TTable
    DatabaseName = 'db01'
    IndexFieldNames = 'category'
    TableName = 'cats'
    Left = 344
    Top = 32
  end
  object Table2: TTable
    DatabaseName = 'db01'
    FilterOptions = [foNoPartialCompare]
    IndexFieldNames = 'prog_desc'
    TableName = 'prog_info'
    Left = 344
    Top = 96
  end
  object Database1: TDatabase
    AliasName = 'bcspwb'
    Connected = True
    DatabaseName = 'db01'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 240
    Top = 152
  end
end
