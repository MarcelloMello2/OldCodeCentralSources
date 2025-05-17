object dmCodeCentral: TdmCodeCentral
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 218
  Top = 114
  Height = 251
  Width = 466
  object mnuCodeCentral: TPopupMenu
    Left = 32
    Top = 16
    object miCodeCentral: TMenuItem
      Caption = 'CodeCentral'
      OnClick = miCodeCentralClick
    end
  end
end
