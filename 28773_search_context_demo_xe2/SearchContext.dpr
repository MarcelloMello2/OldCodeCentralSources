program SearchContext;

uses
  FMX.Forms,
  formMain in 'formMain.pas' {frmMain},
  unitSearchMenuHelper in 'unitSearchMenuHelper.pas',
  formData in 'formData.pas' {frmManageData},
  dataData in 'dataData.pas' {dtmdlData: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdtmdlData, dtmdlData);
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
