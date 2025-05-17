program SmartSampleEx;

uses
  Forms,
  Settings_Form in 'Settings_Form.pas' {SettingsForm},
  Data_Module in 'Data_Module.pas' {DataModule2: TDataModule},
  Main_Form in 'Main_Form.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataModule2, DataModule2);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.Run;
end.
