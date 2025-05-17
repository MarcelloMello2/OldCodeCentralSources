program EmbededForm;

uses
  Forms,
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  EmbededFormUnit in 'EmbededFormUnit.pas' {FirstEmbededForm},
  DeepEmbededFormUnit in 'DeepEmbededFormUnit.pas' {DeepEmbededForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDeepEmbededForm, DeepEmbededForm);
  Application.CreateForm(TFirstEmbededForm, FirstEmbededForm);
  Application.Run;
end.

