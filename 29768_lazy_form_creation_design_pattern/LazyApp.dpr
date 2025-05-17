program LazyApp;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  uFormSplash in 'uFormSplash.pas' {FormSplash},
  uFormMain in 'uFormMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSplash, FormSplash);
  Application.Run;
end.
