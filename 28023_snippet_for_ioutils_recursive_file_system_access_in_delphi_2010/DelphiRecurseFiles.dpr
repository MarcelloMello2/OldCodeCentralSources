program DelphiRecurseFiles;

uses
  Forms,
  DelphiRecurseFiles_fmMain in 'DelphiRecurseFiles_fmMain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
