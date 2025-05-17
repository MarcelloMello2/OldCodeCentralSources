program DelphiAdvD2DGeometries;

uses
  Forms,
  AdvGeometriesForm in 'AdvGeometriesForm.pas' {FormAdvGeometries};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormAdvGeometries, FormAdvGeometries);
  Application.Run;
end.
