program CCForm;

uses
  Forms,
  frmCodeCentralU in 'frmCodeCentralU.pas' {frmCodeCentral};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCodeCentral, frmCodeCentral);
  Application.Run;
end.
