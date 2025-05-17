{*-----------------------------------------------------------------------------
 Program Name: BCSFiorml
 Date:      25-May-2015
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}

program BCSFiorml;

uses
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes,
  BCSFioru in 'BCSFioru.pas',
  frm001u in 'frm001u.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amethyst Kamri');
  rpFior.Caption := 'BCS Direct Access IO';
  BCSFiorc.frmShowModal;
  Application.Run;
  Application.Terminate;
end.
