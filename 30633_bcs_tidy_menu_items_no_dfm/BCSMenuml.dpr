{*-----------------------------------------------------------------------------
 Program Name: BCSMenuml
 Date:      25-May-2015
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}

program BCSMenuml;

uses
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes,
  BCSMenuu in 'BCSMenuu.pas',
  frm001u in 'frm001u.pas',
  frm016u in 'frm016u.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amethyst Kamri');
  rp0.Caption := 'BCS Tidy Menu Source Code';
  BCSMenuc.frmShowModal;
  Application.Run;
  Application.Terminate;
end.
