{*-----------------------------------------------------------------------------
 Program Name: BCSCpml
 Date:      25-May-2015
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}

program BCSCpml;

uses
  Vcl.Forms,
  Vcl.Styles,
  Vcl.Themes,
  BCSCpu in 'BCSCpu.pas',
  frm001u in 'frm001u.pas',
  frm019u in 'frm019u.pas',
  frm022u in 'frm022u.pas',
  frm016u in 'frm016u.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amethyst Kamri');
  rpCSCp.Caption := 'BCS Static Form No Data';
  rp019.Caption := 'BCS Compile Delphi Project';
  rp019.Database := 'dcp';
  rp019.UsrId := 'bcs';
  rp019.Password := 'Peace007';
  rp019.MasTab := 'dp';
  rp019.MasIdx := 'sdes';
  frm019c.ZapDge;
  frm019c.AddCol('id', 'Rec No', 50, True);
  frm019c.AddCol('sdes', 'Description', 250, false);
  frm019c.AddCol('proj', 'Project', 200, false);
  frm019c.frmShowModal;
  //BCSCpc.frmShowModal;
  Application.Run;
  Application.Terminate;
end.
