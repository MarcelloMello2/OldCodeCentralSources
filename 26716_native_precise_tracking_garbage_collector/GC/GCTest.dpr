program GCTest;

uses
  Forms,
  Main in 'Main.pas' {frmMain},
  stGC in 'stGC.pas',
  SampleGCObjects in 'SampleGCObjects.pas',
  SampleGCThread in 'SampleGCThread.pas',
  stGCFieldFinder in 'stGCFieldFinder.pas',
  SampleGCObjects2 in 'SampleGCObjects2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
