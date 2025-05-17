program ThreadingDemo;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  AsyncTasksU in 'AsyncTasksU.pas',
  CommonTypesU in 'CommonTypesU.pas',
  CrossThreadMessengerU in 'CrossThreadMessengerU.pas',
  DebugHelpers in 'DebugHelpers.pas',
  ExecutorIntfU in 'ExecutorIntfU.pas',
  InterlockedOpsU in 'InterlockedOpsU.pas',
  Win64CompatU in 'Win64CompatU.pas',
  NotificationDispatcherU in 'NotificationDispatcherU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
