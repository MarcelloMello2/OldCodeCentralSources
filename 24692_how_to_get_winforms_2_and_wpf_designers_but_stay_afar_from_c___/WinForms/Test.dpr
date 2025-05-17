program Test;

{$R *.res}

uses
  System.Windows.Forms,
  Form2 in 'Form2.pas',
  Form1 in 'Form1.pas',
  AssemblyInfo in 'AssemblyInfo.pas';

begin
  Application.Run(TForm1.Create);
end.

