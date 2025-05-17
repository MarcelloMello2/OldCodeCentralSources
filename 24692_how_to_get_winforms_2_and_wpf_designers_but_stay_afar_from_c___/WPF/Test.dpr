program Test;

{$R *.res}

uses
  System.Windows,
  FormsDsg,
  AssemblyInfo in 'AssemblyInfo.pas',
  WPFUtils in 'WPFUtils.pas',
  Form1 in 'Form1.pas',
  Form2 in 'Form2.pas';

[STAThread]
begin
  with System.Windows.Application.Create do begin
    EnableVisualStyle(Aero);
    Run(TForm1.Create.WPFObject);
  end;
end.

