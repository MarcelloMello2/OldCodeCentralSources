library XmlOtaExpert;

{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Delphi.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.SCI.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Studio.ToolsAPI.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Data.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.XML.dll'}
{$R 'DataView.TViewDataForm.resources' 'DataView.resx'}
{$R 'GenerateClasses.TGenerateClassesForm.resources' 'GenerateClasses.resx'}

uses
  System.Reflection,
  XmlOtaExpertImpl in 'XmlOtaExpertImpl.pas',
  PetrVones.Utils.DataPacket in 'PetrVones.Utils.DataPacket.pas',
  PetrVones.Utils.OpenTools in 'PetrVones.Utils.OpenTools.pas',
  CmdLineTool in 'CmdLineTool.pas',
  DataView in 'DataView.pas' {DataView.TViewDataForm: System.Windows.Forms.Form},
  GenerateClasses in 'GenerateClasses.pas' {GenerateClasses.TGenerateClassesForm: System.Windows.Forms.Form};

[assembly: AssemblyTitle('XML OTA Expert for Delphi 9')]
[assembly: AssemblyDescription('Delphi .NET XML Open Tools API addin')]
[assembly: AssemblyCompany('Petr Vones')]
[assembly: AssemblyProduct('XML OTA Expert')]
[assembly: AssemblyCopyright('')]

[assembly: AssemblyVersion('1.0.*')]

begin
end.
