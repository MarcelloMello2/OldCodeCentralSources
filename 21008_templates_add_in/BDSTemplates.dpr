library BDSTemplates;
//------------------------------------------------------------------------------
//  File name:      BDSTemplates.dpr
//  Last updated:   4/11/04
//  Author:         Sergey Mishkovskiy
//  Company:        USysWare, Inc.
//  Contact info:   usysware@comcast.net
//
//  Compatibility:  Borland Delphi for .NET (Update Pack 2 or higher)
//
//  Description:    Borland Delphi for .NET Templates add-in.
//                  It exposes multiple File|New menu items, allowing you
//                  to create template based projects or units. The template
//                  configuration file and location are stored under
//                  HKEY_CURRENT_USER\Software\USysWare\BDSTemplates\1.0\Templates.
//                  Under that key each string value name indicates full path
//                  and file name to the XML configuration file.
//                  The value data indicates the template sub-folder name
//                  located under add-in's folder with all of the project or
//                  unit template files.
//------------------------------------------------------------------------------

{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Drawing.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.XML.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Globalization.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.SCI.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Studio.Host.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Studio.ToolsAPI.dll'}
{%DelphiDotNetAssemblyCompiler '$(BDS)\bin\Borland.Studio.TypeMappings.dll'}

uses
  System.Reflection,
  BDSTemplatesMain in 'BDSTemplatesMain.pas',
  BDSOTAUtils in 'BDSOTAUtils.pas',
  BDSConfig in 'BDSConfig.pas',
  BDSDefaultsForm in 'BDSDefaultsForm.pas' {BDSDefaultsForm.TBDSDefaultsForm: System.Windows.Forms.Form};

[assembly: AssemblyTitle      ('BDS Templates add-in')]
[assembly: AssemblyDescription('Borland Delphi for .NET Templates add-in')]
[assembly: AssemblyCompany    ('USysWare, Inc.')]
[assembly: AssemblyProduct    ('BDSTemplates')]
[assembly: AssemblyCopyright  ('Copyright © 2003-2004 USysWare, Inc.')]
[assembly: AssemblyTrademark  ('USysWare')]
[assembly: AssemblyVersion    ('1.0.0.11')]

begin
end.
