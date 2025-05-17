{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

library pgXmlDocViewer30;

{%DelphiDotNetAssemblyCompiler 'c:\program files\borland\bds\3.0\bin\Borland.Studio.ToolsAPI.dll'}
{$R 'uWinFormManageXSLs.TWinFormManageXSLs.resources' 'uWinFormManageXSLs.resx'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Data.dll'}
{%DelphiDotNetAssemblyCompiler '$(SystemRoot)\microsoft.net\framework\v1.1.4322\System.Windows.Forms.dll'}
{%DelphiDotNetAssemblyCompiler 'comimports\Interop.SHDocVw.dll'}
{%DelphiDotNetAssemblyCompiler 'comimports\AxInterop.SHDocVw.dll'}
{%DelphiDotNetAssemblyCompiler 'c:\program files\borland\bds\3.0\bin\Borland.mshtml.dll'}
{%File 'ModelSupport\uBaseOTAMenuWizard\uBaseOTAMenuWizard.txvpck'}
{%File 'ModelSupport\uXmlDocViewerWizard\uXmlDocViewerWizard.txvpck'}
{%File 'ModelSupport\uBaseOTA\uBaseOTA.txvpck'}
{%File 'ModelSupport\uWinFormXmlDocViewer\uWinFormXmlDocViewer.txvpck'}
{%File 'ModelSupport\uXmlDocCfg\uXmlDocCfg.txvpck'}
{%File 'ModelSupport\uWinFormManageXSLs\uWinFormManageXSLs.txvpck'}
{%File 'ModelSupport\uBaseOTAWizard\uBaseOTAWizard.txvpck'}
{%File 'ModelSupport\default.txvpck'}
{$R 'uWinFormXmlXsls.TWinFormXmlXsls.resources' 'uWinFormXmlXsls.resx'}


uses
  SysUtils,
  Classes,
  System.Reflection,
  System.Runtime.InteropServices,
  uBaseOTAWizard in 'uBaseOTAWizard.pas',
  uBaseOTAMenuWizard in 'uBaseOTAMenuWizard.pas',
  uWinFormManageXSLs in 'uWinFormManageXSLs.pas' {uWinFormManageXSLs.TWinFormManageXSLs: System.Windows.Forms.Form},
  uXmlDocViewerWizard in 'uXmlDocViewerWizard.pas',
  uBaseOTA in 'uBaseOTA.pas',
  uWinFormXmlXsls in 'uWinFormXmlXsls.pas' {uWinFormXmlXsls.TWinFormXmlXsls};

[assembly: AssemblyTitle('BDS 3.0 XML Documentation Viewer')]
[assembly: AssemblyDescription('')]
[assembly: AssemblyConfiguration('')]
[assembly: AssemblyCompany('Paweł Głowacki [Borland Services]')]
[assembly: AssemblyProduct('XML Doc Viewer 3.0')]
[assembly: AssemblyCopyright('')]
[assembly: AssemblyTrademark('')]
[assembly: AssemblyCulture('')]

//
// Version information for an assembly consists of the following four values:
//
//      Major Version
//      Minor Version 
//      Build Number
//      Revision
//
// You can specify all the values or you can default the Revision and Build Numbers 
// by using the '*' as shown below:

[assembly: AssemblyVersion('3.1.*')]

//
// In order to sign your assembly you must specify a key to use. Refer to the 
// Microsoft .NET Framework documentation for more information on assembly signing.
//
// Use the attributes below to control which key is used for signing. 
//
// Notes: 
//   (*) If no key is specified, the assembly is not signed.
//   (*) KeyName refers to a key that has been installed in the Crypto Service
//       Provider (CSP) on your machine. KeyFile refers to a file which contains
//       a key.
//   (*) If the KeyFile and the KeyName values are both specified, the 
//       following processing occurs:
//       (1) If the KeyName can be found in the CSP, that key is used.
//       (2) If the KeyName does not exist and the KeyFile does exist, the key 
//           in the KeyFile is installed into the CSP and used.
//   (*) In order to create a KeyFile, you can use the sn.exe (Strong Name) utility.
//       When specifying the KeyFile, the location of the KeyFile should be
//       relative to the project output directory. For example, if your KeyFile is
//       located in the project directory, you would specify the AssemblyKeyFile 
//       attribute as [assembly: AssemblyKeyFile('mykey.snk')], provided your output
//       directory is the project directory (the default).
//   (*) Delay Signing is an advanced option - see the Microsoft .NET Framework
//       documentation for more information on this.
//
[assembly: AssemblyDelaySign(false)]
[assembly: AssemblyKeyFile('')]
[assembly: AssemblyKeyName('')]

//
// Use the attributes below to control the COM visibility of your assembly. By
// default the entire assembly is visible to COM. Setting ComVisible to false
// is the recommended default for your assembly. To then expose a class and interface
// to COM set ComVisible to true on each one. It is also recommended to add a
// Guid attribute.
//

[assembly: ComVisible(False)]
//[assembly: Guid('')]
//[assembly: TypeLibVersion(1, 0)]



begin
end.
