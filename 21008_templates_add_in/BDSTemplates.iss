;------------------------------------------------------------------------------
;  File name:      BDSTemplates.iss
;  Last updated:   4/11/04
;  Author:         Sergey Mishkovskiy
;  Company:        USysWare, Inc.
;  Contact info:   usysware@comcast.net
;
;  Compatibility:  Inno Setup 4.2.0 or higher with ISPP is required
;                  (aka QuickStart Pack).
;
;  Description:    Setup script for Templates add-in with options to install
;                  several templates - Windows Service Application, 
;                  OTA Menu Project and Design Patterns classes and demos. 
;
;                  Visit http://www.jrsoftware.org to download a free copy
;                  of Inno Setup.
;------------------------------------------------------------------------------

#define APP_VER "1.0"
#define APP_VER_LONG GetFileVersion(AddBackslash(SourcePath) + "BDSTemplates.dll")
#define APP_NAME "BDSTemplates"
#define APP_NAME_LONG "Templates add-in"
#define APP_NAME_LONG_DELPHI "Borland Delphi for .NET Templates add-in"
#define APP_URL "http://www.usysware.com"
#define APP_KEY "Software\USysWare"
#define DELPHI "Borland Delphi for .NET"
#define DELPHI_KEY "Software\Borland\BDS\2.0"

[Setup]
AlwaysShowGroupOnReadyPage=yes
AppId={#APP_NAME}{#APP_VER}
AppName={#APP_NAME_LONG_DELPHI}
AppVerName={#APP_NAME_LONG_DELPHI} {#APP_VER_LONG}
AppPublisher=USysWare, Inc.
AppCopyright=Copyright © 2003-2004 USysWare, Inc.
AppVersion={#APP_VER_LONG}
AppPublisherURL={#APP_URL}
AppSupportURL={#APP_URL}/support.htm
AppUpdatesURL={#APP_URL}/downloads.htm
AppMutex=USysWare{#APP_NAME}{#APP_VER}
Compression=lzma/normal
DefaultDirName={pf}\{#APP_NAME}
DirExistsWarning=no
DisableProgramGroupPage=yes
InfoBeforeFile=BDSTemplates ReadMe.txt
LicenseFile=BDSTemplates License.txt
OutputBaseFilename=BDSTemplatesSetup
OutputDir=.
PrivilegesRequired=poweruser
UninstallDisplayIcon={app}\BDSTemplates.dll
UninstallDisplayName={#APP_NAME_LONG_DELPHI} {#APP_VER_LONG}
VersionInfoVersion={#APP_VER_LONG}

[Tasks]
Name: WinService; Description: Windows Service Application Template; GroupDescription: Templates:
Name: OTAMenuProject; Description: OTA Menu Project Template; GroupDescription: Templates:
Name: DesignPatterns; Description: Design Patterns Templates and Demos; GroupDescription: Templates:

[Files]
Source: BDSTemplates.dll; DestDir: {app}; Flags: ignoreversion
Source: readme.txt; DestDir: {app}
; WinService
Source: WinService.xml; DestDir: {app}; Tasks: WinService
Source: WinService\WinServiceTemplate.dpr; DestDir: {app}\WinService; Tasks: WinService
Source: WinService\ProjectInstallerTemplate.pas; DestDir: {app}\WinService; Tasks: WinService
Source: WinService\ServiceTemplate.pas; DestDir: {app}\WinService; Tasks: WinService
Source: WinService\WinService.ico; DestDir: {app}\WinService; Tasks: WinService
; OTAMenuProject
Source: OTAMenuProject.xml; DestDir: {app}; Tasks: OTAMenuProject
Source: OTAMenuProject\MenuProjectTemplate.dpr; DestDir: {app}\OTAMenuProject; Tasks: OTAMenuProject
Source: OTAMenuProject\MenuProjectMainTemplate.pas; DestDir: {app}\OTAMenuProject; Tasks: OTAMenuProject
Source: OTAMenuProject\BDSOTAUtilsTemplate.pas; DestDir: {app}\OTAMenuProject; Tasks: OTAMenuProject
; DesignPatterns
Source: DesignPatterns.xml; DestDir: {app}; Tasks: DesignPatterns
Source: DesignPatterns\SingletonClassTemplate.pas; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\SafeSingletonClassTemplate.pas; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\ObserverInterfaceTemplate.pas; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\ObserverServerTemplate.pas; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\ObserverClientTemplate.pas; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\SingletonTemplate.dpr; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns
Source: DesignPatterns\ObserverTemplate.dpr; DestDir: {app}\DesignPatterns; Tasks: DesignPatterns

[Registry]
Root: HKCU; Subkey: {#APP_KEY}; Flags: uninsdeletekeyifempty
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}; Flags: uninsdeletekey
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}; Flags: uninsdeletekey
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}; ValueType: string; ValueName: InstallPath; ValueData: {app}
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}\Templates; Flags: uninsdeletekey
; Delphi
Root: HKCU; Subkey: {#DELPHI_KEY}\Known IDE Assemblies; Flags: uninsdeletevalue; ValueType: string; ValueName: {app}\BDSTemplates.dll; ValueData: {#APP_NAME_LONG}
; WinService
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}\Templates; ValueType: string; ValueName: {app}\WinService.xml; ValueData: WinService; Tasks: WinService
; OTAMenuProject
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}\Templates; ValueType: string; ValueName: {app}\OTAMenuProject.xml; ValueData: OTAMenuProject; Tasks: OTAMenuProject
; DesignPatterns
Root: HKCU; Subkey: {#APP_KEY}\{#APP_NAME}\{#APP_VER}\Templates; ValueType: string; ValueName: {app}\DesignPatterns.xml; ValueData: DesignPatterns; Tasks: DesignPatterns

[Messages]
SetupAppTitle=Setup - {#APP_NAME_LONG_DELPHI}
UninstallAppTitle={#APP_NAME_LONG_DELPHI} Uninstall
WelcomeLabel2=This will install [name/ver] on your computer.%n%nIt is recommended that you close {#DELPHI} before continuing.

[Code]
function InitializeSetup: Boolean;
var
  UpdatePack: string;
begin
  Result := RegKeyExists(HKLM, '{#DELPHI_KEY}');
  if not Result then
  	MsgBox('{#DELPHI} must be installed first on your computer.'+#13#10+#13#10+
      'Setup cannot continue.' , mbError, MB_OK)
  else
  begin
    Result := RegKeyExists(HKCU, '{#DELPHI_KEY}');
    if not Result then
  	  MsgBox('{#DELPHI} must be brought up at least once under this user account.'+#13#10+#13#10+
        'Setup cannot continue.' , mbError, MB_OK)
    else
    begin
      Result := RegQueryStringValue(HKLM, '{#DELPHI_KEY}', 'UpdatePackInstalled', UpdatePack);
      if Result then
        Result := (StrToIntDef(UpdatePack, 0) >= 2);
      if not Result then
  	    MsgBox('{#DELPHI} Update Pack 2 or higher must be installed first.'+#13#10+#13#10+
          'Setup cannot continue.' , mbError, MB_OK)
    end;
  end;
end;

function SkipCurPage(CurPage: Integer): Boolean;
begin
 if (CurPage = wpSelectDir) or (CurPage = wpLicense) then
   Result := RegKeyExists(HKLM, 'Software\Microsoft\Windows\CurrentVersion\Uninstall\{#APP_NAME}{#APP_VER}_is1')
 else
   Result := False;
end;

