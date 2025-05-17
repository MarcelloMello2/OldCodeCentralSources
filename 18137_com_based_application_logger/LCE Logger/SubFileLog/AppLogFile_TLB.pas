unit AppLogFile_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.130.1.0.1.0.1.4  $
// File generated on 10/15/2001 8:37:37 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\R&D\LCE Logger\SubFileLog\AppLogFile.tlb (1)
// LIBID: {0B7D2346-224A-4F3D-8BB4-9FCA25639964}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v1.0 AppLogEvents, (E:\R&D\LCE Logger\Event\AppLogEvents.dll)
//   (2) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (3) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

uses Windows, ActiveX, AppLogEvents_TLB, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AppLogFileMajorVersion = 1;
  AppLogFileMinorVersion = 0;

  LIBID_AppLogFile: TGUID = '{0B7D2346-224A-4F3D-8BB4-9FCA25639964}';

  CLASS_FileLog: TGUID = '{C6D201D8-7F38-4C53-95D7-4A62EE9DBACD}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  FileLog = IAppLog;


// *********************************************************************//
// The Class CoFileLog provides a Create and CreateRemote method to          
// create instances of the default interface IAppLog exposed by              
// the CoClass FileLog. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoFileLog = class
    class function Create: IAppLog;
    class function CreateRemote(const MachineName: string): IAppLog;
  end;

implementation

uses ComObj;

class function CoFileLog.Create: IAppLog;
begin
  Result := CreateComObject(CLASS_FileLog) as IAppLog;
end;

class function CoFileLog.CreateRemote(const MachineName: string): IAppLog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FileLog) as IAppLog;
end;

end.
