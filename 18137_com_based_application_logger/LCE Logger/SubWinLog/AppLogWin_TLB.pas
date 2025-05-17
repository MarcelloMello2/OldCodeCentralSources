unit AppLogWin_TLB;

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

// PASTLWTR : $Revision:   1.130  $
// File generated on 9/30/2001 5:28:17 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\R&D\LCE Logger\SubWinLog\AppLogWin.tlb (1)
// LIBID: {E7DA8F08-2518-4F59-82EE-BF0ACD6244E6}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
//   (3) v1.0 AppLogEvents, (E:\R&D\LCE Logger\Event\AppLogEvents.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

uses ActiveX, AppLogEvents_TLB, Classes, Graphics, StdVCL, Variants, Windows;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AppLogWinMajorVersion = 1;
  AppLogWinMinorVersion = 0;

  LIBID_AppLogWin: TGUID = '{E7DA8F08-2518-4F59-82EE-BF0ACD6244E6}';

  CLASS_WinLog: TGUID = '{BDF963FE-F718-4E68-AE8F-F1DDE65CDA21}';
type

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WinLog = IAppLog;


// *********************************************************************//
// The Class CoWinLog provides a Create and CreateRemote method to          
// create instances of the default interface IAppLog exposed by              
// the CoClass WinLog. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWinLog = class
    class function Create: IAppLog;
    class function CreateRemote(const MachineName: string): IAppLog;
  end;

implementation

uses ComObj;

class function CoWinLog.Create: IAppLog;
begin
  Result := CreateComObject(CLASS_WinLog) as IAppLog;
end;

class function CoWinLog.CreateRemote(const MachineName: string): IAppLog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WinLog) as IAppLog;
end;

end.
