unit AppLogEvents_TLB;

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
// Type Lib: E:\R&D\LCE Logger\Event\AppLogEvents.dll (1)
// LIBID: {28EB1BC6-F5A4-4FBC-81B3-69929317CCAD}
// LCID: 0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\stdole2.tlb)
// Parent TypeLibrary:
//   (0) v1.0 AppLogFile, (E:\R&D\LCE Logger\SubFileLog\AppLogFile.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}

interface

uses Windows, ActiveX, Classes, Graphics, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  AppLogEventsMajorVersion = 1;
  AppLogEventsMinorVersion = 0;

  LIBID_AppLogEvents: TGUID = '{28EB1BC6-F5A4-4FBC-81B3-69929317CCAD}';

  IID_IAppLog: TGUID = '{14B04119-12EA-4142-A088-4B4DB7455CB4}';
  CLASS_AppLog: TGUID = '{D6A1FB6B-1A7D-4085-B632-1F8D00F91C48}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum eMsgKind
type
  eMsgKind = TOleEnum;
const
  emkError = $00000000;
  emkWarning = $00000001;
  emkInfo = $00000002;
  emkAuditOk = $00000003;
  emkAuditErr = $00000004;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IAppLog = interface;
  IAppLogDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  AppLog = IAppLog;


// *********************************************************************//
// Interface: IAppLog
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {14B04119-12EA-4142-A088-4B4DB7455CB4}
// *********************************************************************//
  IAppLog = interface(IDispatch)
    ['{14B04119-12EA-4142-A088-4B4DB7455CB4}']
    procedure Log(const App: WideString; const Msg: WideString; Kind: eMsgKind); safecall;
  end;

// *********************************************************************//
// DispIntf:  IAppLogDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {14B04119-12EA-4142-A088-4B4DB7455CB4}
// *********************************************************************//
  IAppLogDisp = dispinterface
    ['{14B04119-12EA-4142-A088-4B4DB7455CB4}']
    procedure Log(const App: WideString; const Msg: WideString; Kind: eMsgKind); dispid 1;
  end;

// *********************************************************************//
// The Class CoAppLog provides a Create and CreateRemote method to          
// create instances of the default interface IAppLog exposed by              
// the CoClass AppLog. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoAppLog = class
    class function Create: IAppLog;
    class function CreateRemote(const MachineName: string): IAppLog;
  end;

implementation

uses ComObj;

class function CoAppLog.Create: IAppLog;
begin
  Result := CreateComObject(CLASS_AppLog) as IAppLog;
end;

class function CoAppLog.CreateRemote(const MachineName: string): IAppLog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_AppLog) as IAppLog;
end;

end.
