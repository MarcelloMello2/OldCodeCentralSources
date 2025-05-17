{== DebugHelpers ======================================================}
{! <summary>
 Collects some routines for creating more useful debug output.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2000-10-12</para>
<para>Version 1.1 created 2009-07-26, changed docs to XML, checked for
 Unicode issues, changed precondition checks to raise exceptions.</para>
<para>Last modified       2009-07-26</para>
</history>
<remarks>
</remarks>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}
unit DebugHelpers;
interface
uses Windows, Classes, Controls;

{!
<summary>
 WaitResultToString returns a string describing a wait result returned
 from one of the API Wait* or MsgWait* routines.</summary>
<returns>
 a string of the type 'WAIT_FAILED', basically the API constants symbolic
 name.</returns>
<param name="res">
 is the function result to translate</param>
}
function WaitResultToString(res: DWORD): string;
{!
<summary>
 ODS outputs a string to the debug console. This is just a shortcut for
 OutputDebugString. </summary>
}
procedure ODS(const aString: string); overload;
{!
<summary>
 ODS outputs a formatted string to the debug console. Parameters are the
 same as for the Sysutils.Format standard function. </summary>
}
procedure ODS(const formatstring: string;
  const values: array of const); overload;
{!
<summary>
 AspectName returns a string representing an OLE aspect value. </summary>
<returns>
 A string of the form 'DVASPECT_CONTENT', basically the API constants
 symbolic name.</returns>
<param name="fmtAspect">
 is the aspect value to translate.</param>
}
function AspectName(fmtAspect: Integer): string;
{!
<summary>
 TymedName returns a string representing an OLE tymed (media type)
 value. </summary>
<returns>
 A string of the form 'TYMED_GDI', basically the API constants
 symbolic name.</returns>
<param name="fmtTymed">
 is the tymed value to translate.</param>
}
function TymedName(fmtTymed: integer): string;
{!
<summary>
 LogTimestamp returns the current date and time in format
 yyyy-mm-dd hh:nn:ss:zzz with an additional space at the end. Can be used
 for logging purposes. This function is thread-safe. </summary>
}
function LogTimestamp: string;
{!
<summary>
 VarTypeToString returns a string name for a variant type. </summary>
<returns>
 the symbolic name for the variant type, e.g. 'varSingle'. This may be
 prefixed by 'ByRef ' and/or 'array of ' if the passed variant type
 code has the array or reference bits set.</returns>
<param name="aVarType">
 is the variant type to interpret, usually obtained via the VarType
 function.</param>
}
function VarTypeToString(aVarType: Integer): string;
{!
<summary>
 ProcessorArchitecture returns the symbolic name for one of the Windows
 PROCESSOR_ARCHITECTURE_* constants. See the GetSystemInfo API function.
 </summary>
}
function ProcessorArchitecture(w: Word): string;
{!
<summary>
 MessageToText converts a message constant to a display string. We only
 support a subset of all messages here, anything else will just return
 the message constants value as a hexadecimal representation.</summary>
<returns>
 the symbolic name for the message, e.g. 'WM_KEYDOWN'.</returns>
<param name="aMsg">
 is the message. This can be a Windows (WM_) or VCL (CM_, CN_) message.
 </param>
}
function MessageToText(aMsg: Cardinal): string;
{!
<summary>
 ShiftstateToString returns a string showing the content of a TShiftstate
 set. This set type is used by the VCLs OnKey* events.</summary>
<returns>
 a string of the form '[ssShift,ssLeft]'. If the set is empty '[]' is
 returned.</returns>
<param name="Shift">
 is the set to interpret.</param>
}
function ShiftstateToString(Shift: TShiftState): String;
{!
<summary>
 AppCommand extracts an app commands identifier from a message lparam
 value and returns the matching symbolic name.</summary>
<returns>
 a symbolic name like 'APPCOMMAND_BASS_BOOST'.</returns>
<param name="aLparam">
 is the lparam from an WM_APPCOMMAND message</param>
}
function AppCommand(aLparam: LPARAM): string;


implementation

uses Sysutils, ActiveX, SyncObjs, Messages, TypInfo;

function WaitResultToString(res: DWORD): string;
begin
  case res of
    WAIT_FAILED       : Result := 'WAIT_FAILED';
    WAIT_OBJECT_0     : Result := 'WAIT_OBJECT_0';
    WAIT_ABANDONED    : Result := 'WAIT_ABANDONED';
    WAIT_TIMEOUT      : Result := 'WAIT_TIMEOUT';
    WAIT_IO_COMPLETION: Result := 'WAIT_IO_COMPLETION';
  else
    if (res > WAIT_OBJECT_0) and
      (res < (WAIT_OBJECT_0 + MAXIMUM_WAIT_OBJECTS))
    then
      Result := Format('WAIT_OBJECT_0 + %d', [res - WAIT_OBJECT_0])
    else if (res > WAIT_ABANDONED) and
      (res < (WAIT_ABANDONED + MAXIMUM_WAIT_OBJECTS))
    then
      Result := Format('WAIT_ABANDONED_0 + %d', [res - WAIT_ABANDONED])
    else
      Result := Format('unknown wait result: %d', [res]);
  end; { Case }
end; { WaitResultToString }

procedure ODS(const aString: string);
begin
  OutputDebugString(PChar(aString));
end; { ODS }

procedure ODS(const formatstring: string; const values: array of
  const);
begin
  ODS(Format(formatstring, values));
end; { ODS }

function AspectName(fmtAspect: Integer): string;
type
  TInfo = record value: integer; text: string end;
const
  KnownFormats : array [1..4] of TInfo = (
    (value: DVASPECT_CONTENT; text: 'DVASPECT_CONTENT'),
    (value: DVASPECT_THUMBNAIL; text: 'DVASPECT_THUMBNAIL'),
    (value: DVASPECT_ICON; text: 'DVASPECT_ICON'),
    (value: DVASPECT_DOCPRINT; text: 'DVASPECT_DOCPRINT')
  );
var
  LTemp: TStringlist;
  I: Integer;
begin
  LTemp:= TStringlist.Create;
  try
    for I := Low(KnownFormats) to High(KnownFormats) do
      if (fmtAspect and KnownFormats[I].value) <> 0 then
        LTemp.Add(KnownFormats[I].text);
    Result := LTemp.CommaText;
  finally
    LTemp.Free;
  end;
end; { AspectName }

function TymedName(fmtTymed: integer): string;
type
  TInfo = record value: integer; text: string end;
const
  KnownFormats : array [1..8] of TInfo = (
    (value: TYMED_HGLOBAL; text: 'TYMED_HGLOBAL'),
    (value: TYMED_FILE; text: 'TYMED_FILE'),
    (value: TYMED_ISTREAM; text: 'TYMED_ISTREAM'),
    (value: TYMED_ISTORAGE; text: 'TYMED_ISTORAGE'),
    (value: TYMED_GDI; text: 'TYMED_GDI'),
    (value: TYMED_MFPICT; text: 'TYMED_MFPICT'),
    (value: TYMED_ENHMF; text: 'TYMED_ENHMF'),
    (value: TYMED_NULL; text: 'TYMED_NULL')
  );
var
  LTemp: TStringlist;
  I: Integer;
begin
  LTemp:= TStringlist.Create;
  try
    for I := Low(KnownFormats) to High(KnownFormats) do
      if (fmttymed and KnownFormats[I].value) <> 0 then
        LTemp.Add(KnownFormats[I].text);
    Result := LTemp.CommaText;
  finally
    LTemp.Free;
  end;
end; { TymedName }

var
  guardian: TCriticalSection = nil;

function LogTimestamp: string;
begin
  guardian.Acquire;
  try
    Result := FormatDatetime('yyyy-mm-dd hh:nn:ss:zzz ', Now);
  finally
    guardian.Release;
  end; { finally }
end; { LogTimestamp }

type
  TKeyValue = record key: string;
    value: Integer
  end;
const
  VarTypes: array[1..25] of TKeyValue = (
    (Key: 'varEmpty'; Value: $0000),
    (Key: 'varNull'; Value: $0001),
    (Key: 'varSmallint'; Value: $0002),
    (Key: 'varInteger'; Value: $0003),
    (Key: 'varSingle'; Value: $0004),
    (Key: 'varDouble'; Value: $0005),
    (Key: 'varCurrency'; Value: $0006),
    (Key: 'varDate'; Value: $0007),
    (Key: 'varOleStr'; Value: $0008),
    (Key: 'varDispatch'; Value: $0009),
    (Key: 'varError'; Value: $000A),
    (Key: 'varBoolean'; Value: $000B),
    (Key: 'varVariant'; Value: $000C),
    (Key: 'varUnknown'; Value: $000D),
    (Key: 'varDecimal'; Value: $000E),
    (Key: 'varShortInt'; Value: $0010),
    (Key: 'varByte'; Value: $0011),
    (Key: 'varWord'; Value: $0012),
    (Key: 'varLongWord'; Value: $0013),
    (Key: 'varInt64'; Value: $0014),
    (Key: 'varWord64'; Value: $0015),
    (Key: 'varStrArg'; Value: $0048),
    (Key: 'varString'; Value: $0100),
    (Key: 'varAny'; Value: $0101),
    (Key: 'varUString'; Value: $0102)
    );

function VarTypeToString(aVarType: Integer): string;
var
  i: Integer;
begin
  Result := '';
  if (varByRef and aVarType) <> 0 then
    Result := 'ByRef ';
  if (varArray and aVarType) <> 0 then
    Result := Result + 'array of ';
  aVarType := aVarType and varTypeMask;
  for i := Low(VarTypes) to High(VarTypes) do
    if VarTypes[i].value = aVarType then begin
      Result := Result + VarTypes[i].Key;
      Exit;
    end;
  Result := Result + Format('unknown (%4.4x)', [aVarType]);
end;

const
  PROCESSOR_ARCHITECTURE_INTEL = 0;
  PROCESSOR_ARCHITECTURE_MIPS = 1;
  PROCESSOR_ARCHITECTURE_ALPHA = 2;
  PROCESSOR_ARCHITECTURE_PPC = 3;
  PROCESSOR_ARCHITECTURE_SHX = 4;
  PROCESSOR_ARCHITECTURE_ARM = 5;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
  PROCESSOR_ARCHITECTURE_ALPHA64 = 7;
  PROCESSOR_ARCHITECTURE_MSIL = 8;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;

function ProcessorArchitecture(w: Word): string;
begin
  case w of
    PROCESSOR_ARCHITECTURE_INTEL        : Result := 'PROCESSOR_ARCHITECTURE_INTEL';
    PROCESSOR_ARCHITECTURE_MIPS         : Result := 'PROCESSOR_ARCHITECTURE_MIPS';
    PROCESSOR_ARCHITECTURE_ALPHA        : Result := 'PROCESSOR_ARCHITECTURE_ALPHA';
    PROCESSOR_ARCHITECTURE_PPC          : Result := 'PROCESSOR_ARCHITECTURE_PPC';
    PROCESSOR_ARCHITECTURE_SHX          : Result := 'PROCESSOR_ARCHITECTURE_SHX';
    PROCESSOR_ARCHITECTURE_ARM          : Result := 'PROCESSOR_ARCHITECTURE_ARM';
    PROCESSOR_ARCHITECTURE_IA64         : Result := 'PROCESSOR_ARCHITECTURE_IA64';
    PROCESSOR_ARCHITECTURE_ALPHA64      : Result := 'PROCESSOR_ARCHITECTURE_ALPHA64';
    PROCESSOR_ARCHITECTURE_MSIL         : Result := 'PROCESSOR_ARCHITECTURE_MSIL';
    PROCESSOR_ARCHITECTURE_AMD64        : Result := 'PROCESSOR_ARCHITECTURE_AMD64';
    PROCESSOR_ARCHITECTURE_IA32_ON_WIN64: Result := 'PROCESSOR_ARCHITECTURE_IA32_ON_WIN64';
  else
    Result := 'PROCESSOR_ARCHITECTURE_UNKNOWN';
  end;
end;

function MessageToText(aMsg: Cardinal): string;
const
  WM_XBUTTONDOWN  = $020B;
  WM_XBUTTONUP    = $020C;
  WM_XBUTTONDBLCLK= $020D;
begin
  case aMsg of
    WM_COMMAND              : Result := 'WM_COMMAND';
    WM_SYSCOMMAND           : Result := 'WM_SYSCOMMAND';
    WM_KEYDOWN              : Result := 'WM_KEYDOWN';
    WM_SYSKEYDOWN           : Result := 'WM_SYSKEYDOWN';
    WM_CHAR                 : Result := 'WM_CHAR';
    WM_SYSCHAR              : Result := 'WM_SYSCHAR';
    WM_KEYUP                : Result := 'WM_KEYUP';
    WM_SYSKEYUP             : Result := 'WM_SYSKEYUP';
    WM_SYSDEADCHAR          : Result := 'WM_SYSDEADCHAR';
    WM_DEADCHAR             : Result := 'WM_DEADCHAR';
    WM_UNICHAR              : Result := 'WM_UNICHAR';
    WM_NCMOUSEMOVE          : Result := 'WM_NCMOUSEMOVE';
    WM_NCLBUTTONDOWN        : Result := 'WM_NCLBUTTONDOWN';
    WM_NCLBUTTONUP          : Result := 'WM_NCLBUTTONUP';
    WM_NCLBUTTONDBLCLK      : Result := 'WM_NCLBUTTONDBLCLK';
    WM_NCRBUTTONDOWN        : Result := 'WM_NCRBUTTONDOWN';
    WM_NCRBUTTONUP          : Result := 'WM_NCRBUTTONUP';
    WM_NCRBUTTONDBLCLK      : Result := 'WM_NCRBUTTONDBLCLK';
    WM_NCMBUTTONDOWN        : Result := 'WM_NCMBUTTONDOWN';
    WM_NCMBUTTONUP          : Result := 'WM_NCMBUTTONUP';
    WM_NCMBUTTONDBLCLK      : Result := 'WM_NCMBUTTONDBLCLK';
    WM_NCXBUTTONDOWN        : Result := 'WM_NCXBUTTONDOWN';
    WM_NCXBUTTONUP          : Result := 'WM_NCXBUTTONUP';
    WM_NCXBUTTONDBLCLK      : Result := 'WM_NCXBUTTONDBLCLK';
    WM_MOUSEMOVE            : Result := 'WM_MOUSEMOVE';
    WM_LBUTTONDOWN          : Result := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP            : Result := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK        : Result := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN          : Result := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP            : Result := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK        : Result := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN          : Result := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP            : Result := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK        : Result := 'WM_MBUTTONDBLCLK';
    WM_MOUSEWHEEL           : Result := 'WM_MOUSEWHEEL';
    WM_XBUTTONDOWN          : Result := 'WM_XBUTTONDOWN';
    WM_XBUTTONUP            : Result := 'WM_XBUTTONUP';
    WM_XBUTTONDBLCLK        : Result := 'WM_XBUTTONDBLCLK';
    WM_MOUSEHOVER           : Result := 'WM_MOUSEHOVER';
    WM_MOUSELEAVE           : Result := 'WM_MOUSELEAVE';
    WM_NCMOUSEHOVER         : Result := 'WM_NCMOUSEHOVER';
    WM_NCMOUSELEAVE         : Result := 'WM_NCMOUSELEAVE';
    WM_TIMER                : Result := 'WM_TIMER';
    WM_GETDLGCODE           : Result := 'WM_GETDLGCODE';
    WM_PAINT                : Result := 'WM_PAINT';
    WM_NCPAINT              : Result := 'WM_NCPAINT';
    WM_ERASEBKGND           : Result := 'WM_ERASEBKGND';
    WM_KILLFOCUS            : Result := 'WM_KILLFOCUS';
    CM_ACTIVATE             : Result := 'CM_ACTIVATE';
    CM_DEACTIVATE           : Result := 'CM_DEACTIVATE';
    CM_GOTFOCUS             : Result := 'CM_GOTFOCUS';
    CM_LOSTFOCUS            : Result := 'CM_LOSTFOCUS';
    CM_CANCELMODE           : Result := 'CM_CANCELMODE';
    CM_DIALOGKEY            : Result := 'CM_DIALOGKEY';
    CM_DIALOGCHAR           : Result := 'CM_DIALOGCHAR';
    CM_FOCUSCHANGED         : Result := 'CM_FOCUSCHANGED';
    CM_PARENTFONTCHANGED    : Result := 'CM_PARENTFONTCHANGED';
    CM_PARENTCOLORCHANGED   : Result := 'CM_PARENTCOLORCHANGED';
    CM_HITTEST              : Result := 'CM_HITTEST';
    CM_VISIBLECHANGED       : Result := 'CM_VISIBLECHANGED';
    CM_ENABLEDCHANGED       : Result := 'CM_ENABLEDCHANGED';
    CM_COLORCHANGED         : Result := 'CM_COLORCHANGED';
    CM_FONTCHANGED          : Result := 'CM_FONTCHANGED';
    CM_CURSORCHANGED        : Result := 'CM_CURSORCHANGED';
    CM_CTL3DCHANGED         : Result := 'CM_CTL3DCHANGED';
    CM_PARENTCTL3DCHANGED   : Result := 'CM_PARENTCTL3DCHANGED';
    CM_TEXTCHANGED          : Result := 'CM_TEXTCHANGED';
    CM_MOUSEENTER           : Result := 'CM_MOUSEENTER';
    CM_MOUSELEAVE           : Result := 'CM_MOUSELEAVE';
    CM_MENUCHANGED          : Result := 'CM_MENUCHANGED';
    CM_APPKEYDOWN           : Result := 'CM_APPKEYDOWN';
    CM_APPSYSCOMMAND        : Result := 'CM_APPSYSCOMMAND';
    CM_BUTTONPRESSED        : Result := 'CM_BUTTONPRESSED';
    CM_SHOWINGCHANGED       : Result := 'CM_SHOWINGCHANGED';
    CM_ENTER                : Result := 'CM_ENTER';
    CM_EXIT                 : Result := 'CM_EXIT';
    CM_DESIGNHITTEST        : Result := 'CM_DESIGNHITTEST';
    CM_ICONCHANGED          : Result := 'CM_ICONCHANGED';
    CM_WANTSPECIALKEY       : Result := 'CM_WANTSPECIALKEY';
    CM_INVOKEHELP           : Result := 'CM_INVOKEHELP';
    CM_WINDOWHOOK           : Result := 'CM_WINDOWHOOK';
    CM_RELEASE              : Result := 'CM_RELEASE';
    CM_SHOWHINTCHANGED      : Result := 'CM_SHOWHINTCHANGED';
    CM_PARENTSHOWHINTCHANGED: Result := 'CM_PARENTSHOWHINTCHANGED';
    CM_SYSCOLORCHANGE       : Result := 'CM_SYSCOLORCHANGE';
    CM_WININICHANGE         : Result := 'CM_WININICHANGE';
    CM_FONTCHANGE           : Result := 'CM_FONTCHANGE';
    CM_TIMECHANGE           : Result := 'CM_TIMECHANGE';
    CM_TABSTOPCHANGED       : Result := 'CM_TABSTOPCHANGED';
    CM_UIACTIVATE           : Result := 'CM_UIACTIVATE';
    CM_UIDEACTIVATE         : Result := 'CM_UIDEACTIVATE';
    CM_DOCWINDOWACTIVATE    : Result := 'CM_DOCWINDOWACTIVATE';
    CM_CONTROLLISTCHANGE    : Result := 'CM_CONTROLLISTCHANGE';
    CM_GETDATALINK          : Result := 'CM_GETDATALINK';
    CM_CHILDKEY             : Result := 'CM_CHILDKEY';
    CM_DRAG                 : Result := 'CM_DRAG';
    CM_HINTSHOW             : Result := 'CM_HINTSHOW';
    CM_DIALOGHANDLE         : Result := 'CM_DIALOGHANDLE';
    CM_ISTOOLCONTROL        : Result := 'CM_ISTOOLCONTROL';
    CM_RECREATEWND          : Result := 'CM_RECREATEWND';
    CM_INVALIDATE           : Result := 'CM_INVALIDATE';
    CM_SYSFONTCHANGED       : Result := 'CM_SYSFONTCHANGED';
    CM_CONTROLCHANGE        : Result := 'CM_CONTROLCHANGE';
    CM_CHANGED              : Result := 'CM_CHANGED';
    CM_DOCKCLIENT           : Result := 'CM_DOCKCLIENT';
    CM_UNDOCKCLIENT         : Result := 'CM_UNDOCKCLIENT';
    CM_FLOAT                : Result := 'CM_FLOAT';
    CM_BORDERCHANGED        : Result := 'CM_BORDERCHANGED';
    CM_BIDIMODECHANGED      : Result := 'CM_BIDIMODECHANGED';
    CM_PARENTBIDIMODECHANGED: Result := 'CM_PARENTBIDIMODECHANGED';
    CM_ALLCHILDRENFLIPPED   : Result := 'CM_ALLCHILDRENFLIPPED';
    CM_ACTIONUPDATE         : Result := 'CM_ACTIONUPDATE';
    CM_ACTIONEXECUTE        : Result := 'CM_ACTIONEXECUTE';
    CM_HINTSHOWPAUSE        : Result := 'CM_HINTSHOWPAUSE';
    CM_DOCKNOTIFICATION     : Result := 'CM_DOCKNOTIFICATION';
    CM_MOUSEWHEEL           : Result := 'CM_MOUSEWHEEL';
    CM_ISSHORTCUT           : Result := 'CM_ISSHORTCUT';
    CM_INVALIDATEDOCKHOST   : Result := 'CM_INVALIDATEDOCKHOST';
    CM_SETACTIVECONTROL     : Result := 'CM_SETACTIVECONTROL';
    CM_POPUPHWNDDESTROY     : Result := 'CM_POPUPHWNDDESTROY';
    CM_CREATEPOPUP          : Result := 'CM_CREATEPOPUP';
    CM_DESTROYHANDLE        : Result := 'CM_DESTROYHANDLE';
    CM_MOUSEACTIVATE        : Result := 'CM_MOUSEACTIVATE';
    CM_CONTROLLISTCHANGING  : Result := 'CM_CONTROLLISTCHANGING';
    CM_BUFFEREDPRINTCLIENT  : Result := 'CM_BUFFEREDPRINTCLIENT';
    CM_UNTHEMECONTROL       : Result := 'CM_UNTHEMECONTROL';
    CN_CHARTOITEM           : Result := 'CN_CHARTOITEM';
    CN_COMMAND              : Result := 'CN_COMMAND';
    CN_COMPAREITEM          : Result := 'CN_COMPAREITEM';
    CN_CTLCOLORBTN          : Result := 'CN_CTLCOLORBTN';
    CN_CTLCOLORDLG          : Result := 'CN_CTLCOLORDLG';
    CN_CTLCOLOREDIT         : Result := 'CN_CTLCOLOREDIT';
    CN_CTLCOLORLISTBOX      : Result := 'CN_CTLCOLORLISTBOX';
    CN_CTLCOLORMSGBOX       : Result := 'CN_CTLCOLORMSGBOX';
    CN_CTLCOLORSCROLLBAR    : Result := 'CN_CTLCOLORSCROLLBAR';
    CN_CTLCOLORSTATIC       : Result := 'CN_CTLCOLORSTATIC';
    CN_DELETEITEM           : Result := 'CN_DELETEITEM';
    CN_DRAWITEM             : Result := 'CN_DRAWITEM';
    CN_HSCROLL              : Result := 'CN_HSCROLL';
    CN_MEASUREITEM          : Result := 'CN_MEASUREITEM';
    CN_PARENTNOTIFY         : Result := 'CN_PARENTNOTIFY';
    CN_VKEYTOITEM           : Result := 'CN_VKEYTOITEM';
    CN_VSCROLL              : Result := 'CN_VSCROLL';
    CN_KEYDOWN              : Result := 'CN_KEYDOWN';
    CN_KEYUP                : Result := 'CN_KEYUP';
    CN_CHAR                 : Result := 'CN_CHAR';
    CN_SYSKEYDOWN           : Result := 'CN_SYSKEYDOWN';
    CN_SYSCHAR              : Result := 'CN_SYSCHAR';
    CN_NOTIFY               : Result := 'CN_NOTIFY';
  else
    Result := Format('$%4.4X', [aMsg]);
  end; {case}
end;

type
{$IF RtlVersion < 21.0}
  TShifts = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble);
{$ELSE}
  TShifts = (ssShift, ssAlt, ssCtrl, ssLeft, ssRight, ssMiddle, ssDouble, ssTouch, ssPen);
{$IFEND}

  TShiftSet = set of TShifts;

function ShiftstateToString( Shift: TShiftState ): String;
var
  state: TShifts;
  sl: TStringlist;
begin
  sl:= Tstringlist.create;
  Try
    For state := Low( state ) to HIgh(state) Do
      If state In TShiftSet( shift ) Then
        sl.Add(GetEnumname( Typeinfo( TShifts ), Ord( state )));
    result := '['+sl.CommaText+']';
  Finally
    sl.free;
  End; { Finally }
end;

{ If your Delphi version chokes on the symbols used in the AppCommand
 function, uncomment the following section. The constants are from
 the Delphi 2009 Windows unit.}
{
  APPCOMMAND_BROWSER_BACKWARD       = 1;
  APPCOMMAND_BROWSER_FORWARD  = 2;
  APPCOMMAND_BROWSER_REFRESH        = 3;
  APPCOMMAND_BROWSER_STOP           = 4;
  APPCOMMAND_BROWSER_SEARCH         = 5;
  APPCOMMAND_BROWSER_FAVORITES      = 6;
  APPCOMMAND_BROWSER_HOME           = 7;
  APPCOMMAND_VOLUME_MUTE            = 8;
  APPCOMMAND_VOLUME_DOWN            = 9;
  APPCOMMAND_VOLUME_UP              = 10;
  APPCOMMAND_MEDIA_NEXTTRACK        = 11;
  APPCOMMAND_MEDIA_PREVIOUSTRACK    = 12;
  APPCOMMAND_MEDIA_STOP             = 13;
  APPCOMMAND_MEDIA_PLAY_PAUSE       = 14;
  APPCOMMAND_LAUNCH_MAIL            = 15;
  APPCOMMAND_LAUNCH_MEDIA_SELECT    = 16;
  APPCOMMAND_LAUNCH_APP1            = 17;
  APPCOMMAND_LAUNCH_APP2            = 18;
  APPCOMMAND_BASS_DOWN              = 19;
  APPCOMMAND_BASS_BOOST             = 20;
  APPCOMMAND_BASS_UP                = 21;
  APPCOMMAND_TREBLE_DOWN            = 22;
  APPCOMMAND_TREBLE_UP              = 23;
  APPCOMMAND_MICROPHONE_VOLUME_MUTE = 24;
  APPCOMMAND_MICROPHONE_VOLUME_DOWN = 25;
  APPCOMMAND_MICROPHONE_VOLUME_UP   = 26;
  APPCOMMAND_HELP                   = 27;
  APPCOMMAND_FIND                   = 28;
  APPCOMMAND_NEW                    = 29;
  APPCOMMAND_OPEN                   = 30;
  APPCOMMAND_CLOSE                  = 31;
  APPCOMMAND_SAVE                   = 32;
  APPCOMMAND_PRINT                  = 33;
  APPCOMMAND_UNDO                   = 34;
  APPCOMMAND_REDO                   = 35;
  APPCOMMAND_COPY                   = 36;
  APPCOMMAND_CUT                    = 37;
  APPCOMMAND_PASTE                  = 38;
  APPCOMMAND_REPLY_TO_MAIL          = 39;
  APPCOMMAND_FORWARD_MAIL           = 40;
  APPCOMMAND_SEND_MAIL              = 41;
  APPCOMMAND_SPELL_CHECK            = 42;
  APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE    = 43;
  APPCOMMAND_MIC_ON_OFF_TOGGLE      = 44;
  APPCOMMAND_CORRECTION_LIST        = 45;
  APPCOMMAND_MEDIA_PLAY             = 46;
  APPCOMMAND_MEDIA_PAUSE            = 47;
  APPCOMMAND_MEDIA_RECORD           = 48;
  APPCOMMAND_MEDIA_FAST_FORWARD     = 49;
  APPCOMMAND_MEDIA_REWIND           = 50;
  APPCOMMAND_MEDIA_CHANNEL_UP       = 51;
  APPCOMMAND_MEDIA_CHANNEL_DOWN     = 52;
  APPCOMMAND_DELETE                 = 53;
  APPCOMMAND_DWM_FLIP3D             = 54;
  FAPPCOMMAND_MOUSE = $8000;
  FAPPCOMMAND_KEY   = 0;
  FAPPCOMMAND_OEM   = $1000;
  FAPPCOMMAND_MASK  = $F000;
function GET_APPCOMMAND_LPARAM(const lParam: LongInt): Shortint;
begin
  Result := LongRec(lParam).Hi and not FAPPCOMMAND_MASK;
end;
}
function AppCommand(aLparam: LPARAM): string;
begin
  case GET_APPCOMMAND_LPARAM(aLParam) of
    APPCOMMAND_BASS_BOOST            : Result:= 'APPCOMMAND_BASS_BOOST';
    APPCOMMAND_BASS_DOWN             : Result:= 'APPCOMMAND_BASS_DOWN';
    APPCOMMAND_BASS_UP               : Result:= 'APPCOMMAND_BASS_UP';
    APPCOMMAND_BROWSER_BACKWARD      : Result:= 'APPCOMMAND_BROWSER_BACKWARD';
    APPCOMMAND_BROWSER_FAVORITES     : Result:= 'APPCOMMAND_BROWSER_FAVORITES';
    APPCOMMAND_BROWSER_FORWARD       : Result:= 'APPCOMMAND_BROWSER_FORWARD';
    APPCOMMAND_BROWSER_HOME          : Result:= 'APPCOMMAND_BROWSER_HOME';
    APPCOMMAND_BROWSER_REFRESH       : Result:= 'APPCOMMAND_BROWSER_REFRESH';
    APPCOMMAND_BROWSER_SEARCH        : Result:= 'APPCOMMAND_BROWSER_SEARCH';
    APPCOMMAND_BROWSER_STOP          : Result:= 'APPCOMMAND_BROWSER_STOP';
    APPCOMMAND_CLOSE                 : Result:= 'APPCOMMAND_CLOSE';
    APPCOMMAND_COPY                  : Result:= 'APPCOMMAND_COPY';
    APPCOMMAND_CORRECTION_LIST       : Result:= 'APPCOMMAND_CORRECTION_LIST';
    APPCOMMAND_CUT                   : Result:= 'APPCOMMAND_CUT';
    APPCOMMAND_DELETE                : Result:= 'APPCOMMAND_DELETE';
    APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE:
                                       Result:= 'APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE';
    APPCOMMAND_FIND                  : Result:= 'APPCOMMAND_FIND';
    APPCOMMAND_FORWARD_MAIL          : Result:= 'APPCOMMAND_FORWARD_MAIL';
    APPCOMMAND_HELP                  : Result:= 'APPCOMMAND_HELP';
    APPCOMMAND_LAUNCH_APP1           : Result:= 'APPCOMMAND_LAUNCH_APP1';
    APPCOMMAND_LAUNCH_APP2           : Result:= 'APPCOMMAND_LAUNCH_APP2';
    APPCOMMAND_LAUNCH_MAIL           : Result:= 'APPCOMMAND_LAUNCH_MAIL';
    APPCOMMAND_LAUNCH_MEDIA_SELECT   : Result:= 'APPCOMMAND_LAUNCH_MEDIA_SELECT';
    APPCOMMAND_MEDIA_CHANNEL_DOWN    : Result:= 'APPCOMMAND_MEDIA_CHANNEL_DOWN';
    APPCOMMAND_MEDIA_CHANNEL_UP      : Result:= 'APPCOMMAND_MEDIA_CHANNEL_UP';
    APPCOMMAND_MEDIA_FAST_FORWARD    : Result:= 'APPCOMMAND_MEDIA_FAST_FORWARD';
    APPCOMMAND_MEDIA_NEXTTRACK       : Result:= 'APPCOMMAND_MEDIA_NEXTTRACK';
    APPCOMMAND_MEDIA_PAUSE           : Result:= 'APPCOMMAND_MEDIA_PAUSE';
    APPCOMMAND_MEDIA_PLAY            : Result:= 'APPCOMMAND_MEDIA_PLAY';
    APPCOMMAND_MEDIA_PLAY_PAUSE      : Result:= 'APPCOMMAND_MEDIA_PLAY_PAUSE';
    APPCOMMAND_MEDIA_PREVIOUSTRACK   : Result:= 'APPCOMMAND_MEDIA_PREVIOUSTRACK';
    APPCOMMAND_MEDIA_RECORD          : Result:= 'APPCOMMAND_MEDIA_RECORD';
    APPCOMMAND_MEDIA_REWIND          : Result:= 'APPCOMMAND_MEDIA_REWIND';
    APPCOMMAND_MEDIA_STOP            : Result:= 'APPCOMMAND_MEDIA_STOP';
    APPCOMMAND_MIC_ON_OFF_TOGGLE     : Result:= 'APPCOMMAND_MIC_ON_OFF_TOGGLE';
    APPCOMMAND_MICROPHONE_VOLUME_DOWN: Result:= 'APPCOMMAND_MICROPHONE_VOLUME_DOWN';
    APPCOMMAND_MICROPHONE_VOLUME_MUTE: Result:= 'APPCOMMAND_MICROPHONE_VOLUME_MUTE';
    APPCOMMAND_MICROPHONE_VOLUME_UP  : Result:= 'APPCOMMAND_MICROPHONE_VOLUME_UP';
    APPCOMMAND_NEW                   : Result:= 'APPCOMMAND_NEW';
    APPCOMMAND_OPEN                  : Result:= 'APPCOMMAND_OPEN';
    APPCOMMAND_PASTE                 : Result:= 'APPCOMMAND_PASTE';
    APPCOMMAND_PRINT                 : Result:= 'APPCOMMAND_PRINT';
    APPCOMMAND_REDO                  : Result:= 'APPCOMMAND_REDO';
    APPCOMMAND_REPLY_TO_MAIL         : Result:= 'APPCOMMAND_REPLY_TO_MAIL';
    APPCOMMAND_SAVE                  : Result:= 'APPCOMMAND_SAVE';
    APPCOMMAND_SEND_MAIL             : Result:= 'APPCOMMAND_SEND_MAIL';
    APPCOMMAND_SPELL_CHECK           : Result:= 'APPCOMMAND_SPELL_CHECK';
    APPCOMMAND_TREBLE_DOWN           : Result:= 'APPCOMMAND_TREBLE_DOWN';
    APPCOMMAND_TREBLE_UP             : Result:= 'APPCOMMAND_TREBLE_UP';
    APPCOMMAND_UNDO                  : Result:= 'APPCOMMAND_UNDO';
    APPCOMMAND_VOLUME_DOWN           : Result:= 'APPCOMMAND_VOLUME_DOWN';
    APPCOMMAND_VOLUME_MUTE           : Result:= 'APPCOMMAND_VOLUME_MUTE';
    APPCOMMAND_VOLUME_UP             : Result:= 'APPCOMMAND_VOLUME_UP';
  else
    Result:= 'Unknown command';
  end; {case}
end;



initialization
  guardian := TCriticalSection.Create;
finalization
{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Finalizing DebugHelpers...');
{$ENDIF}

    guardian.Free;

{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Done finalizing DebugHelpers');
{$ENDIF}
end { Debughelpers }.
