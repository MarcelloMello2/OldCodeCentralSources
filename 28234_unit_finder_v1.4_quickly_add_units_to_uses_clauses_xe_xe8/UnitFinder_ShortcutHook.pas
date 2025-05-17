unit UnitFinder_ShortcutHook;

interface

uses
  Windows, Messages, Types, Classes, Menus;


type
  TOnShortcutEvent = procedure(Shortcut:TShortcut; var Allow:boolean) of Object;


procedure SetOnShortcutEvent(OnShortcutEvent:TOnShortcutEvent);

implementation

uses
  SysUtils;

const
  cAllow:DWORD = 0;
  cPrevent:DWORD = 1;
  cKeyDownFlag:DWORD = $40000000;

var
  HookHandle:THandle = 0;
  OnShortcut:TOnShortcutEvent;


function GetShortcutFromParams(wParam:DWORD; lParam:DWORD):TShortcut;
const
  AltMask = $20000000;
begin
  result := Byte(wParam);
  if result = 0 then
    exit;
  if GetKeyState(VK_SHIFT) < 0 then
    Inc(result, scShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Inc(result, scCtrl);
  if lParam and AltMask <> 0 then
    Inc(result, scAlt);
end;

function KeyboardProc(nCode:Integer; wParam:DWORD; lParam:DWORD):DWORD; stdcall;
var
  Allow:boolean;
begin
  if nCode < 0 then begin
    result := CallNextHookEx(HookHandle, nCode, wParam,  lParam);
    exit;
  end;

  result := cAllow;
  if (nCode = HC_ACTION) and
     (lParam and cKeyDownFlag = 0) and
     (Assigned(OnShortcut)) then begin
    Allow := True;
    OnShortcut(GetShortcutFromParams(wParam,lParam), Allow);
    if Allow then
      result := CallNextHookEx(HookHandle, nCode, wParam,  lParam)
    else
      result := cPrevent;
  end;
end;


procedure HookKeyboard;
begin
  if HookHandle <> 0 then
    exit;
  HookHandle := SetWindowsHookEx(WH_KEYBOARD, @KeyboardProc, 0, Windows.GetCurrentThreadId);
  if HookHandle = 0 then
    RaiseLastOSError;
end;

procedure UnhookKeyboard;
begin
  if UnhookWindowsHookEx(HookHandle) = False then
    RaiseLastOSError;
  HookHandle := 0;
end;

procedure SetOnShortcutEvent(OnShortcutEvent:TOnShortcutEvent);
begin
  OnShortcut := OnShortcutEvent;
  HookKeyboard;
end;


initialization

finalization
  UnhookKeyboard;

end.
