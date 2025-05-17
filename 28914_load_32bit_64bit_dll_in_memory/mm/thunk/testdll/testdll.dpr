library testdll;

uses
  //System, SysInit,
  Windows;

//{$R *.res}

procedure b();
asm
  int 3
end;

function A(p : PWideChar):Integer; stdcall;
begin
  //b();
  Result := MessageBox(0, P, '', MB_OK);
end;

exports
  A index 2;

begin
  //MessageBox(0,'','',MB_OK);
end.
