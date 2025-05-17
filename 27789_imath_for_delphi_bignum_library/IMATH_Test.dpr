program IMATH_Test;

{$APPTYPE CONSOLE}

uses
  SysUtils, IMath;

var
  buf : array[0..255] of Ansichar;
  tmpvar1, tmpvar2 : mp_int;

begin
  tmpvar1 := mp_int_alloc;
  tmpvar2 := mp_int_alloc;
  mp_int_init(tmpvar1);
  mp_int_read_string(tmpvar1,10,'99999999999999999');
  mp_int_read_string(tmpvar2,10,'11111111111111111');
  mp_int_add(tmpvar1, tmpvar2, tmpvar1);
  mp_int_to_string(tmpvar1,10,buf,254);
  if mp_int_is_even(tmpvar1) then
    writeln('even');
  mp_int_clear(tmpvar1);
  mp_int_clear(tmpvar2);
  mp_int_free(tmpvar1);
  mp_int_free(tmpvar2);
  writeln(buf);
  readln;
end.
