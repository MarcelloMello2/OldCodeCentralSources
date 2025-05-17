program IMATH_Test;

{$APPTYPE CONSOLE}

uses
  SysUtils, IMath;

var
  buf : array[0..255] of Ansichar;
  tmpvar1, tmpvar2 : mp_int;
  tmpvar3, tmpvar4 : mp_rat;
begin
  tmpvar1 := mp_int_alloc;
  tmpvar2 := mp_int_alloc;
  mp_int_init(tmpvar1);
  mp_int_init(tmpvar2);
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
  //
  tmpvar3 := mp_rat_alloc;
  tmpvar4 := mp_rat_alloc;
  mp_rat_init(tmpvar3);
  mp_rat_init(tmpvar4);
  mp_rat_read_ustring(tmpvar3,10,'99999999999999999.99999999999999999',nil);
  mp_rat_read_ustring(tmpvar4,10,'11111111111111111.11111111111111110',nil);
  mp_rat_add(tmpvar3,tmpvar4,tmpvar3);
  mp_rat_to_decimal(tmpvar3,10,50,MP_ROUND_UP,buf,254);

  mp_rat_clear(tmpvar3);
  mp_rat_clear(tmpvar4);
  mp_rat_free(tmpvar3);
  mp_rat_free(tmpvar4);
  writeln(buf);

  readln;
end.
