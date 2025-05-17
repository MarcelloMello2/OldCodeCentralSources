program SimpleGenericEnumTest;

{$APPTYPE CONSOLE}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  SysUtils,
  SimpleGenericEnum in 'SimpleGenericEnum.pas';

type
  TJim = (jjHigh,jjLow,jjHello_There);
var
  J: TEnum<TJim>;
  S: String;
  I: INteger;
begin
   Writeln('Sizeof(J):',Sizeof(J));


   J := jjHigh;
   I := J;
   S := J;
   WriteLn(S,'(',I,')');
   case J.Value of // Have to specify the value because case gets confused
     jjHigh..jjLow
     : WriteLn(String(J),'(',Integer(J),')'); // Type casting quired because writeln will write anything
   end;

   J := JJLow;
   WriteLn(String(J),'(',Integer(J),')');

   J := jjHello_There;
   WriteLn(String(J),'(',Integer(J),')');

   J := 0;
   WriteLn(String(J),'(',Integer(J),')');

   J := 1;
   WriteLn(String(J),'(',Integer(J),')');

   try
     J := 2;
   except
     on E: Exception do
       writeln(E.Message);
   end;

   J := 'jjHigh';
   WriteLn(String(J),'(',Integer(J),')');

   J := 'jjLow';
   WriteLn(String(J),'(',Integer(J),')');

   J := 'jjHello_There';
   WriteLn(String(J),'(',Integer(J),')');

   J := 'Hello There';
   WriteLn(String(J),'(',Integer(J),')');
   try
     J := 'Hommie';
   except
     on E: Exception do
       writeln(E.Message);
   end;

   Writeln;
   writeln('Press enter to exit');
   readln;

end.

