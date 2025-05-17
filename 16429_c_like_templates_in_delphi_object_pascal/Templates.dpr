program Templates;

{$APPTYPE CONSOLE}

uses SysUtils,
     MyCollectionUnit in 'MyCollectionUnit.pas',
     FloatVectorUnit  in 'FloatVectorUnit.pas';

var  aFloatVector : IFloatVector;
     aIndex       : Integer;
     
begin
 aFloatVector := CreateFloatVector;

 aFloatVector.Extend.Last := 1;
 aFloatVector.Extend.Last := 2;

 for aIndex := aFloatVector.Low to aFloatVector.High do
 begin
  WriteLn (FloatToStr (aFloatVector [aIndex]));
 end;

 ReadLn;
end.
