unit UnitName;

interface

uses
  TypInfo;

function GetUnitName(argObject: TObject): string; overload;

implementation

uses
  Sysutils;

function GetUnitName(argObject: TObject): string;
var
  ptrTypeData: PTypeData;
begin
  if (argObject.ClassInfo <> nil) then
  begin
    ptrTypeData := GetTypeData(argObject.ClassInfo);
    Result := ptrTypeData.UnitName;
  end;
end;

end.
