unit Fn_StringIndex;

interface
uses
  SysUtils;

function StringIndex(const SearchString: string; StrList: array of string): Integer;

implementation

function StringIndex(const SearchString: string; StrList: array of string): Integer;
var
  I: Integer;
begin
  Result:= -1;
  for I:= 0 to High(StrList) do
    if CompareText(SearchString, StrList[I]) = 0 then
    begin
      Result:= I;
      Break;
    end;
end;

end.
