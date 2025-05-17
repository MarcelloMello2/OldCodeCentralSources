unit UnitFinder_SystemSupport;

interface

uses
  SysUtils, Classes;

function IfBlank(s,Default:string):string;
function StartsWith(const s,StartsWith:string; CaseSensitive:boolean = False):boolean;
function IndexOfStartsWith(const sStartsWith:string; Strings:TStrings):integer;

implementation

function StartsWith(const s,StartsWith:string; CaseSensitive:boolean = False):boolean;
begin
  if CaseSensitive then
    result := copy(s,1,Length(StartsWith)) = StartsWith
  else
    result := CompareText(copy(s,1,Length(StartsWith)), StartsWith) = 0;
end;


function IndexOfStartsWith(const sStartsWith:string; Strings:TStrings):integer;
var
  i:integer;
  L, H, C: Integer;
begin
  result := -1;
  if (Strings is TStringList) and (TStringList(Strings).Sorted) then begin
    L := 0;
    H := Strings.Count - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      if StartsWith(Strings[I], sStartsWith) then
        C := 0
      else
        C := CompareText(Strings[I], sStartsWith);
      if C < 0 then L := I + 1 else begin
        H := I - 1;
        if C = 0 then begin
          result := I;
          break;
        end;
      end;
    end;
  end else
    for i := 0 to Strings.Count-1 do begin
      if StartsWith(Strings[i], sStartsWith) then begin
        result := i;
        break;
      end;
    end;
end;

function IfBlank(s,Default:string):string;
begin
  if s = '' then
    result := Default
  else
    result := s;
end;


end.
