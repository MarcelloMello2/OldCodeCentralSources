unit Commons.Utils.InteropServices;

interface

uses
  System.Runtime.InteropServices;

type
  TMarshalHelper = class helper (TObjectHelper) for Marshal
  public
    class function ReleaseComObject(var Ref): Integer; reintroduce;
  end;

implementation

{$region 'TMarshalHelper'}

class function TMarshalHelper.ReleaseComObject(var Ref): Integer;
begin
  if Assigned(TObject(Ref)) then begin
    if TypeOf(TObject(Ref)).IsCOMObject then
      Result := inherited ReleaseComObject(TObject(Ref))
    else
      Result := 0;
    Ref := nil;
  end
  else
    Result := 0;
end;

{$endregion}

end.
