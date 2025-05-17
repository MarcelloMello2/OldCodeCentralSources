unit Commons.Utils.InteropServices;

interface

uses
  System.Runtime.InteropServices;

type
  TMarshalHelper = class helper for Marshal
  public
    class function ReleaseComObject(var Ref): Integer; reintroduce;
  end;

implementation

{ TMarshalHelper }

class function TMarshalHelper.ReleaseComObject(var Ref): Integer;
begin
  if Assigned(TObject(Ref)) then begin
    Result := inherited ReleaseComObject(TObject(Ref));
    Ref := nil;
  end
  else
    Result := 0;
end;

end.
