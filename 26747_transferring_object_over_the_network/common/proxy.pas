unit proxy;

interface

uses
  Classes;

type
  TProxy = class(TComponent)
  private
    FInternalObject : TObject;
  published
    property InternalObject: TObject read FInternalObject write FInternalObject;
  end;

implementation

end.
