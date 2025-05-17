unit MyCollectionUnit;

interface

uses Classes;

type TMyCollectionItem = class (TCollectionItem)
     private
      FMyStringData  : String;
      FMyIntegerData : Integer;
     public
      procedure Assign (aSource : TPersistent); override;
     published
      property MyStringData  : String  read FMyStringData  write FMyStringData;
      property MyIntegerData : Integer read FMyIntegerData write FMyIntegerData;
     end;

     _COLLECTION_ITEM_ = TMyCollectionItem; 

     {$INCLUDE TemplateCollectionInterface}

     TMyCollection = _COLLECTION_;

implementation

uses SysUtils;

{$INCLUDE TemplateCollectionImplementation}

procedure TMyCollectionItem.Assign (aSource : TPersistent);
begin
 if aSource is TMyCollectionItem then
 begin
  FMyStringData  := TMyCollectionItem(aSource).FMyStringData;
  FMyIntegerData := TMyCollectionItem(aSource).FMyIntegerData;
 end
 else inherited;
end;

end.
