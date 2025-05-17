_COLLECTION_ = class (TOwnedCollection)
protected
 function  GetItem (const aIndex : Integer) : _COLLECTION_ITEM_;
 procedure SetItem (const aIndex : Integer;
                    const aValue : _COLLECTION_ITEM_);
public
 constructor Create (const aOwner : TComponent);

 function Add                                 : _COLLECTION_ITEM_;
 function FindItemID (const aID    : Integer) : _COLLECTION_ITEM_;
 function Insert     (const aIndex : Integer) : _COLLECTION_ITEM_;
 property Items      [const aIndex : Integer] : _COLLECTION_ITEM_ read GetItem write SetItem;
end;
