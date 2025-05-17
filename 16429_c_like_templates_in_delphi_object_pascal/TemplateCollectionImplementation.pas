constructor _COLLECTION_.Create (const aOwner : TComponent);
begin
 inherited Create (aOwner, _COLLECTION_ITEM_);
end;

function _COLLECTION_.Add : _COLLECTION_ITEM_;
begin
 Result := _COLLECTION_ITEM_ (inherited Add);
end;

function _COLLECTION_.FindItemID (const aID : Integer) : _COLLECTION_ITEM_;
begin
 Result := _COLLECTION_ITEM_ (inherited FindItemID (aID));
end;

function _COLLECTION_.GetItem (const aIndex : Integer) : _COLLECTION_ITEM_;
begin
 Result := _COLLECTION_ITEM_ (inherited GetItem (aIndex));
end;

function _COLLECTION_.Insert (const aIndex : Integer) : _COLLECTION_ITEM_;
begin
 Result := _COLLECTION_ITEM_ (inherited Insert (aIndex));
end;

procedure _COLLECTION_.SetItem (const aIndex : Integer;
                                const aValue : _COLLECTION_ITEM_);
begin
 inherited SetItem (aIndex, aValue);
end;
