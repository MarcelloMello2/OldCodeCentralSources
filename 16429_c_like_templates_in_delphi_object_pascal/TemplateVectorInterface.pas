_VECTOR_INTERFACE_ = interface
 function  GetLength : Integer;
 procedure SetLength (const aLength : Integer);

 function  GetItems (const aIndex : Integer) : _VECTOR_DATA_TYPE_;
 procedure SetItems (const aIndex : Integer;
                     const aValue : _VECTOR_DATA_TYPE_);

 function  GetFirst : _VECTOR_DATA_TYPE_;
 procedure SetFirst (const aValue : _VECTOR_DATA_TYPE_);

 function  GetLast  : _VECTOR_DATA_TYPE_;
 procedure SetLast  (const aValue : _VECTOR_DATA_TYPE_);

 function  High  : Integer;
 function  Low   : Integer;

 function  Clear                              : _VECTOR_INTERFACE_;
 function  Extend   (const aDelta : Word = 1) : _VECTOR_INTERFACE_;
 function  Contract (const aDelta : Word = 1) : _VECTOR_INTERFACE_; 

 property  Length                         : Integer             read GetLength write SetLength;
 property  Items [const aIndex : Integer] : _VECTOR_DATA_TYPE_  read GetItems  write SetItems; default;
 property  First                          : _VECTOR_DATA_TYPE_  read GetFirst  write SetFirst;
 property  Last                           : _VECTOR_DATA_TYPE_  read GetLast   write SetLast;
end;

_VECTOR_CLASS_ = class (TInterfacedObject, _VECTOR_INTERFACE_)
private
 FArray : array of _VECTOR_DATA_TYPE_;
protected
 function  GetLength : Integer;
 procedure SetLength (const aLength : Integer);

 function  GetItems (const aIndex : Integer) : _VECTOR_DATA_TYPE_;
 procedure SetItems (const aIndex : Integer;
                     const aValue : _VECTOR_DATA_TYPE_);

 function  GetFirst : _VECTOR_DATA_TYPE_;
 procedure SetFirst (const aValue : _VECTOR_DATA_TYPE_);

 function  GetLast  : _VECTOR_DATA_TYPE_;
 procedure SetLast  (const aValue : _VECTOR_DATA_TYPE_);
public
 function  High  : Integer;
 function  Low   : Integer;

 function  Clear                              : _VECTOR_INTERFACE_;
 function  Extend   (const aDelta : Word = 1) : _VECTOR_INTERFACE_;
 function  Contract (const aDelta : Word = 1) : _VECTOR_INTERFACE_; 

 constructor Create (const aLength : Integer);
end;
