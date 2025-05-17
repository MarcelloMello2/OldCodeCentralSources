constructor _VECTOR_CLASS_.Create (const aLength : Integer);
begin
 inherited Create;

 SetLength (aLength);
end;

function _VECTOR_CLASS_.GetLength : Integer;
begin
 Result := System.Length (FArray);
end;

procedure _VECTOR_CLASS_.SetLength (const aLength : Integer);
begin
 System.SetLength (FArray, aLength);
end;

function _VECTOR_CLASS_.GetItems (const aIndex : Integer) : _VECTOR_DATA_TYPE_;
begin
 Result := FArray [aIndex];
end;

procedure _VECTOR_CLASS_.SetItems (const aIndex : Integer;
                                   const aValue : _VECTOR_DATA_TYPE_);
begin
 FArray [aIndex] := aValue;
end;

function _VECTOR_CLASS_.High : Integer;
begin
 Result := System.High (FArray);
end;

function _VECTOR_CLASS_.Low : Integer;
begin
 Result := System.Low (FArray);
end;

function _VECTOR_CLASS_.GetFirst : _VECTOR_DATA_TYPE_;
begin
 Result := FArray [System.Low (FArray)];
end;

procedure _VECTOR_CLASS_.SetFirst (const aValue : _VECTOR_DATA_TYPE_);
begin
 FArray [System.Low (FArray)] := aValue;
end;

function _VECTOR_CLASS_.GetLast : _VECTOR_DATA_TYPE_;
begin
 Result := FArray [System.High (FArray)];
end;

procedure _VECTOR_CLASS_.SetLast (const aValue : _VECTOR_DATA_TYPE_);
begin
 FArray [System.High (FArray)] := aValue;
end;

function _VECTOR_CLASS_.Clear : _VECTOR_INTERFACE_;
begin
 FArray := Nil;

 Result := Self;
end;

function _VECTOR_CLASS_.Extend (const aDelta : Word) : _VECTOR_INTERFACE_;
begin
 System.SetLength (FArray, System.Length (FArray) + aDelta);

 Result := Self;
end;

function _VECTOR_CLASS_.Contract (const aDelta : Word) : _VECTOR_INTERFACE_;
begin
 System.SetLength (FArray, System.Length (FArray) - aDelta);

 Result := Self;
end;
