unit FloatVectorUnit;

interface

uses Classes;

type _VECTOR_DATA_TYPE_ = Double;

     {$INCLUDE TemplateVectorInterface}

     IFloatVector = _VECTOR_INTERFACE_;
     TFloatVector = _VECTOR_CLASS_;

function CreateFloatVector (const aLength : Integer = 0) : IFloatVector;

implementation

{$INCLUDE TemplateVectorImplementation}

function CreateFloatVector (const aLength : Integer = 0) : IFloatVector;     
begin
 Result := TFloatVector.Create (aLength);
end;

end.
