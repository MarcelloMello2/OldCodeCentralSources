Unit MethodExptReg;

Interface

Uses
  Exptintf, MethodExpt;

  Procedure Register;

Implementation

{ Register this expert as part of the component library }

Procedure Register;

Begin
  RegisterLibraryExpert(TDGHMethodExpt.Create);
End;

End.
