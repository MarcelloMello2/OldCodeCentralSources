{ == Win64CompatU ====================================================== }
{ ! <summary>
 This unit collects a number of helper routines useful to prepare code
 for a 64 bit Delphi environment.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2010-11-05</para>
<para>Last modified       2010-11-05</para>
</history>
<remarks>
We deal with size differences between pointers and other types here
by providing helper routines to use instead of direct typecasts. This
way the differences between platforms can be hidden inside the helper
routines.</remarks> }
{ ====================================================================== }
unit Win64CompatU;
{$INCLUDE PBDEFINES.INC}

interface

{$STACKFRAMES OFF}

function IntToPointer(I: integer): Pointer; inline;
function UIntToPointer(C: Cardinal): Pointer; inline;
function PointerToInt(P: Pointer): integer; inline;
function PointerToUInt(P: Pointer): Cardinal; inline;

implementation

type
  TInt = packed record
    case boolean of
      false:
        (I: integer);
      true:
        (P: Pointer);
  end; { case }

  TUInt = packed record
    case boolean of
      false:
        (C: Cardinal);
      true:
        (P: Pointer);
  end; { case }

function IntToPointer(I: integer): Pointer; inline;
begin
{$IFDEF CPU64}
  Result := nil;
  TInt(Result).I := I;
{$ELSE}
  Result := Pointer(I);
{$ENDIF}
end;

function UIntToPointer(C: Cardinal): Pointer; inline;
begin
{$IFDEF CPU64}
  Result := nil;
  TUInt(Result).C := C;
{$ELSE}
  Result := Pointer(C);
{$ENDIF}
end;

function PointerToInt(P: Pointer): integer; inline;
begin
  Result := TInt(P).I;
end;

function PointerToUInt(P: Pointer): Cardinal; inline;
begin
  Result := TUInt(P).C;
end;

end.
