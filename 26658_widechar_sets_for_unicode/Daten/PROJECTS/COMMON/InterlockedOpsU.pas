{== InterlockedOpsU ===================================================}
{! <summary>
 This unit declares a number of typesafe wrappers for the Windows
  Interlocked* functions.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2007-01-17</para>
<para>Last modified       2009-02-13</para>
</history>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}

unit InterlockedOpsU;
{$INCLUDE PBDEFINES.INC}
interface

{! <summary>
Places the passed Value into the memory location of Target and returns
the old content of Target.</summary>
<remarks>
Target has to be a variable of Sizeof(TObject) bytes at minimum! The
operation is thread-safe.</remarks> }
function InterlockedExchangeObject(
  var Target; Value: TObject): TObject; stdcall;

{! <summary>
Places the passed Value into the memory location of Target and returns
the old content of Target.</summary>
<remarks>
Target has to be a variable of Sizeof(THandle) bytes at minimum! The
operation is thread-safe.<para>
The function can be used to swap interface references by casting them
to TObject. Just keep in mind that this will not update the interface
reference counts appropriately!</para></remarks> </remarks> }
function InterlockedExchangeHandle(
  var Target; Value: THandle): THandle; stdcall;

{! <summary>
Compares the current value of Target with Comperand. If they
are equal the old content of Target is replaced with Exchange.</summary>
<returns> the old value of Target.</returns>
<remarks> Target has to be a variable of Sizeof(TObject) bytes at
minimum! The operation is thread-safe.<para>
The function can be used to swap interface references by casting them
to TObject. Just keep in mind that this will not update the interface
reference counts appropriately!</para>
<para>
CAVEAT! With Delphi 2010 a cast like TObject(InterfaceVar) is no longer
a NOP! Do it as TObject(Pointer(InterfaceVar)) or it will not work as
intended and can even cause an access violation if InterfaceVar happens to
be nil!</para></remarks> }
function InterlockedCompareExchangeObject(var Target; Exchange,
    Comperand: TObject): TObject; stdcall;

{! <summary>
Compares the current value of Target with Comperand. If they
are equal the old content of Target is replaced with Exchange.</summary>
<returns> the old value of Target.</returns>
<remarks> Target has to be a variable of Sizeof(THandle) bytes at
minimum! The operation is thread-safe.</remarks> }
function InterlockedCompareExchangeHandle(
  var Target; Exchange, Comperand: THandle): THandle; stdcall;

implementation

uses Windows;

function InterlockedExchangeObject;

{$IFDEF WIN64}
  external kernel32 name 'InterlockedExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedExchange';
{$ENDIF}
function InterlockedExchangeHandle;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedExchange';
{$ENDIF}
function InterlockedCompareExchangeObject;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedCompareExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedCompareExchange';
{$ENDIF}
function InterlockedCompareExchangeHandle;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedCompareExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedCompareExchange';
{$ENDIF}

end.
