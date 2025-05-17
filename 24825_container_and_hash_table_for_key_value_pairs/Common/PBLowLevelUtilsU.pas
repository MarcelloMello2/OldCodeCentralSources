{== PBLowLevelUtilsU ==================================================}
{! <summary>
 This unit collects a number of assembly routines for low-level bit
  twiddling.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2009-02-14</para>
<para>Last modified       2009-02-14</para>
</history>
<remarks>The unit was created by refactoring code from other units.
</remarks>}
{======================================================================}

unit PBLowLevelUtilsU;
{$INCLUDE PBDEFINES.INC}
interface

{! <summary>Checks if a range of memory contains a dword that is not 0.
 </summary>
 <returns> true if a dword not 0 was found, false otherwise.</returns>
 <param name="P"> is the start address of the memory to check. </param>
 <param name="NumDwords">is the number of dwords to check.</param>
 <exception cref="EAccessViolation">If P is nil or the checked memory
  area is not in a valid address range an access violation will likely
  result.</exception> }
function AnyBitSet(P: Pointer; NumDwords: Longword): Boolean; register;

{! <summary>Counts the number of set bits in a memory range.</summary>
 <returns> the count of set bits.</returns>
 <param name="P"> is the start address of the memory to check. </param>
 <param name="NumDwords">is the number of bytes to process.</param>
 <exception cref="EAccessViolation">If P is nil or the checked memory
  area is not in a valid address range an access violation will likely
  result.</exception> }
function CountBits(P: Pointer; NumBytes: Longword): Integer; register;

{! <summary>Fills a memory range with a dword value.</summary>
 <param name="value"> is the value to use for the fill. </param>
 <param name="P"> is the start address of the memory to fill. </param>
 <param name="NumDwords">is the number of dwords to fill.</param>
 <exception cref="EAccessViolation">If P is nil or the filled memory
  area is not in a valid address range an access violation will likely
  result.</exception> }
procedure DWordFill(value: Longword; P: Pointer; NumDwords: Longword); register;

{! <summary>Invert all bits in a memory range.</summary>
 <param name="P"> is the start address of the memory to flip. </param>
 <param name="NumDwords">is the number of dwords to invert.</param>
 <exception cref="EAccessViolation">If P is nil or the processed memory
  area is not in a valid address range an access violation will likely
  result.</exception> }
procedure FlipDwordBits(P: Pointer; NumDwords: Longword);register;

{! <summary>
 Combines two array of longwords using a bitwise AND operation. </summary>
<param name="p1">is the address of the reference array. This array will
 be modified by the operation. </param>
<param name="p2">is the address of the array to merge.</param>
<param name="NumDwords"> is the number of longwords in the arrays.</param>
<remarks>After the operation the array at P1 will have all bits set that
were set in the original array and also in the one at P2 at the same position.
<para>Note that using this with overlapping memory blocks will likely not yield
the expected result!</para>
If p1 = p2 or NumDWords = 0 the routine will do nothing.</remarks>
<exception cref="EAccessViolation">If P1 or P2 is nil or the processed memory
 area is not in a valid address range an access violation will likely
 result.</exception> }
procedure MergeAnd(p1, p2: Pointer; NumDwords: Longword); register;

{! <summary>
 Combines two array of longwords using a bitwise NOT AND operation. </summary>
<param name="p1">is the address of the reference array. This array will
 be modified by the operation. </param>
<param name="p2">is the address of the array to merge.</param>
<param name="NumDwords"> is the number of longwords in the arrays.</param>
<remarks>After the operation the array at P1 will have all bits set that
were set in the original array but not in the one at P2. If P1 = P2 that
means the array would be set to all 0!
<para>Note that using this with overlapping memory blocks will likely not yield
the expected result!</para></remarks>
<exception cref="EAccessViolation">If P1 or P2 is nil or the processed memory
 area is not in a valid address range an access violation will likely
 result.</exception> }
procedure MergeNot(p1, p2: Pointer; NumDwords: Longword); register;

{! <summary>
 Combines two array of longwords using a bitwise OR operation. </summary>
<param name="p1">is the address of the reference array. This array will
 be modified by the operation. </param>
<param name="p2">is the address of the array to merge.</param>
<param name="NumDwords"> is the number of longwords in the arrays.</param>
<remarks>After the operation the array at P1 will have all bits set that
were set in the original array or in the one at P2 at the same position.
<para>Note that using this with overlapping memory blocks will likely not yield
the expected result!</para>
If p1 = p2 or NumDWords = 0 the routine will do nothing.</remarks>
<exception cref="EAccessViolation">If P1 or P2 is nil or the processed memory
 area is not in a valid address range an access violation will likely
 result.</exception> }
procedure MergeOr(p1, p2: Pointer; NumDwords: Longword); register;

{! <summary>
 Combines two array of longwords using a bitwise XOR operation. </summary>
<param name="p1">is the address of the reference array. This array will
 be modified by the operation. </param>
<param name="p2">is the address of the array to merge.</param>
<param name="NumDwords"> is the number of longwords in the arrays.</param>
<remarks>After the operation the array at P1 will have all bits set that
were set in the original array or in the one at P2, but not in both.
If P1 = P2 that means the array would be set to all 0!
<para>Note that using this with overlapping memory blocks will likely not yield
the expected result!</para></remarks>
<exception cref="EAccessViolation">If P1 or P2 is nil or the processed memory
 area is not in a valid address range an access violation will likely
 result.</exception> }
procedure MergeXOr(p1, p2: Pointer; NumDwords: Longword); register;

implementation

var
  // contains the count of set bits in a byte
  Bytemap : array [Byte] of Byte;


{ input: p1 in eax, p2 in edx, NumDWords in ecx}
procedure MergeOr(p1, p2: Pointer; NumDwords: Longword);
asm
  jcxz @done
  cmp eax, edx
  je @done
  push ebx
@start:
  mov ebx, [edx]
  or dword ptr [eax], ebx
  add eax, 4
  add edx, 4
  dec ecx
  jnz @start
  pop ebx
@done:
end;

{! input: p1 in eax, p2 in edx, NumDWords in ecx}
procedure MergeXOr(p1, p2: Pointer; NumDwords: Longword);
asm
  jcxz @done
  push ebx
@start:
  mov ebx, [edx]
  xor dword ptr [eax], ebx
  add eax, 4
  add edx, 4
  dec ecx
  jnz @start
  pop ebx
@done:
end;

{ input: p1 in eax, p2 in edx, NumDWords in ecx}
procedure MergeAnd(p1, p2: Pointer; NumDwords: Longword);
asm
  jcxz @done
  cmp eax, edx
  je @done
  push ebx
@start:
  mov ebx, [edx]
  and dword ptr [eax], ebx
  add eax, 4
  add edx, 4
  dec ecx
  jnz @start
  pop ebx
@done:
end;

{ input: p1 in eax, p2 in edx, NumDWords in ecx}
procedure MergeNot(p1, p2: Pointer; NumDwords: Longword);
asm
  jcxz @done
  push ebx
@start:
  mov ebx, [edx]
  not ebx
  and dword ptr [eax], ebx
  add eax, 4
  add edx, 4
  dec ecx
  jnz @start
  pop ebx
@done:
end;

{ input: p1 in eax, NumDWords in edx
  output: al 0 or 1 }
function AnyBitSet(P: Pointer; NumDwords: Longword): Boolean;
asm
  push edi
  mov edi, eax
  mov eax, 0
  mov ecx, edx
  jcxz @done
  cld
  repe scasd
  je @done
  inc al
@done:
  pop edi
end;

{ input: value in eax, P in edx, NumDWords in ecx}
procedure DWordFill(value: Longword; P: Pointer; NumDwords: Longword);
asm
  jcxz @done
  push edi
  mov edi, edx
  cld
  rep stosd
  pop edi
@done:
end;

{! input: p in eax, NumDWords in edx
 Inverts all bits in the NumDword longwords starting at address P}
procedure FlipDwordBits(P: Pointer; NumDwords: Longword);
asm
  mov ecx, edx
  jcxz @done
@start:
  not dword ptr [eax]
  add eax, 4
  dec ecx
  jnz @start
@done:
end;

{ input: p in eax, NumBytes in edx
  output: number of set bits in the Numbytes at address p in eax
  register use:
  ebx points to the Bytemap
  ecx is the loop counter
  esi points to the current byte in the input buffer
  edx accumulates the count of bits
  al holds the current byte to calculate the bit count for and also
    receives this bit count after the lookup.
  The loop uses a table lookup in the Bytemap array, which contains
  the count of set bits for all possible values of a byte. }
function CountBits(P: Pointer; NumBytes: Longword): Integer;
asm
  push ebx
  push esi
  mov esi, eax
  mov ecx, edx
  mov edx, 0
  jcxz @done
  lea ebx, Bytemap
  mov eax, 0
@loop:
  mov al, [esi]
  xlat
  add edx, eax
  inc esi
  dec ecx
  jnz @loop
@done:
  mov eax, edx
  pop esi
  pop ebx
end;

{ Calculates the number of set bits for all possible values of a byte
 and stores the result in the Bytemap array, using the value of the
 byte as index.
 Register usage:
 ecx as loop counter
 edi pointer to the current entry of the Bytemap
 dl  index of the entry edi points to
 al  accumulates the number of bits set in the value in dl
 ah  temp copy of dl. This copy is shifted bitwise through the carry
     register, the value of which is then added to al.
 The inner loop counting the bits is unrolled for better performance.    }
procedure InitializeBytemap;
asm
  push edi
  lea edi, Bytemap
  mov ecx, 256
  mov edx, 0
  mov eax, 0;
@loop:
  xor ax, ax
  mov ah, dl
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  rcr ah, 1
  adc al, 0
  mov [edi], al
  inc edi
  inc dl
  dec ecx
  jnz @loop
  pop edi
end;


initialization
  InitializeBytemap;
end.
