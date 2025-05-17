{== UnicodeHelpersU ===================================================}
{! <summary>
 This unit collects some helper routines for working with Unicodestring
 in pre-D2009 versions.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2008-03-16</para>
<para>Last modified       2009-02-13</para>
</history>
<remarks>
This is a compatibility helper unit for Delphi version that do not have
a native Unicodestring type and thus also no standard functions that
operate on such strings. For versions that do the implemented functions
will map to the RTL functions.  </remarks>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!
{======================================================================}

unit UnicodeHelpersU;
{$INCLUDE PBDEFINES.INC}

interface

uses Windows, SysUtils, Classes, CommonTypesU, Charsets;

{! Return a copy of the passed in string with all letters converted
 to upper case. }
function UnicodeUpperCase(const S: UnicodeString): UnicodeString;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{! Compare the two strings passed in and return true if they are equal.
The comparison is case-sensitive. }
function UnicodeSameStr(const S1, S2: UnicodeString): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{! Compare the two strings passed in and return true if they are equal.
The comparison is not case-sensitive. }
function UnicodeSameText(const S1, S2: UnicodeString): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{! Compare the two strings passed in and return -1 if S1 considered
smaller than S2, 0 if both strings are equal, 1 if S1 is considered
larger than S2. The comparison is based on the Windows' collation
tables. The comparison is case-sensitive. }
function UnicodeCompareStr(const S1, S2: UnicodeString): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{! Compare the two strings passed in and return -1 if S1 considered
smaller than S2, 0 if both strings are equal, 1 if S1 is considered
larger than S2. The comparison is based on the Windows' collation
tables. The comparison is not case-sensitive. }
function UnicodeCompareText(const S1, S2: UnicodeString): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{! Compose a formatted string based on the mask in Fmt and the values
passed in A. See the docs for the Format function. }
function UnicodeFormat(const Fmt: UnicodeString; const A: array of const): UnicodeString;
  {function cannot be inlined due to open array parameter. }

{! Check if the passed character is considered whitespace in the Unicode
character set. Uses the GetStringTypeExW Windows API function.}
function UnicodeIsWhitespace(const Ch: Unicodechar): Boolean;

{! Convert the value of the passed pointer to a hexadecimal string.
Useful for displaying addresses. Using a %p placeholder with the
Wideformat function in D2007 returns garbage! }
function PointerToHex(P: Pointer): UnicodeString;

{$IFNDEF UNICODE}
type
  TSysCharset = TCharset;
function CharInSet(const Ch: AnsiChar; const aSet: TSysCharset): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

{$IFDEF UNICODE}
procedure Str(const value: Extended; var S: string);
{$ENDIF}


implementation

{$IFDEF UNICODE}
procedure Str(const value: Extended; var S: string);
const
  Settings: TFormatSettings =
    (
     ThousandSeparator: ',';
     DecimalSeparator: '.';
     );
begin
  S:= FloatToStr(Value, Settings);
end;
{$ENDIF}

function UnicodeUpperCase(const S: UnicodeString): UnicodeString;
begin
  {$IFDEF UNICODE}
  Result := AnsiUppercase(S);
  {$ELSE}
  Result := WideUppercase(S);
  {$ENDIF}
end;

function UnicodeSameStr(const S1, S2: UnicodeString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := AnsiSameStr(S1, S2);
  {$ELSE}
  Result := WideCompareStr(S1, S2) = 0;
  {Note: using WideCompareStr here causes a bogus warning in D2007 since
   WideSameStr is inline itself. }
  {$ENDIF}
end;

function UnicodeSameText(const S1, S2: UnicodeString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := AnsiSameText(S1, S2);
  {$ELSE}
  Result := WideCompareText(S1, S2) = 0;
  {$ENDIF}
end;

function UnicodeCompareStr(const S1, S2: UnicodeString): Integer;
begin
  {$IFDEF UNICODE}
  Result := AnsiCompareStr(S1, S2);
  {$ELSE}
  Result := WideCompareStr(S1, S2);
  {$ENDIF}
end;

function UnicodeCompareText(const S1, S2: UnicodeString): Integer;
begin
  {$IFDEF UNICODE}
  Result := AnsiCompareText(S1, S2);
  {$ELSE}
  Result := WideCompareText(S1, S2);
  {$ENDIF}
end;

function UnicodeFormat(const Fmt: UnicodeString; const A: array of const): UnicodeString;
begin
  {$IFDEF UNICODE}
  Result := Format(Fmt, A);
  {$ELSE}
  Result := WideFormat(Fmt, A);
  {$ENDIF}
end;

function UnicodeIsWhitespace(const Ch: Unicodechar): Boolean;
var
  Res: Word;
begin
  Result :=
    GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE1, @Ch, 1, Res);
  if Result then
    Result := (Res and (C1_SPACE or C1_CNTRL or C1_BLANK)) <> 0;
end;

function PointerToHex(P: Pointer): UnicodeString;
var
  Buffer: array [0..Sizeof(Pointer)*2] of AnsiChar;
begin
  BinToHex(@P, Buffer, Sizeof(Pointer));
  Buffer[High(Buffer)] := #0;
  {$IFDEF UNICODE}
  Result := String(Buffer);
  {$ELSE}
  Result := Buffer;
  {$ENDIF}
end;

{$IFNDEF UNICODE}
function CharInSet(const Ch: AnsiChar; const aSet: TSysCharset): Boolean;
begin
  Result := Ch IN aSet;
end;
{$ENDIF}


end.
