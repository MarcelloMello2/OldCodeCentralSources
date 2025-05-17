{== Charsets ==========================================================}
{! <summary>
 This unit collects a few useful sets of (ANSI) characters.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created in the distant past</para>
<para>Last modified       2009-02-13</para>
</history>
<remarks>The unit composes some of the sets at startup, depending on the
current system locale. If the locale changes the SetupCharsets method
should be called to update these sets. In the Unicode (D2009+) version
the sets containing letters are composed based on the Latin1 codepage
regardless of the current system locale, however.
</remarks>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}

unit Charsets;
interface
{$INCLUDE PBDEFINES.INC}
uses CharactersU, Sysutils;

const
  Latin1Codepage = 1252;
type
{$IFNDEF UNICODE}
  TCharSet = set of AnsiChar;
{$ELSE}
  TCharSet = TSysCharset;
{$ENDIF}
const
  Signs: TCharset = ['-', '+'];
  Numerals: TCharset = ['0'..'9'];
  HexNumerals: TCharset = ['A'..'F', 'a'..'f', '0'..'9'];
  IntegerChars: TCharset = ['0'..'9', '-', '+'];
  IdentifierChars: TCharset = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  SqlWildcards: TCharset = ['_', '%'];
  DosWildcards: TCharset = ['?', '*'];
  EditOperations: TCharset = [^C, ^V, ^X, Backspace];
  TextChars: TCharSet = [Tab, LF, CR, Space..#255];
var
  Digits, Letters, LowerCaseLetters, UpperCaseLetters: TCharSet;
  FloatChars, SciFloatChars: TCharset;
  AlphaNum, NonAlphaNum: TCharset;

{ Need to call this again when locale changes.  }
procedure SetupCharsets;

{! Returns true if the passed string S contains any characters from
the set CSet, false if not.}
function StringContainsChars(const S: string; const CSet: TCharset): Boolean;

implementation

uses Windows, UnicodeHelpersU;

var
  locale: DWORD = 0;

procedure SetupCharsets;
{$IFDEF UNICODE}
type
  Latin1String = type Ansistring(Latin1Codepage);
{$ENDIF}
var
  ch: AnsiChar;
{$IFDEF UNICODE}
  S: Latin1String;
  WS: Unicodestring;
  I: Integer;
  c: char;
{$ENDIF}
begin
  if locale = GetThreadLocale then
    Exit
  else
    Locale := GetThreadLocale;
  LowerCaseLetters := [];
  UpperCaseLetters := [];
  AlphaNum := [];
  NonAlphaNum := [];
  Digits := Numerals;

{$IFDEF UNICODE}
  SetLength(S, 255);
  for ch := #1 to #255 do
    S[Ord(ch)] := ch;
  WS := String(S);
  for I := 1 to Length(WS) do begin
    c:= WS[I];
    if IsCharAlpha(c) then
      if IsCharUpper(c) then
        Include(UpperCaseLetters, S[I])
      else
        Include(LowerCaseLetters, S[I]);
    if IsCharAlphanumeric(c) then
      Include(AlphaNum, S[I])
    else
      Include(NonAlphaNum, S[I]);
  end; {for}
{$ELSE}
  for ch := Low(ch) to High(ch) do begin
    if IsCharAlpha(ch) then
      if IsCharUpper(ch) then
        Include(UpperCaseLetters, ch)
      else
        Include(LowerCaseLetters, ch);
    if IsCharAlphanumeric(ch) then
      Include(AlphaNum, ch)
    else
      Include(NonAlphaNum, ch);
  end; { For }
{$ENDIF}
  Letters := LowerCaseLetters + UpperCaseLetters;
  FloatChars := IntegerChars;
  Include(FloatChars, AnsiChar(DecimalSeparator));
  SciFloatChars := FloatChars + ['e', 'E'];
end; { SetupCharsets }

function StringContainsChars(const S: string; const CSet: TCharset): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 1 to Length(S) do
     if CharInSet(S[I], CSet) then begin
       Result := true;
       Break;
     end; {if}
end;

initialization
  SetupCharsets;
end.
