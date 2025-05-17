{== Utils =============================================================}
{! <summary>This Unit collects some general purpose routines. </summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created a looong time ago</para>
<para>Last modified       2009-03-01</para>
<para>1996-04-05: added TextfileSize, HasWhitespace, IsWhitespace</para>
<para>1997-05-19: added AsserTrailingBackslash</para>
<para>1997-05-21: added EnumDirectories</para>
<para>1997-05-27: added CopyFile, UpdateFile, AssertIsNotReadOnly</para>
<para>1997-06-03: added StringOfChar for Win16</para>
<para>1997-06-06: added ExtractFileDir for Win16</para>
<para>1997-06-30: added CreateBackupfile</para>
<para>1997-07-08: added SplitString</para>
<para>1997-07-22: added DirIsWriteable</para>
<para>1998-05-23: added FreeObject</para>
<para>2000-05-21: added PadLeft and Padright</para>
<para>2000-07-23: added RemoveFileExt</para>
<para>2000-08-11: added ConcatSubstrings, ConcatQuotedSubstrings</para>
<para>2001-03-23: added LoadStringFromFile, SaveStringToFile</para>
<para>2001-03-18: added LoadStringsFromArray</para>
<para>2001-11-09: added CountOfChar</para>
<para>2001-12-12: added TryStrToFloat, StrToFloatDef</para>
<para>2001-02-05: added CompareInt, CompareFloat</para>
<para>2001-03-22: added TryStringToBoolean</para>
<para>2002-06-20: changed all TStringlist parameters to TStrings, added StringIn</para>
<para>2002-08-19: Added JEDI.INC to handle compiler versions better </para>
<para>2002-10-21: Added FindSwitch </para>
<para>2002-11-07: Added ValueInArray </para>
<para>2003-01-24: Added MixedCase</para>
<para>2003-06-05: Added Split</para>
<para>2003-10-30: Added ExtractStringElement and CountOfElements</para>
<para>2004-05-05: Added SameMethod</para>
<para>2005-06-05: Added GetfirstNonBlankString, SplitStringEx</para>
<para>2006-02-06: Added RemoveWhitespace, RemoveCharsInSet</para>
<para>2008-01-23: Added CompareInt64 and added inlining for several functions.</para>
<para>2008-03-18: Added ByteInRange</para>
<para>2008-10-05: Added partial support for Delphi 2009. </para>
<para>2009-01-16: Removed backward compatibility code for older Delphi versions.</para>
<para>2009-02-14: Reworked some routines for Delphi 2009.</para>
<para>2009-03-01: Finished documentation and test cases, removed
  dependency on JEDI.INC.</para>
<para>2009-03-19: Added overloads for ConcatSubstrings and
  ConcatQuotedSubstrings that take arrays of string instead of
  stringlists as input.</para>
<para>2010-04-27: Added SortNatural.</para>
<para>2010-08-14: Merged the old ExUtils and FileFinder units into this one.</para>
 </history>
<remarks>The unit started life in the days of TurboPascal for DOS and
has picked up stuff ever since. Some of the routines have been superseeded
by additions to the Delphi run-time library and were either removed later
or now just call the RTL functions directly.
</remarks>
<copyright>Copyright 1995-2010 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}
unit Utils;

{$INCLUDE PBDEFINES.INC}
interface
uses Windows, Classes, Sysutils, Charsets, CharactersU
  {$IFDEF UNICODE},WidecharSetU{$ENDIF}
 ;


{! <summary>Return the size of a file in bytes.</summary>
   <returns> the size of the file, or 0 if the file is not found.</returns>
   <param name="name">is the files name. This should be a full pathname,
   otherwise the function may not be able find the file since this would
   depend on what the applications current directory is.</param>
   <remarks>The function uses FindFirst to get the file size, so it does
   not need to open the file.</remarks> }
function TextfileSize(const name: string): int64;

{! <summary> Check if the passed string contains whitespace anywhere.
   </summary>
   <returns>True if whitespace is found, false if not.</returns>
   <param name="S"> Is the string to check, can be empty.</param>
   <remarks>What is considered whitespace is defined by the content of
   the Whitespace global variable. The default treats all characters in
   the range #0 to #32 (space) as whitespace, plus some Unicode codepoints
   for the D2009+ version.</remarks> }
function HasWhitespace(const S: string): Boolean;

{!<summary>  Check if the passed character is considered whitespace.</summary>
   <returns>True if ch is whitespace, false if not.</returns>
   <param name="S"> Is the character to check</param>
   <remarks>What is considered whitespace is defined by the content of
   the Whitespace global variable. The default treats all characters in
   the range #0 to #32 (space) as whitespace, plus some Unicode codepoints
   for the D2009+ version.</remarks> }
function IsWhitespace(ch: Char): Boolean;
{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

{!<summary>Remove whitespace from the right side of the passed string.</summary>
  <remarks>Has been superseeded by Sysutils.TrimRight.</remarks> }
function RTrim(const S: string): string;
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use Sysutils.TrimRight instead.';
{$ENDIF}


{!<summary>Remove whitespace from the left side of the passed string.</summary>
  <remarks>Has been superseeded by Sysutils.TrimLeft.</remarks> }
function LTrim(const S: string): string;
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use Sysutils.TrimLeft instead.';
{$ENDIF}

{! <summary>Find the position of a character in a string.</summary>
<returns> the position of the first instance of ch in S, or 0 if
 ch was not found.</returns>
<param name="ch">is the character to look for.</param>
<param name="S">is the string to scan, can be empty.</param>}
function Scan(ch: Char; const S: string): Integer;

{! <summary>Find the position of a character in a string.</summary>
<returns> the position of the next instance of ch in S after fromPos, or 0 if
 ch was not found.</returns>
<param name="ch">is the character to look for.</param>
<param name="S">is the string to scan, can be empty.</param>
<param name="fromPos">is the position of the first character to examine.
 If fromPos is &lt; 1 it will be set to 1.</param>}
function IScan(ch: Char; const S: string; fromPos: Integer): Integer;

{! <summary>Find the last position of a character in a string.</summary>
<returns> the position of the last instance of ch in S, or 0 if
  ch was not found.</returns>
<param name="ch">is the character to look for.</param>
<param name="S">is the string to scan, can be empty.</param>}
function RScan(ch: Char; const S: string): Integer;

{! <summary>Find the last position of a character in a string.</summary>
<returns> the position of the last instance of ch in S before fromPos, or 0 if
  ch was not found.</returns>
<param name="ch">is the character to look for.</param>
<param name="S">is the string to scan, can be empty.</param>
<param name="fromPos">is the position of the first character to examine.
The scan proceeds towards the start of the string from there. If fromPos
is &gt; Length(S) it will be set to Length(S).</param>}
function RIScan(ch: Char; const S: string; fromPos: Integer): Integer;

{! <summary>  Copy the passed string to a PChar buffer. </summary>
<returns>the allocated buffer. The caller has to deallocate the memory
 with StrDispose when it is no longer needed!</returns>
 <remarks>For Delphi 2009+ the routine allocates a buffer of widechars,
 for older versions one of ansichars. If the passed-in string is empty
 the returned buffer will be 1 character long and contain a #0.</remarks> }
function StrToPChar(const S: string): Pchar;

{! <summary>Counts the set bits in a variable.</summary>
 <returns>the number of set bits.</returns>
 <param name="aValue">is the variable to process.</param>
 <param name="size"> is the size of the variable in bytes.</param>
 <remarks>The implementation of this function is not very efficient.
 <see cref="CountBits"/> in unit PBLowLevelUtilsU for a faster
 alternative.</remarks> }
function BitCount(var aValue; size: Word): Integer;

{! <summary>Finds the most significant set bit in a variable.</summary>
<returns>the 0-based index of the highest bit set, or -1, if no bits
 are set in the variable.</returns>
<param name="aValue">is the variable to examine.</param>
<param name="size">is the size in bytes of the variable.</param>
<remarks>The implementation of this function is not very efficient.</remarks> }
function HiBitPosition(var aValue; size: word): Integer;

{!<summary>Adds a trailing backslash to the passed string if it does not
  already end in one. An empty string is not changed, however.</summary>
  <remarks>Superseded by the IncludeTrailingPathDelimiter function.</remarks>}
procedure AssertTrailingBackslash(var S: string);
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use IncludeTrailingPathDelimiter instead.';
{$ENDIF}

{! <summary> If the passed file exists the routine will remove the
   read-only flag if it is set.</summary>
   <exception cref="EWin32Error">is raised if the file attributes cannot
   be changed.</exception>
   <remarks> Of course this will not work if the medium itself is
   read-only!</remarks> }
procedure AssertIsNotReadonly(const fname: string);

{! <summary>  Enumerate all subdirectories directly under the directory
   specified in path.</summary>
   <param name="path">is the directory to process. The parameter cannot be
   empty. If the directory does not exist no files can be found, of course,
   but this will not cause an exception.</param>
   <param name="list">is the list to add found subdirectories to. This
   parameter cannot be nil. The list only gets the folder names, not their
   full path!</param>
   <exception cref="EParameterCannotBeNil">is raised if list is nil.
   </exception>
   <remarks> The list is not cleared first, so repeated calls using the
   same list are accumulative. </remarks> }
procedure EnumDirectories(path: string; list: TStrings);

{! <summary>Copies a file. </summary>
   <returns> true if the copy succeeded, false if it did not. </returns>
   <param name="source">is the name of the source file. This should be a
   full pathname. The file must exist for the copy to succeed, obviously.
   </param>
   <param name="target">is the name of the target file. This should be a
   full pathname. If the file already exists it will be overwritten.
   </param>
   <remarks>This is a simple wrapper for the Windows.CopyFile function. If
   the copy fails you can call GetLastError to figure out why, or just use
   RaiseLastOSError to raise an exception. The copy will typically fail if
   the source file does not exist or the target folder does not exist or
   is read-only. The copy can also fail due to insufficient access rights
   to source or target.
   <para>If you do not use full pathnames the outcome depends on what the
   application's current directory is. Since this can change without
   warning you should always use full pathnames. </para></remarks> }
function CopyFile(const source, target: string): Boolean;

{! <summary>Copies a file if the target does not exist or is older than
  the source file. </summary>
   <returns> true if the copy succeeded, false if it did not. </returns>
   <param name="source">is the name of the source file. This should be a
   full pathname. The file must exist for the copy to succeed, obviously.
   </param>
   <param name="target">is the name of the target file. This should be a
   full pathname. If the file already exists and is older than the source
   it will be overwritten.
   </param>
   <remarks>If
   the copy fails you can call GetLastError to figure out why, or just use
   RaiseLastOSError to raise an exception. The copy will typically fail if
   the source file does not exist or the target folder does not exist or
   is read-only. The copy can also fail due to insufficient access rights
   to source or target.
   <para>If you do not use full pathnames the outcome depends on what the
   application's current directory is. Since this can change without
   warning you should always use full pathnames. </para></remarks> }
function UpdateFile(const source, target: string): Boolean;

{! <summary>Copies the file with the passed filename to the new extension,
   overwrites any previous file with the new name.</summary>
   <param name="filename">is the name of the file to back up. This should
   be a full pathname and the file has to exists for the procedure to
   succeed.</param>
   <param name="newExt">is the extension to use for the copy. This
   parameter cannot be empty. The extension can be specified with or
   without a leading period.</param>
   <exception cref="EParameterCannotBeNil">is raised if newExt is an empty
   string.</exception>
   <exception cref="EInvalidParameter">is raised if the backup filename
   turns out to be identical to the original filename.</exception>
   }
procedure CreateBackupfile(const filename: string; const newExt:
    string);

{! <summary>Split the passed string into substrings at the position of the
   separator character and add the substrings to the passed list.
   </summary>
   <param name="S">Is the string to split.</param>
   <param name="separator">is the character to use as separator. </param>
   <param name="substrings">takes the found substrings. This parameter
   cannot be nil. Note that the list is not cleared first by the routine,
   any substrings found will be added to whatever is already in the
   list</param>
   <exception cref="EParameterCannotBeNil">if substrings is nil.
   </exception>
   <remarks>The routine does not deal with separators in quoted substrings
   as a special case, the substring would be split inside a quoted strings
   as well. Two separators following each other will be interpreted as an
   empty substring, a separator at the start or end of S will also cause
   an empty substring to be added to the substrings list.
   <para>Since Delphi 2006 TStringlists DelimitedText property with
     StrictDelimiter set to true is an alternative, since it deals with
     separators in quoted substrings as a special case. </para>
   </remarks> }
procedure SplitString(const S: string; separator: Char; substrings:
    TStrings);

{! <summary>Split the passed string into substrings at the position of the
   separator string and add the substrings to the passed list.
   </summary>
   <param name="S">Is the string to split.</param>
   <param name="separator">is the string to use as separator. Cannot be
     an empty string.</param>
   <param name="substrings">takes the found substrings. This parameter
   cannot be nil. Note that the list is not cleared first by the routine,
   any substrings found will be added to whatever is already in the
   list</param>
   <exception cref="EParameterCannotBeNil">if substrings is nil or separator
   is empty. </exception>
   <remarks>The routine does not deal with separators in quoted substrings
   as a special case, the substring would be split inside a quoted strings
   as well. Two separators following each other will be interpreted as an
   empty substring, a separator at the start or end of S will also cause
   an empty substring to be added to the substrings list. </remarks> }
procedure SplitString2(S: string; const separator: string;
  substrings: TStrings);

{! <summary>Concatenate the items of the passed list into a single string,
   separating the items with the separator string.</summary>
   <returns>the produced string</returns>
   <param name="separator">is the separator to insert between items. This
   can be an empty string.</param>
   <param name="substrings">is the list holding the items to concatenate.
   This parameter cannot be nil</param>
   <exception cref="EParameterCannotBeNil">if substrings is nil</exception>
   <remarks>Since the routine repeatedly grows the result string it is
   prone to cause memory fragmentation if used on larger lists. Using
   a stringbuilder is an alternative in such cases.</remarks>}
function ConcatSubstrings(const separator: string; substrings:
    TStrings): string; overload;
function ConcatSubstrings(const separator: string; const substrings:
    array of string): string; overload;

{! <summary>Concatenate the items of the passed list into a single string,
   separating the items with the separator string  and enclosing each
   item with the quote characters.</summary>
   <returns>the produced string</returns>
   <param name="separator">is the separator to insert between items.
   This can be an empty string.</param>
   <param name='quote'>is the quote character to use.  Instances of the
   quote character inside an item will be doubled.</param>
   <param name="substrings">is the list holding the items to concatenate.
   This parameter cannot be nil</param>
   <exception cref="EParameterCannotBeNil">if substrings is nil</exception>
   <remarks>Since the routine repeatedly grows the result string it is
   prone to cause memory fragmentation if used on larger lists. Using
   a stringbuilder is an alternative in such cases.</remarks> }
function ConcatQuotedSubstrings(separator, quote: Char; substrings:
    TStrings): string; overload;
function ConcatQuotedSubstrings(separator, quote: Char; const substrings:
    array of string): string; overload;

{! <summary>  Checks if the passed directory can be written to.</summary>
   <returns>true if a file can be created in the directory, false if not.
   </returns>
   <param name="S">is the full pathname of the directory to check,
   optionally with a backslash at the end.</param>
   <remarks>The function tries to create  a test file in the folder, which
   is deleted again when the create operation succeeds. Critical errors (
   like accessing a drive with no media in it) are trapped.</remarks> }
function DirIsWriteable(S: string): Boolean;

{! <summary>Return the highest value in an array, or 0 if the array
  is empty.</summary> }
function MaxValue(const values: array of Extended): Extended;

{! <summary>  Return the lowest value in an array, or 0 if the array
  is empty.</summary> }
function MinValue(const values: array of Extended): Extended;

{! Free the passed object and set the variable to Nil. Superseded
   by Sysutils.FreeAndNil. }
procedure FreeObject(var anObject: TObject);
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use FreeAndNil instead.';
{$ENDIF}

{! <summary>Pad a string to a given length by adding characters on the
   left.</summary>
   <returns>the resulting string</returns>
   <param name="S">is the string to pad</param>
   <param name="toLength">is the requested minimum length of the result
   string. If S is already this long or longer the function will return S
   unchanged.</param>
   <param name="withChar">is the character to use for padding.</param> }
function PadLeft(const S: string; toLength: Integer; withChar: Char):
    string;

{! <summary>Pad a string to a given length by adding characters on the
   right.</summary>
   <returns>the resulting string</returns>
   <param name="S">is the string to pad</param>
   <param name="toLength">is the requested minimum length of the result
   string. If S is already this long or longer the function will return S
   unchanged.</param>
   <param name="withChar">is the character to use for padding.</param> }
function PadRight(const S: string; toLength: Integer;
  withChar: Char): string;

{!  <summary>   Remove a files extension including the dot.</summary>
  <returns>the modified string</returns>}
function RemoveFileExt(const pathname: string): string;

{! <summary>Load a file into a string.</summary>
<param name="filename">is the name of the file to load. The file must exist.
This should be a full pathname to avoid dependencies on the current
directory.</param>
<param name="aEncoding">is an optional encoding to assume for the file. If
this parameter is nil the default encoding (ANSI, current codepage) is
assumed. This parameter is only available in the D2009+ version, the prior
version always loads the file as is and ignores any encoding issues. </param>
<exception cref="EStreamError">will be raised if the file does not exist.</exception>
<remarks>
If the file starts with byte-order mark the Unicode version of this function
is able to figure out the encoding used, so aEncoding can be passed in as
nil. If the correct encoding is passed in such a case the BOM will be
skipped. If another encoding is passed, however, the result will be
garbage! If no BOM is present then the correct encoding has to be passed
or the file will be interpreted as an ANSI file in the current codepage,
which will result in garbage again if it is in fact in another encoding.
</remarks>}
{$IFDEF UNICODE}
function LoadStringFromFile(const filename: string; aEncoding: TEncoding = nil): string;
{$ELSE}
function LoadStringFromFile(const filename: string): string;
{$ENDIF}

{! <summary>
Store the passed string into a file</summary>
<param name="S">is the string to store. Passing in an empty string will
result in a 0 byte file!</param>
<param name="filename">is the name of the file to create. This should be
a full pathname to avoid dependencies on the current directory. If the
file already exists it will be overwritten!</param>
<param name="aEncoding">is an optional encoding to use for the file. If
this parameter is nil the default encoding (ANSI, current codepage) is
assumed. This parameter is only available in the D2009+ version, the prior
version always write the string as it is. </param>
<exception cref="EStreamError">will be raised if the file cannot be created.</exception>
<remarks>
If the requested encoding uses a byte-order mark this will be written
to the start of the file, unless the passed string is empty.
</remarks>}
{$IFDEF UNICODE}
procedure SaveStringToFile(const S, filename: string; aEncoding: TEncoding = nil);
{$ELSE}
procedure SaveStringToFile(const S, filename: string);
{$ENDIF}

{! <summary>
Load an array of strings into a TStrings descendent.</summary>
<param name="list">is the list to fill. This parameter cannot be nil.</param>
<param name="A">is the array to load. </param>
<param name="clearlist">determines whether to discard any old content
 of the list or append to it. The default is true.</param>
<exception cref="EParameterCannotBeNil">is raised if list is nil. </exception>
}
procedure LoadStringsFromArray(list: TStrings; const A: array of
    string; clearlist: Boolean = true);

{! <summary>
Count the number of occurence of a given character in a string</summary>
<param name="ch">is the character to look for.</param>
<param name="S">is the string to examine.</param>
<returns>the number of times ch is found i S, or 0 if it is not found.</returns>}
function CountOfChar(ch: Char; const S: string): Cardinal;

{! <summary>
 Extract the Index-th element from a string of elements separated by a
 separator character.  </summary>
<returns> the found string, or an empy string if index is &gt;= the
 number of elements. If the separator is not found at all the complete
 input string will be returned.  </returns>
<param name="Value">is the string to dissect</param>
<param name="Index"> is the zero-based index of the element to extract.</param>
<param name="Separator">is the separator character, a tab by default. </param>
<remarks>Note that the elements cannot contain the separator character
 themselves, we do not treat quoted strings specially here.</remarks> }
function ExtractStringElement(const Value: string; Index: Cardinal;
    Separator: Char = #9): string;

{! <summary>
 Count the elements present in the Value string. </summary>
<returns>the count of elements found.</returns>
<param name="Value">is the string to dissect</param>
<param name="Separator">is the separator character, a tab by default. </param>
<remarks>
Elements are substrings separated by the Separator character, they must not
contain the separator themselves. </remarks>}
function CountOfElements( const Value: string;
  Separator: Char = #9 ): Integer;

{! <summary>
 Compare two integers using the same semantics as CompareStr.</summary>
<returns> -1 if i1 &lt; i2, 1 if i1 &gt; i2, 0 if i1 = i2</returns>   }
function CompareInt(i1, i2: Integer): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{! <summary>
 Compare two integers using the same semantics as CompareStr.</summary>
<returns> -1 if i1 &lt; i2, 1 if i1 &gt; i2, 0 if i1 = i2</returns>   }
function CompareInt64(i1, i2: Int64): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{! <summary>
 Compare two integers using the same semantics as CompareStr.</summary>
<returns> -1 if f1 &lt; f2, 1 if f1 &gt; f2, 0 if f1 = f2
Note that two values will be considered equal if their difference
is &lt; Epsilon. The default for Epsilon is 1.0E-10. </returns>}
function CompareFloat(f1, f2: Extended): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{!  <summary>
 Try to convert a string to a boolean value</summary>
<returns>  true if the value could be converted, false if not.</returns>
<param name="value">is the string to convert</param>
<param name="b">will return the converted value, if successful</param>
<remarks>
The value string can be 0, 1, true, false, yes, no, wahr, falsch, Y, J,
or N. The match is not case-sensitive. </remarks>
}
function TryStringToBoolean(const value: string; var b: Boolean):
  Boolean;

{! <summary>
Tests if aString is in the passed array of strings</summary>
<returns>  true if aString is one of the strings in the array, false if
 not. If aString or A is empty false is returned.</returns>
<param name="aString"> is the string to test for</param>
<param name="A">is an array of strings to test against</param>
<param name="caseSensitive">determines whether the test is case-sensitive
 or not. This parameter is optional, the default is true.</param>
<remarks>Has been made a bit redundant by routines like AnsiMatchStr
and AnsiMatchText from the StrUtils unit that comes with Delphi.</remarks>  }
function StringIn(const aString: string;
  const A: array of string;
  caseSensitive: Boolean = true): Boolean;

{! <summary>
 Find a commandline switch and return its value</summary>
<returns>True if the switch was found, false if not.</returns>
<param name="switchchar">is the character identifying the switch, usually a
  letter.</param>
<param name="value">returns the switches value, if it has one. A colon
  following the switchchar is removed, if the result is enclosed
  in double quotes it is unquoted.</param>
<remarks>
Note that no whitespace is allowed between the switch and its value,
unless the whole construct has been enclosed in double quotes when it
was passed on the commandline. The routine uses ParamStr, which splits
unquoted parameters on whitespace. The switchchar has to preceeded by
either a hyphen or a slash character to mark the parameter as a switch.</remarks> }
function FindSwitch(switchchar: Char; var value: string): Boolean;

{! <summary>
Test if a value is contained in an array of integer.</summary>
<returns>true if a match for value is found in A, false if not.</returns>
<param name="value">is the value to look for</param>
<param name="A">is the array to look in</param>}
function ValueInArray(value: Integer; const A: array of Integer):
  Boolean;

{! <summary>
Convert a string to all lower case, with the first character in
upper case.</summary>
<returns>the converted string, with any whitespace at start or end
  removed.</returns>
<param name="S">is the string to convert</param> }
function MixedCase(const S: string): string;

{! <summary>
Split a string on the first occurence of a separator substring.</summary>
<param name="S">is the string to split</param>
<param name="firstpart">receives the part before the separator</param>
<param name="secondpart">receives the part after the separator</param>
<param name="separator"> is the substring to split the string on.</param>
<remarks>  
The search for the substring is case-sensitive. If the substring
does not appear in the passed string S firstpart will return a copy of S
and secondpart will be empty.</remarks>}
procedure Split(const S: string; var firstpart, secondpart: string;
  const separator: string);

{! <summary>  
Compare two method pointers, consider them the same if both code
and data parts are the same </summary>}
function SameMethod( M1, M2: TMethod ): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

type
  {! <summary>
  Exception class used to report errors in <see cref="SplitStringEx"/> </summary>}
  ESplitStringError = class(Exception);

{! <summary>  
 Split a string on a separator, handling quoted items that may contain
 the separator.</summary>
<param name="S"> is the string to split.</param>
<param name="List">takes the found items. This parameter cannot be nil.</param>
<param name="Separator">is the character separating items, a tab by default.</param>
<param name="QuoteChar">is the character used to quote items, a double quote
   by default.</param> 
<param name="Clearlist">determines whether List will be cleared before new items
   are added, true by default.</param> 
<exception cref="EParameterCannotBeNil">is raised if list is nil. </exception>
<exception cref="ESplitStringError">
 if the input string is not formatted correctly. This will typically 
 happen if quote characters are not properly balanced.</exception>
<remarks>  
Any quoted items found will be returned unquoted. A literal quote 
character inside a quoted string has to be doubled in the input string 
to be processed correctly. A sequence of two separators is interpreted as 
an empty item, as is a separator at the end of the input string. </remarks>}
procedure SplitStringEx(const S: String; List: TStrings;
  Separator: char = #9; QuoteChar: char = '"'; ClearList: Boolean = true );

{!    <summary>
 Returns the first non-empty string in Strings</summary>
<returns>
 The string found, or an empty string if all strings are empty. A string
 only containing whitespace is considered empty!</returns>}
function GetFirstNonBlankString( const Strings: array of string ): string;

{! <summary>
 Remove all characters considered whitespace from the passed string
 and returns the resulting string</summary>}
function RemoveWhitespace(const S: string):string;

{! <summary>
Remove all characters in aSet from the passed string and returns the
resulting string</summary>}
{$IFDEF UNICODE}
  function RemoveCharsInSet(const S: string; const aSet: IWideCharset):string; overload;
  function RemoveCharsInSet(const S: string; const aSet: TCharset):string; overload;
{$ELSE}
  function RemoveCharsInSet(const S: string; const aSet: TCharset):string;
{$ENDIF}

{! <summary>  
 Check if a byte value is in the range LowBound..HiBound, including the
 bounds.</summary>
<remarks>
If HiBound &lt; LowBound the two are swapped internally, this 
is not considered an error but slows down the routine a bit.</remarks> }
function ByteInRange(const Value, LowBound, HiBound: Byte): Boolean;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{!
<summary>
 SortNatural will sort the strings in a stringlist. The sort is
 case-sensistive and will treat the first sequences of digits inside
 a string as a cardinal number and sort in correct numeric sequence
 in such cases. </summary>
<param name="aList">
 is the list to sort. </param>
<exception cref="EParameterCannotBeNil">
 is raised if aList is nil.</exception>
}
procedure SortNatural(aList: TStringlist);

type
{$IFDEF COMPILER6_UP}
  EFileSystemError = Class( EOSError );
{$ELSE}
  EFileSystemError = Class( EWin32Error );

{This is a compatibility wrapper for ExcludeTrailingBackslash for pre-Delphi 6}
function ExcludeTrailingPathDelimiter(const S: string): string;
{$ENDIF}

{!
<summary>
 MakeTimestampedbackup create a copy of a file with a new name based on
 the current date and time and the extension BAK. </summary>
<param name="filename">
 is the original name of the file to copy. This should  be a full path.
 If the file does not exist no action is taken.</param>
<param name="newdir">
 is the directory the backup should be generated in. If this is an empty
 string the file will be created in the original directory of the source
 file. The name need not end in a backslash.</param>
<exception cref="EFileSystemError">
 is raised if the file cannot be copied. This may be due to a sharing
 conflict or because the directory the file resides in could not be
 created.</exception>
<remarks>
 An existing file with the same generated name would be overwritten
 but it is very unlikely that a name collision would ever happen. If
 the target directory does not exist is it created.
 <para>
 The new name generated has the format oldname_yyyymmd_hhmmss.BAK.
 </para> </remarks>
}
procedure MakeTimestampedbackup( const filename, newdir: string );

{!
<summary>
 DateString returns a string encoding the current date in the format yyyymmdd.</summary>
}
function DateString: string;

{!
<summary>
 TimeString returns a string encoding the current time in the format hhmmss.</summary>
}
function TimeString: string;

{!
<summary>
 ClipIntToRange limits a value to a range.</summary>
<returns>
 If value is not in the range minallowed to maxallowed the
 function returns the bound value violates, otherwise it
 returns value itself.</returns>
<param name="value">
 is the value to clip.</param>
<param name="minallowed">
 is the lower bound of the allowed range.</param>
<param name="maxallowed">
 is the upper bound of the allowed range.</param>
<exception cref="EPreconditionViolation">
 is raised if minallowed is &gt; maxallowed.</exception>
}
function ClipIntToRange(value, minallowed, maxallowed: Integer):
  Integer;

{!
<summary>
 IntMin returns the smaller of the two input values.
 Has been superseded by Math.Min. </summary>
}
function IntMin(i1, i2: Integer): Integer;
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use Math.Min instead.';
{$ENDIF}

{!
<summary>
 IntMax returns the larger of the two input values.
 Has been superseded by Math.Max. </summary>
}
function IntMax(i1, i2: Integer): Integer;
{$IFDEF SUPPORTS_DEPRECATEDMSG}
deprecated 'use Math.Max instead.';
{$ENDIF}

{!
<summary>
 IIF returns one of two values depending on a condition.</summary>
<returns>
 ifvalue, if condition is true, elsevalue otherwise.</returns>
<param name="condition">
 is the condition to test for.</param>
<param name="ifvalue">
 is the value to return if condition is true.</param>
<param name="elsevalue">
 is the value to return if condition is false.</param>
<remarks>
 Due to the use of variants the function is not very efficient. The
 Sysutils and Math units provide a number of overloaded versions of
 a function named IfThen, which accept integer, int64, double, and string
 parameters, and are more efficient.</remarks>
}
function IIF(condition: Boolean; ifvalue, elsevalue: Variant): Variant;

{!
<summary>
 PostKeyEx32 uses keybd_event to manufacture a series of key events
 matching the passed parameters.</summary>
<param name="key">
 is the virtual keycode of the key to send. For printable keys this is
 simply the ANSI code (Ord(character)). </param>
<param name="shift">
 encodes the state of the modifier keys. This is a set, so
 you can set several of these keys (shift, control, alt, mouse buttons)
 in tandem. The TShiftState type is declared in the Classes unit.</param>
<param name="specialkey">
 normally this should be False. Set it to True to pecify a key on the
 numeric keypad, for example.</param>
<remarks>
 The events go to the control with focus, even if it is part of another
 process.
 Note that for characters key is always the upper-case version of
 the character. Sending without any modifier keys will result in
 a lower-case character, sending it with [ssShift] will result
 in an upper-case character!  </remarks>
}
procedure PostKeyEx32(key: Word; const shift: TShiftState;
  specialkey: Boolean);

{!
<summary>
 SendTextToForegroundWindow passes a string via key events to the control
 with focus.</summary>
<param name="S">
 is the string to pass.</param>
}
procedure SendTextToForegroundWindow(S: string);

{!
<summary>
 WaitForWindowchange waits for the foreground window to change.</summary>
<returns>
 true if the window changed inside 2 seconds, false if not.</returns>
<param name="win">
 is the current foreground window on entry and returns the new
 one on exit.</param>
}
function WaitForWindowchange(var win: HWND): Boolean;

{!
<summary>
 FindFileOnPath searches for a file in a directory and its subdirectories.
</summary>
<returns>
 the full path of the file found, or an empty string, if the file was not
 found </returns>
<param name="SPath">
 contains the path of the directory to search. If it is empty
 no action is taken and an empty string is returned  </param>
<param name="mask">
 contains the filename to search for, can contain wildcards </param>
<remarks>
 Performs a recursive search throught the passed directory and its
 subdirectories for the passed file. The first hit that is not a
 directory is returned. The search is a depth-first search.</remarks>
}
function FindFileOnPath(SPath: string; const mask: string): string;

{!
<summary>
 FindComponentByTag searches a container for a component of a given
 class with a certain Tag value.</summary>
<returns>
 the found component or Nil, if none matching the criteria was found.</returns>
<param name="owner">
 is the component that owns the components to search through. </param>
<param name="aClass">
 is the class of the component to find, descendents will also be accepted.
 If this parameter is nil all classes are accepted</param>
<param name="tagvalue">
 is the tag value of the component to find</param>
<param name="recursive">
 determines whether nested containers are also scanned or not. The default
 is false.</param>
<exception cref="EParameterCannotBeNil">
 is raised if owner is nil.</exception>
}
function FindComponentByTag(
  owner: TComponent; aClass: TComponentClass;
  tagvalue: Integer; recursive: boolean = false): TComponent;

var
  {! Set of characters considered whitespace in parsing strings. }
  {$IFDEF UNICODE}
    Whitespace: IWidecharSet;
  {$ELSE}
    Whitespace: set of Char;
  {$ENDIF}
  {! Floating point numbers with a difference less than Epsilon
     are considered equal. }
  Epsilon: Extended = 1.0E-10;

implementation

uses
  UnicodeHelpersU, CommonTypesU;

resourcestring
{$IFDEF GERMAN}
  SCannotCreateBackupfile =
    'CreateBackupfile: Name der Orginal- und der Backup-Datei sind identisch!';
  eCopyFailed =
    'MakeTimestampedBackup: Konnte Datei %s nicht nach %s kopieren.'#13#10'%s';
{$ELSE}
  SCannotCreateBackupfile =
    'CreateBackupfile: name of backup file and original are identical!';
  eCopyFailed =
    'MakeTimestampedBackup: Could not copy file %s to %s.'#13#10'%s';
{$ENDIF}


{$IFNDEF COMPILER6_UP}
function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := ExcludeTrailingBackslash(S);
end;
{$ENDIF}


function TextfileSize(const name: string): int64;
var
  SRec: TSearchRec;
begin
  if FindFirst(name, faAnyfile, SRec) = 0 then begin
    Result := SRec.Size;
    Sysutils.FindClose(SRec);
  end
  else
    Result := 0;
end; { TextfileSize }

function HasWhitespace(const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do begin
    if IsWhitespace(S[i]) then begin
      Result := True;
      Break;
    end; { If }
  end; { For }
end; { HasWhitespace }

function IsWhitespace(ch: Char): Boolean;
begin
{$IFDEF UNICODE}
  Result := Whitespace.Contains(Ch);
{$ELSE}
  Result := Ch in Whitespace;
{$ENDIF}
end;

function StrToPChar(const S: string): Pchar;
begin
  Result := StrAlloc(Length(S) + 1);
  if Length(S) > 0 then
    StrPCopy(Result, S)
  else
    Result^ := #0;
end;

{!
<summary>Find the first non-whitespace character in the passed string.</summary>
<returns>the index of the found character, or 0 if the string
consists of whitespace only or is empty.</returns> }
function FindStart(const S: string): Integer;
begin
  if Length(S) > 0 then begin
    Result := 1;
    while (Result <= Length(S)) and IsWhitespace(S[Result]) do
      Inc(Result);
  end {if}
  else
    Result := 0;
end;

{!
<summary>Find the last non-whitespace character in the passed string.</summary>
<returns>the index of the found character, or 0 if the string
consists of whitespace only or is empty.</returns> }
function FindEnd(const S: string): Integer;
begin
  Result := Length(S);
  while (Result > 0) and IsWhitespace(S[Result]) do
    Dec(Result);
end;

function RTrim(const S: string): string;
var
  n: Integer;
begin
  n := FindEnd(S);
  if n > 0 then
    Result := Copy(S, 1, n)
  else
    Result := '';
end;

function LTrim(const S: string): string;
var
  n: Integer;
begin
  n := FindStart(S);
  if (n > 0) and (n <= Length(S)) then
    Result := Copy(S, n, Maxint)
  else
    Result := '';
end;

procedure CheckBitset(size: Integer);
begin
  if size <> 1 then
    raise Exception.Create(
      'Unit Utils. Fatal Error: the size of a Set of 0..7 is not 1 byte!');
end; { CheckBitset }

function BitCount(var aValue; size: Word): Integer;
type
{$Z-}
  Bitset = set of 0..7;
var
  proxy: array[0..High(Word) - 1] of Bitset absolute aValue;
  n: Integer;
begin
  CheckBitset(Sizeof(Bitset));
  Result := 0;
  if Size = 0 then Exit;

  Dec(size);
  while true do begin
    if proxy[size] <> [] then
      for n := 0 to 7 do begin
        if n in proxy[size] then
          Inc(Result);
      end; { For }
    if size = 0 then
      Break;
    Dec(size);
  end; { While }
end;

function HiBitPosition(var aValue; size: word): Integer;
type
{$Z-}
  Bitset = set of 0..7;
var
  proxy: array[0..High(Word) - 1] of Bitset absolute aValue;
  n: Integer;
begin
  CheckBitset(Sizeof(Bitset));
  Result := -1;
  if Size = 0 then Exit;
  Dec(size);
  while true do begin
    if proxy[size] <> [] then
      for n := 7 downto 0 do begin
        if n in proxy[size] then begin
          Result := size * 8 + n;
          Exit;
        end; { If }
      end; { For }
    if size = 0 then
      Break;
    Dec(size);
  end; { While }
end;

function IScan(ch: Char; const S: string; fromPos: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if fromPos < 1 then
    fromPos := 1;
  for i := fromPos to Length(S) do begin
    if S[i] = ch then begin
      Result := i;
      Break;
    end; {if}
  end; {for}
end;

function Scan(ch: Char; const S: string): Integer;
begin
  Result := IScan(ch, S, 1);
end;

function RIScan(ch: Char; const S: string; fromPos: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  if fromPos > Length(S) then
    fromPos := Length(S);
  for i := fromPos downto 1 do begin
    if S[i] = ch then begin
      Result := i;
      Break;
    end; { If }
  end; { For }
end;

function RScan(ch: Char; const S: string): Integer;
begin
  Result := RIScan(ch, S, Length(S));
end;

procedure AssertTrailingBackslash(var S: string);
begin
  if (Length(S) > 0) and (S[Length(S)] <> '\') then
    S := S + '\';
end; { AssertTrailingBackslash }

procedure EnumDirectories(path: string; list: TStrings);
var
  sRec: TSearchRec;
  err: Integer;
begin
  if not Assigned(list) then
    raise EParameterCannotBeNil.Create('EnumDirectories', 'list');

  path := IncludeTrailingPathDelimiter(path) + '*.*';
  err := FindFirst(path, faDirectory, sRec);
  if err = 0 then begin
    while err = 0 do begin
      if ((SRec.Attr and faDirectory) = faDirectory) and
        (SRec.name[1] <> '.') then
        list.Add(SRec.Name);
      err := FindNext(SRec);
    end;
    Sysutils.FindClose(SRec);
  end; {if}
end;

procedure AssertIsNotReadonly(const fname: string);
var
  attr: Integer;
begin
  if FileExists(fname) then begin
    attr := FileGetAttr(fname);
    if (attr and faReadOnly) <> 0 then
      Win32Check(0 = FileSetAttr(fname, attr xor faReadOnly));
  end; { If }
end;

function CopyFile(const source, target: string): Boolean;
begin
  Result := Windows.CopyFile(PChar(Source), PChar(target), false);
end;

{$IFDEF IGNOREFILEAGEWARNING}{$WARN SYMBOL_DEPRECATED OFF}{$ENDIF}
{Delphi 2006 deprecates the old version of FileAge.}
function UpdateFile(const source, target: string): Boolean;
begin
  if FileExists(target) then
    if FileAge(source) <= FileAge(target) then begin
      Result := True;
      Exit; { nothing to do }
    end;
  Result := CopyFile(source, target);
end; { UpdateFile }
{$IFDEF IGNOREFILEAGEWARNING}{$WARN SYMBOL_DEPRECATED ON}{$ENDIF}

procedure CreateBackupfile(const filename: string; const newExt:
    string);
var
  bakname: string;
begin
  if newExt = '' then
    raise EParameterCannotBeNil.Create('CreateBackupfile', 'newExt');
  if FileExists(filename) then begin
    if newExt[1] = '.' then
      bakname := ChangeFileExt(filename, newExt)
    else
      bakname := ChangeFileExt(filename, '.' + newExt);
    if not AnsiSameText(filename, bakname) then
      CopyFile(filename, bakname)
    else
      raise EInvalidParameter.Create(SCannotCreateBackupfile);
  end; { If }
end; { CreateBackupfile }

procedure SplitString(const S: string; separator: Char; substrings:
    TStrings);
var
  i, n: Integer;
begin
  if not Assigned(substrings) then
    raise EParameterCannotBeNil.Create('SplitString','substrings');
  if Length(S) > 0 then begin
    i := 1;
    repeat
      n := IScan(separator, S, i);
      if n = 0 then
        n := Length(S) + 1;
      substrings.Add(Copy(S, i, n - i));
      i := n + 1;
    until i > Length(S);
    if n = Length(S) then
      substrings.Add('');
  end; { If }
end; { SplitString }

procedure SplitString2(S: string; const separator: string;
  substrings: TStrings);
var
  i: Integer;
begin
  if not Assigned(substrings) then
    raise EParameterCannotBeNil.Create('SplitString','substrings');
  if separator = '' then
    raise EParameterCannotBeNil.Create('SplitString','separator');
  if Length(S) > 0 then begin
    repeat
      i := Pos(separator, S);
      if i > 0 then begin
        substrings.Add(Copy(S, 1, i - 1));
        Delete(S, 1, i + Length(separator) - 1);
        if S = '' then
          substrings.Add('');
      end { If }
      else begin
        substrings.Add(S);
        Break;
      end; { Else }
    until Length(S) = 0;
  end;
end; { SplitString2 }

function DirIsWriteable(S: string): Boolean;
var
  olderr: Cardinal;
  H: THandle;
begin
  S := IncludeTrailingPathDelimiter(S) + IntToHex(GetTickCount, 8) + '.TMP';
  olderr := SetErrormode(SEM_FAILCRITICALERRORS or
    SEM_NOOPENFILEERRORBOX);
  try
    H := CreateFile(Pchar(S),
           GENERIC_READ or GENERIC_WRITE,
           0,
           nil,
           CREATE_ALWAYS,
           FILE_ATTRIBUTE_NORMAL or FILE_FLAG_DELETE_ON_CLOSE,
           0);
    Result := H <> INVALID_HANDLE_VALUE;
    CloseHandle(H);
  except
    Result := False;
  end;
  SetErrormode(olderr);
end;

function MaxValue(const values: array of Extended): Extended;
var
  isFirst: Boolean;
  i: Integer;
begin
  Result := 0.0;
  isFirst := True;

  for i := Low(values) to High(values) do begin
    if isFirst then begin
      isFirst := False;
      Result := values[i];
    end { If }
    else if Result < values[i] then
      Result := values[i];
  end; { For }
end; { MaxValue }

function MinValue(const values: array of Extended): Extended;
var
  isFirst: Boolean;
  i: Integer;
begin
  Result := 0.0;
  isFirst := True;

  for i := Low(values) to High(values) do begin
    if isFirst then begin
      isFirst := False;
      Result := values[i];
    end { If }
    else if Result > values[i] then
      Result := values[i];
  end; { For }
end; { MinValue }

procedure FreeObject(var anObject: TObject);
var
  temp: TObject;
begin
  temp := anObject;
  anObject := nil;
  temp.Free;
end; { FreeObject }

function PadLeft(const S: string; toLength: Integer; withChar: Char):
    string;
begin { PadLeft }
  if Length(S) < toLength then
    Result := StringOfChar(withChar, toLength - Length(S)) + S
  else
    Result := S;
end; { PadLeft }

function PadRight(const S: string; toLength: Integer;
  withChar: Char): string;
begin { PadRight }
  if Length(S) < toLength then
    Result := S + StringOfChar(withChar, toLength - Length(S))
  else
    Result := S;
end; { PadRight }

function RemoveFileExt(const pathname: string): string;
begin
  Result := ChangeFileExt(pathname, '');
end; { RemoveFileExt }

function ConcatSubstrings(const separator: string; substrings:
    TStrings): string;
var
  i: Integer;
begin
  if not Assigned(substrings) then
    raise EParameterCannotBeNil.Create('ConcatSubstrings', 'substrings');
  Result := '';
  for i := 0 to substrings.count - 1 do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + substrings[i];
  end; { For }
end;

function ConcatSubstrings(const separator: string; const substrings:
    array of string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(substrings) = 0  then
    Exit;
  for i := 0 to High(substrings) do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + substrings[i];
  end; { For }
end;

function ConcatQuotedSubstrings(separator, quote: Char; substrings:
    TStrings): string;
var
  i: Integer;
begin
  if not Assigned(substrings) then
    raise EParameterCannotBeNil.Create('ConcatQuotedSubstrings', 'substrings');
  Result := '';
  for i := 0 to substrings.count - 1 do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + AnsiQuotedStr(substrings[i], quote);
  end; { For }
end;

function ConcatQuotedSubstrings(separator, quote: Char; const substrings:
    array of string): string;
var
  i: Integer;
begin
  Result := '';
  if Length(substrings) = 0 then
    Exit;
  for i := 0 to High(substrings) do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + AnsiQuotedStr(substrings[i], quote);
  end; { For }
end;


{$IFDEF UNICODE}
function LoadStringFromFile(const filename: string; aEncoding: TEncoding = nil): string;
var
  fs: TFilestream;
  Bytes: TBytes;
  PreambleSize: Integer;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, fs.Size);
    fs.ReadBuffer(Bytes[0], Length(Bytes));
    PreambleSize:= TEncoding.GetBufferEncoding(Bytes, aEncoding);
    Result := aEncoding.GetString(Bytes, PreambleSize, Length(Bytes) - PreambleSize);
  finally
    fs.free
  end;
end;
{$ELSE}
function LoadStringFromFile(const filename: string): string;
var
  fs: TFilestream;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    fs.ReadBuffer(Result[1], Length(Result));
  finally
    fs.free
  end;
end;
{$ENDIF}

{$IFDEF UNICODE}
procedure SaveStringToFile(const S, filename: string; aEncoding: TEncoding = nil);
var
  Bytes: TBytes;
  fs: TFilestream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    if Length(S) > 0 then begin
      if not Assigned(aEncoding) then
        aEncoding := TEncoding.Default;
      Bytes := aEncoding.GetPreamble;
      if Length(Bytes) > 0 then
        fs.WriteBuffer(Bytes[0], Length(Bytes));
      Bytes := aEncoding.GetBytes(S);
      fs.WriteBuffer(Bytes[0], Length(Bytes));
    end; {if}
  finally
    fs.free
  end;
end;
{$ELSE}
procedure SaveStringToFile(const S, filename: string);
var
  fs: TFilestream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    if Length(S) > 0 then
      fs.WriteBuffer(S[1], Length(S));
  finally
    fs.free
  end;
end;
{$ENDIF}

procedure LoadStringsFromArray(list: TStrings; const A: array of
    string; clearlist: Boolean = true);
var
  i: Integer;
begin
  if not Assigned(list) then
    raise EParameterCannotBeNil.Create('LoadStringsFromArray', 'list');
  if clearlist then
    list.Clear;
  list.BeginUpdate;
  try
    for i := Low(A) to HIgh(A) do
      list.Add(A[i]);
  finally
    list.EndUpdate;
  end;
end;

function CountOfChar(ch: Char; const S: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = ch then
      Inc(Result);
end;

function CompareInt(i1, i2: Integer): Integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function CompareInt64(i1, i2: Int64): Integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function CompareFloat(f1, f2: Extended): Integer;
begin
  if Abs(f1 - f2) < Epsilon then
    Result := 0
  else if f1 < f2 then
    Result := -1
  else
    Result := 1;
end;

function TryStringToBoolean(const value: string; var b: Boolean):
  Boolean;
const
  BoolValues: array[Boolean] of string =
  ('"0","FALSE","NO","NEIN","N","FALSCH","F"',
    '"1","TRUE","YES","JA","Y","J","WAHR","W"');
var
  S: string;
begin
  S := '"' + AnsiUpperCase(value) + '"';
  Result := Pos(S, BoolValues[false]) > 0;
  if Result then
    b := False
  else begin
    Result := Pos(S, BoolValues[true]) > 0;
    if Result then
      b := true;
  end; { Else }
end;

function StringIn(const aString: string;
  const A: array of string;
  caseSensitive: Boolean = true): Boolean;
var
  i: Integer;
begin { StringIn }
  Result := false;
  if (aString <> '') and (Length(A) > 0) then
    for i := Low(A) to High(A) do begin
      if caseSensitive then
        Result := AnsiCompareStr(aString, A[i]) = 0
      else
        Result := AnsiCompareText(aString, A[i]) = 0;
      if Result then
        Break;
    end; { For }
end; { StringIn }

function IsCmdLineSwitch(const S: string): Boolean;
begin
  Result := (S <> '') and CharInSet(S[1], ['-', '/']);
end; {  }

function FindSwitch(switchchar: Char; var value: string): Boolean;
var
  I: Integer;
  S: string;
begin
  Result := false;
  value := '';
  for i := 1 to ParamCount do begin
    S := ParamStr(i);
    if IsCmdLineSwitch(S) and (Length(S) >= 2) and
      AnsiSameText(S[2], switchchar)
    then begin
      result := true;
      if Length(S) > 2 then begin
        value := Copy(S, 3, maxint);
        if value[1] = ':' then
          Delete(value, 1, 1);
        value := Trim(value);
        if (value <> '') and (value[1] = '"') then
          value := AnsiDequotedStr(value, '"');
      end; { If }
      Break;
    end; { If }
  end; { For }
end;

function ValueInArray(value: Integer; const A: array of Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(A) to HIgh(A) do
    if A[i] = value then begin
      Result := true;
      Exit;
    end; { If }
end;

function MixedCase(const S: string): string;
begin
  Result := AnsiLowerCase(Trim(S));
  if Result <> '' then
    Result[1] := AnsiUpperCase(Result[1])[1];
end;

procedure Split(const S: string; var firstpart, secondpart: string;
  const separator: string);
var
  n: Integer;
begin { Split }
  n := Pos(separator, S);
  if n > 0 then begin
    firstpart := Copy(S, 1, n - 1);
    secondpart := Copy(S, n + Length(separator), maxint);
  end
  else begin
    firstpart := S;
    secondpart := '';
  end;
end; { Split }

function ExtractStringElement(const Value: string; Index: Cardinal;
    Separator: Char = #9): string;
var
  i: Cardinal;
  Startpos, Endpos: Integer;
begin
  Startpos := 0;
  for i:= 1 to Index do begin
    Startpos := IScan( Separator, Value, Startpos+1 );
    if Startpos = 0 then begin
      Result := '';
      Exit
    end; { if }
  end; { for }
  Inc( Startpos );
  EndPos := IScan( Separator, Value, Startpos );
  if Endpos = 0 then
    Result := Copy( Value, Startpos, Maxint )
  else
    Result := Copy( Value, Startpos, EndPos-Startpos );
end;

function CountOfElements( const Value: string;
  Separator: Char ): Integer;
begin
  Result := CountOfChar( Separator, Value ) + 1;
end;

function SameMethod( M1, M2: TMethod ): Boolean;
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

procedure SplitStringEx(const S: String; List: TStrings;
  Separator: char = #9; QuoteChar: char = '"'; ClearList: Boolean = true );
var
  ScanState : (ssStart, ssReadItem, ssReadQuotedItem);
  CurrentIndex, TargetIndex: Integer;
  Buffer: string;
  CurrentChar: Char;

  procedure MalformedItemError;
  begin
    raise ESplitStringError.CreateFmt(
      'SplitStringEx: encountered imbalanced quotes in string <%s>',
      [S]);
  end;

begin
  if not Assigned(List) then
    raise EParameterCannotBeNil.Create('SplitStringEx', 'List');

  if ClearList then
    List.Clear;
  if S = '' then Exit;
  SetLength(Buffer, Length(S));
  CurrentIndex := 1;
  TargetIndex := 0;
  ScanState := ssStart;
  while CurrentIndex <= Length(S) do begin
    CurrentChar := S[CurrentIndex];
    if CurrentChar =  Separator then begin
      if ScanState <> ssReadQuotedItem then begin
        List.Add(Copy(Buffer, 1, TargetIndex));
        TargetIndex := 0;
        ScanState := ssStart;
      end {if}
      else begin
        Inc(TargetIndex);
        Buffer[TargetIndex] := Separator;
      end; {else}
    end {if separator}
    else if CurrentChar =  QuoteChar then begin
        case ScanState of
          ssStart:
            ScanState := ssReadQuotedItem;
          ssReadQuotedItem:
            if (CurrentIndex = Length(S)) or
              (S[CurrentIndex+1] = Separator)
            then
              ScanState := ssReadItem
            else begin
              Inc(CurrentIndex);
              if S[CurrentIndex] = QuoteChar then begin
                Inc(TargetIndex);
                Buffer[TargetIndex] := QuoteChar;
              end
              else
                MalformedItemError;
            end;
        else
          MalformedItemError;
        end; {case ScanState}
    end  {if QuoteChar}
    else begin
      if ScanState <> ssReadQuotedItem then
        ScanState := ssReadItem;
      Inc(TargetIndex);
      Buffer[TargetIndex] := CurrentChar;
    end; {else}
    Inc(CurrentIndex);
  end; {while}
  if ScanState = ssReadQuotedItem then
    MalformedItemError;
  List.Add(Copy(Buffer, 1, TargetIndex));
end;

function GetFirstNonBlankString( const Strings: array of string ): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Strings) to High(Strings) do
    if Trim(Strings[I]) <> '' then begin
      Result := Strings[I];
      Break;
    end; { if }
end;

function RemoveWhitespace(const S: string):string;
begin
  Result := RemoveCharsInSet(S, Whitespace);
end;

function DateString: string;
var
  year, month, day: Word;
begin
  DecodeDate(Sysutils.Date, year, month, day);
  Result := Format('%4.4d%2.2d%2.2d', [year, month, day]);
end; { DateString }

function TimeString: string;
var
  hour, min, sec, msec: Word;
begin
  DecodeTime(Sysutils.Time, hour, min, sec, msec);
  Result := Format('%2.2d%2.2d%2.2d', [hour, min, sec]);
end; { TimeString; }

procedure MakeTimestampedbackup(const filename, newdir: string);
var
  target: string;
begin
  if not FileExists(filename) then
    Exit;

  if Length(newdir) = 0 then
    target := ExtractFileDir(filename)
  else
    target := ExcludeTrailingPathDelimiter(newdir);

  if not Directoryexists(target) then begin
    if (Length(target) > 2) or (target[2] <> ':') then begin
      ForceDirectories(target);
    end; { If }
  end; { If }

  target := Format('%s\%s_%s_%s.BAK',
    [target,
     ChangeFileExt(ExtractFilename(filename), ''),
     DateString,
     TimeString
    ]);

  if not Windows.CopyFile(PChar(filename), PChar(target), false)
  then
    raise EFilesystemError.CreateFmt(eCopyFailed,
      [filename, target,
       SysErrorMessage(GetLastError)
      ]);
end; { MakeTimestampedbackup }

{$IFDEF UNICODE}
function RemoveCharsInSet(const S: string; const aSet: TCharset):string;
begin
  Result := RemoveCharsInSet(S, WidecharSets.Create(aSet));
end;

function RemoveCharsInSet(const S: string; const aSet: IWideCharset):string;
{$ELSE}
function RemoveCharsInSet(const S: string; const aSet: TCharset):string;
{$ENDIF}
var
  I: Integer;
begin
  Result := S;
  for I := Length(S) downto 1 do
{$IFDEF UNICODE}
    if aSet.Contains(S[I]) then
{$ELSE}
    if S[I] in aSet then
{$ENDIF}
      Delete(Result, I, 1);
end;

function ByteInRange(const Value, LowBound, HiBound: Byte): Boolean;
begin
  if LowBound <= HiBound then
    Result := (Value >= LowBound) and (Value <= HiBound)
  else
    Result := ByteInRange(Value, HiBound, LowBound);  
end;

procedure InitVars;
begin
{$IFDEF UNICODE}
   WhiteSpace := WidecharSets.Create([#0..' ']);
   WhiteSpace.MergeWith(WidecharSets.Whitespace);
{$ELSE}
   Whitespace := [#0..' '];
{$ENDIF}
end;

function FirstDigit(const S: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
    if CharInSet(S[I], Charsets.Digits) then begin
      Result := I;
      Break;
    end;
end;

function GetNumber(const S: string; StartAt: Integer):Integer;
var
  N: Integer;
begin
  N:= StartAt;
  while (N <= Length(S)) and CharInSet(S[N], Charsets.Digits) do
    Inc(N);
  if not TryStrToInt(Copy(S, StartAt, N-StartAt), Result) then
    Result := 0;
end;

function CompareNatural(aList: TStringlist; index1, index2: Integer): Integer;
var
  S1, S2: string;
  N1, N2: integer;
begin
  S1:= aList[index1];
  S2:= aList[index2];
  N1:= FirstDigit(S1);
  N2:= FirstDigit(S2);
  if (N1 = 0) or (N2 = 0) then
    Result := AnsiCompareStr(S1, S2)
  else begin
    Result :=  AnsiCompareStr(Copy(S1, 1, N1-1), Copy(S2, 1, N2-1));
    if Result = 0 then
      Result := CompareInt(GetNumber(S1, N1), GetNumber(S2, N2));
    if Result = 0 then
      Result := AnsiCompareStr(S1, S2)
  end; {else}
end;

procedure SortNatural(aList: TStringlist);
begin
  if not Assigned(aList) then
    raise EParameterCannotBeNil.Create('SortNatural','aList');

  aList.CustomSort(CompareNatural);
end;

function ClipIntToRange(value, minallowed, maxallowed: Integer):
  Integer;
begin
  if minallowed > maxallowed then
    raise EPreconditionViolation.Create('ClipIntToRange', 'minallowed is larger than maxallowed!');
  if value < minallowed then
    Result := minallowed
  else if value > maxallowed then
    Result := maxallowed
  else
    Result := value;
end; { ClipIntToRange }

function IntMin(i1, i2: Integer): Integer;
begin
  if i1 < i2 then
    Result := i1
  else
    Result := i2;
end; { IntMin }

{: Returns the larger of the two input values. }
function IntMax(i1, i2: Integer): Integer;
begin
  if i1 > i2 then
    Result := i1
  else
    Result := i2;
end; { IntMax }

function IIF(condition: Boolean; ifvalue, elsevalue: Variant): Variant;
begin
  if condition then
    Result := ifvalue
  else
    Result := elsevalue;
end; { IIF }

procedure PostKeyEx32(key: Word; const shift: TShiftState;
  specialkey: Boolean);
type
  TShiftKeyInfo = record
    shift: Byte;
    vkey: Byte;
  end;
  byteset = set of 0..7;
const
  shiftkeys: array[1..3] of TShiftKeyInfo =
  ((shift: Ord(ssCtrl); vkey: VK_CONTROL),
    (shift: Ord(ssShift); vkey: VK_SHIFT),
    (shift: Ord(ssAlt); vkey: VK_MENU));
var
  flag: DWORD;
  bShift: ByteSet absolute shift;
  i: Integer;
begin
  for i := 1 to 3 do begin
    if shiftkeys[i].shift in bShift then
      keybd_event(shiftkeys[i].vkey,
        MapVirtualKey(shiftkeys[i].vkey, 0),
        0, 0);
  end; { For }
  if specialkey then
    flag := KEYEVENTF_EXTENDEDKEY
  else
    flag := 0;

  keybd_event(key, MapvirtualKey(key, 0), flag, 0);
  flag := flag or KEYEVENTF_KEYUP;
  keybd_event(key, MapvirtualKey(key, 0), flag, 0);

  for i := 3 downto 1 do begin
    if shiftkeys[i].shift in bShift then
      keybd_event(shiftkeys[i].vkey,
        MapVirtualKey(shiftkeys[i].vkey, 0),
        KEYEVENTF_KEYUP, 0);
  end; { For }
end; { PostKeyEx32 }

procedure SendTextToForegroundWindow(S: string);
var
  flags: TShiftState;
  vcode: word;
  ret: word;
  i, n: Integer;
  mask: word;
begin
  for i := 1 to Length(S) do begin
    ret := VkKeyScan(S[i]);
    vcode := Lobyte(ret);
    flags := [];
    mask := $100;
    for n := 1 to 3 do begin
      if (ret and mask) <> 0 then begin
        case mask of
          $100: Include(flags, ssShift);
          $200: Include(flags, ssCtrl);
          $400: Include(flags, ssAlt);
        end; { Case }
      end; { If }
      mask := mask shl 1;
    end; { For }
    PostKeyEx32(vcode, flags, false);
  end; { For }
end; { SendText }

{!
<summary>
 WaitForWindowchange wait for the foreground window to change.</summary>
<returns>
 true if the window changed inside 2 seconds, false if not.</returns>
<param name="win">
 is the current foreground window on entry and returns the new
  one on exit.</param>
}
function WaitForWindowchange(var win: HWND): Boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 1 to 20 do begin
    Sleep(100);
    if win <> GetForegroundWindow then
      win := GetForegroundWindow;
    if win <> 0 then Exit;
  end;
  Result := false;
end;

function FindFileOnPath(SPath: string; const mask: string): string;
var
  SRec: TSearchrec;
  res: Integer;
begin { FindFileOnPath }
  Result := '';
  if SPath = '' then
    Exit;
  SPath := IncludeTrailingPathDelimiter(SPath);
  if FindFirst(SPath + mask, faAnyfile, SRec) = 0 then try
    if (SREc.Attr and faDirectory) = 0 then begin
        { We have a winna! }
      Result := SPath + SRec.Name;
      Exit;
    end; { If }
  finally
    FindClose(SRec);
  end; { finally }

    { No joy yet, seach the subdirectories of SPath. }
  res := FindFirst(SPath + '*.*', faDirectory, SRec);
  if res = 0 then try
    while res = 0 do begin
      if ((Srec.attr and faDirectory) = faDirectory) and
        (SRec.Name <> '.') and
        (SRec.Name <> '..')
        then begin
        Result := FindFileOnPath(SPath + SRec.Name, mask);
        if Result <> '' then
          Exit;
      end; { If }
      res := FindNext(SRec);
    end; { While }
  finally
    FindClose(Srec);
  end;
end; { FindFileOnPath }

function FindComponentByTag(
  owner: TComponent; aClass: TComponentClass;
  tagvalue: Integer; recursive: boolean = false): TComponent;
var
  i: Integer;
begin
  if not Assigned(owner) then
    raise EParameterCannotBeNil.Create('FindComponentByTag', 'Owner');

  if not Assigned(aClass) then
    aClass := TComponent;

  for i := 0 to owner.ComponentCount - 1 do begin
    Result := owner.Components[i];
    if Result.InheritsFrom(aClass) and (Result.Tag = tagvalue) then
      Exit;
    if recursive and (Result.ComponentCount > 0) then begin
      Result := FindComponentByTag(Result, aClass, tagvalue, true);
      if Assigned(Result) then
        Exit;
    end; {if}
  end; { For }
  Result := nil;
end;


initialization
  InitVars;
end.
