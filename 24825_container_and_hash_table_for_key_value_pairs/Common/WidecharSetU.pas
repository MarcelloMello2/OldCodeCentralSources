{== WidecharSetU ======================================================}
{! <summary>
 This unit implements a class that can be used instead of Delphi character
 sets for wide character. </summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2009-02-09</para>
<para>Version 1.1 created 2010-01-03, adapted comment style to new
 convention.</para>
<para>Last modified       2010-01-03</para>
</history>
<remarks>
<para>To ease memory management the unit provides an interface-based
implementation of widechar sets with a factory singleton. The factory
can create sets from ANSI set of chars, strings, and from other sets,
either as a simple duplicate or through the standard set operations.</para>
<para>
The factory can also provide a number of pre-fabricated sets that are
frequently needed. These sets are immutable, they cannot be modified.</para>
<para>
The sets support methods to include and exclude wide characters, test
for character inclusion, as well as comparison and operations with
another set. Since interfaces do not support operator overloading all
this is implemented through methods.</para>
<para>
The WidecharSets factory is thread-safe but the widechar sets themselves
are not. A IWidecharSet interface can be shared between different threads
as long as they do not modify its content (basically only call the
Contains method). </para>
</remarks>
<copyright>Copyright 2009, 2010 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}

{$IFNDEF UNICODE}This unit requires Delphi 2009 or above!{$ENDIF}
unit WidecharSetU;
{$INCLUDE PBDEFINES.INC}
interface

uses Sysutils;

type
  {! <summary>This enumeration describes the possible ways two sets
    A and B can be related. </summary>
    <remarks>
    <list type="table">
      <listheader> <term>Value</term><description>Description</description>
      </listheader>
      <item><term>srEqual</term>
      <description>The two sets are equal, their union is the same as
        each set and their difference is the empty set.</description>
      </item>
      <item><term>srProperSubset</term>
        <description>A is a proper subset of B, all elements of A are
          also elements of B but B contains elements not in A. An empty
          set is always considered a subset of any non-empty set.</description>
      </item>
      <item><term>srProperSuperset</term>
        <description>A is a proper superset of B. All elements of B are
          also elements of A but A also contains elements not in B.</description>
        </item>
      <item><term>srOverlap</term>
        <description>A and B intersect but each also contains elements that
          are not in the other set. </description>
        </item>
      <item><term>srDisjunct</term>
        <description>A and B do not overlap, neither set is empty but their
          intersection is empty.</description>
        </item>
    </list> </remarks>}
  TSetRelation = (srEqual, srProperSubset, srProperSuperset, srOverlap,
    srDisjunct);

  {!
  <remarks>
   This is the public interface of a widechar set. The methods
   of this interface are not thread-safe in the current implementation.
   </remarks>
  }
  IWidecharSet =  interface(IInterface)
  ['{43B22A11-7526-419E-9D3C-041B5B1BA929}']
    {!
    <summary>
     IWidecharSet.Cardinality returns the number of elements in the
     set. </summary>
    <returns>
     the number of elements in the set.</returns>
    }
    function Cardinality: Integer;
    {!
    <summary>
     IWidecharSet.Clear sets the set to empty.</summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure Clear;
    {!
    <summary>
     Clone returns a deep copy of the current set. Cloning an immutable set
     yields a copy that is not immutable.</summary>
    }
    function Clone: IWidecharset;
    {!
    <summary>
     IWidecharSet.CompareWith determines the relation of the
     passed set with this set.      </summary>
    <returns>
     the set relation.  </returns>
    <param name="aSet">
     is the set to compare to the reference. This parameter cannot be
     nil.</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed for the parameter.</exception>
    <remarks>
     Two sets can be equal (contain the same elements), one
     can be a subset or superset of the other, they can intersect or
     not (be disjunct). Note that srIntersect is only returned if the
     intersecting sets do not have a more stringent relation, e.g.
     equal, sub- or superset.</remarks>
    }
    function CompareWith(aSet: IWidecharSet): TSetRelation;
    {!
    <summary>
     Contains tests if the passed character is in the set. </summary>
    <returns>
     true if the character is in the set, false if not. </returns>
    }
    function Contains(C: Widechar): Boolean;
    {!
    <summary>
     Exclude removes the passed character from the set. If the character
     is not in the set nothing will be done, this is not an error. </summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure Exclude(C: Widechar);
    {!
    <summary>
     IWidecharSet.ExclusiveOr performs a set xor operation.   </summary>
    <param name="aSet">
     is the set to combine with. This parameter cannot be nil.</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed as parameter.</exception>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    <remarks>
     After the operation the set will contain all characters in
     aSet that are not in the original set and all characters from the
     original set that are not in aSet. This is the equivalent of
     subtracting the intersection of both sets from their union. </remarks>
    }
    procedure ExclusiveOr(aSet: IWidecharSet);
    {!
    <summary>
     Include adds the (ANSI) characters in the passed set to the set. </summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    <param name="aCharset">
     contains the 1-byte characters to include.</param>
    <param name="aEncoding">
     is an optional encoding that identifies the codepage the characters
     in aCharset belong to. If this parameter is nil the current system
     codepage is assumed.</param>
    <remarks>
     If aCharset contains characters in the range #128..#255
     they will be converted to Unicode using the passed encoding. </remarks>
    }
    procedure Include(aCharset: TSysCharset; aEncoding: TEncoding = nil); overload;
    {!
    <summary>
     Include adds the passed character to the set. The character can
     already be in the set, this is not an error. </summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure Include(C: Widechar); overload;
    {!
    <summary>
     IWidecharSet.IncludeAll sets all bits in the set. Note that
     not all possible values for a Widechar are valid codepoints in the
     UCS-2 encoding. </summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure IncludeAll;
    {!
    <summary>
     IWidecharSet.IntersectWith performs an AND operation. As a
     result the set will contain all characters from the original set that
     are also present in the passed set.  </summary>
    <param name="aSet">
     is the set to intersect with. This parameter cannot be nil.</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed as parameter.</exception>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure IntersectWith(aSet: IWidecharSet);
    {!
    <summary>
     IWidecharSet.Invert performs a unary NOT on the set. As a result the
     set will now contain all characters it did not contain before and
     none of those it did contain before.  </summary>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure Invert;
    {!
    <summary>
     IWidecharSet.IsEmpty test if the set is empty.  </summary>
    <returns>
     true if the set is empty, false otherwise</returns>
    }
    function IsEmpty: Boolean;
    {!
    <summary>
     IWidecharSet.IsEqualTo checks if the passed set contains the
     same elements as this one.  </summary>
    <returns>
     true if both sets are in fact the same set or both contain
     the same elements, false otherwise. </returns>
    <param name="aSet">
     is the set to compare this one to. This parameter
     cannot be nil. </param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed as parameter.</exception>
    }
    function IsEqualTo(aSet: IWidecharSet): Boolean;
    {!
    <summary>
     IWidecharSet.IsImmutable checks if the set is immutable.  </summary>
    <returns>
     true if the set cannot be modified, false otherwise.  </returns>
    }
    function IsImmutable: Boolean;
    {!
    <summary>
     IWidecharSet.Matches checks whether all characters in the
     passed string are contained in the set or not. </summary>
    <returns>
     true if all characters in S are in the set, false if not. An
     empty string always matches a set.    </returns>
    }
    function Matches(const S: string): Boolean;
    {!
    <summary>
     IWidecharSet.MergeWith performs a set union operation.  </summary>
    <param name="aSet">
     is the set to merge with. This parameter cannot be nil.</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed as parameter.</exception>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    <remarks>
     After the operation the set will contain all characters in
     aSet in addition to those originally present.  </remarks>
    }
    procedure MergeWith(aSet: IWidecharSet);
    {!
    <summary>
     IWidecharSet.Subtract excludes all characters in the passed
     set from this set.</summary>
    <param name="aSet">
     contains the characters to remove. This parameter cannot be nil</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed as parameter.</exception>
    <exception cref="EWidecharSetError">
     If the set is immutable the method will raise an EWidecharSetError
     exception.</exception>
    }
    procedure Subtract(aSet: IWidecharSet);
  end;

  {!
  <summary>
   This is the public interface of the widechar set factory.</summary>
  <remarks>
   The factory offers methods to create a widechar set either
   de novo (empty), from ANSI character sets, Unicode strings, and other
   character sets. It also has a method to compare sets to determine
   how they are related. In addition the factory maintains a number of
   predefined, immutable sets that are frequently used in applications
   for input validation.
  <para>
   The methods of this interface are thread-safe in the current
   implementation.</para></remarks>
  }
  IWidecharSets = interface(IInterface)
  ['{6536A3BC-51A1-4B2B-AC08-3C66270A4A64}']
    {!
    <summary>
     IWidecharSets.Compare determines the relation between two sets. </summary>
    <returns>
     the set relation.  </returns>
    <param name="Set1">
     is the reference set. This parameter cannot be nil.  </param>
    <param name="Set2">
     is the set to compare to the reference. This parameter cannot be nil.</param>
    <exception cref="EWidecharSetParameterError">
     Is raised if nil is passed for one of the parameters.</exception>
    <remarks>
     Two sets can be equal (contain the same elements), one
     can be a subset or superset of the other, they can intersect or
     not (be disjunct). Note that srIntersect is only returned if the
     intersecting sets do not have a more stringent relation, e.g.
     equal, sub- or superset.</remarks>
    }
    function Compare(Set1, Set2: IWidecharset): TSetRelation;
    {!
    <summary>
     This overload of IWidecharSets.Create returns an empty set. The set
     can be modified.</summary>
    }
    function Create: IWidecharset; overload;
    {!
    <summary>
     This overload of IWidecharSets.Create returns a set containing the
     characters found in the passed string. The set can be modified. </summary>
    <param name="S">
     contains the characters to incude in the set. A character can appear
     more than once and the string can also be empty. This would create
     an empty set.</param>
    }
    function Create(const S: string): IWidecharset; overload;
    {!
    <summary>
     This overload of IWidecharSets.Create returns a deep copy of the
     passed set. </summary>
    <param name="aSet">
     is the set to copy. If nil is passed an empty set will be returned.</param>
    }
    function Create(aSet: IWidecharSet): IWidecharset; overload;
    {!
    <summary>
     This overload of IWidecharSets.Create returns a set based on a set
     of Ansichar. </summary>
    <param name="aCharset">
     contains the characters to include in the set. These are ANSI
     characters. Those in the range #0 to #127 will be copied
     as is, characters in the range #128 to #255 are supposed to be in the
     codepage identified by the aEncoding parameter and will be converted
     to their Unicode equivalents. </param>
    <param name="aEncoding">
     specifies which codepage the characters in the passed set belong to.
     If this parameter is nil the default (current system) codepage is
     assumed. </param>
    }
    function Create(const aCharset: TSyscharset; aEncoding: TEncoding =
        nil): IWidecharset; overload;

    {!
    <summary>
     IWidecharSets.Digits returns an immutable set containing
     the digits 0..9. </summary>
    }
    function Digits: IWidecharset;
    {!
    <summary>
     IWidecharSets.EditCommands returns an immutable set containing
     the control characters ^C, ^V, ^X and ^H (backspace). These are used
     by edit controls for clipboard operations and undo. </summary>
    }
    function EditCommands:IWidecharSet;
    {!
    <summary>
     IWidecharSets.ExclusiveOr returns a new set that is the combination
     of the two passed sets by an exclusive or operation. </summary>
    <remarks>
     If either of the source sets is nil a copy of the second
     set is returned. If both are nil an empty set is returned.
     The result set contains all characters present in one of the source
     sets but not the other.   </remarks>
    }
    function ExclusiveOr(Set1, Set2: IWidecharset): IWidecharset;
    {!
    <summary>
     IWidecharSets.FloatChars returns an immutable set containing
     the digits 0..9, the + and - sign, and the current locales decimal
     separator. </summary>
    }
    function FloatChars: IWidecharSet;
    {!
    <summary>
     IWidecharSets.HexNumerals returns an immutable set containing
     the characters 0 to 9, a to f, and A to F.  </summary>
    }
    function HexNumerals: IWidecharSet;
    {!
    <summary>
     IWidecharSets.IdentifierChars returns an immutable set containing
     the characters usually accepted in identifiers by most classic
     programming languages. These are the letters a to z, A to Z, the
     digits 0 to 9, and the underbar _.  </summary>
    }
    function IdentifierChars: IWidecharSet;
    {!
    <summary>
     IWidecharSets.Intersect returns a new set that is the
     intersection of the two passed sets. </summary>
    <remarks>
     If either of the source sets is nil an empty set is returned.
     The result set contains all characters present in both source sets.
    </remarks>
    }
    function Intersect(Set1, Set2: IWidecharset): IWidecharset;
    {!
    <summary>
     IWidecharSets.Invert returns a copy of the source set that has
     all bits flipped.  </summary>
    <returns>
     a set that contains all characters not included in the source
     set.  </returns>
    <param name="aSet">
     is the set to invert. This parameter cannot be nil.  </param>
    <exception cref="EWidecharSetParameterError">
     This exception is raised if the parameter is nil.</exception>
    }
    function Invert(aSet: IWidecharSet): IWidecharset;
    {!
    <summary>
     IWidecharSets.Latin1Letters returns an immutable set containing
     the Unicode equivalents of all letters from the Latin1 ANSI codepage.
    </summary>
    }
    function Latin1Letters:IWidecharSet;
    {!
    <summary>
     IWidecharSets.Latin1AlphaNum returns an immutable set containing
     the Unicode equivalents of all alphanumeric characters from the Latin1
     ANSI codepage. </summary>
    }
    function Latin1AlphaNum:IWidecharSet;
    {!
    <summary>
     IWidecharSets.Latin1Lowercase returns an immutable set containing
     the Unicode equivalents of all lower case letters from the Latin1
     ANSI codepage. </summary>
    }
    function Latin1Lowercase:IWidecharSet;
    {!
    <summary>
     IWidecharSets.Latin1Uppercase returns an immutable set containing
     the Unicode equivalents of all upper case letters from the Latin1
     ANSI codepage. </summary>
    }
    function Latin1Uppercase:IWidecharSet;
    {!
    <summary>
     IWidecharSets.Merge returns a new set that is the union of the
     two passed sets. </summary>
    <remarks>
     If either of the source sets is nil a copy of the other set
     will be returned. If both sets are nil an empty set is returned.
     The result set contains all characters from the source sets. </remarks>
    }
    function Merge(Set1, Set2: IWidecharset): IWidecharset;
    {!
    <summary>
     IWidecharSets.SciFloatChars returns an immutable set containing
     the digits 0..9, the + and - sign, the letters e and E, and the
     current locales decimal separator. </summary>
    }
    function SciFloatChars: IWidecharSet;
    {!
    <summary>
     IWidecharSets.Subtract returns a new set that is the difference of the
     two passed sets. </summary>
    <remarks>
     If the first set is nil an empty set is returned. If the  second set
     is nil a copy of the first set is returned. The result set contains
     all characters that are in Set1 but not in Set2.  </remarks>
    }
    function Subtract(Set1, Set2: IWidecharset): IWidecharset;
    {!
    <summary>
     IWidecharSets.Whitespace returns a set containing the tab, space,
     carriage return, linefeed, and Unicode characters used like a space
     (e.g. the nonbreaking space). </summary>
    <remarks>
     This set is not immutable since what an application considers whitespace
     may be different from the default set here. Changes made to the returned
     set will be reflected by subsequent calls to Whitespace, since the
     returned set is a singleton.  </remarks>
    }
    function Whitespace: IWidecharSet;
  end;

  EWidecharSetError = class(Exception);
  EWidecharSetParameterError = class(EWidecharSetError);

{! Returns the public interface of the widechar set factory. The function
 is thread-safe. }
function WidecharSets: IWidecharSets;

{These functions are convenience shortcuts to the factory methods of the
same name. The functions are thread-safe.}

{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Digits: IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function EditCommands:IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function FloatChars: IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function HexNumerals: IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function IdentifierChars: IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Latin1Letters:IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Latin1AlphaNum:IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Latin1Lowercase:IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Latin1Uppercase:IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function SciFloatChars: IWidecharSet;
{! Calls the <see cref="IWidecharSets"/> factory method of the same name. }
function Whitespace: IWidecharSet;

implementation

uses InterlockedOpsU, Charsets, CharactersU, PBLowLevelUtilsU;

resourcestring
  SParameterCannotBeNil = 'Precondition violated: parameter cannot be nil.';
  SCannotModifySet = 'Cannot modify an immutable WidecharSet';

const
  BitsPerLongword = 32;

{== private types =====================================================}

type
  // maps the bits in a longword
  TBitSet = set of 0..31;
  // maps words or widechars
  TWidecharMap = array [0..High(Word) div 32] of TBitSet;

  {! This interface gives us access to the internal TWidecharMap of
    a <see cref="TWidecharSet"/> . }
  IMapAccess = interface
    ['{9F2BEC03-68C3-484F-BAB7-F3EA0E8DD4BE}']
    {! Returns the address of the objects TWidecharmap. }
    function MapAddress: Pointer;
  end;

  {! Common base class for <see cref="TWidecharSet"/> and <see cref="TWidecharSets"/>.
   The only method it implements is used by the descendents to raise an exception
   if a nil parameter is passed to a method in violation of a precondition. }
  TWCBase = class(TInterfacedObject)
  protected
    {! Raises a <see cref="EWidecharSetParameterError"/> exception. }
    procedure ParameterCannotBeNilError;
  end;

  {! This class implements a set for wide characters. It should only be
   used through <see cref="IWidecharSet"/> references. See the documentation
   of the interfaces methods.}
  TWidecharSet = class(TWCBase, IWidecharSet, IMapAccess)
  private
    FImmutable: Boolean;
    FMap : TWidecharMap;
    procedure CannotModifySetError;
    function MapAddress: Pointer;
    function Matches(const S: string): Boolean;
    procedure MergeNonAsciiChars(const aCharset: TSyscharset; aEncoding:
        TEncoding = nil);
  public
    constructor Create(const S: string); overload;
    constructor Create(const aCharset: TSyscharset; aEncoding: TEncoding =
        nil; asImmutable: Boolean = false); overload;
    constructor Create(aMap: TWidecharMap); overload;
    procedure Add(S: String);
    function Cardinality: Integer;
    procedure Clear;
    function Clone: IWidecharset;
    function CompareWith(aSet: IWidecharSet): TSetRelation;
    function Contains(C: Widechar): Boolean;
    procedure Exclude(C: Widechar);
    procedure ExclusiveOr(aSet: IWidecharSet);
    procedure Include(aCharset: TSysCharset; aEncoding: TEncoding = nil);
        overload;
    procedure Include(C: Widechar); overload;
    procedure IncludeAll;
    procedure IntersectWith(aSet: IWidecharSet);
    procedure Invert;
    function IsEmpty: Boolean;
    function IsEqualTo(aSet: IWidecharSet): Boolean;
    function IsImmutable: Boolean;
    procedure MergeWith(aSet: IWidecharSet);
    procedure Subtract(aSet: IWidecharSet);
    property Immutable: Boolean read FImmutable write FImmutable;
  end;

  {! This class implements the factory for widechar sets. The unit
   provides a singleton instance of this class through the
   <see cref="WidecharSets"/> function. See the documentation of the
   <see cref="IWidecharSets"/> interface for a description of what
   this class offers.}
  TWidecharSets = class(TWCBase, IWidecharSets)
  private
    FAlphaNum: IWidecharset;
    FDigits: IWidecharset;
    FEditCommands: IWidecharset;
    FFloatChars: IWidecharset;
    FHexNumerals: IWidecharset;
    FIdentifierChars: IWidecharset;
    FLetters: IWidecharset;
    FLowerCaseLetters: IWidecharset;
    FSciFloatChars: IWidecharset;
    FUpperCaseLetters: IWidecharset;
    FWhiteSpace: IWidecharset;
  protected
  public
    constructor CreateInstance;
    function Compare(Set1, Set2: IWidecharset): TSetRelation;
    function Create(const S: string): IWidecharset; overload;
    function Create(aSet: IWidecharSet): IWidecharset; overload;
    function Create(const aCharset: TSyscharset; aEncoding: TEncoding =
        nil): IWidecharset; overload;
    function Create: IWidecharset; overload;
    function Digits: IWidecharset;
    function EditCommands: IWidecharSet;
    function ExclusiveOr(Set1, Set2: IWidecharset): IWidecharset;
    function FloatChars: IWidecharSet;
    function HexNumerals: IWidecharSet;
    function IdentifierChars: IWidecharSet;
    function Intersect(Set1, Set2: IWidecharset): IWidecharset;
    function Invert(aSet: IWidecharSet): IWidecharset;
    function Latin1AlphaNum: IWidecharSet;
    function Latin1Letters: IWidecharSet;
    function Latin1Lowercase: IWidecharSet;
    function Latin1Uppercase: IWidecharSet;
    function Merge(Set1, Set2: IWidecharset): IWidecharset;
    function ReturnOrCreateImmutableSet(const aCharset: TSyscharset; var
        aField: IWidecharSet): IWidecharset;
    function SciFloatChars: IWidecharSet;
    function Subtract(Set1, Set2: IWidecharset): IWidecharset;
    function Whitespace: IWidecharSet;
  end;

var
  InternalWidecharSets: IWidecharSets = nil;

{== Exported functions ================================================}

function WidecharSets: IWidecharSets;
var
  P: TObject;
begin
  if Assigned(InternalWidecharSets) then
    Result := InternalWidecharSets
  else begin
    Result := TWidecharSets.CreateInstance;
    Result._AddRef; // the call below does not increment the refcount!
    P:= InterlockedCompareExchangeObject(InternalWidecharSets,
      TObject(Pointer(Result)), nil);
    if P <> nil then begin
      Result._Release;
      Result := InternalWidecharSets;
    end; {if}
  end; {else}
end; {WidecharSets}

function Digits: IWidecharset;
begin Result := Widecharsets.Digits end;

function FloatChars: IWidecharSet;
begin Result := Widecharsets.FloatChars end;

function SciFloatChars: IWidecharSet;
begin Result := Widecharsets.SciFloatChars end;

function IdentifierChars: IWidecharSet;
begin Result := Widecharsets.IdentifierChars end;

function HexNumerals: IWidecharSet;
begin Result := Widecharsets.HexNumerals end;

function EditCommands:IWidecharSet;
begin Result := Widecharsets.EditCommands end;

function Latin1Letters:IWidecharSet;
begin Result := Widecharsets.Latin1Letters end;

function Latin1AlphaNum:IWidecharSet;
begin Result := Widecharsets.Latin1AlphaNum end;

function Latin1Lowercase:IWidecharSet;
begin Result := Widecharsets.Latin1Lowercase end;

function Latin1Uppercase:IWidecharSet;
begin Result := Widecharsets.Latin1Uppercase end;

function Whitespace: IWidecharSet;
begin Result := Widecharsets.Whitespace end;


{== TWCBase ===========================================================}

procedure TWCBase.ParameterCannotBeNilError;
begin
  raise EWidecharSetParameterError.Create(SParameterCannotBeNil);
end;

{== TWidecharSets =====================================================}

function TWidecharSets.Create(aSet: IWidecharSet): IWidecharset;
begin
  if Assigned(aSet) then
    Result := aSet.Clone
  else
    Result := Create
end;

constructor TWidecharSets.CreateInstance;
begin
  inherited Create;
end;

function TWidecharSets.Compare(Set1, Set2: IWidecharset): TSetRelation;
begin
  if not (Assigned(Set1) and Assigned(Set2)) then
    ParameterCannotBeNilError;

  Result := Set1.CompareWith(Set2);
end;

function TWidecharSets.Create(const S: string): IWidecharset;
begin
  Result := TWidecharSet.Create(S) as IWidecharSet;
end;

function TWidecharSets.Create(const aCharset: TSyscharset; aEncoding:
    TEncoding = nil): IWidecharset;
begin
  Result := TWidecharSet.Create(aCharset, aEncoding) as IWidecharSet;
end;

function TWidecharSets.Create: IWidecharset;
begin
  Result := TWidecharSet.Create('') as IWidecharSet;
end;

function TWidecharSets.Digits: IWidecharset;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.Digits, FDigits);
end;

function TWidecharSets.EditCommands: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.EditOperations, FEditCommands);
end;

function TWidecharSets.ExclusiveOr(Set1, Set2: IWidecharset):
    IWidecharset;
begin
  if Assigned(Set1)then begin
    Result := Set1.Clone;
    if Assigned(Set2) then
      Result.ExclusiveOr(Set2);
  end {if}
  else if Assigned(Set2) then
    Result := Set2.Clone
  else
    Result:= Create;
end;

function TWidecharSets.FloatChars: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.FloatChars, FFloatChars);
end;

function TWidecharSets.HexNumerals: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.HexNumerals, FHexNumerals);
end;

function TWidecharSets.IdentifierChars: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.IdentifierChars, FIdentifierChars);
end;

function TWidecharSets.Intersect(Set1, Set2: IWidecharset):
    IWidecharset;
begin
  if not (Assigned(Set1) and Assigned(Set2)) then
    Result:= Create
  else begin
    Result := Set1.Clone;
    Result.IntersectWith(Set2);
  end; {else}
end;

function TWidecharSets.Invert(aSet: IWidecharSet): IWidecharset;
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  Result := aSet.Clone;
  Result.Invert;
end;

function TWidecharSets.Latin1Letters: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.Letters, FLetters);
end;

function TWidecharSets.Latin1AlphaNum: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.AlphaNum, FAlphaNum);
end;

function TWidecharSets.Latin1Lowercase: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.LowerCaseLetters, FLowerCaseLetters);
end;

function TWidecharSets.Latin1Uppercase: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.UpperCaseLetters, FUpperCaseLetters);
end;

function TWidecharSets.Merge(Set1, Set2: IWidecharset): IWidecharset;
begin
  if Assigned(Set1) then begin
    Result := Set1.Clone;
    if Assigned(Set2) and (Set1 <> Set2) then
      Result.MergeWith(Set2);
  end {if}
  else
    if Assigned(Set2) then
      Result := Set2.Clone
    else
      Result := Create;
end;

function TWidecharSets.ReturnOrCreateImmutableSet(const aCharset:
    TSyscharset; var aField: IWidecharSet): IWidecharset;
var
  encoding: TEncoding;
  P: TObject;
begin
  if Assigned(aField) then
    Result := aField
  else begin
    encoding := TEncoding.GetEncoding(Latin1Codepage);
    try
      Result := TWidecharSet.Create(aCharset, encoding, true) as IWidecharSet;
      Result._AddRef; // the call below does not increment the refcount!
      P:= InterlockedCompareExchangeObject(aField,TObject(Pointer(Result)), nil);
      if P <> nil then begin
        Result._Release;
        Result := aField;
      end; {if}
    finally
      encoding.Free;
    end; {finally}
  end; {else}
end;

function TWidecharSets.SciFloatChars: IWidecharSet;
begin
  Result := ReturnOrCreateImmutableSet(Charsets.SciFloatChars, FSciFloatChars);
end;

function TWidecharSets.Subtract(Set1, Set2: IWidecharset): IWidecharset;
begin
  if not Assigned(Set1) then
    Result := Create
  else begin
    Result := Set1.Clone;
    if Assigned(Set2) then
      Result.Subtract(Set2);
  end; {else}
end;

function TWidecharSets.Whitespace: IWidecharSet;
const
  UnicodeSpaces =
    #$0009#$000A#$000B#$000C#$000D#$0020+
    #$0085#$00A0#$1680#$180E#$2000#$2001#$2002#$2003#$2004#$2005#$2006+
    #$2007#$2008#$2009#$200A#$2028#$2029#$202F#$205F#$3000;
  {according to http://en.wikipedia.org/wiki/Whitespace_(computer_science)}
var
  P: TObject;
begin
  if Assigned(FWhiteSpace) then
    Result := FWhiteSpace
  else begin
    Result := TWidecharSet.Create(UnicodeSpaces) as IWidecharSet;
    Result._AddRef; // the call below does not increment the refcount!
    P:= InterlockedCompareExchangeObject(FWhiteSpace,TObject(Pointer(Result)), nil);
    if P <> nil then begin
      Result._Release;
      Result := FWhiteSpace;
    end; {if}
  end; {else}
end;

{== TWidecharSet ======================================================}

constructor TWidecharSet.Create(const S: string);
begin
  inherited Create;
  Add(S);
end;

constructor TWidecharSet.Create(const aCharset: TSyscharset; aEncoding:
    TEncoding = nil; asImmutable: Boolean = false);
begin
  inherited Create;
  Assert(Sizeof(aCharset) = 32);

  {The characters in the range #0 to #127 are supposed to be codepage-
   neutral, so we can copy these bits en bloc.}
  Move(aCharset, FMap, 16);
  MergeNonAsciiChars(aCharset, aEncoding);
  FImmutable := asImmutable;
end;

constructor TWidecharSet.Create(aMap: TWidecharMap);
begin
  inherited Create;
  FMap := aMap;
end;

procedure TWidecharSet.Add(S: String);
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    Include(S[I]);
end;

procedure TWidecharSet.CannotModifySetError;
begin
  raise EWidecharSetError.Create(SCannotModifySet);
end;

function TWidecharSet.Cardinality: Integer;
begin
  Result := CountBits(@FMap, Length(FMap) * Sizeof(FMap[0]));
end;

procedure TWidecharSet.Clear;
begin
  if not FImmutable then
    DWordFill(0, @Fmap, Length(FMap))
  else
    CannotModifySetError;
end;

function TWidecharSet.Clone: IWidecharset;
begin
  Result := TWidecharSet.Create(FMap) as IWidecharSet;
end;

function TWidecharSet.CompareWith(aSet: IWidecharSet): TSetRelation;
var
  Temp: IWidecharset;
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;

  if IsEqualTo(aSet) then
    Result := srEqual
  else if aSet.IsEmpty then
    Result := srProperSuperset
  else if IsEmpty then
    Result := srProperSubset
  else begin
    Temp := Clone;
    Temp.Subtract(aSet);
    if Temp.IsEmpty then
      Result := srProperSubset
    else if (Temp.Cardinality + aSet.Cardinality) = Self.Cardinality then
      Result := srProperSuperset
    else if IsEqualTo(Temp) then
      Result := srDisjunct
    else
      Result := srOverlap;
  end; {else}
end;

function TWidecharSet.Contains(C: Widechar): Boolean;
begin
  Result := (Ord(C) mod BitsPerLongword) In (FMap[Ord(C) div BitsPerLongword]);
end;

procedure TWidecharSet.Exclude(C: Widechar);
begin
  if not FImmutable then
    System.Exclude(FMap[Ord(C) div BitsPerLongword], Ord(C) mod BitsPerLongword)
  else
    CannotModifySetError;
end;

procedure TWidecharSet.ExclusiveOr(aSet: IWidecharSet);
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  if not FImmutable then
    MergeXOr(MapAddress, (aSet as IMapAccess).MapAddress, Length(FMap))
  else
    CannotModifySetError;
end;

procedure TWidecharSet.Include(C: Widechar);
begin
  if not FImmutable then
    System.Include(FMap[Ord(C) div BitsPerLongword], Ord(C) mod BitsPerLongword)
  else
    CannotModifySetError;
end;

procedure TWidecharSet.Include(aCharset: TSysCharset; aEncoding:
    TEncoding = nil);
begin
  if not FImmutable then begin
    // #0 to #127 are the same for ANSI and Unicode, merge directly
    MergeOr(MapAddress, @aCharset, 4);
    MergeNonAsciiChars(aCharset, aEncoding);
  end {if}
  else
    CannotModifySetError;
end;

procedure TWidecharSet.IncludeAll;
begin
  if not FImmutable then
    DWordFill($FFFFFFFF, @Fmap, Length(FMap))
  else
    CannotModifySetError;
end;

procedure TWidecharSet.IntersectWith(aSet: IWidecharSet);
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  if not FImmutable then
    MergeAnd(MapAddress, (aSet as IMapAccess).MapAddress, Length(FMap))
  else
    CannotModifySetError;
end;

procedure TWidecharSet.Invert;
begin
  if not FImmutable then
    FlipDwordBits(@FMap, Length(FMap))
  else
    CannotModifySetError;
end;

function TWidecharSet.IsEmpty: Boolean;
begin
  Result := not AnyBitSet(@FMap, Length(FMap));
end;

function TWidecharSet.IsEqualTo(aSet: IWidecharSet): Boolean;
var
  p: Pointer;
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  p := (aSet as IMapAccess).MapAddress;
  Result := p = MapAddress;
  if not Result  then
    Result := CompareMem(MapAddress, p, Sizeof(FMap));
end;

function TWidecharSet.IsImmutable: Boolean;
begin
  Result := FImmutable;
end;

function TWidecharSet.MapAddress: Pointer;
begin
  Result := @FMap;
end;

function TWidecharSet.Matches(const S: string): Boolean;
var
  I: Integer;
begin
  Result := true;
  for I := 1 to Length(S) do
    if not Contains(S[I]) then begin
      Result := false;
      Break;
    end; {if}
end;

procedure TWidecharSet.MergeNonAsciiChars(const aCharset: TSyscharset;
    aEncoding: TEncoding = nil);
var
  Bytes: TBytes;
  C: AnsiChar;
  N: Integer;
  I: Integer;
begin
  N := CountBits(PByte(@aCharset)+16, 16);
  if N > 0 then begin
    {We assume characters > #127 in the set to be in the codepage identified
     by the passed encoding object, so we need to convert them to Unicode.}
    if not Assigned(aEncoding) then
      aEncoding := TEncoding.Default;
    SetLength(Bytes, N);
    I:= 0;
    for C := #128 to #255 do begin
      if C In aCharset then begin
        Assert(I < Length(Bytes));
        Bytes[I] := Byte(C);
        Inc(I);
      end; {if}
    end; {for}
     Bytes := TEncoding.Convert(aEncoding, TEncoding.Unicode, Bytes);
     Add(TEncoding.Unicode.GetString(Bytes));
  end; {if};
end;

procedure TWidecharSet.MergeWith(aSet: IWidecharSet);
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  if not FImmutable then
    MergeOr(MapAddress, (aSet as IMapAccess).MapAddress, Length(FMap))
  else
    CannotModifySetError;
end;

procedure TWidecharSet.Subtract(aSet: IWidecharSet);
begin
  if not Assigned(aSet) then
    ParameterCannotBeNilError;
  if not FImmutable then
    MergeNot(MapAddress, (aSet as IMapAccess).MapAddress, Length(FMap))
  else
    CannotModifySetError;
end;

initialization
finalization
  InternalWidecharSets := nil;
end.
