{== EnumeratorIntfU ===================================================}
{! <summary>
 This interface defines a base interface for enumerators.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2006-03-13</para>
<para>Version 1.1 created 2009-01-26, added the EEnumeratorOutOfBounds
  exception class and checked D2009 compatibility.</para>
<para>Version 1.2 created 2009-08-01, changed docs to XML.</para>
<para>Last modified       2009-08-01</para>
</history>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
{======================================================================}

unit EnumeratorIntfU;
{$INCLUDE PBDEFINES.INC}
interface
uses
  SysUtils;

type
  {!
  <remarks>
   Base type for enumerators that do not support Count and Reset</remarks>
  }
  IBaseEnumerator = interface(IInterface)
  ['{72CA877E-5241-48E5-864F-94AA06A97911}']
    {!
    <summary>
     If there are more items to enumerate, move to the next one
     and return true, otherwise return false.</summary>
    }
    function MoveNext: Boolean;
  end;

  {!
  <remarks>
   Base interface for an enumerator. Specific decendents need to
   add a function Current: someType; that returns the item at the
   current position of the enumerator.</remarks>
  }
  IEnumerator = interface(IBaseEnumerator)
  ['{B22884F4-5D34-492D-B01E-34A5D33F159B}']
    {!
    <summary>
     Read accessor for the <see cref="Count"/> property.</summary>
    }
    function GetCount: Integer;
    {!
    <summary>
     Resets the enumerator to before the start of the list.</summary>
    }
    procedure Reset;

    {!
    <value>
     Returns the number of items the enumerator has in its list.</value>
    }
    property Count: Integer read GetCount;
  end;

  {!
  <remarks>
   A class can implement this interface to indicate that it support
   an enumerator. </remarks>
  }
  IEnumerable = interface(IInterface)
  ['{BCF96FF0-3A69-413B-AD4C-B5182B671D11}']
    {!
    <summary>
     Returns the enumerator instance to use. The instance will
     typically implement an enumerator with additional methods,
     based on IEnumerator. </summary>
    }
    function GetEnumerator: IEnumerator;
  end;

  {!
  <remarks>
   Abstract base class for enumerators. Descendents will typically
   add a new constructor which takes a kind of list or array as
   parameter, override the GetCount method, and add a new Current
   method to return the item the enumerator points at.</remarks>
  }
  TAbstractEnumerator = class {$IFDEF SUPPORTS_CLASS_ABSTRACT}abstract{$ENDIF}
   (TInterfacedObject, IBaseEnumerator, IEnumerator)
  private
    FCurrentIndex: Integer;
  protected
    {!
    <summary>
     Return the number of items the enumerator can enumerate.
     Descendents must override this method.</summary>
    }
    function GetCount: Integer; virtual; abstract;
    {!
    <summary>
     Raise an EEnumeratorOutOfBounds exception with the current index
     value.
     </summary>
    <param name="aProcname">
     is the name of the method that detected the out of bounds error,
     it is incorporated into the error message.</param>
    <exception cref="EEnumeratorOutOfBounds">
     is raised unconditionally. </exception>
    }
    procedure IndexOutOfBounds(const aProcname: string); virtual;
    {!
    <summary>
     Test if an index value is valid.</summary>
    <returns>
     true if the passed index in between 0 and GetCount-1, inclusive,
     false otherwise.</returns>
    <param name="aIndex">
     is the index to check.</param>
    }
    function IsValidIndex(Index: Integer): Boolean; virtual;
    {!
    <summary>
     Increment the current index to move to the next item to enumerate.</summary>
    <returns>
     true if the new index is valid, false if not.</returns>
    }
    function MoveNext: Boolean; virtual;
    {!
    <summary>
     Sets the current index to -1 to reset the enumerator to the starting
     state. </summary>
    }
    procedure Reset;
    {!
    <value>
     Returns the current index.</value>
    }
    property CurrentIndex: Integer read FCurrentIndex;
  public
    {!
    <summary>
     Initializes the current index to -1. Descendents will typically
     provide a different version of the constructor that takes the
     list/array to enumerate. These constructors must call this one!</summary>
    }
    constructor Create; virtual;
  end;

  {!
  <remarks>
   Use this exception class to report out of bounds errors in the
   implementation of your enumerators. </remarks>
  }
  EEnumeratorOutOfBounds = class(Exception);

implementation

resourcestring
  SEnumeratorOutOfBounds =
    '%s: Enumerator is out of bounds. Current index is %d, allowed are 0 to %d';


constructor TAbstractEnumerator.Create;
begin
  inherited;
  FCurrentIndex := -1;
end;

procedure TAbstractEnumerator.IndexOutOfBounds(const aProcname: string);
begin
  raise EEnumeratorOutOfBounds.CreateFmt(
    SEnumeratorOutOfBounds, [aProcname, FCurrentIndex, GetCount]);
end;

function TAbstractEnumerator.IsValidIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < GetCount);
end;

function TAbstractEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := IsValidIndex(FCurrentIndex);
end;

procedure TAbstractEnumerator.Reset;
begin
  FCurrentIndex := -1;
end;

end.
