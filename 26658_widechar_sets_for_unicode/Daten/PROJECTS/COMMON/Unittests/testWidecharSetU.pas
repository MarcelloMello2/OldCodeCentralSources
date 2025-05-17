unit testWidecharSetU;

interface

uses TestFramework, WidecharsetU;

type
  TWidecharSetTest = class(TTestcase)

  private
    FFactory: IWidecharsets;
    procedure CheckDigitSet(Inst: IWidecharset);
  protected
    property Factory: IWidecharsets read FFactory;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testCardinality;
    procedure TestClear;
    procedure TestClone;
    procedure TestCompare;
    procedure TestConstructionFromSet;
    procedure TestDigits;
    procedure TestExclude;
    procedure TestExclusiveOr;
    procedure TestFactoryConstruction;
    procedure TestInclude;
    procedure TestIncludeAll;
    procedure testIncludeAnsiSet;
    procedure TestIntersect;
    procedure TestInvert;
    procedure TestIsEmpty;
    procedure TestMerge;
    procedure TestNonAsciiCharacters;
    procedure TestSubtract;
  end;

implementation

uses
  Charsets, SysUtils;

const
  TestChars = 'abcÄÖÜБьφϊκ'#$FFFE;


procedure TWidecharSetTest.CheckDigitSet(Inst: IWidecharset);
var
  C: Char;
begin
  CheckNotNull(Inst);
  for C := '0' to '9' do
    CheckTrue(Inst.Contains(C));
  for C := 'a' to 'z' do
    CheckFalse(Inst.Contains(C));
end;

procedure TWidecharSetTest.SetUp;
begin
  inherited;
  FFactory := WidecharSets;
end;

procedure TWidecharSetTest.TearDown;
begin
  FFactory := nil;
  inherited;
end;

procedure TWidecharSetTest.testCardinality;
var
  Inst: IWidecharset;
begin
  Inst := Factory.Create;
  CheckEquals(0, Inst.Cardinality);
  Inst := Factory.Create(TestChars);
  CheckEquals(Length(TestChars), Inst.Cardinality);
end;

procedure TWidecharSetTest.TestClear;
var
  Inst1, Inst2: IWidecharset;
begin
  Inst1 := Factory.Digits;
  Inst2 := Inst1.Clone;
  Inst2.Include(#$FFFE);
  CheckFalse(Inst2.IsEmpty);
  Inst2.Clear;
  CheckTrue(Inst2.IsEmpty);
  ExpectedException := EWidecharSetError;
  Inst1.Clear;
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestClone;
var
  Inst1, Inst2: IWidecharset;
  C: Char;
begin
  Inst1 := Factory.Digits;
  Inst2 := Inst1.Clone;
  CheckNotNull(Inst2);
  for C := '0' to '9' do
    CheckTrue(Inst2.Contains(C));
  CheckTrue(Inst1.IsImmutable);
  CheckFalse(Inst2.IsImmutable);
end;

procedure TWidecharSetTest.TestCompare;
var
  Inst1: IWidecharset;
begin
  Inst1 := Factory.Create(TestChars);
  // a set should be equal with itself.
  CheckTrue(Factory.Compare(Inst1, Inst1) = srEqual, 'same set');
  // a set should also be equal with a copy of itself.
  CheckTrue(Factory.Compare(Inst1, Inst1.Clone) = srEqual, 'equal set');
  // the digits set is a subset of the hexnumerals set
  CheckTrue(Factory.Compare(Factory.Digits, Factory.HexNumerals) = srProperSubset, 'subset');
  // the SciFloatChars set is a superset of the digits set
  CheckTrue(Factory.Compare(Factory.SciFloatChars, Factory.Digits) = srProperSuperset, 'superset');
  // the hexnumerals set overlaps with the testchars set
  CheckTrue(Factory.Compare(Factory.HexNumerals, Inst1) = srOverlap, 'overlap');
  // the digits set is disjunct with the testchars set
  CheckTrue(Factory.Compare(Factory.Digits, Inst1) = srDisjunct, 'disjunct sets');

  ExpectedException := EWidecharSetParameterError;
  Factory.Compare(nil, Inst1);
end;

procedure TWidecharSetTest.TestConstructionFromSet;
var
  Inst: IWidecharset;
begin
  Inst := Factory.Create(Charsets.Digits);
  CheckDigitSet(Inst);
end;

procedure TWidecharSetTest.TestDigits;
var
  Inst1, Inst2: IWidecharset;
begin
  CheckDigitSet(WidecharSetU.Digits);
  Inst1:= WidecharSetU.Digits;
  Inst2:= WidecharSetU.Digits;
  CheckSame(Inst1, Inst2);
  CheckTrue(Inst1.IsImmutable);
  ExpectedException := EWidecharSetError;
  Inst1.Include('A');
end;

procedure TWidecharSetTest.TestExclude;
var
  Inst: IWidecharset;
  I: Integer;
begin
  Inst := Factory.Create(TestChars);
  CheckNotNull(Inst);
  for I := 1 to Length(TestChars) do begin
    CheckTrue(Inst.Contains(TestCHars[I]));
    Inst.Exclude(TestCHars[I]);
    CheckFalse(Inst.Contains(TestCHars[I]));
  end; {for}
  ExpectedException := EWidecharSetError;
  Factory.Digits.Exclude('0');
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestExclusiveOr;
var
  I: Integer;
  Inst1, Inst2, Inst3: IWidecharset;
  S, Remainder: string;
begin
  S := Copy(TestChars, 1, 5);
  Remainder := Copy(TestChars, 6, Maxint);
  Inst1 := Factory.Create(S);
  Inst2 := Factory.Create(TestChars);
  Inst3 := Factory.ExclusiveOr(Inst1, Inst2);
  for I := 1 to Length(S) do
      CheckFalse(Inst3.Contains(S[I]));
  for I := 1 to Length(Remainder) do
      CheckTrue(Inst3.Contains(Remainder[I]));
  ExpectedException := EWidecharSetError;
  Factory.Digits.ExclusiveOr(Inst1);
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestFactoryConstruction;
begin
  CheckNotNull(Factory);
end;

procedure TWidecharSetTest.TestInclude;
var
  Inst: IWidecharset;
  I: Integer;
begin
  Inst := Factory.Create;
  CheckNotNull(Inst);
  for I := 1 to Length(TestChars) do begin
    Inst.Include(TestCHars[I]);
    CheckTrue(Inst.Contains(TestCHars[I]));
  end; {for}
  ExpectedException := EWidecharSetError;
  Factory.Digits.Include('A');
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestIncludeAll;
var
  Inst: IWidecharSet;
  C: Char;
begin
  Inst := Factory.Create;
  CheckTrue(Inst.IsEmpty);
  Inst.IncludeAll;
  CheckFalse(Inst.IsEmpty);
  for C := Low(C) to High(C) do
    CheckTrue(Inst.Contains(C), C);
  ExpectedException := EWidecharSetError;
  Factory.Digits.IncludeAll;
  Fail('Should not get here');
end;

procedure TWidecharSetTest.testIncludeAnsiSet;
const
  TestSet = ['a'..'f','ä','ö','ü'];
var
  encoding: TEncoding;
  Inst1, Inst2: IWidecharset;
begin
  encoding:= TEncoding.GetEncoding(Latin1Codepage);
  try
    Inst1 := Factory.Create(TestSet, encoding);
    Inst2 := Factory.Create;
    CheckEquals(0, Inst2.Cardinality, 'Cardinality = 0');
    Inst2.Include(TestSet, encoding);
    CheckTrue(Inst1.IsEqualTo(Inst2));
  finally
    encoding.Free;
  end; {finally}
  ExpectedException := EWidecharSetError;
  Factory.Digits.Include(TestSet);
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestIntersect;
var
  I: Integer;
  Inst1, Inst2, Inst3: IWidecharset;
  S, Remainder: string;
begin
  S := Copy(TestChars, 1, 5);
  Remainder := Copy(TestChars, 6, Maxint);
  Inst1 := Factory.Create(S);
  Inst2 := Factory.Create(TestChars);
  Inst3 := Factory.Intersect(Inst1, Inst2);
  for I := 1 to Length(S) do
      CheckTrue(Inst3.Contains(S[I]));
  for I := 1 to Length(Remainder) do
      CheckFalse(Inst3.Contains(Remainder[I]));
  ExpectedException := EWidecharSetError;
  Factory.Digits.IntersectWith(Inst1);
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestInvert;
var
  Inst: IWidecharSet;
begin
  Inst := Factory.Create;
  Inst.IncludeAll;
  CheckFalse(Inst.IsEmpty);
  Inst.Invert;
  CheckTrue(Inst.IsEmpty);
  ExpectedException := EWidecharSetError;
  Factory.Digits.Invert;
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestIsEmpty;
var
  Inst: IWidecharSet;
begin
  Inst := Factory.Create;
  CheckTrue(Inst.IsEmpty, 'Test 1');
  Inst.Include(#$FFFE);
  CheckFalse(Inst.IsEmpty, 'Test 2');
  Inst.Exclude(#$FFFE);
  CheckTrue(Inst.IsEmpty, 'Test 3');
end;

procedure TWidecharSetTest.TestMerge;
var
  I: Integer;
  Inst1, Inst2, Inst3: IWidecharset;
begin
  Inst1 := Factory.Create(Copy(TestChars, 1, 5));
  Inst2 := Factory.Create(Copy(TestChars, 6, Maxint));
  Inst3 := Factory.Merge(Inst1, Inst2);
  for I := 1 to Length(TestChars) do
      CheckTrue(Inst3.Contains(TestChars[I]));
  ExpectedException := EWidecharSetError;
  Factory.Digits.MergeWith(Inst1);
  Fail('Should not get here');
end;

procedure TWidecharSetTest.TestNonAsciiCharacters;
const
  S: String='0123456789äöüßÄ';
var
  encoding: TEncoding;
  I: Integer;
  Inst: IWidecharset;
begin
  encoding:= TEncoding.GetEncoding(Latin1Codepage);
  try
    Inst := Factory.Create(['0'..'9','ä','ö','ü','ß','Ä'], encoding);
    CheckNotNull(Inst);
    for I := 1 to Length(S) do
      CheckTrue(Inst.Contains(S[I]), 'Character: '+S[I]);
  finally
    encoding.Free;
  end; {finally}
end;

procedure TWidecharSetTest.TestSubtract;
var
  I: Integer;
  Inst1, Inst2, Inst3: IWidecharset;
  S, Remainder: string;
begin
  S := Copy(TestChars, 1, 5);
  Remainder := Copy(TestChars, 6, Maxint);
  Inst1 := Factory.Create(TestChars);
  Inst2 := Factory.Create(Remainder);
  Inst3 := Factory.Subtract(Inst1, Inst2);
  for I := 1 to Length(S) do
    CheckTrue(Inst3.Contains(S[I]));
  for I := 1 to Length(Remainder) do
    CheckFalse(Inst3.Contains(Remainder[I]));
  ExpectedException := EWidecharSetError;
  Factory.Digits.Subtract(Inst1);
  Fail('Should not get here');
end;

initialization
  RegisterTest('',TWidecharSetTest.Suite);
end.

