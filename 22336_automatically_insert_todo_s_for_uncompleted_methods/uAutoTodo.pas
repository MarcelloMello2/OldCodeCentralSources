unit uAutoTodo;

interface

{/$/DEFINE TEST}

uses
  Windows, SysUtils, Classes, ToolsAPI, ActnList, Menus, ImgList, Controls,
  Graphics;

type

  {!~TAutoTodo facilitates automatic insertion of TODO items for each
  empty begin/end block in the current unit }
  TAutoTodo = class(TDataModule)
    ActionList: TActionList;
    ActionEditInsertAutoTodoS: TAction;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MainMenuEdit: TMenuItem;
    MainMenuEditInsertAutoTodoS: TMenuItem;
    procedure ActionEditInsertAutoTodoSUpdate(Sender: TObject);
    procedure ActionEditInsertAutoTodoSExecute(Sender: TObject);
  private
    function GetWindowsUser: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {!~AddTodo adds to-do items for all empty begin/end blocks }
    function AddTodo(const Source: string): string; overload;

    {!~AddTodo generates a list of string inserts for all empty
    begin/end blocks. The objects part contains the original source positions
    where to insert (0-based) }
    procedure AddTodo(const Source: string; List: TStrings); overload;
  end;

var
  AutoTodo: TAutoTodo = nil;

procedure Register;

implementation

{$R *.dfm}

{$IFNDEF TEST}
uses
  uIdeUtils;
{$ENDIF}

const
  IdChars = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
  SepChars = [#9, #32, #13, #10];

type
  TRoutine = record
    Keyword: string;
    PrevPos,
    NextPos: Integer;
    PrevName,
    NextName: string;
  end;

  TStringArray = array of string;

  TKeywordPair = record
    BeginKey: string;
    EndKeys: TStringArray;
  end;

  TKeywordPairArray = array of TKeywordPair;

procedure Register;
var
  NtaServices: INTAServices;
  GlyphIndex: Integer;

  function CopyImage(const FromImageList, ToImageList: TCustomImageList;
    const FromIndex: Integer): Integer;
  begin
    Result := ToImageList.Count;
    ToImageList.AddImages(FromImageList);
  end; //CopyImage

begin //RegisterAction
  AutoTodo := nil;
  if not Supports(BorlandIDEServices, INTAServices, NtaServices) then Exit;
  AutoTodo := TAutoTodo.Create(nil);
  GlyphIndex := CopyImage(AutoTodo.ImageList, NtaServices.ImageList, 0);

  AutoTodo.MainMenuEdit.Delete(0);
  NtaServices.MainMenu.Items[1].Add(AutoTodo.MainMenuEditInsertAutoTodoS);
  AutoTodo.ActionEditInsertAutoTodoS.ImageIndex := GlyphIndex;
end;

{$IFDEF TEST}
procedure UnCommentSourceCode(var SourceCode: string;
  const ClearStrings: Boolean);
var
  I: Integer;
  Ch, Prev: Char;
  State: (sCode, sAccolade, sParentAsterisk, sDoubleSlash, sQuotedString);
begin
  Prev := #0;
  State := sCode;
  for I := 0 to Length(SourceCode) do
  begin
    Ch := SourceCode[I];
    case State of
      sCode:
        case Ch of
          '{': begin
                 State := sAccolade;
                 SourceCode[I] := #32;
               end;
          '''':begin
                 if ClearStrings then
                 begin
                   State := sQuotedString;
                   SourceCode[I] := #32;
                 end;
               end;
          '*': if Prev = '(' then
               begin
                 State := sParentAsterisk;
                 SourceCode[I] := #32;
                 SourceCode[I-1] := #32;
               end;
          '/': if Prev = '/' then
               begin
                 State := sDoubleSlash;
                 SourceCode[I] := #32;
                 SourceCode[I-1] := #32;
               end;
        end;
      sAccolade:
        begin
          if not (SourceCode[I] in [#13, #10]) then
            SourceCode[I] := #32;
          if Ch = '}' then
            State := sCode;
        end;

      sParentAsterisk:
        begin
          if not (SourceCode[I] in [#13, #10]) then
            SourceCode[I] := #32;
          if (Prev = '*') and (Ch = ')') then
            State := sCode;
        end;

      sDoubleSlash:
        begin
          if Ch = #13 then
            State := sCode
          else
            SourceCode[I] := #32;
        end;

      sQuotedString:
        begin
          SourceCode[I] := #32;
          if Ch = '''' then
            State := sCode;
        end;
    end;
    Prev := Ch;
  end; //for
end;
{$ENDIF}

function IsSubstrHere(const SubStr, S: string; I: Integer): Boolean;
var
  L, SubL: Integer;
  J: Integer;
begin
  J := 1;
  L := Length(S);
  SubL := Length(SubStr);
  Result := True;
  while (I <= L)  do
    if J > SubL then
      Exit
    else if SubStr[J] <> S[I] then
      begin
        Result := False;
        Exit;
      end
    else
      begin
        Inc(I);
        Inc(J);
      end;
  Result := False;
end;

function FindKeyPairBegin(const KeyPairs: TKeywordPairArray;
  const S: string; var StartPos: Integer; out PairIndex: Integer): Boolean;
var
  Lo, Hi, I, L: Integer;
begin
  Lo := Low(KeyPairs);
  Hi := High(KeyPairs);
  L := Length(S);
  while StartPos <= L do
  begin
    for I := Lo to Hi do
      if IsSubstrHere(KeyPairs[I].BeginKey, S, StartPos) then
        begin
          Result := True;
          PairIndex := I;
          Exit;
        end;
    Inc(StartPos);
  end; //while StartPos <= L
  Result := False;
end;

function FindId(const Id, S: string; StartPos: Integer): Integer;
var
  Done: Boolean;
  After: Integer;
begin
  repeat
    Result := Pos(Id, Copy(S, StartPos, MaxInt));
    if Result > 0 then
      Inc(Result, StartPos - 1);
    After := Result + Length(Id);
    Done := (Result = 0) or
            ((Result = 1) or not (S[Result-1] in IdChars)) and
            ((After > Length(S)) or not (S[After] in IdChars));
    if not Done then
      StartPos := Result + 1;
  until Done;
end;

function FindKeyPairEnd(const KeyPairs: TKeywordPairArray;
  const S: string; var StartPos: Integer; out EndKeywordIndex: Integer;
  const PairIndex: Integer): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  while StartPos <= L do
  begin
    for I := 0 to Length(KeyPairs[PairIndex].EndKeys) - 1 do
      if IsSubstrHere(KeyPairs[PairIndex].EndKeys[I], S, StartPos) then
        begin
          Result := True;
          EndKeywordIndex := I;
          Exit;
        end;
    Inc(StartPos);
  end; //while StartPos <= L
  Result := False;
end;

procedure SetStringArray(var SA: TStringArray; const A: array of string);
var
  I: Integer;
begin
  SetLength(SA, Length(A));
  for I := 0 to Length(SA) - 1 do
    SA[I] := A[I];
end;

{ TAutoTodo }

constructor TAutoToDo.Create(AOwner: TComponent);
begin
  inherited;
  AutoToDo := Self;
end;

destructor TAutoToDo.Destroy;
begin
  AutoToDo := nil;
  inherited;
end;

procedure TAutoToDo.AddTodo(const Source: string; List: TStrings);
resourcestring
  STODOFormat = '//TODO 5 -o%s -cEmpty Structure : %s (%s/%s in %s)';
const
  SInherited = 'inherited;';
var
  LowSource: string;
  BegPos: Integer;
  CodePos: Integer;
  EndPos: Integer;
  FirstCR, LastCR: Integer;
  I: Integer;
  EmptyBlock: Boolean;
  Procs: array [0 .. 5] of TRoutine;
  KeyPairs: TKeywordPairArray;
  CurProc: Integer;
  Todo: string;
  Pri: Integer;
  KeyPairIndex: Integer;
  EndKeywordIndex: Integer;

  procedure CheckRoutine(var R: TRoutine);
  var
    I, J, L: Integer;
  begin
    while R.NextPos < BegPos do
    begin
      R.PrevPos := R.NextPos;
      R.PrevName := R.NextName;
      R.NextPos := FindId(R.Keyword, LowSource, BegPos + 1);
      if R.NextPos = 0 then
      begin
        R.NextPos := MaxInt; //No further searching
        R.NextName := '';
        Exit;
      end;

      //We found a routine, determine its name
      L := Length(LowSource);
      I := R.NextPos + Length(R.Keyword);
      while (I <= L) and (LowSource[I] in SepChars) do Inc(I);
      J := I; //Start of routine name
      while (I <= L) and (LowSource[I] in IdChars + ['.']) do Inc(I);

      //Take the name from the mixed case original source string }
      R.NextName := Copy(Source, J, I - J);
    end; //while R.NextPos < BegPos
  end; //CheckRoutine

  function GetIndentOf(const Pos: Integer): string;
  var
    I: Integer;
  begin
    I := Pos;
    while (I > 0) and (LowSource[I] <> #10) do Dec(I);
    I := (Pos - I - 1);
    SetLength(Result, I);
    if I > 0 then
      FillChar(Result[1], I, #32);
  end; //GetIndentOf

begin //AddTodo
  List.BeginUpdate;
  try
    List.Clear;
    LowSource := LowerCase(Source);
    UnCommentSourceCode(LowSource, True);
    BegPos := FindId('implementation', LowSource, 1);
    FillChar(Procs, SizeOf(Procs), 0);
    Procs[0].Keyword := 'class procedure';
    Procs[1].Keyword := 'class function';
    Procs[2].Keyword := 'procedure';
    Procs[3].Keyword := 'function';
    Procs[4].Keyword := 'constructor';
    Procs[5].Keyword := 'destructor';
    SetLength(KeyPairs, 9);
    KeyPairs[0].BeginKey := 'begin';
    SetLength(KeyPairs[0].EndKeys, 1);
    KeyPairs[0].EndKeys[0] := 'end';

    KeyPairs[1].BeginKey := 'try';
    SetLength(KeyPairs[1].EndKeys, 2);
    KeyPairs[1].EndKeys[0] := 'finally';
    KeyPairs[1].EndKeys[1] := 'except';

    KeyPairs[2].BeginKey := 'finally';
    SetLength(KeyPairs[2].EndKeys, 1);
    KeyPairs[2].EndKeys[0] := 'end';

    KeyPairs[3].BeginKey := 'except';
    SetLength(KeyPairs[3].EndKeys, 1);
    KeyPairs[3].EndKeys[0] := 'end';

    KeyPairs[4].BeginKey := 'repeat';
    SetLength(KeyPairs[4].EndKeys, 1);
    KeyPairs[4].EndKeys[0] := 'until';

    KeyPairs[5].BeginKey := 'then';
    SetLength(KeyPairs[5].EndKeys, 1);
    KeyPairs[5].EndKeys[0] := 'else';

    KeyPairs[6].BeginKey := 'repeat';
    SetLength(KeyPairs[6].EndKeys, 1);
    KeyPairs[6].EndKeys[0] := 'until';

    KeyPairs[7].BeginKey := 'of'; //Case of
    SetLength(KeyPairs[7].EndKeys, 2);
    KeyPairs[7].EndKeys[0] := 'end';
    KeyPairs[7].EndKeys[1] := 'else';

    KeyPairs[8].BeginKey := 'else';
    SetLength(KeyPairs[8].EndKeys, 1);
    KeyPairs[8].EndKeys[0] := 'end';

    for Pri := Low(Procs) to High(Procs) do
      CheckRoutine(Procs[Pri]);
    if BegPos = 0 then Exit; //There's no implementation part
    Inc(BegPos);
    repeat
      if not FindKeyPairBegin(KeyPairs, LowSource, BegPos, KeyPairIndex) then
        Exit;
      if BegPos = 0 then Exit;

      //Determine the routine that owns the begin/end block
      for Pri := Low(Procs) to High(Procs) do
        CheckRoutine(Procs[Pri]);

      CodePos := BegPos + Length(KeyPairs[KeyPairIndex].BeginKey);
      EndPos := CodePos;
      if not FindKeyPairEnd(KeyPairs, LowSource, EndPos, EndKeywordIndex,
        KeyPairIndex)
      then
        Exit;

      //Check if this begin/end block is empty
      EmptyBlock := True;
      FirstCR := 0;
      LastCR := 0;
      I := CodePos;
      while I < EndPos do
      begin
        if (Source[I] in ['i', 'I']) and
           SameText(Copy(Source, I, Length(SInherited)), SInherited)
        then
          begin
            Inc(I, Length(SInherited) - 1);
            if (I < EndPos) and (Source[I] = ';') then
              Inc(I);
            CodePos := I;
          end
        else if not (Source[I] in SepChars) then
          begin
            EmptyBlock := False;
            Break;
          end
        else if Source[I] = #13 then
          begin
            if FirstCR = 0 then
              FirstCR := I;
            LastCR := I;
          end;
        Inc(I);
      end; //while I < EndPos

      //If this is an empty block, insert the TODO
      if EmptyBlock then
      begin
        //Determine current routine
        CurProc := 0;
        for Pri := Succ(Low(Procs)) to High(Procs) do
          if Procs[Pri].PrevPos > Procs[CurProc].PrevPos then
            CurProc := Pri;
        Todo := Procs[CurProc].PrevName;
        Todo := Format(STODOFormat,
                  [GetWindowsUser,
                   Todo,
                   KeyPairs[KeyPairIndex].BeginKey,
                   KeyPairs[KeyPairIndex].EndKeys[EndKeywordIndex],
                   Procs[CurProc].Keyword]);

        I := CodePos; //Insert position

        //No line break between begin/end?
        if FirstCR = 0 then
          begin
            Todo := #13#10+GetIndentOf(BegPos)+#32#32+ToDo+'???'#13#10+
                      GetIndentOf(BegPos);
            I := EndPos;
          end
        else //There are line breaks between begin/end
          begin
            I := LastCR;
            Todo := GetIndentOf(BegPos)+#32#32+ToDo;

            if FirstCR = LastCR then
              Todo := #13#10 + Todo;
          end;
        List.AddObject(ToDo, TObject(I - 1));
      end; //if EmptyBlock
      BegPos := CodePos;
    until False;
  finally
    List.EndUpdate;
  end;
end;

procedure TAutoTodo.ActionEditInsertAutoTodoSUpdate(Sender: TObject);
begin
  {$IFNDEF TEST}
  (Sender as TAction).Enabled :=
    ((BorlandIDEServices AS IOTAModuleServices).ModuleCount > 0) and
    (GetCurrentEditBuffer <> nil);
  {$ENDIF}
end;

procedure TAutoTodo.ActionEditInsertAutoTodoSExecute(Sender: TObject);
var
  Patches: TStringList;
  Buffer: IOTAEditBuffer;
  Reader: IOTAEditReader;
  Writer: IOTAEditWriter;
  S: string;
  I: Integer;
  CurPos,
  PatchPos: Integer;
  TextLength: Integer;
begin
  {$IFNDEF TEST}
  Buffer := GetCurrentEditBuffer;
  Patches := TStringList.Create;
  try
    S := ReadSourceCode(Buffer);
    TextLength := Length(S);
    AddTodo(S, Patches);

    //Create undoable writer to modify the code
    Writer := Buffer.CreateUndoableWriter;

    //First travers all patches
    CurPos := 0;
    for I := 0 to Patches.Count - 1 do
    begin
      PatchPos := Integer(Patches.Objects[I]);
      if PatchPos > CurPos then
      begin
        Writer.CopyTo(PatchPos);
        CurPos := PatchPos;
      end;
      Writer.Insert(PChar(Patches[I]));
    end; //for I := 0 to Patches.Count - 1
    if CurPos < TextLength then
      Writer.CopyTo(TextLength); 
  finally
    Patches.Free;
  end;
  {$ENDIF}
end;

function TAutoTodo.AddTodo(const Source: string): string;
var
  S: TStrings;
  I: Integer;
begin
  S := TStringList.Create;
  try
    AddTodo(Source, S);
    Result := Source;
    for I := S.Count - 1 downto 0 do
      Insert(S[I], Result, Integer(S.Objects[I])+1);
  except
    S.Free;
    raise;
  end;
end;

function TAutoTodo.GetWindowsUser: string;
const
  InitialSize = 255;
var
  P: PChar;
  Size: Cardinal;
begin
  Size := InitialSize;
  GetMem(P, Size);
  try
    if not GetUserName(P, Size) then
    begin
      if Size = InitialSize then
        RaiseLastOSError
      else
        begin
          ReallocMem(P, Size);
          if not GetUserName(P, Size) then
            RaiseLastOSError;
        end;
    end;
    SetString(Result, P, Size - 1);
  finally
    FreeMem(P);
  end;
end;

initialization
finalization
  AutoTodo.Free;
end.
