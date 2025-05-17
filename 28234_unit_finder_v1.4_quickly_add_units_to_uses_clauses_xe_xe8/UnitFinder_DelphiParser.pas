unit UnitFinder_DelphiParser;

interface

uses
  AnsiStrings, Sysutils, Classes;

const
  cMaxBuffer = 1024;

type
  EParseException = class(Exception);

  TTokenType = (ttSymbol, ttComment, ttString, ttPunctuation, ttWhitespace, ttOther);
  TGetSourceEvent = procedure(Sender:TObject; Buffer:PAnsiChar; MaxLength:integer; var Length:integer) of object;
  TInsertSourceEvent =  procedure(Sender:TObject; Position:integer; Source:AnsiString) of object;
  TDeleteSourceEvent =  procedure(Sender:TObject; Position:integer; Count:integer) of object;


  { TDelphiScanner

    This is a simple Delphi language tokenizer that breaks up source code into a few simple types:

      ttSymbol: Any keyword or identifier.
                These are made up of alphanumeric ASCII characters, underscore, and all UTF-8 multi-byte characters.

      ttComment: Brace Comments, Parenthesis Asterisk Comments, and Double Slash Comments

      ttString: Characters delimited with single quotes.

      ttPunctuation: All non symbol characters except whitespace, comment delimiters, and string delimiters.

    To use the scanner, simply provide source code through the OnGetSource event.  Then call GetNextToken until
    it returns an empty string signifying EOF.  All source code fed to the tokenizer through the OnGetSource
    event can be accessed through Source property for later use. You can feed as much or as little of the source
    to the event as you want.  The event will be called every time the class needs to read in more source.
    Sending an empty string through the OnGetSource event signals that EOF is reached in the actual source.
  }
  TDelphiScanner = class(TObject)
  private
    FBuffer:array[0..cMaxBuffer] of AnsiChar;
    FSource:AnsiString;
    FOnGetSource: TGetSourceEvent;
    FTokenType: TTokenType;
    FToken: AnsiString;
    FPosition: integer;
    FEOF: boolean;
    FOnDeleteSource: TDeleteSourceEvent;
    FOnInsertSource: TInsertSourceEvent;
    FSymbolCharacters: TSysCharSet;
    FCurrentLine: integer;
    procedure SetPosition(const Value: integer);


    function PeekCh(LookAhead:integer = 0):AnsiChar;
    function GetCh:AnsiChar;

    function IsSymbolChar(ch:AnsiChar):boolean;

    function AtNewLine:boolean;
    function AtWhitespace:boolean;
    function GetWhitespace:AnsiString;
    function AtSymbol:boolean;
    function GetSymbol:AnsiString;
    function AtString:boolean;
    function GetString:AnsiString;
    function AtSlashComment:boolean;
    function GetSlashComment:AnsiString;
    function AtBraceComment:boolean;
    function GetBraceComment:AnsiString;
    function AtParenthesisAsteriskComment:boolean;
    function GetParenthesisAsteriskComment:AnsiString;
    function GetPunctuation:AnsiString;
    property EOF:boolean read FEOF;
  public
    constructor Create; virtual;
    procedure Clear;
    function GetNextToken(ExtraSymbolCharacters:TSysCharSet = []; SkipComments:boolean = True; SkipWhitespace:boolean = True):boolean;
    procedure InsertSource(Pos:integer; Source:AnsiString);
    procedure DeleteSource(Pos, Count:integer);
    function GetLineLength(Pos:integer):integer;
    function CleanAfterRemoveUnit(Pos:integer):integer;
    property Token:AnsiString read FToken;
    property TokenType: TTokenType read FTokenType;
    property Position:integer read FPosition write SetPosition;
    property Source:AnsiString read FSource;
    property OnGetSource:TGetSourceEvent read FOnGetSource write FOnGetSource;
    property OnInsertSource: TInsertSourceEvent read FOnInsertSource write FOnInsertSource;
    property OnDeleteSource: TDeleteSourceEvent read FOnDeleteSource write FOnDeleteSource;
    property SymbolCharacters:TSysCharSet read FSymbolCharacters write FSymbolCharacters;
    property CurrentLine:integer read FCurrentLine write FCurrentLine;
  end;


  TFileDeclaration = class(TObject)
  private
    FFileName: string;
    FIsProgram: boolean;
  public
    procedure LoadFromScanner(Scanner:TDelphiScanner);
    property IsProgram:boolean read FIsProgram write FIsProgram;
    property FileName:string read FFileName write FFileName;
  end;


  TUsesUnit = class(TObject)
  private
    FUnitTokenEnd: integer;
    FSeparatorTokenStart: integer;
  published
    property UnitTokenEnd: integer read FUnitTokenEnd write FUnitTokenEnd;
    property SeparatorTokenStart: integer read FSeparatorTokenStart write FSeparatorTokenStart;
  end;


  TUsesClause = class(TObject)
  private
    FSectionToken: AnsiString;
    FSectionTokenStart: integer;
    FUsesTokenStart: integer;
    FSemiColonPosition: integer;
    FUnits: TStringList;
    FVertical: boolean;
    function GetSectionMessage:string;
  public
    procedure LoadFromScanner(Scanner:TDelphiScanner);
    constructor Create; virtual;
    destructor Destroy; override;
    property Units: TStringList read FUnits write FUnits;
    property SectionTokenStart:integer read FSectionTokenStart;
    property SectionToken: AnsiString read FSectionToken write FSectionToken;
    property Vertical: boolean read FVertical write FVertical;
  end;

  TInterfaceUsesClause = class(TUsesClause)
  public
    constructor Create; override;
  end;

  TImplementationUsesClause = class(TUsesClause)
  public
    constructor Create; override;
  end;


  { TDelphiUnit

    This class encapsulates the information about a delphi unit.  The main information it handles
    is the locations and contents of the uses clauses.
  }
  TDelphiUnit = class(TObject)
  private
    FScanner:TDelphiScanner;
    FInterfaceUsesClause: TInterfaceUsesClause;
    FImplementationUsesClause: TImplementationUsesClause;
    FIsLoaded: boolean;
    FErrorMessage: string;
    FFileDeclaration: TFileDeclaration;
    function GetImplementationStart: integer;
    function GetOnGetSource: TGetSourceEvent;
    procedure SetOnGetSource(const Value: TGetSourceEvent);
    procedure AdjustUses(UsesClause:TUsesClause; Adjustment:integer);
    function RemoveUses(UsesClause:TUsesClause; UnitName:string):integer;
    function AddUses(UsesClause:TUsesClause; UnitName:string):integer;
    function GetOnDeleteSource: TDeleteSourceEvent;
    function GetOnInsertSource: TInsertSourceEvent;
    procedure SetOnDeleteSource(const Value: TDeleteSourceEvent);
    procedure SetOnInsertSource(const Value: TInsertSourceEvent);
    function GetSourceCode: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    function InInterfaceUses(UnitName:string):boolean;
    function InImplementationUses(UnitName:string):boolean;
    procedure AddToInterfaceUses(UnitName:string);
    procedure AddToImplementationUses(UnitName:string);
    property FileDeclaration: TFileDeclaration read FFileDeclaration write FFileDeclaration;
    property InterfaceUsesClause:TInterfaceUsesClause read FInterfaceUsesClause;
    property ImplementationUsesClause:TImplementationUsesClause read FImplementationUsesClause;
    property OnGetSource: TGetSourceEvent read GetOnGetSource write SetOnGetSource;
    property OnInsertSource: TInsertSourceEvent read GetOnInsertSource write SetOnInsertSource;
    property OnDeleteSource: TDeleteSourceEvent read GetOnDeleteSource write SetOnDeleteSource;
    property ImplementationStart:integer read GetImplementationStart;
    property SourceCode:AnsiString read GetSourceCode;
    property IsLoaded:boolean read FIsLoaded;
    property ErrorMessage:string read FErrorMessage;
  end;

var
  DelphiSourceIndent:integer = 2;
  DelphiSourceWidth:integer = 80;

implementation

uses
  UnitFinder_SystemSupport;


const
  cWhitespace = [' ',#13,#10,#9];

{
********************************* TUsesClause **********************************
}


constructor TUsesClause.Create;
begin
  FUnits := TStringList.Create;
  FUnits.CaseSensitive := False;
end;

destructor TUsesClause.Destroy;
var
  i:integer;
begin
  for i := 0 to FUnits.Count-1 do
    FUnits.Objects[i].Free;
  FUnits.Free;
  inherited;
end;

function TUsesClause.GetSectionMessage: string;
begin
  if FSectionToken = '' then
    result := ''
  else
    result := ' in the '+ string(FSectionToken) +' section';
end;

procedure TUsesClause.LoadFromScanner(Scanner:TDelphiScanner);
var
  UsesUnit:TUsesUnit;
  LastLine:integer;
begin
  FUnits.Clear;

  // Find Section Token

  if FSectionToken = '' then begin
    FSectionTokenStart := Scanner.Position;
  end else begin

    FSectionTokenStart := -1;
    while Scanner.GetNextToken do begin
      if (Scanner.TokenType = ttSymbol) and
         (FSectionToken = AnsiStrings.LowerCase(Scanner.Token) ) then begin
        FSectionTokenStart := Scanner.Position - Length(FSectionToken);
        break;
      end;
    end;
    if FSectionTokenStart = -1 then
      raise EParseException.Create('Can not find the '+ string(FSectionToken) +' section.');

  end;

  // Find the "uses" keyword
  FUsesTokenStart := Scanner.Position;
  Scanner.GetNextToken;
  if AnsiStrings.Lowercase(Scanner.Token) <> 'uses' then begin
    Scanner.Position := Scanner.Position - Length(Scanner.Token);
    exit;
  end;
  FUsesTokenStart := Scanner.Position - 4;

  FVertical := True;
  LastLine := -1;

  // Get all unit names
  while Scanner.GetNextToken(['.']) do begin
    if Scanner.TokenType <> ttSymbol then
      raise EParseException.Create('Could not understand uses clause' + GetSectionMessage + '.');
    if (FVertical) and (Scanner.CurrentLine = LastLine) then
      FVertical := False;
    LastLine := Scanner.CurrentLine;

    UsesUnit := TUsesUnit.Create;
    FUnits.AddObject(UTF8ToUnicodeString(Scanner.Token),UsesUnit);
    UsesUnit.UnitTokenEnd := Scanner.Position;
    Scanner.GetNextToken;

    // Skip any 'in' clauses
    if Scanner.Token = 'in' then begin
      Scanner.GetNextToken; // eat up the directory
      Scanner.GetNextToken;
    end;

    if (Scanner.Token = ';') or (Scanner.Token = ',') then begin
      UsesUnit.SeparatorTokenStart := Scanner.Position-1;
      if Scanner.Token = ';' then begin
        FSemiColonPosition := Scanner.Position - 1;
        break;
      end;
    end else
      raise EParseException.Create('Could not understand uses clause' + GetSectionMessage + '.');

  end;
  if FUnits.Count = 0 then
    raise EParseException.Create('Missing units' + IfBlank(GetSectionMessage,' in the program''s ') + ' uses clause.');

  if (FSectionToken <> '') and (FUnits.Count <= 1) then
    FVertical := False;
end;


{ TInterfaceUsesClause }

constructor TInterfaceUsesClause.Create;
begin
  inherited;
  FSectionToken := 'interface';
end;

{ TImplementationUsesClause }


constructor TImplementationUsesClause.Create;
begin
  inherited;
  FSectionToken := 'implementation';
end;

function TDelphiScanner.AtBraceComment: boolean;
begin
  result := PeekCh = '{';
end;

function TDelphiScanner.AtNewLine: boolean;
begin
  result := PeekCh = #13;
end;

function TDelphiScanner.AtParenthesisAsteriskComment: boolean;
begin
  result := (PeekCh = '(') and (PeekCh(1) = '*');
end;

function TDelphiScanner.AtSlashComment: boolean;
begin
  result := (PeekCh = '/') and (PeekCh(1) = '/');
end;

function TDelphiScanner.AtString: boolean;
begin
  result := PeekCh = '''';
end;

function TDelphiScanner.AtSymbol: boolean;
begin
  result := IsSymbolChar(PeekCh);
end;

function TDelphiScanner.AtWhitespace: boolean;
begin
  result := PeekCh in cWhitespace;
end;

function TDelphiScanner.CleanAfterRemoveUnit(Pos: integer):integer;
var
  LastPos,Ct:integer;
  CommentEncountered:Boolean;
begin
  result := 0;

  // Find separating comma, and erase it and all whitespace after it, unless a comment is encountered.
  CommentEncountered := False;
  LastPos := Pos;
  Position := Pos;
  while GetNextToken([],False) do begin
    if Token = ',' then begin
      Ct := FPosition - LastPos;
      DeleteSource(LastPos,Ct);
      result := result + (Ct * -1);
      LastPos := FPosition;
    end else if (TokenType = ttSymbol) or (Token = ';') then begin
      if not CommentEncountered then begin
        Ct := (FPosition - Length(Token)) - LastPos;
        DeleteSource(LastPos,Ct);
        result := result + (Ct * -1);
      end;
      break;
    end else if TokenType = ttComment then begin
      CommentEncountered := True;
      LastPos := FPosition;
    end;
  end;

end;

procedure TDelphiScanner.Clear;
begin
   FPosition := 1;
   FSource := '';
   FEOF := False;
end;

constructor TDelphiScanner.Create;
begin
  FSymbolCharacters := ['A'..'Z','a'..'z','0'..'9','_'];
  Clear;
end;

procedure TDelphiScanner.DeleteSource(Pos, Count: integer);
begin
  Delete(FSource,Pos,Count);
  if Assigned(FOnDeleteSource) then
    FOnDeleteSource(Self,Pos,Count);

  if FPosition >= Pos then begin
    Dec(FPosition,Count);
    if FPosition > Length(FSource) then
      FEOF := True;
  end;
end;

function TDelphiScanner.GetBraceComment: AnsiString;
begin
  result := GetCh;
  while not EOF do begin
    if PeekCh = '}' then begin
      result := result + GetCh;
      break;
    end;
    result := result + GetCh;
  end;
end;

function TDelphiScanner.GetCh: AnsiChar;
begin
  result := PeekCh;
  Inc(FPosition);
end;

function TDelphiScanner.GetNextToken(ExtraSymbolCharacters:TSysCharSet = []; SkipComments:boolean = True; SkipWhitespace:boolean = True): boolean;
var
  OldSymbolCharacters: TSysCharSet;
begin
  result := False;
  FToken := '';
  FTokenType := ttOther;

  OldSymbolCharacters := SymbolCharacters;
  try
    SymbolCharacters := SymbolCharacters + ExtraSymbolCharacters;

    while not EOF do begin
      if AtNewLine then
        Inc(FCurrentLine);

      if AtWhitespace then begin
        if SkipWhitespace then
          GetWhitespace
        else begin
          FToken := GetWhitespace;
          FTokenType := ttWhitespace;
          result := True;
          exit;
        end;
      end else if AtSymbol then begin
          FToken := GetSymbol;
          FTokenType := ttSymbol;
          result := True;
          exit;
      end else if AtString then begin
          FToken := GetString;
          FTokenType := ttString;
          result := True;
          exit;
      end else if AtSlashComment then begin
        if SkipComments then
          GetSlashComment
        else begin
          FToken := GetSlashComment;
          FTokenType := ttComment;
          result := True;
          exit;
        end;
      end else if AtBraceComment then begin
        if SkipComments then
          GetBraceComment
        else begin
          FToken := GetBraceComment;
          FTokenType := ttComment;
          result := True;
          exit;
        end;
      end else if AtParenthesisAsteriskComment then begin
        if SkipComments then
          GetParenthesisAsteriskComment
        else begin
          FToken := GetParenthesisAsteriskComment;
          FTokenType := ttComment;
          result := True;
          exit;
        end;
      end else begin
        FToken := GetPunctuation;
        FTokenType := ttPunctuation;
        result := True;
        exit;
      end;

    end;

  finally
    SymbolCharacters := OldSymbolCharacters;
  end;

end;

function TDelphiScanner.GetParenthesisAsteriskComment: AnsiString;
begin
  result := GetCh + GetCh;
  while not EOF do begin
    if PeekCh = '*' then begin
      result := result + GetCh;
      if PeekCh = ')' then begin
        result := result + GetCh;
        break;
      end else
        continue;
    end;
    result := result + GetCh;
  end;
end;

function TDelphiScanner.GetPunctuation: AnsiString;
begin
  result := GetCh;
end;

function TDelphiScanner.GetSlashComment: AnsiString;
begin
  result := GetCh + GetCh;
  while not EOF do begin
    if PeekCh in [#13,#10] then
      break;
    result := result + GetCh;
  end;
end;

function TDelphiScanner.GetString: AnsiString;
begin
  result := GetCh;
  if EOF then
    raise EParseException.Create('Unexpected end of string.');
  while not EOF do begin
    if PeekCh = '''' then begin
      result := result + GetCh;
      if PeekCh = '''' then begin
        GetCh;
      end else
        break;
    end else
      result := result + GetCh;
  end;
end;

function TDelphiScanner.GetSymbol: AnsiString;
begin
  result := GetCh;
  while not EOF do
    if AtSymbol then
      result := result + GetCh
    else
      break;
end;

function TDelphiScanner.GetWhitespace: AnsiString;
begin
  result := GetCh;
end;

procedure TDelphiScanner.InsertSource(Pos: integer; Source: AnsiString);
begin
  Insert(Source,FSource,Pos);
  if Assigned(FOnInsertSource) then
    FOnInsertSource(Self,Pos,Source);

  if FPosition >= Pos then
    Inc(FPosition,Length(Source));
end;

function TDelphiScanner.IsSymbolChar(ch: AnsiChar): boolean;
begin
  result := (ch in FSymbolCharacters) or
            (Ord(ch) > 127);
end;

function TDelphiScanner.PeekCh(LookAhead:integer = 0): AnsiChar;
var
  ResultLen:integer;
begin
  // If we run out of source, try to get more.
  if (FPosition+LookAhead) > Length(FSource) then begin
    OnGetSource(Self, FBuffer, cMaxBuffer, ResultLen);
    if ResultLen = 0 then begin
      result := #0;
      FEOF := True;
      exit;
    end;
    FBuffer[ResultLen] := #0;
    FSource := FSource + AnsiString(FBuffer);
  end;

  if (FPosition+LookAhead) > Length(FSource) then
    result := #0
  else
    result := FSource[FPosition+LookAhead];

end;


procedure TDelphiScanner.SetPosition(const Value: integer);
begin
  FPosition := Value;
  FEOF := False;
end;

{ TDelphiUnit }

procedure TDelphiUnit.AddToImplementationUses(UnitName: string);
begin
  if InImplementationUses(UnitName) then
    exit;
  if InInterfaceUses(UnitName) then
    AdjustUses(ImplementationUsesClause, RemoveUses(InterfaceUsesClause,UnitName) );
  AddUses(ImplementationUsesClause,UnitName);
end;

procedure TDelphiUnit.AddToInterfaceUses(UnitName: string);
begin
  if InInterfaceUses(UnitName) then
    exit;
  if InImplementationUses(UnitName) then
    RemoveUses(ImplementationUsesClause,UnitName);
  AdjustUses(ImplementationUsesClause, AddUses(InterfaceUsesClause,UnitName) );
end;

function TDelphiUnit.AddUses(UsesClause: TUsesClause; UnitName: string): integer;
var
  s:AnsiString;
  p:integer;
  UsesUnit:TUsesUnit;
begin
  if FScanner.Source = '' then
    raise Exception.Create('TDelphiUnit.AddUses: No source code loaded.');


  if UsesClause.Units.Count = 0 then begin
    p := UsesClause.SectionTokenStart + Length(UsesClause.FSectionToken);
    s := #13#10#13#10 + 'uses' + #13#10 + AnsiStrings.DupeString(' ',DelphiSourceIndent) + UTF8Encode(UnitName) + ';';
  end else begin
    p := TUsesUnit(UsesClause.Units.Objects[UsesClause.Units.Count-1]).SeparatorTokenStart;
    s := ', '+ UTF8Encode(UnitName);
    if (UsesClause.Vertical) or
       (Length(s) + FScanner.GetLineLength(p) > DelphiSourceWidth) then
      s := ',' + #13#10 + AnsiStrings.DupeString(' ',DelphiSourceIndent) + UTF8Encode(UnitName);
   end;

  FScanner.InsertSource(p, s);
  result := Length(s);

  UsesUnit := TUsesUnit.Create;
  UsesClause.Units.AddObject(UnitName,UsesUnit);
  UsesUnit.UnitTokenEnd := p + result;
  UsesUnit.SeparatorTokenStart := UsesUnit.UnitTokenEnd;

end;

procedure TDelphiUnit.AdjustUses(UsesClause: TUsesClause; Adjustment: integer);
var
  i:integer;
  UsesUnit:TUsesUnit;
begin
  Inc(UsesClause.FSectionTokenStart,Adjustment);
  Inc(UsesClause.FUsesTokenStart,Adjustment);
  Inc(UsesClause.FSemiColonPosition,Adjustment);
  for i := 0 to UsesClause.Units.Count - 1 do begin
    UsesUnit := TUsesUnit(UsesClause.Units.Objects[i]);
    UsesUnit.UnitTokenEnd := UsesUnit.UnitTokenEnd + Adjustment;
    UsesUnit.SeparatorTokenStart := UsesUnit.SeparatorTokenStart + Adjustment;
  end;
end;

constructor TDelphiUnit.Create;
begin
  FScanner := TDelphiScanner.Create;
  FFileDeclaration := TFileDeclaration.Create;
  FInterfaceUsesClause := TInterfaceUsesClause.Create;
  FImplementationUsesClause := TImplementationUsesClause.Create;
end;

destructor TDelphiUnit.Destroy;
begin
  FInterfaceUsesClause.Free;
  FImplementationUsesClause.Free;
  FFileDeclaration.Free;
  FScanner.Free;
  inherited;
end;

function TDelphiUnit.GetImplementationStart: integer;
begin
  result := ImplementationUsesClause.SectionTokenStart;
end;

function TDelphiScanner.GetLineLength(Pos: integer): integer;
var
  p:integer;
  Start:integer;
begin
  if Pos > Length(FSource) then
    Pos := Length(FSource)-1;

  p := Pos;
  while (p >= 1) and (not(FSource[p] in [#13,#10])) do
    Dec(p);
  Start := p;

  p := Pos;
  while (p < Length(FSource)) and (not(FSource[p] in [#13,#10])) do
    Inc(p);

  result := p - Start;

end;

function TDelphiUnit.GetOnDeleteSource: TDeleteSourceEvent;
begin
  result := FScanner.OnDeleteSource;
end;

function TDelphiUnit.GetOnGetSource: TGetSourceEvent;
begin
  result := FScanner.OnGetSource;
end;


function TDelphiUnit.GetOnInsertSource: TInsertSourceEvent;
begin
  result := FScanner.OnInsertSource;
end;

function TDelphiUnit.GetSourceCode: AnsiString;
begin
  result := FScanner.Source;
end;

function TDelphiUnit.InImplementationUses(UnitName: string): boolean;
begin
  result := ImplementationUsesClause.Units.IndexOf(UnitName) <> -1;
end;

function TDelphiUnit.InInterfaceUses(UnitName: string): boolean;
begin
  result := InterfaceUsesClause.Units.IndexOf(UnitName) <> -1;
end;

procedure TDelphiUnit.Load;
begin
  FIsLoaded := False;
  FErrorMessage := '';

  if not Assigned(FScanner.OnGetSource) then
    raise Exception.Create('OnGetSource event not assigned.');

  FScanner.Clear;

  try
    FileDeclaration.LoadFromScanner(FScanner);
    if FileDeclaration.IsProgram then
      InterfaceUsesClause.SectionToken := '';
    InterfaceUsesClause.LoadFromScanner(FScanner);
    if not FileDeclaration.IsProgram then
      ImplementationUsesClause.LoadFromScanner(FScanner);
    FIsLoaded := True;
  except
    on E:EParseException do
      FErrorMessage := 'Parse error on line ' + IntToStr(FScanner.CurrentLine+1) + ': ' + E.Message;
  end;
end;

function TDelphiUnit.RemoveUses(UsesClause: TUsesClause; UnitName: string): integer;
var
  i,Ct:integer;
  p:integer;
begin
  result := 0;
  if FScanner.Source = '' then
    raise Exception.Create('TDelphiUnit.AddUses: No source code loaded.');

  i := UsesClause.Units.IndexOf(UnitName);
  if i = -1 then
    exit;

  if UsesClause.Units.Count = 1 then begin
    Ct := (UsesClause.FSemiColonPosition+1) - UsesClause.FUsesTokenStart;
    FScanner.DeleteSource(UsesClause.FUsesTokenStart,Ct);
    UsesClause.Units.Clear;
    result := Ct * -1;
  end else begin
    Ct := Length(UTF8Encode(UnitName));
    p := TUsesUnit(UsesClause.Units.Objects[i]).UnitTokenEnd - Ct;
    FScanner.DeleteSource(p, Ct);
    result := Ct * -1;
    if i = UsesClause.Units.Count-1 then
      result := result + FScanner.CleanAfterRemoveUnit(TUsesUnit(UsesClause.Units.Objects[i-1]).UnitTokenEnd)
    else
      result := result + FScanner.CleanAfterRemoveUnit(p);
    UsesClause.Units.Delete(i);
  end;

end;

procedure TDelphiUnit.SetOnDeleteSource(const Value: TDeleteSourceEvent);
begin
  FScanner.OnDeleteSource := Value;
end;

procedure TDelphiUnit.SetOnGetSource(const Value: TGetSourceEvent);
begin
  FScanner.OnGetSource := Value;
end;

procedure TDelphiUnit.SetOnInsertSource(const Value: TInsertSourceEvent);
begin
  FScanner.OnInsertSource := Value;
end;

{ TFileDeclaration }

procedure TFileDeclaration.LoadFromScanner(Scanner: TDelphiScanner);
var
  Found:boolean;
begin
  Found := False;
  while Scanner.GetNextToken do begin
    if (Scanner.TokenType = ttSymbol) then begin
      if SameText(Scanner.Token, 'unit') then
        Found := True
      else if SameText(Scanner.Token, 'program') then begin
        Found := True;
        FIsProgram := True;
      end;
      if Found then begin
        Scanner.GetNextToken(['.']);
        FFileName := Scanner.Token;

        Scanner.GetNextToken;
        if Scanner.Token <> ';' then
          raise EParseException.Create('Expecting semicolon after "' + FFileName + '"');

        break;
      end;
    end;
  end;
  if not Found then
    raise EParseException.Create('Can not find "unit" or "program" declaration.');
end;

end.

