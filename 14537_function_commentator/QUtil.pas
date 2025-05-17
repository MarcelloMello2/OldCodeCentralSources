unit QUtil;

interface

uses
  Classes,
  SysUtils;

type
  TFuncKind = (fncNone, fncFunction, fncProcedure, fncConstructor, fncDestructor,
               fncUnit, fncClass);

  TUniqueStringList = class(TStringList)
  public
    function Add(const S : string) : Integer; override;
  end;

  CompareFunc = Function : boolean of object;

  TSimpleParser = class
  private
    FBuffer : PChar;
    FInParams,
    FOutParams,
    FInOutParams,
    FDataTypes : TStrings;
    FBufPos : integer;
    FPrevDelimiter : Char;
    FKind : TFuncKind;
    FFunctionName : String;
    function checkDelOnly : boolean;
    function checkALL : boolean;
    function FetchFunctionKind : Boolean;
    function GetNextToken(cmpfunc : CompareFunc) : string;
    procedure ParseParams;
    procedure SkipWS;
  public
    procedure Parse;
    constructor create;
    destructor Destroy; override;
    property buffer : PChar read FBuffer write FBuffer;
    property FunctionKind : TFuncKind read FKind;
    property FunctionName : String read FFunctionName;
    property InParams : TStrings read FInParams;
    property OutParams : TStrings read FOutParams;
    property InOutParams : TStrings read FInOutParams;
    property DataTypes : TStrings read FDataTypes;
  end;

const
  WhiteSpace = [#10, #13, ' ']; //possibly: [#0..' ', #255]?
  Delimiters = [',', ';', ':', '(', ')'];
  AllDelimiters = WhiteSpace + Delimiters;

implementation

function TUniqueStringList.Add(const S : string) : Integer;
begin
  result := IndexOf(s);
  if result = -1 then result := inherited Add(s);
end;

function SelectString(const s : string; sa : array of string) : integer;
var
  i : integer;
begin
  i := low(sa);
  while (i <= high(sa)) and (CompareText(s, sa[i])<>0) do
    inc(i);
  result := i;
end;

constructor TSimpleParser.Create;
begin
  inherited Create;
  FInParams := TStringList.Create;
  FOutParams := TStringList.Create;
  FInOutParams := TStringList.Create;
  FDataTypes := TUniqueStringList.Create;
  Fbufpos := 0;
end;

destructor TSimpleParser.Destroy;
begin
  FInParams.Free;
  FOutParams.Free;
  FInOutParams.Free;
  FDataTypes.Free;
  inherited Destroy;
end;

function TSimpleParser.checkALL : boolean;
begin
  result := FBuffer[FBufPos] IN AllDelimiters;
end;

function TSimpleParser.checkDelOnly : boolean;
begin
  result := FBuffer[FBufPos] IN Delimiters;
end;

function TSimpleParser.GetNextToken;
begin
  result := '';
  while (not cmpfunc) or (FBuffer[FBufPos + 1] = '*') do
  begin
    if FBuffer[FBufPos] = '{' then
    begin
      FBufPos := Integer(StrScan(FBuffer + FBufPos, '}'));
      if FBufPos = 0 then raise Exception.Create('no closing comment');
      FBufPos := FBufPos - Integer(FBuffer);
    end
    else
      if StrLComp(FBuffer + FBufPos, '(*', 2) = 0 then
      begin
        FBufPos := Integer(StrPos(FBuffer + FBufPos, '*)'));
        if FBufPos = 0 then raise Exception.Create('no closing comment');
        FBufPos := FBufPos - Integer(FBuffer) + 1;
      end
      else
        result := result + FBuffer[FBufPos];
    inc (FBufPos);
  end;
  FPrevDelimiter := FBuffer[FBufPos];
  inc(FBufPos);
  result := Trim(result);
end;

function TSimpleParser.FetchFunctionKind : Boolean;
var
  i : integer;
begin
  FKind := fncNone;
  repeat
    i := SelectString(GetNextToken(checkALL),
                      ['function', 'procedure', 'constructor', 'destructor', 'unit', 'class']);
  until i < 6;
  FKind := TFuncKind(i + 1);
  result := True;
end;

procedure TSimpleParser.SkipWS;
begin
  while FBuffer[FBufPos] IN WhiteSpace do
    inc(FBufPos);
end;

procedure TSimpleParser.ParseParams;
var
  IsOutParam : Boolean;
  s          : string;
  i, j       : integer;
  tmpList    : TStringList;
  aSList     : TStrings;
begin
  tmpList := TStringList.Create;
  try
    repeat
      SkipWS;
      s := GetNextToken(checkALL); tmpList.Clear;
      IsOutParam := CompareText(s, 'var') = 0;
      if IsOutParam or (CompareText(s, 'const') = 0) then
        s := GetNextToken(checkDelOnly)
      else
        if FPrevDelimiter IN WhiteSpace then
        begin
          SkipWS;
          FPrevDelimiter := FBuffer[FBufPos];
          inc(FBufPos);
        end;
      tmpList.Add(s);
      while FPrevDelimiter = ',' do
        tmpList.Add(GetNextToken(checkDelOnly));
      i := -1;
      if FPrevDelimiter = ':' then
        i := DataTypes.Add(GetNextToken(checkDelOnly));
      if IsOutParam then aSList := InOutParams else aSList := InParams;
      for j := 0 to tmpList.Count - 1 do
        aSList.AddObject(tmpList[j], pointer(i));
    until FPrevDelimiter = ')';
    GetNextToken(checkDelOnly);
  finally
    tmpList.Free;
  end;
end;

procedure TSimpleParser.Parse;
begin
  FetchFunctionKind;
  case FKind of
    fncFunction..fncDestructor :
        begin
          FFunctionName := GetNextToken(checkDelOnly);
          repeat
            case FPrevDelimiter of
              ':' : begin
                      FOutParams.AddObject('function result',
                                           Pointer(DataTypes.Add(
                                                   GetNextToken(checkDelOnly))));
                      break;  //assume trailing ';'
                    end;
              ';' : break;
              '(' : ParseParams;
              else raise Exception.Create('unanticipated data encountered');
            end;
          until false;
        end;
    fncUnit :
          FFunctionName := GetNextToken(checkDelOnly);
  end;
end;

end.
