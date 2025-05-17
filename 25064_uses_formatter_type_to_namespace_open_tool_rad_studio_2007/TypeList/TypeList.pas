unit TypeList;

interface

uses
  Commons.Settings,
  System.Collections,
  System.Xml.Serialization;

type
  TTypeList = class(TSettingsObject)
  strict private
    procedure Hash;
  public
    [XmlIgnore]
    KnownTypeHash: HashTable;
  public
    KnownTypes: ArrayList;
    class function Load(Path: string): TTypeList;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TTypeList }

constructor TTypeList.Create;
begin
  inherited Create;
  KnownTypeHash := HashTable.Create;
  KnownTypes := ArrayList.Create;
end;

destructor TTypeList.Destroy;
begin
  KnownTypeHash.Clear;
  KnownTypes.Clear;
  inherited Destroy;
end;

procedure TTypeList.Hash;
var
  I,P: Integer;
  N: string;
  S: array of string;
begin
  KnownTypeHash.Clear;
  for I := 0 to Pred(KnownTypes.Count) do begin
    S := (KnownTypes[I] as string).Split([',']);
    P := S[0].IndexOf('<');
    if P >= 0 then begin
      S[0] := S[0].Substring(0,P + 1) + '>';
      S[1] := S[Length(S) - 1];
    end;
    N := KnownTypeHash[S[0].ToUpper] as string;
    if not Assigned(N) then
      KnownTypeHash[S[0].ToUpper] := S[1]
    else if N.ToUpper.IndexOf(S[1].ToUpper) < 0 then
      KnownTypeHash[S[0].ToUpper] := S[1] + ',' + N;
  end;
end;

class function TTypeList.Load(Path: string): TTypeList;
begin
  if Path = '' then
    Result := TTypeList.Create
  else begin
    try
      Result := TTypeList.LoadFrom(Path) as TTypeList;
      Result.Hash;
    except
      Result := TTypeList.Create;
    end;
  end;
end;

end.
