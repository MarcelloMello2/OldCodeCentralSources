unit Options;

interface

uses
  Commons.Settings,
  System.Collections,
  System.Reflection,
  System.Xml.Serialization,
  System.Windows.Forms;

type
  TOptions = class(TSettingsObject)
  strict private
    procedure Hash;
  public
    [XmlIgnore]
    UnitPostfix: string;
    [XmlIgnore]
    UsesIndent: string;
    [XmlIgnore]
    KnownNamespaceHash: HashTable;
  public
    ClassPrefix: string;
    Indent: Integer;
    AddPrefixDefault: Boolean;
    RemoveUnprefixedDefault: Boolean;
    PrefixUnitPath: string;
    TypeListPath: string;
    ItemInterfaceShortcut: Keys;
    ItemImplementationShortcut: Keys;
    ItemRemoveNonPrefixShortCut: Keys;
    ItemFindNamespacesShortCut: Keys;
    TrayAreaInjector: Boolean;
    TrayAreaPersonality: Boolean;
  public
    class function Load: TOptions;
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  System.IO;

{ TOptions }

constructor TOptions.Create;
begin
  inherited Create;
  KnownNamespaceHash := HashTable.Create;
  ClassPrefix := 'T';
  Indent := 2;
  AddPrefixDefault := true;
  RemoveUnprefixedDefault := false;
  PrefixUnitPath := '';
  TypeListPath := '';
  ItemInterfaceShortcut := Keys.None;
  ItemImplementationShortcut := Keys.None;
  ItemRemoveNonPrefixShortCut := Keys.None;
  ItemFindNamespacesShortCut := Keys.None;
end;

destructor TOptions.Destroy;
begin
  KnownNamespaceHash.Clear;
  inherited Destroy;
end;

procedure TOptions.Hash;
var
  Files: array of string;
  S: string;
  I: Integer;
begin
  UnitPostfix := '.' + ClassPrefix.ToUpper;
  UsesIndent := System.string.Create(' ',Indent);
  KnownNamespaceHash.Clear;
  try
    Files := Directory.GetFileSystemEntries(PrefixUnitPath,'*.pas');
    for I := 0 to Pred(Length(Files)) do begin
      S := Path.GetFileNameWithoutExtension(Files[I]).ToUpper;
      if S.EndsWith(UnitPostfix.ToUpper) then begin
        S := S.Substring(0,S.Length - UnitPostfix.Length);
        if S <> 'SYSTEM' then
          KnownNamespaceHash.Add(S,nil);
      end;
    end;
  except
    ;
  end;
  if KnownNamespaceHash.Count = 0 then
    AddPrefixDefault := false;
end;

class function TOptions.Load: TOptions;
begin
  try
    Result := TOptions.LoadFrom(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + 'PUses.xml') as TOptions;
  except
    Result := TOptions.Create;
  end;
  Result.Hash;
end;


end.
