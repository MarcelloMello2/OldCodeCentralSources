unit Options;

interface

uses
  Commons.Settings,
  ProjectOptionsSets,
  System.Collections,
  System.Reflection,
  System.Xml.Serialization,
  System.Windows.Forms;

type
  [XmlInclude(typeof(TProjectOption))]
  TReleaseProjectOption = class (TObject)
  public
    Name: string;
    Value: TObject;
    Conditions: ArrayList; {of TProjectOption}
    constructor Create;
  end;
  [XmlInclude(typeof(TReleaseProjectOption))]
  TOptions = class(TSettingsObject)
  public
    [XmlIgnore]
    HashedIgnoreOptions: HashTable;
    [XmlIgnore]
    HashedReleaseValues: HashTable;
    IgnoreOptions: ArrayList; {of string}
    TrayAreaInjector: Boolean;
    ReleaseValues: ArrayList; {of TReleaseProjectOption}
  public
    procedure ModifyAsRelease(OptionsSet: TProjectOptionsSet);
    procedure Clear;
    procedure HashRefresh;
    class function Load: TOptions;
    procedure Store;
    constructor Create; override;
  end;

implementation

uses
  System.IO;

{ TReleaseProjectOption }

constructor TReleaseProjectOption.Create;
begin
  inherited Create;
  Conditions := ArrayList.Create;
end;

{ TOptions }

procedure TOptions.ModifyAsRelease(OptionsSet: TProjectOptionsSet);
var
  Option: TObject;
  OptionsHash,ReleaseOptionsHash: HashTable;

  function IsSatisfied(Condition: TProjectOption): Boolean;
  var
    Value: TObject;
  begin
    if OptionsHash.Contains(Condition.Name.ToUpper) then begin
      Value := OptionsHash[Condition.Name.ToUpper];
      if not Assigned(Value) and not Assigned(Condition.Value) then
        Result := true
      else if (Assigned(Value) xor Assigned(Condition.Value)) then
        Result := false
      else
        Result := (System.String.Compare(Value.ToString,Condition.Value.ToString,true) = 0);
    end
    else
      Result := false;
  end;

  function AreSatisfied(Conditions: ArrayList): Boolean;
  var
    I: Integer;
  begin
    Result := true;
    for I := 0 to Pred(Conditions.Count) do begin
      Result := IsSatisfied(Conditions[I] as TProjectOption);
      if not Result then
        Break;
    end;
  end;

begin
  OptionsHash := HashTable.Create;
  ReleaseOptionsHash := HashTable.Create;
  for Option in OptionsSet.Values do with Option as TProjectOption do
    OptionsHash.Add(Name.ToUpper,Value);
  for Option in ReleaseValues do with Option as TReleaseProjectOption do
    if AreSatisfied(Conditions) then
      ReleaseOptionsHash.Add(Name.ToUpper,Value);
  for Option in OptionsSet.Values do with Option as TProjectOption do
    if ReleaseOptionsHash.Contains(Name.ToUpper) then
      Value := ReleaseOptionsHash[Name.ToUpper];
  OptionsHash.Clear;
  ReleaseOptionsHash.Clear;
end;

procedure TOptions.Clear;
var
  I: Integer;
begin
  HashedIgnoreOptions.Clear;
  HashedReleaseValues.Clear;
  TrayAreaInjector := false;
  IgnoreOptions.Clear;
  for I := 0 to Pred(ReleaseValues.Count) do
    TReleaseProjectOption(ReleaseValues[I]).Conditions.Clear;
  ReleaseValues.Clear;
end;

procedure TOptions.HashRefresh;
var
  I: Integer;
begin
  HashedIgnoreOptions.Clear;
  for I := 0 to Pred(IgnoreOptions.Count) do
    HashedIgnoreOptions.Add(string(IgnoreOptions[I]).ToUpper,TObject(I));
  HashedReleaseValues.Clear;
  for I := 0 to Pred(ReleaseValues.Count) do
    HashedReleaseValues.Add(TReleaseProjectOption(ReleaseValues[I]).Name.ToUpper,TObject(I));
end;

class function TOptions.Load: TOptions;
begin
  try
    Result := TOptions.LoadFrom(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + 'OptionsSets.xml') as TOptions;
  except
    Result := TOptions.Create;
  end;
  Result.HashRefresh;
end;

procedure TOptions.Store;
begin
  StoreTo(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + 'OptionsSets.xml');
end;

constructor TOptions.Create;
begin
  inherited Create;
  IgnoreOptions := ArrayList.Create;
  ReleaseValues := ArrayList.Create;
  HashedIgnoreOptions := HashTable.Create;
  HashedReleaseValues := HashTable.Create;
end;


end.
