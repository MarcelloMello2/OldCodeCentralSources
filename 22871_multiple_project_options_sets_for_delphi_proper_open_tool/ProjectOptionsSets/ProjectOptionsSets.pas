unit ProjectOptionsSets;

interface

uses
  Commons.Settings,
  System.Collections,
  System.IO,
  System.Reflection,
  System.Xml.Serialization;

type
  TProjectOption = class (TObject)
  public
    Name: string;
    Value: TObject;
  end;
  [XmlInclude(typeof(TProjectOption))]
  TProjectOptionsSet = class (TObject,ICloneable)
  public
    Name: string;
    Values: ArrayList; {of TProjectOption}
  public
    function Clone: TObject;
    constructor Create;
  end;
  [XmlInclude(typeof(TProjectOptionsSet))]
  TProjectOptionsSets = class(TSettingsObject)
  public
    LastActivated: string;
    Item: ArrayList; {of TProjectOptionsSet}
  public
    constructor Create; override;
  end;

implementation

{ TProjectOptionsSet }

function TProjectOptionsSet.Clone: TObject;
begin
  Result := Self.MemberwiseClone;
end;

constructor TProjectOptionsSet.Create;
begin
  inherited Create;
  Values := ArrayList.Create;
  Name := '';
end;

constructor TProjectOptionsSets.Create;
begin
  inherited Create;
  Item := ArrayList.Create;
  LastActivated := '';
end;


end.
