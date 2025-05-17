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
  public
    ItemRemoveShortcut: Keys;
    ItemAddShortcut: Keys;
  public
    class function Load: TOptions;
    constructor Create; override;
  end;

implementation

uses
  System.IO;

{ TOptions }

class function TOptions.Load: TOptions;
begin
  try
    Result := TOptions.LoadFrom(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + 'Regionalize.xml') as TOptions;
  except
    Result := TOptions.Create;
  end;
end;

constructor TOptions.Create;
begin
  inherited Create;
  ItemAddShortcut := Keys.None;
  ItemRemoveShortcut := Keys.None;
end;


end.
