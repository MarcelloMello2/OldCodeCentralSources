unit Options;

interface

uses
  Commons.Settings,
  System.Reflection,
  System.Xml.Serialization,
  System.Windows.Forms;

type
  TOptions = class(TSettingsObject)
  public
    DynamicEnabledDefault: Boolean;
    ItemCheckShortcut: Keys;
    TrayAreaInjector: Boolean;
  public
    class function Load: TOptions;
    constructor Create; override;
  end;

implementation

uses
  System.IO;

class function TOptions.Load: TOptions;
begin
  try
    Result := TOptions.LoadFrom(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + 'DEInsight.xml') as TOptions;
  except
    Result := TOptions.Create;
  end;
end;

constructor TOptions.Create;
begin
  inherited Create;
  ItemCheckShortcut := Keys.None;
end;


end.
