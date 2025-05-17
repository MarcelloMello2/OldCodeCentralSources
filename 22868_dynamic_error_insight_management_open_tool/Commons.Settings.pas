unit Commons.Settings;

interface

{$region 'Commons.Settings unit'} {
class TSettingsObject
  Is an abstract class to be derived to obtain a specific custom setting class. Any public field (both
  value and reference types) of this class can be saved to an XML file or Stream using the StoreTo methods.
  LoadFrom class methods create a new instance of the class and fill its public fields from a XML file,
  a Stream or a XMLNode. If the class name is TMySettings the XML format is:

    <?xml version="1.0"?>
    <TMySettings xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
    :
    </TMySettings>

  N.B. Derived classes are to be public, that is declared inside the interface part of units.
} {$endregion}

uses
  System.Collections,
  System.IO,
  System.Xml,
  System.Xml.Serialization;

type
  TSettingsObject = class abstract (TObject,ICloneable)
  private
    Serializer: XmlSerializer;
  public
    procedure StoreTo(Stream: System.IO.Stream); overload;
    procedure StoreTo(const FileName: string); overload;
    procedure StoreTo(Node: XmlDocument); overload;
    class function LoadFrom(Stream: System.IO.Stream): TObject; overload;
    class function LoadFrom(const FileName: string): TObject; overload;
    class function LoadFrom(Node: XmlDocument): TObject; overload;
    class function LoadFrom(Node: XmlNode): TObject; overload;
    function Clone: TObject;
    constructor Create; virtual;
  end;

  TSettingsObjectClass = class of TSettingsObject;

implementation

{ TSettingsObject }

procedure TSettingsObject.StoreTo(Stream: System.IO.Stream);
begin
  if not Assigned(Serializer) then
    Serializer := XmlSerializer.Create(ClassInfo);
  Serializer.Serialize(Stream,Self);
end;

procedure TSettingsObject.StoreTo(const FileName: string);
var
  Stream: System.IO.Stream;
begin
  Stream := FileStream.Create(FileName,FileMode.Create);
  try
    StoreTo(Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;

procedure TSettingsObject.StoreTo(Node: XmlDocument);
var
  Stream: System.IO.Stream;
begin
  Stream := MemoryStream.Create;
  try
    StoreTo(Stream);
    Stream.Position := 0;
    Node.RemoveAll;
    Node.Load(Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;

class function TSettingsObject.LoadFrom(Stream: System.IO.Stream): TObject;
var
  Serializer: XmlSerializer;
begin
  Serializer := XmlSerializer.Create(ClassInfo);
  Result := Serializer.Deserialize(Stream) as TObject;
  (Result as TSettingsObject).Serializer := Serializer;
end;

class function TSettingsObject.LoadFrom(const FileName: string): TObject;
var
  Stream: System.IO.Stream;
begin
  Stream := FileStream.Create(FileName,FileMode.Open,FileAccess.Read);
  try
    Result := LoadFrom(Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;

class function TSettingsObject.LoadFrom(Node: XmlDocument): TObject;
begin
  Result := LoadFrom(XmlNode(Node));
end;

class function TSettingsObject.LoadFrom(Node: XmlNode): TObject;
var
  Serializer: XmlSerializer;
begin
  Serializer := XmlSerializer.Create(ClassInfo);
  Result := Serializer.Deserialize(XmlNodeReader.Create(Node)) as TObject;
  (Result as TSettingsObject).Serializer := Serializer;
end;

function TSettingsObject.Clone: TObject;
var
  Stream: MemoryStream;
begin
  Stream := MemoryStream.Create;
  StoreTo(Stream);
  Stream.Position := 0;
  Result := Serializer.Deserialize(Stream) as TObject;
  Stream.Free;
end;

constructor TSettingsObject.Create; {Virtual constructor needed for metaclass management}
begin
  inherited Create;
end;


end.
