unit Commons.Settings;

interface

{$region 'Commons.Settings'} {
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
  strict private
    class var
      PreloadedSerializers: HashTable;
      FPreloadRequired: Boolean;
    class constructor Create;
  strict private
    Serializer: XmlSerializer;
  public
    class property PreloadRequired: Boolean read FPreloadRequired write FPreloadRequired;
    class procedure PreloadSerializer(const SerializerAssemblyFile, SerializerTypeName: string);
    procedure StoreTo(Stream: System.IO.Stream); overload;
    procedure StoreTo(const FileName: string); overload;
    procedure StoreTo(Node: XmlDocument); overload;
    class function LoadFrom(Stream: System.IO.Stream): TObject; overload;
    class function LoadFrom(const FileName: string): TObject; overload;
    class function LoadFrom(Node: XmlDocument): TObject; overload;
    class function LoadFrom(Node: XmlNode): TObject; overload;
    function Clone: TObject;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TSettingsObjectClass = class of TSettingsObject;

  TSettingsSerializer = class (TObject)
  strict private
    TargetClassInfo: &Type;
    Attributes: XmlAttributes;
    AttributeOverrides: XmlAttributeOverrides;
    Serializer: XmlSerializer;
    FPreloadRequired: Boolean;
    procedure InitSerializer;
  public
    property PreloadRequired: Boolean read FPreloadRequired write FPreloadRequired;
    procedure ExcludeField(const FieldName: string; DefiningType: &Type);
    procedure StoreTo(Target: TObject; Stream: System.IO.Stream); overload;
    procedure StoreTo(Target: TObject; const FileName: string); overload;
    procedure StoreTo(Target: TObject; Node: XmlDocument); overload;
    function LoadFrom(Stream: System.IO.Stream): TObject; overload;
    function LoadFrom(const FileName: string): TObject; overload;
    function LoadFrom(Node: XmlDocument): TObject; overload;
    function LoadFrom(Node: XmlNode): TObject; overload;
    procedure PreloadSerializer(const SerializerAssemblyFile: string; const SerializerTypeName: string);
    constructor Create(TargetClass: TClass);
    destructor Destroy; override;
  end;

implementation

uses
  System.Diagnostics,
  System.Reflection;

{ TSettingsObject }

class constructor TSettingsObject.Create;
begin
  PreloadedSerializers := HashTable.Create;
  FPreloadRequired := not Debugger.IsAttached;
end;

class procedure TSettingsObject.PreloadSerializer(const SerializerAssemblyFile, SerializerTypeName: string);
var
  XMLAssembly: Assembly;
  Serializer: XMLSerializer;
  FullName: string;
begin
  FullName := Self.ClassInfo.FullName;
  Serializer := PreloadedSerializers[FullName] as XMLSerializer;
  if not Assigned(Serializer) then begin
    XMLAssembly := Assembly.GetExecutingAssembly;
    try
      Serializer := XMLAssembly.CreateInstance(SerializerTypeName) as XMLSerializer;
    except
      ;
    end;
  end;
  if not Assigned(Serializer) then begin
    try
      XMLAssembly := Assembly.LoadFrom(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location),
                                                    SerializerAssemblyFile));
      Serializer := XMLAssembly.CreateInstance(SerializerTypeName,true) as XMLSerializer;
    except
      on E: Exception do
        if FPreloadRequired then
          raise TypeLoadException.Create('Could not preload XMLSerializer for type ' + FullName,E);
    end;
  end;
  if Assigned(Serializer) then
    PreloadedSerializers.Add(FullName,Serializer)
  else if FPreloadRequired then
    raise TypeLoadException.Create('Could not preload XMLSerializer for type ' + FullName);
end;

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
  Serializer := PreloadedSerializers[Self.ClassInfo.FullName] as XmlSerializer;
  if not Assigned(Serializer) then
    Serializer := XmlSerializer.Create(ClassInfo);
  Result := Serializer.Deserialize(Stream);
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
  Serializer := PreloadedSerializers[Self.ClassInfo.FullName] as XmlSerializer;
  if not Assigned(Serializer) then
    Serializer := XmlSerializer.Create(ClassInfo);
  Result := Serializer.Deserialize(XmlNodeReader.Create(Node));
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

destructor TSettingsObject.Destroy;
begin
  Serializer.Free;
  inherited Destroy;
end;

{ TSettingsSerializer }

constructor TSettingsSerializer.Create(TargetClass: TClass);
begin
  inherited Create;
  TargetClassInfo := TargetClass.ClassInfo;
  FPreloadRequired := not Debugger.IsAttached;
end;

destructor TSettingsSerializer.Destroy;
begin
  Serializer.Free;
  AttributeOverrides.Free;
  Attributes.Free;
  inherited Destroy;
end;

procedure TSettingsSerializer.PreloadSerializer(const SerializerAssemblyFile, SerializerTypeName: string);
var
  XMLAssembly: Assembly;
begin
  if not Assigned(Serializer) then begin
    XMLAssembly := Assembly.GetExecutingAssembly;
    try
      Serializer := XMLAssembly.CreateInstance(SerializerTypeName) as XMLSerializer;
    except
      ;
    end;
  end;
  if not Assigned(Serializer) then begin
    try
      XMLAssembly := Assembly.LoadFrom(Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location),
                                                    SerializerAssemblyFile));
      Serializer := XMLAssembly.CreateInstance(SerializerTypeName) as XMLSerializer;
    except
      on E: Exception do
        if FPreloadRequired then
          raise TypeLoadException.Create('Could not preload XMLSerializer for type ' + TargetClassInfo.FullName,E);
    end;
  end;
  if not Assigned(Serializer) and FPreloadRequired then
    raise TypeLoadException.Create('Could not preload XMLSerializer for type ' + TargetClassInfo.FullName);
end;

procedure TSettingsSerializer.InitSerializer;
begin
  if not Assigned(Serializer) then begin
    if not Assigned(AttributeOverrides) then
      Serializer := XmlSerializer.Create(TargetClassInfo)
    else
      Serializer := XmlSerializer.Create(TargetClassInfo,AttributeOverrides);
  end;
end;

procedure TSettingsSerializer.ExcludeField(const FieldName: string; DefiningType: &Type);
begin
  if not Assigned(Attributes) then begin
    Attributes := XmlAttributes.Create;
    Attributes.XmlIgnore := true;
  end;
  if not Assigned(AttributeOverrides) then
    AttributeOverrides := XmlAttributeOverrides.Create;
  AttributeOverrides.Add(DefiningType,FieldName,Attributes);
end;

procedure TSettingsSerializer.StoreTo(Target: TObject; Stream: System.IO.Stream);
begin
  InitSerializer;
  Serializer.Serialize(Stream,Target);
end;

procedure TSettingsSerializer.StoreTo(Target: TObject; const FileName: string);
var
  Stream: System.IO.Stream;
begin
  Stream := FileStream.Create(FileName,FileMode.Create);
  try
    StoreTo(Target,Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;

procedure TSettingsSerializer.StoreTo(Target: TObject; Node: XmlDocument);
var
  Stream: System.IO.Stream;
begin
  Stream := MemoryStream.Create;
  try
    StoreTo(Target,Stream);
    Stream.Position := 0;
    Node.RemoveAll;
    Node.Load(Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;

function TSettingsSerializer.LoadFrom(Stream: System.IO.Stream): TObject;
begin
  InitSerializer;
  Result := Serializer.Deserialize(Stream);
end;

function TSettingsSerializer.LoadFrom(const FileName: string): TObject;
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

function TSettingsSerializer.LoadFrom(Node: XmlDocument): TObject;
begin
  Result := LoadFrom(XmlNode(Node));
end;

function TSettingsSerializer.LoadFrom(Node: XmlNode): TObject;
begin
  InitSerializer;
  Result := Serializer.Deserialize(XmlNodeReader.Create(Node));
end;


end.
