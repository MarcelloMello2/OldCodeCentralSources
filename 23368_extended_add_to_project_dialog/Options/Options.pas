unit Options;

interface

uses
  Commons.Settings,
  System.Reflection,
  System.Drawing,
  System.Xml.Serialization,
  System.Windows.Forms;

type
  TOptions = class (TSettingsObject)
  public
    OpenFilesShortcut: Keys;
  public
    class function Load: TOptions;
    constructor Create; override;
  end;

  TAddNewDialog = class (TObject)
    Size: System.Drawing.Size;
    Location: Point;
  end;
  
  TOpenDialog = class (TObject)
    Size: System.Drawing.Size;
    Location: Point;
    ColumnFileWidth: Integer;
    Sorted: Boolean;
  end;

  TApplicationSts = class (TSettingsObject)
  public
    OpenDialog: TOpenDialog;
    AddNewDialog: TAddNewDialog;
    SearchPath: string;
  public
    class function Load: TApplicationSts;
    procedure Store;
    constructor Create; override;
  end;

implementation

uses
  System.IO,
  System.IO.IsolatedStorage;

const
  XmlFileName = 'AddMany.xml';

class function TOptions.Load: TOptions;
begin
  try
    Result := TOptions.LoadFrom(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location) + Path.DirectorySeparatorChar + XmlFileName) as TOptions;
  except
    Result := TOptions.Create;
  end;
end;

constructor TOptions.Create;
begin
  inherited Create;
  OpenFilesShortcut := Keys.None;
end;

constructor TApplicationSts.Create;
begin
  inherited Create;
  OpenDialog := TOpenDialog.Create;
  AddNewDialog := TAddNewDialog.Create;
  SearchPath := Environment.GetFolderPath(Environment.SpecialFolder.MyComputer);
end;

class function TApplicationSts.Load: TApplicationSts;
var
  Stream: System.IO.Stream;
begin
  try
    Stream := IsolatedStorageFileStream.Create(XmlFileName,FileMode.Open,FileAccess.Read,IsolatedStorageFile.GetUserStoreForAssembly);
    try
      Result := TApplicationSts.LoadFrom(Stream) as TApplicationSts;
    finally
      Stream.Close;
      Stream.Free;
    end;
  except
    Result := TApplicationSts.Create;
  end;
end;

procedure TApplicationSts.Store;
var
  Stream: System.IO.Stream;
begin
  Stream := IsolatedStorageFileStream.Create(XmlFileName,FileMode.Create,FileAccess.ReadWrite,IsolatedStorageFile.GetUserStoreForAssembly);
  try
    StoreTo(Stream);
  finally
    Stream.Close;
    Stream.Free;
  end;
end;


end.
