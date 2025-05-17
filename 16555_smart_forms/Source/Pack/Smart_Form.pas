unit Smart_Form;

{

Copyright (c) 2001 by Alexander Rodygin
rodigin@yahoo.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software") to use, copy, publish, distribute or sell copies of the Software
without modifications, and to permit persons to whom the Software is furnished
to do so, subject to the following conditions:

THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL Alexander Rodygin BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

}

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
  ESmartFormException = class(Exception);

type
  TSavedComponent = class(TCollectionItem)
  private
    FComponent: TComponent;
    FProperties: TStringList;
    FSaveName: string;
    FCustomProperties: TStringList;
    procedure SetComponent(const Value: TComponent);
    function GetProperties: TStrings;
    procedure SetProperties(const Value: TStrings);
    procedure SetSaveName(const Value: string);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property CustomProperties: TStringList read FCustomProperties write FCustomProperties;
  published
    property Component: TComponent read FComponent write SetComponent;
    property Properties: TStrings read GetProperties write SetProperties;
    property SaveName: string read FSaveName write SetSaveName;
  end;

type
  TSavedComponentClass = class of TSavedComponent;

type
  TSavedComponents = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TSavedComponent;
    procedure SetItem(Index: Integer; Value: TSavedComponent);
    function GetOwner: TComponent; reintroduce;
    function GetValidSaveName(Sender: TObject; const Value: string): string;
  public
    function Add: TSavedComponent;
    property Items[Index: Integer]: TSavedComponent read GetItem write SetItem; default;
  end;

type
  TStorageComponent = class(TComponent)
  public
    // Loads settings of particular component from memory. If settings do not
    // exist in the memory does nothing.
    procedure LoadComponentProps(AComponent: TSavedComponent); virtual; abstract;
    // Saves settings of particular component to memory overwriting previous
    // settings of that component. Does not persist them.
    procedure SaveComponentProps(AComponent: TSavedComponent);  virtual; abstract;
    // Deletes all settings from memory.
    procedure Dispose; virtual; abstract;
    // Loads all settings to memory.
    function Retrieve(const Path: string): Boolean; virtual; abstract;
    // Writes settings to persistent storage. Does not clear memory.
    procedure Persist(const Path: string); virtual; abstract;
    // Deletes settings of particular component from the storage.
    procedure DeleteSettings(const Path: string); virtual; abstract;
    // Delete the whole storage. May affect other storage components.
    // Intended to get rid of out of date settings.
    procedure DeleteStorage(const Path: string); virtual; abstract;
  end;

type
  TSmartStorage = class(TPersistent)
  private
    FOwner: TComponent;
    FComponent: TStorageComponent;
    FEnabled: Boolean;
    FPath: string;
    FParsedPath: string;
    FInMemory: Boolean;
    procedure SetPath(const Value: string);
    procedure SetParsedPath(const Value: string);
    function GetComponent: TStorageComponent;
    procedure SetComponent(Value: TStorageComponent);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;
    property InMemory: Boolean read FInMemory write FInMemory;
  published
    property Path: string read FPath write SetPath;
    property ParsedPath: string read FParsedPath write SetParsedPath;
    property Component: TStorageComponent read GetComponent write SetComponent;
    property Enabled: Boolean read FEnabled write FEnabled nodefault;
  end;

type
  TSavedComponentEvent = procedure(SavedComponent: TSavedComponent) of object;

type
  TSmartFormAbout = type string;

type
  TSmartForm = class(TForm)
  private
    FAbout: TSmartFormAbout;
    FDefaultSettings: Boolean;
    FSavedComponents: TSavedComponents;
    FStorage: TSmartStorage;
    FSavedProperties: TStrings;
    FOnLoadBegin: TNotifyEvent;
    FOnLoadComponentBegin: TSavedComponentEvent;
    FOnLoadComponentEnd: TSavedComponentEvent;
    FOnLoadEnd: TNotifyEvent;
    FOnSaveBegin: TNotifyEvent;
    FOnSaveComponentBegin: TSavedComponentEvent;
    FOnSaveComponentEnd: TSavedComponentEvent;
    FOnSaveEnd: TNotifyEvent;
    FSaveName: string;
    procedure SetSavedComponents(const Value: TSavedComponents);
    procedure SetStorage(const Value: TSmartStorage);
    procedure SetSavedProperties(const Value: TStrings);
    procedure SetSaveName(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;
    procedure SaveComponent(AComponent: TSavedComponent);
    procedure LoadComponent(AComponent: TSavedComponent);
    procedure RemoveSavedComponent(AComponent: TComponent);
    function MakeSavedComponent(AComponent: TComponent; AProperties: TStrings; const ASaveName: string): TSavedComponent;
    function SaveComponentsToMemory: Boolean;
    procedure LoadComponentsFromMemory;
    property DefaultSettings: Boolean read FDefaultSettings write FDefaultSettings;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    constructor Create(AOwner: TComponent; DefaultSettings: Boolean); reintroduce; overload;
    constructor Create(AOwner: TComponent); overload; override;
    destructor Destroy; override;
    procedure SaveComponents;
    procedure LoadComponents;
    function EditComponents: Boolean;
  published
    property AboutSmartForm: TSmartFormAbout read FAbout write FAbout;
    property SavedComponents: TSavedComponents read FSavedComponents write SeTSavedComponents;
    property SavedProperties: TStrings read FSavedProperties write SetSavedProperties;
    property SaveName: string read FSaveName write SetSaveName;
    property Storage: TSmartStorage read FStorage write SetStorage;
    property OnLoadBegin: TNotifyEvent read FOnLoadBegin write FOnLoadBegin;
    property OnLoadComponentBegin: TSavedComponentEvent read FOnLoadComponentBegin write FOnLoadComponentBegin;
    property OnLoadComponentEnd: TSavedComponentEvent read FOnLoadComponentEnd write FOnLoadComponentEnd;
    property OnLoadEnd: TNotifyEvent read FOnLoadEnd write FOnLoadEnd;
    property OnSaveBegin: TNotifyEvent read FOnSaveBegin write FOnSaveBegin;
    property OnSaveComponentBegin: TSavedComponentEvent read FOnSaveComponentBegin write FOnSaveComponentBegin;
    property OnSaveComponentEnd: TSavedComponentEvent read FOnSaveComponentEnd write FOnSaveComponentEnd;
    property OnSaveEnd: TNotifyEvent read FOnSaveEnd write FOnSaveEnd;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
implementation
uses
  TypInfo, Consts, IniFiles, Registry, ShlObj;

{$R *.DFM}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSmartWriter }

type
  TSmartWriter = class(TWriter)
  private
    FLookupRoot: TComponent;
    FSavedComponent: TSavedComponent;
    FPropertyLevel: Integer;
    FPropPath: string;
    function IsStoredProp(const Name: string): Boolean;
  protected
    procedure WriteProperty(Instance: TPersistent; PropInfo: Pointer);
    procedure WriteProperties(Instance: TPersistent);
    procedure WritePropName(const PropName: string);
  public
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    procedure WriteCollection(Value: TCollection);
    procedure WriteSavedComponent(Component: TSavedComponent);
    property LookupRoot: TComponent read FLookupRoot write FLookupRoot;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ Global }

procedure GetPublishedProperties(AObject: TObject; AStrings: TStrings);
var
  PropList: PPropList;
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  I: integer;
begin
  TypeInfo:= AObject.ClassInfo;
  TypeData := GetTypeData(TypeInfo);
  if TypeData.PropCount <> 0 then
  begin
    GetMem(PropList, SizeOf(PPropInfo) * TypeData.PropCount);
    try
      GetPropInfos(AObject.ClassInfo, PropList);
      for I:= 0 to TypeData.PropCount - 1 do
        if (PropList[I]^.PropType^.Kind <> tkMethod) then
          AStrings.Add(PropList[I]^.Name);
    finally
      FreeMem(PropList, SizeOf(PPropInfo) * TypeData.PropCount);
    end;
  end;
end;

function TextToComponent(Value: string; Component:TComponent): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream:= TStringStream.Create(Value);
  try
    BinStream:= TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result:= BinStream.ReadComponent(Component);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

function ComponentToText(Component: TSavedComponent; LookupRoot: TComponent): string;
var
  BinStream:TMemoryStream;
  StrStream: TStringStream;
  Writer: TSmartWriter;
  S: string;
begin
  BinStream := TMemoryStream.Create;
  StrStream:= TStringStream.Create(S);
  Writer:= TSmartWriter.Create(BinStream, 4096);
  try
    try
      Writer.WriteSavedComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    except
    end;
  finally
    Writer.Free;
    StrStream.Free;
    BinStream.Free
  end;
end;

function CompareDigits(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result:= StrToInt(List.Names[Index1]) - StrToInt(List.Names[Index2]);
end; 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TRegIniStorage }

type
  TRegIniStorage = class(TStorageComponent)
  private
    FSection: string;
    FStoredProps: TStringList;
    FStorageHelper: TCustomIniFile;
    FHelpList: TStrings;
    function PathToClass(const Path: string): TCustomIniFile;
    procedure DeleteSection(Storage: TCustomIniFile);
    procedure Delete(Storage: TCustomIniFile);
    procedure CloseStorage;
    procedure ResolveOwnerName(List: TStrings; ToName: Boolean);
  public
    procedure LoadComponentProps(AComponent: TSavedComponent); override;
    procedure SaveComponentProps(AComponent: TSavedComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Dispose; override;
    function Retrieve(const Path: string): Boolean; override;
    procedure Persist(const Path: string); override;
    procedure DeleteStorage(const Path: string); override;
    procedure DeleteSettings(const Path: string); override;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSmartWriter}

procedure TSmartWriter.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc; HasData: Boolean);
begin
  if HasData and Assigned(WriteData) and IsStoredProp(Name) then
  begin
    WritePropName(Name);
    WriteData(Self);
  end;
end;

procedure TSmartWriter.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc; HasData: Boolean);
begin
  if HasData and Assigned(WriteData) and IsStoredProp(Name) then
  begin
    WritePropName(Name);
    WriteBinary(WriteData);
  end;
end;

function TSmartWriter.IsStoredProp(const Name: string): Boolean;
begin
  if FPropertyLevel <= 1 then
    Result:= FSavedComponent.Properties.IndexOf(Name) >= 0
  else
    Result:= True;
end;

procedure TSmartWriter.WriteCollection(Value: TCollection);
var
  I: Integer;
begin
  WriteValue(vaCollection);
  if Value <> nil then
    for I := 0 to Value.Count - 1 do
    begin
      WriteListBegin;
      WriteProperties(Value.Items[I]);
      WriteListEnd;
    end;
  WriteListEnd;
end;

procedure TSmartWriter.WriteSavedComponent(Component: TSavedComponent);
begin
  FSavedComponent:= Component;
  Root:= FSavedComponent.Component;
  LookupRoot:= Root;
  WriteSignature;
  WritePrefix([], 0);
  WriteStr(Root.ClassName);
  WriteStr(Root.Name);
  WriteProperties(Root);
  WriteListEnd;
  WriteListEnd;
  FlushBuffer;
end;

type
  TComp = class(TComponent);
  
procedure TSmartWriter.WriteProperties(Instance: TPersistent);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  PropList: PPropList;
begin
  Inc(FPropertyLevel);
  try
    Count := GetTypeData(Instance.ClassInfo)^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        GetPropInfos(Instance.ClassInfo, PropList);
        for I := 0 to Count - 1 do
        begin
          PropInfo := PropList^[I];
          if PropInfo = nil then break;
          if IsStoredProp(PropInfo^.Name) then
            WriteProperty(Instance, PropInfo);
        end;
      finally
        FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;
    TComp(Instance).DefineProperties(Self);
  finally
    Dec(FPropertyLevel);
  end;
end;

procedure TSmartWriter.WriteProperty(Instance: TPersistent; PropInfo: Pointer);
var
  PropType: PTypeInfo;

  function AncestorValid: Boolean;
  begin
    Result:= (Ancestor <> nil) and ((Instance.ClassType = Ancestor.ClassType) or (Instance = Root));
  end;

  procedure WritePropPath;
  begin
    WritePropName(PPropInfo(PropInfo)^.Name);
  end;

  procedure WriteSet(Value: Longint);
  var
    I: Integer;
    BaseType: PTypeInfo;
  begin
    BaseType := GetTypeData(PropType)^.CompType^;
    WriteValue(vaSet);
    for I := 0 to SizeOf(TIntegerSet) * 8 - 1 do
      if I in TIntegerSet(Value) then WriteStr(GetEnumName(BaseType, I));
    WriteStr('');
  end;

  procedure WriteIntProp(IntType: PTypeInfo; Value: Longint);
  var
    Ident: string;
    IntToIdent: TIntToIdent;
  begin
    IntToIdent := FindIntToIdent(IntType);
    if Assigned(IntToIdent) and IntToIdent(Value, Ident) then
      WriteIdent(Ident)
    else
      WriteInteger(Value);
  end;

  procedure WriteCollectionProp(Collection: TCollection);
  var
    SavePropPath: string;
  begin
    WritePropPath;
    SavePropPath := FPropPath;
    try
      FPropPath := '';
      WriteCollection(Collection);
    finally
      FPropPath := SavePropPath;
    end;
  end;

  procedure WriteOrdProp;
  var
    Value: Longint;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    WritePropPath;
    case PropType^.Kind of
      tkInteger:
        WriteIntProp(PPropInfo(PropInfo)^.PropType^, Value);
      tkChar:
        WriteChar(Chr(Value));
      tkSet:
        WriteSet(Value);
      tkEnumeration:
        WriteIdent(GetEnumName(PropType, Value));
    end;
  end;

  procedure WriteFloatProp;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    WritePropPath;
    WriteFloat(Value);
  end;

  procedure WriteInt64Prop;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    WritePropPath;
    WriteInteger(Value);
  end;

  procedure WriteStrProp;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    WritePropPath;
    WriteString(Value);
  end;

  procedure WriteObjectProp;
  var
    Value: TObject;
    OldAncestor: TPersistent;
    SavePropPath, ComponentValue: string;

    function GetComponentValue(Component: TComponent): string;
    begin
      if (Component.Owner = FLookupRoot) then
        Result := Component.Name
      else if Component = FLookupRoot then
        Result := 'Owner'
      else if (Component.Owner <> nil) and (Component.Owner.Name <> '') and
        (Component.Name <> '') then
        Result := Component.Owner.Name + '.' + Component.Name
      else if Component.Name <> '' then
        Result := Component.Name + '.Owner'
      else Result := '';
    end;

  begin
    Value := TObject(GetOrdProp(Instance, PropInfo));
    if (Value = nil) then
    begin
      WritePropPath;
      WriteValue(vaNil);
    end
    else if Value is TPersistent then
      if Value is TComponent then
      begin
        ComponentValue := GetComponentValue(TComponent(Value));
        if ComponentValue <> '' then
        begin
          WritePropPath;
          WriteIdent(ComponentValue);
        end
      end else if Value is TCollection then
      begin
        if not AncestorValid or
          not CollectionsEqual(TCollection(Value),
            TCollection(GetOrdProp(Ancestor, PropInfo))) then
            WriteCollectionProp(TCollection(Value));
      end else
      begin
        OldAncestor := Ancestor;
        SavePropPath := FPropPath;
        try
          FPropPath := FPropPath + PPropInfo(PropInfo)^.Name + '.';
          if AncestorValid then
            Ancestor := TPersistent(GetOrdProp(Ancestor, PropInfo));
          WriteProperties(TPersistent(Value));
        finally
          Ancestor := OldAncestor;
          FPropPath := SavePropPath;
        end;
      end
  end;

  procedure WriteMethodProp;
  var
    Value: TMethod;

    function IsDefaultValue: Boolean;
    var
      DefaultCode: Pointer;
    begin
      DefaultCode := nil;
      if AncestorValid then DefaultCode := GetMethodProp(Ancestor, PropInfo).Code;
      Result := (Value.Code = DefaultCode) or
        ((Value.Code <> nil) and (FLookupRoot.MethodName(Value.Code) = ''));
    end;

  begin
    Value := GetMethodProp(Instance, PropInfo);
    if not IsDefaultValue then
    begin
      WritePropPath;
      if Value.Code = nil then
        WriteValue(vaNil) else
        WriteIdent(FLookupRoot.MethodName(Value.Code));
    end;
  end;

  procedure WriteVariantProp;
  var
    Value: Variant;
    VType: Integer;
  begin
    Value := GetVariantProp(Instance, PropInfo);
    if VarIsArray(Value) then raise EWriteError.CreateRes(@SWriteError);
    WritePropPath;
    VType := VarType(Value);
    case VType and varTypeMask of
      varEmpty: WriteValue(vaNil);
      varNull: WriteValue(vaNull);
      varOleStr: WriteWideString(Value);
      varString: WriteString(Value);
      varByte, varSmallInt, varInteger: WriteInteger(Value);
      varSingle: WriteSingle(Value);
      varDouble: WriteFloat(Value);
      varCurrency: WriteCurrency(Value);
      varDate: WriteDate(Value);
      varBoolean:
        if Value then
          WriteValue(vaTrue)
        else
          WriteValue(vaFalse);
      else
        try
          WriteString(Value);
        except
          raise EWriteError.CreateRes(@SWriteError);
        end;
    end;
  end;

begin
  if (PPropInfo(PropInfo)^.SetProc <> nil) and (PPropInfo(PropInfo)^.GetProc <> nil) then
  begin
    PropType := PPropInfo(PropInfo)^.PropType^;
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet: WriteOrdProp;
      tkFloat: WriteFloatProp;
      tkString, tkLString, tkWString: WriteStrProp;
      tkClass: WriteObjectProp;
      tkMethod: WriteMethodProp;
      tkVariant: WriteVariantProp;
      tkInt64: WriteInt64Prop;
    end;
  end;
end;

procedure TSmartWriter.WritePropName(const PropName: string);
begin
  WriteStr(FPropPath + PropName);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSavedComponent }

constructor TSavedComponent.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FProperties:= TStringList.Create;
  FProperties.Sorted:= True;
  FCustomProperties:= TStringList.Create;
end;

destructor TSavedComponent.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TSavedComponent.GetProperties: TStrings;
begin
  Result:= FProperties;
end;

type THack = class(TOwnedCollection);
  
procedure TSavedComponent.SetComponent(const Value: TComponent);
var
  OwnerForm: TComponent;
begin
  if FComponent <> Value then
  begin
    OwnerForm:= THack(Collection).GetOwner as TComponent;
    FComponent:= Value;
    if (csDesigning in OwnerForm.ComponentState) and not
       (csReading in OwnerForm.ComponentState) then
    begin
      FProperties.Clear;
      if FComponent <> nil then
        GetPublishedProperties(FComponent, FProperties);
    end;
    if not (csLoading in OwnerForm.ComponentState) then
      SaveName:= FComponent.Name;
  end;
end;

procedure TSavedComponent.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

procedure TSavedComponent.SetSaveName(const Value: string);
begin
  if FSaveName <> Value then
  begin
    if Collection is TSavedComponents then
      FSaveName:= TSavedComponents(Collection).GetValidSaveName(self, Value)
    else
      FSaveName:= Value;
  end
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSavedComponents }

function TSavedComponents.Add: TSavedComponent;
begin
  Result:= inherited Add as TSavedComponent;
end;

function TSavedComponents.GetItem(Index: Integer): TSavedComponent;
begin
  Result:= TSavedComponent(inherited GetItem(Index));
end;

function TSavedComponents.GetOwner: TComponent;
begin
  Result:= inherited GetOwner as TComponent;
end;

function TSavedComponents.GetValidSaveName(Sender: TObject; const Value: string): string;
var
  I, J: Integer;
begin
  J:= 0;
  if Sender is TSmartForm then
  begin
    if Value = '' then
      Result:= TSmartForm(Sender).Name
    else
      Result:= Value;
  end
  else if Sender is TSavedComponent then
  begin
    if (Value = '') and (TSavedComponent(Sender).Component <> nil) then
      Result:= TSavedComponent(Sender).Component.Name
    else
      Result:= Value;
    if SameText(TSmartForm(GetOwner).SaveName, Value) then
      Inc(J);
  end
  else
    Exit;
  for I:= 0 to Count - 1 do
    if SameText(Items[I].FSaveName, Result) then
      if Items[I].Component <> TSavedComponent(Sender).Component then
        Inc(J);
  if J > 0 then
    Result:= GetValidSaveName(Sender, Value + IntToStr(J));
end;

procedure TSavedComponents.SetItem(Index: Integer; Value: TSavedComponent);
begin
  inherited SetItem(Index, Value);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSmartStorage }

procedure TSmartStorage.Assign(Source: TPersistent);
var
  Src: TSmartStorage;
begin
  inherited;
  Src:= Source as TSmartStorage;
  self.FOwner:= Src.FOwner;
  self.FComponent:= Src.FComponent;
  self.FEnabled:= Src.FEnabled;
  self.FPath:= Src.FPath;
  self.FParsedPath:= Src.FParsedPath;
end;

constructor TSmartStorage.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner:= AOwner;
  FEnabled:= True;
end;

function TSmartStorage.GetComponent: TStorageComponent;
begin
  if (FComponent = nil) and not (csDesigning in FOwner.ComponentState) then
    FComponent:= TRegIniStorage.Create(FOwner);
  Result:= FComponent;
end;

procedure TSmartStorage.SetComponent(Value: TStorageComponent);
begin
  if FComponent <> Value then
  begin
    if FComponent is TRegIniStorage then
      FComponent.Free;
    FComponent:= Value;
  end;
end;

procedure TSmartStorage.SetParsedPath(const Value: string);
begin
  // set by SetPath;
end;

procedure TSmartStorage.SetPath(const Value: string);

  function ExeNamePart: string;
  var
    I: Integer;
    S: string;
  begin
    S:= ExtractFileName(Application.ExeName);
    I:= Pos('.', S);
    if I > 0 then
      Result:= Copy(S, 1, I - 1)
    else
      Result:= S;
  end;

  function AppDataPath: string;
  begin
    with TRegistry.Create() do
    try
      RootKey:= HKEY_CURRENT_USER;
      if OpenKey(REGSTR_PATH_SPECIAL_FOLDERS, False) then
      begin
        Result:= ReadString('Local AppData');
        if Result = '' then
          Result:= ReadString('AppData');
      end;
    finally
      CloseKey;
      Free;
    end
  end;

begin
  if FPath <> Value then
  begin
    FParsedPath:= StringReplace(Value, '[APPDATA]', AppDataPath, [rfReplaceAll, rfIgnoreCase]);
    FParsedPath:= StringReplace(FParsedPath, '[APPNAME]', ExeNamePart, [rfReplaceAll, rfIgnoreCase]);
    FParsedPath:= StringReplace(FParsedPath, '[APPPATH]', ExtractFilePath(Application.ExeName), [rfReplaceAll, rfIgnoreCase]);
    if not (csDesigning in FOwner.ComponentState) then
      FPath:= FParsedPath
    else
      FPath:= Value;
  end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSmartForm }

procedure TSmartForm.AfterConstruction;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    LoadComponents;
end;

procedure TSmartForm.BeforeDestruction;
begin
  if not (csDesigning in ComponentState) then
    SaveComponents;
  inherited;
end;

constructor TSmartForm.Create(AOwner: TComponent);
begin
  Create(AOwner, False);
end;

constructor TSmartForm.Create(AOwner: TComponent; DefaultSettings: Boolean);
begin
  GlobalNameSpace.BeginWrite;
  try
    CreateNew(AOwner);
    FSavedComponents:= TSavedComponents.Create(self, TSavedComponent);
    FStorage:= TSmartStorage.Create(self);
    FDefaultSettings:= DefaultSettings;
    FSavedProperties:= TStringList.Create;
    (FSavedProperties as TStringList).Sorted:= True;
    if (csDesigning in ComponentState) then
    begin
      GetPublishedProperties(self, FSavedProperties);
    end;
    if (ClassType <> TForm) and not (csDesigning in ComponentState) then
    begin
      Include(FFormState, fsCreating);
      try
        if not InitInheritedComponent(Self, TForm) then
          raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
      finally
        Exclude(FFormState, fsCreating);
      end;
      if OldCreateOrder then DoCreate;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;
end;

destructor TSmartForm.Destroy;
begin
  FSavedProperties.Free;
  FSavedComponents.Free;
  FStorage.Free;
  inherited;
end;

function TSmartForm.EditComponents: Boolean;
var
  Vis: Boolean;
  RetVal: Integer;
begin
  Vis:= Visible;
  Storage.InMemory:= True;
  try
    SaveComponents;
    if Visible then Hide;
    try
      RetVal:= ShowModal;
      Result:= (RetVal = mrOK) or (RetVal = mrYes);
      if Result then
      begin
        Storage.InMemory:= False;
        SaveComponents;
      end
      else
        LoadComponents;
    except
      on E: Exception do
      begin
        Result:= False;
      end;
    end;
  finally
    Storage.InMemory:= False;
    Storage.Component.Dispose;
    Visible:= Vis;
  end
end;

procedure TSmartForm.LoadComponent(AComponent: TSavedComponent);
begin
  if Assigned(OnLoadComponentBegin) then
    OnLoadComponentBegin(AComponent);
  if Storage.Enabled then
    Storage.Component.LoadComponentProps(AComponent);
  if Assigned(OnLoadComponentEnd) then
    OnLoadComponentEnd(AComponent);
end;

procedure TSmartForm.LoadComponents;
var
  SettingsExist: Boolean;
begin
  if (Storage.Enabled) and (not DefaultSettings) then
  begin
    if Assigned(OnLoadBegin) then
      OnLoadBegin(self);
    if not Storage.InMemory then
      SettingsExist:= Storage.Component.Retrieve(Storage.Path)
    else
      SettingsExist:= True;
    if SettingsExist then
    try
      LoadComponentsFromMemory;
    finally
      if not Storage.InMemory then
        Storage.Component.Dispose;
    end;
    if Assigned(OnLoadEnd) then
      OnLoadEnd(self);
  end;
end;

procedure TSmartForm.LoadComponentsFromMemory;
var
  I: Integer;
  SelfItem: TSavedComponent;
begin
  SelfItem:= MakeSavedComponent(self, SavedProperties, SaveName);
  try
    try
      LoadComponent(SelfItem);
      for I:= 0 to SavedComponents.Count - 1 do
        LoadComponent(SavedComponents[I]);
    except
    end
  finally
    SelfItem.Free;
  end;
end;

function TSmartForm.MakeSavedComponent(AComponent: TComponent; AProperties: TStrings; const ASaveName: string): TSavedComponent;
begin
  Result:= TSavedComponent.Create(nil);
  Result.FComponent:= AComponent;
  Result.FProperties.Assign(AProperties);
  Result.FSaveName:= ASaveName
end;

procedure TSmartForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    RemoveSavedComponent(AComponent);
    if AComponent = FStorage.FComponent then
      FStorage.FComponent:= nil
  end;
end;

procedure TSmartForm.RemoveSavedComponent(AComponent: TComponent);
var
  I: Integer;
begin
  for I:= SavedComponents.Count - 1 downto 0 do
    if SavedComponents[I].FComponent = AComponent then
      SavedComponents.Delete(I);
end;

procedure TSmartForm.SaveComponent(AComponent: TSavedComponent);
begin
  if Assigned(OnSaveComponentBegin) then
    OnSaveComponentBegin(AComponent);
  if Storage.Enabled then
    Storage.Component.SaveComponentProps(AComponent);
  if Assigned(OnSaveComponentEnd) then
    OnSaveComponentEnd(AComponent);
end;

procedure TSmartForm.SaveComponents;
begin
  if (Storage.Enabled) and (not DefaultSettings) then
  begin
    if Assigned(OnSaveBegin) then
      OnSaveBegin(self);
    Storage.Component.Dispose;
    try
      if SaveComponentsToMemory and not Storage.InMemory then
        Storage.Component.Persist(Storage.Path);
    finally
      if not Storage.InMemory then
        Storage.Component.Dispose;
    end;
    if Assigned(OnSaveEnd) then
      OnSaveEnd(self);
  end;
end;

function TSmartForm.SaveComponentsToMemory: Boolean;
var
  I: Integer;
  SelfItem: TSavedComponent;
begin
  Result:= False;
  SelfItem:= MakeSavedComponent(self, SavedProperties, SaveName);
  try
    try
      SaveComponent(SelfItem);
      for I:= 0 to SavedComponents.Count - 1 do
        SaveComponent(SavedComponents[I]);
      Result:= True;
    except
    end
  finally
    SelfItem.Free;
  end;
end;

procedure TSmartForm.SetName(const NewName: TComponentName);
begin
  inherited;
  if (csDesigning in ComponentState) and (FSaveName = '') then
    FSaveName:= NewName;
end;

procedure TSmartForm.SetSavedComponents(const Value: TSavedComponents);
begin
  FSavedComponents:= Value;
end;

procedure TSmartForm.SetSavedProperties(const Value: TStrings);
begin
  FSavedProperties.Assign(Value);
end;

procedure TSmartForm.SetSaveName(const Value: string);
begin
  if FSaveName <> Value then
  begin
    FSaveName:= SavedComponents.GetValidSaveName(self, Value)
  end
end;

procedure TSmartForm.SetStorage(const Value: TSmartStorage);
begin
  FStorage:= Value;
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TRegIniStorage }

const
  ObjFmt = 'object %s: %s';
  SaveFmt = '@%s';
  
constructor TRegIniStorage.Create(AOwner: TComponent);
begin
  inherited;
  FStoredProps:= TStringList.Create;
  FHelpList:= TStringList.Create;
end;

destructor TRegIniStorage.Destroy;
begin
  FStoredProps.Free;
  FHelpList.Free;
  inherited;
end;

procedure TRegIniStorage.Dispose;
begin
  FStoredProps.Clear;
end;

procedure TRegIniStorage.LoadComponentProps(AComponent: TSavedComponent);
var
  I, J, L: Integer;
  C: TComponent;
  OwnerName, SaveName, ObjName: string;
begin
  C:= AComponent.Component;
  if (C = nil) or (FStoredProps.Count = 0) then
    Exit;
  SaveName:= Format(SaveFmt, [AComponent.SaveName]);
  ObjName:= Format(ObjFmt, [C.Name, C.ClassName]);
  I:= FStoredProps.IndexOf(SaveName);
  if I >= 0 then
  try
    FHelpList.Clear;
    for J:= I to FStoredProps.Count - 1 do
    begin
      L:= Length(FStoredProps[J]);
      if L > 0 then
      begin
        if J = I then
          FHelpList.Add(ObjName)
        else if FStoredProps[J][1] <> '@' then
        begin
          if FStoredProps[J][1] = '<' then
            FHelpList.Add(Copy(FStoredProps[J], 2, L - 2))
          else if FStoredProps[J][1] = '{' then
            AComponent.CustomProperties.Add(Copy(FStoredProps[J], 2, L - 2));
        end
        else
          Break;
      end;
    end;
    if FHelpList.Count > 1 then
    begin
      FHelpList.Append('end');
      if C = Owner then
        OwnerName:= Owner.Name;
      TextToComponent(FHelpList.Text, C);
      if C = Owner then
        C.Name:= OwnerName;
    end;
  except
    on E: Exception do
      raise ESmartFormException.Create(E.Message);
  end;
end;

procedure TRegIniStorage.Persist(const Path: string);
var
  I: Integer;
begin
  try
    if Owner is TSmartForm then
      FSection:= TSmartForm(Owner).SaveName;
    try
      FStorageHelper:= PathToClass(Path);
      ResolveOwnerName(FStoredProps, False);
      DeleteSection(FStorageHelper);
      for I:= 0 to FStoredProps.Count - 1 do
        FStorageHelper.WriteString(FSection, IntToStr(I), FStoredProps[I]);
    except
      on E: Exception do
      begin
        CloseStorage;
        raise ESmartFormException.Create(E.Message)
      end;
    end;
  finally
    CloseStorage
  end
end;

function TRegIniStorage.Retrieve(const Path: string): Boolean;
var
  I, P: Integer;
  S: string;
begin
  try
    Dispose;
    if Owner is TSmartForm then
      FSection:= TSmartForm(Owner).SaveName;
    try
      FStorageHelper:= PathToClass(Path);
      FStorageHelper.ReadSectionValues(FSection, FStoredProps);
      FStoredProps.CustomSort(CompareDigits);
      for I:= 0 to FStoredProps.Count - 1 do
      begin
        S:= FStoredProps[I];
        P:= AnsiPos('=', S);
        if P > 0 then
        begin
          S:= Copy(S, P + 1, Length(S) - P + 1);
          FStoredProps[I]:= S;
        end;
      end;
      Result:= FStoredProps.Count > 0;
      if Result then
        ResolveOwnerName(FStoredProps, True);
    except
      CloseStorage;
      Result:= False;
    end;
  finally
    CloseStorage;
  end
end;

procedure TRegIniStorage.SaveComponentProps(AComponent: TSavedComponent);

  procedure Prepare(List: TStrings);
  const
    PropTempl = '<%s>';
    CustPropTempl = '{%s}';
  var
    I, Count: Integer;
  begin
    if List.Count < 2 then
    begin
      List.Clear;
      Exit
    end
    else
      List.Delete(List.Count - 1);
    Count:= List.Count;
    List[0]:= Format(SaveFmt, [AComponent.SaveName]);
    for I:= 1 to Count - 1 do
      List[I]:= Format(PropTempl, [Trim(List[I])]);
    Count:= AComponent.CustomProperties.Count;
    for I:= 0 to Count - 1 do
      List.Add(Format(CustPropTempl, [AComponent.CustomProperties[I]]));
  end;

begin
  if AComponent.Component <> nil then
  try
    FHelpList.Clear;
    FHelpList.Text:= ComponentToText(AComponent, Owner);
    Prepare(FHelpList);
    if FHelpList.Count > 1 then
      FStoredProps.AddStrings(FHelpList);
  except
    on E: Exception do
      raise ESmartFormException.Create(E.Message);
  end
end;

procedure TRegIniStorage.Delete(Storage: TCustomIniFile);

  procedure RecursiveDeleteKey(Reg: TRegistry; Key: string);
  var
    Keys: TStrings;
    I: Integer;
  begin
    if Reg.OpenKey(Key, False) then
    begin
      Keys:= TStringList.Create;
      Reg.GetKeyNames(Keys);
      try
        for I:= 0 to Keys.Count - 1 do
          RecursiveDeleteKey(Reg, Keys[I]);
        Reg.DeleteKey(Key);
      finally
        Keys.Free;
      end;
    end;
  end;

begin
  try
    if Storage is TRegistryIniFile then
      with TRegistryIniFile(Storage) do
        RecursiveDeleteKey(RegIniFile, '\' + (RegIniFile as TRegistry).CurrentPath)
    else
      with TIniFile(Storage) do
        DeleteFile(FileName)
  except
    on E: Exception do
      raise ESmartFormException.Create(E.Message);
  end
end;

procedure TRegIniStorage.DeleteSection(Storage: TCustomIniFile);
begin
  Storage.EraseSection(FSection);
end;

procedure TRegIniStorage.DeleteSettings(const Path: string);
begin
  FStorageHelper:= PathToClass(Path);
  try
    DeleteSection(FStorageHelper);
  finally
    CloseStorage;
  end
end;

procedure TRegIniStorage.DeleteStorage(const Path: string);
begin
  FStorageHelper:= PathToClass(Path);
  try
    Delete(FStorageHelper);
  finally
    CloseStorage;
  end
end;

function TRegIniStorage.PathToClass(const Path: string): TCustomIniFile;
const
  Keys: array [HKEY_CURRENT_USER..HKEY_LOCAL_MACHINE] of string = ('HKEY_CURRENT_USER', 'HKEY_LOCAL_MACHINE');
var
  I, Key: DWORD;
  P: Integer;
  S: string;
begin
  Key:= 0;
  P:= AnsiPos('\', Path);
  S:= Copy(Path, 1, P - 1);
  for I:= Low(Keys) to High(Keys) do
    if SameText(Keys[I], S) then
    begin
      Key:= I;
      Break;
    end;
  if Key <> 0 then
  begin
    S:= Copy(Path, P, Length(Path) - P + 1);
    Result:= TRegistryIniFile.Create(S);
    TRegistryIniFile(Result).RegIniFile.RootKey:= Key;
  end
  else begin
    Result:= TIniFile.Create(Path);
  end
end;

procedure TRegIniStorage.CloseStorage;
begin
  if FStorageHelper = nil then
    Exit;
  try
    if FStorageHelper is TRegistryIniFile then
      TRegistryIniFile(FStorageHelper).RegIniFile.CloseKey
    else
      TIniFile(FStorageHelper).UpdateFile;
  finally
    FreeAndNil(FStorageHelper);
  end;
end;

procedure TRegIniStorage.ResolveOwnerName(List: TStrings; ToName: Boolean);
var
  I: Integer;
  Match1, Match2: string;
begin
  List.SaveToFile('c:\temp\resolve.txt');
  if ToName then
  begin
    Match1:= ' = Owner.';
    Match2:= ' = ' + Owner.Name + '.';
  end
  else begin
    Match1:= ' = ' + Owner.Name + '.';
    Match2:= ' = Owner.';
  end;
  for I:= 1 to List.Count - 1 do
  begin
    List[I]:= StringReplace(List[I], Match1, Match2, [rfReplaceAll, rfIgnoreCase]);
  end;
end;

end.
