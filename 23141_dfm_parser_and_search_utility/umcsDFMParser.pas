{-----------------------------------------------------------------------------
The Initial Developer of the Original Code is Charles McAllister charles@avimark.net
Portions created by Charles McAllister are Copyright (C) 2005 Charles McAllister
All Rights Reserved.

Disclaimer:
This code and information are provided "As is" without warranty of any kind, either expressed or
implied, including but not limited to the implied warranties of merchantability and/or fitness
for a particular purpose, or non-infringement.

Contributor(s):
  Wolfgang Prinzjakowitsch

History
  1/31/2011
    added support for Delphi 2007
    fixed access violation bug with vaBinary
    fixed classname problem where vaCollection was added to the class name redundantly

  2/28/2011
    TmcsDFMPersistent/TmcsDFMPersistentList now form a tree pattern
    TmcsDFMParser.RootPersistent prepresents the root node of the tree
    use TmcsDFMPersistent.ChildPersistentList to access child nodes recursively

Description:
  A set of classes that represent a parsed DFM file.
  Use TmcsDFMPersistentListParser.BuildPersistentList to parse a binary or text DFM file into
  a tree structure, TmcsDFMPersistentListParser.PersistentList

Known Issues:
  See various TODO's below.
  If you find a bug, you may submit your fix to charles@avimark.net

}

unit umcsDFMParser;

interface

uses
  SysUtils,
  Classes,
  Dialogs,
  TypInfo,
  RTLConsts,
  Contnrs;

type
  TmcsDFMProperty = class(TObject)
  private
    FPropName: string;
    FValueType: TValueType;
  public
    constructor Create(APropName: string; AValueType: TValueType); virtual;
    property PropName: string read FPropName;
    property ValueType: TValueType read FValueType;
    function AsString: string; virtual; abstract;
    procedure WriteToDFM(Writer: TWriter);
  end;

  TmcsDFMPropertyClass = class of TmcsDFMProperty;

  TmcsDFMIntegerProperty = class(TmcsDFMProperty)
  private
    //vaInt8, vaInt16, vaInt32
    FInteger: Integer;
  protected
    procedure SetInteger(AInteger: Integer);
  public
    property _Integer: Integer read FInteger;
    function AsString: string; override;
  end;

  TmcsDFMListProperty = class(TmcsDFMProperty)
  private
    //vaList
  public
    function AsString: string; override;
  end;

  TmcsDFMFloatProperty = class(TmcsDFMProperty)
  private
    //vaExtended
    FFloat: Extended;
  protected
    procedure SetFloat(AFloat: Extended);
  public
    property Float: Extended read FFloat;
    function AsString: string; override;
  end;

  TmcsDFMSingleProperty = class(TmcsDFMProperty)
  private
    //vaSingle
    FSingle: Single;
  protected
    procedure SetSingle(ASingle: Single);
  public
    property _Single: Single read FSingle;
    function AsString: string; override;
  end;

  TmcsDFMCurrencyProperty = class(TmcsDFMProperty)
  private
    //vaCurrency
    FCurrency: Currency;
  protected
    procedure SetCurrency(ACurrency: Currency);
  public
    property _Currency: Currency read FCurrency;
    function AsString: string; override;
  end;

  TmcsDFMDateProperty = class(TmcsDFMProperty)
  private
    //vaDate
    FDateTime: TDateTime;
  protected
    procedure SetDateTime(ADateTime: TDateTime);
  public
    property _DateTime: TDateTime read FDateTime;
    function AsString: string; override;
  end;

  TmcsDFMWideStringProperty = class(TmcsDFMProperty)
  private
    //vaWString, vaUTF8String
    FWideString: WideString;
  protected
    procedure SetWideString(AWideString: WideString);
  public
    property _WideString: WideString read FWideString;
    function AsString: string; override;
  end;

  TmcsDFMStringProperty = class(TmcsDFMProperty)
  private
    //vaString, vaLString
    FString: string;
  protected
    procedure SetString(AString: string);
  public
    property _String: string read FString;
    function AsString: string; override;
  end;

  TmcsDFMBooleanProperty = class(TmcsDFMProperty)
  private
    //vaFalse, vaTrue
    FBoolean: Boolean;
  protected
    procedure SetBoolean(ABoolean: Boolean);
  public
    property _Boolean: Boolean read FBoolean;
    function AsString: string; override;
  end;

  TmcsDFMNullProperty = class(TmcsDFMProperty)
  private
    //vaIdent, vaNil, vaNull
  protected
  public
    function AsString: string; override;
  end;

  TmcsDFMBinaryProperty = class(TmcsDFMProperty)
  private
    //vaBinary
    FMemoryStream: TMemoryStream;
  protected
    procedure SetBinaryData(const ABuffer; ASize: Integer);
  public
    constructor Create(APropName: String; AValueType: TValueType); override;
    destructor Destroy; override;
    property MemoryStream: TMemoryStream read FMemoryStream;
    function AsString: string; override;
  end;

  TmcsDFMSetProperty = class(TmcsDFMProperty)
  private
    //vaSet
    FSetItems: TStringList;
  protected
    procedure SetSetItem(ASetItem: string);
  public
    constructor Create(APropName: String; AValueType: TValueType); override;
    destructor Destroy; override;
    property SetItems: TStringList read FSetItems;
    function AsString: string; override;
  end;

  TmcsDFMCollectionProperty = class(TmcsDFMProperty)
  private
    //vaCollection
  protected
  public
    function AsString: string; override;
  end;

  TmcsDFMInt64Property = class(TmcsDFMProperty)
  private
    //vaInt64
    FInt64: Int64;
  protected
    procedure SetInt64(AInt64: Int64);
  public
    property _Int64: Int64 read FInt64;
    function AsString: string; override;
  end;

  {$IFDEF VER180}
  TmcsDFMDoubleProperty = class(TmcsDFMProperty)
  private
    //vaDouble
    FDouble: Double;
  protected
    procedure SetDouble(ADouble: Double);
  public
    property _Double: Double read FDouble;
    function AsString: string; override;
  end;
  {$ENDIF}

const
  DFMPropertyClassArray: array[TValueType] of TmcsDFMPropertyClass = (
    TmcsDFMNullProperty, //vaNull,
    TmcsDFMListProperty, //vaList,
    TmcsDFMIntegerProperty, //vaInt8,
    TmcsDFMIntegerProperty, //vaInt16,
    TmcsDFMIntegerProperty, //vaInt32,
    TmcsDFMFloatProperty, //vaExtended,
    TmcsDFMStringProperty, //vaString,
    TmcsDFMNullProperty, //vaIdent,
    TmcsDFMBooleanProperty, //vaFalse,
    TmcsDFMBooleanProperty, //vaTrue,
    TmcsDFMBinaryProperty, //vaBinary,
    TmcsDFMSetProperty, //vaSet,
    TmcsDFMStringProperty, //vaLString,
    TmcsDFMNullProperty, //vaNil,
    TmcsDFMCollectionProperty, //vaCollection,
    TmcsDFMSingleProperty, //vaSingle,
    TmcsDFMCurrencyProperty, //vaCurrency,
    TmcsDFMDateProperty, //vaDate,
    TmcsDFMWideStringProperty, //vaWString,
    TmcsDFMInt64Property, //vaInt64,
    TmcsDFMWideStringProperty //vaUTF8String
    {$IFDEF VER180}
    , TmcsDFMDoubleProperty //vaDouble
    {$ENDIF}
    );

type
  TmcsDFMPropertyList = class(TObject)
  private
    FObjectList: TObjectList;
    function GetProperty(Index: Integer): TmcsDFMProperty;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    procedure Clear;
    function Add(ADFMProperty: TmcsDFMProperty): Integer;
    property Properties[Index: Integer]: TmcsDFMProperty read GetProperty; default;
  end;

  TmcsDFMPersistentList = class;

  TmcsDFMPersistent = class(TObject)
  private
    FParent: TmcsDFMPersistent;
    FPropertyList: TmcsDFMPropertyList;
    FPersistentClassName: string;
    FPersistentName: string;
    FFlags: TFilerFlags;
    FChildPersistentList: TmcsDFMPersistentList;
  public
    constructor Create(AParent: TmcsDFMPersistent; APersistentName,
      APersistentClassName: string; AFlags: TFilerFlags);
    destructor Destroy; override;
    property PropertyList: TmcsDFMPropertyList read FPropertyList;
    property PersistentName: string read FPersistentName;
    property PersistentClassName: string read FPersistentClassName;
    property Flags: TFilerFlags read FFlags;
    property Parent: TmcsDFMPersistent read FParent;
    property ChildPersistentList: TmcsDFMPersistentList read FChildPersistentList;
  end;

  TmcsDFMPersistentList = class(TObject)
  private
    FObjectList: TObjectList;
    function GetPersistent(Index: Integer): TmcsDFMPersistent;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    procedure Clear;
    function Add(ADFMPersistent: TmcsDFMPersistent): Integer;
    property Persistents[Index: Integer]: TmcsDFMPersistent read GetPersistent; default;
  end;

  TmcsDFMParser = class(TObject)
  private
    FRootPersistent: TmcsDFMPersistent;
  protected
    procedure DoBuild(AStream: TStream); virtual;
    procedure DoNewPersistent(AReader: TReader; APersistentName, APersistentClassName: string;
      AFlags: TFilerFlags; const AParent: TmcsDFMPersistent;
      out ADFMPersistent: TmcsDFMPersistent); virtual;
    procedure DoNewProperty(AReader: TReader; APropName: string; AValueType: TValueType;
      const ADFMPersistent: TmcsDFMPersistent); virtual;
    procedure DoEndPropertyList(AReader: TReader; const ADFMPersistent: TmcsDFMPersistent); virtual;
  public
    destructor Destroy; override;
    class procedure Build(AStream: TStream); overload;
    class procedure Build(AFileName: string); overload;
    class function IsBinary(AStream: TStream): Boolean; overload;
    class function IsBinary(AFileName: string): Boolean; overload;
    procedure BuildPersistentList(AStream: TStream); overload;
    procedure BuildPersistentList(AFileName: string); overload;
    property RootPersistent: TmcsDFMPersistent read FRootPersistent;
  end;

implementation

type
  THackWriter = class(TWriter);

{ TmcsDFMProperty }

constructor TmcsDFMProperty.Create(APropName: string; AValueType: TValueType);
begin
  inherited Create;
  FPropName := APropName;
  FValueType := AValueType;
end;

procedure TmcsDFMProperty.WriteToDFM(Writer: TWriter);
begin

end;

{ TmcsDFMIntegerProperty }

function TmcsDFMIntegerProperty.AsString: string;
begin
  Result := IntToStr(FInteger);
end;

procedure TmcsDFMIntegerProperty.SetInteger(AInteger: Integer);
begin
  FInteger := AInteger;
end;

{ TmcsDFMListProperty }

function TmcsDFMListProperty.AsString: string;
begin
  Result := '';
end;

{ TmcsDFMFloatProperty }

function TmcsDFMFloatProperty.AsString: string;
begin
  Result := FloatToStrF(FFloat, ffFixed, 16, 18);
end;

procedure TmcsDFMFloatProperty.SetFloat(AFloat: Extended);
begin
  FFloat := AFloat;
end;

{ TmcsDFMSingleProperty }

function TmcsDFMSingleProperty.AsString: string;
begin
  Result := FloatToStr(FSingle) + 's';
end;

procedure TmcsDFMSingleProperty.SetSingle(ASingle: Single);
begin
  FSingle := ASingle;
end;

{ TmcsDFMCurrencyProperty }

function TmcsDFMCurrencyProperty.AsString: string;
begin
  Result := FloatToStr(FCurrency * 10000) + 'c';
end;

procedure TmcsDFMCurrencyProperty.SetCurrency(ACurrency: Currency);
begin
  FCurrency := ACurrency;
end;

{ TmcsDFMDateProperty }

function TmcsDFMDateProperty.AsString: string;
begin
  Result := FloatToStr(FDateTime) + 'd';
end;

procedure TmcsDFMDateProperty.SetDateTime(ADateTime: TDateTime);
begin
  FDateTime := ADateTime;
end;

{ TmcsDFMWideStringProperty }

function TmcsDFMWideStringProperty.AsString: string;
begin
  Result := FWideString;
end;

procedure TmcsDFMWideStringProperty.SetWideString(AWideString: WideString);
begin
  FWideString := AWideString;
end;

{ TmcsDFMStringProperty }

function TmcsDFMStringProperty.AsString: string;
begin
  Result := FString;
end;

procedure TmcsDFMStringProperty.SetString(AString: string);
begin
  FString := AString;
end;

{ TmcsDFMBooleanProperty }

function TmcsDFMBooleanProperty.AsString: string;
begin
  Result := BoolToStr(FBoolean, True);
end;

procedure TmcsDFMBooleanProperty.SetBoolean(ABoolean: Boolean);
begin
  FBoolean := ABoolean;
end;

{ TmcsDFMNullProperty }

function TmcsDFMNullProperty.AsString: string;
begin
  Result := '';
end;

{ TmcsDFMBinaryProperty }

function TmcsDFMBinaryProperty.AsString: string;
begin
  Result := ''; //TODO:
end;

constructor TmcsDFMBinaryProperty.Create(APropName: String;
  AValueType: TValueType);
begin
  inherited;
  FMemoryStream := TMemoryStream.Create;
end;

destructor TmcsDFMBinaryProperty.Destroy;
begin
  FMemoryStream.Free;
  inherited;
end;

procedure TmcsDFMBinaryProperty.SetBinaryData(const ABuffer; ASize: Integer);
begin
  FMemoryStream.Size := ASize;
  FMemoryStream.Write(ABuffer, ASize);
end;

{ TmcsDFMSetProperty }

function TmcsDFMSetProperty.AsString: string;
begin
  Result := FSetItems.Text;
end;

constructor TmcsDFMSetProperty.Create(APropName: String;
  AValueType: TValueType);
begin
  inherited;
  FSetItems := TStringList.Create;
end;

destructor TmcsDFMSetProperty.Destroy;
begin
  FSetItems.Free;
  inherited;
end;

procedure TmcsDFMSetProperty.SetSetItem(ASetItem: string);
begin
  FSetItems.Add(ASetItem);
end;

{ TmcsDFMCollectionProperty }

function TmcsDFMCollectionProperty.AsString: string;
begin
  Result := '';
end;

{ TmcsDFMInt64Property }

function TmcsDFMInt64Property.AsString: string;
begin
  Result := IntToStr(FInt64);
end;

procedure TmcsDFMInt64Property.SetInt64(AInt64: Int64);
begin
  FInt64 := AInt64;
end;

{ TmcsDFMDoubleProperty }

{$IFDEF VER180}
function TmcsDFMDoubleProperty.AsString: string;
begin
  Result := FloatToStrF(FDouble, ffFixed, 16, 18);
end;

procedure TmcsDFMDoubleProperty.SetDouble(ADouble: Double);
begin
  FDouble := ADouble;
end;
{$ENDIF}

{ TmcsDFMPropertyList }

constructor TmcsDFMPropertyList.Create;
begin
  inherited Create;
  FObjectList := TObjectList.Create(True);
end;

destructor TmcsDFMPropertyList.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TmcsDFMPropertyList.Add(ADFMProperty: TmcsDFMProperty): Integer;
begin
  Result := FObjectList.Add(ADFMProperty);
end;

function TmcsDFMPropertyList.Count: Integer;
begin
  Result := FObjectList.Count;
end;

function TmcsDFMPropertyList.GetProperty(Index: Integer): TmcsDFMProperty;
begin
  Result := FObjectList[Index] as TmcsDFMProperty;
end;

procedure TmcsDFMPropertyList.Clear;
begin
  FObjectList.Clear;
end;

{ TmcsDFMPersistent }

constructor TmcsDFMPersistent.Create(AParent: TmcsDFMPersistent;
  APersistentName, APersistentClassName: string; AFlags: TFilerFlags);
begin
  inherited Create;
  FParent := AParent;
  FPersistentClassName := APersistentClassName;
  FPersistentName := APersistentName;
  FFlags := AFlags;
  FPropertyList := TmcsDFMPropertyList.Create;
  FChildPersistentList := TmcsDFMPersistentList.Create;
end;

destructor TmcsDFMPersistent.Destroy;
begin
  FPropertyList.Free;
  FChildPersistentList.Free;
  inherited;
end;

{ TmcsDFMPersistentList }

constructor TmcsDFMPersistentList.Create;
begin
  inherited Create;
  FObjectList := TObjectList.Create(True);
end;

destructor TmcsDFMPersistentList.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TmcsDFMPersistentList.Add(ADFMPersistent: TmcsDFMPersistent): Integer;
begin
  Result := FObjectList.Add(ADFMPersistent);
end;

function TmcsDFMPersistentList.Count: Integer;
begin
  Result := FObjectList.Count;
end;

function TmcsDFMPersistentList.GetPersistent(Index: Integer): TmcsDFMPersistent;
begin
  Result := FObjectList[Index] as TmcsDFMPersistent;
end;

procedure TmcsDFMPersistentList.Clear;
begin
  FObjectList.Clear;
end;

{ TmcsDFMParser }

destructor TmcsDFMParser.Destroy;
begin
  FRootPersistent.Free;
  inherited;
end;

procedure TmcsDFMParser.DoBuild(AStream: TStream);
var
  SaveSeparator: Char;
  AReader: TReader;
  APersistentName: string;
  APersistentClassName: string;
  AFlags: TFilerFlags;

  procedure BuildValue(APersistentName, APersistentClassName: string; AFlags: TFilerFlags;
    APropName: string; const AParent: TmcsDFMPersistent);
  var
    AValueType: TValueType;
    ADFMPersistent: TmcsDFMPersistent;
  begin
    AValueType := AReader.NextValue;
    if not (AValueType in [Low(TValueType)..High(TValueType)]) then
      raise EReadError.CreateResFmt(@sPropertyException, [APersistentName, DotSep, APropName,
        IntToStr(Ord(AReader.NextValue))]);
    DoNewProperty(AReader, APropName, AValueType, AParent);
    case AValueType of
      vaList: begin
        AReader.ReadValue;
        while not AReader.EndOfList do
          BuildValue(Format('%s.%s', [APersistentName, 'vaList']), Format('%s.%s',
            [APersistentClassName, 'vaList']), [], APropName, AParent);
        AReader.ReadListEnd;
        DoEndPropertyList(AReader, AParent);
      end;
      vaCollection:
      begin
        APersistentName := Format('%s.%s', [APersistentName, 'vaCollection']);
        APersistentClassName := Format('%s.%s', [APersistentClassName, 'vaCollection']);
        AReader.ReadValue;
        while not AReader.EndOfList do
        begin
          if AReader.NextValue in [vaInt8, vaInt16, vaInt32] then
            AReader.ReadInteger; //TODO: number not exposed
          DoNewPersistent(AReader, APersistentName, APersistentClassName, [],
            AParent, ADFMPersistent);
          AReader.CheckValue(vaList);
          while not AReader.EndOfList do
            BuildValue(APersistentName, APersistentClassName, [], AReader.ReadStr, ADFMPersistent);
          AReader.ReadListEnd;
          DoEndPropertyList(AReader, ADFMPersistent);
        end;
        AReader.ReadListEnd;
      end;
      else
    end;
  end;

  procedure BuildObject(const AParent: TmcsDFMPersistent = nil);

    procedure BuildHeader(AParent: TmcsDFMPersistent; out ANewParent: TmcsDFMPersistent);
    var
      APosition: Integer;
    begin
      AReader.ReadPrefix(AFlags, APosition);
      APersistentClassName := AReader.ReadStr;
      APersistentName := AReader.ReadStr;
      DoNewPersistent(AReader, APersistentName, APersistentClassName, AFlags, AParent,
        ANewParent);
      if APersistentName = '' then
        APersistentName := APersistentClassName;
    end;

  var
    ANewParent: TmcsDFMPersistent;
  begin
    BuildHeader(AParent, ANewParent);
    while not AReader.EndOfList do
      BuildValue(APersistentName, APersistentClassName, AFlags, AReader.ReadStr, ANewParent);
    AReader.ReadListEnd;
    while not AReader.EndOfList do
      BuildObject(ANewParent);
    AReader.ReadListEnd;
    DoEndPropertyList(AReader, ANewParent);
  end;

begin
  FreeAndNil(FRootPersistent);
  AReader := TReader.Create(AStream, 4096);
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    AReader.ReadSignature;
    BuildObject(nil);
  finally
    DecimalSeparator := SaveSeparator;
    AReader.Free;
  end;
end;

class function TmcsDFMParser.IsBinary(AStream: TStream): Boolean;
var
  SavePosition: Integer;
  AHeader: array[0..2] of Byte;
begin
  SavePosition := AStream.Position;
  try
    AStream.Position := 0;
    if AStream.Read(AHeader, SizeOf(AHeader)) = SizeOf(AHeader) then
      Result := (AHeader[0] = $FF) and (AHeader[1] = $0A) and (AHeader[2] = $00)
    else begin
      Result := False;
      raise Exception.Create('Unable to read from stream.');
    end;
  finally
    AStream.Position := SavePosition;
  end;
end;

class function TmcsDFMParser.IsBinary(AFileName: string): Boolean;
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := IsBinary(AFileStream);
  finally
    AFileStream.Free;
  end;
end;

class procedure TmcsDFMParser.Build(AStream: TStream);
var
  ADFMParser: TmcsDFMParser;
begin
  ADFMParser := Self.Create;
  try
    ADFMParser.DoBuild(AStream);
  finally
    ADFMParser.Free;
  end;
end;

class procedure TmcsDFMParser.Build(AFileName: string);
var
  AMemoryStream: TMemoryStream;
  AFileStream: TFileStream;
  ABinaryStream: TStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  ABinaryStream := AFileStream;
  try
    if not IsBinary(AFileStream) then
    begin
      ABinaryStream := TMemoryStream.Create;
      ObjectTextToBinary(AFileStream, ABinaryStream);
    end else
    begin
      AMemoryStream := TMemoryStream.Create;
      ObjectResourceToText(AFileStream, AMemoryStream);
      ABinaryStream := TMemoryStream.Create;
      AMemoryStream.Position := 0;
      ObjectTextToBinary(AMemoryStream, ABinaryStream);
    end;
    ABinaryStream.Position := 0;
    Build(ABinaryStream);
  finally
    if ABinaryStream <> AFileStream then
      ABinaryStream.Free;
    AFileStream.Free;
  end;
end;

procedure TmcsDFMParser.DoNewPersistent(AReader: TReader;
  APersistentName, APersistentClassName: string; AFlags: TFilerFlags;
  const AParent: TmcsDFMPersistent; out ADFMPersistent: TmcsDFMPersistent);
begin
  inherited;
  ADFMPersistent := TmcsDFMPersistent.Create(AParent, APersistentName, APersistentClassName, AFlags);
  if Assigned(AParent) then
    AParent.ChildPersistentList.Add(ADFMPersistent)
  else
  begin
    Assert(not Assigned(FRootPersistent));
    FRootPersistent := ADFMPersistent;
  end;
end;

procedure TmcsDFMParser.DoNewProperty(AReader: TReader; APropName: string;
  AValueType: TValueType; const ADFMPersistent: TmcsDFMPersistent);
var
  ADFMProperty: TmcsDFMProperty;
  ASetItem: string;
  ABinaryCount: Integer;
  ABinaryBuffer: Pointer;
begin
  inherited;
  ADFMProperty := DFMPropertyClassArray[AValueType].Create(APropName, AValueType);
  ADFMPersistent.PropertyList.Add(ADFMProperty);
  case AValueType of
    vaInt8, vaInt16, vaInt32:
      (ADFMProperty as TmcsDFMIntegerProperty).SetInteger(AReader.ReadInteger);
    vaExtended:
      (ADFMProperty as TmcsDFMFloatProperty).SetFloat(AReader.ReadFloat);
    vaSingle:
      (ADFMProperty as TmcsDFMSingleProperty).SetSingle(AReader.ReadSingle);
    vaCurrency:
      (ADFMProperty as TmcsDFMCurrencyProperty).SetCurrency(AReader.ReadCurrency);
    vaDate:
      (ADFMProperty as TmcsDFMDateProperty).SetDateTime(AReader.ReadDate);
    vaWString, vaUTF8String:
      (ADFMProperty as TmcsDFMWideStringProperty).SetWideString(AReader.ReadWideString);
    vaString, vaLString:
      (ADFMProperty as TmcsDFMStringProperty).SetString(AReader.ReadString);
    vaFalse, vaTrue: begin
      AReader.ReadIdent;
      case AValueType of
        vaTrue: (ADFMProperty as TmcsDFMBooleanProperty).SetBoolean(True);
        vaFalse: (ADFMProperty as TmcsDFMBooleanProperty).SetBoolean(False);
        else
          Assert(False);
      end;
    end;
    vaIdent, vaNil, vaNull:
      AReader.ReadIdent;
    vaSet: begin
      AReader.ReadValue;
      repeat
        ASetItem := AReader.ReadStr;
        if ASetItem = '' then
          Break
        else
          (ADFMProperty as TmcsDFMSetProperty).SetSetItem(ASetItem);
      until False;
    end;
    vaInt64:
      (ADFMProperty as TmcsDFMInt64Property).SetInt64(AReader.ReadInt64);
    vaBinary: begin
      AReader.ReadValue;
      AReader.Read(ABinaryCount, SizeOf(ABinaryCount));
      GetMem(ABinaryBuffer, ABinaryCount);
      AReader.Read(ABinaryBuffer^, ABinaryCount);
      (ADFMProperty as TmcsDFMBinaryProperty).SetBinaryData(ABinaryBuffer^, ABinaryCount);
      FreeMem(ABinaryBuffer, ABinaryCount);
    end;
    vaCollection: ;
    vaList: ;
    {$IFDEF VER180}
    vaDouble: 
      (ADFMProperty as TmcsDFMDoubleProperty).SetDouble(AReader.ReadDouble);
    {$ENDIF}
    else
      Assert(False);
  end;
end;

procedure TmcsDFMParser.DoEndPropertyList(AReader: TReader; const ADFMPersistent: TmcsDFMPersistent);
begin

end;

procedure TmcsDFMParser.BuildPersistentList(AStream: TStream);
begin
  DoBuild(AStream);
end;

procedure TmcsDFMParser.BuildPersistentList(AFileName: string);
var
  AMemoryStream: TMemoryStream;
  AFileStream: TFileStream;
  ABinaryStream: TStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  ABinaryStream := nil;
  try
    if not IsBinary(AFileStream) then
    begin
      ABinaryStream := TMemoryStream.Create;
      ObjectTextToBinary(AFileStream, ABinaryStream);
    end else
    begin
      AMemoryStream := TMemoryStream.Create;
      try
        ObjectResourceToText(AFileStream, AMemoryStream);
        ABinaryStream := TMemoryStream.Create;
        AMemoryStream.Position := 0;
        ObjectTextToBinary(AMemoryStream, ABinaryStream);
      finally
        AMemoryStream.Free;
      end;
    end;
    ABinaryStream.Position := 0;
    BuildPersistentList(ABinaryStream);
  finally
    ABinaryStream.Free;
    AFileStream.Free;
  end;
end;

end.

