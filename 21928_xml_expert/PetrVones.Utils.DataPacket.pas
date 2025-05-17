unit PetrVones.Utils.DataPacket;

interface

uses
  System.Data, System.IO, System.Text, System.Xml;

type
  EDataPacketError = class(System.Data.DataException);

procedure FillDataSet(DS: DataSet; DatPacketReader: TextReader); overload;
procedure FillDataSet(DS: DataSet; DatPacket: string); overload;

implementation

resourcestring
  RsElelementNotFound = 'Expected element {0} not found';
  RsInvalidFieldType  = 'Invalid field type "{0}"';

type
  TDataPacketElement = (peDataPacket, peMetadata, peFields, peField, peRowData);
  TDataPacketConversion = (coNone, coDate, coDateTime, coTime, coBoolean);

const
  DataPacketElementNames: array [TDataPacketElement] of string = ('DATAPACKET', 'METADATA', 'FIELDS', 'FIELD', 'ROWDATA');

type
  TDataPacketDataColumn = class(DataColumn)
  private
    FDataPacketConversion: TDataPacketConversion;
  protected
    property DataPacketConversion: TDataPacketConversion read FDataPacketConversion write FDataPacketConversion;
  end;

type
  TDataPacketXmlReader = class(XmlTextReader)
  private
    FCurrentTable: DataTable;
    FDescription: DataSet;
  public
    function get_Value: string; override;
    function Read: Boolean; override;
    property Description: DataSet read FDescription write FDescription;
  end;

{ TDataPacketXmlReader }

function TDataPacketXmlReader.get_Value: string;
var
  Column: TDataPacketDataColumn;
begin
  Result := inherited get_Value;
  case NodeType of
    XmlNodeType.Attribute:
      begin
        if Assigned(FCurrentTable) then
          Column := TDataPacketDataColumn(FCurrentTable.Columns[Name])
        else
          Column := nil;  
        if Assigned(Column) and (Column.ColumnMapping <> MappingType.Hidden) then
          case Column.DataPacketConversion of
            coTime:
              Insert('.', Result, 9);
            coDate, coDateTime:
              if (Result.Length = 20) and (Result[9] = 'T') and (Result[12] = ':') then
              begin
                Insert('-', Result, 5);
                Insert('-', Result, 8);
                Delete(Result, 20, 3);
              end
              else
              if Result.Length = 8 then
              begin
                Insert('-', Result, 5);
                Insert('-', Result, 8);
              end;
            coBoolean:
              Result := Result.ToLower;
         end;
      end;
   end;
end;

function TDataPacketXmlReader.Read: Boolean;
begin
  Result := inherited Read;
  if Result and (NodeType = XmlNodeType.Element) and HasAttributes then
    FCurrentTable := Description.Tables[Name];
end;

{ FillDataSet }

procedure LoadDataPacket(DS: DataSet; Reader: XmlReader);
const
  RootTableName = 'ROW';
var
  ElementName: string;
  ExpectingElement: TDataPacketElement;

  procedure CheckElementName(Element: TDataPacketElement);
  begin
    if DataPacketElementNames[Element] <> ElementName then
      raise EDataPacketError.Create(System.String.Format(RsElelementNotFound, [ElementName]));
  end;

  function CreateIdColumn(Table, NameTable: DataTable): DataColumn;
  var
    IdColumnName: string;
  begin
    IdColumnName := NameTable.TableName + '_IdNestedRel';
    Result := Table.Columns[IdColumnName];
    if not Assigned(Result) then
    begin
      Result := Table.Columns.Add(IdColumnName);
      Result.DataType := typeof(System.Int32);
      Result.ColumnMapping := MappingType.Hidden;
    end;
  end;

  procedure InvalidFieldType(FieldType: string);
  begin
    raise EDataPacketError.Create(System.String.Format(RsInvalidFieldType, [FieldType]));
  end;

  procedure ReadFields(CurrentTable: DataTable);
  type
    TDataColumn_Array = array of DataColumn;
  var
    AttrName, AttrNameValue, FieldTypeValue, SubTypeValue: string;
    Column: TDataPacketDataColumn;
    ParentIdColumn, ChildIdColumn: DataColumn;
    NestedTable: DataTable;
    NestedRelation: DataRelation;
    RelationName: string;
  begin
    while Reader.Read do
      case Reader.NodeType of
        XmlNodeType.Element:
          if Reader.Name = DataPacketElementNames[peField] then
          begin
            AttrName := '';
            FieldTypeValue := '';
            SubTypeValue := '';
            while Reader.MoveToNextAttribute do
            begin
              AttrName := Reader.Name.ToLower;
              if AttrName = 'attrname' then
                AttrNameValue := Reader.Value
              else
              if AttrName = 'fieldtype' then
                FieldTypeValue := Reader.Value
              else
              if AttrName = 'subtype' then
                SubTypeValue := Reader.Value;
            end;
            if FieldTypeValue = 'nested' then
            begin
              ParentIdColumn := CreateIdColumn(CurrentTable, CurrentTable);
              ParentIdColumn.AllowDBNull := False;
              ParentIdColumn.AutoIncrement := True;
              ParentIdColumn.Unique := True;
              CurrentTable.PrimaryKey := TDataColumn_Array.Create(ParentIdColumn);
              NestedTable := DS.Tables.Add(CurrentTable.TableName + AttrNameValue);
              ChildIdColumn := CreateIdColumn(NestedTable, CurrentTable);
              RelationName := CurrentTable.TableName + '_' + NestedTable.TableName;
              NestedRelation := DS.Relations.Add(RelationName, ParentIdColumn, ChildIdColumn, True);
              NestedRelation.Nested := True;
              ReadFields(NestedTable);
            end
            else
            begin
              Column := TDataPacketDataColumn.Create;
              Column.DataPacketConversion := coNone;
              Column.ColumnName := AttrNameValue;
              Column.ColumnMapping := MappingType.Attribute;
              Column.Caption := AttrNameValue;
              if FieldTypeValue = 'fixed' then
                Column.DataType := typeof(System.Decimal)
              else
              if FieldTypeValue = 'boolean' then
              begin
                Column.DataType := typeof(System.Boolean);
                Column.DataPacketConversion := coBoolean;
              end
              else
              if FieldTypeValue = 'r8' then
                Column.DataType := typeof(System.Double)
              else
              if FieldTypeValue = 'date' then
              begin
                Column.DataType := typeof(System.DateTime);
                Column.DataPacketConversion := coDate;
              end
              else
              if FieldTypeValue = 'dateTime' then
              begin
                Column.DataType := typeof(System.DateTime);
                Column.DataPacketConversion := coDateTime;
              end
              else
              if FieldTypeValue = 'i4' then
                Column.DataType := typeof(System.Int32)
              else
              if FieldTypeValue = 'i8' then
                Column.DataType := typeof(System.Int64)
              else
              if FieldTypeValue = 'i2' then
                Column.DataType := typeof(System.Int16)
              else
              if FieldTypeValue = 'string' then
                Column.DataType := typeof(System.String)
              else
              if FieldTypeValue = 'time' then
              begin
                Column.DataType := typeof(System.DateTime);
                Column.DataPacketConversion := coTime;
              end
              else
              if FieldTypeValue = 'string.uni' then
                Column.DataType := typeof(System.String)
              else
              if FieldTypeValue = 'ui2' then
                Column.DataType := typeof(System.UInt16)
              else
              if FieldTypeValue = 'ui4' then
                Column.DataType := typeof(System.UInt32)
              else
              if FieldTypeValue = 'bin.hex' then
              begin
                if SubTypeValue = 'Text' then
                  Column.DataType := typeof(System.String)
                else
                if (SubTypeValue = 'Graphics') or (SubTypeValue = 'TypedBinary') then
                  Column.DataType := typeof(TBytes)
                else
                  InvalidFieldType(System.String.Format('{0}/{1}', [FieldTypeValue, SubTypeValue]));
              end
              else
                InvalidFieldType(FieldTypeValue);
              CurrentTable.Columns.Add(Column);
            end;
          end;
        XmlNodeType.EndElement:
          if Reader.Name = DataPacketElementNames[peFields] then
            Break;
      end;
  end;

  procedure ReadData;
  begin
    DS.ReadXml(Reader, XmlReadMode.IgnoreSchema);
  end;

begin
  ExpectingElement := peDataPacket;
  while Reader.Read do
    case Reader.NodeType of
      XmlNodeType.Element:
        begin
          ElementName := Reader.Name;
          case ExpectingElement of
            peDataPacket:
              begin
                CheckElementName(peDataPacket);
                ExpectingElement := peMetadata;
              end;
            peMetadata:
              begin
                CheckElementName(peMetadata);
                ExpectingElement := peFields;
              end;
            peFields:
              begin
                ReadFields(DS.Tables.Add(RootTableName));
                ExpectingElement := peRowData;
                Reader.Skip;
              end;
            peRowData:
              if ElementName = DataPacketElementNames[peRowData] then
              begin
                ReadData;
                Break;
              end;  
          end;
        end;
    end;
end;

procedure FillDataSet(DS: DataSet; DatPacketReader: TextReader);
var
  Reader: TDataPacketXmlReader;
begin
  DS.Reset;
  Reader := TDataPacketXmlReader.Create(DatPacketReader);
  try
    Reader.Description := DS;
    LoadDataPacket(DS, Reader);
  finally
    Reader.Close;
  end;  
end;

procedure FillDataSet(DS: DataSet; DatPacket: string);
var
  Reader: StringReader;
begin
  Reader := StringReader.Create(DatPacket);
  try
    FillDataSet(DS, Reader);
  finally
    Reader.Close;
  end;
end;

end.
