unit BDSConfig;
//------------------------------------------------------------------------------
//  File name:      BDSConfig.pas
//  Last updated:   11/28/03
//  Author:         Sergey Mishkovskiy
//  Company:        USysWare, Inc.
//  Contact info:   usysware@comcast.net
//
//  Compatibility:  Borland Delphi for .NET
//
//  Description:    Consists of classes that hold .xml configuration data.
//                  TConfigAddin is a driver class, which is also responsible
//                  for parsing the xml file(s).
//------------------------------------------------------------------------------

interface

uses
  System.XML;  // XML

type
  TConfigReference = class
  private
    FAssembly: string;
  public
    property Assembly: string read FAssembly;
  end;

  TConfigDialogGroup = class
  private
    FName: string;
    FCaption: string;
    FEnabled: Boolean;
  public
    property Name: string read FName;
    property Caption: string read FCaption;
    property Enabled: Boolean read FEnabled;
  end;

  TConfigDialog = class
  private
    FTitle: string;
    FEnabled: Boolean;
    FGroups: array of TConfigDialogGroup;
    function GetGroup(Index: Integer): TConfigDialogGroup;
    procedure SetGroup(Index: Integer; const Value: TConfigDialogGroup);
    function GetGroupCount: Integer;
  public
    constructor Create;

    function IndexOf(const Group: string): Integer;

    property Title: string read FTitle;
    property Enabled: Boolean read FEnabled;
    property GroupCount: Integer read GetGroupCount;
    property Groups[Index: Integer]: TConfigDialogGroup read GetGroup
      write SetGroup;
  end;

  TConfigGroupItem = class
  private
    FName: string;
    FCaption: string;
    FGroup: TConfigDialogGroup;
  public
    property Name: string read FName;
    property Caption: string read FCaption;
    property Group: TConfigDialogGroup read FGroup;
  end;

  TConfigExpression = class
  private
    FReplace: string;
    FValue: string;
    FUnitName: string;
    FUnitExpression: TConfigExpression;
    FGroupItem: TConfigGroupItem;
  public
    property Replace: string read FReplace;
    property Value: string read FValue write FValue;
    property UnitName: string read FUnitName;
    property UnitExpression: TConfigExpression read FUnitExpression;
    property GroupItem: TConfigGroupItem read FGroupItem;
  end;

  TConfigUnit = class
  private
    FName: string;
    FTemplateFile: string;
    FUnitType: string;
    FIsProject: Boolean;
    FExpressions: array of TConfigExpression;

    function GetExpressionCount: Integer;
    function GetExpression(Index: Integer): TConfigExpression;
    procedure SetExpression(Index: Integer; Value: TConfigExpression);
  public
    constructor Create;

    function GetTemplateSource: string;

    property Name: string read FName write FName;
    property TemplateFile: string read FTemplateFile;
    property UnitType: string read FUnitType;
    property IsProject: Boolean read FIsProject;
    property ExpressionCount: Integer read GetExpressionCount;
    property Expressions[Index: Integer]: TConfigExpression read GetExpression
      write SetExpression;
  end;

  TAddinType = (atProject, atUnit);

  TConfig = class;

  TConfigAddin = class
  private
    FConfig: TConfig;
    FName: string;
    FAddinType: TAddinType;
    FVersion: string;
    FCompany: string;
    FCompanyLong: string;
    FMenuCategory: string;
    FImageFile: string;
    FDialog: TConfigDialog;
    FUnits: array of TConfigUnit;
    FUnitProject: Integer;
    FReferences: array of TConfigReference;

    function GetUnitCount: Integer;
    function GetUnit(Index: Integer): TConfigUnit;
    procedure SetUnit(Index: Integer; Value: TConfigUnit);
    function GetReferenceCount: Integer;
    function GetReference(Index: Integer): TConfigReference;
    procedure SetReference(Index: Integer; Value: TConfigReference);

    function GetAddInId: string;
  protected
    procedure ParseStaticData(Node: XmlNode);
    procedure Clear;
  public
    constructor Create(const AConfig: TConfig; Node: XmlNode);

    procedure ParseDynamicData;
    function IndexOf(const UnitName: string): Integer;

    property Name: string read FName;
    property AddinType: TAddinType read FAddinType;
    property Version: string read FVersion;
    property Company: string read FCompany;
    property CompanyLong: string read FCompanyLong;
    property MenuCategory: string read FMenuCategory;
    property ImageFile: string read FImageFile;
    property Dialog: TConfigDialog read FDialog;
    property UnitCount: Integer read GetUnitCount;
    property Units[Index: Integer]: TConfigUnit read GetUnit write SetUnit;
    property UnitProject: Integer read FUnitProject;
    property ReferenceCount: Integer read GetReferenceCount;
    property References[Index: Integer]: TConfigReference read GetReference
      write SetReference;

    property Config: TConfig read FConfig;
    property AddInId: string read GetAddInId;
  end;

  TConfig = class
  private
    FConfigFile: string;
    FTemplateFolder: string;
    FTemplatesPath: string;
    FBDSPath: string;
    FParsedStaticData: Boolean;
    FAddins: array of TConfigAddin;

    function GetAddinCount: Integer;
    function GetAddin(Index: Integer): TConfigAddin;
    procedure SetAddin(Index: Integer; const Value: TConfigAddin);
  protected
    procedure ParseStaticData;
  public
    constructor Create(const ConfigFile, TemplateFolder: string);

    property AddinCount: Integer read GetAddinCount;
    property Addins[Index: Integer]: TConfigAddin read GetAddin write SetAddin;
    property ConfigFile: string read FConfigFile;
    property TemplatesPath: string read FTemplatesPath;
    property BDSPath: string read FBDSPath;
  end;

function AddBackSlash(const APath: string): string;

implementation

uses
  System.IO,                // Path
  System.Reflection,        // Assembly
  System.Windows.Forms,
  Borland.Studio.ToolsAPI,
  BDSOTAUtils;

type
  SubstExpr = record
    Replace: string;
    Value: string;
  end;

  DefaultSubst = (dsProgram, dsUnit, dsTimeStamp);

const
  DefaultBDSPath = '$(BDS)';

  DefaultExpressions: array[DefaultSubst] of string =
    ('[!ProgramName]',
     '[!UnitName]',
     '[!TimeStamp]');

  BDSRegKey = 'Software\Borland\BDS\2.0';
  BDSRegApp = 'App';

function AddBackSlash(const APath: string): string;
begin
  if (APath <> '') and
     (APath[Length(APath)] <> Path.DirectorySeparatorChar) then
    Result := APath + Path.DirectorySeparatorChar
  else
    Result := APath;
end;

function RemoveBackSlash(const APath: string): string;
begin
  if (APath <> '') and
     (APath[Length(APath)] = Path.DirectorySeparatorChar) then
    Result := APath.Substring(0, APath.Length - 1)
  else
    Result := APath;
end;

{ TConfigDialog }

constructor TConfigDialog.Create;
begin
  inherited Create;

  SetLength(FGroups, 0);
end;

function TConfigDialog.GetGroupCount: Integer;
begin
  Result := Length(FGroups);
end;

function TConfigDialog.GetGroup(Index: Integer): TConfigDialogGroup;
begin
  if (Index >= 0) and (Index < Length(FGroups)) then
    Result := FGroups[Index]
  else
    Result := nil;
end;

procedure TConfigDialog.SetGroup(Index: Integer;
  const Value: TConfigDialogGroup);
begin
  if (Index >= 0) and (Index < Length(FGroups)) then
    FGroups[Index] := Value
  else
  begin
    SetLength(FGroups, Length(FGroups) + 1);
    FGroups[Length(FGroups) - 1] := Value;
  end;
end;

function TConfigDialog.IndexOf(const Group: string): Integer;
var
  Index: Integer;
begin
  Result := -1;

  for Index := 0 to Length(FGroups) - 1 do
    if FGroups[Index].FName.ToLower.Equals(Group) then
    begin
      Result := Index;
      Break;
    end;
end;

{ TConfigUnit }

constructor TConfigUnit.Create;
begin
  inherited;

  SetLength(FExpressions, 0);
end;

function TConfigUnit.GetExpressionCount: Integer;
begin
  Result := Length(FExpressions);
end;

function TConfigUnit.GetExpression(Index: Integer): TConfigExpression;
begin
  if (Index >= 0) and (Index < Length(FExpressions)) then
    Result := FExpressions[Index]
  else
    Result := nil;
end;

procedure TConfigUnit.SetExpression(Index: Integer; Value: TConfigExpression);
begin
  if (Index >= 0) and (Index < Length(FExpressions)) then
    FExpressions[Index] := Value
  else
  begin
    SetLength(FExpressions, Length(FExpressions) + 1);
    FExpressions[Length(FExpressions) - 1] := Value;
  end;
end;

function TConfigUnit.GetTemplateSource: string;
var
  FileStream: StreamReader;
  Subst: array of SubstExpr;
  Index: Integer;
begin
  Result := '';

  FileStream := System.IO.File.OpenText(FTemplateFile);
  try
    Result := FileStream.ReadToEnd;
  finally
    FileStream.Close;
  end;

  if FUnitType.ToLower.Equals(OTACreatorTypes.sText.ToLower) then
    Exit;

  SetLength(Subst, 2);
  if FIsProject then
    Subst[0].Replace := DefaultExpressions[dsProgram]
  else
    Subst[0].Replace := DefaultExpressions[dsUnit];
  Subst[0].Value := FName;
  Subst[1].Replace := DefaultExpressions[dsTimeStamp];
  Subst[1].Value := System.DateTime.Now.ToString;

  for Index := 0 to Length(Subst) - 1 do
    with Subst[Index] do
      Result := Result.Replace(Replace, Value);

  for Index := 0 to ExpressionCount - 1 do
    with Expressions[Index] do
      if FUnitExpression = nil then
        Result := Result.Replace(FReplace, FValue)
      else
        Result := Result.Replace(FUnitExpression.FReplace,
          FUnitExpression.FValue);
end;

{ TConfigAddin }

constructor TConfigAddin.Create(const AConfig: TConfig; Node: XmlNode);
begin
  inherited Create;

  FConfig := AConfig;

  SetLength(FUnits, 0);
  SetLength(FReferences, 0);
  FUnitProject := -1;

  ParseStaticData(Node);
end;

function TConfigAddin.GetUnitCount: Integer;
begin
  Result := Length(FUnits);
end;

function TConfigAddin.GetUnit(Index: Integer): TConfigUnit;
begin
  if (Index >= 0) and (Index < Length(FUnits)) then
    Result := FUnits[Index]
  else
    Result := nil;
end;

procedure TConfigAddin.SetUnit(Index: Integer; Value: TConfigUnit);
begin
  if (Index >= 0) and (Index < Length(FUnits)) then
    FUnits[Index] := Value
  else
  begin
    SetLength(FUnits, Length(FUnits) + 1);
    FUnits[Length(FUnits) - 1] := Value;
  end;
end;

function TConfigAddin.GetReferenceCount: Integer;
begin
  Result := Length(FReferences);
end;

function TConfigAddin.GetReference(Index: Integer): TConfigReference;
begin
  if (Index >= 0) and (Index < Length(FReferences)) then
    Result := FReferences[Index]
  else
    Result := nil;
end;

procedure TConfigAddin.SetReference(Index: Integer; Value: TConfigReference);
begin
  if (Index >= 0) and (Index < Length(FReferences)) then
    FReferences[Index] := Value
  else
  begin
    SetLength(FReferences, Length(FReferences) + 1);
    FReferences[Length(FReferences) - 1] := Value;
  end;
end;

function TConfigAddin.GetAddInId: string;
begin
  if (FCompany <> '') and (FName <> '') and (FVersion <> '') then
    Result := FCompany + '.' + FName.Replace(' ', '') + '.' + FVersion
  else
    Result := '';
end;

function TConfigAddin.IndexOf(const UnitName: string): Integer;
var
  Index: Integer;
begin
  Result := -1;

  for Index := 0 to Length(FUnits) - 1 do
    if FUnits[Index].FName.ToLower.Equals(UnitName.ToLower) then
    begin
      Result := Index;
      Break;
    end;
end;

procedure TConfigAddin.Clear;
var
  Index: Integer;
begin
  for Index := 0 to UnitCount - 1 do
    Units[Index].Free;

  for Index := 0 to ReferenceCount - 1 do
    References[Index].Free;

  SetLength(FUnits, 0);
  SetLength(FReferences, 0);
  FUnitProject := -1;

  if FDialog <> nil then
  begin
    FDialog.Free;
    FDialog := nil;
  end;
end;

procedure TConfigAddin.ParseStaticData(Node: XmlNode);
var
  Attr: XmlNode;
  Temp: string;
begin
  Attr := Node.Attributes.GetNamedItem('name');
  if Attr <> nil then
    FName := Attr.Value;
  Attr := Node.Attributes.GetNamedItem('version');
  if Attr <> nil then
    FVersion := Attr.Value;
  Attr := Node.Attributes.GetNamedItem('company');
  if Attr <> nil then
    FCompany := Attr.Value;
  Attr := Node.Attributes.GetNamedItem('companyLong');
  if Attr <> nil then
    FCompanyLong := Attr.Value;
  Attr := Node.Attributes.GetNamedItem('menuCategory');
  if Attr <> nil then
    FMenuCategory := Attr.Value;
  Attr := Node.Attributes.GetNamedItem('imageFile');
  if Attr <> nil then
  begin
    FImageFile := Attr.Value.Trim;
    if FImageFile <> '' then
    begin
      Temp := Path.GetDirectoryName(FImageFile);
      if Temp = '' then
        FImageFile := FConfig.TemplatesPath + FImageFile;
    end;
  end;

  if (FName = '') or (FCompany = '') or (FVersion = '') or
     (FMenuCategory = '') then
    raise ApplicationException.Create(
      FConfig.ConfigFile + ' has to be fully configured.');

  if FImageFile <> '' then
    // This is not a critical error - ignore missing image and keep on going
    if not System.IO.File.Exists(FImageFile) then
      MessageBox.Show('Image file ' + FImageFile + ' not found.',
        FName + ' add-in');
end;

procedure TConfigAddin.ParseDynamicData;
var
  Doc: XmlDataDocument;
  Root: XmlElement;
  QueryPath: string;
  Node, SubNode, SubNode2, Attr: XmlNode;
  Nodes, SubNodes: XmlNodeList;
  Index, Idx, SubIndex, ProjectCount: Integer;
  ProjectFound, UnitFound, EnabledFound: Boolean;
  TempDialog: TConfigDialog;
  TempDialogGroup: TConfigDialogGroup;
  TempUnit, TempUnit2: TConfigUnit;
  TempExpr: TConfigExpression;
  TempGroupItem: TConfigGroupItem;
  TempRef: TConfigReference;
begin
  Clear;

  Doc := XmlDataDocument.Create;
  try
    Doc.Load(FConfig.ConfigFile);
    Root := Doc.DocumentElement;
  except
    raise ApplicationException.Create(
      FConfig.ConfigFile + ' invalid file format.');
  end;

  QueryPath := '/configuration/addins/addin[@name = ''' + FName + ''']/';

  // dialog

  Node := Root.SelectSingleNode(QueryPath + 'dialog');
  if Node <> nil then
  begin
    TempDialog := TConfigDialog.Create;

    Attr := Node.Attributes.GetNamedItem('title');
    if Attr <> nil then
      TempDialog.FTitle := Attr.Value
    else
      TempDialog.FTitle := FName;
    Attr := Node.Attributes.GetNamedItem('enabled');
    if Attr <> nil then
      TempDialog.FEnabled := Attr.Value.ToLower.Equals('true')
    else
      TempDialog.FEnabled := True;

    // groups

    SubNodes := Node.SelectNodes('group');
    if SubNodes <> nil then
      for Index := 0 to SubNodes.Count - 1 do
      begin
        SubNode := SubNodes.Item(Index);
        TempDialogGroup := TConfigDialogGroup.Create;

        Attr := SubNode.Attributes.GetNamedItem('name');
        if Attr <> nil then
          TempDialogGroup.FName := Attr.Value.ToLower;
        Attr := SubNode.Attributes.GetNamedItem('caption');
        if Attr <> nil then
          TempDialogGroup.FCaption := Attr.Value;
        Attr := SubNode.Attributes.GetNamedItem('enabled');
        if Attr <> nil then
          TempDialogGroup.FEnabled := Attr.Value.ToLower.Equals('true')
        else
          TempDialogGroup.FEnabled := True;

        if (TempDialogGroup.FName = '') or (TempDialogGroup.FCaption = '') then
          raise ApplicationException.Create(FName + ' add-in' +
            ' dialog entry ' + System.Convert.ToString(Index + 1) +
            ' has to be fully configured.');

        TempDialog.Groups[Index] := TempDialogGroup;
      end;

    if TempDialog.GroupCount = 0 then
      raise ApplicationException.Create(
        FName + ' add-in doesn''t have any dialog groups configured.');

    // at least one group has to be enabled

    if TempDialog.Enabled then
    begin
      EnabledFound := False;
      for Index := 0 to TempDialog.GroupCount - 1 do
        if TempDialog.Groups[Index].Enabled then
        begin
          EnabledFound := True;
          Break;
        end;

      if not EnabledFound then
        raise ApplicationException.Create(
          'At least one dialog group has to be enabled for ' +
          FName + ' add-in.');
    end;

    FDialog := TempDialog;
  end;

  // units

  Nodes := Root.SelectNodes(QueryPath + 'units/unit');
  if (Nodes = nil) or (Nodes.Count = 0) then
    raise ApplicationException.Create(
      FName+ ' add-in information is not found or' +
      ' it doesn''t have any units configured.');

  for Index := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes.Item(Index);
    TempUnit := TConfigUnit.Create;

    Attr := Node.Attributes.GetNamedItem('name');
    if Attr <> nil then
      TempUnit.FName := Attr.Value;
    Attr := Node.Attributes.GetNamedItem('template');
    if Attr <> nil then
      TempUnit.FTemplateFile := Attr.Value;
    Attr := Node.Attributes.GetNamedItem('type');
    if Attr <> nil then
      TempUnit.FUnitType := Attr.Value
    else
      TempUnit.FUnitType := OTACreatorTypes.sUnit;

    if (TempUnit.FName = '') or (TempUnit.FTemplateFile = '') then
      raise ApplicationException.Create(FName + ' add-in' +
        ' unit entry ' + System.Convert.ToString(Index + 1) +
        ' has to be fully configured.');

    TempUnit.FIsProject :=
      Path.GetExtension(TempUnit.FTemplateFile).ToLower.Equals('.dpr') or
      Path.GetExtension(TempUnit.FTemplateFile).ToLower.Equals('.dpk');
    TempUnit.FTemplateFile := FConfig.TemplatesPath + TempUnit.FTemplateFile;

    // expressions

    SubNodes := Node.SelectNodes('expressions/expression');
    if SubNodes <> nil then
      for SubIndex := 0 to SubNodes.Count - 1 do
      begin
        SubNode := SubNodes.Item(SubIndex);
        TempExpr := TConfigExpression.Create;

        Attr := SubNode.Attributes.GetNamedItem('replace');
        if Attr <> nil then
          TempExpr.FReplace := Attr.Value;
        Attr := SubNode.Attributes.GetNamedItem('value');
        if Attr <> nil then
          TempExpr.FValue := Attr.Value;
        Attr := SubNode.Attributes.GetNamedItem('unit');
        if Attr <> nil then
          TempExpr.FUnitName := Attr.Value;

        if (TempExpr.FReplace = '') or
           ((TempExpr.FValue = '') and (TempExpr.FUnitName = '')) then
          raise ApplicationException.Create(FName + ' add-in' +
            ' unit entry ' + System.Convert.ToString(Index + 1) +
            ' expression entry ' + System.Convert.ToString(SubIndex + 1) +
            ' has to be fully configured.');

        if (TempExpr.FUnitName <> '') and
           (TempExpr.FUnitName.ToLower.Equals(TempUnit.FName.ToLower)) then
          raise ApplicationException.Create(FName + ' add-in' +
            ' unit entry ' + System.Convert.ToString(Index + 1) +
            ' expression entry ' + System.Convert.ToString(SubIndex + 1) +
            ' unit cannot refer to itself.');

        SubNode2 := SubNode.SelectSingleNode('groupItem');
        if SubNode2 <> nil then
        begin
          TempGroupItem := TConfigGroupItem.Create;

          Attr := SubNode2.Attributes.GetNamedItem('group');
          if Attr <> nil then
            TempGroupItem.FName := Attr.Value;
          Attr := SubNode2.Attributes.GetNamedItem('caption');
          if Attr <> nil then
            TempGroupItem.FCaption := Attr.Value;

          if (TempGroupItem.FName = '') or (TempGroupItem.FCaption = '') then
            raise ApplicationException.Create(FName + ' add-in' +
              ' unit entry ' + System.Convert.ToString(Index + 1) +
              ' expression entry ' + System.Convert.ToString(SubIndex + 1) +
              ' has to have group item fully configured.');

          if (TempGroupItem.FName <> '') and (FDialog = nil) then
            raise ApplicationException.Create(FName + ' add-in' +
              ' unit entry ' + System.Convert.ToString(Index + 1) +
              ' expression entry ' + System.Convert.ToString(SubIndex + 1) +
              ' refers to non existing dialog.');

          Idx := FDialog.IndexOf(TempGroupItem.FName);
          if Idx = -1 then
            raise ApplicationException.Create(FName + ' add-in' +
              ' unit entry ' + System.Convert.ToString(Index + 1) +
              ' expression entry ' + System.Convert.ToString(SubIndex + 1) +
              ' has an invalid dialog group assignment.');

          TempExpr.FGroupItem := TempGroupItem;
        end;

        TempUnit.Expressions[SubIndex] := TempExpr;
      end;

    Units[Index] := TempUnit;
  end; // units

  // make sure only one unit is marked as a project one (or none)

  ProjectFound := False;
  ProjectCount := 0;
  for Index := 0 to UnitCount - 1 do
    if Units[Index].FIsProject then
    begin
      ProjectFound := True;
      FUnitProject := Index;
      Inc(ProjectCount);
    end;
  if ProjectFound and (ProjectCount > 1) then
    raise ApplicationException.Create(
      'Only one unit can be designated as a project one for ' +
      FName + ' add-in.');

  if ProjectFound then
    FAddinType := atProject
  else
    FAddinType := atUnit;

  // make sure all expression references to other units are valid

  for Index := 0 to UnitCount - 1 do
  begin
    TempUnit := Units[Index];

    for Idx := 0 to TempUnit.ExpressionCount - 1 do
    begin
      TempExpr := TempUnit.Expressions[Idx];

      if TempExpr.FUnitName <> '' then
      begin
        SubIndex := IndexOf(TempExpr.FUnitName);
        if SubIndex = -1 then
          raise ApplicationException.Create(FName + ' add-in' +
            ' expression ' + TempExpr.FReplace +
            ' has an invalid unit reference.');

        UnitFound := False;
        TempUnit2 := Units[SubIndex];
        for SubIndex := 0 to TempUnit2.ExpressionCount - 1 do
          if TempUnit2.Expressions[SubIndex].FReplace.ToLower.Equals(
             TempExpr.FReplace.ToLower) then
          begin
            UnitFound := True;
            TempExpr.FUnitExpression := TempUnit2.Expressions[SubIndex];
            Break;
          end;

        if not UnitFound then
          raise ApplicationException.Create(FName + ' add-in' +
            ' expression ' + TempExpr.FReplace +
            ' is not defined in referenced unit.');
      end;
    end;
  end;

  // references

  Nodes := Root.SelectNodes(QueryPath + 'references/reference');
  if Nodes <> nil then
    for Index := 0 to Nodes.Count - 1 do
    begin
      Node := Nodes.Item(Index);
      TempRef := TConfigReference.Create;

      Attr := Node.Attributes.GetNamedItem('name');
      if Attr <> nil then
        TempRef.FAssembly := Attr.Value;

      if (TempRef.FAssembly = '') then
        raise ApplicationException.Create(FName + ' add-in' +
          ' reference entry ' + System.Convert.ToString(Index + 1) +
          ' has to be fully configured.');

      TempRef.FAssembly := TempRef.FAssembly.Replace(DefaultBDSPath,
        FConfig.BDSPath);

      References[Index] := TempRef;
    end;
end;

{ TConfig }

constructor TConfig.Create(const ConfigFile, TemplateFolder: string);
var
  IService: IOTAService;
begin
  inherited Create;

  FConfigFile := ConfigFile;
  FTemplateFolder := TemplateFolder;

  SetLength(FAddins, 0);

  // If config XML file doesn't include path then use add-in's path.
  // Otherwise use config's path. 
  FTemplatesPath := AddBackSlash(Path.GetDirectoryName(ConfigFile));
  if FTemplatesPath = '' then
    FTemplatesPath := AddBackSlash(
      Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location));
  FTemplatesPath := AddBackSlash(FTemplatesPath + TemplateFolder);

  IService := GetService;
  if IService <> nil then
    FBDSPath := RemoveBackSlash(IService.BinDirectory)
  else
    FBDSPath := '';

  ParseStaticData;
end;

function TConfig.GetAddinCount: Integer;
begin
  Result := Length(FAddins);
end;

function TConfig.GetAddin(Index: Integer): TConfigAddin;
begin
  if (Index >= 0) and (Index < Length(FAddins)) then
    Result := FAddins[Index]
  else
    Result := nil;
end;

procedure TConfig.SetAddin(Index: Integer; const Value: TConfigAddin);
begin
  if (Index >= 0) and (Index < Length(FAddins)) then
    FAddins[Index] := Value
  else
  begin
    SetLength(FAddins, Length(FAddins) + 1);
    FAddins[Length(FAddins) - 1] := Value;
  end;
end;

procedure TConfig.ParseStaticData;
var
  Doc: XmlDataDocument;
  Root: XmlElement;
  Node: XmlNode;
  Nodes: XmlNodeList;
  Index: Integer;
  TempAddin: TConfigAddin;
begin
  if FParsedStaticData then
    Exit;

  Doc := XmlDataDocument.Create;
  try
    Doc.Load(FConfigFile);
    Root := Doc.DocumentElement;
  except
    raise ApplicationException.Create(FConfigFile +
      ' has invalid XML file format.');
  end;

  // addin

  Nodes := Root.SelectNodes('/configuration/addins/addin');
  if (Nodes = nil) or (Nodes.Count = 0) then
    raise ApplicationException.Create(
      FConfigFile + ' doesn''t have any add-ins configured.');

  for Index := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes.Item(Index);
    TempAddin := TConfigAddin.Create(Self, Node);

    Addins[Index] := TempAddin;
  end;
end;

end.
