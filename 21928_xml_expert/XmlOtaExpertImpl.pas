unit XmlOtaExpertImpl;

interface

uses
  Borland.Studio.ToolsAPI, System.Xml.Schema, System.Text, System.Windows.Forms;

type
  TSourceFileType = (ftUnknown, ftXML, ftXSD, ftConfig);

  TXmlOtaExpert = class(TObject)
  private
    FSchemaOpenDialog: OpenFileDialog;
    FPluginInfoIndex: Integer;
    FXmlGroup: IOTAMessageGroup;
    FXmlGroupFileName: string;
    XmlMenuItem: IOTAMenuItem;
    FormatXmlMenuItem: IOTAMenuItem;
    GenerateClassesMenuItem: IOTAMenuItem;
    GenerateSchemaMenuItem: IOTAMenuItem;
    Separator1MenuItem: IOTAMenuItem;
    Separator2MenuItem: IOTAMenuItem;
    ValidateMenuItem: IOTAMenuItem;
    ValidateSchemaMenuItem: IOTAMenuItem;
    ViewDataMenuItem: IOTAMenuItem;
    procedure FormatXmlMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure GenerateClassesMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure GenerateSchemaMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure ValidateMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure ValidateSchemaMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure ViewDataMenuItemExecuted(Sender: TObject; E: EventArgs);
    procedure ValidateCallBack(Sender: TObject; Args: ValidationEventArgs);
    function GetXmlGroup: IOTAMessageGroup;
    // Notifiers
    procedure FileNotify(Sender: TObject; Args: FileNotificationEventArgs);
    procedure IdleNotify(Sender: TObject; Args: EventArgs);
    procedure MessageGroupDeletedNotify(Sender: TObject; Args: MessageGroupEventArgs);
  public
    constructor Create;
    procedure CreateAboutBox;
    procedure CreateMenuItems;
    procedure CreateSchemaOpenDialog;
    function CurrentFileType: TSourceFileType;
    function CurrentContent: string;
    procedure HandleXmlException(E: Exception; const FileName: string);
    procedure RemoveXmlGroup(const NewFileName: string);
    procedure ReportToolErrors(const Errors, ToolName: string);
    procedure ShowXmlGroup;
    procedure TryFormatXmlFile(const FileName: string);
    procedure ValidateMessageBox(const MessageText: string);
    procedure ValidateXmlContent(const SchemaFileName: string);
    property XmlGroup: IOTAMessageGroup read GetXmlGroup;
    class function FileType(const FileName: string): TSourceFileType;
    class function FormatXmlData(const Xml: string): string;
    class procedure IDERegister; static;
  end;

implementation

uses
  System.IO, System.Xml, System.Collections.Specialized,
  PetrVones.Utils.OpenTools, DataView, GenerateClasses, CmdLineTool;

const
  XmlExtension = '.xml';
  XsdExtension = '.xsd';
  ConfigExtension = '.config';

  XmlGroupName = 'XML Errors';

procedure HandleException(E: Exception);
var
  S, M: string;
begin
  while E <> nil do
  begin
    S := typeof(E).ToString + #13#10;
    S := S + E.Stacktrace;
    S := S + #13#10;
    M := E.Message;
    E := E.InnerException;
  end;
  MessageBox.Show(S, M);
end;

{ TOtaDemo }

constructor TXmlOtaExpert.Create;
begin
  inherited Create;
  CreateMenuItems;
  CreateAboutBox;
  Include(TOtaUtils.OTAService.FileNotification, FileNotify);
  Include(TOtaUtils.IdleNotifier.Idle, IdleNotify);
  Include(TOTAUtils.MessageService.MessageGroupDeleted, MessageGroupDeletedNotify);
  CreateSchemaOpenDialog;
end;

procedure TXmlOtaExpert.CreateAboutBox;
begin
  FPluginInfoIndex := TOtaUtils.AboutBoxService.AddPluginInfo('XML IDE Expert', 'XML IDE expert Open Tools API example', nil, False, '', '');
end;

procedure TXmlOtaExpert.CreateMenuItems;
var
  Menu: IOTAMainMenuService;
begin
  Menu := TOtaUtils.MainMenuService;
  XmlMenuItem := Menu.AddMenuItem('HTMLTidySubmenuItem', OTAMenuItemLocation.otamlAfter, 'XMLSubmenuItem', 'XML');
  // Format document
  FormatXmlMenuItem := Menu.AddMenuItem(XmlMenuItem.Name, OTAMenuItemLocation.otamlChild, 'XMLFormatDocumentItem', 'Format Document');
  Include(FormatXmlMenuItem.Executed, FormatXmlMenuItemExecuted);
  // View Data
  ViewDataMenuItem := Menu.AddMenuItem(FormatXmlMenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLViewDataItem', 'View Data');
  Include(ViewDataMenuItem.Executed, ViewDataMenuItemExecuted);
  // separator
  Separator1MenuItem := Menu.AddMenuItem(ViewDataMenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLSeparator1Item', '-');
  // Validate document
  ValidateMenuItem := Menu.AddMenuItem(Separator1MenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLValidateItem', 'Validate Document');
  Include(ValidateMenuItem.Executed, ValidateMenuItemExecuted);
  // Validate using Schmema
  ValidateSchemaMenuItem := Menu.AddMenuItem(ValidateMenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLValidateSchemaItem', 'Validate using Schema');
  Include(ValidateSchemaMenuItem.Executed, ValidateSchemaMenuItemExecuted);
  // separator
  Separator2MenuItem := Menu.AddMenuItem(ValidateSchemaMenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLSeparator2Item', '-');
  // Generate Classes
  GenerateClassesMenuItem := Menu.AddMenuItem(Separator2MenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLGenerateClassesItem', 'Generate Classes');
  Include(GenerateClassesMenuItem.Executed, GenerateClassesMenuItemExecuted);
  // Generate Schema
  GenerateSchemaMenuItem := Menu.AddMenuItem(GenerateClassesMenuItem.Name, OTAMenuItemLocation.otamlAfter, 'XMLGenerateSchematem', 'Generate Schema');
  Include(GenerateSchemaMenuItem.Executed, GenerateSchemaMenuItemExecuted);
end;

procedure TXmlOtaExpert.CreateSchemaOpenDialog;
begin
  FSchemaOpenDialog := OpenFileDialog.Create;
  FSchemaOpenDialog.Filter := 'XSD Schemas (*.xsd)|*.xsd';
  FSchemaOpenDialog.RestoreDirectory := True;
  FSchemaOpenDialog.Title := 'Select XSD schema for validating current document';
end;

function TXmlOtaExpert.CurrentContent: string;
var
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
begin
  Module := TOtaUtils.ModuleServices.CurrentModule;
  SourceEditor := TOTAUtils.GetSourceEditorForModule(Module);
  Result := TOTAUtils.GetSourceEditorText(SourceEditor);
end;

function TXmlOtaExpert.CurrentFileType: TSourceFileType;
var
  Module: IOTAModule;
begin
  Module := TOtaUtils.ModuleServices.CurrentModule;
  if Assigned(Module) then
    Result := FileType(Module.FileName)
  else
    Result := ftUnknown;
end;

procedure TXmlOtaExpert.FileNotify(Sender: TObject; Args: FileNotificationEventArgs);
begin
  if Assigned(FXmlGroup) and (Args.NotifyCode = OTAFileNotification.ofnFileClosing) and (Args.FileName = FXmlGroupFileName) then
    TOTAUtils.MessageService.ClearMessageGroup(FXmlGroup);
{ TODO : Notification never called with OTAFileNotification.ofnFileOpening }
  if (Args.NotifyCode = OTAFileNotification.ofnFileOpened) and (Length(Args.FileName) > 0) and (FileType(Args.FileName) = ftXML) then
    TryFormatXmlFile(Args.FileName);
end;

class function TXmlOtaExpert.FileType(const FileName: string): TSourceFileType;
var
  Ext: string;
begin
  Ext := Path.GetExtension(FileName).ToLower;
  if Ext = XmlExtension then
    Result := ftXML
  else
  if Ext = XsdExtension then
    Result := ftXSD
  else
  if Ext = ConfigExtension then
    Result := ftConfig
  else  
    Result := ftUnknown;
end;

class function TXmlOtaExpert.FormatXmlData(const Xml: string): string;
var
  Doc: XmlDocument;
  Ms: MemoryStream;
  Xw: XmlTextWriter;
  Sr: StreamReader;
begin
  Doc := XmlDocument.Create;
  Doc.LoadXml(Xml);
  Ms := MemoryStream.Create;
  Xw := XmlTextWriter.Create(Ms, System.Text.Encoding.UTF8);
  Xw.Formatting := Formatting.Indented;
  Xw.Indentation := 2;
  Xw.IndentChar := ' ';
  doc.Save(Xw);
  Xw.Flush;
  Ms.Position := 0;
  Sr := StreamReader.Create(Ms);
  Result := Sr.ReadToEnd;
  Sr.Close;
  Xw.Close;
  Ms.Close;
end;

procedure TXmlOtaExpert.FormatXmlMenuItemExecuted(Sender: TObject; E: EventArgs);
var
  Content: string;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
begin
  Cursor.Current := Cursors.WaitCursor;
  try
    Module := TOtaUtils.ModuleServices.CurrentModule;
    RemoveXmlGroup(Module.FileName);
    SourceEditor := TOTAUtils.GetSourceEditorForModule(Module);
    Content := TOTAUtils.GetSourceEditorText(SourceEditor);
    try
      Content := FormatXmlData(Content);
      TOTAUtils.SetSourceEditorText(SourceEditor, Content);
      IOTAEditor(SourceEditor).MarkModified;
    except
      on E: Exception do
        HandleXmlException(E, Module.FileName);
    end;
    ShowXmlGroup;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TXmlOtaExpert.GenerateClassesMenuItemExecuted(Sender: TObject; E: EventArgs);
var
  G: TGenerateClassesForm;
  Module: IOTAModule;
  FileName: string;
begin
  G := TGenerateClassesForm.Create;
  try
    FileName := TOTAUtils.CurrentModule.FileName;
    RemoveXmlGroup(FileName);
    FileName := TOTAUtils.UniqueModuleName(Path.ChangeExtension(FileName, '.pas'));
    G.LoadSchema(CurrentContent, Path.GetFileNameWithoutExtension(FileName));
    if G.ShowDialog = System.Windows.Forms.DialogResult.OK then
    begin
      if Assigned(G.ExecuteException) then
      begin
        ReportToolErrors(G.ErrorOutput, 'XSD.EXE');
        ShowXmlGroup;
      end
      else
        Module := TOTAUtils.ModuleServices.CreateModule(TOTASourceModuleCreator.Create(G.UnitContent, OTACreatorTypes.sUnit, FileName));
    end;
  finally
    G.Dispose;
  end;
end;

procedure TXmlOtaExpert.GenerateSchemaMenuItemExecuted(Sender: TObject; E: EventArgs);
const
  XmlName = 'temp.xml';
  XsdName = 'temp.xsd';
var
  Cmd: TCommandLineToolOutput;
  CmdLine: string;
  Module: IOTAModule;
  SchemaFileName, Content: string;
begin
  Cursor.Current := Cursors.WaitCursor;
  Cmd := TCommandLineToolOutput.Create;
  try
    SchemaFileName := TOTAUtils.UniqueModuleName(Path.ChangeExtension(TOTAUtils.CurrentModule.FileName, XsdExtension));
    RemoveXmlGroup('');
    try
      Cmd.SaveFile(XmlName, CurrentContent);
      CmdLine := System.String.Format('"{0}" "/o:{1}" /nologo', [Cmd.ExpandFileName(XmlName), Cmd.LocalDirectoryName]);
      Cmd.Run(XsdToolPathName, CmdLine);
      Content := Cmd.ReadFile(XsdName);
      Module := TOTAUtils.ModuleServices.CreateModule(TOTASourceModuleCreator.Create(Content, OTACreatorTypes.sText, SchemaFileName));
    except
      on E: Exception do
        ReportToolErrors(Cmd.ErrorOutput, 'XSD.EXE');
    end;
    ShowXmlGroup;
  finally
    Cmd.Close;
    Cursor.Current := Cursors.Default;
  end;
end;

function TXmlOtaExpert.GetXmlGroup: IOTAMessageGroup;
begin
  if FXmlGroup = nil then
    FXmlGroup := TOTAUtils.MessageService.AddMessageGroup(XmlGroupName);
  Result := FXmlGroup;
end;

procedure TXmlOtaExpert.HandleXmlException(E: Exception; const FileName: string);
var
  Ref: IntPtr;
  L, P: Integer;
begin
  if E is XmlException then
  begin
    L := XmlException(E).LineNumber;
    P := XmlException(E).LinePosition;
  end
  else
  begin
    P := 0;
    L := 0;
  end;
  TOTAUtils.MessageService.AddToolMessage(FileName, E.Message, 'Error', L, P, nil, Ref, XmlGroup);
end;

class procedure TXmlOtaExpert.IDERegister;
begin
  TXmlOtaExpert.Create;
end;

procedure TXmlOtaExpert.IdleNotify(Sender: TObject; Args: EventArgs);
var
  F: TSourceFileType;
begin
  F := CurrentFileType;
  XmlMenuItem.Enabled := F in [ftXML, ftXSD, ftConfig];
  FormatXmlMenuItem.Enabled := F in [ftXML, ftXSD, ftConfig];
  ViewDataMenuItem.Enabled := F = ftXML;
  ValidateMenuItem.Enabled := F in [ftXML, ftXSD, ftConfig];
  ValidateSchemaMenuItem.Enabled := F = ftXML;
  GenerateClassesMenuItem.Enabled := F = ftXSD;
  GenerateSchemaMenuItem.Enabled := F in [ftXML, ftConfig];
end;

procedure TXmlOtaExpert.MessageGroupDeletedNotify(Sender: TObject; Args: MessageGroupEventArgs);
begin
  if Args.Group = FXmlGroup then
    FXmlGroup := nil;
end;

procedure TXmlOtaExpert.RemoveXmlGroup(const NewFileName: string);
begin
  if Assigned(FXmlGroup) then
  begin
    TOTAUtils.MessageService.RemoveMessageGroup(FXmlGroup);
    FXmlGroup := nil;
  end;
  FXmlGroupFileName := NewFileName;
end;

procedure TXmlOtaExpert.ReportToolErrors(const Errors, ToolName: string);
var
  I: Integer;
  Ref: IntPtr;
  StringCol: StringCollection;
  S, E: string;
begin
  if Errors = nil then
    E := ''
  else
    E := Errors;
  StringCol := StringCollection.Create;
  StringCol.AddRange(E.Replace(#13#10, #10).Split([#10]));
  for I := 0 to StringCol.Count - 1 do
  begin
    S := StringCol[I].Trim;
    if S <> '' then
      TOTAUtils.MessageService.AddToolMessage('', S, ToolName, 0, 0, nil, Ref, XmlGroup);
  end;
end;

procedure TXmlOtaExpert.ShowXmlGroup;
begin
  if Assigned(FXmlGroup) then
    TOTAUtils.MessageService.ShowMessageView(XmlGroup);
end;

procedure TXmlOtaExpert.TryFormatXmlFile(const FileName: string);
var
  FormattedContent: string;
  Reader: StreamReader;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;

  function CheckLongLines(SourceReader: TextReader): Boolean;
  const
    MaxEditorLineLength = 4095;
  var
    Content: string;
    MaxLength: Integer;
  begin
    MaxLength := 0;
    repeat
      Content := SourceReader.ReadLine;
      MaxLength := Math.Max(MaxLength, Length(Content));
      Result := MaxLength >= MaxEditorLineLength;
    until (Content = nil) or Result;
    SourceReader.Close;
  end;

begin
  Cursor.Current := Cursors.WaitCursor;
  try
    try
      if CheckLongLines(StreamReader.Create(FileName)) then
      begin
        Reader := StreamReader.Create(FileName);
        FormattedContent := FormatXmlData(Reader.ReadToEnd);
        Reader.Close;
        if not CheckLongLines(StringReader.Create(FormattedContent)) then
        begin
          Module := TOTAUtils.ModuleServices.FindModule(FileName);
          if Assigned(Module) then
          begin
            SourceEditor := TOTAUtils.GetSourceEditorForModule(Module);
            TOTAUtils.SetSourceEditorText(SourceEditor, FormattedContent);
          end;
        end;
      end;
    except
    end;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TXmlOtaExpert.ValidateCallBack(Sender: TObject; Args: ValidationEventArgs);
var
  Ref: IntPtr;
begin
  TOTAUtils.MessageService.AddToolMessage(FXmlGroupFileName, Args.Message, TObject(Args.Severity).ToString,
    Args.Exception.LineNumber, Args.Exception.LinePosition, nil, Ref, XmlGroup);
end;

procedure TXmlOtaExpert.ValidateMenuItemExecuted(Sender: TObject; E: EventArgs);

  procedure ValidateXSD;
  var
    Sr: StringReader;
  begin
    Sr := StringReader.Create(CurrentContent);
    try
      XmlSchema.Read(Sr, ValidateCallBack).Compile(ValidateCallBack);
    except
      on E: Exception do
        HandleXmlException(E, FXmlGroupFileName);
    end;
    Sr.Close;
  end;

begin
  RemoveXmlGroup(TOTAUtils.CurrentModule.FileName);
  Cursor.Current := Cursors.WaitCursor;
  try
    case CurrentFileType of
      ftXML:
        ValidateXmlContent('');
      ftXSD:
        ValidateXSD;
    end;
    if Assigned(FXmlGroup) then
      ShowXmlGroup
    else
      ValidateMessageBox('Validation OK');
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TXmlOtaExpert.ValidateMessageBox(const MessageText: string);
begin
  MessageBox.Show(MessageText, 'Validate', MessageBoxButtons.OK, MessageBoxIcon.Information);
end;

procedure TXmlOtaExpert.ValidateSchemaMenuItemExecuted(Sender: TObject; E: EventArgs);
begin
  if FSchemaOpenDialog.ShowDialog = DialogResult.OK then
  begin
    RemoveXmlGroup(TOTAUtils.CurrentModule.FileName);
    Cursor.Current := Cursors.WaitCursor;
    try
      ValidateXmlContent(FSchemaOpenDialog.FileName);
      if Assigned(FXmlGroup) then
        ShowXmlGroup
      else
        ValidateMessageBox('Validation using schema OK');
    finally
      Cursor.Current := Cursors.Default;
    end;
  end;
end;

procedure TXmlOtaExpert.ValidateXmlContent(const SchemaFileName: string);
var
  Ms: MemoryStream;
  Xr: XmlTextReader;
  Vr: XmlValidatingReader;
  Module: IOTAModule;
  SourceEditor: IOTASourceEditor;
  ErrorFileName: string;
begin
  Module := TOtaUtils.ModuleServices.CurrentModule;
  SourceEditor := TOTAUtils.GetSourceEditorForModule(Module);
  Ms := MemoryStream.Create(TOTAUtils.GetSourceEditorData(SourceEditor));
  Xr := XmlTextReader.Create(Ms);
  Vr := XmlValidatingReader.Create(Xr);
  Include(Vr.ValidationEventHandler, ValidateCallBack);
  try
    ErrorFileName := FXmlGroupFileName;
    if SchemaFileName <> '' then
    begin
      ErrorFileName := SchemaFileName;
      Vr.Schemas.Add(string(nil), SchemaFileName);
      Vr.ValidationType := ValidationType.Schema;
    end;
    while Vr.Read do;
  except
    on E: Exception do
      HandleXmlException(E, ErrorFileName);
  end;
  Vr.Close;
end;

procedure TXmlOtaExpert.ViewDataMenuItemExecuted(Sender: TObject; E: EventArgs);
begin
  TViewDataForm.ShowXmlData(CurrentContent, Path.GetFileName(TOtaUtils.ModuleServices.CurrentModule.FileName));
end;

end.
