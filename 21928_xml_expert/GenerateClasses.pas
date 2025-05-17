unit GenerateClasses;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, System.Xml.Schema;

type
  TGenerateClassesForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    ButtonGenerate: System.Windows.Forms.Button;
    Label1: System.Windows.Forms.Label;
    GroupBox1: System.Windows.Forms.GroupBox;
    ComboBoxElements: System.Windows.Forms.ComboBox;
    ButtonCancel: System.Windows.Forms.Button;
    CheckBoxCode: System.Windows.Forms.CheckBox;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure ButtonGenerate_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    type
      TSchemaInfo = record
        Name: string;
        TypeName: string;
      public
        function ToString: string; override;
      end;
  private
    FExecuteException: Exception;
    FSchemaContent: string;
    FUnitName: string;
    FErrorOutput: string;
    FUnitContent: string;
  public
    constructor Create;
    procedure Generate;
    procedure LoadSchema(const Schema, UnitName: string);
    property ExecuteException: Exception read FExecuteException;
    property ErrorOutput: string read FErrorOutput;
    property UnitContent: string read FUnitContent;
  end;

implementation

uses
  System.Diagnostics, System.IO, System.Collections.Specialized,
  CmdLineTool;

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TGenerateClassesForm.InitializeComponent;
begin
  Self.ButtonGenerate := System.Windows.Forms.Button.Create;
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.GroupBox1 := System.Windows.Forms.GroupBox.Create;
  Self.CheckBoxCode := System.Windows.Forms.CheckBox.Create;
  Self.ComboBoxElements := System.Windows.Forms.ComboBox.Create;
  Self.ButtonCancel := System.Windows.Forms.Button.Create;
  Self.GroupBox1.SuspendLayout;
  Self.SuspendLayout;
  // 
  // ButtonGenerate
  // 
  Self.ButtonGenerate.Location := System.Drawing.Point.Create(24, 128);
  Self.ButtonGenerate.Name := 'ButtonGenerate';
  Self.ButtonGenerate.TabIndex := 1;
  Self.ButtonGenerate.Text := 'Generate';
  Include(Self.ButtonGenerate.Click, Self.ButtonGenerate_Click);
  // 
  // Label1
  // 
  Self.Label1.AutoSize := True;
  Self.Label1.Location := System.Drawing.Point.Create(8, 16);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(102, 16);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := '&Select root element';
  // 
  // GroupBox1
  // 
  Self.GroupBox1.Controls.Add(Self.CheckBoxCode);
  Self.GroupBox1.Controls.Add(Self.ComboBoxElements);
  Self.GroupBox1.Controls.Add(Self.Label1);
  Self.GroupBox1.Location := System.Drawing.Point.Create(8, 8);
  Self.GroupBox1.Name := 'GroupBox1';
  Self.GroupBox1.Size := System.Drawing.Size.Create(224, 104);
  Self.GroupBox1.TabIndex := 0;
  Self.GroupBox1.TabStop := False;
  Self.GroupBox1.Text := 'Options';
  // 
  // CheckBoxCode
  // 
  Self.CheckBoxCode.AccessibleDescription := 'Generate &code for loading from file';
  Self.CheckBoxCode.Location := System.Drawing.Point.Create(8, 64);
  Self.CheckBoxCode.Name := 'CheckBoxCode';
  Self.CheckBoxCode.Size := System.Drawing.Size.Create(208, 24);
  Self.CheckBoxCode.TabIndex := 0;
  Self.CheckBoxCode.Text := '&Generate code for loading from file';
  // 
  // ComboBoxElements
  // 
  Self.ComboBoxElements.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.ComboBoxElements.Location := System.Drawing.Point.Create(8, 32);
  Self.ComboBoxElements.Name := 'ComboBoxElements';
  Self.ComboBoxElements.Size := System.Drawing.Size.Create(208, 21);
  Self.ComboBoxElements.TabIndex := 1;
  // 
  // ButtonCancel
  // 
  Self.ButtonCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.ButtonCancel.Location := System.Drawing.Point.Create(144, 128);
  Self.ButtonCancel.Name := 'ButtonCancel';
  Self.ButtonCancel.TabIndex := 2;
  Self.ButtonCancel.Text := 'Cancel';
  // 
  // TGenerateClassesForm
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(242, 167);
  Self.Controls.Add(Self.GroupBox1);
  Self.Controls.Add(Self.ButtonGenerate);
  Self.Controls.Add(Self.ButtonCancel);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TGenerateClassesForm';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Generate Classes';
  Self.GroupBox1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TGenerateClassesForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TGenerateClassesForm.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //
end;

procedure TGenerateClassesForm.LoadSchema(const Schema, UnitName: string);
var
  Xsd: XmlSchema;
  Sr: StringReader;
  I: Integer;
  O: TObject;
  Info: TSchemaInfo;
begin
  FSchemaContent := Schema;
  FUnitName := UnitName;
  Sr := StringReader.Create(Schema);
  try
    Cursor.Current := Cursors.WaitCursor;
    try
      Xsd := XmlSchema.Read(Sr, nil);
      for I := 0 to Xsd.Items.Count - 1 do
      begin
        O := Xsd.Items[I];
        if O is XmlSchemaElement then
        begin
          Info.Name := XmlSchemaElement(O).Name;
          Info.TypeName := XmlSchemaElement(O).SchemaTypeName.Name;
          if Info.TypeName = '' then
            Info.TypeName := Info.Name;
          ComboBoxElements.Items.Add(Info);
        end;  
      end;
      ComboBoxElements.SelectedIndex := 0;
    except
      on E: Exception do
        ButtonGenerate.Enabled := False;
    end;
  finally
    Cursor.Current := Cursors.Default;
    Sr.Close;
  end;
end;

procedure TGenerateClassesForm.ButtonGenerate_Click(sender: System.Object; e: System.EventArgs);
begin
  Cursor.Current := Cursors.WaitCursor;
  try
    Generate;
    DialogResult := System.Windows.Forms.DialogResult.OK;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TGenerateClassesForm.Generate;
var
  SchemaFileName, UnitFileName: string;
  Cmd: TCommandLineToolOutput;
  RootElementInfo: TSchemaInfo;

  procedure RunXsd;
  var
    CmdLine: string;
  begin
    CmdLine := System.String.Format('"{0}" /c "/l:{1}" "/n:{2}" "/o:{3}" "/e:{4}" /nologo',
      [Cmd.ExpandFileName(SchemaFileName), DelphiProviderFullName, FUnitName, Cmd.LocalDirectoryName, RootElementInfo.Name]);
    Cmd.Run(XsdToolPathName, CmdLine);
    FErrorOutput := Cmd.ErrorOutput;
  end;

  procedure GenerateLoadCode;
  var
    S: StringCollection;
    ImplLine: Integer;
    L: string;
    Temp: array of string;
  begin
    S := StringCollection.Create;
    S.AddRange(UnitContent.Replace(#13#10, #10).Split([#10]));
    ImplLine := S.IndexOf('end.');
    if ImplLine = -1 then
      Exit;
    S.RemoveAt(ImplLine);
    S.RemoveAt(ImplLine - 1);
    ImplLine := S.IndexOf('implementation');
    if ImplLine = -1 then
      Exit;
    L := System.String.Format('function Load{0}(const FileName: string): {1};', [RootElementInfo.Name, RootElementInfo.TypeName]);
    S.Insert(ImplLine - 1, L);
    S.Insert(ImplLine - 1, '');
    S.Add('uses');
    S.Add('  System.IO;');
    S.Add('');
    S.Add(L);
    S.Add('var');
    S.Add('  Stream: FileStream;');
    S.Add('begin');
    S.Add('  Stream := FileStream.Create(FileName, FileMode.Open);');
    S.Add('  try');
    S.Add(System.String.Format('    Result := XmlSerializer.Create({0}.ClassInfo).Deserialize(Stream) as {0};', [RootElementInfo.TypeName]));
    S.Add('  finally');
    S.Add('    Stream.Close;');
    S.Add('  end;');
    S.Add('end;');
    S.Add('');
    S.Add('end.');
    SetLength(Temp, S.Count);
    S.CopyTo(Temp, 0);
    FUnitContent := System.String.Join(#13#10, Temp);
  end;

begin
  Cmd := TCommandLineToolOutput.Create;
  try
    try
      Assert(ComboBoxElements.SelectedIndex >= 0);
      RootElementInfo := TSchemaInfo(ComboBoxElements.SelectedItem);
      SchemaFileName := FUnitName + '.xsd';
      UnitFileName := FUnitName +  '.pas';
      Cmd.SaveFile(SchemaFileName, FSchemaContent);
      RunXsd;
      FUnitContent := Cmd.ReadFile(UnitFileName);
      if CheckBoxCode.Checked then
        GenerateLoadCode;
    except
      on E: Exception do
      begin
        FErrorOutput := Cmd.ErrorOutput;
        if FErrorOutput = '' then
          FErrorOutput := Cmd.Output;
        FExecuteException := E;
      end;
    end;
  finally
    Cmd.Close;
  end;
end;

{ TGenerateClassesForm.TSchemaInfo }

function TGenerateClassesForm.TSchemaInfo.ToString: string;
begin
  Result := Name;
end;

end.
