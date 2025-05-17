unit DataView;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data, System.IO, System.Resources;

type
  TViewDataForm = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    ListBoxTables: System.Windows.Forms.ListBox;
    DataGrid: System.Windows.Forms.DataGrid;
    LabelError: System.Windows.Forms.Label;
    ToolBar: System.Windows.Forms.ToolBar;
    ToolBarButtonDataPacket: System.Windows.Forms.ToolBarButton;
    ToolBarButtonSave: System.Windows.Forms.ToolBarButton;
    ImageList: System.Windows.Forms.ImageList;
    SaveFileDialogXml: System.Windows.Forms.SaveFileDialog;
    Splitter1: System.Windows.Forms.Splitter;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure ListBoxTables_SelectedIndexChanged(sender: System.Object; e: System.EventArgs);
    procedure TViewDataForm_Closed(sender: System.Object; e: System.EventArgs);
    procedure ToolBar_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    FData: string;
    FDataSet: DataSet;
    class var FViewDataForm: TViewDataForm;
  public
    constructor Create;
    procedure LoadXmlData(const Data: string);
    procedure FillXmlData(ViewAsDataPacket: Boolean);
    procedure SaveXmlData;
    procedure UpdateControls(const ErrorMessage: string);
    class procedure ShowXmlData(const Data, Caption: string); static;
  end;

implementation

uses PetrVones.Utils.DataPacket;

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TViewDataForm.InitializeComponent;
type
  TSystem_Windows_Forms_ToolBarButtonArray = array of System.Windows.Forms.ToolBarButton;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TViewDataForm));
  Self.ListBoxTables := System.Windows.Forms.ListBox.Create;
  Self.LabelError := System.Windows.Forms.Label.Create;
  Self.ToolBar := System.Windows.Forms.ToolBar.Create;
  Self.ToolBarButtonDataPacket := System.Windows.Forms.ToolBarButton.Create;
  Self.ToolBarButtonSave := System.Windows.Forms.ToolBarButton.Create;
  Self.ImageList := System.Windows.Forms.ImageList.Create(Self.components);
  Self.SaveFileDialogXml := System.Windows.Forms.SaveFileDialog.Create;
  Self.Splitter1 := System.Windows.Forms.Splitter.Create;
  Self.DataGrid := System.Windows.Forms.DataGrid.Create;
  (System.ComponentModel.ISupportInitialize(Self.DataGrid)).BeginInit;
  Self.SuspendLayout;
  // 
  // ListBoxTables
  // 
  Self.ListBoxTables.Dock := System.Windows.Forms.DockStyle.Left;
  Self.ListBoxTables.IntegralHeight := False;
  Self.ListBoxTables.Location := System.Drawing.Point.Create(0, 28);
  Self.ListBoxTables.Name := 'ListBoxTables';
  Self.ListBoxTables.Size := System.Drawing.Size.Create(96, 476);
  Self.ListBoxTables.TabIndex := 0;
  Self.ListBoxTables.Visible := False;
  Include(Self.ListBoxTables.SelectedIndexChanged, Self.ListBoxTables_SelectedIndexChanged);
  // 
  // LabelError
  // 
  Self.LabelError.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.LabelError.Location := System.Drawing.Point.Create(0, 28);
  Self.LabelError.Name := 'LabelError';
  Self.LabelError.Size := System.Drawing.Size.Create(504, 476);
  Self.LabelError.TabIndex := 1;
  Self.LabelError.Text := 'LabelError';
  Self.LabelError.TextAlign := System.Drawing.ContentAlignment.MiddleCenter;
  Self.LabelError.Visible := False;
  // 
  // ToolBar
  // 
  Self.ToolBar.Buttons.AddRange(TSystem_Windows_Forms_ToolBarButtonArray.Create(Self.ToolBarButtonDataPacket, Self.ToolBarButtonSave));
  Self.ToolBar.DropDownArrows := True;
  Self.ToolBar.ImageList := Self.ImageList;
  Self.ToolBar.Location := System.Drawing.Point.Create(0, 0);
  Self.ToolBar.Name := 'ToolBar';
  Self.ToolBar.ShowToolTips := True;
  Self.ToolBar.Size := System.Drawing.Size.Create(504, 28);
  Self.ToolBar.TabIndex := 3;
  Self.ToolBar.Wrappable := False;
  Include(Self.ToolBar.ButtonClick, Self.ToolBar_ButtonClick);
  // 
  // ToolBarButtonDataPacket
  // 
  Self.ToolBarButtonDataPacket.ImageIndex := 1;
  Self.ToolBarButtonDataPacket.ToolTipText := 'View as DATAPACKET';
  // 
  // ToolBarButtonSave
  // 
  Self.ToolBarButtonSave.ImageIndex := 0;
  Self.ToolBarButtonSave.ToolTipText := 'Save as .NET DataSet XML';
  //
  // ImageList
  // 
  Self.ImageList.ColorDepth := System.Windows.Forms.ColorDepth.Depth16Bit;
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('ImageList.ImageStream')));
  Self.ImageList.TransparentColor := System.Drawing.Color.White;
  // 
  // SaveFileDialogXml
  // 
  Self.SaveFileDialogXml.Filter := 'XML Files (*.xml)|*.xml';
  Self.SaveFileDialogXml.FilterIndex := 0;
  Self.SaveFileDialogXml.Title := 'Save as .NET DataSet XML';
  // 
  // Splitter1
  // 
  Self.Splitter1.Location := System.Drawing.Point.Create(96, 28);
  Self.Splitter1.Name := 'Splitter1';
  Self.Splitter1.Size := System.Drawing.Size.Create(3, 476);
  Self.Splitter1.TabIndex := 1;
  Self.Splitter1.TabStop := False;
  // 
  // DataGrid
  // 
  Self.DataGrid.DataMember := '';
  Self.DataGrid.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.DataGrid.HeaderForeColor := System.Drawing.SystemColors.ControlText;
  Self.DataGrid.Location := System.Drawing.Point.Create(99, 28);
  Self.DataGrid.Name := 'DataGrid';
  Self.DataGrid.ReadOnly := True;
  Self.DataGrid.Size := System.Drawing.Size.Create(405, 476);
  Self.DataGrid.TabIndex := 3;
  Self.DataGrid.Visible := False;
  // 
  // TViewDataForm
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(504, 504);
  Self.Controls.Add(Self.DataGrid);
  Self.Controls.Add(Self.Splitter1);
  Self.Controls.Add(Self.ListBoxTables);
  Self.Controls.Add(Self.LabelError);
  Self.Controls.Add(Self.ToolBar);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MinimumSize := System.Drawing.Size.Create(200, 100);
  Self.Name := 'TViewDataForm';
  Self.ShowInTaskbar := False;
  Self.Text := 'Data';
  Include(Self.Closed, Self.TViewDataForm_Closed);
  (System.ComponentModel.ISupportInitialize(Self.DataGrid)).EndInit;
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TViewDataForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TViewDataForm.Create;
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

procedure TViewDataForm.LoadXmlData(const Data: string);
begin
  FData := Data;
  FillXmlData(False);
  ToolBarButtonDataPacket.Enabled := FDataSet.Tables.Contains('DATAPACKET');
  ToolBarButtonDataPacket.Pushed := False;
  Show;
end;

procedure TViewDataForm.FillXmlData(ViewAsDataPacket: Boolean);
var
  Reader: StringReader;
  I: Integer;
begin
  Cursor.Current := Cursors.WaitCursor;
  Update;
  DataGrid.DataSource := nil;
  ListBoxTables.Items.Clear;
  if Assigned(FDataSet) then
    FDataSet.Dispose;
  FDataSet := DataSet.Create;
  Reader := StringReader.Create(FData);
  try
    try
      if ViewAsDataPacket then
        PetrVones.Utils.DataPacket.FillDataSet(FDataSet, Reader)
      else
        FDataSet.ReadXml(Reader);
      UpdateControls('');
      for I := 0 to FDataSet.Tables.Count - 1 do
        ListBoxTables.Items.Add(FDataSet.Tables[I].TableName);
      ListBoxTables.SelectedIndex := 0;
    except
      on E: Exception do
        UpdateControls(E.Message);
    end;
  finally
    Reader.Close;
  end;
  Cursor.Current := Cursors.Default;
end;

procedure TViewDataForm.TViewDataForm_Closed(sender: System.Object; e: System.EventArgs);
begin
  FViewDataForm := nil;
end;

procedure TViewDataForm.ListBoxTables_SelectedIndexChanged(sender: System.Object;
    e: System.EventArgs);
var
  TableName: string;
begin
  TableName := ListBoxTables.SelectedItem.ToString;
  DataGrid.SetDataBinding(FDataSet, TableName);
  DataGrid.CaptionText := TableName;
end;

class procedure TViewDataForm.ShowXmlData(const Data, Caption: string);
begin
  if not Assigned(FViewDataForm) then
    FViewDataForm := TViewDataForm.Create;
  FViewDataForm.Text := System.String.Format('Data - {0}', [Caption]);
  FViewDataForm.LoadXmlData(Data);
  FViewDataForm.BringToFront;
  FViewDataForm.DataGrid.Focus;
end;

procedure TViewDataForm.ToolBar_ButtonClick(sender: System.Object; e: System.Windows.Forms.ToolBarButtonClickEventArgs);
begin
  if e.Button = ToolBarButtonDataPacket then
  begin
    ToolBarButtonDataPacket.Pushed := not ToolBarButtonDataPacket.Pushed;
    FillXmlData(ToolBarButtonDataPacket.Pushed);
  end
  else
  if e.Button = ToolBarButtonSave then
    SaveXmlData;
end;

procedure TViewDataForm.UpdateControls(const ErrorMessage: string);
var
  IsError: Boolean;
begin
  SuspendLayout;
  IsError := ErrorMessage <> '';
  LabelError.Text := ErrorMessage;
  LabelError.Visible := IsError;
  ListBoxTables.Visible := not IsError;
  Splitter1.Visible := not IsError;
  DataGrid.Visible := not IsError;
  ToolBarButtonSave.Enabled := not IsError;
  ResumeLayout(True);
end;

procedure TViewDataForm.SaveXmlData;
var
  R: System.Windows.Forms.DialogResult;
begin
  SaveFileDialogXml.FileName := '';
  ShowInTaskbar := True; // Workaround for an OTA bug
  try
    R := SaveFileDialogXml.ShowDialog;
  finally
    ShowInTaskbar := False;
  end;
  if R = System.Windows.Forms.DialogResult.OK then
    FDataSet.WriteXml(SaveFileDialogXml.FileName);
end;

end.
