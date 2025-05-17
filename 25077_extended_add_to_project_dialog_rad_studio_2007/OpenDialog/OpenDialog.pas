unit OpenDialog;

interface

uses
  Borland.Studio.ToolsAPI,
  Options,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms,
  Visibles.BorderExtender;

type
  IOTAModuleInfoArray = array of IOTAModuleInfo;
  
  TOpenDialog = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    components: System.ComponentModel.IContainer;
    TBorderExtender: Visibles.BorderExtender.TBorderExtender;
    BtnCancel: System.Windows.Forms.Button;
    BtnOk: System.Windows.Forms.Button;
    ListView: System.Windows.Forms.ListView;
    ImageList: System.Windows.Forms.ImageList;
    ColumnFile: System.Windows.Forms.ColumnHeader;
    ColumnPath: System.Windows.Forms.ColumnHeader;
    CheckSorted: System.Windows.Forms.CheckBox;
    procedure InitializeComponent;
    procedure BtnOk_Click(sender: System.Object; e: System.EventArgs);
    procedure TOpenDialog_Load(sender: System.Object; e: System.EventArgs);
    procedure CheckSorted_Click(sender: System.Object; e: System.EventArgs);
    procedure ListView_Click(sender: System.Object; e: System.EventArgs);
    procedure ListView_Resize(sender: System.Object; e: EventArgs);
    procedure ListView_ColumnWidthChanged(sender: System.Object; e: ColumnWidthChangedEventArgs);
    procedure ListView_DoubleClick(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    FSelectedModules: IOTAModuleInfoArray;
    ProjectName: string;
    Extensions: array of string;
    Modules: IOTAModuleInfoArray;
    FileImageIndex: Integer;
    ApplicationSts: TApplicationSts;
    procedure CheckSelectionSts;
  public
    property SelectedModules: IOTAModuleInfoArray read FSelectedModules;
    constructor Create(ApplicationSts: TApplicationSts; ProjectName: string; Extensions: array of string; Modules: IOTAModuleInfoArray; FileImageIndex: Integer); overload;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TOpenDialog))]

implementation

uses
  System.IO;

{$REGION 'Windows Form Designer generated code'}
procedure TOpenDialog.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_ColumnHeader = array of System.Windows.Forms.ColumnHeader;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TOpenDialog));
  Self.TBorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.BtnOk := System.Windows.Forms.Button.Create;
  Self.ListView := System.Windows.Forms.ListView.Create;
  Self.ColumnFile := System.Windows.Forms.ColumnHeader.Create;
  Self.ColumnPath := System.Windows.Forms.ColumnHeader.Create;
  Self.ImageList := System.Windows.Forms.ImageList.Create(Self.components);
  Self.CheckSorted := System.Windows.Forms.CheckBox.Create;
  Self.SuspendLayout;
  //
  // BtnCancel
  //
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.TBorderExtender.SetBorder3DStyle(Self.BtnCancel, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.BtnCancel.Location := System.Drawing.Point.Create(448, 336);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCancel.TabIndex := 2;
  Self.BtnCancel.Text := '&Cancel';
  //
  // BtnOk
  //
  Self.BtnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnOk.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.TBorderExtender.SetBorder3DStyle(Self.BtnOk, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnOk.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnOk.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.BtnOk.Location := System.Drawing.Point.Create(358, 336);
  Self.BtnOk.Name := 'BtnOk';
  Self.BtnOk.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnOk.TabIndex := 3;
  Self.BtnOk.Text := '&OK';
  Include(Self.BtnOk.Click, Self.BtnOk_Click);
  //
  // ListView
  //
  Self.ListView.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.TBorderExtender.SetBorder3DStyle(Self.ListView, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.ListView.BorderStyle := System.Windows.Forms.BorderStyle.None;
  Self.ListView.Columns.AddRange(TArrayOfSystem_Windows_Forms_ColumnHeader.Create(Self.ColumnFile,
          Self.ColumnPath));
  Self.ListView.HeaderStyle := System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
  Self.ListView.HideSelection := False;
  Self.ListView.Location := System.Drawing.Point.Create(1, 1);
  Self.ListView.Name := 'ListView';
  Self.ListView.Size := System.Drawing.Size.Create(540, 327);
  Self.ListView.SmallImageList := Self.ImageList;
  Self.ListView.TabIndex := 4;
  Self.ListView.View := System.Windows.Forms.View.Details;
  Include(Self.ListView.Click, Self.ListView_Click);
  Include(Self.ListView.Resize,Self.ListView_Resize);
  Include(Self.ListView.ColumnWidthChanged,Self.ListView_ColumnWidthChanged);
  Include(Self.ListView.DoubleClick, Self.ListView_DoubleClick);
  //
  // ColumnFile
  //
  Self.ColumnFile.Text := 'File';
  Self.ColumnFile.Width := 240;
  //
  // ColumnPath
  //
  Self.ColumnPath.Text := 'Path';
  Self.ColumnPath.Width := 300;
  //
  // ImageList
  //
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('I' +
    'mageList.ImageStream')));
  Self.ImageList.TransparentColor := System.Drawing.Color.Transparent;
  //
  // CheckSorted
  //
  Self.CheckSorted.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.TBorderExtender.SetBorder3DStyle(Self.CheckSorted, System.Windows.Forms.Border3DStyle.Adjust);
  Self.CheckSorted.Checked := True;
  Self.CheckSorted.CheckState := System.Windows.Forms.CheckState.Checked;
  Self.CheckSorted.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.CheckSorted.Location := System.Drawing.Point.Create(2, 339);
  Self.CheckSorted.Name := 'CheckSorted';
  Self.CheckSorted.Size := System.Drawing.Size.Create(60, 16);
  Self.CheckSorted.TabIndex := 5;
  Self.CheckSorted.Text := 'Sorted';
  //
  // TOpenDialog
  //
  Self.AcceptButton := Self.BtnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(542, 364);
  Self.Controls.Add(Self.CheckSorted);
  Self.Controls.Add(Self.ListView);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.BtnOk);
  Self.DockPadding.All := 1;
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(300, 100);
  Self.Name := 'TOpenDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Show;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Include(Self.Load, Self.TOpenDialog_Load);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

{$region 'TOpenDialog'}

procedure TOpenDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    ApplicationSts.OpenDialog.Sorted := CheckSorted.Checked;
    ApplicationSts.OpenDialog.ColumnFileWidth := ListView.Columns[0].Width;
    ApplicationSts.OpenDialog.Size := Size;
    ApplicationSts.OpenDialog.Location := Location;
    ApplicationSts.Store;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TOpenDialog.Create(ApplicationSts: TApplicationSts; ProjectName: string; Extensions: array of string; Modules: IOTAModuleInfoArray; FileImageIndex: Integer);
begin
  inherited Create;
  InitializeComponent;
  Self.ApplicationSts := ApplicationSts;
  FSelectedModules := New(IOTAModuleInfoArray,0);
  if ApplicationSts.OpenDialog.Size.Width = 0 then
    CenterToScreen
  else begin
    CheckSorted.Checked := ApplicationSts.OpenDialog.Sorted;
    Size := ApplicationSts.OpenDialog.Size;
    Location := ApplicationSts.OpenDialog.Location;
    ListView.Columns[0].Width := ApplicationSts.OpenDialog.ColumnFileWidth;
    ListView_Resize(nil,nil);
  end;
  if CheckSorted.Checked then
    ListView.Sorting := SortOrder.Ascending
  else
    ListView.Sorting := SortOrder.None;
  Include(Self.CheckSorted.Click, Self.CheckSorted_Click);
  Self.ProjectName := ProjectName;
  Self.Extensions := Extensions;
  Self.Modules := Modules;
  Self.FileImageIndex := FileImageIndex;
  CheckSelectionSts;
end;

procedure TOpenDialog.CheckSelectionSts;
begin
  BtnOk.Enabled := (ListView.SelectedItems.Count > 0);
  if ListView.SelectedItems.Count > 0 then
    Text := System.String.Format('{0} - Open ({1} selected)',[ProjectName,ListView.SelectedItems.Count])
  else
    Text := System.String.Format('{0} - Open (none selected)',[ProjectName]);
end;

procedure TOpenDialog.CheckSorted_Click(sender: System.Object; e: System.EventArgs);
begin
  if CheckSorted.Checked then
    ListView.Sorting := SortOrder.Ascending
  else
    ListView.Sorting := SortOrder.None;
  TOpenDialog_Load(nil,nil);
end;

procedure TOpenDialog.ListView_Resize(sender: TObject; e: EventArgs);
begin
  ListView.Columns[1].Width := ListView.ClientSize.Width - ListView.Columns[0].Width - 2;
end;

procedure TOpenDialog.ListView_ColumnWidthChanged(sender: TObject; e: ColumnWidthChangedEventArgs);
begin
  Exclude(Self.ListView.ColumnWidthChanged,Self.ListView_ColumnWidthChanged);
  ListView.Columns[1].Width := ListView.ClientSize.Width - ListView.Columns[0].Width - 1;
  Include(Self.ListView.ColumnWidthChanged,Self.ListView_ColumnWidthChanged);
end;

procedure TOpenDialog.ListView_Click(sender: System.Object; e: System.EventArgs);
begin
  CheckSelectionSts;
end;

procedure TOpenDialog.ListView_DoubleClick(sender: System.Object; e: System.EventArgs);
begin
  if BtnOk.Enabled then begin
    BtnOk_Click(nil,nil);
    DialogResult := BtnOk.DialogResult;
  end;
end;

procedure TOpenDialog.BtnOk_Click(sender: System.Object; e: System.EventArgs);
var
  I: Integer;
begin
  FSelectedModules := New(IOTAModuleInfoArray,ListView.SelectedItems.Count);
  for I := 0 to Pred(ListView.SelectedItems.Count) do
    FSelectedModules[I] := ListView.SelectedItems[I].Tag as IOTAModuleInfo;
end;

procedure TOpenDialog.TOpenDialog_Load(sender: System.Object; e: System.EventArgs);
var
  Module: IOTAModuleInfo;
  Item: ListViewItem;
  I: Integer;
  Extension,FileName: string;
begin
  try
    Cursor.Current := Cursors.WaitCursor;
    ListView.BeginUpdate;
    ListView.Items.Clear;
    try
      for Module in Modules do begin
        FileName := Module.FileName;
        if Assigned(FileName) then
          Extension := Path.GetExtension(FileName)
        else
          Extension := '';
        I := &Array.IndexOf(Extensions,Extension.ToLower);
        if I >= 0 then begin
          Item := ListViewItem.Create(Module.Name + Extensions[I]);
          Item.SubItems.Add(FileName);
          Item.Tag := Module;
          Item.ImageIndex := FileImageIndex;
          ListView.Items.Add(Item);
        end;
      end;
      if ListView.Items.Count > 0 then
        ListView.Items[0].EnsureVisible;
    finally
      ListView.EndUpdate;
      Cursor.Current := Cursors.Default;
    end;
  except
    ;
  end;
  CheckSelectionSts;
end;

{$endregion}

end.
