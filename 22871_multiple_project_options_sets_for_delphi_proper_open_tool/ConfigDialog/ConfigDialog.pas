unit ConfigDialog;

interface

uses
  Borland.Studio.ToolsAPI,
  Options,
  ProjectOptionsSets,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Windows.Forms,
  Visibles.BorderExtender;

type
  TOptionNode = class (TreeNode)
  public
    Name: string;
    Value: TObject;
    FormatString: string;
    procedure Format; overload;
    constructor Create; overload;
    constructor Create(Text: string); overload;
  end;

  TConfigDialog = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    Components: System.ComponentModel.Container;
    BtnCancel: System.Windows.Forms.Button;
    BtnOk: System.Windows.Forms.Button;
    Panel: System.Windows.Forms.Panel;
    BorderExtender: Visibles.BorderExtender.TBorderExtender;
    TabControl: System.Windows.Forms.TabControl;
    TabPageIgnore: System.Windows.Forms.TabPage;
    TabPageChange: System.Windows.Forms.TabPage;
    TreeChange: System.Windows.Forms.TreeView;
    TreeIgnore: System.Windows.Forms.TreeView;
    ContextMenu: System.Windows.Forms.ContextMenu;
    ItemFetch: System.Windows.Forms.MenuItem;
    ItemAddCondition: System.Windows.Forms.MenuItem;
    ItemRemoveCondition: System.Windows.Forms.MenuItem;
    procedure InitializeComponent;
    procedure BtnOk_Click(sender: System.Object; e: System.EventArgs);
    procedure TreeIgnore_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
    procedure TreeChange_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
    procedure ItemFetch_Click(sender: System.Object; e: System.EventArgs);
    procedure ContextMenu_Popup(sender: System.Object; e: System.EventArgs);
    procedure ItemAddCondition_Click(sender: System.Object; e: System.EventArgs);
    procedure ItemRemoveCondition_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    class var
      Size_1: Size;
      Location_1: Point;
  strict private
    OTAOptions: IOTAProjectOptions;
    Options: TOptions;
  public
    constructor Create(OTAOptions: IOTAProjectOptions; PluginOptions: TOptions);
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TConfigDialog))]

implementation

uses
  OptionsList;

{$R 'ConfigDialog\ConfigDialog.TConfigDialog.resources' 'ConfigDialog\ConfigDialog.resx'}

procedure TOptionNode.Format;
begin
  try
    Text := System.string.Format(FormatString,[Name,Value.ToString]);
  except
    Text := System.string.Format(FormatString,[Name,'<unassigned>']);
  end;
end;

constructor TOptionNode.Create;
begin
  inherited Create;
  FormatString := '{0} - [{1}]';
end;

constructor TOptionNode.Create(Text: string);
begin
  inherited Create(Text);
  FormatString := '{0} - [{1}]';
end;

{$REGION 'Windows Form Designer generated code'}
procedure TConfigDialog.InitializeComponent;
type
  TArrayOfSystem_Windows_Forms_MenuItem = array of System.Windows.Forms.MenuItem;
begin
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.BtnOk := System.Windows.Forms.Button.Create;
  Self.Panel := System.Windows.Forms.Panel.Create;
  Self.TabControl := System.Windows.Forms.TabControl.Create;
  Self.TabPageIgnore := System.Windows.Forms.TabPage.Create;
  Self.TreeIgnore := System.Windows.Forms.TreeView.Create;
  Self.TabPageChange := System.Windows.Forms.TabPage.Create;
  Self.TreeChange := System.Windows.Forms.TreeView.Create;
  Self.ContextMenu := System.Windows.Forms.ContextMenu.Create;
  Self.ItemFetch := System.Windows.Forms.MenuItem.Create;
  Self.ItemAddCondition := System.Windows.Forms.MenuItem.Create;
  Self.ItemRemoveCondition := System.Windows.Forms.MenuItem.Create;
  Self.BorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.Panel.SuspendLayout;
  Self.TabControl.SuspendLayout;
  Self.TabPageIgnore.SuspendLayout;
  Self.TabPageChange.SuspendLayout;
  Self.SuspendLayout;
  //
  // BtnCancel
  //
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BorderExtender.SetBorder3DStyle(Self.BtnCancel, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnCancel.Location := System.Drawing.Point.Create(194, 371);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCancel.TabIndex := 8;
  Self.BtnCancel.Text := '&Cancel';
  //
  // BtnOk
  //
  Self.BtnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnOk.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BorderExtender.SetBorder3DStyle(Self.BtnOk, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnOk.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnOk.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnOk.Location := System.Drawing.Point.Create(104, 371);
  Self.BtnOk.Name := 'BtnOk';
  Self.BtnOk.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnOk.TabIndex := 9;
  Self.BtnOk.Text := '&OK';
  Include(Self.BtnOk.Click, Self.BtnOk_Click);
  //
  // Panel
  //
  Self.Panel.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BorderExtender.SetBorder3DStyle(Self.Panel, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.Panel.Controls.Add(Self.TabControl);
  Self.Panel.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel.Name := 'Panel';
  Self.Panel.Size := System.Drawing.Size.Create(288, 364);
  Self.Panel.TabIndex := 10;
  //
  // TabControl
  //
  Self.BorderExtender.SetBorder3DStyle(Self.TabControl, System.Windows.Forms.Border3DStyle.Adjust);
  Self.TabControl.Controls.Add(Self.TabPageIgnore);
  Self.TabControl.Controls.Add(Self.TabPageChange);
  Self.TabControl.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.TabControl.Location := System.Drawing.Point.Create(0, 0);
  Self.TabControl.Name := 'TabControl';
  Self.TabControl.SelectedIndex := 0;
  Self.TabControl.Size := System.Drawing.Size.Create(288, 364);
  Self.TabControl.TabIndex := 0;
  //
  // TabPageIgnore
  //
  Self.BorderExtender.SetBorder3DStyle(Self.TabPageIgnore, System.Windows.Forms.Border3DStyle.Adjust);
  Self.TabPageIgnore.Controls.Add(Self.TreeIgnore);
  Self.TabPageIgnore.DockPadding.All := 1;
  Self.TabPageIgnore.Location := System.Drawing.Point.Create(4, 22);
  Self.TabPageIgnore.Name := 'TabPageIgnore';
  Self.TabPageIgnore.Size := System.Drawing.Size.Create(280, 338);
  Self.TabPageIgnore.TabIndex := 0;
  Self.TabPageIgnore.Text := 'Ignore in Sets';
  //
  // TreeIgnore
  //
  Self.TreeIgnore.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.TreeIgnore, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.TreeIgnore.BorderStyle := System.Windows.Forms.BorderStyle.None;
  Self.TreeIgnore.CheckBoxes := True;
  Self.TreeIgnore.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.TreeIgnore.HideSelection := False;
  Self.TreeIgnore.ImageIndex := -1;
  Self.TreeIgnore.Location := System.Drawing.Point.Create(1, 1);
  Self.TreeIgnore.Name := 'TreeIgnore';
  Self.TreeIgnore.SelectedImageIndex := -1;
  Self.TreeIgnore.ShowRootLines := False;
  Self.TreeIgnore.Size := System.Drawing.Size.Create(278, 336);
  Self.TreeIgnore.TabIndex := 0;
  Include(Self.TreeIgnore.AfterCheck, Self.TreeIgnore_AfterCheck);
  //
  // TabPageChange
  //
  Self.BorderExtender.SetBorder3DStyle(Self.TabPageChange, System.Windows.Forms.Border3DStyle.Adjust);
  Self.TabPageChange.Controls.Add(Self.TreeChange);
  Self.TabPageChange.DockPadding.All := 1;
  Self.TabPageChange.Location := System.Drawing.Point.Create(4, 22);
  Self.TabPageChange.Name := 'TabPageChange';
  Self.TabPageChange.Size := System.Drawing.Size.Create(280, 338);
  Self.TabPageChange.TabIndex := 1;
  Self.TabPageChange.Text := 'Change for Release Set';
  //
  // TreeChange
  //
  Self.TreeChange.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.TreeChange, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.TreeChange.BorderStyle := System.Windows.Forms.BorderStyle.None;
  Self.TreeChange.CheckBoxes := True;
  Self.TreeChange.ContextMenu := Self.ContextMenu;
  Self.TreeChange.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.TreeChange.HideSelection := False;
  Self.TreeChange.ImageIndex := -1;
  Self.TreeChange.Location := System.Drawing.Point.Create(1, 1);
  Self.TreeChange.Name := 'TreeChange';
  Self.TreeChange.SelectedImageIndex := -1;
  Self.TreeChange.Size := System.Drawing.Size.Create(278, 336);
  Self.TreeChange.TabIndex := 0;
  Include(Self.TreeChange.AfterCheck, Self.TreeChange_AfterCheck);
  //
  // ContextMenu
  //
  Self.ContextMenu.MenuItems.AddRange(TArrayOfSystem_Windows_Forms_MenuItem.Create(Self.ItemFetch,
          Self.ItemAddCondition, Self.ItemRemoveCondition));
  Include(Self.ContextMenu.Popup, Self.ContextMenu_Popup);
  //
  // ItemFetch
  //
  Self.ItemFetch.Index := 0;
  Self.ItemFetch.Text := 'Fetch Current &Value';
  Include(Self.ItemFetch.Click, Self.ItemFetch_Click);
  //
  // ItemAddCondition
  //
  Self.ItemAddCondition.Index := 1;
  Self.ItemAddCondition.Text := '&Add Condition Value';
  Include(Self.ItemAddCondition.Click, Self.ItemAddCondition_Click);
  //
  // ItemRemoveCondition
  //
  Self.ItemRemoveCondition.Index := 2;
  Self.ItemRemoveCondition.Text := '&Remove Condition Value';
  Include(Self.ItemRemoveCondition.Click, Self.ItemRemoveCondition_Click);
  //
  // TConfigDialog
  //
  Self.AcceptButton := Self.BtnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(288, 399);
  Self.Controls.Add(Self.Panel);
  Self.Controls.Add(Self.BtnOk);
  Self.Controls.Add(Self.BtnCancel);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.Name := 'TConfigDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Show;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'Project Options Sets - Configuration';
  Self.Panel.ResumeLayout(False);
  Self.TabControl.ResumeLayout(False);
  Self.TabPageIgnore.ResumeLayout(False);
  Self.TabPageChange.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TConfigDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    Size_1 := Size;
    Location_1 := Location;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TConfigDialog.Create(OTAOptions: IOTAProjectOptions; PluginOptions: TOptions);
var
  OptionNames: IOTAOptionNames;
  OptionI,ReleaseOptionI: Integer;
  Node,ChildNode: TOptionNode;
  ReleaseOption: TReleaseProjectOption;
  Option: TProjectOption;
begin
  inherited Create;
  InitializeComponent;
  TreeIgnore.BackColor := BorderExtender.TweakColor(TreeIgnore.BackColor,10);
  TreeChange.BackColor := TreeIgnore.BackColor;
  if Size_1.Width = 0 then
    CenterToScreen
  else begin
    Size := Size_1;
    Location := Location_1;
  end;
  Self.OTAOptions := OTAOptions;
  Options := PluginOptions;
  OptionNames := OTAOptions.OptionNames;
  TreeIgnore.BeginUpdate;
  TreeChange.BeginUpdate;
  for OptionI := 0 to Pred(OptionNames.Count) do begin
    Node := TOptionNode.Create(OptionNames.GetName(OptionI));
    Node.Name := Node.Text;
    Node.Value := OTAOptions.GetOptionValue(Node.Name);
    Node.Format;
    Node.Checked := Options.HashedIgnoreOptions.Contains(Node.Name.ToUpper);
    TreeIgnore.Nodes.Add(Node);
    Node := TOptionNode.Create(Node.Name);
    Node.Name := Node.Text;
    Node.Value := OTAOptions.GetOptionValue(Node.Name);
    Node.Format;
    TreeChange.Nodes.Add(Node);
    if Options.HashedReleaseValues.Contains(Node.Name.ToUpper) then begin
      Node.Checked := true;
      ReleaseOptionI := Integer(Options.HashedReleaseValues[Node.Name.ToUpper]);
      ReleaseOption := Options.ReleaseValues[ReleaseOptionI] as TReleaseProjectOption;
      Node.Value := ReleaseOption.Value;
      Node.FormatString := '{0} - [Shall be: {1}]';
      Node.Format;
      for ReleaseOptionI := 0 to Pred(ReleaseOption.Conditions.Count) do begin
        Option := ReleaseOption.Conditions[ReleaseOptionI] as TProjectOption;
        ChildNode := TOptionNode.Create;
        ChildNode.Name := Option.Name;
        ChildNode.Value := Option.Value;
        ChildNode.FormatString := '{0} - [Is to be: {1}]';
        ChildNode.Format;
        ChildNode.Checked := true;
        Node.Nodes.Add(ChildNode);
      end;
    end;
  end;
  TreeIgnore.Sorted := true;
  TreeChange.Sorted := true;
  TreeIgnore.EndUpdate;
  TreeChange.EndUpdate;
  BtnOk.Enabled := false;
end;

procedure TConfigDialog.ContextMenu_Popup(sender: System.Object; e: System.EventArgs);
begin
  if not Assigned(TreeChange.SelectedNode) then begin
    ItemFetch.Enabled := false;
    ItemAddCondition.Enabled := false;
    ItemRemoveCondition.Enabled := false;
  end
  else begin
    ItemFetch.Enabled := TreeChange.SelectedNode.Checked;
    ItemAddCondition.Enabled := ItemFetch.Enabled and not Assigned(TreeChange.SelectedNode.Parent);
    ItemRemoveCondition.Enabled := ItemFetch.Enabled and Assigned(TreeChange.SelectedNode.Parent);
  end;
end;

procedure TConfigDialog.TreeIgnore_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
begin
  BtnOk.Enabled := true;
end;

procedure TConfigDialog.TreeChange_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
var
  Value_1: TObject;
begin
  if not Assigned(e.Node.Parent) then begin
    BtnOk.Enabled := true;
    if not e.Node.Checked then with e.Node as TOptionNode do begin
      Value_1 := Value;
      Value := OTAOptions.GetOptionValue(Name);
      FormatString := '{0} - [{1}]';
      Format;
      Value := Value_1;
    end
    else with e.Node as TOptionNode do begin
      FormatString := '{0} - [Shall be: {1}]';
      Format;
    end;
  end
  else if not e.Node.Checked then
    e.Node.Checked := true;
end;

procedure TConfigDialog.ItemFetch_Click(sender: System.Object; e: System.EventArgs);
var
  Node: TOptionNode;
begin
  Node := TreeChange.SelectedNode as TOptionNode;
  try
    Node.Value := OTAOptions.GetOptionValue(Node.Name);
  except
    Node.Value := nil;
  end;
  Node.Format;
  BtnOk.Enabled := true;
end;

procedure TConfigDialog.ItemAddCondition_Click(sender: System.Object; e: System.EventArgs);
var
  Option: TObject;
  ChildNode: TOptionNode;
begin
  with TOptionsList.Create(OTAOptions) do begin
    Location := MousePosition;
    if ShowDialog = System.Windows.Forms.DialogResult.OK then begin
      for Option in GetSelectedOptions do begin
        ChildNode := TOptionNode.Create(string(Option));
        ChildNode.Name := ChildNode.Text;
        try
          ChildNode.Value := OTAOptions.GetOptionValue(ChildNode.Name);
        except
          ChildNode.Value := nil;
        end;
        ChildNode.FormatString := '{0} - [Is to be: {1}]';
        ChildNode.Format;
        ChildNode.Checked := true;
        TreeChange.SelectedNode.Nodes.Add(ChildNode);
      end;
    end;
    Free;
  end;
end;

procedure TConfigDialog.ItemRemoveCondition_Click(sender: System.Object; e: System.EventArgs);
begin
  TreeChange.Nodes.Remove(TreeChange.SelectedNode);
  BtnOk.Enabled := true;
end;

procedure TConfigDialog.BtnOk_Click(sender: System.Object; e: System.EventArgs);
var
  ReleaseOption: TReleaseProjectOption;
  Option: TProjectOption;
  Node: TOptionNode;
  I,J: Integer;
  ABoolean: Boolean;
begin
  ABoolean := Options.TrayAreaInjector;
  Options.Clear;
  Options.TrayAreaInjector := ABoolean;
  for I := 0 to Pred(TreeIgnore.Nodes.Count) do
    if TreeIgnore.Nodes[I].Checked then
      Options.IgnoreOptions.Add((TreeIgnore.Nodes[I] as TOptionNode).Name);
  for I := 0 to Pred(TreeChange.Nodes.Count) do
    if TreeChange.Nodes[I].Checked then begin
      Node := TreeChange.Nodes[I] as TOptionNode;
      ReleaseOption := TReleaseProjectOption.Create;
      ReleaseOption.Name := Node.Name;
      ReleaseOption.Value := Node.Value;
      for J := 0 to Pred(Node.Nodes.Count) do begin
        Option := TProjectOption.Create;
        with Node.Nodes[J] as TOptionNode do begin
          Option.Name := Name;
          Option.Value := Value;
        end;
        ReleaseOption.Conditions.Add(Option);
      end;
      Options.ReleaseValues.Add(ReleaseOption);
    end;
  Options.HashRefresh;
end;

end.
