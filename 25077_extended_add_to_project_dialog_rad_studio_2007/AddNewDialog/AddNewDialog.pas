unit AddNewDialog;

interface

uses
  Borland.Studio.ToolsAPI,
  CodersLab.Windows.Controls,
  Options,
  RADStudioProject,
  System.Collections,
  System.Collections.Generic,
  System.Collections.ObjectModel,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms,
  Visibles.BorderExtender;

type
  TAddItem = record (IOTAModuleCreator)
  strict private
    type
      TOTAFile = class (TObject,IOTAFile)
      strict private
        FAge: DateTime;
        FSource: string;
      public
        property Age: DateTime read FAge;
        property Source: string read FSource;
        constructor Create(Path: string);
      end;
  strict private
    FProject: IOTAProject;
    FAncestorName: string;
    FExisting: Boolean;
    FFileSystem: string;
    FFormName: string;
    FIntfFileName: string;
    FMainForm: Boolean;
    FShowForm: Boolean;
    FShowSource: Boolean;
    FUnnamed: Boolean;
  public
    FilePath: string;
    DependentFilePaths: array of string;
    ClassName: string;
    DesignClassName: string;
    TweakResource: Boolean;
  public
    property Project: IOTAProject read FProject write FProject;
    constructor Create(Project: IOTAProject);
  public
    property AncestorName: string read FAncestorName;
    property FormName: string read FFormName write FFormName;
    property IntfFileName: string read FIntfFileName;
    property MainForm: Boolean read FMainForm;
    property ShowForm: Boolean read FShowForm;
    property ShowSource: Boolean read FShowSource;
    property Existing: Boolean read FExisting;
    property FileSystem: string read FFileSystem;
    property Unnamed: Boolean read FUnnamed;
    function get_Owner: IOTAModule;
    function get_CreatorType: string;
    function get_ImplFileName: string;
    function NewImplSource(moduleIdent: string; formIdent: string; ancestorIdent: string): IOTAFile;
    function NewFormFile(formIdent: string; ancestorIdent: string): IOTAFile;
    function NewIntfSource(moduleIdent: string; formIdent: string; ancestorIdent: string): IOTAFile;
  end;

  TDependentDetach = (Forbidden,AllowedDefaultNo,AllowedDefaultYes);
  TAddItemCollection = Collection<TAddItem>;

  TAddNewDialog = class (System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    components: System.ComponentModel.IContainer;
    OpenProjectDialog: System.Windows.Forms.OpenFileDialog;
    ImageList: System.Windows.Forms.ImageList;
    TBorderExtender: Visibles.BorderExtender.TBorderExtender;
    TreeView: CodersLab.Windows.Controls.TreeView;
    BtnOk: System.Windows.Forms.Button;
    BtnCancel: System.Windows.Forms.Button;
    CheckOpen: System.Windows.Forms.CheckBox;
    CheckDependent: System.Windows.Forms.CheckBox;
    procedure InitializeComponent;
    procedure TreeView_MouseUp(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure TreeView_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
    procedure TreeView_BeforeExpand(sender: System.Object; e: System.Windows.Forms.TreeViewCancelEventArgs);
    procedure TMultiSelectionDialog_Load(sender: System.Object; e: System.EventArgs);
    procedure BtnOk_Click(sender: System.Object; e: System.EventArgs);
    procedure TreeView_BeforeSelect(sender: System.Object; e: System.Windows.Forms.TreeViewCancelEventArgs);
    procedure TreeView_DoubleClick(sender: System.Object; e: System.EventArgs);
    procedure CheckOpen_Click(sender: System.Object; e: System.EventArgs);
    procedure CheckDependent_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    class var
      FOpenToo: Boolean;
  strict private
    FItems: TAddItemCollection;
    ProjectName: string;
    SearchPatterns: array of string;
    FileImageIndexes: array of Integer;
    LoadProject: Boolean;
    ApplicationSts: TApplicationSts;
    procedure CheckSelectionSts;
  public
    property Items: TAddItemCollection read FItems;
    class property OpenToo: Boolean read FOpenToo;
    constructor Create(ApplicationSts: TApplicationSts; ProjectName: string; ProjectFilter: string;
                       SearchPatterns: array of string; FileImageIndexes: array of Integer; Detach: TDependentDetach; LoadProject: Boolean = false); overload;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TAddNewDialog))]

implementation

uses
  System.IO,
  System.Text;

const
  SelectableItems: set of Byte = [ItemPasSource,ItemPasFormSource,ItemCsSource,ItemCsFormSource];

{$region 'TAddItem'}

constructor TAddItem.TOTAFile.Create(Path: string);
begin
  inherited Create;
  FSource := &File.ReadAllText(Path);
  FAge := &File.GetLastWriteTime(Path);
end;

constructor TAddItem.Create(Project: IOTAProject);
begin
  inherited Create;
  FProject := Project;
  FAncestorName := '';
  FExisting := true;
  FFileSystem := '';
  FFormName := '';
  FIntfFileName := '';
  FMainForm := false;
  FShowForm := false;
  FShowSource := false;
  FUnnamed := false;
end;

function TAddItem.get_Owner: IOTAModule;
begin
  Result := FProject;
end;

function TAddItem.get_CreatorType: string;
begin
  if not Assigned(DependentFilePaths) then
    Result := OTACreatorTypes.sUnit
  else if (FProject.Personality = OTAIDEPersonalities.sDelphiDotNetPersonality) or
          (FProject.Personality = OTAIDEPersonalities.sCSharpPersonality) then begin
    if Path.GetExtension(DependentFilePaths[0]).ToLower = '.nfm' then
      Result := OTACreatorTypes.sForm
    else
      Result := OTACreatorTypes.sWinForm
  end
  else
    Result := OTACreatorTypes.sForm;
end;

function TAddItem.get_ImplFileName: string;
begin
  Result := FilePath;
end;

function TAddItem.NewImplSource(moduleIdent, formIdent, ancestorIdent: string): IOTAFile;
begin
  Result := TOTAFile.Create(FilePath);
end;

function TAddItem.NewFormFile(formIdent, ancestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TAddItem.NewIntfSource(moduleIdent, formIdent, ancestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{$endregion}

{$REGION 'Windows Form Designer generated code'}
procedure TAddNewDialog.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  Self.components := System.ComponentModel.Container.Create;
  resources := System.Resources.ResourceManager.Create(TypeOf(TAddNewDialog));
  Self.BtnOk := System.Windows.Forms.Button.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.TreeView := CodersLab.Windows.Controls.TreeView.Create;
  Self.ImageList := System.Windows.Forms.ImageList.Create(Self.components);
  Self.OpenProjectDialog := System.Windows.Forms.OpenFileDialog.Create;
  Self.TBorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.CheckOpen := System.Windows.Forms.CheckBox.Create;
  Self.CheckDependent := System.Windows.Forms.CheckBox.Create;
  Self.SuspendLayout;
  //
  // BtnOk
  //
  Self.BtnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnOk.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnOk.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnOk.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.BtnOk.Location := System.Drawing.Point.Create(418, 336);
  Self.BtnOk.Name := 'BtnOk';
  Self.BtnOk.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnOk.TabIndex := 1;
  Self.BtnOk.Text := '&OK';
  Include(Self.BtnOk.Click, Self.BtnOk_Click);
  //
  // BtnCancel
  //
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.BtnCancel.Location := System.Drawing.Point.Create(508, 336);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCancel.TabIndex := 0;
  Self.BtnCancel.Text := '&Cancel';
  //
  // TreeView
  //
  Self.TreeView.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.TreeView.BackColor := System.Drawing.SystemColors.Window;
  Self.TBorderExtender.SetBorder3DStyle(Self.TreeView, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.TreeView.BorderStyle := System.Windows.Forms.BorderStyle.None;
  Self.TreeView.HideSelection := False;
  Self.TreeView.ImageList := Self.ImageList;
  Self.TreeView.Location := System.Drawing.Point.Create(1, 1);
  Self.TreeView.Name := 'TreeView';
  Self.TreeView.SelectionBackColor := System.Drawing.SystemColors.Highlight;
  Self.TreeView.SelectionMode := CodersLab.Windows.Controls.TreeViewSelectionMode.MultiSelect;
  Self.TreeView.Size := System.Drawing.Size.Create(600, 327);
  Self.TreeView.TabIndex := 1;
  Include(Self.TreeView.AfterCheck, Self.TreeView_AfterCheck);
  Include(Self.TreeView.BeforeSelect, Self.TreeView_BeforeSelect);
  Include(Self.TreeView.DoubleClick, Self.TreeView_DoubleClick);
  Include(Self.TreeView.MouseUp, Self.TreeView_MouseUp);
  Include(Self.TreeView.BeforeExpand, Self.TreeView_BeforeExpand);
  //
  // ImageList
  //
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('ImageList.ImageStream')));
  Self.ImageList.TransparentColor := System.Drawing.Color.Transparent;
  //
  // OpenProjectDialog
  //
  Self.OpenProjectDialog.DefaultExt := 'dproj';
  Self.OpenProjectDialog.Filter := 'Any File (*.*)|*.*';
  //
  // CheckOpen
  //
  Self.CheckOpen.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.CheckOpen.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.CheckOpen.Location := System.Drawing.Point.Create(2, 339);
  Self.CheckOpen.Name := 'CheckOpen';
  Self.CheckOpen.Size := System.Drawing.Size.Create(140, 16);
  Self.CheckOpen.TabIndex := 6;
  Self.CheckOpen.Text := 'Always open added files';
  Include(Self.CheckOpen.Click, Self.CheckOpen_Click);
  //
  // CheckDependent
  //
  Self.CheckDependent.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.CheckDependent.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.CheckDependent.Location := System.Drawing.Point.Create(150, 339);
  Self.CheckDependent.Name := 'CheckDependent';
  Self.CheckDependent.Size := System.Drawing.Size.Create(130, 16);
  Self.CheckDependent.TabIndex := 7;
  Self.CheckDependent.Text := 'Add dependent files';
  Include(Self.CheckDependent.Click, Self.CheckDependent_Click);
  //
  // TAddNewDialog
  //
  Self.AcceptButton := Self.BtnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(602, 364);
  Self.Controls.Add(Self.CheckOpen);
  Self.Controls.Add(Self.CheckDependent);
  Self.Controls.Add(Self.TreeView);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.BtnOk);
  Self.DockPadding.All := 1;
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(500, 100);
  Self.Name := 'TAddNewDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Show;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Include(Self.Load, Self.TMultiSelectionDialog_Load);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

{$region 'TAddNewDialog'}

procedure TAddNewDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    FOpenToo := CheckOpen.Checked;
    ApplicationSts.AddNewDialog.Size := Size;
    ApplicationSts.AddNewDialog.Location := Location;
    ApplicationSts.Store;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TAddNewDialog.Create(ApplicationSts: TApplicationSts; ProjectName: string; ProjectFilter: string;
                                 SearchPatterns: array of string; FileImageIndexes: array of Integer; Detach: TDependentDetach; LoadProject: Boolean);
begin
  inherited Create;
  InitializeComponent;
  OpenProjectDialog.Filter := ProjectFilter + '|' + OpenProjectDialog.Filter;
  CheckOpen.Checked := FOpenToo;
  CheckDependent.Checked := (Detach = TDependentDetach.AllowedDefaultNo);
  CheckDependent.Visible := (Detach <> TDependentDetach.Forbidden);
  Self.ApplicationSts := ApplicationSts;
  if ApplicationSts.AddNewDialog.Size.Width = 0 then
    CenterToScreen
  else begin
    Size := ApplicationSts.AddNewDialog.Size;
    Location := ApplicationSts.AddNewDialog.Location;
  end;
  FItems := TAddItemCollection.Create;
  Self.ProjectName := ProjectName;
  Self.FileImageIndexes := FileImageIndexes;
  Self.SearchPatterns := SearchPatterns;
  Self.LoadProject := LoadProject;
  CheckSelectionSts;
end;

procedure TAddNewDialog.CheckSelectionSts;
begin
  BtnOk.Enabled := (TreeView.SelectedNodes.Count > 0);
  if TreeView.SelectedNodes.Count > 0 then
    Text := System.String.Format('{0} - Add ({1} selected)',[ProjectName,TreeView.SelectedNodes.Count])
  else
    Text := System.String.Format('{0} - Add (none selected)',[ProjectName]);
end;

procedure TAddNewDialog.CheckOpen_Click(sender: System.Object; e: System.EventArgs);
begin
  FOpenToo := CheckOpen.Checked;
end;

procedure TAddNewDialog.CheckDependent_Click(sender: System.Object; e: System.EventArgs);

  procedure UpdateNodes(Nodes: TreeNodeCollection);
  var
    Node: TreeNode;
    Ext: string;
  begin
    for Node in Nodes do begin
      if Node.Nodes.Count > 0 then
        if Node.ImageIndex in SelectableItems then begin
          if CheckDependent.Checked then begin
            case Node.ImageIndex of
              ItemPasSource: begin
                Node.ImageIndex := ItemPasFormSource;
                Node.Expand;
              end;
              ItemCsSource: begin
                Node.ImageIndex := ItemCsFormSource;
                Node.Expand;
              end;
            end;
          end
          else begin
            case Node.ImageIndex of
              ItemPasFormSource: begin
                Ext := Path.GetExtension(string(Node.Nodes[0].Tag)).ToLower;
                if not ((Ext = '.dfm') or (Ext = '.nfm') or (Ext = '.aspx')) then begin
                  Node.ImageIndex := ItemPasSource;
                  Node.Collapse;
                end;
              end;
              ItemCsFormSource: begin
                Node.ImageIndex := ItemCsSource;
                Node.Collapse;
              end;
            end;
          end;
        end
        else
          UpdateNodes(Node.Nodes);
    end;
  end;

begin
  TreeView.BeginUpdate;
  UpdateNodes(TreeView.Nodes);
  TreeView.EndUpdate;
end;

procedure TAddNewDialog.TreeView_MouseUp(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
begin
  CheckSelectionSts;
end;

procedure TAddNewDialog.TreeView_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
begin
  CheckSelectionSts;
end;

procedure TAddNewDialog.TreeView_BeforeSelect(sender: System.Object; e: System.Windows.Forms.TreeViewCancelEventArgs);
var
  Node: TreeNode;
begin
  if not (e.Node.ImageIndex in SelectableItems) then
    e.Cancel := true
  else begin
    if Assigned(e.Node.Parent) then
      if e.Node.Parent.ImageIndex in SelectableItems then
        e.Cancel := true;
  end;
  TreeView.BeginUpdate;
  if e.Cancel then begin
    for Node in e.Node.Nodes do
      TreeView.SelectedNodes.Add(Node);
  end;
  TreeView.EndUpdate;
end;

procedure TAddNewDialog.TreeView_BeforeExpand(sender: System.Object; e: System.Windows.Forms.TreeViewCancelEventArgs);
begin
  e.Cancel := (e.Node.ImageIndex = ItemPasSource) or (e.Node.ImageIndex = ItemCsSource);
end;

procedure TAddNewDialog.TreeView_DoubleClick(sender: System.Object; e: System.EventArgs);
var
  Node: TreeNode;
  Ext: string;
begin
  Node := TreeView.GetNodeAt(TreeView.PointToClient(MousePosition));
  if Node.Nodes.Count > 0 then begin
    TreeView.BeginUpdate;
    case Node.ImageIndex of
      ItemPasSource: begin
        Node.ImageIndex := ItemPasFormSource;
        Node.Expand;
      end;
      ItemPasFormSource: begin
        Ext := Path.GetExtension(string(Node.Nodes[0].Tag)).ToLower;
        if not ((Ext = '.dfm') or (Ext = '.nfm') or (Ext = '.aspx')) then begin
          Node.ImageIndex := ItemPasSource;
          Node.Collapse;
        end;
      end;
      ItemCsSource: begin
        Node.ImageIndex := ItemCsFormSource;
        Node.Expand;
      end;
      ItemCsFormSource: begin
        Node.ImageIndex := ItemCsSource;
        Node.Collapse;
      end;
    end;
    TreeView.EndUpdate;
  end;
end;

procedure TAddNewDialog.BtnOk_Click(sender: System.Object; e: System.EventArgs);
var
  Node: TreeNode;
  Item: TAddItem;
  I: Integer;
begin
  FItems.Clear;
  for Node in TreeView.SelectedNodes do begin
    Item := TAddItem.Create(nil);
    Item.FilePath := string(Node.Tag);
    Item.DependentFilePaths := nil;
    if (Node.ImageIndex = ItemPasFormSource) or (Node.ImageIndex = ItemCsFormSource) then begin
      Item.DependentFilePaths := New(array [Node.Nodes.Count] of string);
      for I := 0 to Pred(Node.Nodes.Count) do
        Item.DependentFilePaths[I] := string(Node.Nodes[I].Tag);
    end;
    FItems.Add(Item);
  end;
end;

procedure TAddNewDialog.TMultiSelectionDialog_Load(sender: System.Object; e: System.EventArgs);

  function FileNameMatch(FileName: string; SearchPatterns: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := false;
    FileName := FileName.ToLower;
    for I := 1 to Pred(Length(SearchPatterns)) do
      if FileName.EndsWith(SearchPatterns[I].Substring(2).ToLower) then begin
        Result := true;
        Break;
      end;
  end;

  procedure DirectoryPopulate(Node: TreeNode; Directory: DirectoryInfo);
  var
    SubDirectory: DirectoryInfo;
    &File: FileInfo;
    DependentFileName,DependentFilePath,DependentFileExt: string;
    ChildNode,DependentChildNode: TreeNode;
    I: Integer;
  begin
    for &File in Directory.GetFiles(SearchPatterns[0]) do begin
      if not FileNameMatch(&File.Name,SearchPatterns) then begin
        ChildNode := Node.Nodes.Add(&File.Name);
        ChildNode.Tag := &File.FullName;
        ChildNode.ImageIndex := FileImageIndexes[0];
        for I := 1 to Pred(Length(SearchPatterns)) do begin
          DependentFileName := SearchPatterns[I].Replace(string('*'),Path.GetFileNameWithoutExtension(&File.Name));
          DependentFilePath := Path.GetDirectoryName(&File.FullName) + Path.DirectorySeparatorChar + DependentFileName;
          DependentFileExt := Path.GetExtension(DependentFilePath).ToLower;
          if System.IO.&File.Exists(DependentFilePath) then begin
            if CheckDependent.Checked or (DependentFileExt = '.dfm') or (DependentFileExt = '.nfm') or (DependentFileExt = '.aspx') then
              ChildNode.ImageIndex := FileImageIndexes[0] + 1;
            DependentChildNode := ChildNode.Nodes.Add(DependentFileName);
            DependentChildNode.Tag := DependentFilePath;
            DependentChildNode.ImageIndex := FileImageIndexes[I];
          end;
        end;
        if (ChildNode.ImageIndex = (FileImageIndexes[0] + 1)) and (ChildNode.Nodes.Count > 0) then
          ChildNode.Expand
        else
          ChildNode.Collapse;
      end;
    end;
    if Length(Directory.GetDirectories) > 0 then begin
      for SubDirectory in Directory.GetDirectories do begin
        ChildNode := Node.Nodes.Add(SubDirectory.Name);
        ChildNode.ImageIndex := ItemDirectory;
        DirectoryPopulate(ChildNode, SubDirectory);
      end
    end;
    if Node.Nodes.Count = 0 then
      Node.Remove;
  end;

  procedure ProjectPopulate(Node: TreeNode; FileList: List<string>);

    function CreateNodePath(Node: TreeNode; FilePath: string): TreeNode;

      function GetNode(Node: TreeNode; Token: string): TreeNode;
      var
        I: Integer;
      begin
        for I := 0 to Pred(Node.Nodes.Count) do begin
          if System.string.Compare(Node.Nodes[I].Text,Token,true) = 0 then begin
            Result := Node.Nodes[I];
            Exit;
          end;
        end;
        Result := Node.Nodes.Add(Token);
        Result.ImageIndex := ItemDirectory;
      end;

    var
      Tokens: array of string;
      I: Integer;
    begin
      Tokens := FilePath.Split([Path.DirectorySeparatorChar]);
      I := 0;
      while (I < (Length(Tokens) - 1)) do begin
        if (Tokens[I].Substring(0,2) = '..') and (Tokens[I + 1] = '..') then begin
          Tokens[I + 1] := Tokens[I + 1] + Path.DirectorySeparatorChar + Tokens[I];
          Inc(I);
        end
        else
          Break;
      end;
      Result := Node;
      while I < Length(Tokens) do begin
        Result := GetNode(Node,Tokens[I]);
        Node := Result;
        Inc(I);
      end;
    end;

  var
    FileEntry,Pattern,FilePath,FileDirectory: string;
    DependentFileName,DependentFilePath,DependentFileExt: string;
    ChildNode,DependentChildNode: TreeNode;
    I: Integer;
  begin
    if Assigned(FileList) then begin
      for FileEntry in FileList do begin
        Pattern := '*' + Path.GetExtension(FileEntry);
        if (&Array.IndexOf(SearchPatterns,Pattern.ToLower) = 0) and not FileNameMatch(Path.GetFileName(FileEntry),SearchPatterns) then begin
          if Path.IsPathRooted(FileEntry) then
            FilePath := FileEntry
          else
            FilePath := Path.GetFullPath(ApplicationSts.SearchPath + Path.DirectorySeparatorChar + FileEntry);
          FileDirectory := Path.GetDirectoryName(FileEntry);
          if FileDirectory <> '' then begin
            ChildNode := CreateNodePath(Node,FileDirectory);
            ChildNode := ChildNode.Nodes.Add(Path.GetFileName(FileEntry));
          end
          else
            ChildNode := Node.Nodes.Add(Path.GetFileName(FileEntry));
          ChildNode.Tag := FilePath;
          ChildNode.ImageIndex := FileImageIndexes[0];
          for I := 1 to Pred(Length(SearchPatterns)) do begin
            DependentFileName := SearchPatterns[I].Replace(string('*'),Path.GetFileNameWithoutExtension(ChildNode.Text));
            DependentFilePath := Path.GetDirectoryName(FilePath) + Path.DirectorySeparatorChar + DependentFileName;
            DependentFileExt := Path.GetExtension(DependentFilePath).ToLower;
            if System.IO.&File.Exists(DependentFilePath) then begin
              if CheckDependent.Checked or (DependentFileExt = '.dfm') or (DependentFileExt = '.nfm') or (DependentFileExt = '.aspx') then
                ChildNode.ImageIndex := FileImageIndexes[0] + 1;
              DependentChildNode := ChildNode.Nodes.Add(DependentFileName);
              DependentChildNode.Tag := DependentFilePath;
              DependentChildNode.ImageIndex := FileImageIndexes[I];
            end;
          end;
          if (ChildNode.ImageIndex = (FileImageIndexes[0] + 1)) and (ChildNode.Nodes.Count > 0) then
            ChildNode.Expand
          else
            ChildNode.Collapse;
        end;
      end;
    end;
  end;

var
  RootNode: TreeNode;
  RADStudioProject: TRADStudioProject;
  ItemGroup: ProjectItemGroup;
  DCCReference: ProjectItemGroupDCCReference;
  Compile: ProjectItemGroupCompile;
  FileList: List<string>;
begin
  if not LoadProject then with FolderBrowserDialog.Create do begin
    SelectedPath := ApplicationSts.SearchPath;
    BtnOk.Enabled := false;
    if ShowDialog(Self) = System.Windows.Forms.DialogResult.OK then begin
      try
        Cursor.Current := Cursors.WaitCursor;
        TreeView.BeginUpdate;
        try
          ApplicationSts.SearchPath := SelectedPath;
          RootNode := TreeView.Nodes.Add(ApplicationSts.SearchPath);
          RootNode.ImageIndex := ItemDirectory;
          DirectoryPopulate(RootNode,DirectoryInfo.Create(ApplicationSts.SearchPath));
          TreeView.ExpandAll;
          RootNode.EnsureVisible;
        finally
          TreeView.EndUpdate;
          Cursor.Current := Cursors.Default;
        end;
      except
        ;
      end;
    end
    else
      DialogResult := System.Windows.Forms.DialogResult.Cancel;
    Free;
  end
  else begin
    if OpenProjectDialog.ShowDialog(Self) = System.Windows.Forms.DialogResult.OK then begin
      try
        Cursor.Current := Cursors.WaitCursor;
        FileList := List<string>.Create;
        TreeView.BeginUpdate;
        try
          ApplicationSts.SearchPath := Path.GetFullPath(OpenProjectDialog.FileName);
          RADStudioProject := TRADStudioProject.LoadFrom(ApplicationSts.SearchPath) as TRADStudioProject;
          ApplicationSts.SearchPath := Path.GetDirectoryName(ApplicationSts.SearchPath);
          RootNode := TreeView.Nodes.Add(ApplicationSts.SearchPath);
          RootNode.ImageIndex := ItemProject;
          for ItemGroup in RADStudioProject.ItemGroup do begin
            if Assigned(ItemGroup.DCCReference) then begin
              for DCCReference in ItemGroup.DCCReference do
                FileList.Add(DCCReference.Include);
              Break;
            end
            else if Assigned(ItemGroup.Compile) then begin
              for Compile in ItemGroup.Compile do
                FileList.Add(Compile.Include);
              Break;
            end;
          end;
          ProjectPopulate(RootNode,FileList);
          TreeView.ExpandAll;
          RootNode.EnsureVisible;
        finally
          TreeView.EndUpdate;
          Cursor.Current := Cursors.Default;
        end;
      except
        ;
      end;
    end
    else
      DialogResult := System.Windows.Forms.DialogResult.Cancel;
  end;
  CheckSelectionSts;
end;

{$endregion}

end.
