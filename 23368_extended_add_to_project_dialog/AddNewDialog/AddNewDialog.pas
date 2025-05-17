unit AddNewDialog;

interface

uses
  BorlandProjectSettings,
  CodersLab.Windows.Controls,
  Options,
  System.Collections,
  System.Collections.Specialized,
  System.ComponentModel,
  System.Drawing,
  System.Resources,
  System.Windows.Forms,
  Visibles.BorderExtender;

type
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
    procedure InitializeComponent;
    procedure TreeView_MouseUp(sender: System.Object; e: System.Windows.Forms.MouseEventArgs);
    procedure TreeView_AfterCheck(sender: System.Object; e: System.Windows.Forms.TreeViewEventArgs);
    procedure TMultiSelectionDialog_Load(sender: System.Object; e: System.EventArgs);
    procedure BtnOk_Click(sender: System.Object; e: System.EventArgs);
    procedure TreeView_BeforeSelect(sender: System.Object; e: System.Windows.Forms.TreeViewCancelEventArgs);
    procedure TreeView_DoubleClick(sender: System.Object; e: System.EventArgs);
    procedure CheckOpen_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    class var
      FOpenToo: Boolean;
  strict private
    FFilePaths: StringCollection;
    ProjectName: string;
    SearchPatterns: array of string;
    FileImageIndex: Integer;
    LoadProject: Boolean;
    ApplicationSts: TApplicationSts;
    procedure CheckSelectionSts;
    constructor Create; overload;
  public
    class function get_OpenToo: Boolean; static;
  public
    property FilePaths: StringCollection read FFilePaths;
    class property OpenToo: Boolean read get_OpenToo;
    constructor Create(ApplicationSts: TApplicationSts; ProjectName: string; SearchPatterns: array of string; FileImageIndex: Integer; LoadProject: Boolean = false); overload;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TAddNewDialog))]

implementation

uses
  System.IO;

{$R 'AddNewDialog\AddNewDialog.TAddNewDialog.resources' 'AddNewDialog\AddNewDialog.resx'}

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
  Self.SuspendLayout;
  // 
  // BtnOk
  // 
  Self.BtnOk.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnOk.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnOk.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnOk.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.BtnOk.Location := System.Drawing.Point.Create(268, 336);
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
  Self.BtnCancel.Location := System.Drawing.Point.Create(358, 336);
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
  Self.TreeView.Size := System.Drawing.Size.Create(450, 327);
  Self.TreeView.TabIndex := 1;
  Include(Self.TreeView.AfterCheck, Self.TreeView_AfterCheck);
  Include(Self.TreeView.BeforeSelect, Self.TreeView_BeforeSelect);
  Include(Self.TreeView.DoubleClick, Self.TreeView_DoubleClick);
  Include(Self.TreeView.MouseUp, Self.TreeView_MouseUp);
  // 
  // ImageList
  // 
  Self.ImageList.ImageSize := System.Drawing.Size.Create(16, 16);
  Self.ImageList.ImageStream := (System.Windows.Forms.ImageListStreamer(resources.GetObject('I' +
    'mageList.ImageStream')));
  Self.ImageList.TransparentColor := System.Drawing.Color.Transparent;
  // 
  // OpenProjectDialog
  // 
  Self.OpenProjectDialog.DefaultExt := 'bdsproj';
  Self.OpenProjectDialog.Filter := 'BDS Project files (*.bdsproj)|*.bdsproj|' +
  'All files (*.*)|*.*';
  // 
  // CheckOpen
  // 
  Self.CheckOpen.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.CheckOpen.FlatStyle := System.Windows.Forms.FlatStyle.Popup;
  Self.CheckOpen.Location := System.Drawing.Point.Create(2, 339);
  Self.CheckOpen.Name := 'CheckOpen';
  Self.CheckOpen.Size := System.Drawing.Size.Create(170, 16);
  Self.CheckOpen.TabIndex := 6;
  Self.CheckOpen.Text := 'Open even if source only file';
  Include(Self.CheckOpen.Click, Self.CheckOpen_Click);
  // 
  // TAddNewDialog
  // 
  Self.AcceptButton := Self.BtnOk;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(452, 364);
  Self.Controls.Add(Self.CheckOpen);
  Self.Controls.Add(Self.TreeView);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.BtnOk);
  Self.DockPadding.All := 1;
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(300, 100);
  Self.Name := 'TAddNewDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Show;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Include(Self.Load, Self.TMultiSelectionDialog_Load);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

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

constructor TAddNewDialog.Create;
begin
  inherited Create;
  InitializeComponent;
  CheckOpen.Checked := FOpenToo;
  ApplicationSts := TApplicationSts.Load;
  FFilePaths := StringCollection.Create;
  Self.FileImageIndex := 1;
  CheckSelectionSts;
end;

constructor TAddNewDialog.Create(ApplicationSts: TApplicationSts; ProjectName: string; SearchPatterns: array of string; FileImageIndex: Integer; LoadProject: Boolean);
begin
  inherited Create;
  InitializeComponent;
  CheckOpen.Checked := FOpenToo;
  Self.ApplicationSts := ApplicationSts;
  if ApplicationSts.AddNewDialog.Size.Width = 0 then
    CenterToScreen
  else begin
    Size := ApplicationSts.AddNewDialog.Size;
    Location := ApplicationSts.AddNewDialog.Location;
  end;
  FFilePaths := StringCollection.Create;
  Self.ProjectName := ProjectName;
  Self.FileImageIndex := FileImageIndex;
  Self.SearchPatterns := SearchPatterns;
  Self.LoadProject := LoadProject;
  CheckSelectionSts;
end;

class function TAddNewDialog.get_OpenToo: Boolean;
begin
  Result := FOpenToo;
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
  e.Cancel := not Assigned(e.Node.Tag);
  if e.Cancel then begin
    for Node in e.Node.Nodes do
      TreeView.SelectedNodes.Add(Node);
  end;
end;

procedure TAddNewDialog.TreeView_DoubleClick(sender: System.Object; e: System.EventArgs);
begin
  if BtnOk.Enabled then begin
    BtnOk_Click(nil,nil);
    DialogResult := BtnOk.DialogResult;
  end;
end;

procedure TAddNewDialog.BtnOk_Click(sender: System.Object; e: System.EventArgs);
var
  Node: TreeNode;
begin
  FFilePaths.Clear;
  for Node in TreeView.SelectedNodes do
    FFilePaths.Add(string(Node.Tag));
end;

procedure TAddNewDialog.TMultiSelectionDialog_Load(sender: System.Object; e: System.EventArgs);

  procedure DirectoryPopulate(Node: TreeNode; Directory: DirectoryInfo);
  var
    SubDirectory: DirectoryInfo;
    &File: FileInfo;
    Pattern: string;
    ChildNode: TreeNode;
  begin
    for Pattern in SearchPatterns do
      for &File in Directory.GetFiles(Pattern) do begin
        ChildNode := Node.Nodes.Add(&File.Name);
        ChildNode.Tag := &File.FullName;
        ChildNode.ImageIndex := FileImageIndex;
      end;
    if Length(Directory.GetDirectories) > 0 then begin
      for SubDirectory in Directory.GetDirectories do begin
        ChildNode := Node.Nodes.Add(SubDirectory.Name);
        ChildNode.ImageIndex := 2;
        DirectoryPopulate(ChildNode, SubDirectory);
      end
    end;
    if Node.Nodes.Count = 0 then
      Node.Remove;
  end;

  procedure ProjectPopulate(Node: TreeNode; FileList: array of TFileInfo);

    function CreateNodePath(Node: TreeNode; FilePath: string): TreeNode;

      function GetNode(Node: TreeNode; Token: string): TreeNode;
      var
        I: Integer;
      begin
        for I := 0 to Pred(Node.Nodes.Count) do begin
          if Node.Nodes[I].Text.ToLower = Token.ToLower then begin
            Result := Node.Nodes[I];
            Exit;
          end;
        end;
        Result := Node.Nodes.Add(Token);
        Result.ImageIndex := 2;
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
    Pattern,FilePath,FileDirectory: string;
    FileInfo: TFileInfo;
    ChildNode: TreeNode;
  begin
    if Assigned(FileList) then begin
      for FileInfo in FileList do begin
        Pattern := '*' + Path.GetExtension(FileInfo.FileName);
        if &Array.IndexOf(SearchPatterns,Pattern.ToLower) >= 0 then begin
          if Path.IsPathRooted(FileInfo.FileName) then
            FilePath := FileInfo.FileName
          else
            FilePath := Path.GetFullPath(ApplicationSts.SearchPath + Path.DirectorySeparatorChar + FileInfo.FileName);
          FileDirectory := Path.GetDirectoryName(FileInfo.FileName);
          if FileDirectory <> '' then begin
            ChildNode := CreateNodePath(Node,FileDirectory);
            ChildNode := ChildNode.Nodes.Add(Path.GetFileName(FileInfo.FileName));
          end
          else
            ChildNode := Node.Nodes.Add(Path.GetFileName(FileInfo.FileName));
          ChildNode.Tag := FilePath;
          ChildNode.ImageIndex := FileImageIndex;
        end;
      end;
    end;
  end;

var
  RootNode: TreeNode;
  BorlandProject: TBorlandProject;
  FileList: array of TFileInfo;
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
          RootNode.ImageIndex := 2;
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
        TreeView.BeginUpdate;
        try
          ApplicationSts.SearchPath := Path.GetFullPath(OpenProjectDialog.FileName);
          BorlandProject := TBorlandProject.LoadFrom(ApplicationSts.SearchPath) as TBorlandProject;
          ApplicationSts.SearchPath := Path.GetDirectoryName(ApplicationSts.SearchPath);
          RootNode := TreeView.Nodes.Add(ApplicationSts.SearchPath);
          RootNode.ImageIndex := 3;
          if Assigned(BorlandProject.DelphiDotNet) then
            FileList := BorlandProject.DelphiDotNet.FileList
          else if Assigned(BorlandProject.CSharp) then
            FileList := BorlandProject.CSharp.FileList;
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


end.
