{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uWinFormManageXSLs;

interface

uses
  System.Drawing, System.Collections, System.ComponentModel,
  System.Windows.Forms, System.Data;

type
  TWinFormManageXSLs = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    PanelBottom: System.Windows.Forms.Panel;
    lstbxSheets: System.Windows.Forms.ListBox;
    btnOK: System.Windows.Forms.Button;
    btnCancel: System.Windows.Forms.Button;
    btnAdd: System.Windows.Forms.Button;
    btnRemove: System.Windows.Forms.Button;
    openFileDlgXsls: System.Windows.Forms.OpenFileDialog;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure btnAdd_Click(sender: System.Object; e: System.EventArgs);
    procedure btnRemove_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    { Private Declarations }
  public
    constructor Create;
    procedure ClearList;
    procedure AddItem(s: string);
    function GetCount: integer;
    function GetItem(i: integer): string;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TWinFormManageXSLs))]

implementation

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TWinFormManageXSLs.InitializeComponent;
type
  TArrayOfSystem_Object = array of System.Object;
begin
  Self.PanelBottom := System.Windows.Forms.Panel.Create;
  Self.btnRemove := System.Windows.Forms.Button.Create;
  Self.btnAdd := System.Windows.Forms.Button.Create;
  Self.btnCancel := System.Windows.Forms.Button.Create;
  Self.btnOK := System.Windows.Forms.Button.Create;
  Self.lstbxSheets := System.Windows.Forms.ListBox.Create;
  Self.openFileDlgXsls := System.Windows.Forms.OpenFileDialog.Create;
  Self.PanelBottom.SuspendLayout;
  Self.SuspendLayout;
  // 
  // PanelBottom
  // 
  Self.PanelBottom.Controls.Add(Self.btnRemove);
  Self.PanelBottom.Controls.Add(Self.btnAdd);
  Self.PanelBottom.Controls.Add(Self.btnCancel);
  Self.PanelBottom.Controls.Add(Self.btnOK);
  Self.PanelBottom.Dock := System.Windows.Forms.DockStyle.Bottom;
  Self.PanelBottom.Location := System.Drawing.Point.Create(0, 172);
  Self.PanelBottom.Name := 'PanelBottom';
  Self.PanelBottom.Size := System.Drawing.Size.Create(336, 40);
  Self.PanelBottom.TabIndex := 0;
  // 
  // btnRemove
  // 
  Self.btnRemove.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.btnRemove.Location := System.Drawing.Point.Create(72, 8);
  Self.btnRemove.Name := 'btnRemove';
  Self.btnRemove.Size := System.Drawing.Size.Create(56, 24);
  Self.btnRemove.TabIndex := 3;
  Self.btnRemove.Text := 'Remove';
  Include(Self.btnRemove.Click, Self.btnRemove_Click);
  // 
  // btnAdd
  // 
  Self.btnAdd.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.btnAdd.Location := System.Drawing.Point.Create(8, 8);
  Self.btnAdd.Name := 'btnAdd';
  Self.btnAdd.Size := System.Drawing.Size.Create(56, 24);
  Self.btnAdd.TabIndex := 2;
  Self.btnAdd.Text := 'Add';
  Include(Self.btnAdd.Click, Self.btnAdd_Click);
  // 
  // btnCancel
  // 
  Self.btnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.btnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.btnCancel.Location := System.Drawing.Point.Create(188, 8);
  Self.btnCancel.Name := 'btnCancel';
  Self.btnCancel.Size := System.Drawing.Size.Create(64, 24);
  Self.btnCancel.TabIndex := 1;
  Self.btnCancel.Text := 'Cancel';
  // 
  // btnOK
  // 
  Self.btnOK.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.btnOK.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.btnOK.Location := System.Drawing.Point.Create(260, 8);
  Self.btnOK.Name := 'btnOK';
  Self.btnOK.Size := System.Drawing.Size.Create(64, 24);
  Self.btnOK.TabIndex := 0;
  Self.btnOK.Text := 'OK';
  // 
  // lstbxSheets
  // 
  Self.lstbxSheets.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.lstbxSheets.Items.AddRange(TArrayOfSystem_Object.Create(' '));
  Self.lstbxSheets.Location := System.Drawing.Point.Create(0, 0);
  Self.lstbxSheets.Name := 'lstbxSheets';
  Self.lstbxSheets.Size := System.Drawing.Size.Create(336, 160);
  Self.lstbxSheets.TabIndex := 1;
  // 
  // openFileDlgXsls
  // 
  Self.openFileDlgXsls.DefaultExt := 'xsl';
  Self.openFileDlgXsls.Filter := 'XSL stylesheets|*.xsl';
  Self.openFileDlgXsls.Multiselect := True;
  Self.openFileDlgXsls.RestoreDirectory := True;
  Self.openFileDlgXsls.Title := 'Select XSL stylesheet';
  //
  // TWinFormManageXSLs
  // 
  Self.AcceptButton := Self.btnOK;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(336, 212);
  Self.Controls.Add(Self.lstbxSheets);
  Self.Controls.Add(Self.PanelBottom);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MinimumSize := System.Drawing.Size.Create(300, 120);
  Self.Name := 'TWinFormManageXSLs';
  Self.ShowInTaskbar := False;
  Self.Text := 'Manage XSL stylesheets';
  Self.PanelBottom.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TWinFormManageXSLs.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TWinFormManageXSLs.Create;
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

procedure TWinFormManageXSLs.ClearList;
begin
  lstbxSheets.Items.Clear;
end;

procedure TWinFormManageXSLs.AddItem(s: string);
begin
  lstbxSheets.Items.Add(s);
end;

function TWinFormManageXSLs.GetCount: integer;
begin
  Result := lstbxSheets.Items.Count;
end;

function TWinFormManageXSLs.GetItem(i: integer): string;
begin
  Result := lstbxSheets.Items.Item[i] as string;
end;

procedure TWinFormManageXSLs.btnRemove_Click(sender: System.Object; e: System.EventArgs);
var i: integer;
begin
  i := lstbxSheets.SelectedIndex;
  if i > -1 then lstbxSheets.Items.RemoveAt(i);
end;

procedure TWinFormManageXSLs.btnAdd_Click(sender: System.Object; e: System.EventArgs);
var s: string;
begin
  if openFileDlgXsls.ShowDialog = System.Windows.Forms.DialogResult.OK then
  begin
    for s in openFileDlgXsls.FileNames do
      if lstbxSheets.Items.IndexOf(s) = -1 then lstbxSheets.Items.Add(s);
  end;
end;

end.
