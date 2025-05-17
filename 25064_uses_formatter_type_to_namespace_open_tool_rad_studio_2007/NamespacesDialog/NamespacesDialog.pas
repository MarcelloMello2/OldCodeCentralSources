unit NamespacesDialog;

interface

uses
  Options,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Windows.Forms,
  TypeList,
  Visibles.BorderExtender, Visibles.ComboBoxEx;

type
  TNamespacesDialog = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    Panel1: System.Windows.Forms.Panel;
    BtnCancel: System.Windows.Forms.Button;
    LabelNamespaces: System.Windows.Forms.Label;
    BorderExtender: Visibles.BorderExtender.TBorderExtender;

    BtnCopy: System.Windows.Forms.Button;
    LabelType: System.Windows.Forms.Label;
    ComboNamespaces: Visibles.ComboBoxEx.TComboBoxEx;
    TextType: System.Windows.Forms.TextBox;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure BtnCopy_Click(sender: System.Object; e: System.EventArgs);
    procedure TextType_TextChanged(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    class var
      Size_1: Size;
      Location_1: Point;
  strict private
    TypeList: TTypeList;
  public
    procedure set_TypeName(const Value: string);
  public
    property TypeName: string write set_TypeName;
    constructor Create(TypeList: TTypeList);
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TNamespacesDialog))]

implementation

{ TNamespacesDialog }

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TNamespacesDialog.InitializeComponent;
begin
  Self.Panel1 := System.Windows.Forms.Panel.Create;
  Self.ComboNamespaces := Visibles.ComboBoxEx.TComboBoxEx.Create;
  Self.LabelType := System.Windows.Forms.Label.Create;
  Self.LabelNamespaces := System.Windows.Forms.Label.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.BorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.BtnCopy := System.Windows.Forms.Button.Create;
  Self.TextType := System.Windows.Forms.TextBox.Create;
  Self.Panel1.SuspendLayout;
  Self.SuspendLayout;
  //
  // Panel1
  //
  Self.Panel1.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.Panel1.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.Panel1, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.Panel1.Controls.Add(Self.TextType);
  Self.Panel1.Controls.Add(Self.ComboNamespaces);
  Self.Panel1.Controls.Add(Self.LabelType);
  Self.Panel1.Controls.Add(Self.LabelNamespaces);
  Self.Panel1.DockPadding.All := 1;
  Self.Panel1.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel1.Name := 'Panel1';
  Self.Panel1.Size := System.Drawing.Size.Create(392, 65);
  Self.Panel1.TabIndex := 0;
  //
  // ComboNamespaces
  //
  Self.ComboNamespaces.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.BorderExtender.SetBorder3DStyle(Self.ComboNamespaces, System.Windows.Forms.Border3DStyle.Adjust);
  Self.ComboNamespaces.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.ComboNamespaces.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.ComboNamespaces.Location := System.Drawing.Point.Create(112, 36);
  Self.ComboNamespaces.Name := 'ComboNamespaces';
  Self.ComboNamespaces.Size := System.Drawing.Size.Create(272, 21);
  Self.ComboNamespaces.TabIndex := 5;
  //
  // LabelType
  //
  Self.LabelType.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.LabelType, System.Windows.Forms.Border3DStyle.Adjust);
  Self.LabelType.Location := System.Drawing.Point.Create(4, 11);
  Self.LabelType.Name := 'LabelType';
  Self.LabelType.Size := System.Drawing.Size.Create(100, 16);
  Self.LabelType.TabIndex := 0;
  Self.LabelType.Text := 'Selected Type';
  Self.LabelType.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  //
  // LabelNamespaces
  //
  Self.LabelNamespaces.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.LabelNamespaces, System.Windows.Forms.Border3DStyle.Adjust);
  Self.LabelNamespaces.Location := System.Drawing.Point.Create(4, 38);
  Self.LabelNamespaces.Name := 'LabelNamespaces';
  Self.LabelNamespaces.Size := System.Drawing.Size.Create(100, 16);
  Self.LabelNamespaces.TabIndex := 2;
  Self.LabelNamespaces.Text := 'Namespaces ({0})';
  Self.LabelNamespaces.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  //
  // BtnCancel
  //
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnCancel.Location := System.Drawing.Point.Create(298, 72);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCancel.TabIndex := 7;
  Self.BtnCancel.Text := '&Cancel';
  //
  // BtnCopy
  //
  Self.BtnCopy.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCopy.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnCopy.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnCopy.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnCopy.Location := System.Drawing.Point.Create(208, 72);
  Self.BtnCopy.Name := 'BtnCopy';
  Self.BtnCopy.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCopy.TabIndex := 6;
  Self.BtnCopy.Text := '&Copy';
  Include(Self.BtnCopy.Click, Self.BtnCopy_Click);
  //
  // TextType
  //
  Self.TextType.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.TextType.AutoSize := False;
  Self.BorderExtender.SetBorder3DStyle(Self.TextType, System.Windows.Forms.Border3DStyle.Adjust);
  Self.TextType.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.TextType.Location := System.Drawing.Point.Create(112, 10);
  Self.TextType.Name := 'TextType';
  Self.TextType.Size := System.Drawing.Size.Create(272, 18);
  Self.TextType.TabIndex := 6;
  Self.TextType.Text := '';
  Include(Self.TextType.TextChanged, Self.TextType_TextChanged);
  //
  // TNamespacesDialog
  //
  Self.AcceptButton := Self.BtnCopy;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(392, 100);
  Self.Controls.Add(Self.BtnCopy);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.Panel1);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MaximizeBox := False;
  Self.MaximumSize := System.Drawing.Size.Create(4096, 125);
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(400, 124);
  Self.Name := 'TNamespacesDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Hide;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'Namespace Search';
  Self.Panel1.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TNamespacesDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    Size_1 := Size;
    Location_1 := Location;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TNamespacesDialog.Create(TypeList: TTypeList);
begin
  inherited Create;
  InitializeComponent;
  Self.TypeList := TypeList;
  if Size_1.Width = 0 then
    CenterToScreen
  else begin
    Size := Size_1;
    Location := Location_1;
  end;
  BtnCopy.Enabled := false;
end;

procedure TNamespacesDialog.TextType_TextChanged(sender: System.Object; e: System.EventArgs);
var
  S: string;
  Namespaces: array of string;
  I: Integer;
begin
  ComboNamespaces.Items.Clear;
  S := TypeList.KnownTypeHash[TextType.Text.ToUpper] as string;
  if Assigned(S) then begin
    Namespaces := S.Split([',']);
    for I := 0 to Pred(Length(Namespaces)) do
      ComboNamespaces.Items.Add(Namespaces[I]);
  end;
  if ComboNamespaces.Items.Count > 0 then begin
    ComboNamespaces.SelectedIndex := 0;
    LabelNamespaces.Text := System.string.Format('Namespaces ({0})',[ComboNamespaces.Items.Count]);
    BtnCopy.Enabled := true;
  end
  else begin
    LabelNamespaces.Text := 'Namespaces (0)';
    BtnCopy.Enabled := false;
  end;
end;

procedure TNamespacesDialog.set_TypeName(const Value: string);
begin
  TextType.Text := Value;
end;

procedure TNamespacesDialog.BtnCopy_Click(sender: System.Object; e: System.EventArgs);
begin
  Clipboard.SetDataObject(ComboNamespaces.Items[ComboNamespaces.SelectedIndex],true);
end;


end.
