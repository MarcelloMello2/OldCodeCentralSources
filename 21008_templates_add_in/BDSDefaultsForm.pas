unit BDSDefaultsForm;
//------------------------------------------------------------------------------
//  File name:      BDSDefaultsForm.pas
//  Last updated:   11/30/03
//  Author:         Sergey Mishkovskiy
//  Company:        USysWare, Inc.
//  Contact info:   usysware@comcast.net
//
//  Compatibility:  Borland Delphi for .NET
//
//  Description:    Consists default configuration form.
//------------------------------------------------------------------------------

interface

uses
  System.Drawing,
  System.Collections,
  System.ComponentModel,
  System.Windows.Forms,
  System.Data,
  BDSConfig;

type
  TDialogControls = record
    EditBox: System.Windows.Forms.TextBox;
    Expression: TConfigExpression;
  end;

  TBDSDefaultsForm = class(System.Windows.Forms.Form)
  private
    FConfigAddin: TConfigAddin;
    FControls: array of TDialogControls;
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    BtnOK: System.Windows.Forms.Button;
    BtnCancel: System.Windows.Forms.Button;
    TabsMain: System.Windows.Forms.TabControl;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure TBDSDefaultsForm_Load(sender: System.Object; e: System.EventArgs);
    procedure BtnOK_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  public
    constructor Create;

    property ConfigAddin: TConfigAddin read FConfigAddin write FConfigAddin;
  end;

implementation

{$R 'BDSDefaultsForm.TBDSDefaultsForm.resources'}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support - do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TBDSDefaultsForm.InitializeComponent;
begin
  Self.BtnOK := System.Windows.Forms.Button.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.TabsMain := System.Windows.Forms.TabControl.Create;
  Self.SuspendLayout;
  // 
  // BtnOK
  // 
  Self.BtnOK.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
      or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnOK.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnOK.Enabled := False;
  Self.BtnOK.Location := System.Drawing.Point.Create(224, 168);
  Self.BtnOK.Name := 'BtnOK';
  Self.BtnOK.TabIndex := 0;
  Self.BtnOK.Text := '&OK';
  Include(Self.BtnOK.Click, Self.BtnOK_Click);
  // 
  // BtnCancel
  // 
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom 
      or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.Location := System.Drawing.Point.Create(309, 168);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.TabIndex := 1;
  Self.BtnCancel.Text := '&Cancel';
  // 
  // TabsMain
  // 
  Self.TabsMain.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top 
      or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left) 
      or System.Windows.Forms.AnchorStyles.Right)));
  Self.TabsMain.Location := System.Drawing.Point.Create(8, 8);
  Self.TabsMain.Multiline := True;
  Self.TabsMain.Name := 'TabsMain';
  Self.TabsMain.SelectedIndex := 0;
  Self.TabsMain.Size := System.Drawing.Size.Create(376, 150);
  Self.TabsMain.TabIndex := 2;
  // 
  // TBDSDefaultsForm
  // 
  Self.AcceptButton := Self.BtnOK;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(394, 201);
  Self.Controls.Add(Self.TabsMain);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.BtnOK);
  Self.KeyPreview := True;
  Self.MaximizeBox := False;
  Self.MinimizeBox := False;
  Self.Name := 'TBDSDefaultsForm';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Hide;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterParent;
  Include(Self.Load, Self.TBDSDefaultsForm_Load);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TBDSDefaultsForm.Dispose(Disposing: Boolean);
begin
  if Disposing then
  begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TBDSDefaultsForm.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  
  MinimumSize := Size;
end;

procedure TBDSDefaultsForm.BtnOK_Click(sender: System.Object; e: System.EventArgs);
var
  Index: Integer;
begin
  for Index := 0 to Length(FControls) - 1 do
    with FControls[Index] do
      Expression.Value := EditBox.Text;
end;

procedure TBDSDefaultsForm.TBDSDefaultsForm_Load(sender: System.Object; e: System.EventArgs);
var
  GrpIdx, Index, Idx, ControlIdx, TabIdx, Count: Integer;
  Expr: TConfigExpression;
  Group: TConfigDialogGroup;
  ATabPage: System.Windows.Forms.TabPage;
  ALabel: System.Windows.Forms.Label;
  ATextBox: System.Windows.Forms.TextBox;
begin
  if FConfigAddin = nil then
    Exit;

  Text := FConfigAddin.Dialog.Title;
  ControlIdx := 0;
  SetLength(FControls, 0);

  for GrpIdx := 0 to FConfigAddin.Dialog.GroupCount - 1 do
  begin
    Group := FConfigAddin.Dialog.Groups[GrpIdx];
    if not Group.Enabled then
      Continue;

    ATabPage := nil;
    Count := 0;

    for Index := 0 to FConfigAddin.UnitCount - 1 do
    begin
      TabIdx := 0;

      for Idx := 0 to FConfigAddin.Units[Index].ExpressionCount - 1 do
      begin
        Expr := FConfigAddin.Units[Index].Expressions[Idx];
        if (Expr.GroupItem = nil) or (Expr.GroupItem.Name <> Group.Name) then
          Continue;

        if ATabPage = nil then
        begin
          ATabPage := System.Windows.Forms.TabPage.Create(Group.Caption);
          ATabPage.SuspendLayout;

          ATabPage.Location := System.Drawing.Point.Create(4, 22);
          ATabPage.Name := 'TabPage' + GrpIdx.ToString;
          ATabPage.Size := System.Drawing.Size.Create(336, 190);
          ATabPage.TabIndex := 0;
        end;

        ALabel := System.Windows.Forms.Label.Create;
        ATextBox := System.Windows.Forms.TextBox.Create;
        Inc(ControlIdx);

        // Label
        ALabel.Location := System.Drawing.Point.Create(16, 24 * (Count+1) + 4);
        ALabel.Name := 'Label' + ControlIdx.ToString;
        ALabel.TabIndex := TabIdx;
        ALabel.AutoSize := True;
        ALabel.Text := Expr.GroupItem.Caption;
        Inc(TabIdx);

        // TextBox
        ATextBox.Anchor := System.Windows.Forms.AnchorStyles(
          System.Windows.Forms.AnchorStyles.Top or
          System.Windows.Forms.AnchorStyles.Left or
          System.Windows.Forms.AnchorStyles.Right);
        ATextBox.Location := System.Drawing.Point.Create(150, 24 * (Count+1));
        ATextBox.Name := 'TextBox' + ControlIdx.ToString;
        ATextBox.TabIndex := TabIdx;
        ATextBox.Text := Expr.Value;
        ATextBox.Size := System.Drawing.Size.Create(
          TabsMain.Size.Width - ATextBox.Location.X - 28, ATextBox.Size.Height);
        Inc(TabIdx);

        // resize the dialog if the last edit box doesn't fit
        if (ATextBox.Location.Y + ATextBox.Size.Height) >
           (TabsMain.ClientRectangle.Bottom -
            TabsMain.ClientRectangle.Top - 36) then
          Self.Size := System.Drawing.Size.Create(Self.Size.Width,
            Self.Size.Height + 24);

        ATabPage.Controls.Add(ATextBox);
        ATabPage.Controls.Add(ALabel);
        Inc(Count);

        SetLength(FControls, Length(FControls) + 1);
        with FControls[Length(FControls) - 1] do
        begin
          EditBox := ATextBox;
          Expression := Expr;
        end;
      end;
    end;

    if ATabPage <> nil then
    begin
      Self.TabsMain.Controls.Add(ATabPage);
      ATabPage.ResumeLayout(False);
    end;
  end;

  MaximumSize := System.Drawing.Size.Create(Size.Width * 2, Size.Height);

  BtnOK.Enabled := True;
end;

end.
