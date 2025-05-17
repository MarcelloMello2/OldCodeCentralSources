unit SetsDialog;

interface

uses
  Borland.Studio.ToolsAPI,
  Options,
  ProjectOptionsSets,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Windows.Forms,
  Visibles.TrimmingLabel,
  Visibles.BorderExtender, Visibles.ComboBoxEx;

type
  TSetsDialogMessageEventArgs = class(EventArgs)
  public
    &Message: string;
    constructor Create(AMessage: string);
  end;
  TSetsDialogMessageEvent = procedure (Sender: TObject; Args: TSetsDialogMessageEventArgs) of object;
  TSetsDialog = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    components: System.ComponentModel.IContainer;
    Panel: System.Windows.Forms.Panel;
    BtnCancel: System.Windows.Forms.Button;
    BtnNew: System.Windows.Forms.Button;
    LabelSetsName: System.Windows.Forms.Label;
    BorderExtender: Visibles.BorderExtender.TBorderExtender;
    LabelProject: System.Windows.Forms.Label;
    BtnDelete: System.Windows.Forms.Button;
    BtnActivate: System.Windows.Forms.Button;
    LabelProjectPath: Visibles.TrimmingLabel.TTrimmingLabel;
    BtnNewDebugRelease: System.Windows.Forms.Button;
    BtnConfig: System.Windows.Forms.Button;
    ComboSetsNames: Visibles.ComboBoxEx.TComboBoxEx;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure BtnNew_Click(sender: System.Object; e: System.EventArgs);
    procedure BtnDelete_Click(sender: System.Object; e: System.EventArgs);
    procedure ComboSetsNames_TextChanged(sender: System.Object; e: System.EventArgs);
    procedure BtnActivate_Click(sender: System.Object; e: System.EventArgs);
    procedure ComboSetsNames_Click(sender: System.Object; e: System.EventArgs);
    procedure BtnNewDebugRelease_Click(sender: System.Object; e: System.EventArgs);
    procedure BtnConfig_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    class var
      Size_1: Size;
      Location_1: Point;
  strict private
    FProjectPath: string;
    FOTAOptions: IOTAProjectOptions;
    FCurrentSetName: string;
    FSetsDialogMessage: TSetsDialogMessageEvent;
    Options: TOptions;
    OptionsSets: TProjectOptionsSets;
    CurrentSet: TProjectOptionsSet;
    function ProjectSetsPath(APath: string): string;
    procedure EnableBtns;
  public
    procedure set_ProjectPath(const Value: string);
  public
    property ProjectPath: string read FProjectPath;
    property OTAOptions: IOTAProjectOptions read FOTAOptions;
    property CurrentSetName: string read FCurrentSetName;
    property SetsDialogMessage: TSetsDialogMessageEvent add FSetsDialogMessage remove FSetsDialogMessage;
    function ShowDialog: DialogResult;
    procedure Syncronize;
    procedure Rename(OldPath: string);
    constructor Create(ProjectPath: string; OTAOptions: IOTAProjectOptions; SetsDialogMessageHandler: TSetsDialogMessageEvent; PluginOptions: TOptions);
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TSetsDialog))]

implementation

uses
  ConfigDialog,
  System.IO;

{$R 'SetsDialog\SetsDialog.TSetsDialog.resources' 'SetsDialog\SetsDialog.resx'}

{ TSetsDialogMessageEventArgs }

constructor TSetsDialogMessageEventArgs.Create(AMessage: string);
begin
  inherited Create;
  &Message := AMessage;
end;

{ TSetsDialog }

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>
procedure TSetsDialog.InitializeComponent;
begin
  Self.components := System.ComponentModel.Container.Create;
  Self.Panel := System.Windows.Forms.Panel.Create;
  Self.ComboSetsNames := Visibles.ComboBoxEx.TComboBoxEx.Create;
  Self.BtnDelete := System.Windows.Forms.Button.Create;
  Self.LabelProject := System.Windows.Forms.Label.Create;
  Self.LabelSetsName := System.Windows.Forms.Label.Create;
  Self.BtnNew := System.Windows.Forms.Button.Create;
  Self.LabelProjectPath := Visibles.TrimmingLabel.TTrimmingLabel.Create(Self.components);
  Self.BtnNewDebugRelease := System.Windows.Forms.Button.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.BorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.BtnActivate := System.Windows.Forms.Button.Create;
  Self.BtnConfig := System.Windows.Forms.Button.Create;
  Self.Panel.SuspendLayout;
  Self.SuspendLayout;
  //
  // Panel
  //
  Self.Panel.Anchor := (System.Windows.Forms.AnchorStyles((((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Bottom) or System.Windows.Forms.AnchorStyles.Left)
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.Panel.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.Panel, System.Windows.Forms.Border3DStyle.SunkenOuter);
  Self.Panel.Controls.Add(Self.ComboSetsNames);
  Self.Panel.Controls.Add(Self.BtnDelete);
  Self.Panel.Controls.Add(Self.LabelProject);
  Self.Panel.Controls.Add(Self.LabelSetsName);
  Self.Panel.Controls.Add(Self.BtnNew);
  Self.Panel.Controls.Add(Self.LabelProjectPath);
  Self.Panel.Controls.Add(Self.BtnNewDebugRelease);
  Self.Panel.DockPadding.All := 1;
  Self.Panel.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel.Name := 'Panel';
  Self.Panel.Size := System.Drawing.Size.Create(472, 65);
  Self.Panel.TabIndex := 0;
  //
  // ComboSetsNames
  //
  Self.BorderExtender.SetBorder3DStyle(Self.ComboSetsNames, System.Windows.Forms.Border3DStyle.Adjust);
  Self.ComboSetsNames.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.ComboSetsNames.Location := System.Drawing.Point.Create(112, 36);
  Self.ComboSetsNames.Name := 'ComboSetsNames';
  Self.ComboSetsNames.Size := System.Drawing.Size.Create(119, 21);
  Self.ComboSetsNames.TabIndex := 8;
  Include(Self.ComboSetsNames.TextChanged, Self.ComboSetsNames_TextChanged);
  Include(Self.ComboSetsNames.Click, Self.ComboSetsNames_Click);
  //
  // BtnDelete
  //
  Self.BtnDelete.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnDelete.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnDelete.Location := System.Drawing.Point.Create(311, 36);
  Self.BtnDelete.Name := 'BtnDelete';
  Self.BtnDelete.Size := System.Drawing.Size.Create(71, 21);
  Self.BtnDelete.TabIndex := 7;
  Self.BtnDelete.Text := '&Delete';
  Include(Self.BtnDelete.Click, Self.BtnDelete_Click);
  //
  // LabelProject
  //
  Self.LabelProject.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.LabelProject, System.Windows.Forms.Border3DStyle.Adjust);
  Self.LabelProject.Location := System.Drawing.Point.Create(4, 12);
  Self.LabelProject.Name := 'LabelProject';
  Self.LabelProject.Size := System.Drawing.Size.Create(100, 16);
  Self.LabelProject.TabIndex := 0;
  Self.LabelProject.Text := 'Project Path';
  Self.LabelProject.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  //
  // LabelSetsName
  //
  Self.LabelSetsName.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.LabelSetsName, System.Windows.Forms.Border3DStyle.Adjust);
  Self.LabelSetsName.Location := System.Drawing.Point.Create(4, 38);
  Self.LabelSetsName.Name := 'LabelSetsName';
  Self.LabelSetsName.Size := System.Drawing.Size.Create(100, 16);
  Self.LabelSetsName.TabIndex := 2;
  Self.LabelSetsName.Text := 'Settings Set Name';
  Self.LabelSetsName.TextAlign := System.Drawing.ContentAlignment.MiddleRight;
  //
  // BtnNew
  //
  Self.BtnNew.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnNew.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnNew.Location := System.Drawing.Point.Create(236, 36);
  Self.BtnNew.Name := 'BtnNew';
  Self.BtnNew.Size := System.Drawing.Size.Create(71, 21);
  Self.BtnNew.TabIndex := 4;
  Self.BtnNew.Text := '&New';
  Include(Self.BtnNew.Click, Self.BtnNew_Click);
  //
  // LabelProjectPath
  //
  Self.LabelProjectPath.Anchor := (System.Windows.Forms.AnchorStyles(((System.Windows.Forms.AnchorStyles.Top
    or System.Windows.Forms.AnchorStyles.Left) or System.Windows.Forms.AnchorStyles.Right)));
  Self.BorderExtender.SetBorder3DStyle(Self.LabelProjectPath, System.Windows.Forms.Border3DStyle.Adjust);
  Self.LabelProjectPath.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.LabelProjectPath.Location := System.Drawing.Point.Create(112, 10);
  Self.LabelProjectPath.Name := 'LabelProjectPath';
  Self.LabelProjectPath.Size := System.Drawing.Size.Create(352, 19);
  Self.LabelProjectPath.TabIndex := 1;
  Self.LabelProjectPath.TextAlign := System.Drawing.ContentAlignment.MiddleLeft;
  Self.LabelProjectPath.Trimming := System.Drawing.StringTrimming.EllipsisPath;
  //
  // BtnNewDebugRelease
  //
  Self.BtnNewDebugRelease.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnNewDebugRelease.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnNewDebugRelease.Location := System.Drawing.Point.Create(311, 36);
  Self.BtnNewDebugRelease.Name := 'BtnNewDebugRelease';
  Self.BtnNewDebugRelease.Size := System.Drawing.Size.Create(118, 21);
  Self.BtnNewDebugRelease.TabIndex := 5;
  Self.BtnNewDebugRelease.Text := 'New Debug/Release';
  Self.BtnNewDebugRelease.Visible := False;
  Include(Self.BtnNewDebugRelease.Click, Self.BtnNewDebugRelease_Click);
  //
  // BtnCancel
  //
  Self.BtnCancel.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnCancel.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnCancel.DialogResult := System.Windows.Forms.DialogResult.Cancel;
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnCancel.Location := System.Drawing.Point.Create(378, 72);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnCancel.TabIndex := 7;
  Self.BtnCancel.Text := '&Cancel';
  //
  // BtnActivate
  //
  Self.BtnActivate.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Right)));
  Self.BtnActivate.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnActivate.DialogResult := System.Windows.Forms.DialogResult.OK;
  Self.BtnActivate.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnActivate.Location := System.Drawing.Point.Create(288, 72);
  Self.BtnActivate.Name := 'BtnActivate';
  Self.BtnActivate.Size := System.Drawing.Size.Create(86, 23);
  Self.BtnActivate.TabIndex := 6;
  Self.BtnActivate.Text := '&Activate';
  Include(Self.BtnActivate.Click, Self.BtnActivate_Click);
  //
  // BtnConfig
  //
  Self.BtnConfig.Anchor := (System.Windows.Forms.AnchorStyles((System.Windows.Forms.AnchorStyles.Bottom
    or System.Windows.Forms.AnchorStyles.Left)));
  Self.BtnConfig.BackColor := System.Drawing.Color.LightSteelBlue;
  Self.BtnConfig.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnConfig.Location := System.Drawing.Point.Create(8, 72);
  Self.BtnConfig.Name := 'BtnConfig';
  Self.BtnConfig.Size := System.Drawing.Size.Create(56, 23);
  Self.BtnConfig.TabIndex := 8;
  Self.BtnConfig.Text := '&Config.';
  Include(Self.BtnConfig.Click, Self.BtnConfig_Click);
  //
  // TSetsDialog
  //
  Self.AcceptButton := Self.BtnActivate;
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.CancelButton := Self.BtnCancel;
  Self.ClientSize := System.Drawing.Size.Create(472, 100);
  Self.Controls.Add(Self.BtnConfig);
  Self.Controls.Add(Self.BtnActivate);
  Self.Controls.Add(Self.BtnCancel);
  Self.Controls.Add(Self.Panel);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.SizableToolWindow;
  Self.MaximizeBox := False;
  Self.MaximumSize := System.Drawing.Size.Create(4096, 125);
  Self.MinimizeBox := False;
  Self.MinimumSize := System.Drawing.Size.Create(480, 124);
  Self.Name := 'TSetsDialog';
  Self.ShowInTaskbar := False;
  Self.SizeGripStyle := System.Windows.Forms.SizeGripStyle.Show;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Text := 'Project Options Sets';
  Self.Panel.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TSetsDialog.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    Size_1 := Size;
    Location_1 := Location;
    CurrentSet.Free;
    OptionsSets.Free;
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TSetsDialog.Create(ProjectPath: string; OTAOptions: IOTAProjectOptions; SetsDialogMessageHandler: TSetsDialogMessageEvent; PluginOptions: TOptions);
begin
  inherited Create;
  InitializeComponent;
  if Size_1.Width = 0 then
    CenterToScreen
  else begin
    Size := Size_1;
    Location := Location_1;
  end;
  LabelProjectPath.BackColor := BorderExtender.TweakColor(LabelProjectPath.BackColor,10);
  BtnConfig.Enabled := Assigned(OTAOptions);
  set_ProjectPath(ProjectPath);
  FOTAOptions := OTAOptions;
  Include(SetsDialogMessage,SetsDialogMessageHandler);
  Options := PluginOptions;
  Cursor.Current := Cursors.WaitCursor;
  if &File.Exists(FProjectPath) then begin
    try
      try
        OptionsSets := TProjectOptionsSets.LoadFrom(ProjectSetsPath(FProjectPath)) as TProjectOptionsSets;
      except
        OptionsSets := TProjectOptionsSets.Create;
      end;
      FCurrentSetName := OptionsSets.LastActivated;
    finally
      Cursor.Current := Cursors.Default;
    end;
  end
  else begin
    OptionsSets := TProjectOptionsSets.Create;
    FCurrentSetName := '';
  end;
end;

procedure TSetsDialog.EnableBtns;
begin
  if (OptionsSets.Item.Count > 0) or (ComboSetsNames.Items.Count > 0) then begin
    BtnDelete.Show;
    BtnNewDebugRelease.Hide;
  end
  else begin
    BtnDelete.Hide;
    BtnNewDebugRelease.Show;
  end;
  BtnActivate.Enabled := (ComboSetsNames.Text.Trim <> '') and (ComboSetsNames.Items.Contains(ComboSetsNames.Text.Trim));
  BtnDelete.Enabled := BtnActivate.Enabled;
  BtnNew.Enabled := (ComboSetsNames.Text.Trim <> '') and not (ComboSetsNames.Items.Contains(ComboSetsNames.Text.Trim));
end;

function TSetsDialog.ProjectSetsPath(APath: string): string;
begin
  Result := Path.GetDirectoryName(APath) + Path.DirectorySeparatorChar +
            Path.GetFileNameWithoutExtension(APath) + '.bdsproj.sets';
end;

procedure TSetsDialog.set_ProjectPath(const Value: string);
begin
  FProjectPath := Value;
  LabelProjectPath.Text := Value;
end;

procedure TSetsDialog.ComboSetsNames_Click(sender: System.Object; e: System.EventArgs);
begin
  EnableBtns;
end;

procedure TSetsDialog.ComboSetsNames_TextChanged(sender: System.Object; e: System.EventArgs);
begin
  EnableBtns;
end;

procedure TSetsDialog.BtnNew_Click(sender: System.Object; e: System.EventArgs);
var
  I: Integer;
  NewSet: TProjectOptionsSet;
begin
  NewSet := CurrentSet.Clone as TProjectOptionsSet;
  NewSet.Name := ComboSetsNames.Text;
  for I := 0 to Pred(OptionsSets.Item.Count) do
    if System.string.Compare((OptionsSets.Item[I] as TProjectOptionsSet).Name,NewSet.Name,true) = 0 then begin
      if Assigned(FSetsDialogMessage) then
        FSetsDialogMessage(Self,TSetsDialogMessageEventArgs.Create(System.string.Format('A Options Set named {0} already exist',[NewSet.Name])));
      Exit;
    end;
  Cursor.Current := Cursors.WaitCursor;
  try
    OptionsSets.Item.Add(NewSet);
    OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
    ComboSetsNames.SelectedIndex := ComboSetsNames.Items.Add(NewSet.Name);
    EnableBtns;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TSetsDialog.BtnNewDebugRelease_Click(sender: System.Object; e: System.EventArgs);
var
  ReleaseSet: TProjectOptionsSet;
begin
  ReleaseSet := CurrentSet.Clone as TProjectOptionsSet; {Shallow clone}
  CurrentSet.Name := 'Debug';
  ReleaseSet.Name := 'Release';
  Cursor.Current := Cursors.WaitCursor;
  try
    OptionsSets.Item.Add(CurrentSet);
    OptionsSets.Item.Add(ReleaseSet);
    OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
    try
      OptionsSets := TProjectOptionsSets.LoadFrom(ProjectSetsPath(FProjectPath)) as TProjectOptionsSets;
      {Now the clones are deep}
      CurrentSet := OptionsSets.Item[0] as TProjectOptionsSet;
      ReleaseSet := OptionsSets.Item[1] as TProjectOptionsSet;
      Options.ModifyAsRelease(ReleaseSet);
      OptionsSets.LastActivated := CurrentSet.Name;
      OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
      FCurrentSetName := CurrentSet.Name;
      ComboSetsNames.SelectedIndex := ComboSetsNames.Items.Add(CurrentSet.Name);
      ComboSetsNames.Items.Add(ReleaseSet.Name);
    except
      OptionsSets := TProjectOptionsSets.Create;
    end;
    EnableBtns;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TSetsDialog.BtnDelete_Click(sender: System.Object; e: System.EventArgs);
begin
  Cursor.Current := Cursors.WaitCursor;
  try
    if System.string.Compare((OptionsSets.Item[ComboSetsNames.SelectedIndex] as TProjectOptionsSet).Name,
                             OptionsSets.LastActivated,true) = 0 then begin
      OptionsSets.LastActivated := '';
      FCurrentSetName := '';
    end;
    OptionsSets.Item.RemoveAt(ComboSetsNames.SelectedIndex);
    OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
    ComboSetsNames.Items.RemoveAt(ComboSetsNames.SelectedIndex);
    ComboSetsNames.Text := '';
    EnableBtns;
  finally
    Cursor.Current := Cursors.Default;
  end;
end;

procedure TSetsDialog.BtnActivate_Click(sender: System.Object; e: System.EventArgs);
var
  I: Integer;
  Option: TProjectOption;
  Value: TObject;
begin
  CurrentSet := OptionsSets.Item[ComboSetsNames.SelectedIndex] as TProjectOptionsSet;
  if System.string.Compare(CurrentSet.Name,OptionsSets.LastActivated,true) <> 0 then begin
    Cursor.Current := Cursors.WaitCursor;
    try
      for I := 0 to Pred(CurrentSet.Values.Count) do begin
        Option := CurrentSet.Values[I] as TProjectOption;
        if not Options.HashedIgnoreOptions.Contains(Option.Name.ToUpper) then try
          Value := FOTAOptions.GetOptionValue(Option.Name);
          if Assigned(Value) and not Assigned(Option.Value) then
            FOTAOptions.SetOptionValue(Option.Name,nil)
          else if not Assigned(Value) and Assigned(Option.Value) then
            FOTAOptions.SetOptionValue(Option.Name,Option.Value)
          else if Assigned(Value) and Assigned(Option.Value) then begin
            if System.string.Compare(Value.ToString,Option.Value.ToString,true) <> 0 then
              FOTAOptions.SetOptionValue(Option.Name,Option.Value);
          end;
        except
          ;
        end;
      end;
      OptionsSets.LastActivated := CurrentSet.Name;
      OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
      FCurrentSetName := CurrentSet.Name;
    finally
      Cursor.Current := Cursors.Default;
    end;
  end;
end;

procedure TSetsDialog.BtnConfig_Click(sender: System.Object; e: System.EventArgs);
begin
  with TConfigDialog.Create(FOTAOptions,Options) do begin
    if ShowDialog = System.Windows.Forms.DialogResult.OK then
      Options.Store;
    Free;
  end;
end;

function TSetsDialog.ShowDialog: DialogResult;
var
  OptionNames: IOTAOptionNames;
  I: Integer;
  Option: TProjectOption;
  S: string;
begin
  BtnActivate.Enabled := false;
  BtnNew.Enabled := false;
  BtnDelete.Enabled := false;
  ComboSetsNames.SelectedIndex := -1;
  if Assigned(FOTAOptions) then begin
    Cursor.Current := Cursors.WaitCursor;
    try
      for I := 0 to Pred(OptionsSets.Item.Count) do begin
        S := (OptionsSets.Item[I] as TProjectOptionsSet).Name;
        ComboSetsNames.Items.Add(S);
        if System.string.Compare(S,OptionsSets.LastActivated,true) = 0 then
          ComboSetsNames.SelectedIndex := I;
      end;
      if FOTAOptions.ModifiedState then
        ComboSetsNames.SelectedIndex := -1;
      if (ComboSetsNames.Items.Count > 0) and (ComboSetsNames.SelectedIndex <> -1) then
        CurrentSet := OptionsSets.Item[ComboSetsNames.SelectedIndex] as TProjectOptionsSet
      else begin
        CurrentSet := TProjectOptionsSet.Create;
        OptionNames := FOTAOptions.OptionNames;
        for I := 0 to Pred(OptionNames.Count) do begin
          Option := TProjectOption.Create;
          Option.Name := OptionNames.GetName(I);
          try
            Option.Value := FOTAOptions.GetOptionValue(Option.Name);
          except
            ;
          end;
          CurrentSet.Values.Add(Option);
        end;
      end;
      EnableBtns;
    finally
      Cursor.Current := Cursors.Default;
    end;
  end;
  Result := inherited ShowDialog;
end;

procedure TSetsDialog.Syncronize;
var
  OptionNames: IOTAOptionNames;
  I: Integer;
  Option: TProjectOption;
begin
  if Assigned(FOTAOptions) then begin
    Cursor.Current := Cursors.WaitCursor;
    try
      if OptionsSets.LastActivated <> '' then begin
        CurrentSet := TProjectOptionsSet.Create;
        CurrentSet.Name := OptionsSets.LastActivated;
        OptionNames := FOTAOptions.OptionNames;
        for I := 0 to Pred(OptionNames.Count) do begin
          Option := TProjectOption.Create;
          Option.Name := OptionNames.GetName(I);
          try
            Option.Value := FOTAOptions.GetOptionValue(Option.Name);
          except
            ;
          end;
          CurrentSet.Values.Add(Option);
        end;
        for I := 0 to Pred(OptionsSets.Item.Count) do begin
          if System.string.Compare((OptionsSets.Item[I] as TProjectOptionsSet).Name,OptionsSets.LastActivated,true) = 0 then begin
            OptionsSets.Item[I] := CurrentSet;
            OptionsSets.StoreTo(ProjectSetsPath(FProjectPath));
            Break;
          end;
        end;
      end;
    except
      Cursor.Current := Cursors.Default;
    end
  end;
end;

procedure TSetsDialog.Rename(OldPath: string);
begin
  Cursor.Current := Cursors.WaitCursor;
  try
    &File.Move(ProjectSetsPath(OldPath),ProjectSetsPath(FProjectPath));
  except
    Cursor.Current := Cursors.Default;
  end
end;


end.
