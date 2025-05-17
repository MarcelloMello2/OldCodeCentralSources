unit OptionsList;

interface

uses
  Borland.Studio.ToolsAPI,
  System.Collections,
  System.ComponentModel,
  System.Drawing,
  System.Windows.Forms,
  Visibles.BorderExtender, System.Resources;

type
  TOptionsList = class(System.Windows.Forms.Form)
  {$REGION 'Designer Managed Code'}
  strict private
    Components: System.ComponentModel.Container;
    ListOptions: System.Windows.Forms.CheckedListBox;
    BorderExtender: Visibles.BorderExtender.TBorderExtender;
    Panel: System.Windows.Forms.Panel;
    BtnCancel: System.Windows.Forms.Button;
    BtnOk: System.Windows.Forms.Button;
    procedure InitializeComponent;
    procedure BtnOk_Click(sender: System.Object; e: System.EventArgs);
    procedure BtnCancel_Click(sender: System.Object; e: System.EventArgs);
  {$ENDREGION}
  strict protected
    procedure Dispose(Disposing: Boolean); override;
  strict private
    OptionNames: IOTAOptionNames;
  public
    function GetSelectedOptions: ArrayList;
    constructor Create(OTAOptions: IOTAProjectOptions);
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TOptionsList))]

implementation

{$R 'OptionsList\OptionsList.TOptionsList.resources' 'OptionsList\OptionsList.resx'}

{$REGION 'Windows Form Designer generated code'}
procedure TOptionsList.InitializeComponent;
var
  resources: System.Resources.ResourceManager;
begin
  resources := System.Resources.ResourceManager.Create(TypeOf(TOptionsList));
  Self.ListOptions := System.Windows.Forms.CheckedListBox.Create;
  Self.BorderExtender := Visibles.BorderExtender.TBorderExtender.Create;
  Self.Panel := System.Windows.Forms.Panel.Create;
  Self.BtnOk := System.Windows.Forms.Button.Create;
  Self.BtnCancel := System.Windows.Forms.Button.Create;
  Self.Panel.SuspendLayout;
  Self.SuspendLayout;
  //
  // ListOptions
  //
  Self.ListOptions.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.ListOptions, System.Windows.Forms.Border3DStyle.Adjust);
  Self.ListOptions.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.ListOptions.Dock := System.Windows.Forms.DockStyle.Fill;
  Self.ListOptions.IntegralHeight := False;
  Self.ListOptions.Location := System.Drawing.Point.Create(0, 0);
  Self.ListOptions.Name := 'ListOptions';
  Self.ListOptions.Size := System.Drawing.Size.Create(244, 288);
  Self.ListOptions.TabIndex := 11;
  Self.ListOptions.TabStop := False;
  //
  // Panel
  //
  Self.Panel.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.Panel, System.Windows.Forms.Border3DStyle.Adjust);
  Self.Panel.BorderStyle := System.Windows.Forms.BorderStyle.FixedSingle;
  Self.Panel.Controls.Add(Self.BtnOk);
  Self.Panel.Controls.Add(Self.BtnCancel);
  Self.Panel.Dock := System.Windows.Forms.DockStyle.Top;
  Self.Panel.Location := System.Drawing.Point.Create(0, 0);
  Self.Panel.Name := 'Panel';
  Self.Panel.Size := System.Drawing.Size.Create(244, 17);
  Self.Panel.TabIndex := 12;
  //
  // BtnOk
  //
  Self.BtnOk.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.BtnOk, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnOk.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnOk.Font := System.Drawing.Font.Create('Microsoft Sans Serif', 9.75,
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.BtnOk.Image := (System.Drawing.Image(resources.GetObject('BtnOk.Image')));
  Self.BtnOk.Location := System.Drawing.Point.Create(213, 2);
  Self.BtnOk.Name := 'BtnOk';
  Self.BtnOk.Size := System.Drawing.Size.Create(12, 11);
  Self.BtnOk.TabIndex := 1;
  Self.BtnOk.TabStop := False;
  Include(Self.BtnOk.Click, Self.BtnOk_Click);
  //
  // BtnCancel
  //
  Self.BtnCancel.BackColor := System.Drawing.SystemColors.Control;
  Self.BorderExtender.SetBorder3DStyle(Self.BtnCancel, System.Windows.Forms.Border3DStyle.Adjust);
  Self.BtnCancel.FlatStyle := System.Windows.Forms.FlatStyle.Flat;
  Self.BtnCancel.Font := System.Drawing.Font.Create('Microsoft Sans Serif', 9.75,
      System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, (Byte(0)));
  Self.BtnCancel.Image := (System.Drawing.Image(resources.GetObject('BtnCanc' +
    'el.Image')));
  Self.BtnCancel.Location := System.Drawing.Point.Create(228, 2);
  Self.BtnCancel.Name := 'BtnCancel';
  Self.BtnCancel.Size := System.Drawing.Size.Create(12, 11);
  Self.BtnCancel.TabIndex := 0;
  Self.BtnCancel.TabStop := False;
  Include(Self.BtnCancel.Click, Self.BtnCancel_Click);
  //
  // TOptionsList
  //
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(244, 288);
  Self.Controls.Add(Self.Panel);
  Self.Controls.Add(Self.ListOptions);
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.None;
  Self.Name := 'TOptionsList';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.Manual;
  Self.Panel.ResumeLayout(False);
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TOptionsList.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TOptionsList.Create(OTAOptions: IOTAProjectOptions);
var
  OptionI: Integer;
  S: string;
begin
  inherited Create;
  InitializeComponent;
  ListOptions.BackColor := BorderExtender.TweakColor(ListOptions.BackColor,10);
  OptionNames := OTAOptions.OptionNames;
  ListOptions.BeginUpdate;
  for OptionI := 0 to Pred(OptionNames.Count) do begin
    S := OptionNames.GetName(OptionI);
    try
      S := S + ' - [' + OTAOptions.GetOptionValue(S).ToString + ']';
    except
      S := S + ' - [<unassigned>]';
    end;
    ListOptions.Items.Add(S);
  end;
  ListOptions.EndUpdate;
end;

function TOptionsList.GetSelectedOptions: ArrayList;
var
  OptionI: Integer;
begin
  Result := ArrayList.Create;
  for OptionI := 0 to Pred(ListOptions.Items.Count) do
    if ListOptions.GetItemChecked(OptionI) then
      Result.Add(OptionNames.GetName(OptionI));
end;

procedure TOptionsList.BtnOk_Click(sender: System.Object; e: System.EventArgs);
begin
  DialogResult := System.Windows.Forms.DialogResult.OK;
end;

procedure TOptionsList.BtnCancel_Click(sender: System.Object; e: System.EventArgs);
begin
  DialogResult := System.Windows.Forms.DialogResult.Cancel;
end;

end.
