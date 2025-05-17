{ *-----------------------------------------------------------------------------
  Form Workbench

  This is the main workbench for the tools housed in this code.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit BCSMySQLCompGenwbu;

interface

uses Buttons, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JclStrings, JvComCtrls, JvDialogs, JvEdit, JvExComCtrls,
  JvExControls, JvExStdCtrls, JvLabel, JvExExtCtrls, JvExtComponent,
  JvPanel, JvStatusBar, Menus, StdCtrls, SysUtils, Windows;

type
  { *-----------------------------------------------------------------------------
    Top Class For Dialog

    The overall class for utilization of the dialog.

    ------------------------------------------------------------------------------- }

  TBCSMySQLCompGenC = class(TForm)
    /// Change Colors Menu Item
    ChangeColors1: TMenuItem;
    /// First Exit Point
    Exit1: TMenuItem;
    /// Second Exit Point
    Exit2: TMenuItem;
    /// Invoke Help Sub Systems
    Help1: TMenuItem;
    ColorSelector: TJvColorDialog;
    PageControl1: TJvPageControl;
    Panel1: TJvPanel;
    StatusBar1: TJvStatusBar;
    PrimaryOptions: TMainMenu;
    /// Primary Options Menu
    PrimaryOptoins1: TMenuItem;
    /// Tab Sheet One
    TabSheet1: TTabSheet;
    time_diaplay_lbl: TLabel;
    family_id_lbl: TJvLabel;
    path_lbl: TJvLabel;
    famid_ed: TJvEdit;
    path_ed: TJvEdit;
    generate_btn: TSpeedButton;
    GetPath_dlg: TFileOpenDialog;
    dsn_lbl: TJvLabel;
    table_name_lbl: TJvLabel;
    data_element_lbl: TJvLabel;
    dsn_ed: TJvEdit;
    tablename_ed: TJvEdit;
    dename_ed: TJvEdit;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure generate_btnClick(Sender: TObject);
    procedure path_lblClick(Sender: TObject);
  private
    { Private declarations }
    /// Form Color
    FColor: TColor;
    filebuf: AnsiString;
    ifname: string;
    inpath: string;
    ofname: string;
    ofpath: string;
    sc: TStringList;
  public
    { Public declarations }
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
    procedure CopyBinary(infile, outfile: string);
    procedure GenerateComponentPackaging;
    procedure GeneratePackageTester;
    procedure GenerateTheCode;
    procedure GenerateStandAlone;
    procedure ReadFile(infile: string);
    procedure RepStr(fromval, toval: string);
    procedure WriteFile(outfile: string);
  end;

var
  /// Form Pointer for BCSMySQLCompGen
  BCSMySQLCompGenC: TBCSMySQLCompGenC;

implementation

{$R *.dfm}

var
  /// The Current Application Path
  CurrentPath: string;

procedure TBCSMySQLCompGenC.CopyBinary(infile, outfile: string);
begin
  FileCopy(infile, outfile, true);
end;

procedure TBCSMySQLCompGenC.GenerateComponentPackaging;
begin
  ReadFile(inpath + 'TopFormpk.dproj');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'pk.dproj');

  ReadFile(inpath + 'TopFormpk.dpk');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'pk.dpk');

  ReadFile(inpath + 'TopFormdp.pas');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'dp.pas');

  CopyBinary(inpath + 'TopFormpk.res', ofpath + famid_ed.Text + 'pk.res');

end;

procedure TBCSMySQLCompGenC.GeneratePackageTester;
begin
  ReadFile(inpath + 'TopFormcmpml.dproj');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'cmpml.dproj');

  ReadFile(inpath + 'TopFormcmpml.dpr');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'cmpml.dpr');

  ReadFile(inpath + 'TopFormcmpwbu.pas');
  RepStr('TopForm', famid_ed.Text);
  RepStr('bcswebtools', dsn_ed.Text);
  RepStr('categoreis', tablename_ed.Text);
  RepStr('categories', dename_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'cmpwbu.pas');

  ReadFile(inpath + 'TopFormcmpwbu.dfm');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'cmpwbu.dfm');

  CopyBinary(inpath + 'TopFormcmpml.res', ofpath + famid_ed.Text + 'cmpml.res');

end;

procedure TBCSMySQLCompGenC.GenerateStandAlone;
begin
  ReadFile(inpath + 'TopFormml.dproj');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'ml.dproj');

  ReadFile(inpath + 'TopFormml.dpr');
  RepStr('TopForm', famid_ed.Text);
  RepStr('bcswebtools', dsn_ed.Text);
  RepStr('categoreis', tablename_ed.Text);
  RepStr('categories', dename_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'ml.dpr');

  ReadFile(inpath + 'TopFormwbu.pas');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'wbu.pas');

  ReadFile(inpath + 'TopFormwbu.dfm');
  RepStr('TopForm', famid_ed.Text);
  WriteFile(ofpath + famid_ed.Text + 'wbu.dfm');

  sc.Add('copy ' + ofpath + famid_ed.Text + 'wbu.dfm' + ' c:\xp\dcu\' +
      famid_ed.Text + 'wbu.dfm');
  sc.Add('copy ' + ofpath + famid_ed.Text + 'wbu.dcu' + ' c:\xp\dcu\' +
      famid_ed.Text + 'wbu.dcu');
  sc.SaveToFile(ofpath + 'CopyDcu.Bat');

  CopyBinary(inpath + 'TopFormml.res', ofpath + famid_ed.Text + 'ml.res');

end;

procedure TBCSMySQLCompGenC.GenerateTheCode;
begin
  sc := TStringList.Create;
  ofpath := path_ed.Text;
  ofpath := PathAddSeparator(ofpath);
  ofpath := ofpath + famid_ed.Text;
  ofpath := PathAddSeparator(ofpath);
  JclFileUtils.ForceDirectories(ofpath);
  inpath := CurrentPath + 'skel1';
  inpath := PathAddSeparator(inpath);
  GenerateStandAlone;
  GenerateComponentPackaging;
  GeneratePackageTester;
  sc.Free;
end;

procedure TBCSMySQLCompGenC.ReadFile(infile: string);
begin
  filebuf := FileToString(infile);
end;

procedure TBCSMySQLCompGenC.RepStr(fromval, toval: string);
begin
  filebuf := StringReplace(filebuf, fromval, toval, [rfReplaceAll]);
end;

procedure TBCSMySQLCompGenC.WriteFile(outfile: string);
begin
  StringToFile(outfile, filebuf, false);
end;

{ *-----------------------------------------------------------------------------
  Change Application Colors

  This code alows the user to change the color of the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSMySQLCompGenC.ChangeColors1Click(Sender: TObject);
begin
  if ColorSelector.Execute(Handle) then
  begin
    RColor := ColorSelector.Color;
    BCSMySQLCompGenC.Color := RColor;
    PageControl1.Color := RColor;
    Panel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSMySQLCompGenC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSMySQLCompGenC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSMySQLCompGenC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

procedure TBCSMySQLCompGenC.generate_btnClick(Sender: TObject);
begin
  if (famid_ed.Text = '') then
  begin
    Application.MessageBox('The Family Id Must Not Be Blank!',
      'Blank Family Id Detected!', mb_OkCancel);
    famid_ed.SetFocus;
    exit;
  end;
  if (path_ed.Text = '') then
  begin
    Application.MessageBox('The Target Path Must Not Be Blank!',
      'Blank Target Path Detected!', mb_OkCancel);
    if (GetPath_dlg.Execute) then
    begin
      path_ed.Text := GetPath_dlg.FileName;
    end;
    famid_ed.SetFocus;
    exit;
  end;
  if (dsn_ed.Text = '') then
  begin
    Application.MessageBox('The DSN Must Not Be Blank!', 'Blank DSN Detected!',
      mb_OkCancel);
    dsn_ed.SetFocus;
    exit;
  end;
  if (tablename_ed.Text = '') then
  begin
    Application.MessageBox('The Table Name Must Not Be Blank!',
      'Blank Table Name Detected!', mb_OkCancel);
    tablename_ed.SetFocus;
    exit;
  end;
  if (dename_ed.Text = '') then
  begin
    Application.MessageBox('The Date Element Name Must Not Be Blank!',
      'Blank Data Element Name Detected!', mb_OkCancel);
    dename_ed.SetFocus;
    exit;
  end;
  GenerateTheCode;
  Close;
end;

procedure TBCSMySQLCompGenC.path_lblClick(Sender: TObject);
begin
  if (GetPath_dlg.Execute) then
  begin
    path_ed.Text := GetPath_dlg.FileName;
  end;
end;

{ *-----------------------------------------------------------------------------
  Timer Event

  This Code is ececuted when the Timer Tick is registered.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSMySQLCompGenC.Timer1Timer(Sender: TObject);
begin
  time_diaplay_lbl.Caption := FormatDateTime
    ('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
