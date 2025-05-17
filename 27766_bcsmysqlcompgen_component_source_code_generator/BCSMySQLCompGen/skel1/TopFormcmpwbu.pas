{ *-----------------------------------------------------------------------------
  Form Workbench

  Description

  This is the main workbench for the tools housed in this code.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit TopFormCmpwbu;

interface

uses
  DB, ComCtrls, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvComCtrls, JvDialogs, JvExComCtrls, JvExtComponent,
  JvExExtCtrls, JvStatusBar, JvPanel, Menus, StdCtrls, SysUtils, TopFormdp,
  ADODB;

type
  { *-----------------------------------------------------------------------------
    Top Class For Dialog

    The overall class for utilization of the dialog.

    ------------------------------------------------------------------------------- }

  TTopFormCmpC = class(TForm)
    /// Change Colors Menu Item
    ChangeColors1: TMenuItem;
    /// Date And Time Display Label
    date_n_time_lbl: TLabel;
    /// First Exit Point
    Exit1: TMenuItem;
    /// Second Exit Point
    Exit2: TMenuItem;
    /// Invoke Help Sub Systems
    Help1: TMenuItem;
    /// Color Dialog
    JvColorDialog1: TJvColorDialog;
    /// Dialog Page Control
    JvPageControl1: TJvPageControl;
    /// First Dialog Panel
    JvPanel1: TJvPanel;
    /// Status Bar
    JvStatusBar1: TJvStatusBar;
    /// Main Application Menu
    MainMenu1: TMainMenu;
    /// Primary Options Menu
    PrimaryOptoins1: TMenuItem;
    /// Tab Sheet One
    TabSheet1: TTabSheet;
    /// Timer Elapsed Seocnds
    Timer1: TTimer;
    /// Test Component Menu Item
    TestComponent1: TMenuItem;
    /// Component Driver Program
    TopFormdp1: TTopFormdp;
    ADOTable1: TADOTable;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TestComponent1Click(Sender: TObject);
  private
    { Private declarations }
    /// Form Color
    FColor: TColor;
    FBookMark: TBookMark;
    /// Connection String
    FConStr: string;
    /// Data Element Name
    FDEName: string;
    /// Table Name
    FTableName: string;
  public
    { Public declarations }
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
    property RBookMark: TBookMark read FBookMark write FBookMark;
    property RConStr: string read FConStr write FConStr;
    property RDEName: string read FDEName write FDEName;
    property RTableName: string read FTableName write FTableName;
  end;

var
  /// Form Pointer for TopFormCmp
  TopFormCmp: TTopFormCmpC;

implementation

{$R *.dfm}

var
  /// The Current Application Path
  CurrentPath: string;

  { *-----------------------------------------------------------------------------
    Change Application Colors

    This code alows the user to change the color of the application.

    @param Sender The top Object for interface.
    ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    TopFormCmp.Color := RColor;
    JvPageControl1.Color := RColor;
    JvPanel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

{ *-----------------------------------------------------------------------------
  Invoke Component Test

  This triggers the execution of the component.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.TestComponent1Click(Sender: TObject);
begin
  TopFormdp1.RConStr := RConStr;
  TopFormdp1.RTableName := RTableName;
  TopFormdp1.RDEName := RDEName;
  TopFormdp1.RBookMark := RBookMark;
  TopFormdp1.Execute;
end;

{ *-----------------------------------------------------------------------------
  Timer Event

  This Code is ececuted when the Timer Tick is registered.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormCmpC.Timer1Timer(Sender: TObject);
begin
  date_n_time_lbl.Caption := FormatDateTime
    ('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
