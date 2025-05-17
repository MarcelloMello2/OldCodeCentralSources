{  -----------------------------------------------------------------------------
  Form Workbench
  @Description
  This is the main workbench for the tools housed in this code.

   @author Mr. Arch Brooks
   @version 1.0

  -------------------------------------------------------------------------------

}
unit TopFormwbu;

interface

uses ADODB, Buttons, Classes, ComCtrls, Controls, DB, DBCtrls, Dialogs,
  ExtCtrls, Forms, Graphics, JclFileUtils, JclStrings, JvComCtrls, JvDialogs,
  JvEdit, JvExComCtrls, JvExControls, JvExExtCtrls, JvExStdCtrls,
  JvExtComponent, JvLabel, JvPanel, JvStatusBar, Mask, Menus, StdCtrls,
  SysUtils, Windows;

type
  { *-----------------------------------------------------------------------------
    Top Class For Dialog

    The overall class for utilization of the dialog.

    ------------------------------------------------------------------------------- }
  /// Top Class For Dialog
  TTopFormC = class(TForm)
    /// Connection For Database
    ADOConnection1: TADOConnection;
    /// Data Table Of Database
    ADOTable1: TADOTable;
    /// Change Colors Menu Item
    ChangeColors1: TMenuItem;
    /// DataSource Data Control
    DataSource1: TDataSource;
    /// Date & Time Display
    date_n_time_lbl: TLabel;
    /// Database Edit Control
    DBEdit1: TDBEdit;
    /// Finished Updating Information
    done_btn: TSpeedButton;
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
    /// Elapsed Seconds Timer
    Timer1: TTimer;
    /// Change Dialog Colors
    procedure ChangeColors1Click(Sender: TObject);
    procedure DBEdit1Enter(Sender: TObject);
    procedure done_btnClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    /// The Current Application Path
    CurrentPath: string;
    /// Form Color
    FColor: TColor;
  public
    { Public declarations }
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
  end;

var
  /// Form Pointer for TopForm
  TopFormC: TTopFormC;

implementation

{$R *.dfm}
{ *-----------------------------------------------------------------------------
  Change Application Colors

  This code alows the user to change the color of the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    TopFormC.Color := RColor;
    JvPageControl1.Color := RColor;
    JvPanel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  DB Control Was Entered

  This code puts the DBMS in edit mode for subsequent changes.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.DBEdit1Enter(Sender: TObject);
begin
  DBEdit1.DataSource.DataSet.Edit;
end;

{ *-----------------------------------------------------------------------------
  Basic Execution Database Update

  This code is executed when the user is finished making changes.
  It checks to see in the database control has been modified.
  If modifications were made the post fucntion is called before ending
  execution of the dialog.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.done_btnClick(Sender: TObject);
begin
  if ADOTable1.Modified then
  begin
    ADOTable1.Post;
  end;
  ModalResult := mrOk;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

{ *-----------------------------------------------------------------------------
  Timer Event

  This Code is ececuted when the Timer Tick is registered.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TTopFormC.Timer1Timer(Sender: TObject);
begin
  date_n_time_lbl.Caption := FormatDateTime
    ('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
