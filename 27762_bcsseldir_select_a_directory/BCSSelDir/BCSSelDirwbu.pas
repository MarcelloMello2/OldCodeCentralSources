{ *-----------------------------------------------------------------------------
  Form Workbench

  This is the main workbench for the tools housed in this code.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit BCSSelDirwbu;

interface

uses
  ComCtrls, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvComCtrls, JvDialogs, JvExComCtrls, JvExtComponent,
  JvExExtCtrls, JvStatusBar, JvPanel, Menus, StdCtrls, SysUtils;

type
  { *-----------------------------------------------------------------------------
    Top Class For Dialog

    The overall class for utilization of the dialog.

    ------------------------------------------------------------------------------- }

  TBCSSelDirC = class(TForm)
    /// Change Colors Menu Item
    ChangeColors1: TMenuItem;
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
    /// Wall Clock Timer
    Timer1: TTimer;
    /// Wall Clock Caption
    Label1: TLabel;
    /// Select A Directory Menu Item
    SelDirectory1: TMenuItem;
    SelectDirectoryDialog: TFileOpenDialog;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SelDirectory1Click(Sender: TObject);
  private
    { Private declarations }
    /// Dialog Caption
    FCaption: string;
    /// Form Color
    FColor: TColor;
    /// Selected Directory
    FSelDir: string;
  public
    { Public declarations }
    /// Dialog Caption Property
    property RCaption: string read FCaption write FCaption;
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
    /// Select Directory Property
    property RSelDir: string read FSelDir write FSelDir;
  end;

var
  /// Form Pointer for BCSSelDir
  BCSSelDirC: TBCSSelDirC;

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

procedure TBCSSelDirC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    BCSSelDirC.Color := RColor;
    JvPageControl1.Color := RColor;
    JvPanel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSSelDirC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSSelDirC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSSelDirC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

{ *-----------------------------------------------------------------------------
  Select Directory Execute Command

  This Code is ececuted to prompt the user to select a file directory.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSSelDirC.SelDirectory1Click(Sender: TObject);
begin
  SelectDirectoryDialog.Title := RCaption;
  if SelectDirectoryDialog.Execute then
  begin
    RSelDir := PathAddSeparator(SelectDirectoryDialog.FileName);
  end;
end;

{ *-----------------------------------------------------------------------------
  Timer Event

  This Code is ececuted when the Timer Tick is registered.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSSelDirC.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := FormatDateTime('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
