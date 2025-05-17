{ *-----------------------------------------------------------------------------
  Form Workbench

  This is the main workbench for the tools housed in this code.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit BCSDirSizeCmpwbu;

interface

uses
  ComCtrls, Classes, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvComCtrls, JvDialogs, JvExComCtrls, JvExtComponent,
  JvExExtCtrls, JvStatusBar, JvPanel, Menus, StdCtrls, SysUtils, BCSDirSizedp;

type
  { *-----------------------------------------------------------------------------
    Top Class For Dialog

    The overall class for utilization of the dialog.

    ------------------------------------------------------------------------------- }

  TBCSDirSizeCmpC = class(TForm)
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
    Timer1: TTimer;
    Label1: TLabel;
    BCSDirSizedp1: TBCSDirSizedp;
    estComponent1: TMenuItem;
    ListBox1: TListBox;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure estComponent1Click(Sender: TObject);
  private
    { Private declarations }
    /// Form Color
    FColor: TColor;
  public
    { Public declarations }
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
  end;

var
  /// Form Pointer for BCSDirSizeCmp
  BCSDirSizeCmp: TBCSDirSizeCmpC;

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

procedure TBCSDirSizeCmpC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    BCSDirSizeCmp.Color := RColor;
    JvPageControl1.Color := RColor;
    JvPanel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeCmpC.estComponent1Click(Sender: TObject);
begin
  BCSDirSizedp1.Execute;
  ListBox1.Items.Assign(BCSDirSizedp1.RDirectoryStrings);
end;

procedure TBCSDirSizeCmpC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeCmpC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeCmpC.FormCreate(Sender: TObject);
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

procedure TBCSDirSizeCmpC.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := FormatDateTime('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
