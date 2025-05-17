{ *-----------------------------------------------------------------------------
  Form Workbench

  This is the main workbench for the tools housed in this code.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit BCSDirSizewbu;

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

  TBCSDirSizeC = class(TForm)
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
    GetDirs1: TMenuItem;
    ListBox1: TListBox;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GetDirs1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
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
  /// Form Pointer for BCSDirSize
  BCSDirSizeC: TBCSDirSizeC;

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

procedure TBCSDirSizeC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    BCSDirSizeC.Color := RColor;
    JvPageControl1.Color := RColor;
    JvPanel1.Color := RColor;
  end;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point One

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Exit Our Application Exit Point Two

  This Code hides the application.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{ *-----------------------------------------------------------------------------
  Form Create

  This Code is ececuted when the application is created.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

procedure TBCSDirSizeC.GetDirs1Click(Sender: TObject);
var
  sc: TStringList;
  ii: LongInt;
  fname: String;
begin
  sc := TStringList.Create;
  JclFileUtils.AdvBuildFileList('C:\users\archman\downloads\*.*', faDirectory,
    sc, amExact, [flFullNames, flRecursive]);
  ListBox1.Items.Clear;
  ii := 0;
  Repeat
    fname := ExtractFileName(sc[ii]);
    if fname[1] <> '.' then
    begin
      ListBox1.Items.add
        (FormatFloat('#,##0.00', (GetDirectorySize(sc[ii]) / (1024 * 1024)))
          + ' ' + sc[ii]);
    end;
    Inc(ii);
  Until (ii > (sc.Count - 1));
end;

procedure TBCSDirSizeC.ListBox1Click(Sender: TObject);
begin

end;

{ *-----------------------------------------------------------------------------
  Timer Event

  This Code is ececuted when the Timer Tick is registered.

  @param Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

procedure TBCSDirSizeC.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := FormatDateTime('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
