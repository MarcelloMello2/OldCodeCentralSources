{*-----------------------------------------------------------------------------
Establish Local and Remote Domans In .NET Applications

  This is the main workbench for the domain desigantion sub system.

  @Author Mr. Arch Brooks
  @Version 1.0

-------------------------------------------------------------------------------}

unit BCSChgAspwbu;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvComCtrls, JvClock, JvDialogs, JvExComCtrls,
  JvExExtCtrls, JvExtComponent, JvPanel, JvStatusBar, Menus, SysUtils,
  JvExControls, JvSpeedButton, BCSRolfdp, BCSVistaOpendp, StdCtrls,
  JvExStdCtrls, JvEdit, JclStrings, JvLabel;

type
{*-----------------------------------------------------------------------------
Top Class For Dialog

  The overall class for utilization of the dialog.

-------------------------------------------------------------------------------}

  TBCSChgAspC = class(TForm)
    ///Change Colors Menu Item
    ChangeColors1  : TMenuItem;
    /// First Exit Point
    Exit1  : TMenuItem;
    /// Second Exit Point
    Exit2  : TMenuItem;
    /// Invoke Help Sub Systems
    Help1  : TMenuItem;
    /// System Clock Display
    JvClock1  : TJvClock;
    /// Color Dialog
    JvColorDialog1  : TJvColorDialog;
    /// Status Bar
    JvStatusBar1  : TJvStatusBar;
    /// Main Application Menu
    MainMenu1  : TMainMenu;
    /// Primary Options Menu
    PrimaryOptoins1  : TMenuItem;
    /// BCS Vista File Dialogs
    BCSVistaOpen_c1  : TBCSVistaOpen_c;
    /// BCS Return A List Of Files
    BCSARolf1  : TBCSARolf;
    /// Prod To Local
    JvSpeedButton2: TJvSpeedButton;
    /// Local Domain File Name
    JvEdit2: TJvEdit;
    /// Remote Domain File Name
    JvEdit1: TJvEdit;
    /// Local To Prod
    JvSpeedButton1: TJvSpeedButton;
    /// Local Domain Label
    JvLabel1: TJvLabel;
    /// Remote Domain Label
    JvLabel2: TJvLabel;
    procedure Exit1Click(Sender  : TObject);
    procedure Exit2Click(Sender  : TObject);
    procedure FormCreate(Sender  : TObject);
    procedure ChangeColors1Click(Sender  : TObject);
    procedure JvSpeedButton1Click(Sender  : TObject);
    procedure JvSpeedButton2Click(Sender  : TObject);
  private
    { Private declarations }
    /// Form Color
    FColor  : TColor;
  public
    { Public declarations }
    /// Color Property
    property RColor  : TColor Read FColor Write FColor;
    procedure DoList(ext  : String);
  end;

var
  /// Form Pointer for BCSChgAsp
  BCSChgAsp  : TBCSChgAspC;
  /// Directory
  dir  : String;
  /// From Content
  rfrom  : String;
  /// To Content
  rto  : String;
  /// Incremental Counter
  ii  : LongInt;
  /// File Buffer
  fbuf  : AnsiString;

implementation

{$R *.dfm}

var
  /// The Current Application Path
  CurrentPath  : string;

{*-----------------------------------------------------------------------------
Change Application Colors

  This code alows the user to change the color of the application.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.ChangeColors1Click(Sender  : TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    BCSChgAsp.Color := RColor;
  end;
end;

{*-----------------------------------------------------------------------------
Exit Our Application Exit Point One

  This Code hides the application.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.Exit1Click(Sender  : TObject);
begin
  Close;
end;

{*-----------------------------------------------------------------------------
Exit Our Application Exit Point Two

  This Code hides the application.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.Exit2Click(Sender  : TObject);
begin
  Close;
end;

{*-----------------------------------------------------------------------------
Form Create

  This Code is ececuted when the application is created.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.FormCreate(Sender  : TObject);
begin
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
  ChDir(ExtractFilePath(ParamStr(0)));
end;

{*-----------------------------------------------------------------------------
Process List Of Files

  This Retrieves a list of files that needs to be updated.

@param Ext The Extension of the Files to Be Updated.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.DoList(ext  : String);
begin
  BCSARolf1.RFileExt := ext;
  BCSARolf1.RFilePath := dir;
  BCSARolf1.sExecute;
  ii := 0;
  while (ii <= (BCSARolf1.RFiles.Count - 1)) do
  begin
    fbuf := FileToString(BCSARolf1.RFiles[ii]);
    fbuf := StringReplace(fbuf, rfrom, rto, [rfReplaceAll, rfIgnoreCase]);
    StringToFile(BCSARolf1.RFiles[ii], fbuf);
    Inc(ii);
  end;
end;

{*-----------------------------------------------------------------------------
Local Host To Procution Domain

  This button initiates changes the local Host designatoin to the
  production designation.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.JvSpeedButton1Click(Sender  : TObject);
begin
  dir := '';
  if BCSVistaOpen_c1.SelectADirectory then
  begin
    dir := PathAddSeparator(BcsVistaOpen_c1.RDirName);
    dir := dir;
  end;
  if dir <> '' then
  begin
    rfrom := JvEdit1.Text;
    rto := JvEdit2.Text;
    DoList('*.master');
    DoList('*.aspx');
    DoList('*.pas');
  end;
end;

{*-----------------------------------------------------------------------------
Production Domain To Local Host

  This button initiates changes from the procuction domain to
  the local host.

@param Sender The top Object for interface.
-------------------------------------------------------------------------------}

procedure TBCSChgAspC.JvSpeedButton2Click(Sender  : TObject);
begin
  dir := '';
  if BCSVistaOpen_c1.SelectADirectory then
  begin
    dir := PathAddSeparator(BcsVistaOpen_c1.RDirName);
    dir := dir;
  end;
  if dir <> '' then
  begin
    rfrom := JvEdit2.Text;
    rto := JvEdit1.Text;
    DoList('*.master');
    DoList('*.aspx');
    DoList('*.pas');
  end;
end;

end.
