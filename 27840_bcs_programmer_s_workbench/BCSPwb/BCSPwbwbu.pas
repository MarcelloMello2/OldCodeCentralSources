{*-----------------------------------------------------------------------------
 Programmer's Workbench Primary Workbench Canvas
 <br>
 The lifeblood  of the BCS Programmers Work Bench is this unit and it
 associated components.  This level of functionality establishes a base line
 of functionality which is primed for further development.

 @author Mr. Arch Brooks
 @version 1.0

 -------------------------------------------------------------------------------}

unit BCSPwbwbu;

interface

uses Classes, ComCtrls, Controls, DB, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvExComCtrls, JvDialogs, JvStatusBar, Menus, OvcBase, OvcNbk,
  StdCtrls, SysUtils, Windows, Grids, DBGrids, JvExDBGrids, JvDBGrid, DBCtrls,
  Mask, JvExControls, JvLabel, RpRave, RpDefine, RpBase, RpSystem, Buttons,
  JvExStdCtrls, JvEdit, ExtDlgs;

type
  {*-----------------------------------------------------------------------------
   Top Class For Dialog

   The overall class for utilization of the dialog.

   -------------------------------------------------------------------------------}

  TBCSPwbC = class(TForm)
    /// Change Colors Menu Item
    ChangeColors1: TMenuItem;
    /// Command Line Parameters Data Entry Area
    DBEdit1: TDBEdit;
    /// Initial Directory For Command Line Start Up
    DBEdit2: TDBEdit;
    /// Primary Data Entry For Launch Candidates
    DBGrid1: TDBGrid;
    /// Documentation For Each Candidate (Overview)
    DBMemo1: TDBMemo;
    /// Categories Data Base Navigator
    DBNavigator1: TDBNavigator;
    /// Candidates Data Base Navigator
    DBNavigator2: TDBNavigator;
    /// Display Current Category
    DBText2: TDBText;
    /// Display Current Launch Candidate
    DBText3: TDBText;
    /// Execute This One Menu Item
    ExecuteThisOne1: TMenuItem;
    /// First Exit Point
    Exit1: TMenuItem;
    /// Second Exit Point
    Exit2: TMenuItem;
    /// File Open Dialog
    FileOpenDialog1: TFileOpenDialog;
    /// Invoke Help Sub Systems
    Help1: TMenuItem;
    /// Color Picker Dialog
    JvColorDialog1: TJvColorDialog;
    /// Categories Database Grid Control
    JvDBGrid1: TJvDBGrid;
    /// Default Brower Path
    JvEdit1: TJvEdit;
    /// Used For Pasting From Clipboard (Non Visible)
    JvEdit2: TJvEdit;
    /// Command Line Parameters Label
    JvLabel1: TJvLabel;
    /// Initial Directory Label
    JvLabel2: TJvLabel;
    /// Default Browser Label
    JvLabel3: TJvLabel;
    /// Application Status Bar
    JvStatusBar1: TJvStatusBar;
    /// List All Executables Menu Item
    ListAllExecutables1: TMenuItem;
    /// Load Command Menu Item
    LoadCommand1: TMenuItem;
    /// Main Application Menu )Contains Primary Options)
    MainMenu1: TMainMenu;
    /// Open File In Noetpad Menu Item
    OpenFileInNotepad1: TMenuItem;
    /// Dialog To Open Text File
    OpenTextFileDialog1: TOpenTextFileDialog;
    /// Main Tabbed Notebook Housing For All Other Tabbed Notebooks
    OvcNotebook1: TOvcNotebook;
    /// Candidates Main Notebook
    OvcNotebook2: TOvcNotebook;
    /// Candidates Extended Data Entry
    OvcNotebook3: TOvcNotebook;
    /// Paste From Clipboard Menu Item
    PasteFromClipboard1: TMenuItem;
    /// Pop Up Menu For Candidates Grid
    PopupMenu1: TPopupMenu;
    /// Primary Options Menu
    PrimaryOptoins1: TMenuItem;
    /// Reports Menu Item
    Reports1: TMenuItem;
    /// Component For Rave Reporting System
    RvSystem1: TRvSystem;
    /// Project File For Rave Project
    RvProject1: TRvProject;
    /// Our Clock Dispaly
    RClockDisplay: TLabel;
    /// Change Default Browser Control
    SpeedButton1: TSpeedButton;
    /// Timer Component
    Timer1: TTimer;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LoadCommand1Click(Sender: TObject);
    procedure ExecuteThisOne1Click(Sender: TObject);
    procedure JvDBGrid1CellClick(Column: TColumn);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure ListAllExecutables1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure PasteFromClipboard1Click(Sender: TObject);
    procedure OpenFileInNotepad1Click(Sender: TObject);
  private
    {Private declarations}
    /// Dialog Color
    FColor: TColor;
    /// Message Box Wrapper
    function RMsgBox(RTitle, RCaption: string): integer;
  public
    {Public declarations}
    /// Dialog Color Property
    property RColor: TColor Read FColor Write FColor;
    /// Set Application Colors
    procedure SetColors;
  end;

var
  /// Form Pointer for BCSPwb
  BCSPwbC: TBCSPwbC;
  /// Temporary storage buffer
  buf: string;

implementation

{$R *.dfm}

uses BCSPwbdmu;

var
  /// The Current Application Path
  CurrentPath: string;

  {*-----------------------------------------------------------------------------
   Change Application Colors

   This code alows the user to change the color of the application.

   @param Sender The top Object for interface.
   -------------------------------------------------------------------------------}

procedure TBCSPwbC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    SetColors;
  end;
end;

{*------------------------------------------------------------------------------
 DBGrid Title Click Event

 This event is fired when the title column(s) of the DBGrid is clicked.

 @param Column   DBGrid Column
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.DBGrid1TitleClick(Column: TColumn);
begin
  BCSPwbdmu.BCSPwbD.ADOTable2.Sort := Column.FieldName;
end;

{*------------------------------------------------------------------------------
 Execute This One Event

 This event is fired when the ExecuteThisOne Menu Item is Clicked.

 @param Sender   Primary Class Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.ExecuteThisOne1Click(Sender: TObject);
var
  cmd: AnsiString;
begin
  cmd := BCSPwbdmu.BCSPwbD.ADOTable2.FieldByName('xqt_cmd').AsString;
  if pos('http', cmd) = 1 then
  begin
    cmd := Trim(JvEdit1.Text) + ' ' + cmd;
  end;
  Winexec(PAnsiChar(cmd), sw_normal);
end;

{*-----------------------------------------------------------------------------
 Exit Our Application Exit Point One

 This Code hides the application.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{*-----------------------------------------------------------------------------
 Exit Our Application Exit Point Two

 This Code hides the application.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{*------------------------------------------------------------------------------
 Activate Form

 This event is initiated when the dialog initiated

 @param Sender   Parent Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.FormActivate(Sender: TObject);
var
  ii: integer;
begin
  if RColor > -1 then
  begin
    SetColors;
  end;
end;

{*-----------------------------------------------------------------------------
 Form Create

 This Code is ececuted when the application is created.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbC.FormCreate(Sender: TObject);
begin
  buf := ExtractFilePath(ParamStr(0));
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

{*------------------------------------------------------------------------------
 JvDBGrid Cell Click Event

 This event is fired when the user clicks on the grid.

 @param Column   Primary Class Ogject
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.JvDBGrid1CellClick(Column: TColumn);
begin
  OvcNotebook1.PageIndex := 1;
end;

{*------------------------------------------------------------------------------

 List All Executables Event

 This event is fired when the ListAllExecutables Menu Item is clicked.

 @param Sender   Primaru Class Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.ListAllExecutables1Click(Sender: TObject);
begin
  RvProject1.ExecuteReport('Report1');
end;

{*------------------------------------------------------------------------------
 Load Command Event

 This event gets the executabel path for all windows Executables.
 @param Sender   Class Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.LoadCommand1Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    BCSPwbdmu.BCSPwbD.ADOTable2.Edit;
    BCSPwbdmu.BCSPwbD.ADOTable2.FieldByName('xqt_cmd').AsString :=
      FileOpenDialog1.FileName;
    BCSPwbdmu.BCSPwbD.ADOTable2.Post;
  end;
end;

{*------------------------------------------------------------------------------
 Open File In Notepad Event

 This event is fired when the menu item is clicked.

 @param Sender   Class Primary Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.OpenFileInNotepad1Click(Sender: TObject);
var
  cmd: AnsiString;
begin
  OpenTextFileDialog1.Execute(Handle);
  If OpenTextFileDialog1.FileName > '' then
  begin
    cmd := 'notepad.exe' + ' ' + OpenTextFileDialog1.FileName;
    Winexec(PAnsiChar(cmd), sw_normal);
  end;
end;

{*------------------------------------------------------------------------------
 Paste From Cliboard

 This popup menu item is clicked when the conrtent for the command line
 is to pasted for a URL copied to the clipboard.

 @param Sender   Class Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.PasteFromClipboard1Click(Sender: TObject);
begin
  JvEdit2.Text := '';
  JvEdit2.PasteFromClipboard;
  BCSPwbdmu.BCSPwbD.ADOTable2.Edit;
  BCSPwbdmu.BCSPwbD.ADOTable2.FieldByName('xqt_cmd').AsString := JvEdit2.Text;
  BCSPwbdmu.BCSPwbD.ADOTable2.Post;
end;

{*-----------------------------------------------------------------------------
 Message Box Control

 This Code is ececuted the standard message box is required for the user.

 @param RTitle The Message Text.
 @param RCaption Message Box Caption.
 @return Result From Message Box Execution
 -------------------------------------------------------------------------------}

function TBCSPwbC.RMsgBox(RTitle, RCaption: string): integer;
begin
  // RMsgBox('The Associated Field Cannot Be Blank!', 'Blank Field Detected!');
  result := Application.MessageBox(PWideChar(RTitle), PWideChar(RCaption),
    mb_OkCancel);
end;

{*------------------------------------------------------------------------------
 Set Colors

 This code sets the colors for the RColor Property

 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.SetColors;
begin
  BCSPwbC.Color := RColor;
  JvStatusBar1.Color := RColor;
  OvcNotebook1.Color := RColor;
  OvcNotebook2.Color := RColor;
end;

{*------------------------------------------------------------------------------
 Speed Button 1

 This item is fired when the user clicks the speed button 1 item.

 @param Sender   Class Object
 ------------------------------------------------------------------------------*}

procedure TBCSPwbC.SpeedButton1Click(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    JvEdit1.Text := FileOpenDialog1.FileName;
  end;
end;

{*-----------------------------------------------------------------------------
 Timer Event

 This Code is ececuted when the Timer Tick is registered.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbC.Timer1Timer(Sender: TObject);
begin
  RClockDisplay.Caption := FormatDateTime
    ('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
