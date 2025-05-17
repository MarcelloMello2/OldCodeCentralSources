{*-----------------------------------------------------------------------------
 Workbench For Brooks Computing Systems Programmers Workbench Component Tester
 <br>
 This is the main workbench which launches the compomet comprising the
 Brooks Computing Systems Programmer's Workbench Component.

 @Author Mr. Arch Brooks
 @Version 1.0

 -------------------------------------------------------------------------------}

unit BCSPwbcmpwbu;

interface

uses Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Forms, Graphics,
  JclFileUtils, JvExComCtrls, JvDialogs, JvStatusBar, Menus, OvcBase, OvcNbk,
  StdCtrls, SysUtils, Windows, BCSPwbdp;

type
  {*-----------------------------------------------------------------------------
   Primary Class for the Programmers Workbency
   <br>
   The primary class for invoking the Programmers Workbench.  Of course with
   the component architecture a new component can be created based on this
   component then further enhancements to the tool are avaiable.

   -------------------------------------------------------------------------------}

  TBCSPwbcmpC = class(TForm)
    /// Programmer's Workbench Component Package
    BCSPwbdp1: TBCSPwbdp;
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
    /// Status Bar
    JvStatusBar1: TJvStatusBar;
    /// Main Application Menu
    MainMenu1: TMainMenu;
    /// Tab Notebook
    OvcNotebook1: TOvcNotebook;
    /// Primary Options Menu
    PrimaryOptoins1: TMenuItem;
    /// Our Clock Dispaly
    RClockDisplay: TLabel;
    /// Test Component Menu Item
    TestComponent1: TMenuItem;
    /// Timer Component
    Timer1: TTimer;
    procedure Exit1Click(Sender: TObject);
    procedure Exit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ChangeColors1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TestComponent1Click(Sender: TObject);
  private
    {Private declarations}
    /// Form Color
    FColor: TColor;
    /// Message Box Wrapper
    function RMsgBox(RTitle, RCaption: string): integer;
  public
    {Public declarations}
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
  end;

var
  /// Class Pointer for BCS Programmer's Workbench Component
  BCSPwbcmpC: TBCSPwbcmpC;

implementation

{$R *.dfm}

var
  /// The Current Application Path For This executable
  CurrentPath: string;

  {*-----------------------------------------------------------------------------
   Change Application Colors

   This code alows the user to change the color of the application.

   @param Sender The top Object for interface.
   -------------------------------------------------------------------------------}

procedure TBCSPwbcmpC.ChangeColors1Click(Sender: TObject);
begin
  if JvColorDialog1.Execute(Handle) then
  begin
    RColor := JvColorDialog1.Color;
    BCSPwbcmpC.Color := RColor;
    JvStatusBar1.Color := RColor;
    OvcNotebook1.Color := RColor;
  end;
end;

{*-----------------------------------------------------------------------------
 Exit Our Application Exit Point One

 This Code hides the application.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbcmpC.Exit1Click(Sender: TObject);
begin
  Close;
end;

{*-----------------------------------------------------------------------------
 Exit Our Application Exit Point Two

 This Code hides the application.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbcmpC.Exit2Click(Sender: TObject);
begin
  Close;
end;

{*-----------------------------------------------------------------------------
 Form Create

 This Code is ececuted when the application is created.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbcmpC.FormCreate(Sender: TObject);
begin
  ChDir(ExtractFilePath(ParamStr(0)));
  CurrentPath := GetCurrentDir;
  CurrentPath := PathAddSeparator(CurrentPath);
end;

{*-----------------------------------------------------------------------------
 Message Box Control

 This Code is ececuted the standard message box is required for the user.

 @param RTitle The Message Text.
 @param RCaption Message Box Caption.
 @return Result From Message Box Execution
 -------------------------------------------------------------------------------}

function TBCSPwbcmpC.RMsgBox(RTitle, RCaption: string): integer;
begin
  // RMsgBox('The Associated Field Cannot Be Blank!', 'Blank Field Detected!');
  result := Application.MessageBox(PWideChar(RTitle), PWideChar(RCaption),
    mb_OkCancel);
end;

{*------------------------------------------------------------------------------
 Test Compomemt Event

 This event is fired when the user clicks the Test Component Option

 @param Sender   ParameterDescription
 ------------------------------------------------------------------------------*}

procedure TBCSPwbcmpC.TestComponent1Click(Sender: TObject);
begin
  BCSPwbdp1.RConnStr :=
    'Provider=MSDASQL.1;Persist Security Info=False;Data Source=bcspwb';
  BCSPwbdp1.RTabName := 'cats';
  BCSPwbdp1.RColor := RColor;
  BCSPwbdp1.Execute;
end;

{*-----------------------------------------------------------------------------
 Timer Event

 This Code is ececuted when the Timer Tick is registered.

 @param Sender The top Object for interface.
 -------------------------------------------------------------------------------}

procedure TBCSPwbcmpC.Timer1Timer(Sender: TObject);
begin
  RClockDisplay.Caption := FormatDateTime
    ('dddd, mmmm dd, yyyy hh:mm:ss   ', now);
end;

end.
