{*-----------------------------------------------------------------------------
 Unit Name: frm022u
 Date:      08-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit frm022u;

interface

uses frm001u, Graphics, MySQLUniProvider, Vcl.ComCtrls, Vcl.DBCtrls,
  Vcl.DBGrids, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, System.Classes, SysUtils,
  uni, Winapi.ShellAPI, Winapi.Windows;

type

  /// Dialog Record Area
  frm022_area = record
    /// Caption
    Caption: string;
  end;

  /// Main Class
  Tfrm022 = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; Caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);

  public
    procedure frmShowModal;
    function RetTag(den: string): string;

  end;

var
  /// Form Handle
  frm022c: Tfrm022;
  /// Record Area
  rpm022: frm022_area;

implementation

// uses System.Win;

var
  /// Default Color
  defColor: TColor;
  /// Default Font
  defFont: TFont;
  /// Dialog For Colors
  dlgColors: TColorDialog;
  /// Dialog For FOnts
  dlgFont: TFontDialog;
  /// Main Menu
  menMain: TMainMenu;
  /// Status Bar Declaration
  sta000: TStatusBar;
  /// Timer Control
  tim000: TTimer;
  /// Menu Item
  tmi: TMenuItem;
  /// DB Connection
  ucon: TUniConnection;
  /// Table for Look Up
  uta: TUniTable;

  {*-----------------------------------------------------------------------------
   Procedure: estMainMenu
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      25-Jul-2016
   @Param     None
   @Return    None
   -----------------------------------------------------------------------------}
procedure Tfrm022.estMainMenu;
var
  mun: string;
  procedure TopLevel(cap: String);
  begin
    mun := cap;
    mun := StringReplace(mun, ' ', '', [rfReplaceAll]);
    tmi := TMenuItem.Create(menMain);
    tmi.Name := 'mui' + mun;
    tmi.Caption := cap;
    // tmi.OnClick := OnMenuClick;
    menMain.Items.Add(tmi);
  end;

  procedure FirstLevel(cap: string; itm: Integer);
  begin
    mun := cap;
    mun := StringReplace(mun, ' ', '', [rfReplaceAll]);
    tmi := TMenuItem.Create(menMain);
    tmi.Caption := cap;
    tmi.Name := 'mui' + mun;
    tmi.OnClick := OnMenuClick;
    menMain.Items[itm].Add(tmi);
  end;

begin
  menMain := TMainMenu.Create(frm022c);
  TopLevel('Primary Options');
  TopLevel('Utils');
  // TopLevel('Ok');
  FirstLevel('Exit', 0);
  FirstLevel('Colors', 1);
  FirstLevel('Fonts', 1);
  FirstLevel('Help', 1);
  FirstLevel('Styles', 1);
end;

{*-----------------------------------------------------------------------------
 Procedure: estStatus
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     pan1, pan2, int: Integer
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  frm022c.InsertControl(sta000);
  tim000 := TTimer.Create(frm022c);
  tim000.Interval := int;
  tim000.OnTimer := OnTimer;
end;

{*-----------------------------------------------------------------------------
 Procedure: estWindow
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     top, width, height: Integer; caption: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.estWindow(top, width, height: Integer; Caption: string);
begin
  frm022c := Tfrm022.CreateNew(Application);
  frm022c.top := top;
  frm022c.Position := poScreenCenter;
  frm022c.width := width;
  frm022c.height := height;
  frm022c.Caption := Caption;
  dlgFont := TFontDialog.Create(frm022c);
  dlgColors := TColorDialog.Create(frm022c);
  dlgColors.Options := [cdFullOpen];
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.frmShowModal;
begin
  estWindow(27, 500, 300, rpm022.Caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  frm022c.ShowModal;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.OnMenuClick(Sender: TObject);
var
  buf: string;
  x: Integer;
begin
  With Sender as TMenuItem do
  begin
    buf := name;
    buf := buf;
  end;
  if buf = 'muiExit' then
  begin
    frm022c.Close;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(frm022c.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (frm022c.ComponentCount - 1) do
    begin
      if (frm022c.Components[x] is TDBEdit) or
        (frm022c.Components[x] is TDBGrid) or (frm022c.Components[x] is TDBMemo)
        or (frm022c.Components[x] is TDBText) or
        (frm022c.Components[x] is TComboBox) or (frm022c.Components[x] is TEdit)
        or (frm022c.Components[x] is TLabel) or
        (frm022c.Components[x] is TListBox) or (frm022c.Components[x] is TMemo)
      then
        TButton(frm022c.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(frm022c.Handle);
    defColor := dlgColors.Color;
  end;
  if buf = 'muiHelp' then
  begin
    XQT('http://bcsjava.com/doc/app/BCS%20Compile%20Delphi%20Projects.htm', '',
      '');
  end;
  if buf = 'muiStyles' then
  begin
    frm001c.frmShowModal
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnTimer
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.OnTimer(Sender: TObject);
begin
  sta000.Panels[1].Text :=
    FormatDateTime('dddd, mmm dd, yyyy hh:mm:ss      ', now);
end;

{*-----------------------------------------------------------------------------
 Procedure: RetTag
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      25-Jul-2016
 @Param     den: string
 @Return    string
 -----------------------------------------------------------------------------}
function Tfrm022.RetTag(den: string): string;
var
  res: string;
begin
  ucon := TUniConnection.Create(Application);
  with ucon do
  begin
    Database := 'de';
    ConnectString :=
      'Provider Name=MySQL;Login Prompt=False;User ID=bcs;Password=' +
      'Peace007' + ';' + 'Data Source=localhost;Database=' + 'de' +
      ';Port=3306';
    Connected := true;
    LoginPrompt := false;
  end;
  uta := TUniTable.Create(Application);
  uta.Connection := ucon;
  uta.TableName := 'dl';
  uta.Open;
  if uta.Locate('dna', trim(den), []) then
  begin
    res := uta.FieldByName('lab').AsString;
  end
  else
  begin
    res := '';
  end;

  result := res;
  uta.Free;
  ucon.Free;
end;

{*-----------------------------------------------------------------------------
 Procedure: XQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     cmd, parm, defPath: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm022.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(frm022c.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

end.
