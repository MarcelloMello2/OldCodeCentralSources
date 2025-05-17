{*-----------------------------------------------------------------------------
 Unit Name: frm016u
 Date:      08-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit frm016u;

interface

uses frm001u, Graphics, Vcl.Controls, Vcl.ComCtrls, Vcl.DBCtrls, Vcl.DBGrids,
  Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, System.Classes, SysUtils,
  Winapi.ShellAPI, Winapi.Windows;

type

  /// Dialog Record Area
  frm016_area = record
    /// Caption
    caption: string;
    /// File Name
    fileName: String;
  end;

  /// Main Class
  Tfrm016 = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);

  public
    procedure frmShowModal;

  end;

var
  /// Form Handle
  frm016c: Tfrm016;
  /// Record Area
  rp016: frm016_area;

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
  /// Select File Dialog
  dlgSelFile: TFileOpenDialog;

  {*-----------------------------------------------------------------------------
   Procedure: estMainMenu
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      04-May-2016
   @Param     None
   @Return    None
   -----------------------------------------------------------------------------}
procedure Tfrm016.estMainMenu;
var
  mun: string;
  procedure TopLevel(cap: String);
  begin
    mun := cap;
    mun := StringReplace(mun, ' ', '', [rfReplaceAll]);
    tmi := TMenuItem.Create(menMain);
    tmi.Name := 'mui' + mun;
    tmi.caption := cap;
    // tmi.OnClick := OnMenuClick;
    menMain.Items.Add(tmi);
  end;

  procedure FirstLevel(cap: string; itm: Integer);
  begin
    mun := cap;
    mun := StringReplace(mun, ' ', '', [rfReplaceAll]);
    tmi := TMenuItem.Create(menMain);
    tmi.caption := cap;
    tmi.Name := 'mui' + mun;
    tmi.OnClick := OnMenuClick;
    menMain.Items[itm].Add(tmi);
  end;

begin
  menMain := TMainMenu.Create(frm016c);
  TopLevel('Primary Options');
  TopLevel('Utils');
  // TopLevel('Ok');
  FirstLevel('Select A File', 0);
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
procedure Tfrm016.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  frm016c.InsertControl(sta000);
  tim000 := TTimer.Create(frm016c);
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
procedure Tfrm016.estWindow(top, width, height: Integer; caption: string);
begin
  frm016c := Tfrm016.CreateNew(Application);
  frm016c.top := top;
  frm016c.Position := poScreenCenter;
  frm016c.width := width;
  frm016c.height := height;
  frm016c.caption := caption;
  dlgFont := TFontDialog.Create(frm016c);
  dlgColors := TColorDialog.Create(frm016c);
  dlgColors.Options := [cdFullOpen];
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm016.frmShowModal;
begin
  estWindow(27, 500, 300, rp016.caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  dlgSelFile := TFileOpenDialog.Create(Application);
  dlgSelFile.Title := 'Select A File Now!';
  dlgSelFile.Options := [fdoFileMustExist];
  dlgSelFile.Name := 'SelectAFile';
  dlgSelFile.FileTypes.Add;
  dlgSelFile.FileTypes[0].FileMask := '*.*';
  dlgSelFile.FileTypes[0].DisplayName := 'Any File';
  rp016.fileName := '';
  if dlgSelFile.Execute then
  begin
    rp016.fileName := dlgSelFile.fileName;
  end;
  // frm016c.ShowModal;
  dlgSelFile.Free;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm016.OnMenuClick(Sender: TObject);
var
  buf: string;
  x: Integer;
begin
  With Sender as TMenuItem do
  begin
    buf := name;
    buf := buf;
  end;
  if buf = 'muiSelectAFile' then
  begin
    dlgSelFile.Execute;
  end;
  if buf = 'muiExit' then
  begin
    frm016c.Close;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(frm016c.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (frm016c.ComponentCount - 1) do
    begin
      if (frm016c.Components[x] is TDBEdit) or
        (frm016c.Components[x] is TDBGrid) or (frm016c.Components[x] is TDBMemo)
        or (frm016c.Components[x] is TDBText) or
        (frm016c.Components[x] is TComboBox) or (frm016c.Components[x] is TEdit)
        or (frm016c.Components[x] is TLabel) or
        (frm016c.Components[x] is TListBox) or (frm016c.Components[x] is TMemo)
      then
        TButton(frm016c.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(frm016c.Handle);
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
procedure Tfrm016.OnTimer(Sender: TObject);
begin
  sta000.Panels[1].Text :=
    FormatDateTime('dddd, mmm dd, yyyy hh:mm:ss      ', now);
end;

{*-----------------------------------------------------------------------------
 Procedure: XQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     cmd, parm, defPath: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm016.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(frm016c.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

end.
