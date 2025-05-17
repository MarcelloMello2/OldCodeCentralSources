{*-----------------------------------------------------------------------------
 Unit Name: frm001u
 Date:      04-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}

unit frm001u;

interface

uses Graphics, SysUtils, Vcl.Dialogs, Vcl.Controls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, Vcl.Themes, System.Classes,
  Winapi.ShellAPI,
  Winapi.Windows;

type

  /// Main Class
  Tfrm001 = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure lbxStylesDblClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);

  public
    procedure frmShowModal;

  end;

var
  /// Form Handle
  frm001C: Tfrm001;

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
  /// Styles List Box
  lbxStyles: TListBox;
  /// Main Menu
  menMain: TMainMenu;
  /// Status Bar Declaration
  sta000: TStatusBar;
  /// Timer Control
  tim000: TTimer;
  /// Menu Item
  tmi: TMenuItem;

  {*-----------------------------------------------------------------------------
   Procedure: estMainMenu
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      04-May-2016
   @Param     None
   @Return    None
   -----------------------------------------------------------------------------}
procedure Tfrm001.estMainMenu;
  procedure TopLevel(cap: String);
  begin
    cap := StringReplace(cap, ' ', '', []);
    tmi := TMenuItem.Create(menMain);
    tmi.Name := 'mui' + cap;
    tmi.caption := cap;
    menMain.Items.Add(tmi);
  end;

  procedure FirstLevel(cap: string; itm: Integer);
  begin
    cap := StringReplace(cap, ' ', '', []);
    tmi := TMenuItem.Create(menMain);
    tmi.caption := cap;
    tmi.Name := 'mui' + cap;
    tmi.OnClick := OnMenuClick;
    menMain.Items[itm].Add(tmi);
  end;

begin
  menMain := TMainMenu.Create(frm001c);
  TopLevel('Primary Options');
  TopLevel('Utils');
  FirstLevel('Exit', 0);
  FirstLevel('Colors', 1);
  FirstLevel('Fonts', 1);
  FirstLevel('Help', 1);
end;

{*-----------------------------------------------------------------------------
 Procedure: estStatus
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     pan1, pan2, int: Integer
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  frm001c.InsertControl(sta000);
  tim000 := TTimer.Create(frm001c);
  tim000.Interval := int;
  tim000.OnTimer := OnTimer;
end;

{*-----------------------------------------------------------------------------
 Procedure: estWindow
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     top, width, height: Integer; caption: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.estWindow(top, width, height: Integer; caption: string);
var
  styleName: string;
begin
  frm001c := Tfrm001.CreateNew(Application);
  frm001c.Position := poScreenCenter;
  frm001c.Top := top;
  frm001c.width := width;
  frm001c.height := height;
  frm001c.caption := caption;
  dlgFont := TFontDialog.Create(frm001c);
  dlgColors := TColorDialog.Create(frm001c);
  dlgColors.Options := [cdFullOpen];
  lbxStyles := TListBox.Create(frm001c);
  lbxStyles.Align := alClient;
  lbxStyles.OnDblClick := lbxStylesDblClick;
  frm001c.InsertControl(lbxStyles);
  lbxStyles.Items.Clear;
  for styleName in TStyleManager.StyleNames do
  begin
    lbxStyles.Items.Add(styleName);
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.frmShowModal;
begin
  estWindow(27, 500, 300, 'BCS Styles Dialog');
  estStatus(200, 300, 1000);
  estMainMenu;
  frm001c.ShowModal;
  frm001c.Free;
end;

{*-----------------------------------------------------------------------------
 Procedure: lbxStylesDblClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.lbxStylesDblClick(Sender: TObject);
begin
  TStyleManager.SetStyle(lbxStyles.Items[lbxStyles.ItemIndex]);
  frm001c.ModalResult := mrOk;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.OnMenuClick(Sender: TObject);
var
  buf: string;
begin
  With Sender as TMenuItem do
  begin
    buf := name;
    buf := buf;
  end;
  if buf = 'muiExit' then
  begin
    frm001c.Close;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(frm001c.Handle);
    defFont := dlgFont.Font;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(frm001c.Handle);
    defColor := dlgColors.Color;
  end;
  if buf = 'muiHelp' then
  begin
    XQT('http://bcsjava.com/doc/app/BCS%20File%20IO%20Direct%20Access.htm', '',
      '');
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnTimer
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm001.OnTimer(Sender: TObject);
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
procedure Tfrm001.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(frm001c.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

end.
