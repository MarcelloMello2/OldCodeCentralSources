{*-----------------------------------------------------------------------------
 Unit Name: BCSCpu
 Date:      08-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit BCSCpu;

interface

uses frm001u, Graphics, Vcl.ComCtrls, Vcl.DBCtrls, Vcl.DBGrids, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, System.Classes, SysUtils,
  Winapi.ShellAPI, Winapi.Windows;

type

  /// Dialog Record Area
  BCSCp_area = record
    /// Caption
    Caption: string;
  end;

  /// Main Class
  TBCSCp = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; Caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);

  public
    procedure frmShowModal;

  end;

var
  /// Form Handle
  BCSCpc: TBCSCp;
  /// Record Area
  rpCSCp: BCSCp_area;

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

  {*-----------------------------------------------------------------------------
   Procedure: estMainMenu
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      04-May-2016
   @Param     None
   @Return    None
   -----------------------------------------------------------------------------}
procedure TBCSCp.estMainMenu;
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
  menMain := TMainMenu.Create(BCSCpc);
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
procedure TBCSCp.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  BCSCpc.InsertControl(sta000);
  tim000 := TTimer.Create(BCSCpc);
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
procedure TBCSCp.estWindow(top, width, height: Integer; Caption: string);
begin
  BCSCpc := TBCSCp.CreateNew(Application);
  BCSCpc.top := top;
  BCSCpc.Position := poScreenCenter;
  BCSCpc.width := width;
  BCSCpc.height := height;
  BCSCpc.Caption := Caption;
  dlgFont := TFontDialog.Create(BCSCpc);
  dlgColors := TColorDialog.Create(BCSCpc);
  dlgColors.Options := [cdFullOpen];
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSCp.frmShowModal;
begin
  estWindow(27, 500, 300, rpCSCp.Caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  BCSCpc.ShowModal;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSCp.OnMenuClick(Sender: TObject);
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
    BCSCpc.Close;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(BCSCpc.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (BCSCpc.ComponentCount - 1) do
    begin
      if (BCSCpc.Components[x] is TDBEdit) or (BCSCpc.Components[x] is TDBGrid)
        or (BCSCpc.Components[x] is TDBMemo) or
        (BCSCpc.Components[x] is TDBText) or (BCSCpc.Components[x] is TComboBox)
        or (BCSCpc.Components[x] is TEdit) or (BCSCpc.Components[x] is TLabel)
        or (BCSCpc.Components[x] is TListBox) or (BCSCpc.Components[x] is TMemo)
      then
        TButton(BCSCpc.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(BCSCpc.Handle);
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
procedure TBCSCp.OnTimer(Sender: TObject);
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
procedure TBCSCp.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(BCSCpc.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

end.
