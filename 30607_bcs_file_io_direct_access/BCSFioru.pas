{*-----------------------------------------------------------------------------
 Unit Name: BCSFioru
 Date:      08-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit BCSFioru;

interface

uses frm001u, Graphics, Vcl.ComCtrls, Vcl.DBCtrls, Vcl.DBGrids, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls, System.Classes, SysUtils,
  Winapi.ShellAPI, Winapi.Windows;

const
  /// Maximum Data Elements
  maxDele = 50;
  /// Maximum Rows
  maxRows = 5;
  /// File Name
  fleName = 'test.fle';
  /// File Size
  fleSize = 'fsiz.fle';

type

  /// IO area
  io_area = record
    /// Detail Records Fields
    dr: array [1 .. maxRows, 1 .. maxDele] of AnsiString;
    /// Master Record Fields
    mr: array [1 .. maxDele] of AnsiString;
    /// Other Accomplishments
    oa: array [1 .. maxDele] of AnsiString;
  end;

  /// Dialog Record Area
  BCSFior_area = record
    /// Caption
    Caption: string;
  end;

  /// Main Class
  TBCSFior = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; Caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);

  public
    procedure frmShowModal;
    procedure ReadFile;
    procedure WriteFile;
    procedure ZapRec;
    procedure WriteRecSz;
    procedure ReadRecSz;

  end;

var
  /// Form Handle
  BCSFiorc: TBCSFior;
  /// Record Area
  rpFior: BCSFior_area;
  /// Input Output Area
  ior: io_area;
  /// File Discriptor
  fd: File;
  /// File Size Handle
  fs: File;
  /// Record Size
  recSize: Integer;

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
procedure TBCSFior.estMainMenu;
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
  menMain := TMainMenu.Create(BCSFiorc);
  TopLevel('Primary Options');
  TopLevel('Utils');
  // TopLevel('Ok');
  FirstLevel('Zap Data Area', 0);
  FirstLevel('Write Data Record', 0);
  FirstLevel('Read Data Record', 0);
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
procedure TBCSFior.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  BCSFiorc.InsertControl(sta000);
  tim000 := TTimer.Create(BCSFiorc);
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
procedure TBCSFior.estWindow(top, width, height: Integer; Caption: string);
begin
  BCSFiorc := TBCSFior.CreateNew(Application);
  BCSFiorc.top := top;
  BCSFiorc.Position := poScreenCenter;
  BCSFiorc.width := width;
  BCSFiorc.height := height;
  BCSFiorc.Caption := Caption;
  BCSFiorc.BorderIcons := [biSystemMenu, biMaximize];
  dlgFont := TFontDialog.Create(BCSFiorc);
  dlgColors := TColorDialog.Create(BCSFiorc);
  dlgColors.Options := [cdFullOpen];
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.frmShowModal;
begin
  estWindow(27, 500, 300, rpFior.Caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  BCSFiorc.ShowModal;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.OnMenuClick(Sender: TObject);
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
    BCSFiorc.Close;
  end;
  if buf = 'muiZapDataArea' then
  begin
    ZapRec;
  end;
  if buf = 'muiWriteDataRecord' then
  begin
    WriteFile;
  end;
  if buf = 'muiReadDataRecord' then
  begin
    ReadFile;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(BCSFiorc.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (BCSFiorc.ComponentCount - 1) do
    begin
      if (BCSFiorc.Components[x] is TDBEdit) or
        (BCSFiorc.Components[x] is TDBGrid) or
        (BCSFiorc.Components[x] is TDBMemo) or
        (BCSFiorc.Components[x] is TDBText) or
        (BCSFiorc.Components[x] is TComboBox) or
        (BCSFiorc.Components[x] is TEdit) or (BCSFiorc.Components[x] is TLabel)
        or (BCSFiorc.Components[x] is TListBox) or
        (BCSFiorc.Components[x] is TMemo) then
        TButton(BCSFiorc.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(BCSFiorc.Handle);
    defColor := dlgColors.Color;
  end;
  if buf = 'muiHelp' then
  begin
    XQT('http://bcsjava.com/doc/app/BCS%20File%20IO%20Direct%20Access.htm', '',
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
procedure TBCSFior.OnTimer(Sender: TObject);
begin
  sta000.Panels[1].Text :=
    FormatDateTime('dddd, mmm dd, yyyy hh:mm:ss      ', now);
end;

{*-----------------------------------------------------------------------------
 Procedure: ReadFile
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      26-Aug-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.ReadFile;
begin
  if FileExists(fleName) then
  begin
    ReadRecSz;
    AssignFile(fd, fleName);
    ReSet(fd, recSize);
    Seek(fd, 0);
    BlockRead(fd, ior, 1);
    CloseFile(fd);
  end
  else
  begin
    MessageBox(BCSFiorc.Handle, 'File Does Not Exist!', 'Cannot Read File!',
      mb_OkCancel);
  end;
end;

{*-----------------------------------------------------------------------------
  Procedure: ReadRecSz
  Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
  Date:      08-Sep-2016
  @Param     None
  @Return    None
-----------------------------------------------------------------------------}
procedure TBCSFior.ReadRecSz;
begin
  AssignFile(fs, fleSize);
  recSize := SizeOf(recSize);
  ReSet(fs, recSize);
  Seek(fs, 0);
  BlockRead(fs, recSize, 1);
  CloseFile(fs);
end;

{*-----------------------------------------------------------------------------
 Procedure: WriteFile
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      26-Aug-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.WriteFile;
begin
  AssignFile(fd, fleName);
  recSize := SizeOf(ior);
  ReWrite(fd, SizeOf(ior));
  Seek(fd, 0);
  BlockWrite(fd, ior, 1);
  CloseFile(fd);
  WriteRecSz;
end;

{*-----------------------------------------------------------------------------
 Procedure: WriteRecSz
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      08-Sep-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.WriteRecSz;
begin
  AssignFile(fs, fleSize);
  recSize := SizeOf(recSize);
  ReWrite(fs, recSize);
  Seek(fs, 0);
  BlockWrite(fs, recSize, 1);
  CloseFile(fs);
end;

{*-----------------------------------------------------------------------------
 Procedure: XQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     cmd, parm, defPath: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(BCSFiorc.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

{*-----------------------------------------------------------------------------
 Procedure: ZapRec
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      26-Aug-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSFior.ZapRec;
var
  i: Integer;
  j: Integer;
begin
  if MessageBox(Application.Handle,
    'Are You Sure You Want To Erase Record Area?', 'Erase Record Area?',
    mb_YesNo) = idYes then
  begin
    i := 1;
    repeat
      ior.mr[i] := '';
      ior.oa[i] := '';
      Inc(i);
    until i > maxDele;
    j := 1;
    repeat
      i := 1;
      repeat
        ior.dr[j, i] := '';
        Inc(i);
      until i > maxDele;
      Inc(j);
    until j > maxRows;
  end;
end;

end.
