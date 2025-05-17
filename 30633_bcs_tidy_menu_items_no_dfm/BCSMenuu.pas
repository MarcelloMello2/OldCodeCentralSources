{*-----------------------------------------------------------------------------
 Unit Name: BCSMenuu
 Date:      08-May-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit BCSMenuu;

interface

uses frm001u, frm016U, Graphics, Vcl.ClipBrd, Vcl.ComCtrls, Vcl.DBCtrls,
  Vcl.DBGrids, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms, Vcl.Menus, Vcl.StdCtrls,
  System.Classes, SysUtils, Winapi.ShellAPI, Winapi.Windows;

type

  /// Dialog Record Area
  BCSMenu_area = record
    /// Caption
    Caption: string;
  end;

  /// Main Class
  TBCSMenu = class(TCustomForm)

  private
    procedure cgeCallProc;
    procedure cgeGenXQT;
    procedure cgeMenuItem;
    procedure cgeMenuXqt;
    procedure cgeProcBlock;
    procedure cgeProcDef;
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; Caption: string);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure XQT(cmd, parm, defPath: string);
    procedure proProcessMenus;

  public
    procedure frmShowModal;

  end;

var
  /// Form Handle
  BCSMenuc: TBCSMenu;
  /// Record Area
  rp0: BCSMenu_area;

implementation

// uses System.Win;

var
  /// Generator Menu Item
  cgeMenu: string;
  /// Generator Menu Compressed
  cgeMenuComp: string;
  /// Generated Menu Item Number
  cgeMItemNo: string;
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
  /// Procedure Name
  procName: string;
  /// String Collection
  rsc: TStringList;
  /// Status Bar Declaration
  sta000: TStatusBar;
  /// Timer Control
  tim000: TTimer;
  /// Menu Item
  tmi: TMenuItem;

  {*-----------------------------------------------------------------------------
   Procedure: cgeCallProc
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      03-Oct-2016
   @Param     None
   @Return    None
   -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeCallProc;
var
  rbuf: string;
begin
  if Trim(procName) > '' then
  begin
    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AsText := '  ' + procName + ';' + #13#10;
    Clipboard.Close;
    procName := '';
    MessageBox(BCSMenuc.Handle, 'Move To Call Procedure Insertion Point!',
      'Insert Text!', mb_YesNo);
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: cgeGenXQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeGenXQT;
var
  rbuf: string;
begin
  rp016.FileName := '';
  rp016.Caption := 'Select An Executable File Now!';
  rp016.fileMask := '*.exe';
  rp016.dispName := 'Executable';
  frm016c.frmShowModal;
  if Trim(rp016.FileName) > '' then
  begin
    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AsText := '  XQT(' + '''' + rp016.FileName + '''' + ', ' + '''' +
      '''' + ', ' + '''' + '''' + ');';
    Clipboard.Close;
    MessageBox(BCSMenuc.Handle, 'Move To Execute Program Insertion Point!',
      'Insert Program Execution Code!', mb_YesNo);
    rp016.FileName := rp016.FileName;
  end;
  frm016c.Free;
end;

{*-----------------------------------------------------------------------------
 Procedure: cgeMenuItem
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      02-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeMenuItem;
begin
  cgeMenu := InputBox('Enter Menu Item Now!', 'Menu Item', '');
  if Trim(cgeMenu) > '' then
  begin
    cgeMenuComp := StringReplace(cgeMenu, ' ', '', [rfReplaceAll]);
    cgeMItemNo := InputBox('Enter Element Number!', 'Element Number', '');
    if Trim(cgeMItemNo) > '' then
    begin
      Clipboard.Open;
      Clipboard.Clear;
      // FirstLevel('DVD Cross Reference', 6);
      Clipboard.AsText := '  FirstLevel(' + '''' + cgeMenu + '''' + ', ' +
        cgeMItemNo + ');' + #13#10;
      Clipboard.Close;
      MessageBox(BCSMenuc.Handle, 'Move To Insertion Point!', 'Insert Text!',
        mb_YesNo);
    end;
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: cgeMenuXqt
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      02-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeMenuXqt;
var
  sc: TStringList;
begin
  sc := TStringList.Create;
  sc.Add('  if buf = ' + '''' + 'mui' + cgeMenuComp + '''' + ' then');
  sc.Add('  begin');
  sc.Add('    buf := buf;');
  sc.Add('  end;');
  Clipboard.Open;
  Clipboard.Clear;
  Clipboard.AsText := sc.Text;
  Clipboard.Close;
  MessageBox(BCSMenuc.Handle, 'Move To Insertion Point!', 'Insert Text!',
    mb_YesNo);
  sc.Free;
end;

{*-----------------------------------------------------------------------------
 Procedure: cgeProcBlock
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeProcBlock;
var
  rbuf: string;
begin
  if Trim(procName) > '' then
  begin
    rsc := TStringList.Create;
    rsc.Add('procedure T' + 'BCSMenu' + '.' + procName + ';');
    rsc.Add('var');
    rsc.Add('  rbuf : string;');
    rsc.Add('begin');
    rsc.Add('  rbuf := rbuf;');
    rsc.Add('end;');
    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AsText := rsc.Text + #13#10;
    Clipboard.Close;
    rsc.Free;
    MessageBox(BCSMenuc.Handle, 'Move To Proc Block Insertion Point!',
      'Insert Text!', mb_YesNo);
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: cgeProcDef
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.cgeProcDef;
var
  rbuf: string;
begin
  procName := InputBox('Enter Procedure Name!', 'Procedure Name', '');
  if Trim(procName) > '' then
  begin
    Clipboard.Open;
    Clipboard.Clear;
    Clipboard.AsText := '    procedure ' + procName + ';' + #13#10;
    Clipboard.Close;
    MessageBox(BCSMenuc.Handle, 'Move To Proc Definition Insertion Point!',
      'Insert Text!', mb_YesNo);
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: estMainMenu
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.estMainMenu;
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
  menMain := TMainMenu.Create(BCSMenuc);
  TopLevel('Primary Options');
  TopLevel('Utils');
  TopLevel('Source Code Snippets');
  // TopLevel('Ok');
  FirstLevel('Process Menus', 0);
  FirstLevel('Exit', 0);
  FirstLevel('Colors', 1);
  FirstLevel('Fonts', 1);
  FirstLevel('Help', 1);
  FirstLevel('Styles', 1);
  FirstLevel('Gen Menu Item', 2);
  FirstLevel('Gen Menu Click', 2);
  FirstLevel('Put Exe To Clipboard', 2);
  FirstLevel('Procedure Definition', 2);
  FirstLevel('Procedure Block', 2);
  FirstLevel('Call Procedure', 2);
end;

{*-----------------------------------------------------------------------------
 Procedure: estStatus
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     pan1, pan2, int: Integer
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  BCSMenuc.InsertControl(sta000);
  tim000 := TTimer.Create(BCSMenuc);
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
procedure TBCSMenu.estWindow(top, width, height: Integer; Caption: string);
begin
  BCSMenuc := TBCSMenu.CreateNew(Application);
  BCSMenuc.top := top;
  BCSMenuc.Position := poScreenCenter;
  BCSMenuc.width := width;
  BCSMenuc.height := height;
  BCSMenuc.Caption := Caption;
  BCSMenuc.BorderIcons := [biSystemMenu, biMaximize];
  dlgFont := TFontDialog.Create(BCSMenuc);
  dlgColors := TColorDialog.Create(BCSMenuc);
  dlgColors.Options := [cdFullOpen];
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.frmShowModal;
begin
  estWindow(27, 500, 300, rp0.Caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  BCSMenuc.ShowModal;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.OnMenuClick(Sender: TObject);
var
  buf: string;
  x: Integer;
begin
  With Sender as TMenuItem do
  begin
    buf := name;
    buf := buf;
  end;
  if buf = 'muiProcessMenus' then
  begin
    proProcessMenus;
    buf := buf;
  end;
  if buf = 'muiExit' then
  begin
    BCSMenuc.Close;
  end;
  if buf = 'muiGenMenuItem' then
  begin
    cgeMenuItem;
    buf := buf;
  end;
  if buf = 'muiGenMenuClick' then
  begin
    cgeMenuXqt;
    buf := buf;
  end;
  if buf = 'muiPutExeToClipboard' then
  begin
    cgeGenXQT;
  end;
  if buf = 'muiCallProcedure' then
  begin
    cgeCallProc;
    buf := buf;
  end;
  if buf = 'muiProcedureDefinition' then
  begin
    cgeProcDef;
    buf := buf;
  end;
  if buf = 'muiProcedureBlock' then
  begin
    cgeProcBlock;
    buf := buf;
  end;

  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(BCSMenuc.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (BCSMenuc.ComponentCount - 1) do
    begin
      if (BCSMenuc.Components[x] is TDBEdit) or
        (BCSMenuc.Components[x] is TDBGrid) or
        (BCSMenuc.Components[x] is TDBMemo) or
        (BCSMenuc.Components[x] is TDBText) or
        (BCSMenuc.Components[x] is TComboBox) or
        (BCSMenuc.Components[x] is TEdit) or (BCSMenuc.Components[x] is TLabel)
        or (BCSMenuc.Components[x] is TListBox) or
        (BCSMenuc.Components[x] is TMemo) then
        TButton(BCSMenuc.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(BCSMenuc.Handle);
    defColor := dlgColors.Color;
  end;
  if buf = 'muiHelp' then
  begin
    XQT('http://bcsjava.com/doc/app/BCS Tidy Menu Items.htm', '', '');
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
procedure TBCSMenu.OnTimer(Sender: TObject);
begin
  sta000.Panels[1].Text :=
    FormatDateTime('dddd, mmm dd, yyyy hh:mm:ss      ', now);
end;

{*-----------------------------------------------------------------------------
 Procedure: proProcessMenus
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      16-Oct-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.proProcessMenus;
var
  rbuf: string;
  inpath: String;
  sc: TStringList;
  iend: Integer;
  i: Integer;
  j: Integer;
  mui: string;
  mi: Array [1 .. 100] of TStringList;
  kw: TStringList;
  opf: TStringList;
  isp: Integer;
begin
  kw := TStringList.Create;
  kw.Duplicates := dupIgnore;
  kw.Sorted := True;
  j := 0;
  sc := TStringList.Create;
  inpath := ExtractFileDir(ExtractFileDir(GetCurrentDir));
  sc.LoadFromFile(inpath + '\inp.txt');
  iend := sc.Count - 1;
  i := 0;
  j := 0;
  Repeat
    if Pos('mui', sc[i]) > 0 then
    begin
      mui := System.Copy(sc[i], Pos('mui', sc[i]), Length(sc[i]));
      Delete(mui, Pos(' then', mui), Length(mui));
      mui := StringReplace(mui, '''', '', [rfReplaceAll]);
      Inc(j);
      mi[j] := TStringList.Create;
      mi[j].Count;
      mui := mui + '^' + IntToStr(j);
      kw.Add(mui);
      if j = 1 then
        isp := i;
      mui := mui;
      mui := '';
    end;
    if mi[j] = nil then
    begin
    end
    else
    begin
      if j > 0 then
        mi[j].Add(sc[i]);
    end;
    Inc(i);
  Until ((Pos('end;', sc[i]) = 1) or (i > iend));
  opf := TStringList.Create;
  i := 0;
  Repeat
    rbuf := kw[i];
    j := StrToInt(System.Copy(kw[i], Pos('^', kw[i]) + 1, Length(kw[i])));
    opf.Text := opf.Text + mi[j].Text;
    Inc(i);
  Until i > kw.Count - 1;
  opf.SaveToFile(inpath + '\out.txt');
  j := 0;
  i := isp;
  repeat
    if j <= opf.Count - 1 then
    begin
      sc[i] := opf[j];
      Inc(j);
    end;
    Inc(i);
  until i > iend;
  sc.SaveToFile(inpath + '\ninp.txt');
  rbuf := rbuf;
end;

{*-----------------------------------------------------------------------------
 Procedure: XQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     cmd, parm, defPath: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure TBCSMenu.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(BCSMenuc.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

end.
