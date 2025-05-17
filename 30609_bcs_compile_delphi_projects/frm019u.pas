{*-----------------------------------------------------------------------------
 Unit Name: frm019u
 Date:      13-Jun-2016
 Purpose:
 History:
 @Author    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 @version    1.0.0.0
 -----------------------------------------------------------------------------}
unit frm019u;

interface

uses Data.DB, frm001u, frm016U, frm022U, Graphics, MySQlUniProvider,
  Vcl.ComCtrls,
  Vcl.Controls, Vcl.DBCtrls, Vcl.DBGrids, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Forms,
  Vcl.Menus, Vcl.StdCtrls, System.Classes, SysUtils, Uni, Winapi.ShellAPI,
  Winapi.Windows;

const
  /// verticle Spacing of Data Elemets
  ds = 1;
  /// Left starting point for Data Elements
  lde = 160;
  /// Compiler Loacation
  compLoc = 'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\bds.exe';

type
  /// Dialog Record Area
  frm019_area = record
    /// Caption
    Caption: string;
    /// Notebook Tabs
    nbt: Array [0 .. 4] of string;
    /// Name Of Database
    Database: string;
    /// User Id
    UsrId: string;
    /// Password
    Password: string;
    /// Master Table
    MasTab: string;
    /// Master key
    MasKey: string;
    /// Master Index Field
    MasIdx: string;
    /// Selected Table
    sel: String;
  end;

  /// Data Grid COlums
  dgCols = record
    /// Data Name
    dna: string;
    /// Heading Title
    hdg: string;
    /// Display Width
    width: Integer;
    /// Make Element Read Only
    readOnly: Boolean;
  end;

  /// Main Class
  Tfrm019 = class(TCustomForm)

  private
    procedure estMainMenu;
    procedure estStatus(pan1, pan2, int: Integer);
    procedure estWindow(top, width, height: Integer; Caption: string);
    procedure insAuto(rPar: TWinControl; rName: string;
      rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
    procedure insDBCheck(rPar: TWinControl; rName, rcap: string;
      rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
    procedure insLabel(rName: String; rTop, rLeft, rWidth: Integer;
      rWc: TWinControl);
    procedure insDBEdit(rPar: TWinControl; rName: string;
      rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
    procedure insDBMemo(rPar: TWinControl; rName: string;
      rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
    procedure insCols(rGrid: TDBGrid; Cno: Integer; fn, cap: String;
      rWid: Integer; Roi: Boolean);
    procedure OnMenuClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure PopScroll(MyScroll: TScrollBox; MyDataSource: TDataSource;
      uta: TUniTable; Par: TWinControl);
    procedure XQT(cmd, parm, defPath: string);
    procedure mnavClick(Sender: TObject; Button: TNavigateBtn);
    procedure DBGrid1DblClick(Sender: TObject);

  public
    procedure AddCol(den, ColTitle: String; ColWidth: Integer;
      readOnly: Boolean);
    procedure frmShowModal;
    procedure ZapDge;

  end;

var
  /// Form Handle
  frm019c: Tfrm019;
  /// Record Area
  rp019: frm019_area;

implementation

// uses System.Win;

var
  /// Default Color
  defColor: TColor;
  /// Database Grid for Master Records
  dbg1: TDBGrid;
  /// Default Font
  defFont: TFont;
  /// Data Grid Elements
  dge: Array [1 .. 10] of dgCols;
  /// Data Element Pointer
  dep: Integer;
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
  /// Page Control
  PageCon: TPageControl;
  /// Tab Sheet
  tas: TTabSheet;
  /// Master Navigator
  mnav: TDBNavigator;
  /// Master Data Source
  masds: TDataSource;
  /// Scroll Box For Data Entry
  sb1: TScrollBox;
  /// Connection
  ucon: TUniConnection;
  /// Master Table
  uta1: TUniTable;
  /// Unique Identifier for Data Names
  j: Integer;

  {*-----------------------------------------------------------------------------
   Procedure: AddCol
   Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
   Date:      13-Jun-2016
   @Param     den, ColTitle: String; ColWidth: Integer; readOnly: Boolean
   @Return    None
   -----------------------------------------------------------------------------}
procedure Tfrm019.AddCol(den, ColTitle: String; ColWidth: Integer;
  readOnly: Boolean);
begin
  dge[dep].dna := den;
  dge[dep].hdg := ColTitle;
  dge[dep].width := ColWidth;
  if readOnly then
    dge[dep].readOnly := true;
  Inc(dep);
end;

{*-----------------------------------------------------------------------------
 Procedure: mnavClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      07-Sep-2016
 @Param     Sender: TObject; Button: TNavigateBtn
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.mnavClick(Sender: TObject; Button: TNavigateBtn);
var
  sdes: string;
begin
  if Button = nbInsert then
  begin
    mnav.DataSource.Dataset.Cancel;
    rp016.Caption := 'BCS Select A File';
    rp016.fileName := '';
    frm016c.frmShowModal;
    frm016c.Free;
    sdes := InputBox('Enter Description Now!', 'Short File Descriiption',
      rp016.fileName);
    if ((Trim(rp016.fileName) > '') and (Trim(sdes) > '')) then
    begin
      mnav.DataSource.Dataset.InsertRecord([nil, Trim(rp016.fileName),
        Trim(sdes)]);
    end;
  end;
end;

{*-----------------------------------------------------------------------------
 Procedure: estMainMenu
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.estMainMenu;

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
  menMain := TMainMenu.Create(frm019c);
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
 Procedure: DBGrid1DblClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      07-Sep-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.DBGrid1DblClick(Sender: TObject);
begin
  XQT(compLoc, mnav.DataSource.Dataset.FieldByName('proj').AsString, '');
end;

{*-----------------------------------------------------------------------------
 Procedure: estStatus
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     pan1, pan2, int: Integer
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.estStatus(pan1, pan2, int: Integer);
begin
  sta000 := TStatusBar.Create(Application);
  sta000.Panels.Add;
  sta000.Panels.Add;
  sta000.Panels[0].width := pan1;
  sta000.Panels[1].width := pan2;
  sta000.Panels[1].Alignment := taRightJustify;
  frm019c.InsertControl(sta000);
  tim000 := TTimer.Create(frm019c);
  tim000.Interval := int;
  tim000.OnTimer := OnTimer;
end;

{*-----------------------------------------------------------------------------
 Procedure: insCols
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rGrid: TDBGrid; Cno: Integer; fn, cap: String; rWid: Integer; Roi: Boolean
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insCols(rGrid: TDBGrid; Cno: Integer; fn, cap: String;
  rWid: Integer; Roi: Boolean);
begin
  rGrid.Columns.Add;
  rGrid.Columns.Items[Cno].FieldName := fn;
  rGrid.Columns.Items[Cno].Title.Caption := cap;
  rGrid.Columns.Items[Cno].width := rWid;
  if Roi then
    rGrid.Columns.Items[Cno].readOnly := true;

end;

{*-----------------------------------------------------------------------------
 Procedure: estWindow
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     top, width, height: Integer; caption: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.estWindow(top, width, height: Integer; Caption: string);
var
  i: Integer;
  procedure estConn;
  begin
    ucon := TUniConnection.Create(Application);
    with ucon do
    begin
      Database := rp019.Database;
      ConnectString :=
        'Provider Name=MySQL;Login Prompt=False;User ID=bcs;Password=' +
        rp019.Password + ';' + 'Data Source=localhost;Database=' +
        rp019.Database + ';Port=3306';
      Connected := true;
      LoginPrompt := false;
    end;
    uta1 := TUniTable.Create(Application);
    uta1.Connection := ucon;
    uta1.TableName := rp019.MasTab;
    uta1.IndexFieldNames := rp019.MasIdx;
    if uta1.TableName = 'dge' then
    begin
      // uta1.AfterInsert := BeforeInsert;
    end;
    uta1.Active := true;
    masds := TDataSource.Create(frm019c);
    masds.Dataset := uta1;
    masds.Enabled := true;
  end;
  procedure BldNBook(Pwin: TWinControl; rnb: Array of string);
  begin
    i := 0;
    PageCon := TPageControl.Create(Application);
    PageCon.Parent := Pwin;
    PageCon.TabPosition := tpBottom;
    PageCon.Align := alClient;
    PageCon.Parent := Pwin;
    Repeat
      tas := TTabSheet.Create(Application);
      tas.PageIndex := i;
      tas.PageControl := PageCon;
      tas.Name := 'tas' + StringReplace(rnb[i], ' ', '', [rfReplaceAll]);
      tas.Caption := rnb[i];
      // PageCon.Pages[i].InsertControl(tas);
      Inc(i);
    Until Trim(rnb[i]) = '';
  end;

begin
  frm019c := Tfrm019.CreateNew(Application);
  frm019c.top := top;
  frm019c.Position := poScreenCenter;
  frm019c.width := width;
  frm019c.height := height;
  frm019c.Caption := Caption;
  dlgFont := TFontDialog.Create(frm019c);
  dlgColors := TColorDialog.Create(frm019c);
  dlgColors.Options := [cdFullOpen];
  rp019.nbt[0] := 'Grid';
  rp019.nbt[1] := 'Data Entry';
  BldNBook(frm019c, rp019.nbt);
  estConn;
  mnav := TDBNavigator.Create(Application);
  mnav.Parent := frm019c;
  mnav.DataSource := masds;
  masds.Dataset := uta1;
  mnav.Align := alTop;
  mnav.OnClick := mnavClick;
  dbg1 := TDBGrid.Create(Application);
  dbg1.Parent := PageCon.Pages[0];
  dbg1.Align := alClient;
  dbg1.Name := 'dg1';
  dbg1.DataSource := masds;
  dbg1.OnDblClick := DBGrid1DblClick;
  // dbg1.OnCellClick := dg1CellClick;
  sb1 := TScrollBox.Create(Application);
  sb1.Parent := PageCon.Pages[1];
  sb1.Name := 'sb1';
  sb1.Align := alClient;
  dep := 1;
  repeat
    insCols(dbg1, dep - 1, dge[dep].dna, dge[dep].hdg, dge[dep].width,
      dge[dep].readOnly);
    Inc(dep)
  until dge[dep].dna = '';
  // insCols(dbg1, 0, 'cat', 'Category', 450, false);
  PopScroll(sb1, masds, uta1, sb1);
end;

{*-----------------------------------------------------------------------------
 Procedure: frmShowModal
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      03-May-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.frmShowModal;
begin
  estWindow(27, 500, 300, rp019.Caption);
  estStatus(200, 300, 1000);
  estMainMenu;
  frm019c.ShowModal;
end;

{*-----------------------------------------------------------------------------
 Procedure: OnMenuClick
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     Sender: TObject
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.OnMenuClick(Sender: TObject);
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
    frm019c.Close;
  end;
  if buf = 'muiFonts' then
  begin
    dlgFont.Execute(frm019c.Handle);
    defFont := dlgFont.Font;
    for x := 0 to (frm019c.ComponentCount - 1) do
    begin
      if (frm019c.Components[x] is TDBEdit) or
        (frm019c.Components[x] is TDBGrid) or (frm019c.Components[x] is TDBMemo)
        or (frm019c.Components[x] is TDBText) or
        (frm019c.Components[x] is TComboBox) or (frm019c.Components[x] is TEdit)
        or (frm019c.Components[x] is TLabel) or
        (frm019c.Components[x] is TListBox) or (frm019c.Components[x] is TMemo)
      then
        TButton(frm019c.Components[x]).Font := defFont;
    end;
  end;
  if buf = 'muiColors' then
  begin
    dlgColors.Execute(frm019c.Handle);
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
procedure Tfrm019.OnTimer(Sender: TObject);
begin
  sta000.Panels[1].Text :=
    FormatDateTime('dddd, mmm dd, yyyy hh:mm:ss      ', now);
end;

{*-----------------------------------------------------------------------------
 Procedure: insAuto
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rPar: TWinControl; rName: string; rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insAuto(rPar: TWinControl; rName: string;
  rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
var
  edt: TDBEdit;
begin
  edt := TDBEdit.Create(frm019c);
  edt.Parent := rPar;
  edt.Alignment := taLeftJustify;
  edt.Name := 'edt' + rName + IntToStr(j);
  edt.top := rTop;
  edt.Left := rLeft;
  edt.width := rWidth;
  edt.DataSource := MyDataSource1;
  edt.DataField := rName;
  edt.readOnly := true;
end;

{*-----------------------------------------------------------------------------
 Procedure: insDBCheck
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rPar: TWinControl; rName, rcap: string; rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insDBCheck(rPar: TWinControl; rName, rcap: string;
  rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
var
  dck: TDBCheckBox;
begin
  dck := TDBCheckBox.Create(frm019c);
  dck.Parent := rPar;
  dck.top := rTop;
  dck.Left := rLeft;
  dck.width := rWidth;
  dck.Caption := rcap;
  dck.DataSource := MyDataSource1;
  dck.DataField := rName;
end;

{*-----------------------------------------------------------------------------
 Procedure: insDBEdit
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rPar: TWinControl; rName: string; rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insDBEdit(rPar: TWinControl; rName: string;
  rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
var
  edt: TDBEdit;
begin
  edt := TDBEdit.Create(frm019c);
  edt.Parent := rPar;
  edt.Alignment := taLeftJustify;
  edt.Name := 'edt' + rName + IntToStr(j);
  edt.top := rTop;
  edt.Left := rLeft;
  edt.width := rWidth;
  edt.DataSource := MyDataSource1;
  edt.DataField := rName;
  if edt.Field.FieldName = 'id' then
    edt.Field.readOnly := true;
end;

{*-----------------------------------------------------------------------------
 Procedure: insDBMemo
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rPar: TWinControl; rName: string; rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insDBMemo(rPar: TWinControl; rName: string;
  rTop, rLeft, rWidth: Integer; MyDataSource1: TDataSource);
var
  rDBE: TDBMemo;
begin
  rDBE := TDBMemo.Create(frm019c);
  rDBE.Parent := rPar;
  rDBE.Alignment := taLeftJustify;
  rDBE.Name := rName + IntToStr(j);
  rDBE.top := rTop;
  rDBE.Left := rLeft;
  rDBE.width := rWidth;
  rDBE.DataSource := MyDataSource1;
  rDBE.DataField := rName;
end;

{*-----------------------------------------------------------------------------
 Procedure: insLabel
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     rName: String; rTop, rLeft, rWidth: Integer; rWc: TWinControl
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.insLabel(rName: String; rTop, rLeft, rWidth: Integer;
  rWc: TWinControl);
var
  rla: TLabel;
  sname: string;
begin
  rla := TLabel.Create(Application);
  // rla.Parent := rWc;
  rla.Name := 'lbl' + StringReplace(rName, ' ', '', [rfReplaceAll]) +
    IntToStr(j);
  sname := frm022c.RetTag(rName);
  if sname = '' then
  begin
    rla.Caption := rName + ' : ';
  end
  else
  begin
    rla.Caption := sname + ' : ';
  end;
  rla.Alignment := taRightJustify;
  rla.AutoSize := false;
  rla.top := rTop;
  rla.Left := rLeft;
  rla.width := rWidth;
  rla.Visible := true;
  rWc.InsertControl(rla);
end;

{*-----------------------------------------------------------------------------
 Procedure: PopScroll
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     MyScroll: TScrollBox; MyDataSource: TDataSource; uta: TUniTable; Par: TWinControl
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.PopScroll(MyScroll: TScrollBox; MyDataSource: TDataSource;
  uta: TUniTable; Par: TWinControl);
var
  i: Integer;
  imc: Integer;
begin
  i := 0;
  imc := 1;
  repeat
    case uta.Fields[i].DataType of
      ftAutoInc:
        begin
          insAuto(Par, uta.Fields[i].FieldName, imc, lde, 75, MyDataSource);
          insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
          imc := (imc + 21 + ds);
        end;
      ftInteger, ftSmallInt, ftFloat, ftDateTime, ftLargeInt:
        Begin
          if uta.Fields[i].DataType = ftDateTime then
          begin
            insDBEdit(Par, uta.Fields[i].FieldName, imc, lde, 130,
              MyDataSource);
            insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
          end
          else
            if imc = 1 then
            begin
              insAuto(Par, uta.Fields[i].FieldName, imc, lde, 75, MyDataSource);
              insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
            end
            else
            begin
              insDBEdit(Par, uta.Fields[i].FieldName, imc, lde, 75,
                MyDataSource);
              insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
            end;
          imc := (imc + 21 + ds);
        End;
      ftString:
        begin
          insDBEdit(Par, uta.Fields[i].FieldName, imc, lde, 290, MyDataSource);
          insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
          imc := (imc + 21 + ds);
        end;
      ftMemo:
        begin
          insDBMemo(Par, uta.Fields[i].FieldName, imc, lde, 290, MyDataSource);
          insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
          imc := (imc + 89) + ds;
        end;
      ftBlob:
        begin
          // DoLabel(MyScroll, uta);
          // MyImage := TDBImage.Create(MyForm);
          // MyImage.Parent := MyScroll;
          // MyImage.DataField := uta.Fields[i].FieldName;
          // MyImage.Name := 'edt' + IntToStr(i) + uta.Fields[i].FieldName;
          // MyImage.Width := 290;
          // MyImage.Left := lde;
          // MyImage.Stretch := true;
          // MyImage.Top := imc;
          // MyImage.DataSource := MyDataSource;
          // MyScroll.InsertControl(MyImage);
          imc := (imc + 14) + ds;
        end;
      ftBoolean:
        begin
          insDBCheck(Par, uta.Fields[i].FieldName, 'Caption', imc, lde, 290,
            MyDataSource);
          insLabel(uta.Fields[i].FieldName, imc, 0, 150, Par);
          imc := (imc + 17) + ds;
        end;
    else
      begin
        MessageBox(frm019c.Handle, 'Field Not Detected!', 'Add New Field!',
          mb_OkCancel);
      end;
    end;
    Inc(i);
    Inc(j);
  until i = uta.FieldCount;
end;

{*-----------------------------------------------------------------------------
 Procedure: XQT
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      04-May-2016
 @Param     cmd, parm, defPath: string
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.XQT(cmd, parm, defPath: string);
begin
  ShellExecute(frm019c.Handle, PWideChar('open'), PWideChar(cmd),
    PWideChar(parm), PWideChar(defPath), sw_Normal);
end;

{*-----------------------------------------------------------------------------
 Procedure: ZapDge
 Author:    Mr. Arch Brooks, Software Engineer, Brooks Computing Systems, LLC
 Date:      13-Jun-2016
 @Param     None
 @Return    None
 -----------------------------------------------------------------------------}
procedure Tfrm019.ZapDge;
var
  x: Integer;
begin
  x := 1;
  repeat
    dge[x].dna := '';
    dge[x].hdg := '';
    dge[x].width := 0;
    dge[x].readOnly := false;
    Inc(x);
  until x > 10;
  dep := 1;
end;

end.
