{== Numpad ============================================================}
{: This unit implements a virtual numeric keypad.
 @author Dr. Peter Below
 @desc   Version 1.0 created 1 August 2002<BR>
 Last modified       1 August 2002<P>
 The form will not take focus when clicked on and manufacture key
 events when its buttons are clicked.  These events will go into
 whatever control has focus in the active form.   }
{======================================================================}

{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}

unit Numpad;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms,
  Dialogs, Grids, ExtCtrls;

type
  TDrawGrid = class(Grids.TDrawGrid)
  public
    function CanFocus: Boolean; override;
  end;

  TNumericKeypad = class(TForm)
    Numpad: TDrawGrid;
    Timer: TTimer;
    procedure NumpadDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NumpadClick(Sender: TObject);
  private
    FLastActiveCell: TGridCoord;
    FMouseDown: Boolean;

    procedure WMMouseActivate(var msg: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    procedure WMEnable(var msg: TWMEnable); message WM_ENABLE;
    function SameButton(A, B: TGridCoord): Boolean;
    procedure InvalidateButton(const coord: TGridCoord);
  public
    procedure CreateParams(var params: TCreateparams); override;
  end;

var
  NumericKeypad: TNumericKeypad;

implementation

{$R *.dfm}

{—— Helper functions ——————————————————————————————————————————————————}

function WidthOfRect(const aRect: TRect): Integer;
begin
  Result := aRect.Right - aRect.Left;
end;

function HeightOfRect(const aRect: TRect): Integer;
begin
  Result := aRect.Bottom - aRect.Top;
end;

{: Compare the passed grid coordinates for equality. }
function CoordsEqual(const A, B: TGridCoord): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

{: Test if the passed grid coordinate refers to a cell. }
function ValidCell(const coord: TGridCoord): Boolean;
begin
  Result := (coord.X >= 0) and (coord.Y >= 0);
end;

{: Manufacture a TGridCoord record from the passed column/row values. }
function GridCoord(ACol, ARow: Integer): TGridCoord;
begin
  Result.X := ACol;
  Result.Y := ARow;
end;

{************************************************************
 * Procedure PostKeyEx32
 *
 * Parameters:
 *  key    : virtual keycode of the key to send. For printable
 *           keys this is simply the ANSI code (Ord(character)).
 *  shift  : state of the modifier keys. This is a set, so you
 *           can set several of these keys (shift, control, alt,
 *           mouse buttons) in tandem. The TShiftState type is
 *           declared in the Classes Unit.
 *  specialkey: normally this should be False. Set it to True to
 *           specify a key on the numeric keypad, for example.
 * Description:
 *  Uses keybd_event to manufacture a series of key events matching
 *  the passed parameters. The events go to the control with focus.
 *  Note that for characters key is always the upper-case version of
 *  the character. Sending without any modifier keys will result in
 *  a lower-case character, sending it with [ssShift] will result
 *  in an upper-case character!
 *Created: 17.7.98 by P. Below
 ************************************************************}
procedure PostKeyEx32(key: Word; const shift: TShiftState;
  specialkey: Boolean);
type
  TShiftKeyInfo = record
    shift: Byte;
    vkey: Byte;
  end;

  byteset = set of 0 .. 7;
const
  shiftkeys: array [1 .. 3] of TShiftKeyInfo =
    ((shift: Ord(ssCtrl); vkey: VK_CONTROL),
    (shift: Ord(ssShift); vkey: VK_SHIFT),
    (shift: Ord(ssAlt); vkey: VK_MENU));
var
  flag: DWORD;
  bShift: byteset absolute shift;
  i: Integer;
begin
  for i := 1 to 3 do begin
    if shiftkeys[i].shift in bShift then
      keybd_event(shiftkeys[i].vkey,
        MapVirtualKey(shiftkeys[i].vkey, 0),
        0, 0);
  end; { For }
  if specialkey then
    flag := KEYEVENTF_EXTENDEDKEY
  else
    flag := 0;

  keybd_event(key, MapVirtualKey(key, 0), flag, 0);
  flag := flag or KEYEVENTF_KEYUP;
  keybd_event(key, MapVirtualKey(key, 0), flag, 0);

  for i := 3 downto 1 do begin
    if shiftkeys[i].shift in bShift then
      keybd_event(shiftkeys[i].vkey,
        MapVirtualKey(shiftkeys[i].vkey, 0),
        KEYEVENTF_KEYUP, 0);
  end; { For }
end; { PostKeyEx32 }

procedure SendText(S: string);
  procedure SendRawCharacter(ch: Char);
  var
    i: Integer;
    numStr: string;
  begin
    numStr := Format('%4.4d', [Ord(ch)]);
    keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0),
      0, 0);
    for i := 1 to Length(numStr) do
      PostKeyEx32(VK_NUMPAD0 + Ord(numStr[i]) - Ord('0'), [], false);
    keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0),
      KEYEVENTF_KEYUP, 0);
  end;

var
  flags: TShiftState;
  vcode: Word;
  ret: Word;
  i, n: Integer;
  mask: Word;
begin { SendText }
  for i := 1 to Length(S) do begin
    ret := VkKeyScan(S[i]);
    if ret = $FFFF then
      SendRawCharacter(S[i])
    else begin
      vcode := Lobyte(ret);
      flags := [];
      mask := $100;
      for n := 1 to 3 do begin
        if (ret and mask) <> 0 then begin
          case mask of
            $100:
              Include(flags, ssShift);
            $200:
              Include(flags, ssCtrl);
            $400:
              Include(flags, ssAlt);
          end; { Case }
        end; { If }
        mask := mask shl 1;
      end; { For }
      PostKeyEx32(vcode, flags, false);
    end; { Else }
  end; { For }
end; { SendText }

{== TDrawGrid =========================================================}

{: Just an additional safeguard to make sure the drawgrid on our
 virtual keypad never gets focus. }
function TDrawGrid.CanFocus: Boolean;
begin
  Result := false;
end;

{== TNumericKeypad ====================================================}

type
  {: This type is used to describe a cell in the Numpad drawgrid. }
  TKeyData = record
    Caption: string; { Display string for the cell }
    vkey: Word; { Virtual key code to generate when cell is
      clicked, 0 for none. }
    KeyChar: Char; { Character to generate when the cell is clicked,
      #0 for none. }
    Color: TColor; { Background color of the cell }
    merged: Boolean; { True if the cell is merged with another to
      form a larger virtual button }
    merged_with: TGridCoord; { Cell it is merged with }
  end; { TKeyData }

var
  { The following array describes our virtual keypad. Each entry
    corresponds to a cell in the Numpad drawgrid. The entry order
   is in [rowindex, columnindex] format.   }
  NumpadDescription: array [0 .. 4, 0 .. 3] of TKeyData = (
    { First row of virtual buttons }
    ((Caption: ''; vkey: 0; KeyChar: #0; Color: clBtnFace;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '/'; vkey: 0; KeyChar: '/'; Color: clBtnFace;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '*'; vkey: 0; KeyChar: '*'; Color: clBtnFace;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '-'; vkey: 0; KeyChar: '-'; Color: clBtnFace;
    merged: false; merged_with: (X: - 1; Y: - 1))),

    { Second row of virtual buttons }
    ((Caption: '7'; vkey: 0; KeyChar: '7'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '8'; vkey: 0; KeyChar: '8'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '9'; vkey: 0; KeyChar: '9'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '+'; vkey: 0; KeyChar: '+'; Color: clBtnFace;
    merged: true; merged_with: (X: 3; Y: 2))),

    { Third row of virtual buttons }
    ((Caption: '4'; vkey: 0; KeyChar: '4'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '5'; vkey: 0; KeyChar: '5'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '6'; vkey: 0; KeyChar: '6'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '+'; vkey: 0; KeyChar: '+'; Color: clBtnFace;
    merged: true; merged_with: (X: 3; Y: 1))),

    { Forth row of virtual buttons }
    ((Caption: '1'; vkey: 0; KeyChar: '1'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '2'; vkey: 0; KeyChar: '2'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: '3'; vkey: 0; KeyChar: '3'; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: 'Enter'; vkey: VK_RETURN; KeyChar: #0; Color: clBtnFace;
    merged: true; merged_with: (X: 3; Y: 4))),

    { Fifth row of virtual buttons }
    ((Caption: '0'; vkey: 0; KeyChar: '0'; Color: clWhite;
    merged: true; merged_with: (X: 1; Y: 4)),
    (Caption: '0'; vkey: 0; KeyChar: '0'; Color: clWhite;
    merged: true; merged_with: (X: 0; Y: 4)),
    (Caption: ''; vkey: 0; KeyChar: #0; Color: clWhite;
    merged: false; merged_with: (X: - 1; Y: - 1)),
    (Caption: 'Enter'; vkey: VK_RETURN; KeyChar: #0; Color: clBtnFace;
    merged: true; merged_with: (X: 3; Y: 3))));

{—— Constructors, destructors, form methods ———————————————————————————}

{: Move the form to the top right of the workarea. }
procedure TNumericKeypad.FormCreate(Sender: TObject);
var
  r: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
  SetBounds(r.Right - width, r.Top, width, height);
end; { TNumericKeypad.FormCreate }

{—— Action, button, menu events ———————————————————————————————————————}
{—— Other events triggered by user action —————————————————————————————}

procedure TNumericKeypad.NumpadClick(Sender: TObject);
var
  desc: TKeyData;
  wnd: HWND;
begin
  desc := NumpadDescription[FLastActiveCell.Y, FLastActiveCell.X];
  if desc.vkey > 0 then
    PostKeyEx32(desc.vkey, [], false)
  else if desc.KeyChar <> #0 then begin
    wnd := Windows.GetFocus;
    if wnd <> 0 then
      PostMessage(wnd, WM_CHAR, Ord(desc.KeyChar), 0)
    else
      SendText(desc.KeyChar);
  end;
end;

{—— Other event ———————————————————————————————————————————————————————}

procedure TNumericKeypad.NumpadDrawCell(Sender: TObject;
  ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  mousecell: TGridCoord;
  grid: TDrawGrid;
  pt: TPoint;
  btnRect: TRect;
  desc: TKeyData;
begin
  grid := Sender as TDrawGrid;
  pt := grid.ScreenToClient(mouse.CursorPos);
  grid.MouseToCell(pt.X, pt.Y, mousecell.X, mousecell.Y);
  if SameButton(mousecell, GridCoord(ACol, ARow)) then
    grid.Canvas.Brush.Color := clYellow
  else
    grid.Canvas.Brush.Color := grid.Color;
  grid.Canvas.FillRect(Rect);

  desc := NumpadDescription[ARow, ACol];
  grid.Canvas.Brush.Color := desc.Color;
  grid.Canvas.Pen.Color := clBlack;
  btnRect := Rect;
  if desc.merged then begin
    if desc.merged_with.X < ACol then
      btnRect.Left := btnRect.Left - WidthOfRect(Rect)
    else if desc.merged_with.X > ACol then
      btnRect.Right := btnRect.Right + WidthOfRect(Rect)
    else if desc.merged_with.Y < ARow then
      btnRect.Top := btnRect.Top - HeightOfRect(Rect)
    else if desc.merged_with.Y > ARow then
      btnRect.Bottom := btnRect.Bottom + HeightOfRect(Rect);
  end; { If }
  InflateRect(btnRect, -2, -2);
  grid.Canvas.RoundRect(btnRect.Left, btnRect.Top, btnRect.Right,
    btnRect.Bottom, 12, 12);
  if (ACol = 3) and (ARow >= 3) then begin
    grid.Canvas.font.Size := 8;
    grid.Canvas.font.Style := [];
  end
  else begin
    grid.Canvas.font.Size := 12;
    grid.Canvas.font.Style := [fsBold];
  end;
  DrawText(grid.Canvas.Handle, Pchar(desc.Caption),
    Length(desc.Caption),
    btnRect,
    DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX);
end; { TNumericKeypad.NumpadDrawCell }

procedure TNumericKeypad.TimerTimer(Sender: TObject);
  function ClickDone: Boolean;
  begin
    Result := ValidCell(FLastActiveCell)
      and FMouseDown
      and (GetKeyState(VK_LBUTTON) >= 0);
  end; { ClickDone }

var
  pt: TPoint;
  coord: TGridCoord;
begin { TimerTimer }
  if not Showing then
    Exit;

  pt := Numpad.ScreenToClient(mouse.CursorPos);
  Numpad.MouseToCell(pt.X, pt.Y, coord.X, coord.Y);
  if not SameButton(coord, FLastActiveCell) then begin
    InvalidateButton(FLastActiveCell);
    FLastActiveCell := coord;
    if ValidCell(coord) then
      InvalidateButton(coord);
  end; { If }
  if ClickDone then begin
    FMouseDown := false;
    Numpad.Click;
  end;
end; { TNumericKeypad.TimerTimer }

{—— Message handlers ——————————————————————————————————————————————————}

procedure TNumericKeypad.WMMouseActivate(var msg: TWMMouseActivate);
begin
  msg.Result := MA_NOACTIVATEANDEAT;
  FMouseDown := true;
end; { TNumericKeypad.WMMouseActivate }

{—— Property setters and getters ——————————————————————————————————————}
{—— Other methods —————————————————————————————————————————————————————}

{: Remove the caption bar from the form. Doing it this way allows
 us to retain the bsDialog border.  }
procedure TNumericKeypad.CreateParams(var params: TCreateparams);
begin
  inherited;
  params.Style := params.Style and not WS_CAPTION;
  params.exstyle :=
    params.exstyle or WS_EX_NOACTIVATE or WS_EX_TOPMOST or
    WS_EX_TOOLWINDOW;
  params.WndParent := 0;
end; { TNumericKeypad.CreateParams }

{: Check if the passed grid coordinates map to the same virtual
 button or not. }
function TNumericKeypad.SameButton(A, B: TGridCoord): Boolean;
var
  desc: TKeyData;
begin { SameButton }
  Result := ValidCell(A) and ValidCell(B);
  if Result and not CoordsEqual(A, B) then begin
    desc := NumpadDescription[A.Y, A.X];
    Result := desc.merged and CoordsEqual(B, desc.merged_with);
  end; { If }
end; { TNumericKeypad.SameButton }

procedure TNumericKeypad.InvalidateButton(const coord: TGridCoord);
var
  desc: TKeyData;
begin
  if not ValidCell(coord) then
    Exit;
  desc := NumpadDescription[coord.Y, coord.X];
  Numpad.InvalidateCell(coord.X, coord.Y);
  if desc.merged then
    with desc.merged_with do
      Numpad.InvalidateCell(X, Y);
end; { TNumericKeypad.InvalidateButton }

procedure TNumericKeypad.WMEnable(var msg: TWMEnable);
begin
  inherited;
  if not msg.enabled then
    EnableWindow(Handle, true);
end;

initialization

NumpadDescription[4, 2].Caption := DecimalSeparator;
NumpadDescription[4, 2].KeyChar := DecimalSeparator;

end.
