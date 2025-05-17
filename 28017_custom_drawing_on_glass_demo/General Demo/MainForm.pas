unit MainForm;
{
  Demonstrates a few custom glass drawing techniques: leveraging the theming API for
  button backgrounds; drawing 24 bit colour image lists; creating and using alpha-aware
  solid brushes; generating a bitmap at runtime (in this case, to create a transparent
  to solid colour gradient); and drawing text on glass using the DrawThemeTextEx API.
  Requires Delphi 2007 or later. For a discussion, please refer to my blog post at
  http://delphihaven.wordpress.com/2010/09/06/custom-drawing-on-glass-2/.

  Chris Rolliston, September 2010
}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ImgList,
  StdCtrls, ExtCtrls;

type
{$IF CompilerVersion < 20.0}
  UnicodeString = WideString;
  
  TLabel = class(StdCtrls.TLabel) //backfill GlowSize property for D2007
  protected
    GlowSize: Integer;
    procedure DoDrawText(var Rect: TRect; Flags: Integer); override;
  end;
{$IFEND}

  TfrmMain = class(TForm)
    ImageList24bit: TImageList;
    lblBtns: TLabel;
    pbxBtnNewNoFix: TPaintBox;
    lblBrushes: TLabel;
    pbxBrushes: TPaintBox;
    pbxBtnOpenNoFix: TPaintBox;
    pbxBtnSaveNoFix: TPaintBox;
    pbxBtnNew: TPaintBox;
    pbxBtnOpen: TPaintBox;
    pbxBtnSave: TPaintBox;
    lblBlur: TLabel;
    pbxScanLine: TPaintBox;
    lblDrawnNormally: TLabel;
    lblDrawnWithWorkaround: TLabel;
    chkExtendedGlass: TCheckBox;
    lblExtendedGlass: TLabel;
    lblTextTest: TLabel;
    pbxTextTest: TPaintBox;
    btnDecGlowSize: TPaintBox;
    btnIncGlowSize: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure pbxBrushesPaint(Sender: TObject);
    procedure pbxThemeBtnMouseEnter(Sender: TObject);
    procedure pbxThemeBtnMouseLeave(Sender: TObject);
    procedure pbxThemeBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbxThemeBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbxThemeBtnPaint(Sender: TObject);
    procedure pbxThemeBtnClick(Sender: TObject);
    procedure pbxThemeBtnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbxScanLinePaint(Sender: TObject);
    procedure chkExtendedGlassClick(Sender: TObject);
    procedure lblExtendedGlassClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbxTextTestPaint(Sender: TObject);
    procedure btnDecGlowSizeClick(Sender: TObject);
    procedure btnIncGlowSizeClick(Sender: TObject);
  private
    FBtnMouseIsIn, FPressedBtn: TControl;
    FOldWindowState: TWindowState;
    FTestTextGlowSize: Integer;
    procedure UpdateFontColor;
  end;

var
  frmMain: TfrmMain;

implementation

uses DwmApi, UxTheme, Math, Themes;

{$R *.dfm}

type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..$FFFFFFF] of TRGBQuad;

procedure ClearRGBQuad(var Q: TRGBQuad); inline;
begin
  LongWord(Q) := 0;
end;

function SameRGBQuad(const Q1, Q2: TRGBQuad): Boolean; inline;
begin
  Result := LongWord(Q1) = LongWord(Q2);
end;

function CreatePreMultipliedRGBQuad(Color: TColor; Alpha: Byte = $FF): TRGBQuad;
begin  //TColor's byte order is red, green, blue, reserved; TRGBQuad's is blue, green, red, alpha
  if Alpha = 0 then
  begin
    ClearRGBQuad(Result);
    Exit;
  end;
  Color := ColorToRGB(Color);
  Result.rgbBlue := MulDiv(GetBValue(Color), Alpha, $FF);
  Result.rgbGreen := MulDiv(GetGValue(Color), Alpha, $FF);
  Result.rgbRed := MulDiv(GetRValue(Color), Alpha, $FF);
  Result.rgbReserved := Alpha;
end;

function CreateSolidBrushWithAlpha(Color: TColor; Alpha: Byte = $FF): HBRUSH;
var
  Info: TBitmapInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.bmiHeader.biSize := SizeOf(Info.bmiHeader);
  Info.bmiHeader.biWidth := 1;
  Info.bmiHeader.biHeight := 1;
  Info.bmiHeader.biPlanes := 1;
  Info.bmiHeader.biBitCount := 32;
  Info.bmiHeader.biCompression := BI_RGB;
  Info.bmiColors[0] := CreatePreMultipliedRGBQuad(Color, Alpha);
  Result := CreateDIBPatternBrushPt(@Info, 0);
end;

procedure DrawGlassText(Canvas: TCanvas; GlowSize: Integer; var Rect: TRect;
  var Text: UnicodeString; Format: DWORD); overload;
var
  DTTOpts: TDTTOpts;
begin
  if Win32MajorVersion < 6 then
  begin
    DrawTextW(Canvas.Handle, PWideChar(Text), Length(Text), Rect, Format);
    Exit;
  end;
  ZeroMemory(@DTTOpts, SizeOf(DTTOpts));
  DTTOpts.dwSize := SizeOf(DTTOpts);
  DTTOpts.dwFlags := DTT_COMPOSITED or DTT_TEXTCOLOR;
  if Format and DT_CALCRECT = DT_CALCRECT then
    DTTOpts.dwFlags := DTTOpts.dwFlags or DTT_CALCRECT;
  DTTOpts.crText := ColorToRGB(Canvas.Font.Color);
  if GlowSize > 0 then
  begin
    DTTOpts.dwFlags := DTTOpts.dwFlags or DTT_GLOWSIZE;
    DTTOpts.iGlowSize := GlowSize;
  end;
  with ThemeServices.GetElementDetails(teEditTextNormal) do
    DrawThemeTextEx(ThemeServices.Theme[teEdit], Canvas.Handle, Part, State,
      PWideChar(Text), Length(Text), Format, @Rect, DTTOpts);
end;

procedure DrawGlassText(Canvas: TCanvas; GlowSize: Integer; var Rect: TRect;
  var Text: UnicodeString; TextFormat: TTextFormat = []); overload;
const
  cTextFormats: array[TTextFormats] of DWORD =
  (DT_BOTTOM, DT_CALCRECT, DT_CENTER, DT_EDITCONTROL, DT_END_ELLIPSIS,
   DT_PATH_ELLIPSIS, DT_EXPANDTABS, DT_EXTERNALLEADING, DT_LEFT,
   DT_MODIFYSTRING, DT_NOCLIP, DT_NOPREFIX, DT_RIGHT, DT_RTLREADING,
   DT_SINGLELINE, DT_TOP, DT_VCENTER, DT_WORDBREAK);
var
  Format: DWORD;
  F: TTextFormats;
begin
  Format := 0;
  for F in TextFormat do
    Format := Format or cTextFormats[F];
  DrawGlassText(Canvas, GlowSize, Rect, Text, Format);
end;

procedure DrawImageListWithAlpha(ImageList: TCustomImageList; Canvas: TCanvas;
  DestX, DestY: Integer; ImageIndex: TImageIndex);
const
  MergeFunc: TBlendFunction = (BlendOp: AC_SRC_OVER; BlendFlags: 0;
    SourceConstantAlpha: 255; AlphaFormat: AC_SRC_ALPHA);
const
  TransPixel: TRGBQuad = (rgbBlue: $FE; rgbGreen: $00; rgbRed: $FF; rgbReserved: $00);
var
  Buffer: TBitmap;
  PixelPtr: PRGBQuad;
  X, Y: Integer;
begin
  {$IF DECLARED(cd32Bit)}
  if ImageList.ColorDepth = cd32Bit then
  begin
    ImageList.Draw(Canvas, DestX, DestY, ImageIndex, dsTransparent, itImage);
    Exit;
  end;
  {$IFEND}
  Buffer := TBitmap.Create;
  try
    Buffer.Canvas.Brush.Color := RGB(TransPixel.rgbRed,
      TransPixel.rgbGreen, TransPixel.rgbBlue);
    Buffer.PixelFormat := pf32bit;
    Buffer.SetSize(ImageList.Width, ImageList.Height);
    ImageList.Draw(Buffer.Canvas, 0, 0, ImageIndex, dsTransparent, itImage);
    for Y := Buffer.Height - 1 downto 0 do
    begin
      PixelPtr := Buffer.ScanLine[Y];
      for X := Buffer.Width - 1 downto 0 do
      begin
        if SameRGBQuad(PixelPtr^, TransPixel) then
          ClearRGBQuad(PixelPtr^)
        else
          PixelPtr.rgbReserved := $FF;
        Inc(PixelPtr);
      end;
    end;
    AlphaBlend(Canvas.Handle, DestX, DestY, ImageList.Width, ImageList.Height,
      Buffer.Canvas.Handle, 0, 0, ImageList.Width, ImageList.Height, MergeFunc)
  finally
    Buffer.Free;
  end;
end;

{$IF CompilerVersion < 20.0}
procedure TLabel.DoDrawText(var Rect: TRect; Flags: Integer);
var
  Text: UnicodeString;
begin
  if GlowSize <= 0 then
  begin
    inherited;
    Exit;
  end;
  Canvas.Font := Font;
  Text := Caption;
  DrawGlassText(Canvas, GlowSize, Rect, Text, DWORD(Flags));
end;
{$IFEND}

{ frmMain }

procedure TfrmMain.chkExtendedGlassClick(Sender: TObject);
begin
  GlassFrame.Enabled := chkExtendedGlass.Checked;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FTestTextGlowSize := 20;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  pbxBrushes.Anchors := [akLeft, akTop, akRight];
  pbxScanLine.Anchors := [akLeft, akTop, akRight];
  //The following props would usually be set at design time in D2009+; they're set at
  //runtime here for D2007 compatibility.
  DoubleBuffered := True;
  lblBtns.GlowSize := 14;
  lblDrawnNormally.GlowSize := 14;
  lblDrawnWithWorkaround.GlowSize := 14;
  lblBrushes.GlowSize := 14;
  lblBlur.GlowSize := 14;
  lblTextTest.GlowSize := 14;
  lblExtendedGlass.GlowSize := 14;
  chkExtendedGlass.DoubleBuffered := True;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  if WindowState <> FOldWindowState then UpdateFontColor;
  FOldWindowState := WindowState;
end;

procedure TfrmMain.btnDecGlowSizeClick(Sender: TObject);
begin
  if FTestTextGlowSize = 0 then Exit;
  Dec(FTestTextGlowSize, 2);
  Invalidate;
end;

procedure TfrmMain.btnIncGlowSizeClick(Sender: TObject);
begin
  Inc(FTestTextGlowSize, 2);
  Invalidate;
end;

procedure TfrmMain.lblExtendedGlassClick(Sender: TObject);
begin
  chkExtendedGlass.Checked := not chkExtendedGlass.Checked;
  UpdateFontColor;
end;

procedure TfrmMain.pbxBrushesPaint(Sender: TObject);
var
  R: TRect;
begin
  R := pbxBrushes.ClientRect;
  with pbxBrushes.Canvas do
  begin
    Brush.Handle := CreateSolidBrushWithAlpha(clGray);
    FrameRect(R);
    InflateRect(R, -2, -2);
    Brush.Handle := CreateSolidBrushWithAlpha(clBlack);
    FrameRect(R);
    InflateRect(R, -1, -1);
    Brush.Handle := CreateSolidBrushWithAlpha(clRed, 100);
    FillRect(R);
  end;
end;

procedure TfrmMain.pbxScanLinePaint(Sender: TObject);
const
  MergeFunc: TBlendFunction = (BlendOp: AC_SRC_OVER; BlendFlags: 0;
    SourceConstantAlpha: $FF; AlphaFormat: AC_SRC_ALPHA);
var
  Bitmap: TBitmap;
  Pixels: PRGBQuadArray;
  X: Integer;
begin
  { As we're only drawing a simple linear gradient, we'll generate a 1 pixel thick
    bitmap and stretch it, as supported by the AlphaBlend API. }
  Bitmap := TBitmap.Create;
  try
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(pbxScanLine.Width, 1);
    Pixels := Bitmap.ScanLine[0];
    for X := Bitmap.Width - 1 downto 0 do
      Pixels[X] := CreatePreMultipliedRGBQuad(clNavy, ($FF * X) div Pred(Bitmap.Width));
    Windows.AlphaBlend(pbxScanLine.Canvas.Handle, 0, 0, pbxScanLine.Width,
      pbxScanLine.Height, Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height,
      MergeFunc);
  finally
    Bitmap.Free;
  end;
end;

procedure TfrmMain.pbxTextTestPaint(Sender: TObject);
var
  R: TRect;
  S: UnicodeString;
begin
  R := pbxTextTest.ClientRect;
  S := Format('Example glass text with a glow size of %d', [FTestTextGlowSize]);
  DrawGlassText(pbxTextTest.Canvas, FTestTextGlowSize, R, S,
    [tfCenter, tfSingleLine, tfVerticalCenter]);
end;

procedure TfrmMain.pbxThemeBtnClick(Sender: TObject);
begin
  MessageDlg((Sender as TControl).Hint, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.pbxThemeBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPressedBtn := (Sender as TPaintBox);
  FPressedBtn.Invalidate;
end;

procedure TfrmMain.pbxThemeBtnMouseEnter(Sender: TObject);
begin
  FBtnMouseIsIn := (Sender as TPaintBox);
  FBtnMouseIsIn.Invalidate;
end;

procedure TfrmMain.pbxThemeBtnMouseLeave(Sender: TObject);
begin
  if FBtnMouseIsIn <> nil then FBtnMouseIsIn.Invalidate;
  FBtnMouseIsIn := nil;
end;

procedure TfrmMain.pbxThemeBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  IsInside: Boolean;
begin
  if Sender = FPressedBtn then
  begin
    IsInside := PtInRect(FPressedBtn.ClientRect, Point(X, Y));
    if IsInside <> (FBtnMouseIsIn = FPressedBtn) then
    begin
      if IsInside then
        FBtnMouseIsIn := FPressedBtn
      else
        FBtnMouseIsIn := nil;
      FPressedBtn.Invalidate;
    end;
  end;
end;

procedure TfrmMain.pbxThemeBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FPressedBtn <> nil then FPressedBtn.Invalidate;
  FPressedBtn := nil;
end;

procedure TfrmMain.pbxThemeBtnPaint(Sender: TObject);
var
  Btn: TPaintBox;
  Elem: TThemedToolBar;
  R: TRect;
begin
  Btn := (Sender as TPaintBox);
  R := Btn.ClientRect;
  if Sender = FPressedBtn then
    if Sender = FBtnMouseIsIn then
      Elem := ttbButtonPressed
    else
      Elem := ttbButtonHot
  else if Sender = FBtnMouseIsIn then
    Elem := ttbButtonHot
  else
    Elem := ttbButtonNormal;
  ThemeServices.DrawElement(Btn.Canvas.Handle,
    ThemeServices.GetElementDetails(Elem), R);
  R.Left := (R.Right - ImageList24bit.Width) div 2;
  R.Top := (R.Bottom - ImageList24bit.Height) div 2;
  if Pos('NoFix', Btn.Name) = 0 then
    DrawImageListWithAlpha(ImageList24bit, Btn.Canvas, R.Left, R.Top, Btn.Tag)
  else
    ImageList24bit.Draw(Btn.Canvas, R.Left, R.Top, Btn.Tag);
end;

procedure TfrmMain.UpdateFontColor;
begin
  if (Win32MinorVersion = 0) and (Win32MajorVersion = 6) then
    if (WindowState = wsMaximized) and GlassFrame.FrameExtended then
      Font.Color := clWhite
    else
      Font.Color := clWindowText;
end;

end.
