unit AdvGeometriesForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, D2D1, Direct2D;

type
  TFormAdvGeometries = class(TForm)
  private
    d2dCanvas: TDirect2DCanvas;
    FGridPatternBitmapBrush: ID2D1BitmapBrush;
//    FRadialGradientBrush: ID2D1RadialGradientBrush;
    FRadialGradientBrush: ID2D1Brush;
    FSceneBrush: ID2D1SolidColorBrush;
    FLeftMountainGeometry: ID2D1PathGeometry;
    FRightMountainGeometry: ID2D1PathGeometry;
    FRiverGeometry: ID2D1PathGeometry;
    FSunGeometry: ID2D1PathGeometry;
    procedure CreateDeviceIndependentResources;
    procedure CreateDeviceResources;
    procedure Create_FGridPatternBitmapBrush;
    procedure Create_FSunGeometry;
    procedure Create_FLeftMountainGeometry;
    procedure Create_FRightMountainGeometry;
    procedure Create_FRiverGeometry;
    procedure Create_FRadialGradientBrush;
    procedure Create_FSceneBrush;
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure Resize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormAdvGeometries: TFormAdvGeometries;

implementation

{$R *.dfm}

constructor TFormAdvGeometries.Create(AOwner: TComponent);
begin
  inherited;

  if not TDirect2DCanvas.Supported then
    raise Exception.Create('Direct2D not supported!');

  d2dCanvas := TDirect2DCanvas.Create(Handle);

  CreateDeviceIndependentResources;
end;

procedure TFormAdvGeometries.Resize;
var
  HwndTarget: ID2D1HwndRenderTarget;
begin
  inherited;

  if Assigned(d2dCanvas) then
    if Supports(
      d2dCanvas.RenderTarget, ID2D1HwndRenderTarget, HwndTarget) then
        HwndTarget.Resize(D2D1SizeU(ClientWidth, ClientHeight));

  Invalidate;
end;

procedure TFormAdvGeometries.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1; // http://chrisbensen.blogspot.com/2009/09/touch-demo-part-i.html
end;

destructor TFormAdvGeometries.Destroy;
begin
  d2dCanvas.Free;
  inherited;
end;

procedure TFormAdvGeometries.Create_FRadialGradientBrush;
var
//  aGradientStops: array of TD2D1GradientStop;
//  aGradBrushProps: TD2D1RadialGradientBrushProperties;
//  aGradStopsCollection: ID2D1GradientStopCollection;

  gradColors: array of TColor;
begin
  SetLength(gradColors, 3);
  gradColors[0] := TColor($00D7FF); // Gold (D2D1Helper.h)
  gradColors[1] := TColor($00A5FF); // Orange (D2D1Helper.h)
  gradColors[2] := TColor($0045FF); // OrangeRed (D2D1Helper.h)


  // this is a place-holder.
  // Code below assumes equal spread for positions in gradient stops
  FRadialGradientBrush := d2dCanvas.CreateBrush(
    gradColors,
    D2D1PointF(330, 330),
    D2D1PointF(140, 140),
    140,
    140
    );

//  SetLength(aGradientStops, 3);
//  aGradientStops[0].color := D2D1ColorF($FFD700, 1); // "Gold" from D2D1Helper.h
//  aGradientStops[0].position := 0;
//  aGradientStops[1].color := D2D1ColorF($FFA500, 1); // "Orange" from D2D1Helper.h
//  aGradientStops[1].position := 0.85;
//  aGradientStops[2].color := D2D1ColorF($FF4500, 1); // "OrangeRed" from D2D1Helper.h
//  aGradientStops[2].position := 1;
//
//  d2dCanvas.RenderTarget.CreateGradientStopCollection(
//    @aGradientStops,
//    3,
//    D2D1_GAMMA_2_2,
//    D2D1_EXTEND_MODE_CLAMP,
//    aGradStopsCollection
//    );
//
//  aGradBrushProps.center := D2D1PointF(330, 330);
//  aGradBrushProps.gradientOriginOffset := D2D1PointF(140, 140);
//  aGradBrushProps.radiusX := 140;
//  aGradBrushProps.radiusY := 140;
//
//  d2dCanvas.RenderTarget.CreateRadialGradientBrush(
//    aGradBrushProps,
//    nil,
//    aGradStopsCollection,
//    FRadialGradientBrush
//    );
end;

procedure TFormAdvGeometries.Create_FLeftMountainGeometry;
var
  aSink: ID2D1GeometrySink;
  points: array of TD2DPoint2f;
begin
  D2DFactory.CreatePathGeometry(FLeftMountainGeometry);
  FLeftMountainGeometry.Open(aSink);
  try
    aSink.SetFillMode(D2D1_FILL_MODE_WINDING);

    aSink.BeginFigure(
      D2D1PointF(346,255),
      D2D1_FIGURE_BEGIN_FILLED
      );

    SetLength(points, 5);
    points[0] := D2D1PointF(267, 177);
    points[1] := D2D1PointF(236, 192);
    points[2] := D2D1PointF(212, 160);
    points[3] := D2D1PointF(156, 255);
    points[4] := D2D1PointF(346, 255);
    aSink.AddLines(@points[0], Length(points));

    aSink.EndFigure(D2D1_FIGURE_END_CLOSED);

  finally
    aSink.Close;
  end;
end;

procedure TFormAdvGeometries.Create_FRightMountainGeometry;
var
  aSink: ID2D1GeometrySink;
  points: array of TD2DPoint2f;
begin
  D2DFactory.CreatePathGeometry(FRightMountainGeometry);
  FRightMountainGeometry.Open(aSink);
  try
    aSink.SetFillMode(D2D1_FILL_MODE_WINDING);

    aSink.BeginFigure(
      D2D1PointF(575, 263),
      D2D1_FIGURE_BEGIN_FILLED
      );
    SetLength(points, 7);
    points[0] := D2D1PointF(481, 146);
    points[1] := D2D1PointF(449, 181);
    points[2] := D2D1PointF(433, 159);
    points[3] := D2D1PointF(401, 214);
    points[4] := D2D1PointF(381, 199);
    points[5] := D2D1PointF(323, 263);
    points[6] := D2D1PointF(575, 263);
    aSink.AddLines(@points[0], Length(points));

    aSink.EndFigure(D2D1_FIGURE_END_CLOSED);

  finally
    aSink.Close;
  end;
end;

procedure TFormAdvGeometries.Create_FRiverGeometry;
var
  aSink: ID2D1GeometrySink;
begin
  D2DFactory.CreatePathGeometry(FRiverGeometry);
  FRiverGeometry.Open(aSink);
  try
    aSink.SetFillMode(D2D1_FILL_MODE_WINDING);
    aSink.BeginFigure(
      D2D1PointF(183, 392),
      D2D1_FIGURE_BEGIN_FILLED
      );

    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(238, 284),
        D2D1PointF(472, 345),
        D2D1PointF(356 ,303)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(237, 261),
        D2D1PointF(333, 256),
        D2D1PointF(333, 256)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(335, 257),
        D2D1PointF(241, 261),
        D2D1PointF(411, 306)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(574, 350),
        D2D1PointF(288, 324),
        D2D1PointF(296, 392)
      )
    );

    aSink.EndFigure(D2D1_FIGURE_END_OPEN);
  finally
    aSink.Close;
  end;
end;

procedure TFormAdvGeometries.Create_FSceneBrush;
begin
  d2dCanvas.RenderTarget.CreateSolidColorBrush(
    D2D1ColorF(clBlack, 1),
    nil,
    FSceneBrush);
end;

procedure TFormAdvGeometries.Create_FSunGeometry;
var
  aSink: ID2D1GeometrySink;
begin
  D2DFactory.CreatePathGeometry(FSunGeometry);
  FSunGeometry.Open(aSink);
  try
    aSink.SetFillMode(D2D1_FILL_MODE_WINDING);

    aSink.BeginFigure(
      D2D1PointF(270, 255),
      D2D1_FIGURE_BEGIN_FILLED
      );

    aSink.AddArc(
      D2D1ArcSegment(
        D2D1PointF(440, 255),
        D2D1SizeF(85, 85),
        0,
        D2D1_SWEEP_DIRECTION_CLOCKWISE,
        D2D1_ARC_SIZE_SMALL
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_CLOSED);

    aSink.BeginFigure(
      D2D1PointF(299, 182),
      D2D1_FIGURE_BEGIN_HOLLOW
      );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(299,182),
        D2D1PointF(294,176),
        D2D1PointF(285,178)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(276,179),
        D2D1PointF(272,173),
        D2D1PointF(272,173)
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_OPEN);


    aSink.BeginFigure(
      D2D1PointF(354, 156),
      D2D1_FIGURE_BEGIN_HOLLOW
      );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(354,156),
        D2D1PointF(358,149),
        D2D1PointF(354,142)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(349,134),
        D2D1PointF(354,127),
        D2D1PointF(354,127)
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_OPEN);


    aSink.BeginFigure(
      D2D1PointF(322, 164),
      D2D1_FIGURE_BEGIN_HOLLOW
      );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(322,164),
        D2D1PointF(322,156),
        D2D1PointF(314,152)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(306,149),
        D2D1PointF(305,141),
        D2D1PointF(305,141)
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_OPEN);


    aSink.BeginFigure(
      D2D1PointF(385, 164),
      D2D1_FIGURE_BEGIN_HOLLOW
      );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(385,164),
        D2D1PointF(392,161),
        D2D1PointF(394,152)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(395,144),
        D2D1PointF(402,141),
        D2D1PointF(402,142)
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_OPEN);


    aSink.BeginFigure(
      D2D1PointF(408, 182),
      D2D1_FIGURE_BEGIN_HOLLOW
      );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(408,182),
        D2D1PointF(416,184),
        D2D1PointF(422,178)
      )
    );
    aSink.AddBezier(
      D2D1BezierSegment(
        D2D1PointF(428,171),
        D2D1PointF(435,173),
        D2D1PointF(435,173)
      )
    );
    aSink.EndFigure(D2D1_FIGURE_END_OPEN);

  finally
    aSink.Close;
  end;
end;

procedure TFormAdvGeometries.Create_FGridPatternBitmapBrush;
var
  gridBrush: ID2D1SolidColorBrush;
  bmpBrushProps: D2D1_BITMAP_BRUSH_PROPERTIES;
  bitmapRenderTarget: ID2D1BitmapRenderTarget;
  bmpSize: D2D_SIZE_F;
  gridBitmap: ID2D1Bitmap;
begin
  bmpSize.width := 10;
  bmpSize.height := 10;
  d2dCanvas.RenderTarget.CreateCompatibleRenderTarget(
    @bmpSize, nil, nil, 0, bitmapRenderTarget);
  bitmapRenderTarget.CreateSolidColorBrush(
    D2D1ColorF(0.93, 0.94, 0.96, 1), nil, gridBrush);
  bitmapRenderTarget.BeginDraw;
  bitmapRenderTarget.FillRectangle(Rect(0, 0, 10, 1), gridBrush);
  bitmapRenderTarget.FillRectangle(Rect(0, 0, 1, 10), gridBrush);
  bitmapRenderTarget.EndDraw;
  bitmapRenderTarget.GetBitmap(gridBitmap);
  bmpBrushProps.extendModeX := D2D1_EXTEND_MODE_WRAP;
  bmpBrushProps.extendModeY := D2D1_EXTEND_MODE_WRAP;
  bmpBrushProps.interpolationMode := 0; // could be 1
  d2dCanvas.RenderTarget.CreateBitmapBrush(
    gridBitmap, @bmpBrushProps, nil, FGridPatternBitmapBrush);
end;

procedure TFormAdvGeometries.CreateDeviceResources;
begin
  Create_FSceneBrush;
  Create_FRadialGradientBrush;
  Create_FGridPatternBitmapBrush;
end;

procedure TFormAdvGeometries.CreateDeviceIndependentResources;
begin
  Create_FSunGeometry;
  Create_FLeftMountainGeometry;
  Create_FRightMountainGeometry;
  Create_FRiverGeometry;
end;

procedure TFormAdvGeometries.Paint;
var defMatrix: TD2DMatrix3x2F;
begin
  inherited;

  CreateDeviceResources;

  d2dCanvas.BeginDraw;
  try
    d2dCanvas.RenderTarget.GetTransform (defMatrix);

    // fill with white color the whole window
    d2dCanvas.RenderTarget.Clear(D2D1ColorF(clWhite));

    // fill canvas with little blue rectangles
    d2dCanvas.Brush.Handle := FGridPatternBitmapBrush;
    d2dCanvas.Rectangle(0, 0, ClientWidth + 50, ClientHeight + 50);

    d2dCanvas.Brush.Handle := FRadialGradientBrush;
    d2dCanvas.FillGeometry(FSunGeometry);

    d2dCanvas.Brush.Handle := FSceneBrush;
    d2dCanvas.DrawGeometry(FSunGeometry);

    d2dCanvas.Brush.Color := TColor($238E6B);  // clOliveDrab
    d2dCanvas.FillGeometry(FLeftMountainGeometry);

    d2dCanvas.Brush.Color := clBlack;
    d2dCanvas.DrawGeometry(FLeftMountainGeometry);

    d2dCanvas.Brush.Color := TColor($FACE87); // clLightSkyBlue
    d2dCanvas.FillGeometry(FRiverGeometry);

    d2dCanvas.Brush.Color := clBlack;
    d2dCanvas.DrawGeometry(FRiverGeometry);

    d2dCanvas.Brush.Color := TColor($32CD9A); // YellowGreen
    d2dCanvas.FillGeometry(FRightMountainGeometry);

    d2dCanvas.Brush.Color := clBlack;
    d2dCanvas.DrawGeometry(FRightMountainGeometry);

  // reset standard transformation
    d2dCanvas.RenderTarget.SetTransform (defMatrix);
  finally
    d2dCanvas.EndDraw;
  end;

end;

end.
