unit uMukaEdit;

interface

uses
  StdCtrls, Messages, Classes, Controls, Graphics, Windows;

type
  TMukaEdit = class(TEdit)
  private
    FCanvas: TCanvas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
  end;


  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Muka',[TMukaEdit]);
end;

{ TMukaEdit }

constructor TMukaEdit.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  DoubleBuffered := True;
end;

destructor TMukaEdit.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TMukaEdit.Paint;
begin
  FCanvas.Brush.Color := clSkyBlue;
  FCanvas.Brush.Style := bsSolid;
  FCanvas.Pen.Color := clBlack;
  FCanvas.Pen.Width := 1;
  FCanvas.Rectangle(ClientRect);
end;

procedure TMukaEdit.PaintWindow(DC: HDC);
begin
  inherited;
  FCanvas.Lock;
  try
    FCanvas.Handle := DC;
    try
      TControlCanvas(FCanvas).UpdateTextFlags;
      Paint;
    finally
      FCanvas.Handle := 0;
    end;
  finally
    FCanvas.Unlock;
  end;
end;

procedure TMukaEdit.WMPaint(var Message: TWMPaint);
begin
  //ControlState := ControlState + [csCustomPaint];
  inherited;
  //ControlState := ControlState - [csCustomPaint];
end;

end.
