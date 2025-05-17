unit uMukaLegendaCC;

interface

uses
  Controls, Graphics, Classes;

type
  TMukaLegendaCC = class(TCustomControl)
  private
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
  public
    procedure Paint; override;
    procedure Click; override;
  published
    property TabStop;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Muka',[TMukaLegendaCC]);
end;

{ TMukaLegendaCC }

procedure TMukaLegendaCC.Click;
begin
  inherited;
  SetFocus;
end;

procedure TMukaLegendaCC.CMFocusChanged(var Message: TCMFocusChanged);
begin
  inherited;
  Invalidate;
end;

procedure TMukaLegendaCC.Paint;
begin
  inherited;
  if Focused then
  begin
    Canvas.Brush.Color := clRed;
  end else begin
    Canvas.Brush.Color := clSkyBlue;
  end;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(ClientRect);
end;



end.
