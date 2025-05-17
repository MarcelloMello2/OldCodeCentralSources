unit uMukaLegendaGC;

interface

uses
  Controls, Graphics, Classes;

type
  TMukaLegendaGC = class(TGraphicControl)
  public
    procedure Paint; override;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Muka',[TMukaLegendaGC]);
end;

{ TMukaLegendaGC }
procedure TMukaLegendaGC.Paint;
begin
  inherited;
  Canvas.Brush.Color := clSkyBlue;
  Canvas.Brush.Style := bsSolid;
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Width := 1;
  Canvas.Rectangle(ClientRect);
end;

end.
