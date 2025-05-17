unit uMukaLegendaSW;

interface

uses
  Forms, Graphics, Messages, Windows, Controls, Classes;

type
  TMukaConst = class
  const
    MARGEM_TOP = 6 ;   // Margem superior da legenda
    LEFT_RET = 10;     // Posição esquerda do retangulo de cor da legenda
    RIGHT_RET = 33;    // Posição direita do retangulo de cor da legenda
    HEIGHT_RET = 13;   // Altura do retangulo de cor da legenda
    LEFT_TEXT = 37;    // Posição esquerda do Texto da legenda
    HEIGHT_LINHA = 18; // Tamanho da altura das linhas
  end;

  TMukaLegendaSW = class;

  TLegendaItem = class(TCollectionItem)
  private
    FColor: TColor;
    FCaption: TCaption;
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
  published
    property Color : TColor read FColor write SetColor;
    property Caption : TCaption read FCaption write SetCaption;
  end;

  TLegendas = class(TCollection)
    FMukaLegenda : TMukaLegendaSW;
    function GetItem(Index: Integer): TLegendaItem;
    procedure SetItem(Index: Integer; Value: TLegendaItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(pCompLegenda: TMukaLegendaSW);
    function Add: TLegendaItem;
    property Items[Index: Integer]: TLegendaItem read GetItem write SetItem; default;
  end;

  TMukaLegendaSW = class(TScrollingWinControl)
  private
    FCanvas: TCanvas;
    FLegendas: TLegendas;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
  published
    property Legendas: TLegendas read FLegendas write FLegendas;
    property Color;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Muka',[TMukaLegendaSW]);
end;

{ TMukaLegendaSW }

constructor TMukaLegendaSW.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  DoubleBuffered := True;

  FLegendas := TLegendas.Create(Self);
end;

procedure TMukaLegendaSW.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_BORDER;
end;

destructor TMukaLegendaSW.Destroy;
begin
  FCanvas.Free;
  FLegendas.Free;
  inherited;
end;

procedure TMukaLegendaSW.Paint;
var
  li, lVert, lTop : integer;
begin
  lVert := VertScrollBar.Position;

  FCanvas.Brush.Style := bsSolid;
  FCanvas.Brush.Color := Color;
  FCanvas.FillRect(ClientRect);

  for li := 0 to FLegendas.Count -1 do
  begin
    lTop := TMukaConst.MARGEM_TOP + (li * TMukaConst.HEIGHT_LINHA) - lVert;
    FCanvas.Pen.Color := clBlack;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Color := FLegendas[li].Color;
    FCanvas.RoundRect(TMukaConst.LEFT_RET, lTop, TMukaConst.RIGHT_RET, lTop + TMukaConst.HEIGHT_RET,5,5);
    FCanvas.Brush.Style := bsClear;
    FCanvas.TextOut(TMukaConst.LEFT_TEXT, lTop, FLegendas[li].Caption);
  end;
end;

procedure TMukaLegendaSW.PaintWindow(DC: HDC);
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

procedure TMukaLegendaSW.WMNCHitTest(var Message: TWMNCHitTest);
begin
  DefaultHandler(Message);
end;

procedure TMukaLegendaSW.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

{ TLegendaItem }

procedure TLegendaItem.SetCaption(const Value: TCaption);
begin
  FCaption := Value;
  Changed(False);
end;

procedure TLegendaItem.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed(False);
end;

{ TLegendas }

function TLegendas.Add: TLegendaItem;
begin
  Result := TLegendaItem(inherited Add);
end;

constructor TLegendas.Create(pCompLegenda: TMukaLegendaSW);
begin
  inherited Create(TLegendaItem);
  FMukaLegenda := pCompLegenda;
end;

function TLegendas.GetItem(Index: Integer): TLegendaItem;
begin
  Result := TLegendaItem(inherited GetItem(Index));
end;

function TLegendas.GetOwner: TPersistent;
begin
  Result := FMukaLegenda;
end;

procedure TLegendas.SetItem(Index: Integer; Value: TLegendaItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TLegendas.Update(Item: TCollectionItem);
var
  lLeg : integer;
begin
  inherited;
  lLeg := FMukaLegenda.Legendas.Count;
  FMukaLegenda.VertScrollBar.Range := (lLeg * TMukaConst.HEIGHT_LINHA) + TMukaConst.MARGEM_TOP;
  FMukaLegenda.Invalidate;
end;

end.
