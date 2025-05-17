unit Font_Form2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Font_Form;

type
  TBaseForm2 = class(TBaseForm)
  private
    FFont: TFont;
    procedure SetFont(Value: TFont);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor  Destroy; override;
  published
    property ATestFont2: TFont read FFont write SetFont;
  end;

var
  BaseForm2: TBaseForm2;

implementation

{$R *.DFM}

{ TBaseForm2 }

constructor TBaseForm2.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FFont:= TFont.Create;
end;

destructor TBaseForm2.Destroy;
begin
  FFont.Destroy;
  inherited;
end;

procedure TBaseForm2.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

end.
