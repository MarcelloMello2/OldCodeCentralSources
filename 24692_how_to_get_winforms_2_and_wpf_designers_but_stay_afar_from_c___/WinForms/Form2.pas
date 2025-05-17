unit Form2;

interface

uses
  System.Windows.Forms,
  FormsDsg;

type
  TForm2 = class(TForm2Dsg)
  strict private
    procedure BtnOkClick(Sender: TObject; E: EventArgs);
  public
    constructor Create;
  end;

implementation

procedure TForm2.BtnOkClick(Sender: TObject; E: EventArgs);
begin
  Close;
end;

constructor TForm2.Create;
begin
  inherited Create;
  Include(Self.BtnOk.Click,BtnOkClick);
end;

end.
