unit Form1;

interface

uses
  System.Windows.Forms,
  FormsDsg;

type
  TForm1 = class(TForm1Dsg)
  strict private
    procedure FormActivated(Sender: TObject; E: EventArgs);
    procedure BtnClickMeClick(Sender: TObject; E: EventArgs);
    procedure BtnClearClick(Sender: TObject; E: EventArgs);
    procedure BtnCloseClick(Sender: TObject; E: EventArgs);
  public
    constructor Create;
  end;

implementation

uses
  Form2;

procedure TForm1.FormActivated(Sender: TObject; E: EventArgs);
begin
  ListEvents.Items.Add('Form activated');
end;

procedure TForm1.BtnClearClick(Sender: TObject; E: EventArgs);
begin
  ListEvents.Items.Clear;
  ListEvents.Items.Add('BtnClear clicked');
end;

procedure TForm1.BtnClickMeClick(Sender: TObject; E: EventArgs);
begin
  ListEvents.Items.Add('BtnClickMe clicked');
  with TForm2.Create do begin
    ShowDialog;
    Free;
  end;
end;

procedure TForm1.BtnCloseClick(Sender: TObject; E: EventArgs);
begin
  ListEvents.Items.Add('BtnClose clicked');
  Close;
end;

constructor TForm1.Create;
begin
  inherited Create;
  Include(Self.Activated,FormActivated);
  Include(Self.BtnClickMe.Click,BtnClickMeClick);
  Include(Self.BtnClear.Click,BtnClearClick);
  Include(Self.BtnClose.Click,BtnCloseClick);
end;

end.
