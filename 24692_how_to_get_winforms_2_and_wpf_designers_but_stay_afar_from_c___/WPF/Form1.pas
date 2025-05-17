unit Form1;

interface

uses
  FormsDsg,
  System.Windows,
  WPFUtils;

type
  TForm1 = class (TWPFAdapter<TForm1Dsg>)
  strict private
    procedure FormActivated(Sender: TObject; E: EventArgs);
    procedure BtnClickMeClick(Sender: TObject; E: RoutedEventArgs);
    procedure BtnClearClick(Sender: TObject; E: RoutedEventArgs);
    procedure BtnCloseClick(Sender: TObject; E: RoutedEventArgs);
  public
    constructor Create;
  end;

implementation

uses
  Form2;

procedure TForm1.FormActivated(Sender: TObject; E: EventArgs);
begin
  with WPFObject do begin
    ListEvents.Items.Add('Form activated');
  end;
end;

procedure TForm1.BtnClearClick(Sender: TObject; E: RoutedEventArgs);
begin
  with WPFObject do begin
    ListEvents.Items.Clear;
    ListEvents.Items.Add('BtnClear clicked');
  end;
end;

procedure TForm1.BtnClickMeClick(Sender: TObject; E: RoutedEventArgs);
begin
  with WPFObject do begin
    ListEvents.Items.Add('BtnClickMe clicked');
    with TForm2.Create.WPFObject do begin
      Owner := WPFObject;
      ShowDialog;
    end;
  end;
end;

procedure TForm1.BtnCloseClick(Sender: TObject; E: RoutedEventArgs);
begin
  with WPFObject do begin
    ListEvents.Items.Add('BtnClose clicked');
    Close;
  end;
end;

constructor TForm1.Create;
begin
  inherited Create;
  with WPFObject do begin
    Include(Activated,FormActivated);
    Include(BtnClickMe.Click,BtnClickMeClick);
    Include(BtnClear.Click,BtnClearClick);
    Include(BtnClose.Click,BtnCloseClick);
  end;
end;

end.
