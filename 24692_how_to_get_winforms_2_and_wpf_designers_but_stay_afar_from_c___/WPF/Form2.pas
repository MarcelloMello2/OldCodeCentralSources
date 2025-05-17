unit Form2;

interface

uses
  FormsDsg,
  System.Windows,
  WPFUtils;

type
  TForm2 = class(TWPFAdapter<TForm2Dsg>)
  strict private
    procedure BtnOkClick(Sender: TObject; E: RoutedEventArgs);
  public
    constructor Create;
  end;

implementation

procedure TForm2.BtnOkClick(Sender: TObject; E: RoutedEventArgs);
begin
  with WPFObject do begin
    Close;
  end;
end;

constructor TForm2.Create;
begin
  inherited Create;
  with WPFObject do begin
    Include(BtnOk.Click,BtnOkClick);
  end;
end;

end.
