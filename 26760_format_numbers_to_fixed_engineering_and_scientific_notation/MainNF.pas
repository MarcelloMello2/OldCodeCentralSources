unit MainNF;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, NumbFmt, ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    StaticText1: TStaticText;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    NumbFormat: TNumberFormatter;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
   0: StaticText1.Caption:=
      NumbFormat.FormatNumberString(Edit1.Text,SpinEdit1.Value,nfNone);
   1: StaticText1.Caption:=
      NumbFormat.FormatNumberString(Edit1.Text,SpinEdit1.Value,nfFixed);
   2: StaticText1.Caption:=
      NumbFormat.FormatNumberString(Edit1.Text,SpinEdit1.Value,nfEngineering);
   3: StaticText1.Caption:=
      NumbFormat.FormatNumberString(Edit1.Text,SpinEdit1.Value,nfScientific);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  MyRealNo: Extended;
begin
  MyRealNo:=3.14159265359E-4;
  case RadioGroup1.ItemIndex of
   0: StaticText1.Caption:=
      NumbFormat.FormatNumber(MyRealNo,SpinEdit1.Value,nfNone);
   1: StaticText1.Caption:=
      NumbFormat.FormatNumber(MyRealNo,SpinEdit1.Value,nfFixed);
   2: StaticText1.Caption:=
      NumbFormat.FormatNumber(MyRealNo,SpinEdit1.Value,nfEngineering);
   3: StaticText1.Caption:=
      NumbFormat.FormatNumber(MyRealNo,SpinEdit1.Value,nfScientific);
  end;


end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NumbFormat.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NumbFormat:=TNumberFormatter.Create;
end;

end.
