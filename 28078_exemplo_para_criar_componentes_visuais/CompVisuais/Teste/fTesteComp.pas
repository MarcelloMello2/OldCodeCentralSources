unit fTesteComp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMukaLegendaGC, StdCtrls, uEditConteiner, uMukaLegendaCC,
  uMukaLegendaSW, uMukaEdit, ComCtrls, uCompLegendaSW;

type
  TForm1 = class(TForm)
    MukaLegendaGC1: TMukaLegendaGC;
    MukaLegendaCC1: TMukaLegendaCC;
    EditConteiner1: TEditConteiner;
    EditConteiner2: TEditConteiner;
    MukaEdit1: TMukaEdit;
    Button1: TButton;
    StatusBar1: TStatusBar;
    MukaLegendaSW1: TMukaLegendaSW;
    CompLegendaSW1: TCompLegendaSW;
    MukaLegendaSW2: TMukaLegendaSW;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Invalidate;
end;

end.
