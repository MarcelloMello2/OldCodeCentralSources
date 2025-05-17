unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Font_Form2,
  StdCtrls;

type
  TForm1 = class(TBaseForm2)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Font:= ATestFont;  
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button2.Font:= ATestFont2  
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage(SaveFormSettings.IniFile)
end;

end.
 