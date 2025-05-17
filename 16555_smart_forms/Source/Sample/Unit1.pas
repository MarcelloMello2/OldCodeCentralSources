unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Smart_Form,
  StdCtrls;

type
  TForm1 = class(TSmartForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    Button4: TButton;
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowMessage(Storage.Path)
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Storage.Component.DeleteStorage(Storage.Path);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Storage.Component.DeleteSettings(Storage.Path)
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Form2.EditComponents then
    Memo1.Lines.Assign(Form2.Memo1.Lines);
end;

end.
 