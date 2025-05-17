unit NumPadTest1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, stdctrls, ExtCtrls, Buttons;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses numpad;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 NumericKeypad.Show;
 BringToFront;
 Button1.Enabled := false;
 memo1.setfocus;
end;

Procedure Evaluate( Var expression: String );
Var
  f1, f2: String;
  operator: char;
  i: Integer;
  res: double;
Begin
  res := 0.0;
  for i:= 1 to length(expression) do
    if expression[i] In ['+','-','/','*'] Then Begin
      f1 := Copy( expression, 1, i-1 );
      f2 := Copy( expression, i+1, maxint );
      operator := expression[i];
      Case operator of
        '+': res := StrToFloat( f1 ) + StrToFloat( f2 );
        '-': res := StrToFloat( f1 ) - StrToFloat( f2 );
        '/': res := StrToFloat( f1 ) / StrToFloat( f2 );
        '*': res := StrToFloat( f1 ) * StrToFloat( f2 );
      End;
      expression := expression + ' = '+formatFloat('#0.####',res);
      Exit;
    End;
  expression := expression + ': ERROR';
End;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
var
  expression: String;
begin
  If Key = #13 Then Begin
    Key := #0;
    If ActiveControl = memo1 Then
      expression := memo1.Lines[ memo1.Lines.count-1]
    Else If ActiveControl Is TEdit Then
      expression := Tedit(Activecontrol).Text
    Else
      Exit;

    Evaluate( expression );
    If ActiveControl = memo1 Then
      memo1.Lines[ memo1.Lines.count-1] := expression+#13#10
    Else If ActiveControl Is TEdit Then
      Tedit(Activecontrol).Text := expression;
  End;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to controlcount-1 do
    if controls[i] Is TCustomedit then
      Tcustomedit( controls[i] ).Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SpeedButton1.Click
end;

end.
