unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FObject: TObject;
    procedure CreateObject;
    procedure FreeObject;
    function ObjectAllocated: Boolean;
    procedure ObjectNeeded;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
   ExistIdents: array[Boolean] of string = ('Object does not exist', 'Object Exists');

{ TForm1 }

procedure TForm1.CreateObject;
begin
  FObject := TObject.Create;
end;

procedure TForm1.FreeObject;
begin
  if ObjectAllocated then
    FreeAndNil(FObject);
end;

function TForm1.ObjectAllocated: Boolean;
begin
  Result := (FObject <> nil);
end;

procedure TForm1.ObjectNeeded;
begin
  if not ObjectAllocated then
    CreateObject;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ObjectNeeded;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FreeObject;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ObjectNeeded;
  ShowMessage(FObject.ClassName);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
//  if ObjectAllocated then
//    Showmessage('Object does not exist')
//  else
//    Showmessage('Object Exists');

  // this does exactly the same as above, but is also a preferred technique
  ShowMessage(ExistIdents[ObjectAllocated]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeObject;
end;

end.
