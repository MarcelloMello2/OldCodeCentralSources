unit PackU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses InterfU;

{$R *.dfm}

type
  TPluginInterf = class(TInterfacedObject, IPluginInterf)
    procedure CreateForm(Const InterfParam: IHostInterf);
  end;

  TFormManipulate = class(TInterfacedObject, IFormInterf)
    procedure ShowForm;
  end;

procedure TPluginInterf.CreateForm(Const InterfParam: IHostInterf);
begin
  HostInterf:= InterfParam;
  Form2:= TForm2.Create(nil);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FormInterf:= TFormManipulate.Create;
  SetTls(FormInterf);
end;

procedure TFormManipulate.ShowForm;
begin
  Form2.ShowModal;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  HostInterf.SetCaption('Text from package');
end;

initialization
  PTlsIndx:= TlsGetValue(0);
  PluginInterf:= TPluginInterf.Create;
  SetTls(PluginInterf);

finalization
  PluginInterf:= nil;
  FormInterf:= nil;
  if Form2<>nil then FreeAndNil(Form2);
end.
