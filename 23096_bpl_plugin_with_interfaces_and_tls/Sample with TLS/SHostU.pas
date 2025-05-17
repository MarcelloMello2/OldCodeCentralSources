unit SHostU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)          
    GroupBox1: TGroupBox;
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

{$R *.dfm}

Uses InterfU;

Type
  THostInterf = class(TInterfacedObject, IHostInterf)
    procedure SetCaption(Const Text: String);
  end;

var
  ModuleH: HMODULE;
  TlsIndx: Cardinal;

procedure THostInterf.SetCaption(Const Text: String);
begin
  Form1.Caption:= Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PTlsIndx:= addr(TlsIndx);
  TlsSetValue(0, PTlsIndx);
  ModuleH:= SysUtils.LoadPackage('Plugin.bpl');
  if ModuleH<>0 then begin
    Button2.Enabled:= True;
    Button3.Enabled:= True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  GetTls(IInterface(PluginInterf)); 
  PluginInterf.CreateForm(HostInterf);
  GetTls(IInterface(FormInterf));
  FormInterf.ShowForm;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  PluginInterf:= nil;
  FormInterf:= nil;
  UnLoadPackage(ModuleH);
end;

initialization                                             
  HostInterf:= THostInterf.Create;
finalization
  HostInterf:= nil;
end.
