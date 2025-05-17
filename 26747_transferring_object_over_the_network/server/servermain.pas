unit servermain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPServer, StdCtrls;

type
  TForm1 = class(TForm)
    lblPort: TLabel;
    edtPort: TEdit;
    btnStart: TButton;
    IdTCPServer1: TIdTCPServer;
    procedure btnStartClick(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
  private
    procedure WriteToStream(AInstance: TObject; AStream: TMemoryStream);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  customer,
  Proxy;

{$R *.dfm}

procedure TForm1.btnStartClick(Sender: TObject);
begin
  IdTCPServer1.Active := False;
  IdTCPServer1.DefaultPort := StrToInt(edtPort.Text);
  IdTCPServer1.Active := True;
end;

procedure TForm1.IdTCPServer1Execute(AThread: TIdPeerThread);
var
  lCommand: string;
  lStream: TMemoryStream;
  lCustomers: TCustomers;
begin
  (* Read a command *)
  lStream := TMemoryStream.Create;
  lCustomers := TCustomers.Create;
  lCommand := AThread.Connection.ReadLn;
  try
    if lCommand = 'LOADCUSTOMERS' then
    begin
      lCustomers.LoadCustomers;
      WriteToStream(lCustomers, lStream);
      AThread.Connection.WriteStream(lStream, True, True, lStream.Size);
    end;
  finally
    lCustomers.Free;
    lStream.Free;
  end;
end;

procedure TForm1.WriteToStream(AInstance: TObject; AStream: TMemoryStream);
(* Writes a TPersistent to a MemoryStream *)
var
  lProxy: TProxy;
begin
  lProxy := TProxy.Create(nil);
  try
    lProxy.InternalObject := AInstance;
    AStream.WriteComponent(lProxy);
    AStream.Position := 0;
  finally
    lProxy.Free;
  end;
end;

end.
