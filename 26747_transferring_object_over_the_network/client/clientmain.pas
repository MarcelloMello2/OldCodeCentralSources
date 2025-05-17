unit clientmain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  ComCtrls, StdCtrls,
  Customer;

type
  TForm2 = class(TForm)
    lblIP: TLabel;
    edtIP: TEdit;
    lblPort: TLabel;
    edtPort: TEdit;
    btnConnect: TButton;
    lbl1: TLabel;
    lvCustomers: TListView;
    IdTCPClient1: TIdTCPClient;
    procedure btnConnectClick(Sender: TObject);
  private
    procedure GetCustomers;
    procedure ReadFromStream(AStream: TMemoryStream; AInstance: TObject);
    procedure FillCustomersList(ACustomers: TCustomers);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses
  proxy;

{$R *.dfm}

procedure TForm2.btnConnectClick(Sender: TObject);
begin
  (* Setup connection data *)
  IdTCPClient1.Port := StrToInt(edtPort.Text);
  IdTCPClient1.Host := edtIP.Text;
  IdTCPClient1.Connect;
  (* Get customer's list *)
  GetCustomers;
end;

procedure TForm2.FillCustomersList(ACustomers: TCustomers);
var
  I: Integer;
begin
  for I := 0 to ACustomers.Count - 1 do
  begin
    with lvCustomers.Items.Add do
    begin
      Caption := ACustomers[I].LastName;
      SubItems.Add(ACustomers[I].FirstName);
      SubItems.Add(DateToStr(ACustomers[I].BornDate));
    end;
  end;
end;

procedure TForm2.GetCustomers;
var
  lStream: TMemoryStream;
  lCustomers: TCustomers;
begin
  lStream := TMemoryStream.Create;
  lCustomers := TCustomers.Create;
  try
    (* Send command to server *)
    IdTCPClient1.WriteLn('LOADCUSTOMERS');
    (* Read the server's response *)
    IdTCPClient1.ReadStream(lStream);
    (* Fill lCustomers variable with lStream data *)
    ReadFromStream(lStream, lCustomers);
    (* Fill the customers listview *)
    FillCustomersList(lCustomers);
  finally
    lCustomers.Free;
    lStream.Free;
  end;
end;

procedure TForm2.ReadFromStream(AStream: TMemoryStream; AInstance: TObject);
var
  lProxy: TProxy;
begin
  lProxy := TProxy.Create(nil);
  try
    lProxy.InternalObject := AInstance;
    AStream.Position := 0;
    AStream.ReadComponent(lProxy)
  finally
    lProxy.Free;
  end;
end;

end.
