unit customer;

interface

uses
  Classes;

type
  TDate = TDateTime;

  TCustomer = class(TCollectionItem)
  private
    FFirstName: string;
    FLastName: string;
    FBornDate: TDate;
  published
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property BornDate: TDate read FBornDate write FBornDate;
  end;

  TCustomers = class(TCollection)
  private
    function GetItem(AIndex: Integer): TCustomer;
  public
    constructor Create;
    function Add: TCustomer;
    procedure LoadCustomers;
    property Items[AIndex: Integer]: TCustomer read GetItem; default;
  end;

implementation

uses
  SysUtils;

{ TCustomers }

function TCustomers.Add: TCustomer;
begin
  Result := TCustomer(inherited Add);
end;

constructor TCustomers.Create;
begin
  inherited Create(TCustomer);
end;

function TCustomers.GetItem(AIndex: Integer): TCustomer;
begin
  Result := TCustomer(inherited GetItem(AIndex));
end;

procedure TCustomers.LoadCustomers;
(* This method should load data from an external source, such as a database  *)
begin
   with Add do
   begin
     LastName := 'Ramé';
     FirstName := 'Leonardo';
     BornDate := EncodeDate(1975, 2, 13);
   end;

   with Add do
   begin
     LastName := 'Gomez';
     FirstName := 'Carlos';
     BornDate := EncodeDate(1985, 1, 22);
   end;

   with Add do
   begin
     LastName := 'Stewart';
     FirstName := 'John';
     BornDate := EncodeDate(1980, 5, 12);
   end;
end;

end.
