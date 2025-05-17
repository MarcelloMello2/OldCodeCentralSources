unit BCSMySQLCompGendp;

interface

uses
  Forms, SysUtils, Classes, BCSMySQLCompGenwbu;

type
  TBCSMySQLCompGendp = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
    function Execute: Boolean;
  end;

procedure Register;

implementation

function TBCSMySQLCompGendp.Execute: Boolean;
begin
  Result := True;
  Application.CreateForm(TBCSMySQLCompGenC, BCSMySQLCompGenc);
  BCSMySQLCompGenC.ShowModal;
  BCSMySQLCompGenC.Free;
end;

procedure Register;
begin
  RegisterComponents('AB DE Comps', [TBCSMySQLCompGendp]);
end;

end.
