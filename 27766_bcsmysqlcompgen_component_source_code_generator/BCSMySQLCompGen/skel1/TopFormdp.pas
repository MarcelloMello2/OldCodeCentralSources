{ *-----------------------------------------------------------------------------
  Driver Program Container For TopForm Component

  This is the main component housing.

  @Author Mr. Arch Brooks
  @Version 1.0

  ------------------------------------------------------------------------------- }

unit TopFormdp;

interface

uses
  DB, Classes, Forms, SysUtils, TopFormwbu;

type
  { *-----------------------------------------------------------------------------
    Top Class For TopForm Dialog

    The overall class for utilization of the TopForm dialog.

    ------------------------------------------------------------------------------- }

  TTopFormdp = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FBookMark: TBookMark;
    /// Connection String
    FConStr: string;
    /// Data Element Name
    FDEName: string;
    /// Table Name
    FTableName: string;
  public
    { Public declarations }
  published
    { Published declarations }
    function Execute: Boolean;
    property RBookMark: TBookMark read FBookMark write FBookMark;
    property RConStr: string read FConStr write FConStr;
    property RDEName: string read FDEName write FDEName;
    property RTableName: string read FTableName write FTableName;
  end;

procedure Register;

implementation

{ *-----------------------------------------------------------------------------
  Execution Function For The Component

  Here is were we instantiate and invoke the default execution of the component.

  @return Sender The top Object for interface.
  ------------------------------------------------------------------------------- }

function TTopFormdp.Execute: Boolean;
begin
  Result := True;
  Application.CreateForm(TTopFormC, TopFormC);
  TopFormC.ADOConnection1.ConnectionString := RConStr;
  TopFormC.ADOConnection1.Connected := True;
  TopFormC.ADOTable1.TableName := RTableName;
  TopFormC.DBEdit1.DataField := RDEName;
  TopFormC.ADOTable1.Active := True;
  TopFormC.ADOTable1.GotoBookmark(RBookMark);
  TopFormC.ShowModal;
  TopFormC.Free;
end;

{ *-----------------------------------------------------------------------------
  Register The TopForm Component

  Here is were we register the component and identify the palate name.

  ------------------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('AB DE Comps', [TTopFormdp]);
end;

end.
