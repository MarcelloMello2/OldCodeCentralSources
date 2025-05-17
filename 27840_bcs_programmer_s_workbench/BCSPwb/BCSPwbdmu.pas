{*------------------------------------------------------------------------------
 Data Module for Programmer's Workbench

 Data nodule unit to house data funcitons for the Programmer's Workbench.

 @author    Mr. Arch Brooks
 @version   2010.08.02   Mr. Arch Brooks Initial revision

 -------------------------------------------------------------------------------}

unit BCSPwbdmu;

interface

uses
  ADODB, Classes, DB, SysUtils, DBTables, RpDefine, RpCon, RpConDS, RpConBDE;

type

  {*------------------------------------------------------------------------------
   Primary class for the data module.  Here is where all components related to
   data access, manipulation and utilization are housed.  Also the majority of
   applied business rules should reside here.
   ------------------------------------------------------------------------------*}

  TBCSPwbD = class(TDataModule)
    /// ADO Data Connection
    ADOConnection1: TADOConnection;
    /// ADO Data Table 1
    ADOTable1: TADOTable;
    /// ADO Data Table 2
    ADOTable2: TADOTable;
    /// Database Connections
    Database1: TDatabase;
    /// Data Source 1
    DataSource1: TDataSource;
    /// Data Source 2
    DataSource2: TDataSource;
    /// Rave Table Connection for Master Table
    RvTableConnection1: TRvTableConnection;
    /// Rave Table Connectin for Details Table
    RvTableConnection2: TRvTableConnection;
    /// Master Table For Report
    Table1: TTable;
    /// Detail Table for Report
    Table2: TTable;
  private
    {Private declarations}
    /// Connection Sttring
    FConStr: String;
    /// Table Name
    FTabName: string;
  public
    {Public declarations}
    property RConnStr: string read FConStr write FConStr;
    property RTabName: string read FTabName write FTabName;
    procedure OpenRDS;
    procedure CloseRDS;
  end;

var
  /// This is the primary pointer for access to the data module.
  BCSPwbD: TBCSPwbD;

implementation

{$R *.dfm}
{*------------------------------------------------------------------------------
 This procedure opens the database and associared tables.

 ------------------------------------------------------------------------------*}

procedure TBCSPwbD.OpenRDS;
begin
  ADOConnection1.ConnectionString := RConnStr;
  ADOConnection1.Connected := True;
  ADOTable1.TableName := RTabName;
  ADOTable1.Connection := ADOConnection1;
  ADOTable1.Open;
  ADOTable2.Open;
end;

{*------------------------------------------------------------------------------
 This procedure closes the database  and associared tables.

 ------------------------------------------------------------------------------*}

procedure TBCSPwbD.CloseRDS;
begin
  ADOTable2.Close;
  ADOTable1.Close;
end;

end.
