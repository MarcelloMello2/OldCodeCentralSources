{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uBaseOTAWizard;

interface

uses
  Borland.Studio.ToolsAPI, uBaseOTA;

type
  TBaseOTAWizard = class(TBaseOTA, IOTAWizard)
  private
    FName,
    FIDString: string;
  public
    constructor Create(const aName, aIDString: string);
    destructor Destroy; override;
    // IOTAWizard
    procedure Destroyed; virtual;
    procedure Execute; virtual;
    function get_IDString: string; virtual;
    function get_Name: string; virtual;
    property IDString: string read get_IDString;
    property Name: string read get_Name;
  end;

implementation

{ TBaseOTAWizard }

constructor TBaseOTAWizard.Create(const aName, aIDString: string);
begin
  inherited Create;
  FName := aName;
  FIDString := aIDString;
end;

destructor TBaseOTAWizard.Destroy;
begin
  inherited;
end;

function TBaseOTAWizard.get_Name: string;
begin
  Result := FName;
end;

function TBaseOTAWizard.get_IDString: string;
begin
  Result := FIDString;
end;

procedure TBaseOTAWizard.Destroyed;
begin
end;

procedure TBaseOTAWizard.Execute;
begin
end;

end.
