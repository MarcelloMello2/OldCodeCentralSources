{*********************************************************************}
{*                                                                   *}
{*  BDS XML Documentation Viewer - version 3.1                       *}
{*  Paweł Głowacki [Borland Services]                                *}
{*                                                                   *}
{*********************************************************************}

unit uBaseOTAMenuWizard;

interface

uses
  Borland.Studio.ToolsAPI, uBaseOTAWizard;

type
  TBaseOTAMenuWizard = class(TBaseOTAWizard, IOTAMenuWizard)
  private
    FMenuText: string;
    FChecked,
    FEnabled: boolean;
  public
     constructor Create(const aName, aIDString: string; aMenuText: string; aChecked, aEnabled: boolean);
     destructor Destroy; override;
     // IOTAMenuWizard
     function get_Checked: boolean; virtual;
     function get_Enabled: boolean; virtual;
     function get_MenuText: string; virtual;
     property Checked: boolean read get_Checked;
     property Enabled: boolean read get_Enabled;
     property MenuText: string read get_MenuText;
  end;

implementation

{ TBaseOTAMenuWizard }

constructor TBaseOTAMenuWizard.Create(const aName, aIDString: string;
  aMenuText: string; aChecked, aEnabled: boolean);
begin
  inherited Create(aName, aIDString);
  FMenuText := aMenuText;
  FChecked := aChecked;
  FEnabled := aEnabled;
end;

destructor TBaseOTAMenuWizard.Destroy;
begin
  inherited;
end;

function TBaseOTAMenuWizard.get_Checked: boolean;
begin
  Result := FChecked;
end;

function TBaseOTAMenuWizard.get_Enabled: boolean;
begin
  Result := FEnabled;
end;

function TBaseOTAMenuWizard.get_MenuText: string;
begin
  Result := FMenuText;
end;

end.
