unit DMRepositoryWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, DMWizard;

type
  TWizardDesignerType = (wdtAny, wdtVCL, wdtCLX);

  TRepositoryWizardModule = class(TWizardModule, IOTARepositoryWizard, IOTARepositoryWizard60)
  private
    FAuthor: string;
    FComment: string;
    FDesignerType: TWizardDesignerType;
    FIcon: TIcon;
    FPage: string;

    function IsIconStored: Boolean;
    procedure SetAuthor(const Value: string);
    procedure SetIcon(Value: TIcon);
    procedure SetPage(const Value: string);
  protected
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    function GetGlyph: Cardinal;

    { IOTARepositoryWizard60 }
    function GetDesigner: string;

    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Author: string read GetAuthor write SetAuthor;
    property Comment: string read GetComment write FComment;
    property DesignerType: TWizardDesignerType read FDesignerType write FDesignerType default wdtAny;
    property Icon: TIcon read FIcon write SetIcon stored IsIconStored;
    property Page: string read GetPage write SetPage;
  end;

implementation

uses
  WizardUtils;

{$R *.DFM}

resourcestring
  SEmptyAuthor = 'Author cannot be empty';
  SEmptyPage = 'Page cannot be empty';

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule private }

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.IsIconStored: Boolean;

begin
  Result := (FIcon.Handle <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRepositoryWizardModule.SetAuthor(const Value: string);

begin
  if FAuthor <> Value then
  begin
    if Value = '' then
      raise Exception.Create(SEmptyAuthor);
    if (IDString = '') or (IDString = FAuthor + '.' + Name) then
      IDString := Value + '.' + Name;
    FAuthor := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRepositoryWizardModule.SetIcon(Value: TIcon);

begin
  FIcon.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TRepositoryWizardModule.SetPage(const Value: string);

begin
  if FPage <> Value then
  begin
    if Value = '' then
      raise Exception.Create(SEmptyPage);
    FPage := Value;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule protected: IOTARepositoryWizard }

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.GetAuthor: string;

begin
  Result := FAuthor;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.GetComment: string;

begin
  Result := FComment;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.GetGlyph: Cardinal;

begin
  if Assigned(FIcon) then
    Result := CopyIcon(FIcon.Handle)
  else
    Result := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule protected: IOTARepositoryWizard60 }

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.GetDesigner: string;

const
  DesignerTypes: array[TWizardDesignerType] of string = (dAny, dVCL, dCLX);

begin
  Result := DesignerTypes[FDesignerType];
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule protected }

//----------------------------------------------------------------------------------------------------------------------

procedure TRepositoryWizardModule.SetName(const Value: TComponentName);

var
  OldName: string;

begin
  OldName := Name;
  inherited SetName(Value);
  if (IDString = '') or (IDString = FAuthor + '.' + OldName) then
    IDString := FAuthor + '.' + Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TRepositoryWizardModule.GetPage: string;

begin
  Result := FPage;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TRepositoryWizardModule public }

//----------------------------------------------------------------------------------------------------------------------

constructor TRepositoryWizardModule.Create(AOwner: TComponent);

begin
  FIcon := TIcon.Create;
  try
    FAuthor := GetDefaultAuthor;
    FPage := 'Wizards';
    FDesignerType := wdtAny;
  except
    FIcon.Free;
    raise;
  end;
  inherited Create(AOwner);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TRepositoryWizardModule.Destroy;

begin
  FIcon.Free;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
