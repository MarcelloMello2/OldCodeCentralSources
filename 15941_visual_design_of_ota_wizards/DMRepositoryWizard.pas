unit DMRepositoryWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, DMWizard;

type
  TRepositoryWizardModule = class(TWizardModule, IOTARepositoryWizard)
  private
    FAuthor: string;
    FComment: string;
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
    function GetGlyph: HICON;

    procedure SetName(const Value: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Author: string read GetAuthor write SetAuthor;
    property Comment: string read GetComment write FComment;
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

function TRepositoryWizardModule.GetGlyph: HICON;

begin
  if Assigned(FIcon) then
    Result := CopyIcon(FIcon.Handle)
  else
    Result := 0;
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
