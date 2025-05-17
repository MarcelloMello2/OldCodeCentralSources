unit DMMenuWizard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, DMWizard;

type
  TWizardMenuTextEvent = procedure(Sender: TObject; var Text: string) of object;
  
  TMenuWizardModule = class(TWizardModule, IOTAMenuWizard)
  private
    FMenuText: string;

    FOnGetMenuText: TWizardMenuTextEvent;
  protected
    { IOTAMenuWizard }
    function GetMenuText: string;
  public
  published
    property MenuText: string read FMenuText write FMenuText;

    property OnGetMenuText: TWizardMenuTextEvent read FOnGetMenuText write FOnGetMenuText;
  end;

implementation

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

{ TMenuWizardModule protected: IOTAMenuWizard }

//----------------------------------------------------------------------------------------------------------------------

function TMenuWizardModule.GetMenuText: string;

begin
  Result := FMenuText;
  if Assigned(FOnGetMenuText) then
    FOnGetMenuText(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
