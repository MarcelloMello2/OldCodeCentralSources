unit FormNewWizardPackage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TNewWizardPackageForm = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    CheckBoxDataModule: TCheckBox;
    EditClassName: TEdit;
    LabelClassName: TLabel;
    RadioWizardType: TRadioGroup;

    procedure EditClassNameChange(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

{ TNewWizardPackageForm event handlers }

//----------------------------------------------------------------------------------------------------------------------

procedure TNewWizardPackageForm.EditClassNameChange(Sender: TObject);

begin
  with Sender as TEdit do
    ButtonOK.Enabled := (Text <> '') and (Text[1] = 'T');
end;

//----------------------------------------------------------------------------------------------------------------------

end.
