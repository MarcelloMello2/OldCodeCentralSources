unit Font_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormSettings;

type
  TBaseForm = class(TForm)
  private
    FTestCaption : TCaption;
    FATestFont   : TFont;
    FSaveFormSettings : TSaveFormSettings;
    procedure SetSaveFormSettings(SaveFormSettings : TSaveFormSettings);
    procedure SetFont(Value: TFont);
  public
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    destructor  Destroy; override;
  published
    property TestCaption : TCaption read FTestCaption write FTestCaption;
    property ATestFont   : TFont    read FATestFont   write SetFont;
    property SaveFormSettings : TSaveFormSettings read FSaveFormSettings write SetSaveFormSettings;
  end;

var
  BaseForm: TBaseForm;

implementation

{$R *.DFM}

{ TBaseForm }

constructor TBaseForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FATestFont:= TFont.Create;
  FSaveFormSettings := TSaveFormSettings.Create(Self);
end;

destructor TBaseForm.Destroy;
begin
  FSaveFormSettings.Free;
  FATestFont.Free;
  inherited;
end;

procedure TBaseForm.SetFont(Value: TFont);
begin
  FATestFont.Assign(Value);
end;

procedure TBaseForm.SetSaveFormSettings(SaveFormSettings: TSaveFormSettings);
begin
  FSaveFormSettings.Assign(SaveFormSettings);
end;

end.
