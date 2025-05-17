unit DeepEmbededFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EmbededPanelUnit;

type
  TDeepEmbededForm = class(TForm)
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioButton1: TRadioButton;
    FormLink: TFormLink;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var DeepEmbededForm: TDeepEmbededForm;

implementation

{$R *.DFM}

procedure TDeepEmbededForm.FormShow(Sender: TObject);
begin
 if Tag = 0 then
 begin
//  ShowMessage ('Deep Embedded Form OnShow');

  Tag := 1;
 end;
end;

procedure TDeepEmbededForm.FormCreate(Sender: TObject);
begin
// ShowMessage ('Deep Embeded Form OnCreate');
end;

end.

