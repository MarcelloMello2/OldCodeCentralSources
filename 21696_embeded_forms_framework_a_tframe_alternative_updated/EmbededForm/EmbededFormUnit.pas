unit EmbededFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EmbededPanelUnit, DeepEmbededFormUnit;

type
  TFirstEmbededForm = class(TForm)
    FormLink: TFormLink;
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    EmbededFormPanel1: TEmbededFormPanel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var FirstEmbededForm: TFirstEmbededForm;

implementation

{$R *.DFM}

procedure TFirstEmbededForm.FormShow(Sender: TObject);
begin
 if Tag = 0 then
 begin
//  ShowMessage ('Embeded Form OnShow');

  Tag := 1;
 end;
end;

procedure TFirstEmbededForm.FormCreate(Sender: TObject);
begin
// ShowMessage ('Embeded Form OnCreate');
end;

end.


 

 
 
