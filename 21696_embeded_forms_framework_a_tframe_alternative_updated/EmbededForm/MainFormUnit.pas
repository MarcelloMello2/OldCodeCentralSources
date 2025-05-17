unit MainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, EmbededFormUnit, ExtCtrls, EmbededPanelUnit;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    EmbededFormPanel1: TEmbededFormPanel;
    EmbededFormPanel2: TEmbededFormPanel;
    EmbededInstanceFormPanel1: TEmbededInstanceFormPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

end.

