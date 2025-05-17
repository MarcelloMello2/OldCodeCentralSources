unit Main_Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, Buttons, Smart_Form;

type
  TMainForm = class(TSmartForm)
    MainMenu1: TMainMenu;
    file1: TMenuItem;
    new1: TMenuItem;
    edit1: TMenuItem;
    copy1: TMenuItem;
    help1: TMenuItem;
    about1: TMenuItem;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Splitter2: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses Settings_Form;

{$R *.DFM}

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  if SettingsForm.EditComponents then
  begin
    Memo1.Lines.Assign(Memo2.Lines);
    Memo2.Lines.LoadFromFile(SettingsForm.Storage.Path);
  end
end;

end.
