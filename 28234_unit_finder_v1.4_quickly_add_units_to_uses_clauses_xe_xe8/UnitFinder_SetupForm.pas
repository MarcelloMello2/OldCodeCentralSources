unit UnitFinder_SetupForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TSetupForm = class(TForm)
    InterfaceHotKey: THotKey;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ImplementationHotKey: THotKey;
    OpenHotKey: THotKey;
    BestHotKey: THotKey;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label6: TLabel;
    Image4: TImage;
    Image5: TImage;
    Label7: TLabel;
    Label8: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure InitializeData;
    procedure LoadSettings;
    procedure SaveSettings;
  end;

function StartSetupForm:boolean;

implementation

uses
  UnitFinder_Global;

{$R *.dfm}

function StartSetupForm:boolean;
var
  frm:TSetupForm;
begin
  frm := TSetupForm.Create(nil);
  try
    frm.InitializeData;
    result := frm.ShowModal = mrOk;
  finally
    frm.Free;
  end;
end;

{ TSetupForm }

procedure TSetupForm.Button1Click(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOk;
end;

procedure TSetupForm.Button3Click(Sender: TObject);
begin
  OpenHotKey.HotKey := GetDefaultOpenShortcut;
  BestHotKey.HotKey := GetDefaultBestShortcut;
  InterfaceHotKey.HotKey := GetDefaultInterfaceShortcut;
  ImplementationHotKey.HotKey := GetDefaultImplementationShortcut;
end;

procedure TSetupForm.InitializeData;
begin
  LoadSettings;
end;

procedure TSetupForm.LoadSettings;
begin
  OpenHotKey.HotKey := OpenShortcut;
  BestHotKey.HotKey := BestShortcut;
  InterfaceHotKey.HotKey := InterfaceShortcut;
  ImplementationHotKey.HotKey := ImplementationShortcut;
end;

procedure TSetupForm.SaveSettings;
begin
  OpenShortcut := OpenHotKey.HotKey;
  BestShortcut := BestHotKey.HotKey;
  InterfaceShortcut := InterfaceHotKey.HotKey;
  ImplementationShortcut := ImplementationHotKey.HotKey;
end;

end.
