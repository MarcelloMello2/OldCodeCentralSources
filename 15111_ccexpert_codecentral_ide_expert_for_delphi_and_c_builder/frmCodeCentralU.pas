unit frmCodeCentralU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ImgList, ActnList;

type
  TfrmCodeCentral = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    edtKeywords: TEdit;
    edtSubmitter: TEdit;
    edtHighVersion: TEdit;
    edtLowVersion: TEdit;
    edtShowMe: TEdit;
    btnSearch: TBitBtn;
    ActionList1: TActionList;
    ImageList1: TImageList;
    actSearch: TAction;
    btnClose: TBitBtn;
    Label6: TLabel;
    cbProduct: TComboBox;
    actHome: TAction;
    btnWeb: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSearchExecute(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure actSearchUpdate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actHomeExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProduct: String;
    FCCServer : string;
    procedure SetProduct(const Value: string);
    procedure BrowseTo( URL : string );
    { Private declarations }
  public
    { Public declarations }
    property Product : String read FProduct write SetProduct;
  end;

var
  frmCodeCentral: TfrmCodeCentral;

implementation

uses IniFiles, HTTPApp, ShellApi, dmCodeCentralU;
{$R *.DFM}

procedure TfrmCodeCentral.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmCodeCentral.actSearchExecute(Sender: TObject);
var
  SHTML : String;
begin
  SHTML := FCCServer + '/results?cmbProduct=' +
    cbProduct.Text + '&cmbCategory=0&edKeywords=' +
    HTTPEncode(edtKeyWords.Text) + '&edSubmitter=' + HTTPEncode(edtSubmitter.Text) +
    '&edHighVersion=' + edtHighVersion.Text +
    '&edLowVersion=' + edtLowVersion.Text +
    '&cmbCopyright=0&edMaxHits=' + edtShowMe.Text;
  BrowseTo(SHTML);
end;

procedure TfrmCodeCentral.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmCodeCentral.actSearchUpdate(Sender: TObject);

var
  LowVer,
  HighVer,
  ShowMe : Extended;

  function GetNum( Control : TCustomEdit ) : extended;
  begin
    Result := -1;
    if Trim( Control.Text ) <> '' then
      try
        Result := StrToFloat( Control.Text );
      except
      end;
  end;

begin
  LowVer := GetNum( edtLowVersion );
  HighVer := GetNum( edtHighVersion );
  ShowMe := GetNum( edtShowMe );

  actSearch.Enabled := ( Trim( edtKeywords.Text + edtSubmitter.Text ) <> '' ) and
    ( LowVer >= 0 ) and ( HighVer >= 0 ) and ( ShowMe > 0 ) and ( HighVer >= LowVer );
end;

procedure TfrmCodeCentral.SetProduct(const Value: string);
begin
  FProduct := Value;
  cbProduct.ItemIndex := cbProduct.Items.IndexOf( Value );
end;

procedure TfrmCodeCentral.FormCreate(Sender: TObject);
var
  Ini : TIniFile;
begin
  Ini := TIniFile.Create('ccexpert.ini');
  try
    FCCServer := Ini.ReadString('CodeCentral', 'Server',
      'http://codecentral.borland.com/codecentral/ccweb.exe' );
  finally
    Ini.Free;
  end;
  Product := SAnyProduct;
end;

procedure TfrmCodeCentral.BrowseTo(URL: string);
begin
  ShellExecute(0, 'Open', PChar(URL), '', '', SW_SHOWDEFAULT	);
end;

procedure TfrmCodeCentral.actHomeExecute(Sender: TObject);
begin
  BrowseTo( FCCServer + '/home' );
end;

procedure TfrmCodeCentral.FormDestroy(Sender: TObject);
begin
  frmCodeCentral := nil;
end;

end.
