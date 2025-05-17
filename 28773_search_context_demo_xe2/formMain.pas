unit formMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Filter.Effects,
  FMX.Effects, FMX.Layouts, FMX.Objects, FMX.Edit, unitSearchMenuHelper, System.IOUtils,
  FMX.Ani, FMX.Menus;

type
  TfrmMain = class(TForm)
    Layout1: TLayout;
    Rectangle1: TRectangle;
    ReflectionEffect1: TReflectionEffect;
    OpenDialog1: TOpenDialog;
    VertScrollBox1: TVertScrollBox;
    Layout2: TLayout;
    MenuBar1: TMenuBar;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem1: TMenuItem;
    StyleBook1: TStyleBook;
    edtSearch: TClearingEdit;
    procedure LoadImages(Sender: TObject);
    procedure edtSearchKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure ManageClick(Sender: TObject);
    procedure LoadData(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    SearchBandManager : TSearchBandManager;
    function InitializeBandManager : TSearchBand;
    procedure ItemSelected(Sender : TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}
uses formData, dataData;

procedure TfrmMain.ManageClick(Sender: TObject);
begin
  if frmManageData = nil then
    frmManageData := TfrmManageData.Create(Self);
  frmManageData.Show;
end;

procedure TfrmMain.LoadData(Sender: TObject);
var
  CurrFile : string;
  FilterPredicate : TDirectory.TFilterPredicate;
  Dir: string;
  Band: TSearchBand;
  SearchItem: TSearchItem;
  Image: TImage;
begin
  InitializeBandManager;

  SearchBandManager.Clear;
  //SearchBandManager.
  //Band := TSearchBand.Create(Self,True,Self.Fill.Color,'Images Group '+IntToStr(SearchBandManager.Count+1),100,100);
  //SearchBandManager.Add(Band);

  // Load the image in. (to random sized bands)
  with dtmdlData do begin
    cdsIconData.First;
    while not cdsIcondata.Eof do begin

      Band := SearchBandManager.BandByName(Uppercase(cdsIconDataCategory.AsString));
      if Band = nil then begin
        Band := TSearchBand.Create(Self,True,Self.Fill.Color,UpperCase(cdsIconDataCategory.Text),100,100);
        SearchBandManager.Add(Band);
      end;
      Image := TImage.Create(Self);
      try
        Image.Bitmap.Assign(cdsIconDataIcon);
        SearchItem := TSearchItem.Create(Self,Self.Fill.Color,TAlphaColorRec.White,Image,cdsIconDataDescription.Text);
        SearchItem.SearchText := cdsIconDataSearchTerms.AsString;
        SearchItem.OnDblClick := ItemSelected;
        Band.Add(SearchItem);
      finally
        Image.Free;
      end;
      cdsIconData.Next;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadData(nil);
end;

function TfrmMain.InitializeBandManager : TSearchBand;
begin
  if SearchBandManager = nil then
  begin
    SearchBandManager := TSearchBandManager.Create(Self, True);
    SearchBandManager.Parent := Self.VertScrollBox1;
    SearchBandManager.Align := TAlignLayout.alClient;
    SearchBandManager.Visible := True;
    Result := nil;
  end
  else
    Result := SearchBandManager.Items[SearchBandManager.Count - 1];
end;

procedure TfrmMain.ItemSelected(Sender: TObject);
begin
  ShowMessage('Search Text: '+(Sender as TSearchItem).SearchText);
end;

procedure TfrmMain.LoadImages(Sender: TObject);
var
  CurrFile : string;
  FilterPredicate : TDirectory.TFilterPredicate;
  Dir: string;
  Band: TSearchBand;
  SearchItem: TSearchItem;
  Image: TImage;
begin
  Band := InitializeBandManager;

  // Load pictures as the data currently..
  if not OpenDialog1.Execute then
    Exit;

  // Find Each Image
  Dir := ExtractFilePath(OpenDialog1.FileName);

  FilterPredicate := function(const Path: string; const SearchRec: TSearchRec): Boolean
                     begin
                       Result := (TPath.MatchesPattern(SearchRec.Name, '*.*', False)); // and

                       if Result then // Check if it is a image file
                       begin
                         Result := Pos(LowerCase(ExtractFileExt(SearchRec.Name)),
                                   DefaultBitmapCodecClass.GetFileTypes) > 0;
                       end;
                     end;


  Band := TSearchBand.Create(Self,True,Self.Fill.Color,'Images Group '+IntToStr(SearchBandManager.Count+1),100,100);
  SearchBandManager.Add(Band);

  // Load the image in. (to random sized bands)
  for CurrFile in TDirectory.GetFiles(Dir, FilterPredicate) do begin
    Image := TImage.Create(Self);
    try
      Image.Bitmap.LoadFromFile(CurrFile);
      SearchItem := TSearchItem.Create(Self,Self.Fill.Color,TAlphaColorRec.White,Image,ExtractFileName(CurrFile));
      SearchItem.SearchText := ChangeFileExt(ExtractFileName(CurrFile),'');
      Band.Add(SearchItem);
    finally
      Image.Free;
    end;
  end;
end;

procedure TfrmMain.edtSearchKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Self.SearchBandManager = nil then
    Exit;

  Self.SearchBandManager.TextSearch(Self.edtSearch.Text);
end;

end.
