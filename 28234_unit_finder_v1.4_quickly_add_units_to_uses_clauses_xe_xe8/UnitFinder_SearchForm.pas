unit UnitFinder_SearchForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList, Buttons, UnitFinder_UnitSupport, StrUtils, UnitFinder_ThreadSupport,
  UnitFinder_Global, Menus, UnitFinder_DelphiParser;

type
  TSearchMode = (smOpen,smBest,smForceInterface,smForceImplementation);
  TSearchResult = (srOpen,srInterface,srImplementation,srCancel);


  TSearchThread = class(TffThread)
  private
    FSearching:boolean;
  public
    SearchText:string;
    SearchTexts:TStringList;
    SearchResults:TStringList;
    OnSearchFinished:TNotifyEvent;
    constructor Create; override;
    destructor Destroy; override;
    procedure WrappedExecute; override;
    procedure Search;
    procedure StopSearching;
    procedure DoSearchFinished;
  end;


  TSearchForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    CancelButton: TButton;
    SearchEdit: TEdit;
    Image1: TImage;
    ClearSearchButton: TSpeedButton;
    ImplementationButton: TBitBtn;
    InterfaceButton: TBitBtn;
    OpenButton: TBitBtn;
    ListView: TListView;
    ImplementationBevel: TShape;
    InterfaceBevel: TShape;
    OpenBevel: TShape;
    MagnifyingGlassPopupMenu: TPopupMenu;
    DumpMenuItem: TMenuItem;
    ParseErrorPanel: TPanel;
    ParseErrorLabel: TLabel;
    procedure FormResize(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListViewAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure OpenButtonClick(Sender: TObject);
    procedure InterfaceButtonClick(Sender: TObject);
    procedure ImplementationButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure ClearSearchButtonClick(Sender: TObject);
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ButtonEnter(Sender: TObject);
    procedure ButtonExit(Sender: TObject);
    procedure DumpMenuItemClick(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewClick(Sender: TObject);
    procedure ListViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FNeedToLoad: boolean;
    FLastFileName:string;
    FLoading:boolean;
    FSearchMode:TSearchMode;
    FDefaultSection:TSearchResult;
    FDefaultSearchResult:TSearchResult;
    FDelphiUnit:TDelphiUnit;
    FHasSourceCode: boolean;

    FUnitUpdator:TUnitUpdator;
    FSearchThread:TSearchThread;
    FSearchResults:TStringList;

    procedure UpdateGUI;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure InitializeData;

    procedure UpdateListView;
    procedure UnitUpdatorFinished(Sender: TObject);

    procedure BeginSearch;
    procedure SearchFinished(Sender: TObject);

    function GoToFileName(FileName:string):boolean;

    procedure SearchEditChange(Sender: TObject);

    procedure InitialSearch;

  public
    SearchResult:TSearchResult;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  cFormCaption = 'Unit Finder';

var
  LastSearch:string = '';
  LastFileName:string = '';

function StartSearchForm(SearchMode:TSearchMode; DefaultSection:TSearchResult; HasSourceCode: boolean; DelphiUnit:TDelphiUnit; var FileName:string; var UnitName:string):TSearchResult;

implementation

uses
  DateUtils, CommCtrl, Registry, UnitFinder_SystemSupport;

{$R *.dfm}

procedure SendKeyTohWnd(Handle: HWND; VirtualKey: Word);
begin
  SendMessage(Handle, WM_KEYDOWN, VirtualKey, 0);
  SendMessage(Handle, WM_KEYUP, VirtualKey, 0);
end;


function StartSearchForm(SearchMode:TSearchMode; DefaultSection:TSearchResult; HasSourceCode:boolean; DelphiUnit:TDelphiUnit; var FileName:string; var UnitName:string):TSearchResult;
var
  frm:TSearchForm;
  Ext:string;
begin
  frm := TSearchForm.Create(nil);
  try
    frm.FNeedToLoad := True;
    frm.FSearchMode := SearchMode;
    frm.FDefaultSection := DefaultSection;
    frm.FDelphiUnit := DelphiUnit;
    frm.FHasSourceCode := HasSourceCode;

    frm.InitializeData;
    frm.ShowModal;
    result := frm.SearchResult;
    if result <> srCancel then begin
      LastSearch := frm.SearchEdit.Text;
      // if selected an item
      if (frm.ListView.Selected <> nil) and
         (frm.ListView.Selected.SubItems.Count <> 0) then begin
        // If trying to open a dcu file
        if (result = srOpen) and
           (frm.ListView.Selected.Data = Pointer(0)) then
          result := srCancel
        else begin
          if (frm.ListView.Selected.Data = Pointer(1)) then
            Ext := '.pas'
          else
            Ext := '.dcu';
          FileName := frm.ListView.Selected.SubItems[0] + frm.ListView.Selected.Caption + Ext;
          UnitName := frm.ListView.Selected.Caption;
          LastFileName := FileName;
        end;
      end;
    end;
  finally
    frm.Free;
  end;
end;

procedure TSearchForm.BeginSearch;
begin
  FSearchThread.StopSearching;
  FSearchThread.SearchText := SearchEdit.Text;
  FSearchThread.Search;
end;

procedure TSearchForm.CancelButtonClick(Sender: TObject);
begin
  SearchResult := srCancel;
  ModalResult := mrCancel;
end;

procedure TSearchForm.ClearSearchButtonClick(Sender: TObject);
begin
  SearchEdit.Text := '';
  SearchEdit.SetFocus;
  ListView.Clear;
  UpdateGUI;
end;

constructor TSearchForm.Create(AOwner: TComponent);
begin
  inherited;
  FUnitUpdator := TUnitUpdator.Create;
  FUnitUpdator.OnTerminate := UnitUpdatorFinished;

  FSearchThread := TSearchThread.Create;
  FSearchThread.OnSearchFinished := SearchFinished;
  FSearchThread.Resume;

  Caption := cFormCaption;
  SearchEdit.Text := '';

  UpdateListView;
end;

destructor TSearchForm.Destroy;
begin
  FSearchThread.Free;
  FUnitUpdator.Free;
  FSearchResults.Free;
  inherited;
end;

procedure TSearchForm.DumpMenuItemClick(Sender: TObject);
var
  FileName: string;
begin
  if PromptForFileName(FileName,'','txt','Save Path List as...','',True) then
    SavePathListToFile(FileName);
end;

procedure TSearchForm.FormActivate(Sender: TObject);
begin
  if FNeedToLoad then begin
    FLoading := True;
    Caption := cFormCaption + ' (Loading...)';
    FUnitUpdator.Resume;
  end else
    InitialSearch;
end;

procedure TSearchForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
  FUnitUpdator.OnTerminate := nil;
  FUnitUpdator.Terminate;

  FSearchThread.OnSearchFinished := nil;
  FSearchThread.Terminate;
end;

procedure TSearchForm.FormResize(Sender: TObject);
begin
  UpdateListView;
end;

function TSearchForm.GoToFileName(FileName: string):boolean;
var
  i:integer;
  Path:string;
  Name:string;
begin
  result := False;
  Path := Lowercase(ExtractFilePath(FileName));
  Name := Lowercase(ChangeFileExt(ExtractFileName(FileName),''));
  for i := 0 to ListView.Items.Count-1 do begin
    if Name = Lowercase(ListView.Items[i].Caption) then
      if ListView.Items[i].SubItems.Count > 0 then
        if (Path='') or (Path = Lowercase(ListView.Items[i].SubItems[0])) then begin
          ListView.ItemFocused := ListView.Items[i];
          ListView.Selected := ListView.Items[i];
          ListView.Items[i].MakeVisible(False);
          result := True;
          exit;
        end;
  end;
end;

procedure TSearchForm.Image1Click(Sender: TObject);
begin
  SearchEdit.SetFocus;
end;

procedure TSearchForm.ImplementationButtonClick(Sender: TObject);
begin
  if ListView.Selected = nil then
    exit;
  SearchResult := srImplementation;
  ModalResult := mrOk;
end;

procedure TSearchForm.InitializeData;
begin
  ParseErrorLabel.Caption := FDelphiUnit.ErrorMessage;
  SearchResult := srCancel;
  LoadSettings;
  UpdateListView;
  UpdateGUI;
end;

procedure TSearchForm.InitialSearch;
begin
  if SearchEdit.Text <> '' then
    BeginSearch;
end;

procedure TSearchForm.InterfaceButtonClick(Sender: TObject);
begin
  if ListView.Selected = nil then
    exit;
  SearchResult := srInterface;
  ModalResult := mrOk;
end;

procedure TSearchForm.ListViewAdvancedCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  // Force the selected item to remain blue when the list view is not focused
  // (For this to work, make sure HideSelection is True)
  if Stage = cdPrePaint then begin
    if Item.Data = Pointer(0) then
      Sender.Canvas.Font.Color := clGrayText;
    if (Sender.Focused) or (cdsHot in State) then
      exit;
    if Item.Selected then begin
      Sender.Canvas.Brush.Color := clHighlight;
      Sender.Canvas.Font.Color := clHighlightText;
    end;
  end;
end;

procedure TSearchForm.ListViewAdvancedCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
  SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  // Gray out the Path Column
  if Stage = cdPrePaint then begin
    Sender.Canvas.Brush.Color := TListView(Sender).Color;
    Sender.Canvas.Font.Color := clGrayText;
  end;
end;

procedure TSearchForm.ListViewClick(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TSearchForm.ListViewData(Sender: TObject; Item: TListItem);
var
  p:integer;
  s:string;
begin
  s := fSearchResults[Item.Index];
  p := pos(';',s);
  Item.Caption := Copy(s,1,p-1);
  Item.Data := Pointer(StrToInt(Copy(s,p+1,1)));
  Item.Subitems.Add(Copy(s,p+3,Maxint));
end;

procedure TSearchForm.ListViewDblClick(Sender: TObject);
begin
  if (ListView.Selected = nil) then
    exit;
  if (FDefaultSearchResult = srOpen) and
     (ListView.Selected.Data = Pointer(0)) then
    exit;
  SearchResult := FDefaultSearchResult;
  ModalResult := mrOk;
end;

procedure TSearchForm.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateGUI;
end;

procedure TSearchForm.ListViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateGUI;
end;

procedure TSearchForm.ListViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateGUI;
end;

procedure TSearchForm.SaveSettings;
var
  Registry:TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if not Registry.OpenKey(cRegistryPath, True) then
      exit;
    Registry.WriteInteger('FormWidth',Width);
    Registry.WriteInteger('FormHeight',Height);
    Registry.WriteInteger('NameColumnWidth',ListView.Columns[0].Width);
  finally
    Registry.Free;
  end;
end;

procedure TSearchForm.LoadSettings;
var
  Registry:TRegistry;
begin
  SearchEdit.Text := LastSearch;
  SearchEdit.OnChange := SearchEditChange;
  FLastFileName := LastFileName;

  Registry := TRegistry.Create;
  try
    Registry.OpenKey(cRegistryPath, False);
    if Registry.ValueExists('FormWidth') then
      Width := Registry.ReadInteger('FormWidth');
    if Registry.ValueExists('FormHeight') then
      Height := Registry.ReadInteger('FormHeight');
    if Registry.ValueExists('NameColumnWidth') then
      ListView.Columns[0].Width := Registry.ReadInteger('NameColumnWidth')
    else
      ListView.Columns[0].Width := ListView.Width div 2;
  finally
    Registry.Free;
  end;

end;

procedure TSearchForm.OpenButtonClick(Sender: TObject);
begin
  if ListView.Selected = nil then
    exit;
  SearchResult := srOpen;
  ModalResult := mrOk;
end;

procedure TSearchForm.ButtonEnter(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TSearchForm.ButtonExit(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TSearchForm.Panel1Click(Sender: TObject);
begin
  SearchEdit.SetFocus;
end;


procedure TSearchForm.SearchEditChange(Sender: TObject);
begin
  if FLoading then
    exit;

  if SearchEdit.Text = '' then begin
    ListView.Items.Clear;
    UpdateGUI;
    exit;
  end;

  UpdateGUI;
  BeginSearch;
end;

procedure TSearchForm.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DOWN) or
     (Key = VK_UP) or
     (Key = VK_PRIOR) or
     (Key = VK_NEXT) or
     (
       (ssCtrl in Shift) and
       (
         (Key = VK_HOME) or
         (Key = VK_END)
        )
     ) then begin
    SendKeyTohWnd(ListView.Handle,Key);
    Key := 0;
  end;
end;

procedure TSearchForm.UnitUpdatorFinished(Sender: TObject);
begin
  Caption := cFormCaption;
  FLoading := False;
  InitialSearch;
end;

procedure TSearchForm.UpdateGUI;
var
  ButtonsFocused:boolean;
begin

  // If Unit Finder wants to stick the unit into the implementation automatically,
  // first check to see if the unit is already in the interface. If it is, don't
  // allow it to be moved by default.  Moving the unit down into the implementation
  // probably isn't what the user wants.  (It will probably break their code.)


  if FSearchMode = smBest then begin
    FDefaultSearchResult := FDefaultSection;
    if (FDefaultSearchResult = srImplementation) and (FHasSourceCode) then
      if ListView.Selected <> nil then
        if FDelphiUnit.InterfaceUsesClause.Units.IndexOf(ListView.Selected.Caption) <> -1 then
          FDefaultSearchResult := srInterface;
  end else if FSearchMode = smOpen then
    FDefaultSearchResult := srOpen
  else if FSearchMode = smForceInterface then
    FDefaultSearchResult := srInterface
  else
    FDefaultSearchResult := srImplementation;

  if (FDefaultSearchResult = srImplementation) and
     (FHasSourceCode) and
     (FDelphiUnit.FileDeclaration.IsProgram) then
    FDefaultSearchResult := srInterface;

  // Enabled
  OpenButton.Enabled := (ListView.Selected <> nil) and (ListView.Selected.Data <> Pointer(0));
  InterfaceButton.Enabled := FHasSourceCode and (ListView.Selected <> nil);
  ImplementationButton.Enabled := FHasSourceCode and (not FDelphiUnit.FileDeclaration.IsProgram) and (ListView.Selected <> nil);

  // Default
  OpenButton.Default := (FDefaultSearchResult = srOpen) or (not FHasSourceCode);
  InterfaceButton.Default := (FDefaultSearchResult = srInterface) and FHasSourceCode;
  ImplementationButton.Default := (FDefaultSearchResult = srImplementation) and FHasSourceCode;

  // Visible
  ButtonsFocused := (OpenButton.Focused) or
                    (InterfaceButton.Focused) or
                    (ImplementationButton.Focused) or
                    (CancelButton.Focused);

  ClearSearchButton.Visible := SearchEdit.Text <> '';
  OpenBevel.Visible := OpenButton.Default and (not ButtonsFocused);
  InterfaceBevel.Visible := InterfaceButton.Default and (not ButtonsFocused);
  ImplementationBevel.Visible := ImplementationButton.Default and (not ButtonsFocused);
  ParseErrorPanel.Visible := FDelphiUnit.ErrorMessage <> '';

end;

procedure TSearchForm.UpdateListView;
begin
  if ListView.Items.Count >= ListView.VisibleRowCount then begin
    ListView.Columns[1].Width:= ListView.Width-ListView.Columns[0].Width-GetSystemMetrics(SM_CXVSCROLL)-4;
  end else
    ListView.Columns[1].Width:= ListView.Width-ListView.Columns[0].Width-4;
end;


{ TSearchThread }

constructor TSearchThread.Create;
begin
  inherited;
  SearchTexts := TStringList.Create;
  SearchTexts.Delimiter := ' ';
  SearchTexts.QuoteChar := #0;
  SearchTexts.StrictDelimiter := True;
end;

destructor TSearchThread.Destroy;
begin
  SearchTexts.Free;
  FreeAndNil(SearchResults);
  inherited;
end;

procedure TSearchThread.DoSearchFinished;
begin
  if Assigned(OnSearchFinished) then
    OnSearchFinished(Self);
end;

procedure TSearchThread.Search;
begin
  FSearching := True;
  AbortSleep;
end;

procedure TSearchThread.StopSearching;
begin
  FSearching := False;
  WaitForSleep(True);
end;

procedure TSearchThread.WrappedExecute;
var
  UnitList: TUnitList;
  i,j,k,idx:integer;
  found:boolean;
  UnitName:string;
  SourceCode:Boolean;
  ResultItem:string;
begin
  while not Terminated do begin
    ThreadSleep(INFINITE);

    if Assigned(SearchResults) then
      SearchResults.Clear
    else
      SearchResults := TStringList.Create;

    if Trim(SearchText) = '' then
      continue;

    SearchTexts.DelimitedText := Lowercase(SearchText);

    for i := 0 to PathList.Count-1 do begin
      UnitList := TUnitList(PathList.Objects[i]);
      for j := 0 to UnitList.Count - 1 do begin
        if Terminated then
          exit;
        if not FSearching then
          break;
        found := true;
        for k := 0 to SearchTexts.Count - 1 do
          if (SearchTexts[k] <> '') and (Pos(SearchTexts[k],Lowercase(UnitList[j])) = 0) then begin
            found := false;
            break;
          end;
        if found then begin
          UnitName := UnitList[j];
          SourceCode := Integer(UnitList.Objects[j]) <> 0;
          ResultItem := UnitName+';'+IntToStr(Ord(SourceCode))+';'+PathList[i];
          if SourceCode then begin
            idx := IndexOfStartsWith(UnitName+';0',SearchResults);
            if idx <> -1 then
              SearchResults[idx] := ResultItem
            else
              SearchResults.Add(ResultItem);
          end else begin
            if IndexOfStartsWith(UnitName+';',SearchResults) = -1 then
              SearchResults.Add(ResultItem);
          end;
        end;
      end;
      if not FSearching then
        break;
    end;

    if not FSearching then
      continue;

    SearchResults.Sort;

    if not FSearching then
      continue;

    FSearching := False;
    Synchronize(DoSearchFinished);

  end;
end;

procedure TSearchForm.SearchFinished(Sender:TObject);
begin
  ListView.Items.BeginUpdate;
  try
    FreeAndNil(FSearchResults);
    FSearchResults := FSearchThread.SearchResults;
    FSearchThread.SearchResults := nil;
    ListView.Items.Count := FSearchResults.Count;
    UpdateListView;
    // Need to initialize Listview to receive key commands.
    SendKeyTohWnd(ListView.Handle,VK_DOWN);
  finally
    ListView.Items.EndUpdate;
  end;

  if ListView.Items.Count > 0 then begin
    if FLastFileName <> '' then begin
      GotoFileName(FLastFileName);
      FLastFileName := '';
    end else begin
      if (Length('SearchText') < 2) or (not GotoFileName(FSearchThread.SearchText+'.pas')) then begin
        ListView.ItemFocused := ListView.Items[0];
        ListView.Selected := ListView.Items[0];
      end;
    end;
  end;

  UpdateGUI;
end;


end.
