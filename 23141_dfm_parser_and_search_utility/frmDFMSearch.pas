{-----------------------------------------------------------------------------
The Initial Developer of the Original Code is Charles McAllister charles@avimark.net
Portions created by Charles McAllister are Copyright (C) 2005 Charles McAllister
All Rights Reserved.

Disclaimer:
This code and information are provided "As is" without warranty of any kind, either expressed or
implied, including but not limited to the implied warranties of merchantability and/or fitness
for a particular purpose, or non-infringement.

Contributor(s):

Description:
  A sample search utility that uses the TmcsDFMPersistentListParser class.

Known Issues:
  Binary properties are not currently searched.  See TODO's in umcsParser.pas
  If you find a bug, you may submit your fix to charles@avimark.net

}

unit frmDFMSearch;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  RTLConsts,
  TypInfo,
  DB,
  Grids,
  DBGrids,
  JvComponent,
  JvBaseDlg,
  JvFindFiles,
  JvSearchFiles,
  JvMemoryDataset,
  umcsDFMParser;

type
  TDFMEditComparison = (dcompNone, dcompContains, dcompNotContains, dcompEquals, dcompNotEquals);

const
  DFMEditComparisonStrings: array[TDFMEditComparison] of string = (
    '(no comparison)',
    'Contains',
    'Not Contains',
    'Equals',
    'Not Equals');

type
  TDFMSearchForm = class(TForm)
    SearchButton: TButton;
    SearchFiles: TJvSearchFiles;
    CancelButton: TButton;
    PathEdit: TEdit;
    Label1: TLabel;
    SubDirsCheckBox: TCheckBox;
    ClassNameEdit: TEdit;
    Label2: TLabel;
    ValueEdit: TEdit;
    Label3: TLabel;
    FileLabel: TLabel;
    Label5: TLabel;
    PropNameEdit: TEdit;
    ComponentNameEdit: TEdit;
    Label6: TLabel;
    ComponentNameCompareBox: TComboBox;
    ClassNameCompareBox: TComboBox;
    PropNameCompareBox: TComboBox;
    ValueCompareBox: TComboBox;
    ResultDataSet: TJvMemoryData;
    ResultDataSetComponentName: TStringField;
    ResultDataSetComponentClassName: TStringField;
    ResultDataSetPropName: TStringField;
    ResultDataSetValue: TStringField;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    ResultDataSetFile: TStringField;
    procedure SearchButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSearching: Boolean;
    FCurrentFile: string;
    FUserAbort: Boolean;
    procedure SetUserAbort(const Value: Boolean);
    property UserAbort: Boolean read FUserAbort write SetUserAbort;
  protected
    procedure DoBeginSearch;
    procedure DoEndSearch;
    procedure DoSearchProperty(AFile, AComponentName, AComponentClassName: string;
      AFlags: TFilerFlags; APropName: string; AValueType: TValueType; AValue: string);
  public
    property CurrentFile: string read FCurrentFile;
  end;

var
  DFMSearchForm: TDFMSearchForm;

implementation

{$R *.dfm}

{ TForm1 }

procedure TDFMSearchForm.SearchButtonClick(Sender: TObject);

  procedure Recurse(AParent: TmcsDFMPersistent);
  var
    I: Integer;
    ADFMProperty: TmcsDFMProperty;
  begin
    for I := 0 to AParent.PropertyList.Count - 1 do
    begin
      ADFMProperty := AParent.PropertyList[I];
      DoSearchProperty(FCurrentFile, AParent.PersistentName,
        AParent.PersistentClassName, AParent.Flags, ADFMProperty.PropName,
        ADFMProperty.ValueType, ADFMProperty.AsString);
    end;
    for I := 0 to AParent.ChildPersistentList.Count - 1 do
      Recurse(AParent.ChildPersistentList[I]);
  end;

var
  Index: Integer;
  AIgnore: Boolean;
  ADFMParser: TmcsDFMParser;
begin
  FSearching := True;
  UserAbort := False;
  AIgnore := False;
  if SubDirsCheckBox.Checked then
    SearchFiles.DirOption := doIncludeSubDirs
  else
    SearchFiles.DirOption := doExcludeSubDirs;
  SearchFiles.RootDirectory := PathEdit.Text;
  DoBeginSearch;
  if SearchFiles.Search then
    for Index := 0 to SearchFiles.Files.Count - 1 do
    begin
      try
        FCurrentFile := SearchFiles.Files[Index];
        FileLabel.Caption := FCurrentFile;
        Application.ProcessMessages;
        ADFMParser := TmcsDFMParser.Create;
        try
          ADFMParser.BuildPersistentList(FCurrentFile);
          Recurse(ADFMParser.RootPersistent);
        finally
          ADFMParser.Free;
        end;
      except
        on E: Exception do
          if not AIgnore then
            if MessageDlg(Format('Error in File, %s'#13#10'%s', [SearchFiles.Files[Index],
              E.Message]), mtError, [mbOk, mbIgnore], 0) = mrIgnore then
              AIgnore := True;
      end;
      if UserAbort then
      begin
        ShowMessage('Cancelled');
        Break;
      end;
    end;
  FSearching := False;
  UserAbort := False;
  FileLabel.Caption := '';
  ShowMessage('Finished.');
end;

procedure TDFMSearchForm.CancelButtonClick(Sender: TObject);
begin
  if FSearching then
    UserAbort := True
  else
    Close;
end;

procedure TDFMSearchForm.SetUserAbort(const Value: Boolean);
begin
  if FUserAbort <> Value then
  begin
    FUserAbort := Value;
    CancelButton.Enabled := not FUserAbort;
  end;
end;

procedure TDFMSearchForm.DoSearchProperty(AFile, AComponentName, AComponentClassName: string;
  AFlags: TFilerFlags; APropName: string; AValueType: TValueType; AValue: string);

  function MatchEdit(AEdit: TEdit; ACompareBox: TComboBox; AValue: string): Boolean;
  begin
    Result := False;
    case TDFMEditComparison(ACompareBox.ItemIndex) of
      dcompNone: Result := True;
      dcompContains: Result := Pos(UpperCase(AEdit.Text), UpperCase(AValue)) > 0;
      dcompNotContains: Result := Pos(UpperCase(AEdit.Text), UpperCase(AValue)) = 0;
      dcompEquals: Result := UpperCase(AEdit.Text) = UpperCase(AValue);
      dcompNotEquals: Result := UpperCase(AEdit.Text) <> UpperCase(AValue);
      else
        Assert(False);
    end;
  end;

begin
  if MatchEdit(ComponentNameEdit, ComponentNameCompareBox, AComponentName) and
    MatchEdit(ClassNameEdit, ClassNameCompareBox, AComponentClassName) and
    MatchEdit(PropNameEdit, PropNameCompareBox, APropName) and
    MatchEdit(ValueEdit, ValueCompareBox, AValue) then
  begin
    ResultDataSet.Insert;
    ResultDataSetFile.Value := AFile;
    ResultDataSetComponentName.Value := AComponentName;
    ResultDataSetComponentClassName.Value := AComponentClassName;
    ResultDataSetPropName.Value := APropName;
    ResultDataSetValue.Value := AValue;
    ResultDataSet.Post;
  end;
end;

procedure TDFMSearchForm.DoBeginSearch;
begin
  ResultDataSet.EmptyTable;
end;

procedure TDFMSearchForm.DoEndSearch;
begin

end;

procedure TDFMSearchForm.FormCreate(Sender: TObject);
var
  ADFMComparison: TDFMEditComparison;
begin
  for ADFMComparison := Low(TDFMEditComparison) to High(TDFMEditComparison) do
  begin
    ComponentNameCompareBox.Items.Add(DFMEditComparisonStrings[ADFMComparison]);
    ComponentNameCompareBox.ItemIndex := 0;
    ClassNameCompareBox.Items.Add(DFMEditComparisonStrings[ADFMComparison]);
    ClassNameCompareBox.ItemIndex := 0;
    PropNameCompareBox.Items.Add(DFMEditComparisonStrings[ADFMComparison]);
    PropNameCompareBox.ItemIndex := 0;
    ValueCompareBox.Items.Add(DFMEditComparisonStrings[ADFMComparison]);
    ValueCompareBox.ItemIndex := 0;
  end;
end;

end.
