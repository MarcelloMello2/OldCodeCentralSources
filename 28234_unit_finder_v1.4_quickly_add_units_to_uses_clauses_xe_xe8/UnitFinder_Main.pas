unit UnitFinder_Main;
interface

uses
  Windows, Classes, SysUtils, Menus, ToolsAPI, UnitFinder_ShortcutHook, UnitFinder_UnitSupport,
  UnitFinder_OpenToolsSupport, UnitFinder_DelphiParser, Controls;

type
  TUnitFinder = class(TObject)
  private
    FEditReaderPos:integer;
    FEditReader:IOTAEditReader;
    procedure OnShortcut(Shortcut:TShortcut; var Allow:boolean);
    procedure OnPathEvent(Sender:TObject; Path:string);
    procedure OnFileEvent(Sender:TObject; FileName:string);

    procedure OnGetSourceEvent(Sender: TObject; Buffer:PAnsiChar; MaxLength:integer; var Length:integer);
    procedure OnInsertSourceEvent(Sender:TObject; Position:integer; Source:AnsiString);
    procedure OnDeleteSourceEvent(Sender:TObject; Position:integer; Count:integer);
  protected
    procedure FindUnit(Shortcut:TShortcut);
  public
    constructor Create;
  end;

procedure Register;

implementation

uses
  UnitFinder_Global, UnitFinder_SearchForm, Clipbrd, Dialogs;

var
  UnitFinder:TUnitFinder = nil;


procedure Register;
begin
  UnitFinder_Global.LoadSettings;
  DelphiRootPath := ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(Paramstr(0))));
  UnitFinder := TUnitFinder.Create;
end;


procedure TUnitFinder.OnDeleteSourceEvent(Sender: TObject; Position: integer; Count: integer);
begin
  DeleteTextAtPos(Position-1,Count);
end;

procedure TUnitFinder.OnFileEvent(Sender: TObject; FileName: string);
begin
  ExtraFiles.Add(FileName);
end;

procedure TUnitFinder.OnGetSourceEvent(Sender: TObject; Buffer:PAnsiChar; MaxLength:integer; var Length:integer);
begin
  if not Assigned(FEditReader) then
    raise Exception.Create('TUnitFinder.OnGetSourceEvent: FEditReader not assigned.');

  Length := FEditReader.GetText(FEditReaderPos, Buffer, MaxLength);
  Inc(FEditReaderPos, Length);
end;


procedure TUnitFinder.OnInsertSourceEvent(Sender: TObject; Position: integer; Source: AnsiString);
begin
  InsertTextAtPos(Source,Position-1);
end;

procedure TUnitFinder.OnPathEvent(Sender: TObject; Path:string);
begin
  // Lowercase the drive letter
  if Copy(Path,2,1) = ':' then
    case Path[1] of
      'A'..'Z':
        Path[1] := Char(Word(Path[1]) or $0020);
    end;
  PathList.Add(Path);
end;

procedure TUnitFinder.FindUnit(Shortcut:TShortcut);
var
  FileName:string;
  UnitName:string;
  SourcePaths: TSourcePaths;
  SearchMode: TSearchMode;
  SearchResult: TSearchResult;
  ProjectFiles: TProjectFiles;
  DelphiUnit:TDelphiUnit;
  SourceEditor:IOTASourceEditor;
  BufferOptions: IOTABufferOptions;
  EditOptions: IOTAEditOptions;
  DefaultSection: TSearchResult;
  HasSourceCode: boolean;
begin

  // Refresh Paths
  SourcePaths := TSourcePaths.Create;
  try
    PathList.ResetTouched;
    SourcePaths.OnPath := OnPathEvent;
    SourcePaths.GetPaths;
    PathList.DeleteUntouched;
  finally
    SourcePaths.Free;
  end;

  // Add extra files from the current project
  ProjectFiles := TProjectFiles.Create;
  try
    ExtraFiles.Clear;
    ProjectFiles.OnFile := OnFileEvent;
    ProjectFiles.GetFiles;
  finally
    ProjectFiles.Free;
  end;

  // Initialize Search Mode with a default.
  if Shortcut = BestShortcut then
    SearchMode := smBest
  else if Shortcut = InterfaceShortcut then
    SearchMode := smForceInterface
  else if Shortcut = ImplementationShortcut then
    SearchMode := smForceImplementation
  else
    SearchMode := smOpen;

  DefaultSection := srInterface;

  // Load Uses Clauses
  DelphiUnit := TDelphiUnit.Create;
  try
    DelphiUnit.OnGetSource := OnGetSourceEvent;
    DelphiUnit.OnInsertSource := OnInsertSourceEvent;
    DelphiUnit.OnDeleteSource := OnDeleteSourceEvent;

    SourceEditor := GetCurrentSourceEditor;
    HasSourceCode := Assigned(SourceEditor);

    if HasSourceCode then begin

      // Set indent and word wrap width for writing the uses clause later
      BufferOptions := GetTopMostBufferOptions;
      if Assigned(BufferOptions) then
        DelphiSourceWidth := BufferOptions.RightMargin;

      EditOptions := GetTopMostEditOptions;
      if Assigned(EditOptions) then
        DelphiSourceIndent := EditOptions.BlockIndent;

      FEditReaderPos := 0;
      FEditReader := SourceEditor.CreateReader;
      try
        DelphiUnit.Load;

        if DelphiUnit.IsLoaded then begin
          // Update SearchMode to a better choice since the unit could be parsed.
          if not DelphiUnit.FileDeclaration.IsProgram then
            if SearchMode = smBest then
              if EditorCursorPosition < DelphiUnit.ImplementationStart then
                DefaultSection := srInterface
              else
                DefaultSection := srImplementation;
        end;
      finally
        FEditReader := nil;
      end;
    end else
      SearchMode := smOpen;

    SearchResult := StartSearchForm(SearchMode,DefaultSection,HasSourceCode,DelphiUnit,FileName,UnitName);
    if SearchResult = srOpen then
      OpenUnit(FileName)

    else if (SearchResult = srInterface) or
            (SearchResult = srImplementation) then begin

      if not DelphiUnit.IsLoaded then begin
        if MessageDlg(DelphiUnit.ErrorMessage+#13#10+#13#10+'Copy "'+UnitName+'" to the clipboard instead?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          Clipboard.AsText := UnitName;
      end else begin
        if SearchResult = srInterface then
          DelphiUnit.AddToInterfaceUses(UnitName)
        else if SearchResult = srImplementation then
          DelphiUnit.AddToImplementationUses(UnitName);
      end;
    end;


  finally
    DelphiUnit.Free;
  end;
end;


constructor TUnitFinder.Create;
begin
  SetOnShortcutEvent(OnShortcut);
end;

procedure TUnitFinder.OnShortcut(Shortcut: TShortcut; var Allow: boolean);
begin
  if (UnitFinderRunning = False) and
     (
       (Shortcut = OpenShortcut) or
       (Shortcut = BestShortcut) or
       (Shortcut = InterfaceShortcut) or
       (Shortcut = ImplementationShortcut)
     ) then begin
    UnitFinderRunning := True;
    try
      FindUnit(Shortcut);
    finally
      UnitFinderRunning := False;
    end;
    Allow := False;
  end;
end;

initialization

finalization
  UnitFinder.Free;

end.
