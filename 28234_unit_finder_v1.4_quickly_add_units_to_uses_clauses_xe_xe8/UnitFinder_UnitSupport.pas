unit UnitFinder_UnitSupport;
interface

uses Sysutils, Classes, UnitFinder_ThreadSupport;

type
  TUnitList = class(TStringList)
  public
    IsStaticList: boolean;
    Touched: boolean;
    constructor Create; reintroduce;
    procedure AddUnit(UnitName:string; SourceCode:boolean);
  end;

  TPathList = class(TStringList)
  public
    procedure Add(Path:string); reintroduce;
    procedure AddFileName(FileName:string);
    procedure ResetTouched;
    procedure DeleteUntouched;
    constructor Create; reintroduce;
  end;

  TUnitUpdator = class(TffThread)
  public
    procedure AddUnitFiles(SourceCode:boolean);
    procedure WrappedExecute; override;
  end;

var
  PathList:TPathList = nil;
  DelphiRootPath:string = '';
  ExtraFiles:TStringList = nil;

procedure SavePathListToFile(FileName:string);

implementation

uses
  StrUtils;

procedure SavePathListToFile(FileName:string);
begin
  PathList.SaveToFile(FileName);
end;


{ TUnitList }

procedure TUnitList.AddUnit(UnitName: string; SourceCode: boolean);
var
  i:integer;
begin
  i := IndexOf(UnitName);
  if i = -1 then
    AddObject(UnitName, TObject(Ord(SourceCode)))
  else begin
    if SourceCode then
      Objects[i] := TObject(Ord(True));
  end;
end;

constructor TUnitList.Create;
begin
  inherited;

end;

{ TPathList }

procedure TPathList.Add(Path: string);
var
  i:integer;
  UnitList:TUnitList;
begin
  i := IndexOf(Path);
  if i = -1 then begin
    UnitList := TUnitList.Create;
    AddObject(Path,UnitList);
  end else begin
    UnitList := TUnitList(Objects[i]);
  end;
  UnitList.Touched := True;
end;

{AddFileName

  Only adds filenames to list if it wasn't populated already by the *.pas/*.dcu scan.
  (If the path is there, all units in the path will already be there.)
}
procedure TPathList.AddFileName(FileName: string);
var
  Path:string;
  i:integer;
  UnitList:TUnitList;
  SourceCode:boolean;
begin
  Path := ExtractFilePath(FileName);
  i := PathList.IndexOf(Path);
  if i = -1 then begin
    // Not found, so create it "touched"
    UnitList := TUnitList.Create;
    AddObject(Path,UnitList);
    UnitList.Touched := True;
  end else
    UnitList := TUnitList(Objects[i]);

  if UnitList.Touched then begin
    SourceCode := Lowercase(ExtractFileExt(FileName)) = '.pas';
    UnitList.AddUnit(ChangeFileExt(ExtractFileName(FileName),'') , SourceCode);
  end;
end;

constructor TPathList.Create;
begin
  inherited;
  Sorted := True;
  CaseSensitive := False;
  OwnsObjects := True;
end;


procedure TPathList.DeleteUntouched;
var
  i:integer;
begin
  i := 0;
  while i < Count do begin
    if TUnitList(Objects[i]).Touched = False then
      Delete(i)
    else
      Inc(i);
  end;
end;

procedure TPathList.ResetTouched;
var
  i:integer;
begin
  for i := 0 to Count - 1 do
    TUnitList(Objects[i]).Touched := False;
end;

{ TUnitUpdator }

procedure TUnitUpdator.AddUnitFiles(SourceCode: boolean);
var
  i:integer;
  Path:string;
  sr:TSearchRec;
  UnitList:TUnitList;
  WildCard:string;
begin
  if SourceCode then
    WildCard := '*.pas'
  else
    WildCard := '*.dcu';

  for i := 0 to PathList.Count-1 do begin
    if Terminated then
      exit;
    Path := PathList[i];
    UnitList := TUnitList(PathList.Objects[i]);
    if UnitList.IsStaticList then
      continue;
    if SourceCode then
      UnitList.Clear;
    if FindFirst(Path+WildCard, faAnyFile, sr) = 0 then begin
      repeat
        if Terminated then
          exit;
        if (sr.Name <> '.') and (sr.Name <> '..') then
          UnitList.AddUnit(ChangeFileExt(ExtractFileName(sr.Name),'') , SourceCode);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if (not SourceCode) and ( Copy(Lowercase(Path),1,Length(DelphiRootPath)) = DelphiRootPath ) then
      UnitList.IsStaticList := True;
  end;
end;

procedure TUnitUpdator.WrappedExecute;
var
  i:integer;
begin
  DelphiRootPath := Lowercase(DelphiRootPath);

  // Add pas files
  AddUnitFiles(True);
  if Terminated then
    exit;

  // Add dcu files
  AddUnitFiles(False);
  if Terminated then
    exit;

  // Add Extra Project Files
  PathList.ResetTouched;
  for i := 0 to ExtraFiles.Count - 1 do begin
    if Terminated then
      exit;
    PathList.AddFileName(ExtraFiles[i]);
  end;

end;

initialization
  PathList := TPathList.Create;
  ExtraFiles := TPathList.Create;
finalization
  PathList.Free;
  ExtraFiles.Free;
end.
