unit UnitFinder_OpenToolsSupport;
interface
uses Windows, Sysutils, Classes, ToolsAPI, StrUtils, {} Dialogs, IOUtils;

type

  TOnFileEvent = procedure(Sender:TObject; FileName:string) of object;

  (* TProjectFiles

    Enumerates the "*.pas" project files found in the "contains" section of the
    IDE's Project Manager.  Call GetFiles to start getting files.  Each file is
    enumerated in the OnFile event.
  *)
  TProjectFiles = class(TObject)
  private
    FOnFile: TOnFileEvent;
  public
    procedure GetFiles;
    property OnFile:TOnFileEvent read FOnFile write FOnFile;
  end;


  TOnPathEvent = procedure(Sender:TObject; Path:string) of object;

  (* TSourcePaths

    Enumerates the list of paths that Delphi's IDE and compiler uses for finding
    unit files.  Call GetPaths to start getting paths.  Each path is enumerated
    in the OnPath event.
  *)
  TSourcePaths = class(TObject)
  private
    FOnPath: TOnPathEvent;
    FEnvironmentVariables:TStringList;
    procedure ParsePaths(Paths:string);
    function ExpandMacros(Path:string):string;
    function ExpandMacro(Macro:string):string;
    procedure LoadEnvironmentVariables;
  public
    constructor Create;
    procedure GetPaths;
    property OnPath:TOnPathEvent read FOnPath write FOnPath;
  end;

function AppendCurrentSourceToStream(Stream:TStream):boolean;
procedure OpenUnit(FileName:string);
function EditorCursorPosition:integer;
procedure InsertTextAtPos(Text: AnsiString; Position: Integer);
procedure DeleteTextAtPos(Position, Count: Integer);
function GetCurrentSourceEditor: IOTASourceEditor;
function GetTopMostBufferOptions: IOTABufferOptions;
function GetTopMostEditOptions: IOTAEditOptions;
function GetOptionNames:string;
function GetCurrentPlatform:string;


implementation

uses
  Registry;



procedure SaveStringToFile(const s,FileName:string; Append:Boolean = False);
var
  StringStream:TStringStream;
  FileStream:TFileStream;
  ReallyAppend:boolean;
begin
  ReallyAppend := (Append) and (FileExists(FileName));
  StringStream := TStringStream.Create(s);
  try
    if ReallyAppend then
      FileStream := TFileStream.Create(FileName,fmOpenReadWrite)
    else
      FileStream := TFileStream.Create(FileName,fmCreate);
    try
      StringStream.Seek(0,soFromBeginning);
      if ReallyAppend then
        FileStream.Seek(0,soFromEnd);
      FileStream.CopyFrom(StringStream,0);
    finally
      FileStream.Free;
    end;
  finally
    StringStream.Free;
  end;
end;


function IDEServices:IOTAServices;
begin
  result := BorlandIDEServices as IOTAServices;
  if not Assigned(result) then
    raise Exception.Create('IOTAServices not implemented');
end;

function EditorServices: IOTAEditorServices;
begin
  result := (BorlandIDEServices as IOTAEditorServices);
  if not Assigned(result) then
    raise Exception.Create('IOTAEditorServices not implemented');
end;

function GetOptionNames:string;
var
  Names: TOTAOptionNameArray;
  i:integer;
begin
  result := '';
  Names := IDEServices.GetEnvironmentOptions.GetOptionNames;
  for i := Low(Names) to High(Names) do
    result := result + Names[i].Name + #13#10;
end;

function GetCurrentPlatform:string;
var
  Project:IOTAProject;
begin
  result := '';
  Project := GetActiveProject;
  if not Assigned(Project) then
    exit;

  {$IF CompilerVersion >= 23} // if >= XE2
    result := Project.GetPlatform;
  {$IFEND}

end;


function GetEnvironmentString(Name:string):string;
begin
  result := IDEServices.GetEnvironmentOptions.Values[Name];
end;


function GetTopMostBufferOptions: IOTABufferOptions;
var
  EditView: IOTAEditView;
  Buffer: IOTAEditBuffer;
begin
  result := nil;
  EditView := EditorServices.TopView;
  if not Assigned(EditView) then
    exit;
  Buffer := EditView.Buffer;
  if not Assigned(Buffer) then
    exit;
  result := Buffer.BufferOptions;
end;

function GetTopMostEditOptions: IOTAEditOptions;
var
  EditView: IOTAEditView;
  Buffer: IOTAEditBuffer;
begin
  result := nil;
  EditView := EditorServices.TopView;
  if not Assigned(EditView) then
    exit;
  Buffer := EditView.Buffer;
  if not Assigned(Buffer) then
    exit;
  result := Buffer.EditOptions;
end;



function GetProjectString(Name:string):string;
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
begin
  result := '';
  Project := GetActiveProject;
  if not Assigned(Project) then
    exit;
  ProjectOptions := Project.GetProjectOptions;
  if not Assigned(ProjectOptions) then
    exit;
  result := ProjectOptions.Values[Name];
end;

function GetProjectPath:string;
var
  Project: IOTAProject;
begin
  result := '';
  Project := GetActiveProject;
  if not Assigned(Project) then
    exit;
  result := ExtractFileDir(Project.FileName);
end;


procedure SaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream);
const
  cBufferSize = 1024 * 16;
var
  EditReaderPos: Integer;
  ReadDataSize: Integer;
  Buffer: array[0..cBufferSize] of AnsiChar;
begin
  EditReaderPos := 0;
  ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, cBufferSize);
  Inc(EditReaderPos, ReadDataSize);
  while ReadDataSize = cBufferSize do begin
    Stream.Write(Buffer, ReadDataSize);
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer, cBufferSize);
    Inc(EditReaderPos, ReadDataSize);
  end;
  Stream.Write(Buffer, ReadDataSize);
end;


function GetCurrentModule: IOTAModule;
var
  ModuleServices: IOTAModuleServices;
begin
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if not Assigned(ModuleServices) then
    raise Exception.Create('IOTAModuleServices not implemented');
  result := ModuleServices.CurrentModule;
end;


function GetCurrentSourceEditor: IOTASourceEditor;
var
  Module: IOTAModule;
  i: integer;
begin
  result := nil;
  Module := GetCurrentModule;
  if not Assigned(Module) then
    exit;
  i := Module.GetModuleFileCount - 1;
  // Iterate over modules untill we find a source editor or list exhausted
  while not ((i < 0) or
             SysUtils.Supports(Module.GetModuleFileEditor(i),IOTASourceEditor, result)) do
    Dec(i);
end;


function AppendCurrentSourceToStream(Stream:TStream):boolean;
var
  SourceEditor:IOTASourceEditor;
begin
  result := False;

  SourceEditor := GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    exit;

  SaveReaderToStream(SourceEditor.CreateReader, Stream);
  result := True;
end;


{ TSourcePaths }

constructor TSourcePaths.Create;
begin
  FEnvironmentVariables := TStringList.Create;
  LoadEnvironmentVariables;
end;

function TSourcePaths.ExpandMacro(Macro: string): string;
begin
  result := FEnvironmentVariables.Values[Macro];
end;

function TSourcePaths.ExpandMacros(Path: string): string;
var
  i,p:integer;
  Macro:string;
begin
  result := '';
  i := 1;
  while i < Length(Path) do begin
    p := PosEx('$',Path,i);
    if p = 0 then begin
      result := result + Copy(Path,i,Maxint);
      exit;
    end;
    result := result + Copy(Path,i,p-i);
    i := p + 2;
    p := PosEx(')',Path,i);
    Macro := Copy(Path,i,p-i);
    result := result + ExpandMacro(Macro);
    i := p + 1;
  end;
end;

procedure TSourcePaths.GetPaths;
var
  Paths:string;
  Reg:TRegistry;
  SubKeyNames:TStringList;
  LibraryKey:string;
  i: Integer;
  PlatformName:string;
begin
  if not Assigned(FOnPath) then
    exit;

  (* GetEnvironmentString no longer works properly in XE3 and beyond because
     there is no way to indicate to ITOA interfaces which platform is desired.
     The first one listed in the IDE is always used, which happens to be OSX
     on my version of Delphi.

  // Get Library Paths
  Paths := GetEnvironmentString('LibraryPath');
  ParsePaths(Paths);

  // Get Browsing Paths
  Paths := GetEnvironmentString('BrowsingPath');
  ParsePaths(Paths);     *)

  // Until IOTA is fixed, we will search the Delphi registry entries for paths.
  LibraryKey := IDEServices.GetBaseRegistryKey + '\Library';
  Reg := TRegistry.Create;
  SubKeyNames := TStringList.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKeyReadOnly(LibraryKey) then begin
      MessageDlg('Registry Key not found:'#13#13'HKEY_LOCAL_MACHINE'+LibraryKey,mtWarning,[mbOk],0);
      exit;
    end;

  {$IF CompilerVersion >= 23} // if >= XE2
    PlatformName := GetCurrentPlatform;
    if PlatformName = '' then begin
      Reg.GetKeyNames(SubKeyNames);
      for i := 0 to SubKeyNames.Count-1 do begin
        Reg.OpenKeyReadOnly(LibraryKey+'\'+SubKeyNames[i]);
        ParsePaths(Reg.GetDataAsString('Browsing Path'));
        ParsePaths(Reg.GetDataAsString('Search Path'));
      end;
    end else begin
      Reg.OpenKeyReadOnly(LibraryKey+'\'+PlatformName);
      ParsePaths(Reg.GetDataAsString('Browsing Path'));
      ParsePaths(Reg.GetDataAsString('Search Path'));
    end;
  {$ELSE}
    ParsePaths(Reg.GetDataAsString('Browsing Path'));
    ParsePaths(Reg.GetDataAsString('Search Path'));
  {$IFEND}

  finally
    Reg.Free;
    SubKeyNames.Free;
  end;

  // Get Project Library Paths
  Paths := GetProjectString('SrcDir');
  ParsePaths(Paths);

  // Active Project Folder
  Paths := GetProjectPath;
  ParsePaths(Paths);
end;

procedure TSourcePaths.LoadEnvironmentVariables;
var
  EnvStart: Pointer;
  EnvPos: PChar;
  PlatformName: string;
begin
  EnvStart := GetEnvironmentStrings;
  try
    EnvPos := EnvStart;
    while StrLen(EnvPos) > 0 do begin
      FEnvironmentVariables.Add(String(EnvPos));
      EnvPos := StrEnd(EnvPos) + 1;
    end;
    // The on XE3 and up, the IDE doesn't set the Platform environment variable until it starts a compile, so we'll add it manually.
    PlatformName := GetCurrentPlatform;
    if PlatformName <> '' then
      FEnvironmentVariables.Values['Platform'] := PlatformName;
  finally
    FreeEnvironmentStrings(EnvStart);
  end;
end;


procedure TSourcePaths.ParsePaths(Paths: string);
var
  i,p:integer;
  Path:string;
begin
  i := 1;
  while i < Length(Paths) do begin
    p := PosEx(';',Paths,i);
    if p = 0 then
      p := Length(Paths)+1;
    Path := Copy(Paths,i,p-i);
    FOnPath(Self, IncludeTrailingPathDelimiter( ExpandMacros(Path) ) );
    i := p + 1;
  end;
end;

procedure OpenUnit(FileName:string);
var
  ActionServices: IOTAActionServices;
begin
  ActionServices := BorlandIDEServices as IOTAActionServices;
  if not Assigned(ActionServices) then
    raise Exception.Create('IOTAActionServices not implemented');
  if not ActionServices.OpenFile(FileName) then
    raise Exception.Create('Unit not found: ' + FileName);
end;


function EditorCursorPosition:integer;
var
  EditView:IOTAEditView;
  CursorPos: TOTAEditPos;
  CharPos: TOTACharPos;
begin
  result := -1;
  EditView := EditorServices.TopView;
  if not Assigned(EditView) then
    exit;
  CursorPos := EditView.CursorPos;
  EditView.ConvertPos(True, CursorPos, CharPos);
  result := EditView.CharPosToPos(CharPos);
end;

{ TProjectFiles }

procedure TProjectFiles.GetFiles;
var
  i:integer;
  Project:IOTAProject;
  FileName:string;
begin
  if not Assigned(FOnFile) then
    exit;
  Project := GetActiveProject;
  if not Assigned(Project) then
    exit;
  for i := 0 to Project.GetModuleCount - 1 do begin
    FileName := Project.GetModule(i).FileName;
    if (Trim(FileName) <> '') and
       (LowerCase(ExtractFileExt(FileName)) = '.pas') then
      FOnFile(Self,Project.GetModule(i).FileName);
  end;


end;

procedure InsertTextAtPos(Text: AnsiString; Position: Integer);
var
  SourceEditor: IOTASourceEditor;
  EditWriter: IOTAEditWriter;
begin
  if Text = '' then
    Exit;
  SourceEditor := GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    exit;
  EditWriter := SourceEditor.CreateUndoableWriter;
  if not Assigned(EditWriter) then
    exit;
  EditWriter.CopyTo(Position);
  EditWriter.Insert( PAnsiChar( UTF8String(Text) ) );
end;

procedure DeleteTextAtPos(Position, Count: Integer);
var
  SourceEditor: IOTASourceEditor;
  EditWriter: IOTAEditWriter;
begin
  if Count = 0 then
    exit;
  SourceEditor := GetCurrentSourceEditor;
  if not Assigned(SourceEditor) then
    exit;
  EditWriter := SourceEditor.CreateUndoableWriter;
  if not Assigned(EditWriter) then
    exit;
  EditWriter.CopyTo(Position);
  EditWriter.DeleteTo(Position + Count);
end;

end.
