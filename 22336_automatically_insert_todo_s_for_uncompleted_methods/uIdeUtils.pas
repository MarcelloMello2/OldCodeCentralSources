unit uIdeUtils;

interface

uses
  SysUtils, Classes, DesignIntf, DesignEditors, ToolsApi, ActiveX;

type
  EIdeUtilsError = class(Exception);

{!~GetComponentIntfEditor returns an interface to the source file editor of
the unit that belongs to the Form or Data Module where a component is declared.
If the function fails, an exception is raised. }
function GetComponentIntfEditor(Designer: IDesigner): IOTASourceEditor;

{!~GetComponentImplEditor returns an interface to the source file editor of
the unit that belongs to the Form or Data Module where a component is
implemented. If the function fails, an exception is raised. }
function GetComponentImplEditor(Designer: IDesigner): IOTASourceEditor;

{!~GetComponentFormEditor returns an interface to the form editor of the unit
that belongs to the Form or Data Module where a component resides.
If the function fails, an exception is raised. }
function GetComponentFormEditor(Designer: IDesigner): IOTAFormEditor;

{!~The CheckQueryInterface procedure performs a QueryInterface and returns
the resulting interface reference. If the interface is not supported, an
exception is raised }
procedure CheckQueryInterface(Intf: IUnknown; const Query: TGUID; out Obj);

{!~UnCommentSourceCode uncomments source code by blanking all comments from it.
The comment text is not removed, but replaced by space character in order to
keep all positions in the text unchanged. If ClearString = True, everything
inside quoted strings is banked too }
procedure UnCommentSourceCode(var SourceCode: string;
  const ClearStrings: Boolean);

{!~ReadSourceCode reads the source code from an edit buffer }
function ReadSourceCode(Buffer: IOTAEditBuffer;
  const MaxChars: Integer = MaxInt): string;

{!~IntersectSelections determines the intersection of two selectionlists }
function IntersectSelections(
  List1, List2: IDesignerSelections): IDesignerSelections;

{!~GetCurrentEditBuffer returns the current source code edit buffer, or nil,
if  no edit buffer active }
function GetCurrentEditBuffer: IOTAEditBuffer;

implementation

{ Generic error checker }
procedure Check(const OkCondition: Boolean; const Msg: string;
            const Args: array of const);
begin
  if not OkCondition then
    raise EIdeUtilsError.CreateFmt(Msg, Args);
end;

procedure CheckQueryInterface(Intf: IUnknown; const Query: TGUID; out Obj);
resourcestring
  SInterfaceNotSupported = 'Interface not supported by the IDE';
begin
  Check(Intf.QueryInterface(Query, Obj) = S_OK, SInterfaceNotSupported, [nil]);
end;

type

  {!~TFileRequest defines the type of file request }
  TFileRequest = (frIntf, frImpl, frForm);

{!~GetComponentFileEditor returns an interface to editor of
the unit that belongs to the Form or Data Module where a component resides.
If the function fails, an exception is raised. The interface returned is either
a IOTASourceEditor or a IOTAFormEditor }
function GetComponentFileEditor(Designer: IDesigner;
           const Request: TFileRequest): IOTAEditor;
resourcestring
  SNoSuchEditor = 'No such editor found';
var
  ImplFileName, IntfFileName, FormFileName: string;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I: Integer;
  Editor: IOTAEditor;
  FileNameUsed: string;
  Ok: Boolean;
begin

  { Get the filenames }
  Designer.ModuleFileNames(ImplFileName, IntfFileName, FormFileName);

  { Retrieve the necessary interfaces }
  CheckQueryInterface(BorlandIDEServices, IOTAModuleServices, ModuleServices);
  case Request of
    frIntf: FileNameUsed := IntfFilename;
    frImpl: FileNameUsed := ImplFileName;
    frForm: FileNameUsed := FormFileName;
  end;
  Module := ModuleServices.FindModule(FileNameUsed);

  { Traverse the module editors to see which one we should return }
  Result := nil;
  for I := 0 to Module.GetModuleFileCount - 1 do
  begin
    Editor := Module.ModuleFileEditors[I];
    case Request of
      frIntf,
      frImpl: Ok := Supports(Editor, IOTASourceEditor);
      frForm: Ok := Supports(Editor, IOTAFormEditor);
    end;
    if Ok then
    begin
      Result := Editor;
      Break;
    end;
  end;
  if not Assigned(Result) then
    raise EIdeUtilsError.Create(SNoSuchEditor);
end;

function GetComponentIntfEditor(Designer: IDesigner): IOTASourceEditor;
var
  Editor: IOTAEditor;
begin
  Editor := GetComponentFileEditor(Designer, frImpl);
  CheckQueryInterface(Editor, IOTASourceEditor, Result);
end;

function GetComponentImplEditor(Designer: IDesigner): IOTASourceEditor;
var
  Editor: IOTAEditor;
begin
  Editor := GetComponentFileEditor(Designer, frImpl);
  CheckQueryInterface(Editor, IOTASourceEditor, Result);
end;

function GetComponentFormEditor(Designer: IDesigner): IOTAFormEditor;
var
  Editor: IOTAEditor;
begin
  Editor := GetComponentFileEditor(Designer, frImpl);
  CheckQueryInterface(Editor, IOTAFormEditor, Result);
end;

procedure UnCommentSourceCode(var SourceCode: string;
  const ClearStrings: Boolean);
var
  I: Integer;
  Ch, Prev: Char;
  State: (sCode, sAccolade, sParentAsterisk, sDoubleSlash, sQuotedString);
begin
  Prev := #0;
  State := sCode;
  for I := 0 to Length(SourceCode) do
  begin
    Ch := SourceCode[I];
    case State of
      sCode:
        case Ch of
          '{': begin
                 State := sAccolade;
                 SourceCode[I] := #32;
               end;
          '''':begin
                 if ClearStrings then
                 begin
                   State := sQuotedString;
                   SourceCode[I] := #32;
                 end;
               end;
          '*': if Prev = '(' then
               begin
                 State := sParentAsterisk;
                 SourceCode[I] := #32;
                 SourceCode[I-1] := #32;
               end;
          '/': if Prev = '/' then
               begin
                 State := sDoubleSlash;
                 SourceCode[I] := #32;
                 SourceCode[I-1] := #32;
               end;
        end;
      sAccolade:
        begin
          if not (SourceCode[I] in [#13, #10]) then
            SourceCode[I] := #32;
          if Ch = '}' then
            State := sCode;
        end;

      sParentAsterisk:
        begin
          if not (SourceCode[I] in [#13, #10]) then
            SourceCode[I] := #32;
          if (Prev = '*') and (Ch = ')') then
            State := sCode;
        end;

      sDoubleSlash:
        begin
          if Ch = #13 then
            State := sCode
          else
            SourceCode[I] := #32;
        end;

      sQuotedString:
        begin
          SourceCode[I] := #32;
          if Ch = '''' then
            State := sCode;
        end;
    end;
    Prev := Ch;
  end; //for
end;

function ReadSourceCode(Buffer: IOTAEditBuffer;
  const MaxChars: Integer): string;
const
  ChunkSize = 8192;
var
  Reader: IOTAEditReader;
  Size: Integer;
  Position: Integer;
  CharsRead: Integer;
begin
  Reader := Buffer.CreateReader;
  Size := ChunkSize;
  SetLength(Result, Size);
  Position := 0;
  repeat
    CharsRead := Reader.GetText(Position, PChar(@Result[1]) + Position, ChunkSize);
    if CharsRead = ChunkSize then //There may be more
    begin
      Inc(Size, ChunkSize);
      SetLength(Result, Size);
    end;
    Inc(Position, CharsRead);
  until (CharsRead < ChunkSize) or (Position >= MaxChars);
  Inc(Position);
  if Position <> Size then
    SetLength(Result, Position);
end;

function IntersectSelections(
  List1, List2: IDesignerSelections): IDesignerSelections;
var
  I, J: Integer;
  Obj: TPersistent;
begin
  Result := CreateSelectionList;
  for I := 0 to List1.Count - 1 do
  begin
    Obj := List1.Items[I];
    for J := 0 to List2.Count - 1 do
      if Obj = List2.Items[J] then
      begin
        Result.Add(Obj);
        Break;
      end;
  end;
end;

function GetCurrentEditBuffer: IOTAEditBuffer;
var
  View: IOTAEditView;
  EditorServices: IOTAEditorServices;
begin
  if Assigned(BorlandIDEServices) then
    begin
      if Succeeded(BorlandIDEServices.QueryInterface(IOTAEditorServices,
           EditorServices))
      then
        begin
          View := EditorServices.TopView;
          if Assigned(View) then
            Result := View.Buffer
          else
            Result := nil;
        end;
    end
  else
    Result := nil;
end;

end.
