unit CommentatorBindings;

interface

uses ToolsApi, ToolWin, Classes;

type

  TCommentExpert = class(TNotifierObject, IOTANotifier, IOTAKeyboardBinding)
  private
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    procedure CommentCurrentStatement(SourceEditor: IOTASourceEditor; const position : integer); overload;
    procedure CommentCurrentStatement(const Context: IOTAKeyContext; KeyCode: TShortCut;
                                      var BindingResult: TKeyBindingResult); overload;
    procedure CommentCurrentStatement; overload;
    function FetchCurrentPosition(SourceEditor: IOTASourceEditor) : integer;
    procedure MenuClick(Sender: TObject);
  end;

  procedure Register;

implementation

uses Windows, SysUtils, Menus, QUtil;

var
  miEntry : TMenuItem;

{ TCommentExpert }

procedure TCommentExpert.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
var
  MainMenu : TMainMenu;
begin
  MainMenu := (BorlandIDEServices as INTAServices).GetMainMenu;
  miEntry := TMenuItem.Create(MainMenu);
  with miEntry do
  begin
    Caption := 'Co&mment function at cursor';
    OnClick := MenuClick;
    Name := 'BDEditComment';
  end;
  MainMenu.Items[1].Add(miEntry);
  BindingServices.AddKeyBinding([ShortCut(Ord('M'), [ssShift, ssCtrl])], CommentCurrentStatement, nil,
                                kfImplicitShift or kfImplicitModifier or kfImplicitKeypad, '', 'BDEditComment');
end;

procedure TCommentExpert.CommentCurrentStatement(SourceEditor: IOTASourceEditor; const position: integer);

  function WriteList(slist, dlist : TStrings) : string;
  var
    s, spc    : string;
    i, j : integer;
  begin
    result := ''; spc := '';
    for i := 0 to slist.Count - 1 do
    begin
      s := spc + slist[i] + ': ';
      j := Integer(slist.Objects[i]);
      if j > -1  then
        s := s + dlist[j] + #13#10
      else
        s := s + '<untyped>'#13#10;
      result := result + s;
      spc := Format('%14s', [' ']);
    end;
  end;

var
  Reader   : IOTAEditReader;
  Writer   : IOTAEditWriter;
  c_buffer : array[0..8191] of char;
  s        : string;
  i        : DWord;
begin
  Reader := SourceEditor.CreateReader;
  i := Reader.GetText(position, c_buffer, sizeof(c_buffer)-1);
  c_buffer[i] := #0;
  with TSimpleParser.Create do
  try
    Buffer := c_buffer;
    Parse;
    s := '{'#13#10'Name        : ' + FunctionName + #13#10 +
         'Description : '#13#10;
    if InParams.Count > 0 then
      s := s + 'Input       : ';
    s := s + WriteList(InParams, DataTypes);
    if OutParams.Count > 0 then
      s := s + 'Output      : ';
    s := s + WriteList(OutParams, DataTypes);
    if InOutParams.Count > 0 then
      s := s + 'In-/Output  : ';
    s := s + WriteList(InOutParams, DataTypes);
    i := sizeof(c_buffer);
    GetUserName(c_buffer, i);
    s := s + 'Author      : ' + c_buffer + #13#10 +
             'Date        : ' + DateToStr(Date) + #13#10 +
             'Revision history:'#13#10 +
             '! Date       ! Sign   ! Description                                     !'#13#10 +
             '! ' + Format('%10s', [DateToStr(Date)]) + ' ! ' + StrPas(c_buffer) + #13#10'}'#13#10;
  finally
    free;
  end;
  Reader := nil;
  Writer := SourceEditor.CreateWriter;
  Writer.CopyTo(Position);
  Writer.Insert(pchar(s));
end;

procedure TCommentExpert.CommentCurrentStatement(const Context: IOTAKeyContext; KeyCode: TShortCut;
                                                 var BindingResult: TKeyBindingResult);
begin
  CommentCurrentStatement;
  BindingResult := krHandled;
end;

procedure TCommentExpert.CommentCurrentStatement;
var
  i : integer;
  SourceEditor: IOTASourceEditor;
begin
  i := 0;
  with (BorlandIDEServices as IOTAModuleServices).CurrentModule do
  begin
    while (i < GetModuleFileCount) do
      if GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, SourceEditor) = S_OK then
        break
      else
        inc(i);
    if i = GetModuleFileCount then
      raise Exception.Create('unable to find IOTASourceEditor interface');
  end;
  i := FetchCurrentPosition(SourceEditor);
  CommentCurrentStatement(SourceEditor, i);
end;

function TCommentExpert.FetchCurrentPosition(SourceEditor: IOTASourceEditor): integer;
var
  edtView : IOTAEditView;
  Reader  : IOTAEditReader;
  buffer  : array[0..8191] of char;
  bufptr  : pchar;
  i, line,
  bufsize,
  curpos,
  bufpos  : integer;
begin
  edtView := (SourceEditor).GetEditView(0);
  Reader := SourceEditor.CreateReader;
  line := edtView.CursorPos.Line; i := 1; bufpos := 0; curpos := 0;
  while (i < line) do
  begin
    bufptr := buffer; curpos := bufpos;
    bufsize := Reader.GetText(bufpos, buffer, sizeof(buffer)-1);
    buffer[bufsize] := #0;
    while (i < line) do
    begin
      bufptr := StrScan(bufptr, #13);
      if bufptr <> nil then inc(bufptr, 2)
        else break;
      inc(i);
    end;
    bufpos := bufpos + bufsize;
    curpos := curpos + (bufptr - buffer);
  end;
  result := curpos;
end;

function TCommentExpert.GetBindingType: TBindingType;
begin
  result := btPartial;
end;

function TCommentExpert.GetDisplayName: string;
begin
  result := 'Standard Commentator';
end;

function TCommentExpert.GetName: string;
begin
  result := 'runesbike.commentator';
end;

procedure TCommentExpert.MenuClick(Sender: TObject);
begin
  CommentCurrentStatement;
end;

procedure Register;
begin
  (BorlandIDEServices as IOTAKeyBoardServices).AddKeyboardBinding(TCommentExpert.Create);
end;

initialization
finalization
  miEntry.Free;

end.
