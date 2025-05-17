unit textfilewriter;

// For fast writing of text files in D2010+
// Output in ASCII, ANSI, UTF8 is supported

// Orders of magnitude faster than TStreamWriter when writing 
// many short strings in particular

interface

type
  // for writing ASCII / ANSI kind-of files
  // Use A methods whenever possible - fastest !
  TTextFileWriterANSI = class(TObject)
  private
    f: textfile;
    FCodepage: word;
    function Unicode2Ansi(AString: String): AnsiString;
    function UTF8Ansi(AString: UTF8String): AnsiString;
  public
    constructor Create(filename: string; Codepage: word);
    destructor Destroy; override;
    procedure writeA(s: ansistring);
    procedure writeU(s: string);
    procedure writeU8(s: UTF8string);
    procedure writelineA(s: ansistring);
    procedure writelineU(s: string);
    procedure writelineU8(s: UTF8string);
  end;

  // for writing XML kind-of files
  TTextFileWriterUTF8 = class(TObject)
  private
    f: textfile;
  public
    constructor Create(filename: string; BOM: boolean);
    destructor Destroy; override;
    procedure write(s: UTF8string);
    procedure writeline(s: UTF8string);
  end;

implementation

{ TTextFileWriterANSI }

constructor TTextFileWriterANSI.Create(filename: string; Codepage: word);
begin
  inherited Create;
  assignfile(f,filename);
  rewrite(f);
  Fcodepage:= codepage;
end;

function TTextFileWriterANSI.Unicode2Ansi(AString: String): AnsiString;
begin
  SetAnsiString(@Result,PWideChar(AString),Length(AString),FCodePage);
end;

function TTextFileWriterANSI.UTF8Ansi(AString: UTF8String): AnsiString;
var
  tmp: string;
begin
  tmp:= AString;
  result:= Unicode2Ansi(tmp);
end;

procedure TTextFileWriterANSI.writeA(s: ansistring);
begin
  write(f,s);
end;

procedure TTextFileWriterANSI.writeU(s: string);
begin
  writeA(Unicode2Ansi(s));
end;

procedure TTextFileWriterANSI.writeU8(s: UTF8string);
begin
  writeA(UTF8Ansi(s));
end;

procedure TTextFileWriterANSI.writelineA(s: ansistring);
begin
  writeln(f,s);
end;

procedure TTextFileWriterANSI.writelineU(s: string);
begin
  writelineA(Unicode2Ansi(s));
end;

procedure TTextFileWriterANSI.writelineU8(s: UTF8string);
begin
  writelineA(UTF8Ansi(s));
end;

destructor TTextFileWriterANSI.Destroy;
begin
  closefile(f);
  inherited;
end;

{ TTextFileWriterUTF8 }

constructor TTextFileWriterUTF8.Create(filename: string; BOM: boolean);
begin
  inherited Create;
  assignfile(f,filename);
  rewrite(f);
  if BOM then
  begin
    system.write(f,chr($EF));
    system.write(f,chr($BB));
    system.write(f,chr($BF));
  end;
end;

procedure TTextFileWriterUTF8.write(s: UTF8string);
var
  i: integer;
begin
  for i:= 1 to length(s) do system.write(f,s[i]);
end;

procedure TTextFileWriterUTF8.writeline(s: UTF8string);
begin
  write(s);
  writeln(f);
end;

destructor TTextFileWriterUTF8.Destroy;
begin
  closefile(f);
  inherited;
end;

end.
