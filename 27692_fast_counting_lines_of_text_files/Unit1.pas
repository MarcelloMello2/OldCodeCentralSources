unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  N  = 1024*1024;  // size of buffer (1 MB)
  NN = 8;          // number of files for testing

var
  Buf: array [0..N-1] of AnsiChar;

procedure TForm1.Button1Click(Sender: TObject);
var
  f: textfile;
  i,j,size: integer;
begin
  // Sizes from ~2 to ~588 MB
  size:= 100000;
  for i:= 1 to NN do
  begin
    assignfile(f,'file'+inttostr(i)+'.txt');
    rewrite(f);
    for j:= 1 to size do writeln(f,'abcdefghijklmnopqrstu');
    closefile(f);
    size:= size*2;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);

  function func1(filename: string): integer;
  var
    f: textfile;
    s: string;
  begin
    assignfile(f,filename);
    system.SetTextBuf(f,Buf);
    reset(f);
    result:= 0;
    while not eof(f) do
    begin
//      readln(f);   // faster in D5 & D7
      readln(f,s); // faster in D2007 & D2010
      inc(result);
    end;
    closefile(f);
  end;

  function func2(filename: string): integer;
  var
    SL: TStringList;
  begin
    SL:= TStringList.Create;
    SL.Capacity:= 100000*(1 shl (NN-1));
    SL.LoadFromFile(filename);
    result:= SL.Count;
    SL.Free;
  end;

  function func3(filename: string): integer;
  var
    FS: TFileStream;
    i,s1,s2,cutoff: Integer;
  begin
    Result:= 0;
    FS:= TFileStream.Create(FileName,fmOpenRead);
    s1:= 0;
    s2:= FS.Size;
    cutoff:= 0;
    while FS.Position<s2 do
    begin
      FS.Read(Buf[0],N);
      s1:= s1+N;
      if s1>s2 then cutoff:= s1-s2;
      for i:= 0 to N-1-cutoff do if Buf[i]=#13 then inc(result);
    end;
    FS.Free;
  end;

var
  name: string;
  i,j: integer;
  w: longword;
begin
  for i:= 1 to NN do
  begin
    name:= 'file'+inttostr(i)+'.txt';

    w:= gettickcount;
    j:= func1(name);
    memo1.Lines.Add(inttostr(gettickcount-w)+'  '+inttostr(j));

    w:= gettickcount;
    j:= func2(name);
    memo2.Lines.Add(inttostr(gettickcount-w)+'  '+inttostr(j));

    w:= gettickcount;
    j:= func3(name);
    memo3.Lines.Add(inttostr(gettickcount-w)+'  '+inttostr(j));
  end;
end;

end.
