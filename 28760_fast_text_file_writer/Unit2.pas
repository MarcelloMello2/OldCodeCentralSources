unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  textfilewriter;

procedure TForm2.Button1Click(Sender: TObject);
var
  TFW: TTextFileWriterANSI;
  SW: TStreamWriter;
  i: integer;
  w: longword;
  s: string;
begin
  s:= 'זרו';

  w:= gettickcount;
  TFW:= TTextFileWriterANSI.create('TextFileWriterANSI.txt',1252);
  for i:= 0 to 100000 do
  begin
    if RadioGroup1.ItemIndex=1 then
      TFW.writelineA('abcdefghijklmnopqrstuvwxyz'+s+'>') else
    begin
      TFW.WriteA('ab');
      TFW.WriteA('cdefghi');
      TFW.WriteA('jkl');
      TFW.WriteA('mnopq');
      TFW.WriteA('rstuvwxyz');
      TFW.WriteA(s);
      TFW.WriteLineA('>');
    end;  
  end;
  TFW.Free;
  memo1.lines.add('TextFileWriterANSI: '+inttostr(gettickcount-w)+' ms');

  w:= gettickcount;
  SW:= TStreamWriter.Create('StreamWriter.txt',false,TEncoding.GetEncoding(1252),1024);
  for I := 0 to 100000 do
  begin
    if RadioGroup1.ItemIndex=1 then
      sw.writeline('abcdefghijklmnopqrstuvwxyz'+s+'>') else
    begin
      sw.Write('ab');
      sw.Write('cdefghi');
      sw.Write('jkl');
      sw.Write('mnopq');
      sw.Write('rstuvwxyz');
      sw.Write(s);
      sw.WriteLine('>');
    end;  
  end;
  SW.Close;
  sw.free;
  memo1.lines.add('StreamWriter ANSI: '+inttostr(gettickcount-w)+' ms');
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  TFW: TTextFileWriterUTF8;
  SW: TStreamWriter;
  i: integer;
  w: longword;
  s: string;
begin
  s:= 'זרו';

  w:= gettickcount;
  TFW:= TTextFileWriterUTF8.create('TextFileWriterUTF8.txt',true);
  for i:= 0 to 100000 do
  begin
    if RadioGroup1.ItemIndex=1 then
      TFW.writeline('abcdefghijklmnopqrstuvwxyz'+s+'>') else
    begin
      TFW.Write('ab');
      TFW.Write('cdefghi');
      TFW.Write('jkl');
      TFW.Write('mnopq');
      TFW.Write('rstuvwxyz');
      TFW.Write(s);
      TFW.WriteLine('>');
    end;
  end;
  TFW.Free;
  memo1.lines.add('TextFileWriterUTF8: '+inttostr(gettickcount-w)+' ms');

  w:= gettickcount;
  SW:= TStreamWriter.Create('StreamWriter_UTF8.txt',false,TEncoding.UTF8,1024);
  for I := 0 to 100000 do
  begin
    if RadioGroup1.ItemIndex=1 then
      sw.writeline('abcdefghijklmnopqrstuvwxyz'+s+'>') else
    begin
      sw.Write('ab');
      sw.Write('cdefghi');
      sw.Write('jkl');
      sw.Write('mnopq');
      sw.Write('rstuvwxyz');
      sw.Write(s);
      sw.WriteLine('>');
    end;
  end;
  SW.Close;
  sw.free;
  memo1.lines.add('StreamWriter UTF8: '+inttostr(gettickcount-w)+' ms');
end;

end.
