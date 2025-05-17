unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, MemoryModule, Buttons;

type
  TForm1 = class(TForm)
    btn1: TSpeedButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FDLL: TMemoryStream;
    FM: HMODULE;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

var
  A: function(p: PWideChar): Integer; stdcall;

procedure TForm1.btn1Click(Sender: TObject);
var
  EntryPoint: Pointer;
begin
  FM := MemLoadLibrary(FDLL.Memory);
  //LoadMyResDll(FDLL.Memory, EntryPoint);
  @a := MemGetProcAddress(FM,'A');
  a('msg');
  @a := MemGetProcAddress(FM, PAnsiChar(2));
  a('msg');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  //LoadLibraryA('testdll.dll');
  FDLL := TMemoryStream.Create;
  FDLL.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'testdll.dll');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MemFreeLibrary(FM);
  FDLL.Free;
end;

end.
