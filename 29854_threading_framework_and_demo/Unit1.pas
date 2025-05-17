unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, AsyncTasksU;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FWorker: IThreadManager;
    procedure DisplayMsg(const S: string);
    procedure ThreadedTask(aState: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function CloseQuery: Boolean; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses NotificationDispatcherU, Win64CompatU;

{ TForm1 }

function TForm1.CloseQuery: Boolean;
begin
  Result := inherited CloseQuery;
  if result then begin
    (FWorker as IThreadManagerEx).PrepareForClose(250);
    NotificationDispatcher.UnSubscribe(DisplayMsg);
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FWorker := CreateWorkerthread;
  Memo1.Clear;
  NotificationDispatcher.Subscribe(DisplayMsg);
  Randomize;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  N, I: Integer;
begin
  memo1.clear;
  N:= Random(10);
  memo1.Lines.add(format('%d tasks will be added by thread %x',[N, GetCurrentThreadID]));
  for I := 0 to N - 1 do
    FWorker.QueueUserWorkitem(ThreadedTask, IntToPointer(I));
end;

procedure TForm1.DisplayMsg(const S: string);
begin
  Memo1.lines.add(S);
end;

procedure TForm1.ThreadedTask(aState: TObject);
begin
  NotificationDispatcher.Notify('aState = %d from thread %x',[PointerToInt(aState), GetCurrentThreadID]);
end;

end.
