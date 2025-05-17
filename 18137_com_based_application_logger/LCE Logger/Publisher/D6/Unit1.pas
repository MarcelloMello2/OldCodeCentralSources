unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    BtnLogInfo: TButton;
    BtnLogError: TButton;
    BtnLogWarning: TButton;
    EditInfo: TEdit;
    EditError: TEdit;
    EditWarning: TEdit;
    procedure BtnLogInfoClick(Sender: TObject);
    procedure BtnLogErrorClick(Sender: TObject);
    procedure BtnLogWarningClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  AppLogEvents_TLB;

procedure TForm1.BtnLogInfoClick(Sender: TObject);
var
  vIAppLog: IAppLog;
begin
  vIAppLog := CoAppLog.Create;
  vIAppLog.Log(Self.Caption, EditInfo.Text, emkInfo);
end;

procedure TForm1.BtnLogErrorClick(Sender: TObject);
var
  vIAppLog: IAppLog;
begin
  vIAppLog := CoAppLog.Create;
  vIAppLog.Log(Self.Caption, EditError.Text, emkError);
end;

procedure TForm1.BtnLogWarningClick(Sender: TObject);
var
  vIAppLog: IAppLog;
begin
  vIAppLog := CoAppLog.Create;
  vIAppLog.Log(Self.Caption, EditWarning.Text, emkWarning);
end;

end.
