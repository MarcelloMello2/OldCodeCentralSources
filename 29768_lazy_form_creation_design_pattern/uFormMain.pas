unit uFormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TFormMain = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function FormMain: TFormMain;

implementation

{$R *.fmx}

var
  AFormMain: TFormMain;

function FormMain: TFormMain;
begin
  if AFormMain = nil then
    AFormMain := TFormMain.Create(Application);

  Result := AFormMain;
end;

end.
