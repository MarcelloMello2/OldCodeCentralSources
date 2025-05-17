program KeyValueHashDemo;

uses
  Forms,
  KVH_MainformU in 'KVH_MainformU.pas' {Mainform}
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Key-Value Hash Demo';
  Application.CreateForm(TMainform, Mainform);
  Application.Run;
end.
