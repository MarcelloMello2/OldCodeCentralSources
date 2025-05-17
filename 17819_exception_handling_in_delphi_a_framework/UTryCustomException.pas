unit UTryCustomException;

interface

uses
  SysUtils, UExceptionObj;

procedure GlobalExceptionHandler(AException: Exception; sUnit, sModule,
  sProcedure: string);

implementation

procedure GlobalExceptionHandler(AException: Exception; sUnit, sModule,
  sProcedure: string);
var
  recErrDet: TErrorDetails;
begin
  recErrDet.sModule := sModule;
  recErrDet.sProcedure := sProcedure;
  recErrDet.sUnit := sUnit;

  if AException is ECustomException then
  begin
    (AException as ECustomException).AddToCallStack(recErrDet);
    raise ECustomException.Create(AException.Message, (AException as ECustomException));
  end
  else
    raise ECustomException.Create(AException.Message, recErrDet); 
end;

end.
