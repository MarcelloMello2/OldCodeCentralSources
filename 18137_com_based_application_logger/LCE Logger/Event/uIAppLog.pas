unit uIAppLog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AppLogEvents_TLB, StdVcl;

type
  TAppLog = class(TAutoObject, IAppLog)
  protected
    procedure Log(const App, Msg: WideString; Kind: eMsgKind);virtual; safecall; abstract;
  end;

implementation

uses ComServ;

initialization
  TAutoObjectFactory.Create(ComServer, TAppLog, Class_AppLog,
    ciMultiInstance, tmApartment);
end.
