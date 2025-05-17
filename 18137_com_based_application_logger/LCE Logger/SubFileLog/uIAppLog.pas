unit uIAppLog;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, AppLogFile_TLB, StdVcl, AppLogEvents_TLB;

type
  // imported from COMSVCS.DLL - begin
  IObjectConstruct = interface(IUnknown)
    ['{41C4F8B3-7439-11D2-98CB-00C04F8EE1C4}']
    function  Construct(const pCtorObj: IDispatch): HResult; stdcall;
  end;

  IObjectConstructString = interface(IDispatch)
    ['{41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}']
    function  Get_ConstructString: WideString; safecall;
    property ConstructString: WideString read Get_ConstructString;
  end;
  // imported from COMSVCS.DLL - end

  TFileLog = class(TAutoObject, IAppLog, IObjectConstruct)
  private
    FConstructString: string;
  protected
    // IAppLog
    procedure Log(const App, Msg: WideString; Kind: eMsgKind); safecall;
    // IObjectConstruct
    function Construct(const pCtorObj: IDispatch): HResult; stdcall;
  end;

implementation

uses
  ComServ,
  FileLog;

{ TFileLog }

function TFileLog.Construct(const pCtorObj: IDispatch): HResult; stdcall;
begin
  FConstructString := (pCtorObj as IObjectConstructString).ConstructString;
  Result := S_OK;
end;

procedure TFileLog.Log(const App, Msg: WideString; Kind: eMsgKind);
begin
  // no check for a valid file name, just make sure it is not empty
  if FConstructString <> '' then
    FileLog.SetLogFileName(FConstructString);
  FileLog.LogEvent(App, Msg, Kind);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TFileLog, Class_FileLog,
    ciMultiInstance, tmApartment);
end.
