unit InterfU;

interface

Type
  {That interface is implemented in Host}
  IHostInterf = interface
    procedure SetCaption(Const Text: String);
  end;

  {Next two interfaces are implemented in Plugin}
  IPluginInterf = interface
    procedure CreateForm(Const InterfParam: IHostInterf);
  end;

  IFormInterf = interface
    procedure ShowForm;
  end;

Var
{These variables have different data segment in Host and Plugin}
  HostInterf: IHostInterf;
  PluginInterf: IPluginInterf;
  FormInterf: IFormInterf;
  PTlsIndx: ^Cardinal;

Procedure SetTls(Const InterfVar: IInterface);
Procedure GetTls(Var InterfVar: IInterface);

implementation

Uses Windows;

Procedure SetTls(Const InterfVar: IInterface);
begin
  PTlsIndx^:= TlsAlloc;
  TlsSetValue(PTlsIndx^, Pointer(InterfVar));
end;

Procedure GetTls(Var InterfVar: IInterface);
begin
  InterfVar:= IInterface(TlsGetValue(PTlsIndx^));
end;

end.
