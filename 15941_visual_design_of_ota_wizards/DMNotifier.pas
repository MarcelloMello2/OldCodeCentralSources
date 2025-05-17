unit DMNotifier;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI;

type
  TNotifierModule = class(TDataModule, IUnknown, IOTANotifier)
  private
    FRefCount: Integer;

    FOnAfterSave: TNotifyEvent;
    FOnBeforeSave: TNotifyEvent;
    FOnDestroyed: TNotifyEvent;
    FOnModified: TNotifyEvent;

    function IUnknown._AddRef = _AddRef;
    function IUnknown._Release = _Release;
  protected
    { IUnknown }
    // QueryInterface already implemented in TComponent
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;

    property RefCount: Integer read FRefCount;
  published
    property OnAfterSave: TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave: TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
    property OnDestroyed: TNotifyEvent read FOnDestroyed write FOnDestroyed;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

implementation

uses
  SysConst, 
  WizardUtils;

{$R *.DFM}

//----------------------------------------------------------------------------------------------------------------------

{ TNotifierModule protected: IUnknown }

//----------------------------------------------------------------------------------------------------------------------

function TNotifierModule._AddRef: Integer;

begin
  Result := InterlockedIncrement(FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

function TNotifierModule._Release: Integer;

begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TNotifierModule protected: IOTANotifier }

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.AfterSave;

begin
  if Assigned(FOnAfterSave) then
    FOnAfterSave(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.BeforeSave;

begin
  if Assigned(FOnBeforeSave) then
    FOnBeforeSave(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.Destroyed;

begin
  if Assigned(FOnDestroyed) then
    FOnDestroyed(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.Modified;

begin
  if Assigned(FOnModified) then
    FOnModified(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.AfterConstruction;

begin
  inherited AfterConstruction;
  // release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TNotifierModule.BeforeDestruction;

begin
  if FRefCount <> 0 then
    raise EInvalidPointer.Create(SInvalidPointer);
  inherited BeforeDestruction;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TNotifierModule.NewInstance: TObject;

begin
  Result := inherited NewInstance;
  TNotifierModule(Result).FRefCount := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
