{*******************************************************}
{                                                       }
{     StreamSec Security Library for CodeGear Delphi    }
{     Garbage Collection Base Class Unit                }
{                                                       }
{     Copyright (C) 2009 StreamSec Handelsbolag         }
{     Commercial use requires permission                }
{                                                       }
{*******************************************************}
unit stGC;

interface

uses
  SysUtils;

const
  IID_GCFieldBase: TGUID = '{ED2ED4B6-1521-42D4-853B-3F7FA316B682}';

type
  eGCException = class(Exception);
  eGCUnsupportedClass = class(eGCException);

  tGCManager = class; // forward

  { Managed fields of managed classes MUST be declared as iGCField. }
  iGCField = interface
  ['{ED2ED4B6-1521-42D4-853B-3F7FA316B682}']
    function GetInstance: TObject;
  end;

  { Any managed class that might hold strong references to other instances of
    any managed class MUST either implement the iGCInternal interface or
    descend from tGCAbstractObject. The tGCManager will use this interface for
    walking reference paths }
  iGCInternal = interface
  ['{14D0572A-046A-4158-9F49-B975593F28AA}']
    procedure Lock;
    procedure Unlock;
    procedure InitializeLock;
    procedure FinalizeLock;
  end;

  eGCDestruction = class(eGCException);
  eGCIllegalInstruction = class(eGCException);
  eGCReference = class(eGCException);

  tGCCreateAcquiredMode = (gcamCreateAcquired,gcamFirstAssignment);
  tGCReferenceKind = (gcrkStrong,gcrkWeak);

  tGCAbstractObject = class(TObject,IUnknown,iGCField)
  protected
    procedure CheckReference(var aRef; aKind: tGCReferenceKind = gcrkStrong);
    class function GetManager: tGCManager; virtual;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { iGCField }
    function GetInstance: TObject;
    function _AddFieldRef: Integer; stdcall;
    function _ReleaseField: Integer; stdcall;
    function iGCField._AddRef = _AddFieldRef;
    function iGCField._Release = _ReleaseField;
    { iGCInternal }
    procedure InitializeLock; virtual;
    procedure FinalizeLock; virtual;
    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;
  public
    { Use the Create constructor for creating an instance that is directly
      assigned to an external variable.
       * aMode = gcamCreateAcquired: If this mode is used, the Release method
      MUST be called before the variables goes out of scope, even if the
      variable is of an interface type
       * aMode = gcamFirstAssignment: If this mode is used, the first call
      to Acquire or _AddRef will not increment the reference count from the
      initial value of one. If the instance is directly assigned to e.g. an
      interface, the Release method will not have to be called an extra time }
    constructor Create(aMode: tGCCreateAcquiredMode = gcamCreateAcquired);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    { Call Acquire when assigning an instance to another external variable.
      Each call to Acquire must be matched by a call to Release }
    function Acquire: Integer;
    { Each call to Release must be matched by a prior call to CreateAcquired or
      Acquire }
    function Release: Integer;
  end;

  { The iGCProtected interface must be used internally by the methods of managed
    classes. Use the tGCManager.GetGCObject method for obtaining the iGCObject
    interface of an instance of a managed class. Managed classes SHOULD declare
    wrapper methods with public visibility for Acquire and Release, and MUST
    implement _AddRef and _Release to call Acquire and Release if they support
    interfaces for external use (i.e. except managed fields, which must be
    declared as iGCField). }
  iGCProtected = interface
    function Acquire: Integer;
    function Release: Integer;
    procedure DoCreate;
    procedure DoDestroy;
    procedure IgnoreFirstAcquire;
  end;

  pGCHeader = ^tGCHeader;
  tGCHeader = record
    fVTable: Pointer;
    fRefCount: Integer;
    fNextLive: pGCHeader;
    fNext: pGCHeader;
  end;

  IntPtr = Integer;

  tGCManager = class
  private
    fFirst: pGCHeader;
    fCache: pGCHeader;
    fCacheCapacity: Integer;
    fTempCache: pGCHeader;
    fObjectCount: Integer;
    fLiveObjectCount: Integer;
    fCacheCount: Integer;
    fBlockGranularity: Integer;
    fInSweep: Boolean; // Flag to prevent overlapping calls to Sweep
    fLock: TMultiReadExclusiveWriteSynchronizer;
    fUseLocks: Boolean;
    fNewUseLocks: Boolean;
    fLastLiveObjectCount: Integer;
    procedure GCGetMem(var aObj: pGCHeader; aSize: Integer);
    procedure GCFreeMem(aObj: pGCHeader);
    procedure AddToList(aObj: pGCHeader);
    function Collect(aObj: pGCHeader; aNoRemove: Boolean = False): pGCHeader;
    function SweepMark(aFirst       : pGCHeader;
                   var aPrevLast    : pGCHeader;
                   out aFirstUnknown: pGCHeader)
                                    : Boolean;
    procedure SweepCollect(aFirst,
                           aFirstUnknown,
                           aPrevLast    : pGCHeader;
                       out aFirstLive   : pGCHeader);
  public
    constructor Create;
    destructor Destroy; override;
    class function GetDefault: tGCManager;
    { To be called by NewInstance of the garbage collected class }
    function GCNewInstance(aClass: TClass): TObject;
    { To be called by FreeInstance of the garbage collected class }
    procedure GCFreeInstance(aObject: TObject);
    { GetGCObject returns a weak reference to a iGCProtected interface
      corresponding to aObj. The aObj instance MUST have been allocated using
      GCNewInstance. GetGCObject should only be called from within methods of
      aObj itself. }
    class function GetGCObject(aObj: TObject): iGCProtected;
    { Call Sweep to collect unreachable objects }
    procedure Sweep;
    procedure SetCacheCapacity(aNewCapacity: Integer);
    procedure SetBlockGranularity(aNewGranularity: Integer);
    property CacheCapacity: Integer read fCacheCapacity;
    property ObjectCount: Integer read fObjectCount;
    property LiveObjectCount: Integer read fLastLiveObjectCount;
    property UseLocks: Boolean read fUseLocks;
    function SetUseLocks(aUseLocks: Boolean): Boolean;
    procedure SuspendSweep;
    procedure ResumeSweep;
    procedure LockBeforeSweep;
    procedure UnlockAfterSweep;
  end;

function InterlockedIncrement(var Addend: Integer): Integer;
function InterlockedDecrement(var Addend: Integer): Integer;

procedure SafeClearInterface(var aIntf);
procedure SafeAssignInterface(var aDest; const aSource: IUnknown);
procedure SafeAssignFieldInterface(var aDest; const aSource: iGCField);

implementation

uses
  stGCFieldFinder;

var
  gDefaultManager: tGCManager;
  gLiveEndMarker: tGCHeader;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      INC   EAX
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      XCHG  EAX,EDX
 LOCK XADD  [EDX],EAX
      DEC   EAX
end;

function InterlockedExchange(var Dest: Pointer; aSource: Pointer): Pointer;
asm
  LOCK XCHG [EAX],EDX
       MOV  EAX,  EDX
end;

procedure SafeClearInterface(var aIntf);
var
  lPtr: Pointer;
begin
  lPtr := InterlockedExchange(Pointer(aIntf),nil);
  if Assigned(lPtr) then
    IUnknown(lPtr)._Release;
end;

procedure SafeAssignInterface(var aDest; const aSource: IUnknown);
var
  lPtr: Pointer;
begin
  if Assigned(aSource) then
    aSource._AddRef;
  lPtr := InterlockedExchange(Pointer(aDest),Pointer(aSource));
  if Assigned(lPtr) then
    IUnknown(lPtr)._Release;
end;

procedure MarkAsReachable(aInst: pGCHeader);
asm
  LOCK OR [EAX].tGCHeader.fRefCount,80000000h
end;

procedure UnmarkAsReachable(aInst: pGCHeader);
asm
  LOCK AND [EAX].tGCHeader.fRefCount,7fffffffh
end;

function GCHeaderToObject(aInst: pGCHeader): TObject;
begin
  Result := Pointer(IntPtr(aInst) + SizeOf(tGCHeader));
end;

function ObjectToGCHeader(aObj: TObject): pGCHeader;
begin
  Result := Pointer(IntPtr(aObj) - SizeOf(tGCHeader));
end;

const
  cRefCountMask = $3FFFFFFF;

type
  tGCProtectedImpl = (gcpiConstructor,
                      gcpiFieldConstructor,
                      gcpiFirstAssignment,
                      gcpiNormal,
                      gcpiCollecting);

procedure SwitchGCProtected(aInst: pGCHeader; aImpl: tGCProtectedImpl); forward;

procedure MarkAsCollecting(aInst: pGCHeader);
begin
  SwitchGCProtected(aInst,gcpiCollecting);
end;

function IsCollecting(aInst: pGCHeader): Boolean; forward;

function ResetFirstAssignment(aInst: pGCHeader): Boolean; forward;

{ iGCProtected methods }

function GCProtected_AddRef(aInst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function GCProtected_Release(aInst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function GCProtectedAcquire(aInst: pGCHeader): Integer;
begin
  Result := InterlockedIncrement(aInst.fRefCount) and cRefCountMask;
  if Result = 0 then
    raise eGCException.Create('Reference count overflow - unrecoverable');
end;

function GCProtectedFirAcquire(aInst: pGCHeader): Integer;
begin
  if ResetFirstAssignment(aInst) then
    Result := aInst.fRefCount and cRefCountMask
  else
    Result := GCProtectedAcquire(AInst);
end;

function GCProtectedColAcquire(aInst: pGCHeader): Integer;
begin
  raise eGCIllegalInstruction.Create('Object has already been collected');
end;

function GCProtectedRelease(aInst: pGCHeader): Integer;
begin
  Result := InterlockedDecrement(aInst.fRefCount) and cRefCountMask;
  if Result = cRefCountMask then
    raise eGCException.Create('Reference count underflow - unrecoverable');
  { A reference count underflow occurs if Release or _Release is called one
    time too much. When dealing with class instance references, make sure the
    number of calls to Create(gcamCreateAcquired) and Acquired match the
    number of calls to Release. When dealing with interfaces, beware that
    any assignment to interface variables that are accessible to multiple
    threads must be locked. For instance, two threads assigning to the same
    global interface variable at the same time, might cause _Release to be
    called twice on the instance that was previously assigned to the variable.
    Use the SafeClearInterface and SafeAssignInterface routines as a work
    around. }
end;

function GCProtectedColRelease(aInst: pGCHeader): Integer;
begin
  raise eGCIllegalInstruction.Create('Object has already been collected');
end;

function GCProtectedtQueryInterface(aInst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
const
  E_NOINTERFACE = HResult($80004002);
begin
  Result := E_NOINTERFACE;
end;

procedure GCProtectedColAssignToField(aInst: pGCHeader; out aRef);
begin
  raise eGCIllegalInstruction.Create('Object has already been collected');
end;

procedure GCProtectedDoCreate(aInst: pGCHeader);
begin
  raise eGCIllegalInstruction.Create('DoCreate MUST ONLY be called in AfterConstruction');
end;

procedure GCProtectedFieDoCreate(aInst: pGCHeader);
begin
  InterlockedDecrement(aInst.fRefCount); // Assigned to external field in constructor
  SwitchGCProtected(aInst,gcpiNormal);
end;

procedure GCProtectedFirDoCreate(aInst: pGCHeader);
begin
  // Do nothing, switch in first Acquire
end;

procedure GCProtectedConDoCreate(aInst: pGCHeader);
begin
  SwitchGCProtected(aInst,gcpiNormal); // Created acquired
end;

procedure GCProtectedColDoCreate(aInst: pGCHeader);
begin
  raise eGCIllegalInstruction.Create('Object has already been collected');
end;

procedure GCProtectedDoDestroy(aInst: pGCHeader);
begin
  // Allow destruction only if the constructor failed or called by Collect
  raise eGCDestruction.Create('This instance cannot be destroyed');
end;

procedure GCProtectedConDoDestroy(aInst: pGCHeader);
begin
  // Allow destruction only if the constructor failed or called by Collect
end;

procedure GCProtectedColDoDestroy(aInst: pGCHeader);
begin
  // Allow destruction only if the constructor failed or called by Collect
end;

procedure GCProtectedIgnoreFirstAcquire(aInst: pGCHeader);
begin
  raise eGCIllegalInstruction.Create('IgnoreFirstAcquire can only be called from constructor');
end;

procedure GCProtectedConIgnoreFirstAcquire(aInst: pGCHeader);
begin
  SwitchGCProtected(aInst,gcpiFirstAssignment);
end;

type
  tVTable = array[0..7] of Pointer;
  pVTableEx = ^tVTableEx;
  tVTableEx = record
    fVTable: tVTable;
    fUseLock: Boolean;
  end;

const
  GCProtected_Vtable: tVTable =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCProtectedAcquire,
    @GCProtectedRelease,
    @GCProtectedDoCreate,
    @GCProtectedDoDestroy,
    @GCProtectedIgnoreFirstAcquire
  );

  GCProtectedField_Vtable: tVTable =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCProtectedAcquire,
    @GCProtectedRelease,
    @GCProtectedFieDoCreate,                    // <--
    @GCProtectedDoDestroy,
    @GCProtectedIgnoreFirstAcquire
  );

  GCProtectedFirstAssignment_Vtable: tVTable =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCProtectedFirAcquire,                     // <--
    @GCProtectedRelease,
    @GCProtectedFirDoCreate,                    // <--
    @GCProtectedDoDestroy,
    @GCProtectedIgnoreFirstAcquire
  );

  GCProtectedConstructor_Vtable: tVTable =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCProtectedAcquire,
    @GCProtectedRelease,
    @GCProtectedConDoCreate,                    // <--
    @GCProtectedConDoDestroy,                   // <--
    @GCProtectedConIgnoreFirstAcquire           // <--
  );

  GCProtectedCollecting_Vtable: tVTable =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCProtectedColAcquire,                     // <--
    @GCProtectedColRelease,                     // <--
    @GCProtectedColDoCreate,                    // <--
    @GCProtectedColDoDestroy,                   // <--
    @GCProtectedIgnoreFirstAcquire
  );

procedure SwitchGCProtected(aInst: pGCHeader; aImpl: tGCProtectedImpl);
begin
  case aImpl of
    gcpiConstructor:
      aInst.fVTable := @GCProtectedConstructor_Vtable;
    gcpiFieldConstructor:
      aInst.fVTable := @GCProtectedField_Vtable;
    gcpiFirstAssignment:
      aInst.fVTable := @GCProtectedFirstAssignment_Vtable;
    gcpiNormal:
      aInst.fVTable := @GCProtected_VTable;
    gcpiCollecting:
      aInst.fVTable := @GCProtectedCollecting_Vtable;
  end;
end;


procedure SafeAssignFieldInterface(var aDest; const aSource: iGCField);
var
  lInst: pGCHeader;
begin
  Pointer(aDest) := Pointer(aSource);
  if Assigned(aSource) then begin
    lInst := ObjectToGCHeader(aSource.GetInstance);
    if lInst.fVTable = @GCProtectedConstructor_Vtable then
      SwitchGCProtected(lInst,gcpiFieldConstructor);
    MarkAsReachable(lInst);
  end;
end;

function IsCollecting(aInst: pGCHeader): Boolean;
begin
  Result := (aInst.fVTable <> @GCProtected_Vtable) and
            (aInst.fVTable <> @GCProtectedField_Vtable) and
            (aInst.fVTable <> @GCProtectedFirstAssignment_Vtable) and
            (aInst.fVTable <> @GCProtectedConstructor_Vtable);
end;

function IsCreating(aInst: pGCHeader): Boolean;
begin
  Result := aInst.fVTable = @GCProtectedConstructor_Vtable;
end;

function IsReachable(aInst: pGCHeader): Boolean;
begin
  Result := aInst.fRefCount <> 0;
end;

function Islive(aInst: pGCHeader): Boolean;
begin
  Result := Assigned(aInst.fNextLive);
end;

function ResetFirstAssignment(aInst: pGCHeader): Boolean;
asm
       MOV     ECX,  EAX
       LEA     EAX,  GCProtectedFirstAssignment_Vtable
       LEA     EDX,  GCProtected_Vtable
  LOCK CMPXCHG [ECX],EDX
       JE      @@Equal
       XOR     EAX,  EAX
       RET
@@Equal:
       MOV     EAX,1
end;

type
  pGCInternalRec = ^tGCInternalRec;
  tGCInternalRec = record
    fVTable: Pointer;
    fInst: tGCAbstractObject;
  end;

procedure GCInternalLock(aLock: pGCInternalRec);
begin
  aLock.fInst.Lock;
end;

procedure GCInternalUnlock(aLock: pGCInternalRec);
begin
  aLock.fInst.Unlock;
end;

procedure GCInternalInitializeLock(aLock: pGCInternalRec);
begin
  aLock.fInst.InitializeLock;
end;

procedure GCInternalFinalizeLock(aLock: pGCInternalRec);
begin
  aLock.fInst.FinalizeLock;
end;

const
  GCInternal_Vtable: array[0..6] of Pointer =
  (
    @GCProtectedtQueryInterface,
    @GCProtected_AddRef,
    @GCProtected_Release,
    @GCInternalLock,
    @GCInternalUnlock,
    @GCInternalInitializeLock,
    @GCInternalFinalizeLock
  );

function MakeGCInternal(aObj: TObject; var aRec: tGCInternalRec): iGCInternal;
begin
  if aObj.InheritsFrom(tGCAbstractObject) then begin
    aRec.fVTable := @GCInternal_Vtable;
    aRec.fInst := tGCAbstractObject(aObj);
    Pointer(Result) := @aRec;
  end else begin
    Result := nil;
    if not aObj.GetInterface(iGCInternal,Result) then
      Result := nil;
  end;
end;

procedure AddToList(aObj: pGCHeader; var aFirst: pGCHeader);
asm
// Thread safe
       MOV     ECX,                   EAX
       MOV     EAX,                   [EDX]
@@1 :
       MOV     [ECX].tGCHeader.fNext, EAX
  LOCK CMPXCHG [EDX],                 ECX
       JNZ     @@1
end;

function RemoveFirstFromList(aObj: pGCHeader; var aFirst: pGCHeader): pGCHeader; overload;
asm
// Thread safe
       MOV     ECX,   [EAX].tGCHeader.fNext
  LOCK CMPXCHG [EDX], ECX
end;

function RemoveFirstFromList(var aFirst: pGCHeader): pGCHeader; overload;
asm
// Thread safe
       MOV     ECX,   EAX
       MOV     EAX,   [ECX]
@@1:
       TEST    EAX,   EAX
       JZ      @@Exit
       MOV     EDX,   [EAX].tGCHeader.fNext
  LOCK CMPXCHG [ECX], EDX
       JNZ     @@1
@@Exit:
end;

function ExtractList(var aFirst: pGCHeader): pGCHeader;
asm
// Thread safe
       XOR  EDX,  EDX
  LOCK XCHG [EAX],EDX
       MOV  EAX,  EDX
end;

function RemoveFromList(aObj: pGCHeader; var aFirst: pGCHeader): pGCHeader;
var
  lNext: pGCHeader;
begin
  Result := aObj.fNext;
  { Called from a single thread. As long as aObj isn't gFirst, no locking is
    required. It is also assumed that aObj is in fact somewhere on the list.
  }
  lNext := RemoveFirstFromList(aObj,aFirst);
  if lNext <> aObj then begin
    while lNext.fNext <> aObj do
      lNext := lNext.fNext;
    lNext.fNext := Result;
  end;
end;

function MarkInSweep(aExpected, aNewValue: Boolean; var aInSweep: Boolean): Boolean;
asm
  LOCK CMPXCHG [ECX], DL
end;

procedure MarkReferenced(aInst: pGCHeader);
var
  lRec: tGCInternalRec;
  lIntf: iGCInternal;
  lNext: pGCHeader;
  lLastLive: pGCHeader;

  function MarkAsLive(aObj: pGCHeader): Boolean;
  begin
    Result := Assigned(aObj.fNextLive);
    if not Result then begin
      aObj.fNextLive := lLastLive.fNextLive;
      lLastLive.fNextLive := aObj;
      lLastLive := aObj;
    end;
  end;

begin
  lLastLive := @gLiveEndMarker;
  gLiveEndMarker.fNextLive := lLastLive;
  if Assigned(aInst) and not MarkAsLive(aInst) then begin
    lNext := aInst;
    while lNext <> @gLiveEndMarker do begin
      if IsCollecting(lNext) then
        raise eGCException.Create('Live reference to collected object - check your code');
      lIntf := MakeGCInternal(GCHeaderToObject(lNext),lRec);
      lIntf.Lock;
      try
        tGCFieldDefinitions.GetFields(GCHeaderToObject(lNext),lLastLive);
      finally
        lIntf.Unlock;
      end;
      lNext := lNext.fNextLive;
    end;
  end;
end;

{ tGCManager }

procedure tGCManager.AddToList(aObj: pGCHeader);
begin
  stGC.AddToList(aObj,fFirst);
end;

function tGCManager.Collect(aObj: pGCHeader; aNoRemove: Boolean): pGCHeader;
begin
  if aNoRemove then
    Result := aObj.fNext
  else
  // Return the value of the next item in the linked list
  // This return value is used by the Sweep procedure
    Result := RemoveFromList(aObj,fFirst);
  aObj.fNext := nil;
  aObj.fNextLive := nil;
  MarkAsCollecting(aObj);
  GCHeaderToObject(aObj).Destroy;
end;

constructor tGCManager.Create;
begin
  fBlockGranularity := 32;
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor tGCManager.Destroy;
begin
  fLock.Free;
  inherited;
end;

procedure tGCManager.GCFreeInstance(aObject: TObject);
var
  lRec: tGCInternalRec;
  lInt: iGCInternal;
begin
  { If destroyed by constructor failure, unmark
    If destroyed by Sweep, Collect }
  if IsCreating(ObjectToGCHeader(aObject)) then begin
    UnmarkAsReachable(ObjectToGCHeader(aObject));
    SwitchGCProtected(ObjectToGCHeader(aObject),gcpiNormal);
    InterlockedDecrement(ObjectToGCHeader(aObject).fRefCount);
  end else begin
    lInt := MakeGCInternal(aObject,lRec);
    lInt.FinalizeLock;
    lInt := nil;
    tGCFieldDefinitions.Cleanup(aObject);
    aObject.CleanupInstance;
    GCFreeMem(ObjectToGCHeader(aObject));
  end;
end;

procedure tGCManager.GCFreeMem(aObj: pGCHeader);
begin
  // GCFreeMem is ONLY called within the context of Sweep
  if fCacheCount < fCacheCapacity then begin
    aObj.fNext := fTempCache;
    fTempCache := aObj;
    Inc(fCacheCount);
  end else begin
    System.FreeMem(aObj);
    InterlockedDecrement(fObjectCount);
  end;
end;

procedure tGCManager.GCGetMem(var aObj: pGCHeader; aSize: Integer);

  function RoundUpSize(aSize: Integer): Integer;
  begin
    Result := aSize - (aSize mod fBlockGranularity);
    if Result < aSize then
      Result := Result + fBlockGranularity;
  end;

begin
  aObj := nil;
  if fCacheCapacity >= 0 then begin
    aObj := RemoveFirstFromList(fCache);
  end;
  if Assigned(aObj) then begin
    if not IsCollecting(aObj) then
      raise eGCException.Create('Internal');
    System.ReallocMem(aObj,RoundUpSize(aSize));
  end else begin
    System.GetMem(aObj,RoundUpSize(aSize));
    InterlockedIncrement(fObjectCount);
  end;
end;

function tGCManager.GCNewInstance(aClass: TClass): TObject;
var
  lObj: pGCHeader;
  lRec: tGCInternalRec;
begin
  if aClass.InheritsFrom(tGCAbstractObject) or
     Assigned(aClass.GetInterfaceEntry(iGCInternal)) then begin
    GCGetMem(lObj,aClass.InstanceSize + SizeOf(tGCHeader));
    Assert(Assigned(lObj));
    Result := aClass.InitInstance(GCHeaderToObject(lObj));
    Assert(Assigned(Result));
    lObj.fVTable := @GCProtectedConstructor_Vtable;
    lObj.fRefCount := 1;
    lObj.fNextLive := nil;
    MakeGCInternal(Result,lRec).InitializeLock;
    AddToList(lObj);
  end else
    raise eGCUnsupportedClass.CreateFmt('Class %s is not supported',[aClass.ClassName]);
end;

class function tGCManager.GetDefault: tGCManager;
begin
  Result := gDefaultManager;
end;

class function tGCManager.GetGCObject(aObj: TObject): iGCProtected;
begin
  Pointer(Result) := ObjectToGCHeader(aObj);
end;

procedure tGCManager.LockBeforeSweep;
begin
  if fUseLocks then
    fLock.BeginWrite;
end;

procedure tGCManager.ResumeSweep;
begin
  if fUseLocks then
    fLock.EndRead;
end;

procedure tGCManager.SetBlockGranularity(aNewGranularity: Integer);
begin
  if aNewGranularity > 8 then
    fBlockGranularity := aNewGranularity
  else
    fBlockGranularity := 8;
end;

procedure tGCManager.SetCacheCapacity(aNewCapacity: Integer);
begin
  if aNewCapacity > 0 then
    fCacheCapacity := aNewCapacity
  else
    fCacheCapacity := 0;
end;

function tGCManager.SetUseLocks(aUseLocks: Boolean): Boolean;
begin
  Result := True;
  if aUseLocks then begin
    if fObjectCount = 0 then begin
      fNewUseLocks := True;
      fUseLocks := True;
    end else
      Result := False;
  end else
    fNewUseLocks := False;
end;

procedure tGCManager.SuspendSweep;
begin
  if fUseLocks then
    fLock.BeginRead;
end;

procedure tGCManager.Sweep;
var
  lFirst, lNext,
  lPrevLast, lFirstUnknown, lFirstLive: pGCHeader;

begin
  if MarkInSweep(False,True,fInSweep) then Exit;
  try
  { Any objects added to the list will probably stay alive for the duration of
    of Sweep. Get the value of gFirst once and stick to it }
    lFirst := fFirst;
    // Mark roots and references, first attempt
    fLiveObjectCount := 0;
    lNext := lFirst;
    Assert(Assigned(GCHeaderToObject(lNext)));
    lPrevLast := nil;
    while Assigned(lNext) do begin
      Assert(not IsCollecting(lNext));
      Inc(fLiveObjectCount);
      if IsReachable(lNext) then
        MarkReferenced(lNext)
      else if not Islive(lNext) then
        lPrevLast := lNext;
      lNext := lNext.fNext;
      Assert(Assigned(GCHeaderToObject(lNext)));
    end;
    // Mark
    if SweepMark(lFirst,lPrevLast,lFirstUnknown) then begin
      // Collect
      SweepCollect(lFirst,lFirstUnknown,lPrevLast,lFirstLive);
    end else
      lFirstLive := lFirst;
    // Reset all that are left
    if fUseLocks then
      lNext := lFirstLive
    else
      lNext := fFirst;
    fLiveObjectCount := 0;
    while Assigned(lNext) do begin
      Assert(not IsCollecting(lNext));
      Inc(fLiveObjectCount);
      UnmarkAsReachable(lNext);
      lNext.fNextLive := nil;
      lNext := lNext.fNext;
    end;
    fLastLiveObjectCount := fLiveObjectCount;
  finally
    MarkInSweep(True,False,fInSweep);
  end;
end;

procedure tGCManager.SweepCollect(aFirst, aFirstUnknown, aPrevLast: pGCHeader;
  out aFirstLive: pGCHeader);
var
  lNext, lLast: pGCHeader;

  procedure AcquireCache;
  var
    lNext: pGCHeader;
  begin
    fTempCache := ExtractList(fCache);
    fCacheCount := fObjectCount - fLiveObjectCount;
    while fCacheCount > fCacheCapacity do begin
      lNext := fTempCache;
      if lNext = nil then Break;
      fTempCache := lNext.fNext;
      GCFreeMem(lNext);
      Dec(fCacheCount);
    end;
  end;

  procedure ReleaseCache;
  begin
    fCache := fTempCache;
  end;

begin
  AcquireCache;
  { Collect unmarked and find first marked live object
    Only dead objects are unmarked and they will all be in the run
    aFirstUnknown > ... > aPrevLast
  }
  if Assigned(aPrevLast) then
    lLast := aPrevLast.fNext
  else
    lLast := nil;
  if (aFirst = aFirstUnknown) then
    aFirstLive := lLast
  else
    aFirstLive := aFirst;
  // Collect lFirstUnknown, locked
  if Assigned(aFirstUnknown) then begin
    if Assigned(aFirstUnknown) then
      lNext := aFirstUnknown.fNext
    else
      lNext := nil;
    Assert(not (Assigned(lLast) and not Assigned(lNext)));
    Assert(not IsReachable(aFirstUnknown));
    Assert(not IsLive(aFirstUnknown));
    Assert(not IsCollecting(aFirstUnknown));
    aFirstUnknown.fNext := lLast;
    Collect(aFirstUnknown,False);
    // Collect all except lFirstUnknown, unlocked
    while lNext <> lLast do begin
      Assert(not IsReachable(lNext));
      Assert(not IsLive(lNext));
      Assert(not IsCollecting(lNext));
      lNext := Collect(lNext,True);
    end;
  end;
  ReleaseCache;
end;

function tGCManager.SweepMark(aFirst       : pGCHeader;
                          var aPrevLast    : pGCHeader;
                          out aFirstUnknown: pGCHeader)
                                           : Boolean;
var
  lFound: Boolean;
  lNext, lLast, lPrev: pGCHeader;

  procedure SwapInMarkAndSweep(var aCurrent : pGCHeader;
                                   aPrev    : pGCHeader;
                               var aDestPrev: pGCHeader;
                                   aDestNext: pGCHeader);
  begin
    { In:  O1(aDestPrev) > O2(aDestNext) > ... > O3(aPrev) > 04(aCurrent) > O5
      Out: O1 > O4(aDestPrev) > O2(aDestNext) > ... > O3(aCurrent) > O5
      Live: O3, O2 - Dead?: O4, O1 }
    aPrev.fNext := aCurrent.fNext;
    aDestPrev.fNext := aCurrent;
    aCurrent.fNext := aDestNext;
    aDestPrev := aCurrent;
    aCurrent := aPrev;
  end;

begin
  { Mark and sweep with a nested iteration and recursion.
    The idea behind this algorithm is to make sure that we reach any reachable
    instance, even if any references to it are changed while the sweep is
    running. The only way a reachable instance might evade a single iteration
    of the Mark and Sweep, would be if the instance becomes either a root or a
    field of an already visited instance before the instance that previously
    held a reference to the instance is visited. If that happens, either
    the root flag or the field flag of the evasive instance will be modified,
    and that is what the outer iteration is meant to suck up }
  aFirstUnknown := aFirst;
  repeat
    if Assigned(aPrevLast) then
      lLast := aPrevLast.fNext
    else
      lLast := nil;
    lFound := False;
    Result := False;
    lNext := aFirstUnknown;
    aPrevLast := nil;
    lPrev := nil;
    while lNext <> lLast do begin
      if IsLive(lNext) then begin
        lPrev := lNext;
      end else begin
        if IsReachable(lNext) then begin
          lFound := True;
          MarkReferenced(lNext);
          lPrev := lNext;
        end else begin
          // lNext is a potentially dead object
          if Assigned(aPrevLast) and (aPrevLast.fNext <> lNext) then begin
            { The trail of the first run of potentially dead objects has been
              found and lNext is beyond it. Move it back. Most objects in the
              entire list are probably live, and this can be done lock-lessly,
              so on average this is an optimization }
            Assert(Assigned(aFirstUnknown) and Assigned(lPrev) and
                   (lPrev.fNext = lNext));
            SwapInMarkAndSweep(lNext,lPrev,aPrevLast,aPrevLast.fNext);
          end else begin
            { When done, aFirstUnknown > ... > aPrevLast will point to the
              head and trail of the first run of potentially dead objects }
            if not Assigned(aPrevLast) then begin
              aFirstUnknown := lNext;
              Result := True;
            end;
            aPrevLast := lNext;
          end;
        end;
      end;
      lNext := lNext.fNext;
    end;
    if not fUseLocks then
      repeat
        lLast := aFirst;
        aFirst := fFirst;
        lNext := aFirst;
        while lNext <> lLast do begin
          if not IsLive(lNext) then
            if IsReachable(lNext) then begin
              lFound := True;
              MarkReferenced(lNext);
            end;
          lNext := lNext.fNext;
        end;
      until aFirst = fFirst;
  until not lFound;
end;

procedure tGCManager.UnlockAfterSweep;
begin
  if fUseLocks then begin
    if not fNewUseLocks then
      fUseLocks := False;
    fLock.EndWrite;
  end;
end;

{ tGCAbstractObject }

function tGCAbstractObject.Acquire: Integer;
begin
  Result := tGCManager.GetGCObject(Self).Acquire;
end;

procedure tGCAbstractObject.AfterConstruction;
begin
  tGCManager.GetGCObject(Self).DoCreate;
end;

procedure tGCAbstractObject.BeforeDestruction;
begin
  tGCManager.GetGCObject(Self).DoDestroy;
end;

procedure tGCAbstractObject.CheckReference(var aRef;
  aKind: tGCReferenceKind);
var
  lObj: TObject;
begin
  lObj := TObject(aRef);
  if Assigned(lObj) then
    if IsCollecting(ObjectToGCHeader(lObj)) then begin
      TObject(aRef) := nil;
      if aKind = gcrkStrong then begin
        if IsCollecting(ObjectToGCHeader(Self)) then
          raise eGCReference.Create('Strong reference from collected object to collected object - check your code')
        else
          raise eGCReference.Create('Strong reference to collected object - check your code');
      end;
    end;
end;

constructor tGCAbstractObject.Create(aMode: tGCCreateAcquiredMode);
begin
  if aMode = gcamFirstAssignment then
    tGCManager.GetGCObject(Self).IgnoreFirstAcquire;
end;

procedure tGCAbstractObject.FinalizeLock;
begin

end;

procedure tGCAbstractObject.FreeInstance;
begin
  GetManager.GCFreeInstance(Self);
end;

function tGCAbstractObject.GetInstance: TObject;
begin
  Result := Self;
end;

class function tGCAbstractObject.GetManager: tGCManager;
begin
  // Result := tGCManager.GetDefault;
  Result := gDefaultManager;
end;

procedure tGCAbstractObject.InitializeLock;
begin

end;

class function tGCAbstractObject.NewInstance: TObject;
begin
  Result := GetManager.GCNewInstance(Self);
end;

function tGCAbstractObject.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function tGCAbstractObject.Release: Integer;
begin
  // Result := _Release;
  Result := tGCManager.GetGCObject(Self).Release;
end;

function tGCAbstractObject._AddFieldRef: Integer;
begin
  Result := -1;
end;

function tGCAbstractObject._AddRef: Integer;
begin
  Result := tGCManager.GetGCObject(Self).Acquire;
end;

function tGCAbstractObject._Release: Integer;
begin
  Result := tGCManager.GetGCObject(Self).Release;
end;

function tGCAbstractObject._ReleaseField: Integer;
begin
  Result := -1;
end;

initialization
  gDefaultManager := tGCManager.Create;
finalization
  FreeAndNil(gDefaultManager);
end.
