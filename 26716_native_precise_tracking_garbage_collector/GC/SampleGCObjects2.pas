unit SampleGCObjects2;

interface

uses
  Classes, SysUtils,
  stGC, SampleGCObjects;

type
  tGCStringList = class;

  iGCStringList = interface
  ['{738211F8-2E4C-4037-84B9-1D4E654AEE74}']
    function GetList: tGCStringList;
  end;

  tGCStringList = class(TStringList,IUnknown,iGCInternal,iGCField,iGCStringList)
  private
    fObjects: tGCObjectArray;
    fLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    class function FieldToObject(const aField: iGCField): TObject;
    { TStringList }
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { iGCField }
    function GetInstance: TObject;
    function _AddFieldRef: Integer; stdcall;
    function _ReleaseField: Integer; stdcall;
    function iGCField._AddRef = _AddFieldRef;
    function iGCField._Release = _ReleaseField;
    { iGCInternal }
    procedure FinalizeLock;
    procedure InitializeLock;
    procedure Lock;
    procedure Unlock;
    function iGCInternal._AddRef = _AddFieldRef;
    function iGCInternal._Release = _ReleaseField;
    { iGCStringList }
    function GetList: tGCStringList;
  public
    constructor Create;
    procedure Delete(Index: Integer); override;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  tInnerRec = record
    fObjects: tGCObjectArray;
    fCount: Integer;
  end;

  tInnerArray = array [0..7] of tInnerRec;

  tOuterRec = record
    fStaticList: tInnerArray;
    fDynamicList: array of tInnerRec;
  end;

  tOuterArray = array of tOuterRec;

  tGCConvoluted = class(tGCObject)
  private
    fItems: tOuterArray;
    fCount: Integer;
    fLock: TMultiReadExclusiveWriteSynchronizer;
    procedure Fill;
  protected
    function GetCount: Integer; override;
    function GetItems(aIndex: Integer): iGCObject; override;
    procedure SetItems(aIndex: Integer; const Value: iGCObject); override;
    { iGCInternal }
    procedure Lock; override;
    procedure Unlock; override;
    procedure InitializeLock; override;
    procedure FinalizeLock; override;
  end;

implementation

{ tGCStringList }

procedure tGCStringList.AfterConstruction;
begin
  tGCManager.GetGCObject(Self).DoCreate;
end;

procedure tGCStringList.BeforeDestruction;
begin
  tGCManager.GetGCObject(Self).DoDestroy;
end;

constructor tGCStringList.Create;
begin
  tGCManager.GetGCObject(Self).IgnoreFirstAcquire;
end;

procedure tGCStringList.Delete(Index: Integer);
var
  lOldCount, lIdx: Integer;
begin
  fLock.BeginWrite;
  try
    lOldCount := Count;
    inherited;
    for lIdx := Index+1 to lOldCount - 1 do
      SafeAssignFieldInterface(fObjects[lIdx-1],fObjects[lIdx]);
    fObjects[lOldCount-1] := nil;
  finally
    fLock.EndWrite;
  end;
end;

class function tGCStringList.FieldToObject(const aField: iGCField): TObject;
begin
  if Assigned(aField) then
    Result := aField.GetInstance
  else
    Result := nil;
end;

procedure tGCStringList.FinalizeLock;
begin
  FreeAndNil(fLock);
end;

procedure tGCStringList.FreeInstance;
begin
  tGCManager.GetDefault.GCFreeInstance(Self);
end;

function tGCStringList.GetInstance: TObject;
begin
  Result := Self;
end;

function tGCStringList.GetList: tGCStringList;
begin
  Result := Self;
end;

function tGCStringList.GetObject(Index: Integer): TObject;
begin
  Lock;
  try
    Result := inherited GetObject(Index);
    Result := FieldToObject(fObjects[Index]);
  finally
    Unlock;
  end;
end;

procedure tGCStringList.InitializeLock;
begin
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure tGCStringList.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
var
  lOldCount, lIdx: Integer;
  lFld: iGCField;
begin
  fLock.BeginWrite;
  try
    lOldCount := Count;
    lFld := nil;
    if Assigned(AObject) then
      if Supports(AObject,iGCField,lFld) then
        AObject := nil;
    inherited;
    for lIdx := lOldCount - 1 downto Index do
      SafeAssignFieldInterface(fObjects[lIdx+1],fObjects[lIdx]);
    SafeAssignFieldInterface(fObjects[Index],lFld);
  finally
    fLock.EndWrite;
  end;
end;

procedure tGCStringList.Lock;
begin
  fLock.BeginRead;
end;

class function tGCStringList.NewInstance: TObject;
begin
  Result := tGCManager.GetDefault.GCNewInstance(Self);
end;

procedure tGCStringList.PutObject(Index: Integer; AObject: TObject);
var
  lLockAlways: Boolean;
  lFld: iGCField;
begin
  lLockAlways := not tGCManager.GetDefault.UseLocks;
  if lLockAlways then
    fLock.BeginWrite
  else
    Lock;
  try
    lFld := nil;
    if Supports(AObject,iGCField,lFld) then
      AObject := nil;
    inherited;
    SafeAssignFieldInterface(fObjects[Index],lFld);
  finally
    if lLockAlways then
      fLock.EndWrite
    else
      Unlock;
  end;
end;

function tGCStringList.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure tGCStringList.SetCapacity(NewCapacity: Integer);
var
  lOldCapacity, lIdx: Integer;
begin
  fLock.BeginWrite;
  try
    lOldCapacity := Capacity;
    inherited SetCapacity(NewCapacity);
    for lIdx := NewCapacity to Length(fObjects) - 1 do
      SafeAssignFieldInterface(fObjects[lIdx],nil);
    SetLength(fObjects,NewCapacity);
    for lIdx := lOldCapacity to NewCapacity - 1 do
      Pointer(fObjects[lIdx]) := nil;
  finally
    fLock.EndWrite;
  end;
end;

procedure tGCStringList.Unlock;
begin
  fLock.EndRead;
end;

function tGCStringList._AddFieldRef: Integer;
begin
  Result := -1;
end;

function tGCStringList._AddRef: Integer;
begin
  Result := tGCManager.GetGCObject(Self).Acquire;
end;

function tGCStringList._Release: Integer;
begin
  Result := tGCManager.GetGCObject(Self).Release;
end;

function tGCStringList._ReleaseField: Integer;
begin
  Result := -1;
end;

{ tGCConvoluted }

procedure tGCConvoluted.Fill;
var
  I, J, lIdx: Integer;

  procedure FillInnerRec(var aInnerRec: tInnerRec);
  var
    K, L: Integer;
    lList: iGCStringList;
  begin
    SetLength(aInnerRec.fObjects,16);
    aInnerRec.fCount := 16;
    for K := 0 to 15 do begin
      lList := tGCStringList.Create;
      try
        SafeAssignFieldInterface(aInnerRec.fObjects[K],lList.GetList);
        lList.GetList.SetCapacity(16);
        for L := 0 to 15 do begin
          lList.GetList.AddObject(IntToStr(lIdx),nil);
          Inc(lIdx);
        end;
      finally
        lList := nil;
      end;
    end;
  end;

begin
  lIdx := 0;
  SetLength(fItems,16);
  for I := 0 to Length(fItems) - 1 do begin
    for J := 0 to High(fItems[I].fStaticList) do begin
      FillInnerRec(fItems[I].fStaticList[J]);
    end;
    SetLength(fItems[I].fDynamicList,8);
    for J := 0 to 7 do begin
      FillInnerRec(fItems[I].fDynamicList[J]);
    end;
  end;
  fCount := lIdx;
end;

procedure tGCConvoluted.FinalizeLock;
begin
  FreeAndNil(fLock);
end;

function tGCConvoluted.GetCount: Integer;
begin
  Lock;
  try
    if fCount = 0 then begin
      fLock.BeginWrite;
      try
        if fCount = 0 then Fill;
      finally
        fLock.EndWrite;
      end;
    end;
    Result := fCount;
  finally
    Unlock;
  end;
end;

function tGCConvoluted.GetItems(aIndex: Integer): iGCObject;
var
  I, J, K, L: Integer;
  lObj: TObject;
begin
  Lock;
  try
    Result := nil;
    if aIndex < GetCount then begin
      L := aIndex mod 16;
      aIndex := aIndex div 16;
      K := aIndex mod 16;
      aIndex := aIndex div 16;
      J := aIndex mod 16;
      I := aIndex div 16;
      if J < 8 then begin
        lObj := fItems[I].fStaticList[J].fObjects[K].GetInstance;
        lObj := tGCStringList(lObj).Objects[L];
      end else begin
        lObj := fItems[I].fDynamicList[J mod 8].fObjects[K].GetInstance;
        lObj := tGCStringList(lObj).Objects[L];
      end;
      Supports(lObj,iGCObject,Result);
    end;
  finally
    Unlock;
  end;
end;

procedure tGCConvoluted.InitializeLock;
begin
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure tGCConvoluted.Lock;
begin
  fLock.BeginRead;
end;

procedure tGCConvoluted.SetItems(aIndex: Integer; const Value: iGCObject);
var
  I, J, K, L: Integer;
  lObj: TObject;
  lList: iGCField;
begin
  Lock;
  try
    if aIndex < GetCount then begin
      L := aIndex mod 16;
      aIndex := aIndex div 16;
      K := aIndex mod 16;
      aIndex := aIndex div 16;
      J := aIndex mod 16;
      I := aIndex div 16;
      if J < 8 then begin
        lList := fItems[I].fStaticList[J].fObjects[K];
      end else begin
        lList := fItems[I].fDynamicList[J mod 8].fObjects[K];
      end;
      if Assigned(lList) then begin
        lObj := lList.GetInstance;
        if Assigned(Value) then
          tGCStringList(lObj).Objects[L] := Value.GetObject
        else
          tGCStringList(lObj).Objects[L] := nil;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure tGCConvoluted.Unlock;
begin
  fLock.EndRead;
end;

end.
