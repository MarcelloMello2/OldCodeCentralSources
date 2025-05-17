unit SampleGCObjects;

interface

uses
  SysUtils,
  stGC;

type
  tGCObject = class;

  iGCObject = interface
  ['{272BF9CB-790A-41AC-A5F0-E49D8760E601}']
    function GetObject: tGCObject;
  end;

  tGCObjectArray = array of iGCField;

  tGCObject = class(tGCAbstractObject,iGCObject)
  private
    fOwner: iGCField;
    fLock: TMultiReadExclusiveWriteSynchronizer;
    function GetOwner: tGCObject;
  protected
    function FieldToObject(const aField: iGCField): tGCObject;
    procedure ObjectToField(aObject: tGCObject; var aField: iGCField);
    function GetCount: Integer; virtual;
    function GetItems(aIndex: Integer): iGCObject; virtual;
    procedure SetItems(aIndex: Integer; const Value: iGCObject); virtual; abstract;
    { iGCInternal }
    procedure Lock; override;
    procedure Unlock; override;
    procedure InitializeLock; override;
    procedure FinalizeLock; override;
    { iGCObject }
    function GetObject: tGCObject;
  public
    constructor CreateOwner(aOwner: tGCObject; out aRef);
    procedure RemoveOwner;
    property Owner: tGCObject read GetOwner;
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: iGCObject read GetItems write SetItems;
  end;

  tGCObjectWithStaticFields = class(tGCObject)
  private
    fA: iGCField;
    fB: iGCField;
    fC: iGCField;
    procedure SetA(const Value: tGCObject);
    procedure SetB(const Value: tGCObject);
    procedure SetC(const Value: tGCObject);
    function GetA: tGCObject;
    function GetB: tGCObject;
    function GetC: tGCObject;
  protected
    function GetCount: Integer; override;
    function GetItems(aIndex: Integer): iGCObject; override;
    procedure SetItems(aIndex: Integer; const Value: iGCObject); override;
  public
    property A: tGCObject read GetA write SetA;
    property B: tGCObject read GetB write SetB;
    property C: tGCObject read GetC write SetC;
  end;

  tGCObjectWithDynamicFields = class(tGCObject)
  private
    fItems: tGCObjectArray;
  protected
    function GetCount: Integer; override;
    function GetItems(aIndex: Integer): iGCObject; override;
    procedure SetItems(aIndex: Integer; const Value: iGCObject); override;
    { iGCInternal }
    procedure Lock; override;
    procedure Unlock; override;
    procedure InitializeLock; override;
    procedure FinalizeLock; override;
  public
    procedure AddItem(aItem: iGCObject); overload;
    function AddItem: iGCObject; overload;
    procedure Delete(aIndex: Integer);
  end;


implementation

{ tGCObjectWithStaticFields }

function tGCObjectWithStaticFields.GetA: tGCObject;
begin
  Result := FieldToObject(fA);
end;

function tGCObjectWithStaticFields.GetB: tGCObject;
begin
  Result := FieldToObject(fB);
end;

function tGCObjectWithStaticFields.GetC: tGCObject;
begin
  Result := FieldToObject(fC);
end;

function tGCObjectWithStaticFields.GetCount: Integer;
begin
  Result := 3 + inherited GetCount;
end;

function tGCObjectWithStaticFields.GetItems(aIndex: Integer): iGCObject;
begin
  case aIndex - inherited GetCount of
    0: Result := FieldToObject(fA);
    1: Result := FieldToObject(fB);
    2: Result := FieldToObject(fC);
  else
    Result := inherited GetItems(aIndex);
  end;
end;

procedure tGCObjectWithStaticFields.SetA(const Value: tGCObject);
begin
  ObjectToField(Value,fA);
end;

procedure tGCObjectWithStaticFields.SetB(const Value: tGCObject);
begin
  ObjectToField(Value,fB);
end;

procedure tGCObjectWithStaticFields.SetC(const Value: tGCObject);
begin
  ObjectToField(Value,fC);
end;

procedure tGCObjectWithStaticFields.SetItems(aIndex: Integer;
  const Value: iGCObject);
var
  lObj: tGCObject;
begin
  if Assigned(Value) then
    lObj := Value.GetObject
  else
    lObj := nil;
  case aIndex - inherited GetCount of
    0: SetA(lObj);
    1: SetB(lObj);
    2: SetC(lObj);
  end;
end;

{ tGCObject }

constructor tGCObject.CreateOwner(aOwner: tGCObject; out aRef);
begin
  SafeAssignFieldInterface(aRef,Self);
  SafeAssignFieldInterface(fOwner,aOwner);
end;

function tGCObject.FieldToObject(const aField: iGCField): tGCObject;
begin
  Lock;
  try
    if Assigned(aField) then
      Result := tGCObject(aField.GetInstance)
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

procedure tGCObject.FinalizeLock;
begin
  FreeAndNil(fLock);
end;

function tGCObject.GetCount: Integer;
begin
  Result := 1;
end;

function tGCObject.GetItems(aIndex: Integer): iGCObject;
begin
  Result := nil;
  if aIndex = 0 then begin
    Result := FieldToObject(fOwner);
  end;
end;

function tGCObject.GetObject: tGCObject;
begin
  Result := Self;
end;

function tGCObject.GetOwner: tGCObject;
begin
  Result := FieldToObject(fOwner);
end;

procedure tGCObject.InitializeLock;
begin
  if not GetManager.UseLocks then
    fLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure tGCObject.Lock;
begin
  if Assigned(fLock) then
    fLock.BeginRead;
end;

procedure tGCObject.ObjectToField(aObject: tGCObject; var aField: iGCField);
begin
  if Assigned(fLock) then
    fLock.BeginWrite;
  try
    SafeAssignFieldInterface(aField,aObject);
  finally
    if Assigned(fLock) then
      fLock.EndWrite;
  end;
end;

procedure tGCObject.RemoveOwner;
begin
  fOwner := nil;
end;

procedure tGCObject.Unlock;
begin
  if Assigned(fLock) then
    fLock.EndRead;
end;

{ tGCObjectWithDynamicFields }

procedure tGCObjectWithDynamicFields.AddItem(aItem: iGCObject);
var
  lIdx: Integer;
begin
  fLock.BeginWrite;
  try
    lIdx := Length(fItems);
    SetLength(fItems,lIdx+1);
    if Assigned(aItem) then
      SafeAssignFieldInterface(fItems[lIdx],aItem.GetObject)
    else
      SafeAssignFieldInterface(fItems[lIdx],nil)
  finally
    fLock.EndWrite;
  end;
end;

function tGCObjectWithDynamicFields.AddItem: iGCObject;
var
  lIdx: Integer;
begin
  fLock.BeginWrite;
  try
    lIdx := Length(fItems);
    SetLength(fItems,lIdx+1);
    Result := tGCObjectWithDynamicFields.CreateOwner(Self,fItems[lIdx]);
  finally
    fLock.EndWrite;
  end;
end;

procedure tGCObjectWithDynamicFields.Delete(aIndex: Integer);
var
  lIdx, lCount: Integer;
begin
  fLock.BeginWrite;
  try
    lCount := Length(fItems);
    if aIndex < lCount then begin
      for lIdx := aIndex+1 to lCount-1 do
        SafeAssignFieldInterface(fItems[lIdx-1],fItems[lIdx]);
      SetLength(fItems,lCount-1);
    end;
  finally
    fLock.EndWrite;
  end;
end;

procedure tGCObjectWithDynamicFields.FinalizeLock;
begin
  FreeAndNil(fLock);
end;

function tGCObjectWithDynamicFields.GetCount: Integer;
begin
  Lock;
  try
    Result := Length(fItems) + inherited GetCount;
  finally
    Unlock;
  end;
end;

function tGCObjectWithDynamicFields.GetItems(aIndex: Integer): iGCObject;
var
  lBase: Integer;
begin
  Lock;
  try
    Result := nil;
    lBase := inherited GetCount;
    if aIndex < lBase then
      Result := inherited GetItems(aIndex)
    else if aIndex-lBase < Length(fItems) then
      Result := FieldToObject(fItems[aIndex-lBase])
    else
      Result := nil;
  finally
    Unlock;
  end;
end;

procedure tGCObjectWithDynamicFields.InitializeLock;
begin
  fLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

procedure tGCObjectWithDynamicFields.Lock;
begin
  fLock.BeginRead;
end;

procedure tGCObjectWithDynamicFields.SetItems(aIndex: Integer;
  const Value: iGCObject);
var
  lObj: tGCObject;
  lLockAlways: Boolean;
begin
  if Assigned(Value) then
    lObj := Value.GetObject
  else
    lObj := nil;
  lLockAlways := not GetManager.UseLocks;
  if lLockAlways then
    fLock.BeginWrite
  else
    Lock;
  try
    if (aIndex < Length(fItems)) and (aIndex >= 0) then
      SafeAssignFieldInterface(fItems[aIndex],lObj);
  finally
    if lLockAlways then
      fLock.EndWrite
    else
      Unlock;
  end;
end;

procedure tGCObjectWithDynamicFields.Unlock;
begin
  fLock.EndRead;
end;

end.
