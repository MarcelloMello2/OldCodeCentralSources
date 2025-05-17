unit SampleGCThread;

interface

uses
  Classes, SysUtils, Windows,
  stGC,
  SampleGCObjects, SampleGCObjects2;

type
  tSampleThread = class(TThread)
  private
    fRoots: array [0..15] of iGCObject;
    fDelay: Integer;
    class procedure AddRandom(aParent, aChild: iGCObject);
    procedure AddRandomAndDoSomething(aParent: iGCObject);
    procedure AddStructure(aParent: iGCObject; aLevel: Integer);
    class procedure DeleteRandom(aObj: iGCObject);
    class function Walk(aObj: iGCObject): iGCObject;
    class procedure ClearGlobalObject(aIndex: Integer);
    procedure ClearRandomGlobalObject;
    class procedure AssignGlobalObject(aIndex: Integer; aObj: iGCObject);
    class function GetGlobalObject(aIndex: Integer): iGCObject;
    function GetOrCreateRandomRoot: iGCObject;
    function GetNewGlobal: iGCObject;
    function GetGlobal: iGCObject;
    function GetNewRandomObject: iGCObject;
    procedure DoSomethingTo(aObj: iGCObject);
    procedure DoCreateThread;
  protected
    procedure Execute; override;
  public
    constructor Create(aDelay: Integer);
  end;

  tSweepThread = class(TThread)
  private
    fMaxTicks: Cardinal;
    fTotTicks: Int64;
    fSweepCount: Cardinal;
    fAvgTicks: Cardinal;
    fLastTicks: Cardinal;
  protected
    class procedure IncrementThreadCount;
    class procedure DecrementThreadCount;
    class function GetTerminateAll: Boolean;
    class function GetNoNewThreads: Boolean;
    procedure SetMaxTicks(aValue: Cardinal);
    procedure Execute; override;
  public
    constructor Create(aCreateSuspended: Boolean);
    property MaxTicks: Cardinal read fMaxTicks;
    property AvgTicks: Cardinal read fAvgTicks;
    property LastTicks: Cardinal read fLastTicks;
    class function GetThreadCount: Integer;
    class function GetActiveThreadCount: Integer;
    class procedure ClearGlobalObjects; virtual;
    class procedure NoNewThreads(aValue: Boolean);
    class procedure TerminateAll(aValue: Boolean = True);
    class procedure EnableStatic(aValue: Boolean);
    class procedure EnableDynamic(aValue: Boolean);
    class procedure EnableConvoluted(aValue: Boolean);
    class function GetSampleStepCount: Integer;
    class procedure ResetSampleStepCount;
  end;

implementation

type
  tGlobalObjectRecord = record
    fObj: iGCObject;
    fLock: Boolean; // used in Incremental GC mode
  end;

var
  gGlobalObjects: array [0..$FFF] of tGlobalObjectRecord;
  gGlobalObjectCount: Integer;
  gThreadCount: Integer;
  gActiveThreadCount: Integer;
  gTerminateAll: Boolean;
  gNoNewThreads: Boolean;
  gLastSweep: Cardinal; // used in Incremental GC mode
  gEnableStatic: Boolean = True;
  gEnableDynamic: Boolean = True;
  gEnableConvoluted: Boolean = True;
  gSampleThreadStep: Integer;

// used in Incremental GC mode
function InterlockedExchangeBool(aExpected, aNewValue: Boolean; var aVar: Boolean): Boolean;
asm
  LOCK CMPXCHG [ECX], DL
end;

// used in Incremental GC mode
procedure LockBool(var aLock: Boolean);
begin
  while InterlockedExchangeBool(False,True,aLock) do begin
    Sleep(0);
    if not InterlockedExchangeBool(False,True,aLock) then
      Break;
    Sleep(10);
  end;
end;

// used in Incremental GC mode
procedure UnlockBool(var aLock: Boolean);
begin
  InterlockedExchangeBool(True,False,aLock);
end;

procedure TerminateAll(aValue: Boolean);
begin
  gTerminateAll := aValue;
end;

function GetThreadCount: Integer;
begin
  Result := gThreadCount;
end;

procedure ClearGlobalObjects;
var
  lCount, lIdx: Integer;
begin
  lCount := High(gGlobalObjects);
  if lCount > gGlobalObjectCount then
    lCount := gGlobalObjectCount;
  gGlobalObjectCount := 0;
  for lIdx := 0 to lCount - 1 do
    SafeClearInterface(gGlobalObjects[lIdx].fObj);
end;

procedure NoNewThread(aValue: Boolean);
begin
  gNoNewThreads := aValue;
end;

{ tSampleThread }

class procedure tSampleThread.AddRandom(aParent, aChild: iGCObject);
begin
  if Assigned(aParent) and Assigned(aChild) then begin
    if aParent.GetObject is tGCObjectWithStaticFields then
      aParent.GetObject.Items[Random(3)+1] := aChild
    else if aParent.GetObject is tGCObjectWithDynamicFields then
      tGCObjectWithDynamicFields(aParent.GetObject).AddItem(aChild)
    else if aParent.GetObject is tGCConvoluted then
      aParent.GetObject.Items[Random(4096)] := aChild;
  end;
end;

procedure tSampleThread.AddRandomAndDoSomething(aParent: iGCObject);
var
  lObj: iGCObject;
begin
  if Assigned(aParent) then begin
    if aParent.GetObject is tGCObjectWithDynamicFields then begin
      lObj := tGCObjectWithDynamicFields(aParent.GetObject).AddItem;
      DoSomethingTo(lObj);
    end else begin
      lObj := GetNewRandomObject;
      if Assigned(lObj) then begin
        AddRandom(aParent,lObj);
        DoSomethingTo(lObj);
      end;
    end;
  end;
end;

procedure tSampleThread.AddStructure(aParent: iGCObject; aLevel: Integer);
var
  lObj: iGCObject;
begin
  while Random(3) > 0 do begin
    if aParent.GetObject is tGCObjectWithDynamicFields then begin
      if Random(2) = 0 then
        lObj := tGCObjectWithDynamicFields(aParent.GetObject).AddItem
      else begin
        lObj := tGCObjectWithStaticFields.Create(gcamFirstAssignment);
        AddRandom(aParent,lObj);
      end;
    end else begin
      if Random(2) = 0 then
        lObj := tGCObjectWithStaticFields.Create(gcamFirstAssignment)
      else
        lObj := tGCObjectWithDynamicFields.Create(gcamFirstAssignment);
      AddRandom(aParent,lObj);
    end;
    if aLevel < 4 then
      AddStructure(lObj,aLevel+1);
  end;
end;

class procedure tSampleThread.AssignGlobalObject(aIndex: Integer;
  aObj: iGCObject);
begin
  if tGCManager.GetDefault.UseLocks then
    SafeAssignInterface(gGlobalObjects[aIndex].fObj,aObj)
  else begin
    LockBool(gGlobalObjects[aIndex].fLock);
    try
      gGlobalObjects[aIndex].fObj := aObj;
    finally
      UnlockBool(gGlobalObjects[aIndex].fLock);
    end;
  end;
end;

class procedure tSampleThread.ClearGlobalObject(aIndex: Integer);
begin
  if tGCManager.GetDefault.UseLocks then
    SafeClearInterface(gGlobalObjects[aIndex].fObj)
  else begin
    LockBool(gGlobalObjects[aIndex].fLock);
    try
      gGlobalObjects[aIndex].fObj := nil;
    finally
      UnlockBool(gGlobalObjects[aIndex].fLock);
    end;
  end;
end;

procedure tSampleThread.ClearRandomGlobalObject;
var
  lIndex: Integer;
begin
  lIndex := Random(gGlobalObjectCount);
  if lIndex > High(gGlobalObjects) then
    lIndex := Random(High(gGlobalObjects)+1);
  ClearGlobalObject(lIndex);
end;

constructor tSampleThread.Create(aDelay: Integer);
begin
  fDelay := aDelay;
  FreeOnTerminate := True;
  inherited Create(False);
end;

class procedure tSampleThread.DeleteRandom(aObj: iGCObject);
var
  lIdx: Integer;
begin
  if aObj.GetObject is tGCObjectWithDynamicFields then begin
    lIdx := Random(aObj.GetObject.Count);
    tGCObjectWithDynamicFields(aObj.GetObject).Delete(lIdx);
  end else begin
    lIdx := Random(aObj.GetObject.Count);
    aObj.GetObject.Items[lIdx] := nil;
  end;
end;

procedure tSampleThread.DoCreateThread;
begin
  repeat
    tSampleThread.Create(Random(10000));
  until (GetThreadCount >= 128) or (Random(2) = 0);
end;

procedure tSampleThread.DoSomethingTo(aObj: iGCObject);
begin
  if Assigned(aObj) then
    case Random(16) of
      0: AddRandom(aObj,GetOrCreateRandomRoot);
      1: AddRandom(aObj,GetGlobal);
      2: AddRandom(GetOrCreateRandomRoot,aObj);
      3: AddRandom(GetGlobal,aObj);
      4: AddRandomAndDoSomething(aObj);
      5..8: DoSomethingTo(Walk(aObj));
      9: AddStructure(aObj,0);
      15: aObj.GetObject.RemoveOwner;
    else
      DeleteRandom(aObj);
    end;
  Sleep(1);
end;

procedure tSampleThread.Execute;
var
  lSleepTime: Integer;

  procedure ClearRoots;
  var
    lIdx: Integer;
  begin
    for lIdx := 0 to High(fRoots) do
      fRoots[lIdx] := nil;
  end;
begin
  InterlockedIncrement(gThreadCount);
  try
    try
      while not Terminated do begin
        if gTerminateAll then
          Terminate
        else if fDelay > 0 then begin
          if fDelay > 100 then begin
            Sleep(100);
            fDelay := fDelay - 100;
          end else begin
            Sleep(fDelay);
            fDelay := 0;
          end;
        end else begin
          lSleepTime := -1;
          InterlockedIncrement(gActiveThreadCount);
          tGCManager.GetDefault.SuspendSweep;
          try
            case Random(1024) of
              0:
                begin
                  ClearRoots;
                  Terminate;
                  if GetThreadCount = 1 then
                    Synchronize(DoCreateThread);
                end;
              1..64:
                DoSomethingTo(GetNewGlobal);
              65..256:
                DoSomethingTo(GetGlobal);
              257..512:
                DoSomethingTo(GetOrCreateRandomRoot);
              513..768:
                ClearRandomGlobalObject;
              1023:
                if GetThreadCount < 128 then
                  if not gNoNewThreads then
                    Synchronize(DoCreateThread);
            else
              lSleepTime := (Random(4) + 1)*(Random(2)*(Random(2)*(Random(2)*(Random(2) + 1) + 1) + 1) + 1);
            end;
          finally
            tGCManager.GetDefault.ResumeSweep;
            InterlockedIncrement(gSampleThreadStep);
            InterlockedDecrement(gActiveThreadCount);
          end;
          if lSleepTime >= 0 then
            Sleep(lSleepTime);
        end;
      end;
    finally
      tGCManager.GetDefault.SuspendSweep;
      try
        ClearRoots;
      finally
        tGCManager.GetDefault.ResumeSweep;
      end;
    end;
  finally
    InterlockedDecrement(gThreadCount);
  end;
end;

function tSampleThread.GetGlobal: iGCObject;
var
  lIndex: Integer;
begin
  lIndex := Random(gGlobalObjectCount) and High(gGlobalObjects);
  Result := GetGlobalObject(lIndex);
end;

class function tSampleThread.GetGlobalObject(aIndex: Integer): iGCObject;
begin
  if tGCManager.GetDefault.UseLocks then
    Result := gGlobalObjects[aIndex].fObj
  else begin
    LockBool(gGlobalObjects[aIndex].fLock);
    try
      Result := gGlobalObjects[aIndex].fObj;
    finally
      UnlockBool(gGlobalObjects[aIndex].fLock);
    end;
  end;
end;

function tSampleThread.GetNewGlobal: iGCObject;
var
  lIndex: Integer;
begin
  lIndex := InterlockedIncrement(gGlobalObjectCount) and High(gGlobalObjects);
  Result := GetNewRandomObject;
  AssignGlobalObject(lIndex,Result);
end;

function tSampleThread.GetNewRandomObject: iGCObject;
var
  lCount, lRandomMax, lRandom: Integer;
begin
  // Prevent Out of Memory exceptions:
  if tGCManager.GetDefault.ObjectCount > 1000000 then begin
    lCount := 128;
    while (tGCManager.GetDefault.ObjectCount > 100000) do begin
      Sleep(10);
      if Terminated then Break;
      if gTerminateAll then Break;
      Dec(lCount);
      if lCount = 0 then begin
        ClearRandomGlobalObject;
        Terminate;
        if GetThreadCount = 1 then
          Synchronize(DoCreateThread);
        Break;
      end;
    end;
  end;
  Result := nil;
  if not Terminated then begin
    lRandomMax := 0;
    if gEnableStatic then Inc(lRandomMax,4);
    if gEnableDynamic then Inc(lRandomMax,4);
    if gEnableConvoluted then Inc(lRandomMax);
    lRandom := Random(lRandomMax);
    if gEnableStatic then begin
      if lRandom < 4 then
        Result := tGCObjectWithStaticFields.Create(gcamFirstAssignment)
      else
        Dec(lRandom,4);
    end;
    if (Result = nil) then begin
      if gEnableDynamic and (lRandom < 4) then
        Result := tGCObjectWithDynamicFields.Create(gcamFirstAssignment)
      else
        Result := tGCConvoluted.Create(gcamFirstAssignment);
    end;
  end;
end;

function tSampleThread.GetOrCreateRandomRoot: iGCObject;
var
  lIndex: Integer;
begin
  lIndex := Random(16);
  Result := fRoots[lIndex];
  if not Assigned(Result) then begin
    Result := GetNewRandomObject;
    fRoots[lIndex] := Result;
  end;
end;

class function tSampleThread.Walk(aObj: iGCObject): iGCObject;
begin
  Result := aObj.GetObject.Items[Random(aObj.GetObject.Count)];
  if Assigned(Result) then
    if Random(2) = 0 then
      Result := Walk(Result);
end;

{ tSweepThread }

class procedure tSweepThread.ClearGlobalObjects;
begin
  SampleGCThread.ClearGlobalObjects;
end;

constructor tSweepThread.Create(aCreateSuspended: Boolean);
begin
  inherited Create(True);
  Priority := tpHigher;
  if not aCreateSuspended then Resume;
end;

class procedure tSweepThread.DecrementThreadCount;
begin
  InterlockedDecrement(gThreadCount);
end;

class procedure tSweepThread.EnableConvoluted(aValue: Boolean);
begin
  gEnableConvoluted := aValue;
end;

class procedure tSweepThread.EnableDynamic(aValue: Boolean);
begin
  gEnableDynamic := aValue;
end;

class procedure tSweepThread.EnableStatic(aValue: Boolean);
begin
  gEnableStatic := aValue;
end;

procedure tSweepThread.Execute;
var
  lTicks, lLastSweep: Cardinal;
begin
  TerminateAll(False);
  while not Terminated do begin
    tGCManager.GetDefault.LockBeforeSweep;
    try
      lTicks := GetTickCount;
      tGCManager.GetDefault.Sweep;
      lLastSweep := GetTickCount;
      lTicks := lLastSweep - lTicks;
    finally
      tGCManager.GetDefault.UnlockAfterSweep;
    end;
    gLastSweep := lLastSweep;
    fLastTicks := lTicks;
    if lTicks > fMaxTicks then
      fMaxTicks := lTicks;
    fTotTicks := fTotTicks + lTicks;
    Inc(fSweepCount);
    fAvgTicks := fTotTicks div fSweepCount;
    Sleep(10);
  end;
  TerminateAll();
  while GetThreadCount > 0 do begin
    tGCManager.GetDefault.LockBeforeSweep;
    try
      tGCManager.GetDefault.Sweep;
    finally
      tGCManager.GetDefault.UnlockAfterSweep;
    end;
    Sleep(10);
  end;
  ClearGlobalObjects;
  tGCManager.GetDefault.Sweep;
  if tGCManager.GetDefault.ObjectCount > 0 then begin
    Finalize(gGlobalObjects);
    tGCManager.GetDefault.Sweep;
  end;
end;

class function tSweepThread.GetActiveThreadCount: Integer;
begin
  Result := gActiveThreadCount;
end;

class function tSweepThread.GetNoNewThreads: Boolean;
begin
  Result := gNoNewThreads;
end;

class function tSweepThread.GetSampleStepCount: Integer;
begin
  Result := gSampleThreadStep;
end;

class function tSweepThread.GetTerminateAll: Boolean;
begin
  Result := gTerminateAll;
end;

class function tSweepThread.GetThreadCount: Integer;
begin
  Result := gThreadCount;
end;

class procedure tSweepThread.IncrementThreadCount;
begin
  InterlockedIncrement(gThreadCount);
end;

class procedure tSweepThread.NoNewThreads(aValue: Boolean);
begin
  gNoNewThreads := aValue;
end;

class procedure tSweepThread.ResetSampleStepCount;
begin
  gSampleThreadStep := 0;
end;

procedure tSweepThread.SetMaxTicks(aValue: Cardinal);
begin
  fMaxTicks := aValue;
end;

class procedure tSweepThread.TerminateAll(aValue: Boolean);
begin
  gTerminateAll := aValue;
end;

initialization
  Randomize;
end.
