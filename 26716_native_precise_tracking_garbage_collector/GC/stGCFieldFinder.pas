{*******************************************************}
{                                                       }
{     StreamSec Security Library for CodeGear Delphi    }
{     Garbage Collection Class Definition Unit          }
{                                                       }
{     Copyright (C) 2009 StreamSec Handelsbolag         }
{     Commercial use requires permission                }
{                                                       }
{*******************************************************}
unit stGCFieldFinder;

interface

uses
  SysUtils, TypInfo, stGC;

type
  tGCStaticFieldDefinition = record
    fOffset: Integer;
  end;

  tGCFieldDefinitions = class;

  iGCFieldDefinitions = interface
    procedure IterateFields(aObject: Pointer; var aLast: pGCHeader);
    procedure CleanupFields(aObject: Pointer);
  end;

  tGCDynamicFieldDefinition = record
    fOffset: Integer;
    fSize: Integer;
    fElement: iGCFieldDefinitions;
  end;

  tGCFieldDefinitions = class(TInterfacedObject,iGCFieldDefinitions)
  private
    fClassID: IntPtr;
    fStaticFields: array of tGCStaticFieldDefinition;
    fDynamicFields: array of tGCDynamicFieldDefinition;
    function ParseArray(aTypeInfo: PTypeInfo; aBaseOffset, aCount: Integer): Boolean;
    function ParseRecord(aTypeInfo: PTypeInfo; aBaseOffset: Integer): Boolean;
    function ParseClass(aTypeInfo: PTypeInfo): Boolean;
    class function GetOrCreateDefinitions(aClass: TClass): tGCFieldDefinitions;
    procedure IterateFields(aObject: Pointer; var aLast: pGCHeader);
    procedure CleanupFields(aObject: Pointer);
    class procedure FreeAllDefinitions;
  public
    constructor CreateSub;
    constructor Create(aClass: TClass);
    class procedure GetFields(aObject: TObject; var aLast: pGCHeader);
    class procedure Cleanup(aObject: TObject);
  end;

implementation

type
  tFieldInfo = packed record
    fTypeInfo: PPTypeInfo;
    fOffset: Integer;
  end;

  pFieldTable = ^tFieldTable;
  tFieldTable = packed record
    fSize: Integer;
    fCount: Integer;
    fFields: array [0..0] of tFieldInfo;
  end;

function FieldTableFromTypeInfo(aTypeInfo: PTypeInfo): pFieldTable;
begin
  Result := pFieldTable(IntPtr(aTypeInfo) + Byte(aTypeInfo.Name[0]) + 2);
end;

function CRC16(aID: IntPtr): Word;
const
  cShift = 16;
  cDivisor: Cardinal = $80050000;
var
  lIdx: Integer;
begin
  for lIdx := 1 to cShift do begin
    if aId < 0 then
      Cardinal(aId) := Cardinal(aID) xor cDivisor;
    Cardinal(aID) := Cardinal(aID) + Cardinal(aID);
  end;
  Result := aID shr cShift;
end;

function GCHeaderToObject(aInst: pGCHeader): TObject;
begin
  Result := Pointer(IntPtr(aInst) + SizeOf(tGCHeader));
end;

function ObjectToGCHeader(aObj: TObject): pGCHeader;
begin
  Result := Pointer(IntPtr(aObj) - SizeOf(tGCHeader));
end;

var
  gHashTable: array [0..$FFFF] of array of tGCFieldDefinitions;

{ tGCFieldDefinitions }

class procedure tGCFieldDefinitions.Cleanup(aObject: TObject);
begin
  try
    GetOrCreateDefinitions(aObject.ClassType).CleanupFields(aObject);
  finally
  end;
end;

procedure tGCFieldDefinitions.CleanupFields(aObject: Pointer);
var
  I, J, lLen, lSize: Integer;
  lObj, lObj2: Pointer;
  lElem: iGCFieldDefinitions;
begin
  for I := 0 to Length(fStaticFields) - 1 do try
    PPointer(IntPtr(aObject) + fStaticFields[I].fOffset)^ := nil;
  finally
  end;
  for I := 0 to Length(fDynamicFields) - 1 do try
    lObj := PPointer(IntPtr(aObject) + fDynamicFields[I].fOffset)^;
    if Assigned(lObj) then begin
      lLen := PInteger(IntPtr(lObj) - 4)^;
      lSize := fDynamicFields[I].fSize;
      lElem := fDynamicFields[I].fElement;
      for J := 0 to lLen - 1 do begin
        if Assigned(lElem) then begin
          lObj2 := Pointer(IntPtr(lObj) + J*lSize);
          lElem.CleanupFields(lObj2);
        end else begin
          PPointer(IntPtr(lObj) + J*lSize)^ := nil;
        end;
      end;
    end;
  finally
  end;
end;

constructor tGCFieldDefinitions.Create(aClass: TClass);
var
  lCRC: Word;
  lIdx, lLen: Integer;
begin
  fClassID := IntPtr(aClass);
  while Assigned(aClass) do begin
    ParseClass(PPointer(IntPtr(aClass) + vmtInitTable)^);
    aClass := aClass.ClassParent;
  end;
  lCRC := CRC16(fClassID);
  lLen := Length(gHashTable[lCRC]);
  SetLength(gHashTable[lCRC],lLen + 1);
  lIdx := lLen;
  while lIdx > 0 do begin
    if gHashTable[lCRC][lIdx-1].fClassID > fClassID then
      gHashTable[lCRC][lIdx] := gHashTable[lCRC][lIdx-1]
    else
      Break;
    Dec(lIdx);
  end;
  gHashTable[lCRC][lIdx] := Self;
end;

constructor tGCFieldDefinitions.CreateSub;
begin

end;

class procedure tGCFieldDefinitions.FreeAllDefinitions;
var
  I, J: Integer;
begin
  for I := 0 to High(gHashTable) do
    for J := 0 to Length(gHashTable[I]) - 1 do
      FreeAndNil(gHashTable[I][J]);
end;

class procedure tGCFieldDefinitions.GetFields(aObject: TObject;
  var aLast: pGCHeader);
begin
  GetOrCreateDefinitions(aObject.ClassType).IterateFields(aObject,aLast);
end;

class function tGCFieldDefinitions.GetOrCreateDefinitions(
  aClass: TClass): tGCFieldDefinitions;
var
  lCRC: Word;
  lIdx: Integer;
begin
  lCRC := CRC16(IntPtr(aClass));
  Result := nil;
  lIdx := 0;
  while lIdx < Length(gHashTable[lCRC]) do begin
    Result := gHashTable[lCRC][lIdx];
    if Result.fClassID = IntPtr(aClass) then Break;
    Result := nil;
    Inc(lIdx);
  end;
  if Result = nil then
    Result := tGCFieldDefinitions.Create(aClass);
end;

procedure tGCFieldDefinitions.IterateFields(aObject: Pointer;
  var aLast: pGCHeader);
var
  I, J, lLen, lSize: Integer;
  lObj, lObj2: Pointer;
  lInst: pGCHeader;
  lElem: iGCFieldDefinitions;
begin
  for I := 0 to Length(fStaticFields) - 1 do begin
    lObj := PPointer(IntPtr(aObject) + fStaticFields[I].fOffset)^;
    if Assigned(lObj) then begin
      lInst := ObjectToGCHeader(iGCField(lObj).GetInstance);
      if lInst.fNextLive = nil then begin
        lInst.fNextLive := aLast.fNextLive;
        aLast.fNextLive := lInst;
        aLast := lInst;
      end;
    end;
  end;
  for I := 0 to Length(fDynamicFields) - 1 do begin
    lObj := PPointer(IntPtr(aObject) + fDynamicFields[I].fOffset)^;
    if Assigned(lObj) then begin
      lLen := PInteger(IntPtr(lObj) - 4)^;
      lSize := fDynamicFields[I].fSize;
      lElem := fDynamicFields[I].fElement;
      for J := 0 to lLen - 1 do begin
        if Assigned(lElem) then begin
          lObj2 := Pointer(IntPtr(lObj) + J*lSize);
          lElem.IterateFields(lObj2,aLast);
        end else begin
          lObj2 := PPointer(IntPtr(lObj) + J*lSize)^;
          if Assigned(lObj2) then begin
            lInst := ObjectToGCHeader(iGCField(lObj2).GetInstance);
            if lInst.fNextLive = nil then begin
              lInst.fNextLive := aLast.fNextLive;
              aLast.fNextLive := lInst;
              aLast := lInst;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function tGCFieldDefinitions.ParseArray(aTypeInfo  : PTypeInfo;
                                        aBaseOffset,
                                        aCount     : Integer)
                                                   : Boolean;
var
  lFT: pFieldTable;
  lTD: PTypeData;
  lIdx: Integer;
  lElem: tGCFieldDefinitions;
  lSize: Cardinal;
begin
  Result := False;
  if (aCount = 0) or not Assigned(aTypeInfo) then Exit;
  case aTypeInfo.Kind of
    tkArray:
      begin
        lFT := FieldTableFromTypeInfo(aTypeInfo);
        while aCount > 0 do begin
          Result := ParseArray(lFT.fFields[0].fTypeInfo^,aBaseOffset,lFT.fCount);
          Inc(aBaseOffset,lFT.fSize);
          Dec(aCount);
        end;
      end;
    tkRecord:
      begin
        lFT := FieldTableFromTypeInfo(aTypeInfo);
        while aCount > 0 do begin
          Result := ParseRecord(aTypeInfo,aBaseOffset);
          Inc(aBaseOffset,lFT.fSize);
          Dec(aCount);
        end;
      end;
    tkInterface:
      while Assigned(aTypeInfo) do begin
        lTD := GetTypeData(aTypeInfo);
        if CompareMem(@lTD.Guid,@IID_GCFieldBase,SizeOf(TGUID)) then begin
          Result := True;
          lIdx := Length(fStaticFields);
          SetLength(fStaticFields,lIdx + aCount);
          while aCount > 0 do begin
            fStaticFields[lIdx].fOffset := aBaseOffset;
            Inc(aBaseOffset,SizeOf(Pointer));
            Inc(lIdx);
            Dec(aCount);
          end;
          Break;
        end else begin
          if Assigned(lTD.IntfParent) then
            aTypeInfo := lTD.IntfParent^
          else
            aTypeInfo := nil;
        end;
      end;
    tkDynArray:
      begin
        lTD := GetTypeData(aTypeInfo);
        if Assigned(lTD.elType) then begin
          if lTD.elType^.Kind = tkInterface then begin
            lSize := SizeOf(Pointer);
            lTD := GetTypeData(lTD.elType^);
            if CompareMem(@lTD.Guid,@IID_GCFieldBase,SizeOf(TGUID)) then begin
              Result := True;
              lIdx := Length(fDynamicFields);
              SetLength(fDynamicFields,lIdx + aCount);
              while aCount > 0 do begin
                fDynamicFields[lIdx].fOffset := aBaseOffset;
                fDynamicFields[lIdx].fSize := lSize;
                fDynamicFields[lIdx].fElement := nil;
                Inc(aBaseOffset,lSize);
                Inc(lIdx);
                Dec(aCount);
              end;
            end;
          end else begin
            lElem := tGCFieldDefinitions.CreateSub;
            if lElem.ParseArray(lTD.elType^,0,1) then begin
              Result := True;
              lIdx := Length(fDynamicFields);
              SetLength(fDynamicFields,lIdx + aCount);
              while aCount > 0 do begin
                fDynamicFields[lIdx].fOffset := aBaseOffset;
                fDynamicFields[lIdx].fSize := lTD.elSize;
                fDynamicFields[lIdx].fElement := lElem;
                Inc(aBaseOffset,lTD.elSize);
                Inc(lIdx);
                Dec(aCount);
              end;
            end else
              lElem.Free;
          end;
        end;
      end;
  end;
end;

function tGCFieldDefinitions.ParseClass(aTypeInfo: PTypeInfo): Boolean;
begin
  Result := False;
  if Assigned(aTypeInfo) then
    Result := ParseRecord(aTypeInfo,0);
end;

function tGCFieldDefinitions.ParseRecord(aTypeInfo  : PTypeInfo;
                                         aBaseOffset: Integer)
                                                    : Boolean;
var
  lFT: pFieldTable;
  I: Cardinal;
begin
  Result := False;
  lFT := FieldTableFromTypeInfo(aTypeInfo);
  for I := 0 to lFT.fCount-1 do
    if ParseArray(lFT.fFields[I].fTypeInfo^,aBaseOffset+lFT.fFields[I].fOffset,1) then
      Result := True;
end;

initialization
finalization
  tGCFieldDefinitions.FreeAllDefinitions;
end.
