{******************************************************************************}
{                                                                              }
{       Class Implementing Com Plus Interfaces - IObjectConstruct &            }
{       IObjectControl                                                         }
{       Author: Sri Ram Ambuga Nandakumar                                      }
{                                                                              }
{ Disclaimer                                                                   }
{ ----------                                                                   }
{                                                                              }
{ THIS FILES ARE PROVIDED "AS IS" AND WITHOUT WARRANTIES OF ANY KIND WHETHER   }
{ EXPRESSED OR IMPLIED.                                                        }
{                                                                              }
{ In no event shall the author be held liable for any damages whatsoever,      }
{ including without limitation, damages for loss of business profits,          }
{ business interruption, loss of business information, or any other loss       }
{ arising from the use or inability to use the unit.                           }
{******************************************************************************}
unit ComPlusObj;

interface

uses
  ActiveX, MtsObj, Mtx, Windows, ComObj;

type
  // Type library information of COMSVCS.DLL contains these.
  IObjectConstruct = interface(IUnknown)
    ['{41C4F8B3-7439-11D2-98CB-00C04F8EE1C4}']
    function Construct(const pCtorObj: IDispatch): HResult; stdcall;
  end;
  
  IObjectConstructString = interface(IDispatch)
    ['{41C4F8B2-7439-11D2-98CB-00C04F8EE1C4}']
    function Get_ConstructString: WideString; safecall;
    property ConstructString: WideString read Get_ConstructString;
  end;

type
  TComPlusObject = class(TMtsAutoObject, IObjectConstruct, IObjectControl)
  private
    FConstructString: string;
  protected
    function CanBePooled: Bool; stdcall;
    function Construct(const pCtorObj: IDispatch): HResult; stdcall;

    { This method has to be overriden in the dereived classes to decide whether
      the instance can be pooled or not }
    function OnCanBePooled: Boolean; virtual;

    { This method has to be overriden to do the initilization code for the
      instance. This is triggerd on create of the instance. }
    procedure OnConstruct(AConstructString: string); virtual;
    property ConstructString: string read FConstructString;
  end;

implementation

{ TComPlusObject }

function TComPlusObject.OnCanBePooled: Boolean;
begin
  { Default return value is True.. Be sure not to call 'inherited' after
    assigning the Result in the dereived class }
  Result := True;
end;

function TComPlusObject.CanBePooled: Bool;
begin
  Result := OnCanBePooled;
end;

function TComPlusObject.Construct(const pCtorObj: IDispatch): HResult;
var
  obj: IObjectConstructString;
begin
  obj := pCtorObj as IObjectConstructString;
  FConstructString := obj.Get_ConstructString;
  OnConstruct(FConstructString);
  Result := S_OK;
end;

procedure TComPlusObject.OnConstruct(AConstructString: string);
begin
end;

end.
