{==============================================================================}
{ Copyright 1998-2000, Musson Franck. All Rights Reserved.                     }
{ Ces composants peuvent être librement utilisés et/ou distribués              }
{ pour un usage privé.                                                         }
{==============================================================================}
{ Sorry for my poor english                                                    }
{ NOT TESTED WITH DELPHI 4                                                     }
{ FULLY TESTED WITH DELPHI 3 AND DELPHI 5                                      }
{==============================================================================}
{ NO WARRANTIES ARE EXTENDED. USE AT YOUR OWN RISK.                            }
{                                                                              }
{ To contact the author with suggestions or comments, use                      }
{ franck.musson@wanadoo.fr                                                     }
{==============================================================================}



    "Monikers intelligent and Persistent Names" Kraig Brokschimdt



1.0.0.0 : Initial Release (24/01/2000)
  Demo :
  - you need to compile the inprise sample "MemoEdit"
    C:\Program Files\Borland\Delphi 3\Demos\OLEAUTO\AUTOSERV\MEMOEDIT.DPR (D3)
    C:\Program Files\Borland\Delphi 5\Demos\Activex\Oleauto\Autoserv\MEMOEDIT.DPR (D5) 

  - 3 Ways to get a Memoedit Instance

    a) The Server Call CreateClassRefMoniker and Get the string representing the ClassID 
       to Instanciate with GetMonikerDisplayName and send this string back to the client (directly or        
       written in an asp page for example).

    b) The Server Call CreateClassUrlRefMoniker and Get the string representing 
       the ClassID to instanciate with GetMonikerDisplayName and send this string 
       back to the client (directly or written in an asp page for example).

    C) The Server Call CreateObjectRefMoniker and Get the string representing the 
       Running Object passed in the Obj parameter with GetMonikerDisplayName and 
       send this string back to the client (directly or written in an asp page for example).
       (The Object Passed must be Running if the client want to be able to bind It)

    c+) The Server Call any MS Bultin Root moniker creation function and Get the string         
       representing the moniker with GetMonikerDisplayName and send this string back to the         
       client (directly or written in an asp page for example).

    d) the client has only to call CoBindComObject function to get an instance.
       Here Monikers do their magic.
       - "CoCreateInstance Like" (Local Or Remoted) in the A case
       - "CoCreateInstance Like" (Local) with eventually a download operation in the B case
       - A Bind on a Running Object in the C case.

    Moniker and persistence Interfaces are fully Implemented 
 
    I Want to Thanks Don Box and Chris Sells for their samples found in MSDN library.


1.0.0.1 : (03/02/2000)
  Corrections :

  - Function CreateClassRefMoniker(Const rclsid: TGUID; PLocInfo: PMKLocalizationInfo; 
                                   PKey: PMKCryptInfo; Out Mk: IMoniker): HResult; Stdcall;
    Access violation : if PLocinfo or PKey was passed with nil value

  - Function CreateUrlRefMoniker(Const rclsid: TGUID; PUrlInfo: PMKUrlInfo; 
                                 PKey: PMKCryptInfo; Out Mk: IMoniker): HResult; Stdcall;
    Access violation : if PUrlInfo or PKey was passed with nil value

  - TComClassMonikerFactory, TComObjectMonikerFactory, TComUrlMonikerFactory modified for delphi 3
    ThreadingModel Added for registration

  - Function CoBindComObject(Const DisplayName: WideString; unkOuter: IUnknown; Const IID: TGUID; Out Obj): HResult; Stdcall;
    unkOuter Parameter Added For
    a) Aggregation (not for Remote Objects)
    b) passing an IObjectContext (MTS or COM+ server side transactions) 
    C) passing an ITransactionContextEx (MTS or COM+ client side transactions)

  Added :
  - Support For MTS and COM+ (uMtsObj, uMtx form Borland's MtsObj and Mtx) also for D3
  - TComPlusAutoObject

  Removed :
  - Demo Executables (for size)
