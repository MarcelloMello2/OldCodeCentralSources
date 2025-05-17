Type       : How-To tutorial.
Subject    : Application logger using COM+ event service
Author     : Igor P. Zenkov
Date       : Sep 29, 2001
Last update: Oct 15, 2001

  The purpose of this tutorial is to show how-to implement Application
  logger using COM+ event system.

Scenario:

  1. COM client creates instance of COM+ event object.
  2. Client gets reference to IAppLog interface.
  3. By calling Log method of IAppLog interface client fires an event.
  4. SubWinLog subscriber writes event into event log service.
  5. SubFileLog subscriber writes event into file.


IDL:


    interface IAppLog : IDispatch {
        [id(0x00000001)]
        HRESULT Log(
                        [in] BSTR App, 
                        [in] BSTR Msg, 
                        [in] eMsgKind Kind);
    };

    typedef [uuid(AA21C46C-60CF-4EE0-8005-EC61B107D254), version(1.0)]
    enum {
        emkError    = 0,
        emkWarning  = 1,
        emkInfo     = 2,
        emkAuditOk  = 3,
        emkAuditErr = 4
    } eMsgKind;
