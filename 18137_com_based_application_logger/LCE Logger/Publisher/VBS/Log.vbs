option explicit

''
'' Fire AppLog COM+ event
'' Written by Igor Zenkov
''

const emkError    = 0
const emkWarning  = 1
const emkInfo     = 2
const emkAuditOk  = 3
const emkAuditErr = 4

dim oAppLog

set oAppLog = CreateObject("AppLogEvents.AppLog")

oAppLog.Log "Log.vbs", "Hello", emkInfo
oAppLog.Log "Log.vbs", "You have a problem here", emkError
oAppLog.Log "Log.vbs", "You work too much", emkWarning



