/*
 Fire AppLog COM+ event
 Written by Igor Zenkov
*/

var emkError    = 0
var emkWarning  = 1
var emkInfo     = 2
var emkAuditOk  = 3
var emkAuditErr = 4

var oAppLog

oAppLog = new ActiveXObject("AppLogEvents.AppLog");

oAppLog.Log("Log.js", "Hello", emkInfo);
oAppLog.Log("Log.js", "You have a problem here", emkError);
oAppLog.Log("Log.js", "You work too much", emkWarning);
