Borland Quality Insite
----------------------

Disclaimer
----------
This IDE add-in is provided AS-IS.  No warranty, actual or implied, is provided.
If you elect to install this add-in, please remember to backup all sensitive data
prior to installation.  Also, since the instructions include information on mod-
ifying the Windows Registry, it is a good idea to backup the Registry as well.

What is it?
-----------

This is an IDE add-in that adds automated reporting into Quality Central any 
exception that occurs in the Delphi 8 IDE.  Once this package is installed into 
Delphi 8 and an exception is raised either through the normal operation of the 
IDE or in the case of a latent defect, the user is presented with an enhanced 
exception message box that will display the internal stack trace of the code path 
for where the exception occurred.  The user will then have the option to send 
this report back to Borland as a Quality Central incident report.  Currently 
the user must be online in order to actually send the report.

NOTE: YOU MUST HAVE DELPHI 8 UPDATE 2 INSTALLED PRIOR TO INSTALLING THIS ADD-IN.

You may find the update here:

http://www.borland.com/products/downloads/download_delphi_net.html

Installation
------------

This add-in is manually installed.

1. Shut down Delphi 8 if it is running.
2. Ensure that the Delphi 8 Update 2 pack is installed*
3. Unzip the contents of the zip file into the Delphi 8 "bin" directory
   For example: c:\Program Files\Borland\BDS\2.0\bin
4. Run RegEdit and add the following value:

   [HKCU\Software\Borland\BDS\2.0\Known IDE Packages] 
   $(BDS)\bin\exceptiondiag71.bpl = "(untitled)"



*To Determining if you have Delphi 8 Update 2 installed, select Help|About.  You 
should see the following text in the dialog:

Borland(r) Delphi(tm) for Microsoft(r) .NET version 7.1.1518.31544 (Update 2)

