Delphi Compiler Message Expert
2/14/2002


What is it?
-----------

The Compiler Message Expert installs in the IDE and and displays 
context sensitive information based on messages produced by the compiler.
The message provide additional information on how to resolve various
compiler errors that may occur while using Delphi.  The expert
is particularly useful for upgrading old code to Delphi 6.0 as many 
of the common errors encountered while upgrading are covered by the 
expert.


Installation
------------
1. Unzip the files into your Delphi6\bin directory.
2. From a command prompt type:

   regedit MsgInfo.reg

  (This will add a registry key to the Known IDE Packages section of the
   Delphi registry key)

2. Start the IDE.

This will add a new item to the View menu call "Additional Message Info".
The window is dockable just like any other IDE window.

To remove the expert from the IDE simply delete the registry key from 
"HKEY_CURRENT_USER\Software\Borland\Delphi\6.0\Known IDE Packages".


Toolbar Buttons
---------------

The buttons on the toolbar of the expert window are as follows:

Download - downloads the latest ini file for use with the expert
Clear    - Clears the message list
View     - Displays the messages file using notepad
Copy     - Copies messages from the Message list to the clipboard


International Support
---------------------

Currently, the ini file containing the additional message information 
is available in English and French.  To use the French version of the
messages file simply copy it from the French directory stored in the 
zip file to your Delphi6\Bin directory.  As more languages become 
available additional files will be included.


Version History
---------------
1.1.0.7 - Support for internation Message ini files
        - Added French ini file as translated by F. Gaillard
1.1.0.3 - Minor bug fixes
1.0     - Initial release

============================================================
Copyright (c) 2002 Borland Software Corporation.
All rights reserved.
