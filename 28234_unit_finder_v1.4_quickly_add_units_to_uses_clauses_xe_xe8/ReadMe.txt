Unit Finder ReadMe.txt
---- ------ ----------

Version 1.4


[What's New]

v1.4 - Tested on Delphi XE8
     - Now shows parse errors on a red error bar at the top of the search 
       dialog when your source code can not be understood by Unit Finder. 
     - Bug fix: if the unit you were working on was using a dotted namespace
       in its own unit name, both "Add to Interface" and "Add to 
       Implementation" buttons were mistakenly disabled.
     - Bug fix: for versions of Delphi without source code (such as trial 
       versions), the dcu files in Delphi's 'lib' folder were not made 
       available in the search.

------------------------------------------------------------------------------

v1.3 - Supports adding units to "program" pas files in adition to "unit" pas 
       files.
     - Maintains vertical uses clauses such as those typically found in 
       "program" pas files.
     - Prevents moving units to the implementation section if the unit already 
       exists in the interface uses clause.

v1.2a - Bug fix: Project pas files from the contains section of the Project 
        Manager were not being included in the search. 

v1.2 - Platform support for XE2 and up: See [Usage]
     - 'dcu' files are now part of the search results

v1.1 - Multiple Keyword Search: See [Usage]

v1.0 - Initial Release



[Introduction]

Unit Finder is a Delphi IDE add-on that makes finding and adding units to uses
clauses a breeze.
This has been tested on the following platforms:

- Delphi XE8
- Delphi XE6
- Delphi XE5
- Delphi XE

If you can't get Unit Finder to work on older Delphi versions, you might try
"Unit Expert" by EPocalipse software. This is the add-on that I had been using
since Delphi 6.  I was actually inspired (forced) to create this Unit Finder
because Unit Expert does not support Delphi versions beyond 2009.

Follow this link to download "Unit Expert" for older Delphi versions:
http://www.epocalipse.com/downloads.htm



[Installation]

Unit Finder is distributed as zip file of source code. The code is for a Delphi
design time package that can be installed into the IDE. Here are three easy
steps for installing Unit Finder into the IDE:

1) Unzip the contents of the zip file you downloaded into a folder
   ex. c:\MyDelphiCode\Common\DesignTime\UnitFinder

2) Run Delphi, Open the file UnitFinder.dpk

3) In the Project Manager, right-click the UnitFinder.bpl entry and select
   "Install"



[Usage]

Unit Finder lets you blast through the normally tedious chore of manually adding
units to uses clauses.  You may also open any unit in Delphi's search path just
as quickly.

Quickly Opening Units:

From anywhere in Delphi, press Ctrl+U, type a few characters, highlight the unit
using the down arrow key, and press enter. Any '.dcu' files in Delphi's search 
path that do not have a corresponding '.pas' file will be listed in a gray color 
and, of course, can not be opened.

Quickly Adding Units to a Uses Clause:

While you're in the middle of editing your code, press Alt+F11, type a few
characters, highlight the unit using the down arrow key, and press enter. The
section your cursor was in when you pressed Alt+F11 determines which uses clause
the unit will be added to.

Platform note:

Only units relevant to the current selected platform will be listed in the search
results.  (If you open Unit Finder without any open projects, all units available
to the IDE will be listed.)

Tip:

When you're entering characters in the search edit, you don't have to start with
the first characters of the unit name, you can enter characters that may appear
anywhere within the unit name.  For large projects, starting with the first
characters of a unit name can actually be unproductive. See the example below:

Take the case where you have a large project with hundreds of units.  Many unit
names probably begin with the same prefix, such as:

CustomerSupport 
CustomerBasicInfo
CustomerOrders
CustomerTransactions
CustomerReports
etc.

The best way to search for "CustomerTransactions" is to type something like
"cust tran" into the search.  Typing just "cust" by itself will return way too
many results.  The key is to use the word boundaries within your unit names to 
quickly narrow down the search results.



[Technical Details]

When you activate the Unit Finder using Alt+F11, it will automatically determine
which uses clause to add the new unit to based on where your current cursor
position is.  If you are editing the interface section, the unit will be added
to the interface uses clause, and vice versa for the implementation section.
You may override the decision made by Unit Finder by simply clicking the "Add to
Interface" or "Add to Implementaion" buttons after you've found your unit.

When adding to the interface uses clause, if the unit already is present in the 
implementation section, it will be moved up into the interface.  Moving in the
opposite direction is not done automatically.

Activating the Unit Finder with Ctrl+U will make the Open button become the
default action.  However, you still have access to the "Add to XXXX" buttons if
you change your mind and would like to add the unit instead of opening it.

The list of units available to unit finder comes from searching the following
paths for ".pas" and ".dcu" files. For XE2 and up, the currently selected platform 
in your project determines the actual paths used.

- the IDE's "Library Path" found in Tools|Options|Delphi Options|Library
- the IDE's "Browsing path" found in Tools|Options|Delphi Options|Library
- the active project's root folder
- the active project's "Contains" section

The keyboard shortcuts Ctrl+U and Alt+F11 can be changed in the Unit Finder
Setup menu item under the IDE's Tools menu.



[License and Disclaimer]

You are free to use, modify, distribute and re-distribute this source code at
will.  All I ask is that you give me credit if you use it in your own project.

The annoying disclaimer:
Unit Finder will modify your source code when you use the "Add to XXXX"
functions.  You agree to use Unit Finder at your own risk.  You agree that the
author will not be held responsible for any loss of data or code.  Backup your
code often with version control.



[Acknowledgements]

Unit Finder was inspired by the excellent "Unit Expert" by EPocalypse Software.
http://www.epocalipse.com/downloads.htm

The Open Tool API parts of Unit Finder were made possible by learning from
techniques used by the awesome GExperts.
http://www.gexperts.org/



[Comments and Feedback]

Ctrl+U (Unit Finder - Open) and Alt+F11 (Unit Finder - Add Unit) are among my
top five most used IDE functions. I expect they will become yours as well.  I
would say they rank up there with F9 (Run), Ctrl-Space (Code Completion), and
Ctrl+Shift+Space (Code Insight).

Here's my info in case you'd like to send any feedback:

Bill Friedrich
email: b<at>friedrich.st

The latest version of this code will always be in Code Central at
Embarcadero.com

