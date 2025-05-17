The CodeCentral(tm) Expert for Delphi and C++ Builder
=====================================================

The CodeCentral(tm) Expert (CCExpert) is an IDE expert for searching
the CodeCentral web site, using the search conditions you enter on the search
form. After you enter your search request and hit the "Search" button, your
browser window will be opened and the CodeCentral search results will be
displayed.

This search expert is simply an IDE interface to the web search form that
can be found at http://codecentral.borland.com/codecentral/ccweb.exe/finder.
You may find the help text on that search form a little easier to read
than this document.

Installing CCExpert
-------------------
You are using this IDE expert at your own risk. John Kaster and Jeff Overcash
only provide support for this expert through comments on CodeCentral or
the CodeCentral newsgroup.

The complete source code for CCExpert is provided.

To install CCExpert into your Delphi IDE, open CCExpert.dpk, compile it, and
click "Install." To install CCExpert into your C++ Builder IDE, open
CCCExpert.bpk, compile it and click "Install."

Once it is successfully installed, you will have a new menu item in the Help
menu called CodeCentral.


Using CCExpert
--------------

If you highlight one or more components on a form before starting CCExpert,
the names of those components will automatically be entered as the keywords to
find in the CodeCentral repository. The expert will also attempt to
automatically detect which IDE you are using, and default the product for the
search to that IDE.

Simply fill in the values on the form and click the "Search" button. Any values
you specify beyond the default values help to restrict the search to a smaller
selection list.

If you want to go to the CodeCentral Home page, click the "Home" button.

Click the "Close" button to close the expert.

Search Form Values
------------------

The following values can be entered for searching:

Product
-------
The values you can select are:
 Any product
 Delphi
 C++ Builder
 JBuilder
 MIDAS
 Interbase
 BDE
 Turbo Pascal,
 Borland C++.

"Any Product" will search for a submission in any product category.

Keywords
--------

The keywords for every submission are determined by extracting the unique
"words" from the title and description of the entry. These "words" can be
alphanumeric, since this is a technical database. You can require words in
your search by using a plus (+) sign in front of the word. You can exclude
entries containing specific words in your search by using a minus (-) sign
in front of the word. The or (|) sign is optional, indicating words that may
appear in the search but are not required. You can put any number of keywords
you want in the keyword field. Keyword matching is case-insensitive. Any
keywords you submit are converted to lowercase first.

Wildcards are also supported for searching. The asterisk (*) is the wildcard
for one or more characters in the pattern match. The question mark (?) is
the wildcard for a single character

"+Linux +Database"
Only submissions that have both "Linux" and "Database" in their keyword list

"+OpenGL -raster*"
Only give me submissions that have "OpenGL" and don't have anything beginning
with "raster".

"COM DCOM ActiveX"
Give me submissions that have "COM", "DCOM" or "ActiveX" as keywords.

"+CORBA +Java"
Give me submissions that have both "CORBA" and "Java"

"+graph* -bitmap -metafile"
Give me anything that begins with "graph" but doesn't have "bitmap" or "metafile"

"+Delphi +graph*"
Give me submissions containing the word Delphi, and anything beginning
with "graph", like "graphics", "graph", "graphic", "graphing".

The "*" and "?" get translated to the equivalent SQL LIKE command characters,
so putting them at the start of a word pattern will slow down your search, as
no SQL index can be used for them.

Author
------

All entries are submitted by a specific CodeCentral user. If you want to search
for entries uploaded by a specific person, you can enter their name or email
address in this field.

You can enter:
- first name followed by last name (like "Frank Borland")
- last name, first name (like "Borland, Frank")
- email address (like "fborland@borland.com")

CodeCentral will attempt to match the value you put in this field with a
CodeCentral user, and give you only submissions uploaded by that user.

Low Version and High Version
----------------------------

All entries are submitted for a range of versions. If you want to search for
only one version of the product, enter it as both the low and high value for
the version. If you want to search for a range of versions, enter the low
value as the low version and the high value as the high version, and all
submissions that fall between those two versions will appear in your
search results. If you want to search for all versions of a product, use
zero (0) and zero (0) respectively.

Show Me
-------

Set this value to limit the number of results the search engine returns to you.


Source Code Notes
-----------------

In Delphi 5, the Help menu item names are (in order):

HelpContentsItem
HelpDelphiToolsItem
HelpWinSDKItem
HelpInprisePage
HelpBorlandCommunityPage
HelpDelphiPage
HelpProgramsPage
DirectItem
HelpProgGuideSeparator
HelpCustomizeItem
HelpAboutSeparator
HelpAboutItem


By default, CCExpert is inserted under the HelpBorlandCommunityPage menu
item. If you want to change it, simply modify the SSubMenu constant in 
TdmCodeCentral.DataModuleCreate().

Configuring CCExpert
--------------------

If you want to modify the location of the CodeCentral web server for any reason,
create a file in your package directory called "ccexpert.ini". It should
look like this:

[CodeCentral]
Server=http://codecentral.borland.com/codecentral/ccweb.exe

Where the value for "Server" is the new location for the CodeCentral web server.
