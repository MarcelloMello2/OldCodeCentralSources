XML IDE Expert
--------------

This demo creates an XML menu under in Edit item in main menu with following submenus:

Format Document: (XML, XSD)
---------------------------
Formats XML document in the editor with two spaces indentation.

View Data: (XML)
----------------
Openes XML document from the editor in separate window displayed as DataSet
tables (if possible) in DataGrid. It can also transorm DATAPACKET format to
DataSet, including nested tables.

Validate Document: (XML, XSD)
-----------------------------
Validates XML document or schema in the editor using XmlValidatingReader or
XmlSchema classes. Errors/Warnings are displayed in Message window.

Validate using Schema: (XML)
----------------------------
Validates XML document against selected XML schema from Open File dialog.
Errors/Warnings are displayed in Message window.

Generate Classes: (XSD)
-----------------------
Generates classes for XML Schema in the editor using XSD.EXE command-line
tool and Delphi CodeDom Provider.

Generate Schema: (XML)
----------------------
Generates XML schema for given XML document in the editor using XSD.EXE 
command-line tool.


The expert also automatically tries to format an XML document with a text line 
longer than 4095 characters because the code editor is not able to work 
with it.

To install: run regedit and add a new text entry to 
  HKEY_CURRENT_USER\Software\Borland\BDS\3.0\Known IDE Assemblies

The name for the text entry should be the full path to this demo, for example:
  c:\program files\borland\bds\3.0\demos\OTA\XMLSupport\XMLOTAExpert.dll

The value of the new entry should be "(Untitled)".