Last updated:   4/11/04
Author:         Sergey Mishkovskiy
Company:        USysWare, Inc.
Contact info:   usysware@comcast.net
Compatibility:  Borland Delphi for .NET (Update Pack 2 or higher)
Description:    Borland Delphi for .NET Templates add-in
Version:        1.0.0.11


*** Summary
*** Feature Requests & Bug Reports
*** How It Works
*** Template xml File Layout
*** Installation/Uninstallation
*** File List
*** Future Development Plans


*** Summary

The Templates add-in has been designed to aid with project and unit level 
add-in creation. Instead of writing a new add-in for every project or unit,
you create a configuration file for your project or unit template and setup 
Templates add-in to use that new template. Templates are used to extend 
Delphi's File|New dialog, allowing you to create new projects or units based
on configured templates.

The Templates add-in also demonstrates the BDS IDE project and unit add-in 
implementation. It includes the following OTA interfaces implementation:
IOTAProjectWizard, IOTAProjectGroupCreator, IOTAProjectCreator and 
IOTAModuleCreator. It also accesses the following OTA services: 
IOTAAboutBoxService, IOTASplashScreenService, IOTAWizardService, 
IOTAModuleServices, IOTAGalleryCategoryManager and IOTADotNetProject.

This distribution includes Windows Service, OTA Menu Project and several 
Design Patterns templates:

* Windows Service Application Template
  (File|New|Other|Other Projects|...) - creates a skeleton of the 
  .NET Windows Service application.

* OTA Menu Project Template
  (File|New|Other|Other Projects|...) - creates a skeleton of the 
  menu-based OTA project. The OTA project is setup to add a single menu item 
  to Delphi's main menu and show a message box when that item is selected.

* Design Patterns Templates and Demos
  (File|New|Other|Design Patterns|... and 
   File|New|Other|Design Patterns|Demos|...) - creates singleton and observer
   design patterns classes. It also creates singleton and observer 
   demo applications.


*** Feature Requests & Bug Reports

Please send your comments, feature requests or bug reports to 
usysware@comcast.net . When contacting me please indicate the version of the
Templates add-in you currently use.

The latest version of the source code can be downloaded at:
http://codecentral.borland.com/codecentral/ccweb.exe/listing?id=21008

The latest version of the setup executable can be downloaded at:
http://codecentral.borland.com/codecentral/ccweb.exe/listing?id=21009


*** How It Works

Each template is described via xml configuration file. The template's full 
configuration path is stored in Templates add-in's registry key as 
a registry key value name (similar to how BDS stores add-ins under 
Known IDE Assemblies key). The data for that value is set to the name of 
your template folder (it must be just the folder name without the path). 
The template folder must be located in the same folder where the 
template XML configuration file resides.

After Delphi loads Templates add-in, the add-in retrieves the list of all 
configured templates from under 

HKEY_CURRENT_USER\Software\USysWare\BDSTemplates\1.0\Templates

key, and parses each xml configuration file. It then adds File|New dialog 
items based on the XML file(s) configured information. When the user invokes 
the add-in's File|New item, the Templates add-in creates an appropriate 
project or unit(s) based on the template's configured options and configured 
unit list. Units, including the project file if there is one, are copied from 
the appropriate/configured template folder.


*** Template xml File Layout

Template xml file consists of several major sections/tags: <addins>, <units>
<references> and <dialog>. Multiple <addin> tags are supported under <addins>
one, where each <addin> tag represents a separate File|New dialog item.

<addin> tag's attributes describe the add-in File|New item properties.
Notably, the 'menuCategory' attribute describes the node of the File|New
dialog the item will be added to. The category name could either be one of
the predefined IDE constants, or your own string. If your own category string
is used then it'll be added to the list of all available categories. You can
use '\' character to separate sub-categories in your own category string. Some
of the valid BDS menu category constants are listed in the xml files included
with this distribution. 'name' attribute describes the text that will be shown
in the File|New dialog. 'imageFile' attribute specifies the icon file that will
be associated with the File|New item. If 'imageFile' attribute value is left
blank or indicates an invalid file name then no icon will be loaded and no
error message will be shown. The IDE's default icon will be used in that case.
If 'imageFile' attribute value doesn't include path to the icon file then
add-in's template path is added to the icon file name.

Under <addin> tag you have <units> tag that lists all of the units the 
add-in consists of. You must specify at least one <unit> tag for your add-in.
There you have another tag called <references> to list all of the add-in  
assembly references. Notice that <references> tag is optional. The <reference>
tag 'name' attribute may include $(BDS) constant in front of the file name. 
That constant is automatically resolved to Borland's assembly path (typically 
it is something like ..\BDS\2.0\Bin).

Optional <dialog> tag allows you to define a configuration dialog parameters
for your add-in. The dialog's shown right after user selects the add-in from 
the File|New dialog. The dialog must have a title specified via 'title' 
attribute. The dialog can be temporarily disabled by setting an optional 
'enabled' attribute to 'false'. The dialog must have to have at least one 
<group> tag defined. Each group shows up as a separate tab on the dialog's 
page control. Each group must have a unique name set via 'name' attribute. 
Group's 'caption' attribute is used for its page control tab text. Group may 
also have an optional 'enabled' attribute. When set to to true, the 
corresponding page control tab is not shown. There must be at least one 
enabled group defined for the dialog.

The Templates add-in support project and unit level add-ins. Both appear as 
new items in File|New dialog. Project level add-ins start a new project add 
all their configured units to the new project. Where's unit level add-ins 
add all of their configured units to the current project, if one is opened.
If no project is open at the time then units are simply opened in the IDE 
without any project relation.

<units> tag could have one or more <unit> sub-tags. Each <unit> tag describes 
the file/unit the add-in template consists of. 'name' attribute describes the 
unit or project name as it appears in the source code. 'template' attribute 
describes the actual file name under template folder to be used to create that 
unit. Notice that it must not include the file path. Template files with .dpr
or .dpk extension are designated as a project type units. There can't be more 
than one project unit defined per add-in. If there are no project type units
defined for the add-in then the add-in considered to be a unit level add-in.
And the last attribute available for the <unit> tag is 'type'. 'type' 
attribute allows you specify the unit type as either 'application', 'library'
'package', 'unit', 'class' or 'text'. 'text' unit types are considered as 
passthrough type and those units are simply copied. 'text' type should be used
for .txt or .xml files included with the add-in but is not limited to that.
It could be used for binary files as well.

Under each <unit> tag you could have multiple <expression> tags defined.
<expression> tag allows you to define a 'replace' attribute value to be used
to replace all occurrences of that value in the unit with the value defined
in 'value' attribute. Notice that multiple <expression> tags have to be
defined under <expressions> one. Also notice that <expressions> tag is
optional when unit has no customization of any kind. The Templates add-in
recognizes '[!ProgramName]', '[!UnitName]' and '[!TimeStamp]' expressions and
automatically replaces them with an appropriate <unit> tag 'name' attribute
value or current time stamp appropriately. If 'value' attribute is absent from
the expression definition then 'unit' one is expected to be defined instead.
The expression must always have a value to replace the expression with. That 
value has to either come from the 'value' attribute or from another unit that 
has a value with the same name defined.

<expression> tag may have a single <groupItem> sub-tag defined. The 
<groupItem> tag is a link to <dialog>/<group> tag, which tells the add-in to
make that expression available for customization under that particular group.
The group link is established via <groupItem> tag's 'group' attribute, which
has to match to an existing group 'name' attribute. The 'caption' attribute 
is used as label in front of the edit box populated with the expression's 
value.

Important note about modifying the xml template files while the Templates 
add-in is loaded into the IDE:

<addin> tag attributes are only cashed once at the IDE startup. If you make
any changes to the <addin> tag attributes while the Templates add-in is loaded 
into the IDE, you'll have to restart Delphi in order for the changes to take 
effect. The <units> and <references> tags along with their sub-tags on the 
other hand are parsed every time you invoke File|New add-in item. Therefore 
you can keep tweaking the add-in template files (.dpr and .pas) as well as 
references without having to restart the IDE. That should prove to be very 
useful while fine-tuning your add-in templates.


*** Installation/Uninstallation

To install Templates add-in, you can either build the setup script and run it,
or add a registry key to 

HKEY_CURRENT_USER\Software\Borland\BDS\2.0\Known IDE Assemblies

key. The name should be set to the full path to BDSTemplates.dll (including 
the file name). The data for that value could be set to any non-blank value. 
In addition to that you'll need to setup add-in templates. You'd use a 

HKEY_CURRENT_USER\Software\USysWare\BDSTemplates\1.0\Templates

key for that. The procedure is outlined in How It Works section. The 
process is not straight forward and I'd recommend using the setup application 
until you become more familiar with the procedure.

Please note that Inno Setup 4.0.10 or higher with ISPP is required (a.k.a. 
QuickStart Pack) if you wish to compile the setup script. Inno Setup is a 
great free application and can be download at http://www.jrsoftware.org .


*** File List

BDSTemplates files:

* BDSTemplates.bdsproj
* BDSTemplates.dpr
* BDSTemplatesMain.pas
* BDSConfig.pas
* BDSOTAUtils.pas
* BDSDefaultsForm.*


Windows Service Application Template files:

* WinService.xml
* WinService\*.*


OTA Menu Project Template files:

* OTAMenuProject.xml
* OTAMenuProject\*.*


Design Patterns files:

* DesignPatterns.xml
* DesignPatterns\*.*


Setup files:

* BDSTemplates.iss
* BDSTemplates Readme.txt
* BDSTemplates License.txt


Miscellaneous files:

* readme.txt


*** Future Development Plans

* Add support for <groupItem> tag 'required', 'spacesAllowed', 'minimumLength'
  and 'maximumLength' attributes to be used in customization dialog.
* Convert the add-in and templates to support Borland C# Builder.
