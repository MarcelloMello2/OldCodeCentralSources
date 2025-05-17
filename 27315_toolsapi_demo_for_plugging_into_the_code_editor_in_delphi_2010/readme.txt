This is a simple package that uses new ToolsAPI interfaces introduced in the 2010 release to add
both top-level and bottom-level tabs into the code editor view in the IDE.

The pertinent interfaces are:

INTACustomEditorView
INTACustomEditorViewState
INTACustomEditorViewStatusPanel
INTACustomEditorSubView
IOTAEditorViewServices
INTAEditorServices

Open the project in the Delphi 2010 IDE, and install the package (right click the project in the
Project Manager and select the "Install" menu item).

After installing the package you will see the following:

Under the main IDE View menu, there will be a menu item called "Something Cool", that when selected,
does something that really isn't all that cool <g>.  It opens a top-level editor tab with a colorful
bouncing ball.....

Also, when you open a file that is saved on disk, there will be an additional tab at the bottom of
the editor (along side the Code, Design, and History tabs).  It is called "OS File Information" and
when you select it, you see a frame that displays the Creation time, Modified time, and the File Size.
There is also a combo box to select between different files contained in the module (.pas, .dfm, etc).
This tab does not use the virtual file system, so it will not show correct info for things not saved
to disk.