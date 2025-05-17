How to get WinForms 2 and WPF designers but stay afar from C#...

CodeGear drops Winforms. Therefore, RAD Studio 2007 supports .NET 2, 3 and 3.5 but it has not an integrated WinForms 2 or WPF designer.
What to do? Migrate to Visual Studio and C# ? Nahhh...
An other option does exist: separate the visual design from the coding and debug activities by using Visual C# as an external designer.
The detailed startup procedure for WinForm 2 is as below.

1 - Get Visual C# Express Edition (free at http://msdn.microsoft.com/vstudio/express/downloads/) and install it (there are no licensing restrictions for applications built using the Express Editions).

2 - Using Visual C# create a new assembly and add to it whatever WinForms you need (let's call them the 'design forms'). Build it.

3 - Using RAD Studio 2007 create and save a new Delphi.NET console application.

4 - Inside the .dpr file replace the code under the 'program' heading with this (ignore the warning on the recreation of the missing .res file)

  {$R *.res}

  uses
    System.Windows.Forms;

  begin
    Application.Run(<your main form>.Create);
  end.

5 - Inside the Delphi.NET application reference the C# assembly and whatever required system assemblies (typically System.dll, System.XML.dll and System.Windows.Forms.dll).

6 - Inside the Delphi.NET application subclass every design form from the C# assembly (let's call the derived forms 'editing forms').

7 - Build your Delphi application.

From now on to add components to the design forms and configure them as required use the Visual C# WinForms designer. Important: always set the component visibility to 'protected' (that is not the default).

After visual design build the C# assembly and revert to RAD Studio to add to the 'editing forms' any required fields, properties and methods. Event handlers must be manually added and linked (using Include) to the components, typically inside the 'editing forms' constructors.
If you use 3rd party components you must install them inside Visual C# and reference the assemblies inside Delphi.NET

The detailed startup procedure for WPF is a bit different because a XAML declared class can not be derived (strange but true, C++/CLI people are unhappy too) and an adapter is required (see WPFUtils unit).

1 - Get Visual C# Express Edition (free at http://msdn.microsoft.com/vstudio/express/downloads/) and install it (there are no licensing restrictions for applications built using the Express Editions).

2 - Using Visual C# create a new assembly and add to it whatever WPF window you need (let's call them the 'design forms'). Build it.

3 - Using RAD Studio 2007 create and save a new Delphi.NET console application.

4 - Inside the .dpr file replace the code under the 'program' heading with this (ignore the warning on the recreation of the missing .res file)

  {$R *.res}

  uses
    System.Windows;

  [STAThread]
  begin
    with System.Windows.Application.Create do begin
      EnableVisualStyle(Aero);
      Run(<your main form adapter>.Create.WPFObject);
    end;
  end.

5 - Inside the Delphi.NET application reference the C# assembly and whatever required system assemblies (typically System.dll, System.XML.dll and PresentationFramework.dll). The project search paths must include these paths (or the equivalent for your machine)

  C:\Windows\Microsoft.NET\Framework\v2.0.50727
  C:\Windows\Microsoft.NET\Framework\v3.0\WPF
  C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.0
  C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5

6 - Inside the Delphi.NET application for every design form from the C# assembly declare un adapter class derived from TWPFAdapter (let's call these classes 'editing forms').

7 - Build your Delphi application.

From now on to add components to the design forms and configure them as required use the Visual C# WPF designer. Important: always set the component visibility to 'public' (that is not the default) using the x:FieldModifier Attribute. For example a XAML button like this

  <Button Height="23" HorizontalAlignment="Left" Name="button1" ...

must be modified in

  <Button x:FieldModifier="protected" Height="23" HorizontalAlignment="Left" Name="button1" ...

After visual design build the C# assembly and revert to RAD Studio to add to the 'editing forms' any required fields, properties and methods. Event handlers must be manually added and linked (using Include) to the components, typically inside the 'editing forms' constructors.
If you use 3rd party components you must install them inside Visual C# and reference the assemblies inside Delphi.NET

The cons:
1 - You have to switch between Visual C# and RAD Studio to design the forms (but if you have a large enough screen you can have the designer and the form edit windows both open at the same time, this was not possible with the integrated designer).
2 - You have no designer support for the events and have to write and link them by code editing.

The pro:
1 - You never edit C# code, the designer do it, nor (hopefully) debug it.
2 - You can use now both WinForms and WPF designer.

Enclosed is are two sample projects generated using CodeGear RAD Studio 2007 Version 11.0.2804.9245 and Microsoft C# Express Edition Version 9.0.21022.8 and tested on Windows XP SP2 with .NET 2, 3 and 3.5 runtimes. The WinForms sample requires at least .NET 2, the WPF sample requires .NET 3.5







