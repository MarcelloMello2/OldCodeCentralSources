Delphi 2009 has finally moved to Unicode, so the standard String type is now a Unicode string and Char is a wide character (16 bit). One of the victims of this change is the good old practice of validating input by using constructs like

  if Key In ['0'..'9'] then
    ....

Since a set in Delphi can at most have 256 members sets of Char are no longer possible, instead the set referenced above will be compiled as a set of Ansichar (TSysCharset) and the code (with Key typed as Char) will generate a warning. It will still work as long as Key is in the range #0 to #127 (the 7-bit ASCII range) but the results are basically undefined if this is not the case. 

So, what can we use as equivalent? Delphi offers the CharInSet function that will at least suppress the warning but it will also unconditionally return False for all  characters above #127. That may serve for US americans but certainly not for the rest of the world <g>.

The WidecharSetU unit supplied in this archive aims to solve this problem by implementing the equivalent of a set of widechar. The syntax in using these is necessarily a bit different from using the classic sets, since there is no build-in compiler support for my "widechar set" types and the language also does  not support operator overloading on interface types (which I have choosen as the base for the implementation). 

A typical test like 

  if Key In ['0'..'9'] then
    ....

would have to be rewritten as

  if Digits.Contain(Key) then
    ...

This makes use of one of the predefined widechar sets. These sets are singletons, so using them has little overhead. If you need to create your own sets you do this using the WidecharSets factory:

  if Key in ['0'..'9', 'a'..'z', 'A'..'Z', '$'] then
     ...

would turn into  

  if Widecharsets.Create(['0'..'9', 'a'..'z', 'A'..'Z', '$']).Contains(Key) then

Since this would create a new widechar set for each call it would be more effient to create the set once and keep it for the lifetime of the object that contains the  if test in one of its methods. The constructor (or the OnCreate event for a form) would be a good place for that:

  // object field
  FAllowedChars: IWidecharSet;

  // constructor call
  FAllowedChars := Widecharsets.Create(['0'..'9', 'a'..'z', 'A'..'Z', '$']);

The test would then turn into

   if FAllowedChars.Contains(Key) then

Since the lifetime of widechar sets is controlled via interface reference counting you don't need to do anything to make sure the memory used by them is properly released when they go out of scope.

Creating a widechar set using a set of ANSI characters as input also works if the set contains characters above #127. The Widecharsets.Create method that takes a set as input (you can also create widechar sets from Unicode strings or other widechar sets) takes an optional encoding parameter that defines the codepage for the characters in the passed set of ansichar. This encoding object is used to convert the ANSI characters above #127 in the input set to Unicode characters (UTF-16) for inclusion into the widechar set. 

Widechar sets (and the factory for them) support the full range of set operations. Check the HTML help file in the archive for the members of IWidecharSet (representing a widechar set) and IWidecharSets (the factory interface returned by the WidecharSets function).

The code units in this archive carry no usage restrictions, use as you please (other than selling them!), but there are also no warranties implied or explicit. I have a set of unit tests for the code (included in the archive) and am pretty sure that it will work as advertised, though. The code uses the JEDI.INC file from the JEDI code library (JCL, see http://delphi-jedi.org) for compiler directives. If you use the units in this archive with a Delphi version above 2010 be sure to get an updated version of this file, otherwise the code may not identify the compiler version it is compiled with correctly! The JCL has its own distribution licence, see the comment at the start of JEDI.INC.

Have fun!
Peter Below



  