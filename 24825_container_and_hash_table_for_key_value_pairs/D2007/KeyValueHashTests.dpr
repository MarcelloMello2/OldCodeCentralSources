program KeyValueHashTests;
{

  Delphi DUnit-Testprojekt
  -------------------------
  Dieses Projekt enthält das DUnit-Test-Framework und die GUI/Konsolen-Test-Runner.
  Zum Verwenden des Konsolen-Test-Runners fügen Sie den konditinalen Definitionen  
  in den Projektoptionen "CONSOLE_TESTRUNNER" hinzu. Ansonsten wird standardmäßig 
  der GUI-Test-Runner verwendet.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestKeyValuePairU in '..\COMMON\TestKeyValuePairU.pas',
  KeyValueStoreTestU in '..\COMMON\KeyValueStoreTestU.pas',
  NodeStoreTestU in '..\COMMON\NodeStoreTestU.pas',
  PrimesTestU in '..\COMMON\PrimesTestU.pas',
  HashtableTestU in '..\COMMON\HashtableTestU.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

