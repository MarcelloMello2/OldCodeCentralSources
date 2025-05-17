@echo off

:: ---------------------------------------------------------------------
:: A *DELETE* registry entry Batch-file for the DC# Goodies assembly
:: (c) 2004 by Valentino Kyriakides 
:: ---------------------------------------------------------------------

:: ---------------------------------------------------------------------
:: Please NOTE that this batch script assumes, that the "dcsgoodies.dll"
:: assembly file, resides in the same directory as this batch file too.
::
:: Also make sure that you have general access to the Windows "reg.exe" 
:: file, since it will be used to make the needed registry entry. 
:: ---------------------------------------------------------------------

:: ---------------------------------------------------------------------
:: In most cases you will not need to change the settings below.
:: ---------------------------------------------------------------------
for %%x in (%0) do set CURRDIR=%%~dpsx
for %%x in (%CURRDIR%) do set CURRDIR=%%~dpsx
set DCSGOODYPATH=%CURRDIR%dcsgoodies.dll
set REGLOCATION="HKCU\Software\Borland\BDS\3.0\Known IDE Assemblies"

reg delete %REGLOCATION% /v %DCSGOODYPATH% /f