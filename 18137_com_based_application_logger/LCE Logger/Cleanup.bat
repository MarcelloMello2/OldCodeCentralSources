@echo off

echo This batch script will delete all temporary files,
echo to make distribution as small as possible.
echo .
echo Press Ctrl+Break to cancel,
pause

echo .
echo Delphi ...
echo .
del *.~dpr /s > Cleanup.log
del *.~pas /s >> Cleanup.log
del *.~dfm /s >> Cleanup.log
del *.~tlb /s >> Cleanup.log
del *.dcu /s >> Cleanup.log
echo .

pause
