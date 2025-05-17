@echo off
echo MIDWARE for Delphi XE2 installation
echo ===================================
echo WARNING: You must first install ICS for Delphi XE2 to use Midware !
echo Unzip midware_xe2.zip in the same base folder as you installed ICS. Be sure to
echo restore the directory structure which is in the zip file.
echo Using Delphi XE2, open Install\DXe2Install.grouproj
echo From project manager, rebuild the two packages OverbyteMwDXe2Run (runtime) and
echo OverbyteMwDXe2Design (design time). Then right click OverbyteMwDXe2Design and
echo click on the menu item "install".
echo You should now see a lot of new components installed in "Overbyte Midware"
echo component page.
echo Add Delphi\Midware directory to your library search path.
echo 
echo You are ready to build and test all demos applications.
echo To do a quick test, build OverbyteSrvDemo and OverbyteBiolife demos.
echo Run OverbyteSrvDemo (server) and then run OverbyteBiolife (client).
echo Click "Request" button in OverbyteBiolife and you should see the nice fish :-)
echo The data are comming from Biolife.* files which are part of the installation.
echo.
pause
