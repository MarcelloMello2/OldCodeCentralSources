//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("CCCExpert.res");
USEPACKAGE("vcl50.bpi");
USEFORMNS("frmCodeCentralU.pas", Frmcodecentralu, frmCodeCentral);
USEFORMNS("dmCodeCentralU.pas", Dmcodecentralu, dmCodeCentral); /* TDataModule: File Type */
USEPACKAGE("Inet50.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
