// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'dmCodeCentralU.pas' rev: 5.00

#ifndef dmCodeCentralUHPP
#define dmCodeCentralUHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Menus.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dmcodecentralu
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TdmCodeCentral;
class PASCALIMPLEMENTATION TdmCodeCentral : public Forms::TDataModule 
{
	typedef Forms::TDataModule inherited;
	
__published:
	Menus::TPopupMenu* mnuCodeCentral;
	Menus::TMenuItem* miCodeCentral;
	void __fastcall DataModuleCreate(System::TObject* Sender);
	void __fastcall miCodeCentralClick(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TDataModule.Create */ inline __fastcall virtual TdmCodeCentral(Classes::TComponent* AOwner) : Forms::TDataModule(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDataModule.CreateNew */ inline __fastcall virtual TdmCodeCentral(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TDataModule(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TDataModule.Destroy */ inline __fastcall virtual ~TdmCodeCentral(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TdmCodeCentral* dmCodeCentral;
extern PACKAGE System::ResourceString _SAnyProduct;
#define Dmcodecentralu_SAnyProduct System::LoadResourceString(&Dmcodecentralu::_SAnyProduct)
extern PACKAGE void __fastcall Register(void);

}	/* namespace Dmcodecentralu */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dmcodecentralu;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// dmCodeCentralU
