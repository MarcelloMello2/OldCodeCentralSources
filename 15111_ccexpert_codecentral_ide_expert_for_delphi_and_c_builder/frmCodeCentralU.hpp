// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'frmCodeCentralU.pas' rev: 5.00

#ifndef frmCodeCentralUHPP
#define frmCodeCentralUHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <ActnList.hpp>	// Pascal unit
#include <ImgList.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Frmcodecentralu
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TfrmCodeCentral;
class PASCALIMPLEMENTATION TfrmCodeCentral : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TLabel* Label4;
	Stdctrls::TLabel* Label5;
	Stdctrls::TEdit* edtKeywords;
	Stdctrls::TEdit* edtSubmitter;
	Stdctrls::TEdit* edtHighVersion;
	Stdctrls::TEdit* edtLowVersion;
	Stdctrls::TEdit* edtShowMe;
	Buttons::TBitBtn* btnSearch;
	Actnlist::TActionList* ActionList1;
	Controls::TImageList* ImageList1;
	Actnlist::TAction* actSearch;
	Buttons::TBitBtn* btnClose;
	Stdctrls::TLabel* Label6;
	Stdctrls::TComboBox* cbProduct;
	Actnlist::TAction* actHome;
	Buttons::TBitBtn* btnWeb;
	void __fastcall FormClose(System::TObject* Sender, Forms::TCloseAction &Action);
	void __fastcall actSearchExecute(System::TObject* Sender);
	void __fastcall btnCloseClick(System::TObject* Sender);
	void __fastcall actSearchUpdate(System::TObject* Sender);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall actHomeExecute(System::TObject* Sender);
	
private:
	AnsiString FProduct;
	AnsiString FCCServer;
	void __fastcall SetProduct(const AnsiString Value);
	void __fastcall BrowseTo(AnsiString URL);
	
public:
	__property AnsiString Product = {read=FProduct, write=SetProduct};
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TfrmCodeCentral(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TfrmCodeCentral(Classes::TComponent* AOwner, 
		int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TfrmCodeCentral(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TfrmCodeCentral(HWND ParentWindow) : Forms::TForm(
		ParentWindow) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TfrmCodeCentral* frmCodeCentral;

}	/* namespace Frmcodecentralu */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Frmcodecentralu;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// frmCodeCentralU
