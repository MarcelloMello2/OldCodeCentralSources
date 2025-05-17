//---------------------------------------------------------------------------

#ifndef D3DCntrlCanvasH
#define D3DCntrlCanvasH

#include <Classes.hpp>

#include <Controls.hpp>
#include <Forms.hpp>
#include <Graphics.hpp>
#include <d3d9.h>


class TD3DControlCanvas : public Graphics::TCanvas
{
	typedef Graphics::TCanvas inherited;
private:
	TControl* FControl;
	HDC FDeviceContext;
	HWND FWindowHandle;
	LPDIRECT3DDEVICE9  D3DDeviceHandle;
	LPDIRECT3DSURFACE9 D3DBackSurface;

	void __fastcall SetControl(TControl* AControl);
	void __fastcall D3DCreateDevice();
protected:
	virtual void __fastcall CreateHandle(void);

public:
	__fastcall virtual ~TD3DControlCanvas(void);
	void __fastcall FreeHandle(void);
	void __fastcall UpdateTextFlags(void);
	void __fastcall D3DFlush(void);
	void __fastcall D3DFlush(HWND AWindowHandle);
	void __fastcall D3DFlush(TRect DestRect, HWND AWindowHandle);
	void __fastcall D3DFlush(TRect SrcRect, TRect DestRect, HWND AWindowHandle);

	__property TControl* Control = {read=FControl, write=SetControl};

public:
	#pragma option push -w-inl
	inline __fastcall TD3DControlCanvas(void): Graphics::TCanvas(), FDeviceContext(NULL),
	FControl(NULL),FWindowHandle(NULL),
	D3DDeviceHandle(NULL),D3DBackSurface(NULL){}
	#pragma option pop
};


//---------------------------------------------------------------------------
#endif
