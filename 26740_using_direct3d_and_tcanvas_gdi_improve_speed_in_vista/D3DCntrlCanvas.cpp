//---------------------------------------------------------------------------
#pragma hdrstop
#include "D3DCntrlCanvas.h"


#pragma  link "d3d9.lib"

//---------------------------------------------------------------------------
LPDIRECT3D9       GlobalD3DHandle       = NULL;

#pragma package(smart_init)

void ShutdownDirect3d()
{
	#pragma exit ShutdownDirect3d 200
	if( GlobalD3DHandle != NULL )
		GlobalD3DHandle->Release();
}


void __fastcall TD3DControlCanvas::D3DCreateDevice()
{
	if( GlobalD3DHandle == NULL )
		GlobalD3DHandle = Direct3DCreate9( D3D_SDK_VERSION );

	if( GlobalD3DHandle == NULL )
	  throw Exception(L"Cannot Initialise Direct3D");
	if (D3DDeviceHandle == NULL)
		{
			D3DPRESENT_PARAMETERS d3dpp;
			memset(&d3dpp, 0, sizeof(d3dpp));
			d3dpp.BackBufferFormat       = D3DFMT_X8R8G8B8; //need this for getDC
			d3dpp.SwapEffect             = D3DSWAPEFFECT_COPY;//D3DSWAPEFFECT_DISCARD;
			d3dpp.hDeviceWindow          = FWindowHandle;
			d3dpp.Windowed               = true;
			d3dpp.Flags 				 = D3DPRESENTFLAG_LOCKABLE_BACKBUFFER;
			if( GlobalD3DHandle->CreateDevice( D3DADAPTER_DEFAULT,D3DDEVTYPE_HAL,NULL ,
					D3DCREATE_SOFTWARE_VERTEXPROCESSING, &d3dpp, &D3DDeviceHandle ) != D3D_OK)
				throw Exception(L"Cannot Create Direct3D DeviceHandle");
		}
}


void __fastcall TD3DControlCanvas::D3DFlush(void)
{
	FreeHandle();
	if (D3DDeviceHandle != NULL)
		 D3DDeviceHandle->Present( NULL, NULL, NULL, NULL );
};

void __fastcall TD3DControlCanvas::D3DFlush(HWND AWindowHandle)
{
	FreeHandle();
	if (D3DDeviceHandle != NULL)
		 D3DDeviceHandle->Present( NULL, NULL, AWindowHandle, NULL );
};

void __fastcall TD3DControlCanvas::D3DFlush(TRect DestRect, HWND AWindowHandle)
{
	FreeHandle();
	if (D3DDeviceHandle != NULL)
	 D3DDeviceHandle->Present(&DestRect,NULL, AWindowHandle, NULL );
};
void __fastcall TD3DControlCanvas::D3DFlush(TRect SrcRect, TRect DestRect,  HWND AWindowHandle)
{
	FreeHandle();
	if (D3DDeviceHandle != NULL)
		 D3DDeviceHandle->Present( &SrcRect, &DestRect, AWindowHandle, NULL );
};


void __fastcall TD3DControlCanvas::SetControl(TControl* AControl)
{
  if (FControl != AControl)
  {
	FreeHandle();
	FControl = AControl;
	if (dynamic_cast<TWinControl*>(FControl) != NULL)
	{
		 FWindowHandle = dynamic_cast<TWinControl*>(FControl)->Handle;
		 D3DCreateDevice();
	}
  }
};

void __fastcall TD3DControlCanvas::CreateHandle(void)
{
	if (FDeviceContext == NULL)
	{
		if (D3DBackSurface == NULL)
		   if (D3DDeviceHandle != NULL)
		   {
				D3DDeviceHandle->GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, &D3DBackSurface);
				int RGBValue = ColorToRGB(dynamic_cast<TWinControl*>(FControl)->Brush->Color);
				//clear the back buffer with a colour
				D3DDeviceHandle->Clear( 0, NULL, D3DCLEAR_TARGET ,D3DCOLOR_XRGB(GetRValue(RGBValue),
										  GetGValue(RGBValue),GetBValue(RGBValue)), 1.0f, 0 );
		   }
	  if (D3DBackSurface != NULL)
		 if( D3DBackSurface->GetDC(&FDeviceContext) != D3D_OK) FDeviceContext = NULL;
	  Handle = FDeviceContext;
	  UpdateTextFlags();
	}
};

__fastcall TD3DControlCanvas::~TD3DControlCanvas(void)
{
	FreeHandle();
	if (D3DBackSurface != NULL) D3DBackSurface->Release();
	if( D3DDeviceHandle != NULL ) D3DDeviceHandle->Release();
};

void __fastcall TD3DControlCanvas::FreeHandle(void)
{
  if(FDeviceContext != NULL)
	if (D3DBackSurface != NULL)
		 D3DBackSurface->ReleaseDC(FDeviceContext);
//	if (D3DBackSurface != NULL) D3DBackSurface->Release();
//  D3DBackSurface = NULL;
  FDeviceContext = NULL;
  Handle = NULL;
};

void __fastcall TD3DControlCanvas::UpdateTextFlags(void)
{
  if (Control == NULL) return;
  if (Control->UseRightToLeftReading())
	TextFlags = TextFlags | ETO_RTLREADING;
  else
	TextFlags = TextFlags & ~ETO_RTLREADING;

};

