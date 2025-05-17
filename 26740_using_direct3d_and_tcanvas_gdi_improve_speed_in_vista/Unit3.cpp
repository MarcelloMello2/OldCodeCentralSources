//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit3.h"
#include "D3DCntrlCanvas.h"


//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TForm3 *Form3;



TD3DControlCanvas *D3DCanvas;
//---------------------------------------------------------------------------
__fastcall TForm3::TForm3(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm3::Button1Click(TObject *Sender)
{
	if (D3DCanvas == NULL)
	   D3DCanvas = new TD3DControlCanvas();

	D3DCanvas->Control = Panel1;
	D3DCanvas->Pen->Color = clRed;
	PaintBox1->Canvas->Pen->Color = clRed;
	for(int y=0;y<1000;y=y+2)
	{
		for(int i =0;i<40;i++)
		{
			int x1 = rand() % 1000;
			int x2 = rand() % 40;
			PaintBox1->Canvas->Rectangle(x1,y,x1+x2,y+2);
			D3DCanvas->Rectangle(x1,y,x1+x2,y+2);
		}
	}
	D3DCanvas->D3DFlush();

	//Redirect output to another window...
	//D3DCanvas->D3DFlush(Panel2->Handle);

	//Redirect output to an AREA OF another window...
	//D3DCanvas->D3DFlush(TRect(0,0, 1000, 1000), Panel2->Handle);

	//Redirect output of an AREA to an AREA OF another window...
	//D3DCanvas->D3DFlush(TRect(0,0, 100, 100), TRect(0,0, 100, 100), Panel2->Handle);

}
//---------------------------------------------------------------------------

void __fastcall TForm3::ReleaseD3DCanvas()
{
	if (D3DCanvas != NULL)
	{
	  delete D3DCanvas;
	  D3DCanvas = NULL;
	}
}

void __fastcall TForm3::FormDestroy(TObject *Sender)
{
 ReleaseD3DCanvas();
}
//---------------------------------------------------------------------------

void __fastcall TForm3::Button2Click(TObject *Sender)
{
ReleaseD3DCanvas();
}
//---------------------------------------------------------------------------

