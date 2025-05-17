//---------------------------------------------------------------------------

#ifndef Unit3H
#define Unit3H
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TForm3 : public TForm
{
__published:	// IDE-managed Components
	TPaintBox *PaintBox1;
	TButton *Button1;
	TPanel *Panel1;
	TPanel *Panel2;
	TButton *Button2;
	void __fastcall Button1Click(TObject *Sender);
	void __fastcall FormDestroy(TObject *Sender);
	void __fastcall Button2Click(TObject *Sender);
private:	// User declarations
   void __fastcall ReleaseD3DCanvas();
public:		// User declarations
	__fastcall TForm3(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm3 *Form3;
//---------------------------------------------------------------------------
#endif
