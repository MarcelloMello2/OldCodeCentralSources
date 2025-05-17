unit Smart_Install;

{

Copyright (c) 2001 by Alexander Rodygin
rodigin@yahoo.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software") to use, copy, publish, distribute or sell copies of the Software
without modifications, and to permit persons to whom the Software is furnished
to do so, subject to the following conditions:

THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL Alexander Rodygin BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellApi, Buttons, ExtCtrls, DsgnIntf, Smart_Form;

type
  TSmartFormAboutDlg = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    Label3: TLabel;
    procedure Label3Click(Sender: TObject);
  end;

type
  TSmartFormAboutProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
  end;

type
  TSmartFormCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

type
  TSmartFormCategoryClass = class of TSmartFormCategory;

procedure Register;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
implementation
{$R *.DFM}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

procedure Register;
begin
  RegisterCustomModule(TSmartForm, TCustomModule);
  RegisterPropertyEditor(TypeInfo(TSmartFormAbout), TSmartForm, '', TSmartFormAboutProperty);
  RegisterPropertiesInCategory(TSmartFormCategory, TSmartForm, ['AboutSmartForm', 'SavedComponents',
                               'SavedProperties', 'SaveName', 'Storage', 'OnLoadBegin', 'OnLoadComponentBegin',
                               'OnLoadComponentEnd', 'OnLoadEnd', 'OnSaveBegin',
                               'OnSaveComponentBegin', 'OnSaveComponentEnd', 'OnSaveEnd']);
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{ TSmartFormAboutProperty }

procedure TSmartFormAboutProperty.Edit;
begin
  with TSmartFormAboutDlg.Create(Application) do
  try
    ShowModal
  finally
    Release
  end;
end;

function TSmartFormAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog, paFullWidthName];
end;

function TSmartFormAboutProperty.GetName: string;
begin
  Result:= 'About Smart Forms';
end;

{ TSmartFormCategory }

class function TSmartFormCategory.Description: string;
begin
  Result:= 'Smart Form properties';
end;

class function TSmartFormCategory.Name: string;
begin
  Result:= 'Smart Form';
end;

procedure TSmartFormAboutDlg.Label3Click(Sender: TObject);
var
  Dest: array[0..255] of Char;
begin
  ShellExecute(Handle,'open',StrPCopy(Dest,'mailto:'+Label3.Caption),nil,nil,SW_SHOW);
end;

end.
 