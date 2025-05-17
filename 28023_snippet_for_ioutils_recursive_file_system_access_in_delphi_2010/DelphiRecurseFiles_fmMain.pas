unit DelphiRecurseFiles_fmMain;

// Malcolm Groves published a tutorial on this topic
// at http://www.malcolmgroves.com/blog/?p=447
// This code represents the end-point of his explanation
// and that is probably the starting-point of your own example.

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    edtFilter: TEdit;
    edtPath: TEdit;
    Panel2: TPanel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  IOUtils;

procedure TForm2.FormCreate(Sender: TObject);
begin
  edtPath.Text := 'c:\windows';
  edtFilter.Text := '*.bpl';
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Filespec : string;
  FilterPredicate : TDirectory.TFilterPredicate;
  Directory: string;
begin

  if not TDirectory.Exists(edtPath.Text) then
    Caption := 'Invalid Path'
  else
    Caption := 'Searching ' + edtPath.Text;

  ListBox1.Clear;

  FilterPredicate := function(const Path: string; const SearchRec: TSearchRec)
                     : Boolean
                     begin
                       Result := (TPath.MatchesPattern(SearchRec.Name,
                         edtFilter.Text, False)) {AND
                                 (SearchRec.Attr = faArchive)};
                     end;

  for Directory in TDirectory.GetDirectories(edtPath.Text,
    TSearchOption.soAllDirectories, nil) do
  begin
    for Filespec in TDirectory.GetFiles(Directory, FilterPredicate) do
      Listbox1.Items.Add(Format('Filespec = %s', [Filespec]));
  end;

end;

end.
