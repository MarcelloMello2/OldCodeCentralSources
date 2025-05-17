unit DelphiRecurseFiles_fmMainRegex;

{ Prepared for Code Rage, Ocotober 2010 
 by Ann Lynnworth, HREF Tools Corp.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    edtFilter: TEdit;
    edtPath: TEdit;
    Panel2: TPanel;
    Memo1: TMemo;
    CheckBoxUseRegEx: TCheckBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Presets1: TMenuItem;
    N0windowsBPL1: TMenuItem;
    N1Passource1: TMenuItem;
    N2ProjectLogs1: TMenuItem;
    N3aa1: TMenuItem;
    N4aaregex1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure N0windowsBPL1Click(Sender: TObject);
    procedure N1Passource1Click(Sender: TObject);
    procedure N2ProjectLogs1Click(Sender: TObject);
    procedure N3aa1Click(Sender: TObject);
    procedure N4aaregex1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
  private
    { Private declarations }
    procedure Preset(const i: Integer);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  IOUtils, RegularExpressions;


procedure TForm2.Exit1Click(Sender: TObject);
begin
  Self.Close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Preset(99);
end;

procedure TForm2.N0windowsBPL1Click(Sender: TObject);
begin
  Preset(0);
end;

procedure TForm2.N1Passource1Click(Sender: TObject);
begin
  Preset(1);
end;

procedure TForm2.N2ProjectLogs1Click(Sender: TObject);
begin
  Preset(2);
end;

procedure TForm2.N3aa1Click(Sender: TObject);
begin
  Preset(3);
end;

procedure TForm2.N4aaregex1Click(Sender: TObject);
begin
  Preset(4);
end;

procedure TForm2.Preset(const i: Integer);
begin
  case i of
    0:
    begin
      edtPath.Text := 'c:\windows\system32';
      edtFilter.Text := '*.bpl';
      CheckBoxUseRegEx.Checked := False;
    end;
    1:
    begin
      edtPath.Text := 'D:\Apps\Embarcadero\RADStudio\8.0\source';
      edtFilter.Text := '*.pas';
      CheckBoxUseRegEx.Checked := False;
    end;
    2:
    begin
      edtPath.Text := 'd:\sandbox\samplefiles';
      edtFilter.Text := '?roject*.test.20*.*';
      CheckBoxUseRegEx.Checked := False;
    end;
    3:
    begin
      edtPath.Text := 'd:\sandbox\samplefiles';
      edtFilter.Text := 'a?.txt';
      CheckBoxUseRegEx.Checked := False;
    end;
    4:
    begin
      edtPath.Text := 'd:\sandbox\samplefiles';
      edtFilter.Text := 'a[abd]\.txt';
      CheckBoxUseRegEx.Checked := True;
    end;
    5:
    begin
      edtPath.Text := 'h:\';
      edtFilter.Text := '.+GUI\..[^f].';
      CheckBoxUseRegEx.Checked := False;
    end;
    99:
    begin
      edtPath.Text := '';
      edtFilter.Text := '';
      CheckBoxUseRegEx.Checked := False;
    end;
  end;
  if Self.Visible then
  begin
    Memo1.Clear;
    Button1.SetFocus;
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Filespec : string;
  FilterPredicate : TDirectory.TFilterPredicate;
  Directory: string;
  FlagFoundAny: Boolean;
  RE: TRegEx;   // uses RegularExpressions unit in Delphi XE
  x: Integer;
begin

// Malcolm Groves published a tutorial on recursing through files
// at http://www.malcolmgroves.com/blog/?p=447
// His version is in Code Central #28023

// This version uses a memo instead of a listbox
// and optionally matches files with regex

  edtPath.Text := IncludeTrailingPathDelimiter(edtPath.Text);

  if not TDirectory.Exists(edtPath.Text) then
    Caption := 'Invalid Path'
  else
    Caption := 'Searching ' + edtPath.Text;

  Memo1.Clear;

  FilterPredicate := function(const Path: string; const SearchRec: TSearchRec)
                     : Boolean
                     begin
                       if CheckBoxUseRegEx.Checked then
                         Result := Re.IsMatch(SearchRec.Name, edtFilter.Text)
                       else
                         Result := TPath.MatchesPattern(SearchRec.Name,
                           edtFilter.Text, False);
                     end;

  FlagFoundAny := False;
  Memo1.Text := 'starting';
  Application.ProcessMessages;
  x := Length(edtPath.Text);

  Memo1.Clear;

  // do the parent folder first, separately
  for Filespec in TDirectory.GetFiles(edtPath.Text, FilterPredicate) do
  begin
    Memo1.Lines.Add(Format('File = %s', [Copy(Filespec, X+1, MaxInt)]));
    FlagFoundAny := True;
  end;

  // now all the contained folders
  for Directory in TDirectory.GetDirectories(edtPath.Text,
    TSearchOption.soAllDirectories, nil) do
  begin
    for Filespec in TDirectory.GetFiles(Directory, FilterPredicate) do
    begin
      Memo1.Lines.Add(Format('File = %s', [Copy(Filespec, X+1, MaxInt)]));
      FlagFoundAny := True;
    end;
  end;

  if NOT FlagFoundAny then
    Memo1.Lines.Add('0 found');
end;

end.
