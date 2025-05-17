{-----------------------------------------------------------------------------
The Initial Developer of the Original Code is Charles McAllister charles@avimark.net
Portions created by Charles McAllister are Copyright (C) 2005 Charles McAllister
All Rights Reserved.

Disclaimer:
This code and information are provided "As is" without warranty of any kind, either expressed or
implied, including but not limited to the implied warranties of merchantability and/or fitness
for a particular purpose, or non-infringement.

Contributor(s):

Description:
  A sample search utility that uses the TmcsDFMPersistentListParser class.

Known Issues:
  see frmDFMSearch.pas

}

program DFMSearch;

uses
  Forms,
  frmDFMSearch in 'frmDFMSearch.pas' {DFMSearchForm},
  umcsDFMParser in 'umcsDFMParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDFMSearchForm, DFMSearchForm);
  Application.Run;
end.
