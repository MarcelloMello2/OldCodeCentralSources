unit BCSSelDirdp;

interface

uses
  Classes, Forms, Graphics, BCSSelDirwbu, JclFileUtils, SysUtils;

type
  TBCSSelDirdp = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    /// Dialog Caption
    FCaption: string;
    /// Form Color
    FColor: TColor;
    /// Selected Directory
    FSelDir: string;
  public
    { Public declarations }
  published
    { Published declarations }
    /// Dialog Caption Property
    property RCaption: string read FCaption write FCaption;
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
    /// Select Directory Property
    property RSelDir: string read FSelDir write FSelDir;
    /// Primary Execution Fucntion
    function Execute: Boolean;
  end;

procedure Register;

implementation

function TBCSSelDirdp.Execute: Boolean;
begin
  Result := True;
  Application.CreateForm(TBCSSelDirC, BCSSelDirc);
  BCSSelDirc.RColor := RColor;
  BCSSelDirc.RCaption := RCaption;
  BCSSelDirc.RSelDir := '';
  BCSSelDirc.SelectDirectoryDialog.Title := RCaption;
  RSelDir := '';
  if BCSSelDirc.SelectDirectoryDialog.Execute then
  begin
    RSelDir := PathAddSeparator(BCSSelDirc.SelectDirectoryDialog.FileName);
  end;
  BCSSelDirc.Free;
end;

procedure Register;
begin
  RegisterComponents('AB Comps', [TBCSSelDirdp]);
end;

end.
