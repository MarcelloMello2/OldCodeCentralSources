unit BCSDirSizedp;

interface

uses
  Classes, Forms, Graphics, SysUtils, BCSDirSizewbu;

type
  TBCSDirSizedp = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    /// Form Color
    FColor: TColor;
    /// Input Directory
    FDirectory: string;
    /// List of Directory Strings;
    FDirectoryStrings: TStringList;
    /// Show Directory Size True = Skow Size False = Do Not Show Size
    FShowSize: Boolean;
  public
    { Public declarations }
  published
    { Published declarations }
    function Execute: Boolean;
    /// Color Property
    property RColor: TColor Read FColor Write FColor;
    /// Input Directory Property
    property RDirectory: string read FDirectory write FDirectory;
    /// Collection Of Directory Strings
    property RDirectoryStrings
      : TStringList read FDirectoryStrings write FDirectoryStrings;
    /// Include Directory Size
    property RShowSize: Boolean read FShowSize write FShowSize;
  end;

procedure Register;

implementation

function TBCSDirSizedp.Execute: Boolean;
begin
  Result := True;
  Application.CreateForm(TBCSDirSizeC, BCSDirSizec);
  BCSDirSizeC.RColor := $00AAE3FF;
  BCSDirSizeC.RDirectory := RDirectory;
  BCSDirSizeC.RShowSize := True;
  BCSDirSizeC.RShowSize := False;
  BCSDirSizeC.LoadLists;
  RDirectoryStrings := BCSDirSizeC.RDirectoryStrings;
  BCSDirSizeC.Free;
end;

procedure Register;
begin
  RegisterComponents('AB Comps', [TBCSDirSizedp]);
end;

end.
