unit FormSettings;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

type
  TSaveToOption   = (stIniFile, stRegistry, stNone);
  TSaveTo         = TSaveToOption;

  TSavedSetting  = (ssFormSize, ssFormPosition);
  TSavedSettings = set of TSavedSetting;
  
  TSaveFormSettings = class(TPersistent)
  private
    FControl      : TControl;
    FFormSettings : TSavedSettings;
    FSaveTo       : TSaveTo;
    FIniFile      : string;
    FIniSection   : string;
    FRegPath      : string;
  protected
    property Control : TControl read FControl;
  public
    constructor Create(Control : TControl); virtual;
    procedure Assign(Dest : TPersistent); override;
  published
    property FormSettings : TSavedSettings read FFormSettings write FFormSettings;
    property SaveTo       : TSaveTo        read FSaveTo       write FSaveTo;
    property IniFile      : string         read FIniFile      write FIniFile;
    property IniSection   : string         read FIniSection   write FIniSection;
    property RegPath      : string         read FRegPath      write FRegPath;
  end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
implementation
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TSaveFormSettings.Create(Control : TControl);
begin
  inherited Create;
  FControl := Control;
  FSaveTo  := stNone;
end;

procedure TSaveFormSettings.Assign(Dest : TPersistent);
begin
  if (Dest is TSaveFormSettings) then
  begin
    with TSaveFormSettings(Dest) do
    begin
      FormSettings := Self.FFormSettings;
      SaveTo       := Self.FSaveTo;
      IniFile      := Self.FIniFile;
      IniSection   := Self.FIniSection;
      RegPath      := Self.FRegPath;
    end;
  end
  else
  begin
    inherited AssignTo(Dest);
  end;
end;

end.
 