unit BorlandProjectSettings;

interface

uses
  Commons.Settings,
  System.Xml.Serialization;

type
  TOptionInfo = record
  public
    [System.Xml.Serialization.XmlAttribute]
    Name: string;
    [System.Xml.Serialization.XmlText]
    Value: string;
  end;

  TOptionInfoArray = array of TOptionInfo;

  TFileInfo = record
  public
    [System.Xml.Serialization.XmlAttribute]
    FileName: string;
    [System.Xml.Serialization.XmlAttribute]
    ContainerId: string;
    [System.Xml.Serialization.XmlAttribute]
    ModuleName: string;
    [System.Xml.Serialization.XmlAttribute]
    AssemblyName: string;
    [System.Xml.Serialization.XmlAttribute]
    Version: string;
    [System.Xml.Serialization.XmlAttribute]
    CopyLocal: string;
    [System.Xml.Serialization.XmlAttribute]
    LinkUnits: string;
    [System.Xml.Serialization.XmlAttribute]
    Parent: string;
  end;

  TDelphiDotNet = class (TObject)
    [XmlArrayItem('Source')]
    Source: TOptionInfoArray;
    [XmlArrayItem('FileVersion')]
    FileVersion: TOptionInfoArray;
    [XmlArrayItem('Compiler')]
    Compiler: TOptionInfoArray;
    [XmlArrayItem('Linker')]
    Linker: TOptionInfoArray;
    [XmlArrayItem('Directories')]
    Directories: TOptionInfoArray;
    [XmlArrayItem('Parameters')]
    Parameters: TOptionInfoArray;
    [XmlArrayItem('Language')]
    Language: TOptionInfoArray;
    [XmlArrayItem('VersionInfo')]
    VersionInfo: TOptionInfoArray;
    [XmlArrayItem('VersionInfoKeys')]
    VersionInfoKeys: TOptionInfoArray;
    [XmlArrayItem('File')]
    FileList: array of TFileInfo;
  end;

  TOptionsSet = record
    [System.Xml.Serialization.XmlAttribute]
    Name: string;
    [System.Xml.Serialization.XmlArrayItem('Options')]
    Options: TOptionInfoArray;
  end;

  TOptionsSetArray = array of TOptionsSet;

  TCSharpOptions = record
    [XmlArrayItem('SelectedOptionSet')]
    SelectedOptionSet: TOptionInfoArray;
    [System.Xml.Serialization.XmlElement('OptionsSet')]
    OptionsSet: TOptionsSetArray;
  end;

  TCSharp = class (TObject)
    [System.Xml.Serialization.XmlElement('Options')]
    Options: TCSharpOptions;
    [XmlArrayItem('File')]
    FileList: array of TFileInfo;
  end;

  [XmlRoot(ElementName = 'BorlandProject',IsNullable = true)]
  TBorlandProject = class (TSettingsObject)
  public
    [System.Xml.Serialization.XmlAnyElement]
    More: TObject;
    [System.Xml.Serialization.XmlElement('DelphiDotNet.Personality')]
    DelphiDotNet: TDelphiDotNet;
    [System.Xml.Serialization.XmlElement('CSharp.Personality')]
    CSharp: TCSharp;
  public
    function GetOption(Group: TOptionInfoArray; const Name: string; const DefaultValue: string): string;
    function GetBooleanOption(Group: TOptionInfoArray; const Name: string; const DefaultValue: Boolean): Boolean;
  end;

implementation

{ TBorlandProject }

function TBorlandProject.GetOption(Group: TOptionInfoArray; const Name: string; const DefaultValue: string): string;
var
  Option: TOptionInfo;
begin
  Result := DefaultValue;
  for Option in Group do
    if System.string.Compare(Option.Name,Name,True) = 0 then begin
      Result := Option.Value;
      Exit;
    end;
end;

function TBorlandProject.GetBooleanOption(Group: TOptionInfoArray; const Name: string; const DefaultValue: Boolean): Boolean;
var
  Value: string;
begin
  Value := GetOption(Group,Name,'');
  if (System.String.Compare(Value,'true',True) = 0) or
     (System.String.Compare(Value,'1',True) = 0) then
    Result := True
  else
    Result := DefaultValue;
end;


end.
