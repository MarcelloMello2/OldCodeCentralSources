unit RADStudioProject;

interface

uses
  Commons.Settings,
  System.Xml.Serialization;

const
  ItemProject = 0;
  ItemDirectory = 1;
  ItemDelphiRes = 2;
  ItemRes = 3;
  ItemPasSource = 4;
  ItemPasFormSource = 5;
  ItemCsSource = 6;
  ItemCsFormSource = 7;
  ItemAspx = 8;

type          
  ProjectPropertyGroupConfiguration = class
  public
    [XmlAttribute]
    Condition: string;
    [XmlText]
    Value: string;
  end;

  ProjectPropertyGroup = class
  public
    OutputType: string;
    OutputPath: string;
    TargetName: string;
    Configuration: ProjectPropertyGroupConfiguration;
    DCC_DependencyCheckOutputName: string;
    [XmlAttribute]
    Condition: string;
  end;

  TArrayOfProjectPropertyGroup = array of ProjectPropertyGroup;

  ProjectItemGroupEmbeddedResource = class
  public
    DependentUpon: string;
    [XmlAttribute]
    Include: string;
  end;

  ProjectItemGroupDCCReference = class
  public
    Form: string;
    DesignClass: string;
    [XmlAttribute]
    Include: string;
  end;

  ProjectItemGroupCompile = class
  public
    DependentUpon: string;
    AutoGen: string;
    DesignTime: string;
    Link: string;
    SubType: string;
    [XmlAttribute]
    Include: string;
  end;

  ProjectItemGroupReference = class
  public
    AssemblyName: string;
    CopyLocal: string;
    Version: string;
    HintPath: string;
    AssemblyTag: string;
    LinkUnits: string;
    [XmlAttribute]
    Include: string;
  end;

  ProjectItemGroupDelphiCompile = class
  public
    MainSource: string;
    [XmlAttribute]
    Include: string;
  end;

  TArrayOfProjectItemGroupEmbeddedResource = array of ProjectItemGroupEmbeddedResource;
  TArrayOfProjectItemGroupDCCReference = array of ProjectItemGroupDCCReference;
  TArrayOfProjectItemGroupCompile = array of ProjectItemGroupCompile;
  TArrayOfProjectItemGroupReference = array of ProjectItemGroupReference;

  ProjectItemGroup = class
  public
    [XmlElement('DelphiCompile')]
    DelphiCompile: ProjectItemGroupDelphiCompile;
    [XmlElement('EmbeddedResource')]
    EmbeddedResource: TArrayOfProjectItemGroupEmbeddedResource;
    [XmlElement('DCCReference')]
    DCCReference: TArrayOfProjectItemGroupDCCReference;
    [XmlElement('Compile')]
    Compile: TArrayOfProjectItemGroupCompile;
    [XmlElement('Reference')]
    Reference: TArrayOfProjectItemGroupReference;
  end;

  TArrayOfProjectItemGroup = array of ProjectItemGroup;

  [XmlRoot(ElementName = 'Project', Namespace = 'http://schemas.microsoft.com/developer/msbuild/2003', IsNullable = true)]
  TRADStudioProject = class (TSettingsObject)
  public
    [XmlElement('PropertyGroup')]
    PropertyGroup: TArrayOfProjectPropertyGroup;
    [XmlAnyElement('ProjectExtensions')]
    ProjectExtensions: TObject;
    [XmlAnyElement('Import')]
    Import: TObject;
    [XmlElement('ItemGroup')]
    ItemGroup: TArrayOfProjectItemGroup;
  end;

implementation

end.
