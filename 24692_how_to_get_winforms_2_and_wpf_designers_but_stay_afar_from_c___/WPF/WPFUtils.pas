unit WPFUtils;

interface

uses
  System.Windows;

type
  TSupportedVisualStyles = (Classic,Luna,Royale,Aero);

  TApplicationHelper = class helper for System.Windows.Application
  public
    procedure EnableVisualStyle(Style: TSupportedVisualStyles);
  end;

  TWPFAdapter<WPFClass: constructor> = class (TObject)
  strict private
    FWPFObject: WPFClass;
  public
    property WPFObject: WPFClass read FWPFObject;
    constructor Create;
  end;

implementation

procedure TApplicationHelper.EnableVisualStyle(Style: TSupportedVisualStyles);
var
  Dictionary: ResourceDictionary;
  UriValue: string;
begin
  Dictionary := ResourceDictionary.Create;
  case Style of
    Classic: UriValue := '/PresentationFramework.Classic,Version=3.0.0.0,Culture=neutral,PublicKeyToken=31bf3856ad364e35,ProcessorArchitecture=MSIL;component/themes/classic.xaml';
    Luna: UriValue := '/PresentationFramework.Luna,Version=3.0.0.0,Culture=neutral,PublicKeyToken=31bf3856ad364e35,ProcessorArchitecture=MSIL;component/themes/luna.normalcolor.xaml';
    Royale: UriValue := '/PresentationFramework.Royale,Version=3.0.0.0,Culture=neutral,PublicKeyToken=31bf3856ad364e35,ProcessorArchitecture=MSIL;component/themes/royale.normalcolor.xaml';
    Aero: UriValue := '/PresentationFramework.Aero,Version=3.0.0.0,Culture=neutral,PublicKeyToken=31bf3856ad364e35,ProcessorArchitecture=MSIL;component/themes/aero.normalcolor.xaml';
  end;
  Dictionary.Source := System.Uri.Create(UriValue, System.UriKind.Relative);
  Resources := Dictionary;
end;

constructor TWPFAdapter<WPFClass>.Create;
begin
  inherited Create;
  FWPFObject := WPFClass.Create;
end;

end.
