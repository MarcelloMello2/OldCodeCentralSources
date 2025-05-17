unit UnitFinder_SetupMenu;
interface

uses Windows, Messages, Sysutils, Forms, Menus, Controls,
     ImgList, Graphics, Classes, ActnList,  ComCtrls, ToolsAPI, Dialogs,
     StdCtrls,UnitFinder_SetupForm,UnitFinder_Global;

type

  TUnitFinderMainMenuWizard = class(TNotifierObject, IOTAWizard, IOTANotifier)
  protected
    FUnitFinderMenuItem: TMenuItem;
    FUnitFinderImageIndex:integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure UnitFinderSetupExecute(Sender:TObject);

    // Interface: IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

  end;

procedure Register;

implementation

const
  cWizardIDName = 'UnitFinderMainMenuWizard';
  cWizardName = 'UnitFinder Setup Menu Item';


procedure Register;
begin
  RegisterPackageWizard(TUnitFinderMainMenuWizard.Create);
end;


function GetMenuItemByName(MenuItem:TMenuItem;  MenuItemName:string):TMenuItem;
var i:integer;
begin
  result := nil;
  MenuItemName := lowercase(MenuItemName);

  for i := 0 to MenuItem.Count-1 do begin
    if lowercase(MenuItem.Items[i].Name) = MenuItemName then begin
      result := MenuItem.Items[i];
      break;
    end
  end;
end;



{ TUnitFinderMainMenuWizard }

constructor TUnitFinderMainMenuWizard.Create;
var ImageList:TCustomImageList;
    MainMenu:TMainMenu;
    Icon:TIcon;
    ToolsMenu,ConfigureMenu:TMenuItem;
    AppBuilder:TComponent;
begin
  AppBuilder := Application.FindComponent('AppBuilder');
  MainMenu := (BorlandIDEServices as INTAServices).GetMainMenu;
  ImageList := (BorlandIDEServices as INTAServices).GetImageList;

  Icon := TIcon.Create;
  try

    // ** Prepare the Menu Item for the IDE **

    // Create the MenuItem
    FUnitFinderMenuItem := TMenuItem.Create(AppBuilder);
    FUnitFinderMenuItem.Name := 'UnitFinderMenuItem';
    FUnitFinderMenuItem.Caption := 'Unit Finder Setup';
    FUnitFinderMenuItem.OnClick := UnitFinderSetupExecute;
    Icon.Handle := LoadIcon(HInstance,PChar('ICON_UnitFinder'));
    FUnitFinderImageIndex := ImageList.AddIcon( Icon );
    FUnitFinderMenuItem.ImageIndex := FUnitFinderImageIndex;

    // ** Add the menu items to the correct menu **

    ToolsMenu := GetMenuItemByName(MainMenu.Items,'ToolsMenu');
    if ToolsMenu = nil then
      raise Exception.Create('TUnitFinderMainMenuWizard.Create: TMenuItem "ToolsMenu" could not be found.');
    ConfigureMenu := GetMenuItemByName(ToolsMenu,'ToolsToolsItem');
    if ConfigureMenu = nil then
      raise Exception.Create('TUnitFinderMainMenuWizard.Create: TMenuItem "ToolsToolsItem" could not be found.');

    ToolsMenu.Insert(ConfigureMenu.MenuIndex,FUnitFinderMenuItem);

  finally
    Icon.Free;
  end;

end;

destructor TUnitFinderMainMenuWizard.Destroy;
begin
  FUnitFinderMenuItem.Free;

  {
  Important: Notice there is no code to delete the
  Images from Delphi's ImageList. If we were to delete
  them, any images added after the UnitFinder images were added
  will point to incorrect and/or invalid indexes. We'll just
  have to let them stay in the ImageList until Delphi shuts down.
  }

  inherited;
end;

procedure TUnitFinderMainMenuWizard.Execute;
begin
  //
end;

function TUnitFinderMainMenuWizard.GetIDString: string;
begin
  result := cWizardIDName;
end;

function TUnitFinderMainMenuWizard.GetName: string;
begin
  result := cWizardName;
end;

function TUnitFinderMainMenuWizard.GetState: TWizardState;
begin
  result := [];
end;


procedure TUnitFinderMainMenuWizard.UnitFinderSetupExecute(Sender: TObject);
begin
  UnitFinderRunning := True;
  try
    StartSetupForm;
  finally
    UnitFinderRunning := False;
  end;
end;

end.
