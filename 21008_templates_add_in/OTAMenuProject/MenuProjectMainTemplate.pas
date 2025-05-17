unit [!UnitName];

// Created with USysWare Templates add-in

interface

type
  T[!ClassName] = class
  private
    procedure MenuItemOnExecute(Sender: TObject; E: EventArgs);
  public
    class procedure IDERegister; static;
  end;

implementation

uses
  System.Windows.Forms,
  Borland.Studio.ToolsAPI,
  [!DependantUnitName];

const
  //ToDo: update constants
  AddInName = '[!AddInName]';
  AddInCompanyLong = 'USysWare, Inc.';
  AddInCopyright = 'Copyright (c) 2003 ' + AddInCompanyLong + #13#10 +
                   'All Rights Reserved';
  AddInMenuName = 'MyAddInMenu';
  AddInMenuText = '[!AddInMenuText]';

var
  AddInInitialized: Boolean = False;
  [!ClassName]: T[!ClassName] = nil;

{ T[!ClassName] }

class procedure T[!ClassName].IDERegister;
var
  IAboutBox: IOTAAboutBoxService;
  ISplashScreen: IOTASplashScreenService;
  IMainMenuService: IOTAMainMenuService;
  IMenuItem: IOTAMenuItem;
begin
  if AddInInitialized then
    raise ApplicationException.Create(
      AddInName + ' must be setup just once.');
  AddInInitialized := True;

  IAboutBox := GetAboutBoxService;
  IAboutBox.AddPluginInfo(AddInName, AddInCopyright, nil, False, '', '');

  ISplashScreen := GetSplashScreenService;
  ISplashScreen.StatusMessage('Loading ' + AddInName);

  IMainMenuService := GetMainMenuService;
  // add menu item below 'Tools|Build Tools...'
  IMenuItem := IMainMenuService.AddMenuItem('IDEToolsItem',
    OTAMenuItemLocation.otamlAfter, AddInMenuName, AddInMenuText);
  if IMenuItem <> nil then
  begin
    [!ClassName] := T[!ClassName].Create;
    Include(IMenuItem.Executed, [!ClassName].MenuItemOnExecute);
  end;
end;

procedure T[!ClassName].MenuItemOnExecute(Sender: TObject; E: EventArgs);
begin
  //ToDo: add your code here

  MessageBox.Show('Test');
end;

end.