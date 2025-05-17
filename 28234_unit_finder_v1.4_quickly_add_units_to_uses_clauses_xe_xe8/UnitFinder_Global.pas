unit UnitFinder_Global;

interface

uses
  Windows, Messages, Classes, Registry, Menus;

const
  cRegistryPath = '\Software\BFSoftware\UnitFinder\';

var
  OpenShortcut: TShortcut = 0;
  BestShortcut: TShortcut = 0;
  InterfaceShortcut: TShortcut = 0;
  ImplementationShortcut: TShortcut = 0;

  UnitFinderRunning:boolean = False;


function GetDefaultOpenShortcut: TShortcut;
function GetDefaultBestShortcut: TShortcut;
function GetDefaultInterfaceShortcut: TShortcut;
function GetDefaultImplementationShortcut: TShortcut;

procedure LoadSettings;
procedure SaveSettings;

implementation


function GetDefaultOpenShortcut: TShortcut;
begin
  result := Shortcut(Ord('U'),[ssCtrl]);
end;

function GetDefaultBestShortcut: TShortcut;
begin
  result := Shortcut(VK_F11,[ssALT]);
end;

function GetDefaultInterfaceShortcut: TShortcut;
begin
  result := 0;
end;

function GetDefaultImplementationShortcut: TShortcut;
begin
  result := 0;
end;


procedure LoadSettings;
var
  Registry:TRegistry;
begin

  Registry := TRegistry.Create;
  try
    Registry.OpenKey(cRegistryPath, False);

    if Registry.ValueExists('OpenShortcut') then
      OpenShortcut := Registry.ReadInteger('OpenShortcut')
    else
      OpenShortcut := GetDefaultOpenShortcut;

    if Registry.ValueExists('BestShortcut') then
      BestShortcut := Registry.ReadInteger('BestShortcut')
    else
      BestShortcut := GetDefaultBestShortcut;

    if Registry.ValueExists('InterfaceShortcut') then
      InterfaceShortcut := Registry.ReadInteger('InterfaceShortcut')
    else
      InterfaceShortcut := GetDefaultInterfaceShortcut;

    if Registry.ValueExists('ImplementationShortcut') then
      ImplementationShortcut := Registry.ReadInteger('ImplementationShortcut')
    else
      ImplementationShortcut := GetDefaultImplementationShortcut;

  finally
    Registry.Free;
  end;
end;

procedure SaveSettings;
var
  Registry:TRegistry;
begin
  Registry := TRegistry.Create;
  try
    if not Registry.OpenKey(cRegistryPath, True) then
      exit;
    Registry.WriteInteger('OpenShortcut',OpenShortcut);
    Registry.WriteInteger('BestShortcut',BestShortcut);
    Registry.WriteInteger('InterfaceShortcut',InterfaceShortcut);
    Registry.WriteInteger('ImplementationShortcut',ImplementationShortcut);
  finally
    Registry.Free;
  end;
end;

end.
