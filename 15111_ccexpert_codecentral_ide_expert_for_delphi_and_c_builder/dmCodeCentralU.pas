unit dmCodeCentralU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  TdmCodeCentral = class(TDataModule)
    mnuCodeCentral: TPopupMenu;
    miCodeCentral: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure miCodeCentralClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmCodeCentral: TdmCodeCentral;

resourcestring
  SAnyProduct = 'Any product';

procedure Register;

implementation

uses ToolsAPI, EditIntf, frmCodeCentralU, Exptintf;

{$R *.DFM}

type

  TCodeCentralSearchExpert = class(TInterfacedObject, IOTAWizard, IOTANotifier)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Execute;
    procedure Modified;
    function GetState : TWizardState;
    function GetIDString: string;
    function GetName: string;

    constructor Create;
    destructor Destroy; override;
  end;

{ TCodeCentralSearchExpert }

procedure TCodeCentralSearchExpert.AfterSave;
begin
end;

procedure TCodeCentralSearchExpert.BeforeSave;
begin
end;

constructor TCodeCentralSearchExpert.Create;
begin
  dmCodeCentral := TdmCodeCentral.Create(nil);
end;

destructor TCodeCentralSearchExpert.Destroy;
begin
  FreeAndNil(dmCodeCentral);
  if Assigned(frmCodeCentral) then
    frmCodeCentral.Free;
  frmCodeCentral := nil;
end;

procedure TCodeCentralSearchExpert.Destroyed;
begin
end;

procedure TCodeCentralSearchExpert.Execute;
begin
end;

function TCodeCentralSearchExpert.GetIDString: string;
begin
  Result := 'CodeCentral.CodeCentral Search Expert';
end;

function TCodeCentralSearchExpert.GetName: string;
begin
  Result := 'CodeCentral Search';
end;

function TCodeCentralSearchExpert.GetState: TWizardState;
begin
  result := [];
end;

procedure TCodeCentralSearchExpert.Modified;
begin

end;

procedure TdmCodeCentral.DataModuleCreate(Sender: TObject);
const
  SMainMenu = 'HelpMenu';
  {$ifdef VER130} // Delphi 5 or C++ Builder 5 specific
    {$ifdef BCB}
  SSubMenu = 'HelpCommunityPage';
    {$else}
  SSubMenu = 'HelpBorlandCommunityPage';
    {$endif}
  {$else} // This should be for Delpi 6 and beyond
    {$ifdef VER140}
  SSubMenu = 'HelpCommunityPage';
    {$else} // Previous versions of Delphi or C++ Builder
  SSubMenu = 'HelpWinSDKItem';
    {$endif}
  {$endif}
var
  Main : TMenuItem;
  Item : TMenuItem;
  MainMenu : TMainMenu;
  i : Integer;
  {
    You can use the conditonal LIST_MENUS code to find out what the menu
    item names are for the help menu. Simply define LIST_MENUS by removing
    the space in front of the $define (below), and try to install the menu.
    You will be provided with a list of the Help Menu names when you
    try to install this package in the IDE.
  }
  { $define LIST_MENUS}
  {$ifdef LIST_MENUS}
  Menus : string;
  {$endif}
begin
  Main := nil;
  MainMenu := (BorlandIDEServices as INTAServices).GetMainMenu;
  for i := 0 to MainMenu.Items.Count - 1 do
  begin
    if MainMenu.Items[i].Name = SMainMenu then
    begin
      Main := MainMenu.Items[i];
      break;
    end;
  end;
  if Main <> nil then
  begin
    Item := mnuCodeCentral.Items[0];
    mnuCodeCentral.Items.Delete(0);

    {$ifdef LIST_MENUS}
    Menus := '';
    for i := 0 to Main.Count - 1 do
      Menus := Menus + Main.Items[i].Name + #13;
    Raise Exception.Create( Menus );
    {$endif}

    i := 0;
    while (i < Main.Count) and (Main.Items[i].Name <> SSubMenu) do
      Inc(i);
    Main.Insert(i + 1, Item);
  end;
end;

procedure TdmCodeCentral.miCodeCentralClick(Sender: TObject);
var
  FormIntf: TIFormInterface;
  CompIntf: TIComponentInterface;
  CurrentFile, FileExt, UnitName, ComponentName : String;
  ModIntf: TIModuleInterface;
  i : Integer;
  SList : TStringList;

begin
  ModIntf := nil;
  if not Assigned(frmCodeCentral) then
    frmCodeCentral := TfrmCodeCentral.Create(Application);
  with frmCodeCentral do
  begin
    CurrentFile := ToolServices.GetCurrentFile;
    FileExt := ExtractFileExt(CurrentFile);

    { Get module interface }
    i := Pos(FileExt, CurrentFile);
    if i > 0 then
    begin
      UnitName := Copy(CurrentFile, 1, i) + 'PAS';
      ModIntf := ToolServices.GetModuleInterface(UnitName);
      Product := 'Delphi';
      if ModIntf = nil then
      begin
        UnitName := Copy(CurrentFile, 1, i) + 'CPP';
        ModIntf := ToolServices.GetModuleInterface(UnitName);
        Product := 'C++ Builder';
      end;
      if ModIntf = nil then
        Product := SAnyProduct;
    end;

    if ModIntf <> nil then
    begin
      FormIntf := ModIntf.GetFormInterface;
      if FormIntf <> nil then
      begin
        SList := TStringList.Create;
        try
          SList.Sorted := true;
          SList.Duplicates := dupIgnore;
          for i := 0 to FormIntf.GetSelCount - 1 do
          begin
            CompIntf := FormIntf.GetSelComponent(i);
            try
              if CompIntF <> nil then
              begin
                ComponentName := CompIntf.GetComponentType;
                if Trim(ComponentName) <> '' then
                  SList.Add(ComponentName);
              end;
            finally
              CompIntf.Free;
            end;
          end;
          for i := 0 to SList.Count - 1 do
            edtKeywords.Text := edtKeywords.Text + '+' + SList[i] + ' ';
        finally
          SList.Free;
        end;
      end;
    end;
    Show;
  end;
end;

procedure Register;
begin
  RegisterPackageWizard(TCodeCentralSearchExpert.Create);
end;

end.

