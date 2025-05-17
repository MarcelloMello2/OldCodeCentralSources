(*******************************************************************)
(*                                                                 *}
(*   Borland Delphi Visual Component Library                       *}
(*                                                                 *)
(*  Unit:            RunExpert.pas                                 *)
(*  Expert:          RunExpert                                     *)
(*  Compatible:      Delphi 4-6                                    *)
(*  License:         Freeware with source code                     *)
(*                                                                 *)
(*  Author:          Vadim Vinokur                                 *)
(*  Author email:    vadim@ems-hitech.com                          *)
(*  Author website:  http://www.ems-hitech.com                     *)
(*                                                                 *)
(*  Description:     This expert creates an additional menu item   *)
(*                   in Delphi Run menu and assigns                *)
(*                   shortcut Shift+F9 with it. Use this item      *)
(*                   for compile your project and run your program *)
(*                   without IDE control (as from Windows Explorer *)
(*                   or any other file manager) after that         *)
(*                   (if compile was successful).                  *)
(*                   Also this experts assigns shortcuts Alt+F9    *)
(*                   and Ctrl+Shift+F9 with Syntax Check and       *)
(*                   Build All Projects accordingly.               *)
(*                   You can disable any part of                   *)
(*                   this functionality: just comment or delete    *)
(*                   the corresponding compile directive in the    *)
(*                   beginning of the unit and recompile the       *)
(*                   package RunExpertDX.dpk for Delphi X          *)
(*                                                                 *)
(*  Disclaimer:      This is provided as is, expressly without a   *)
(*                   warranty of any kind.                         *)
(*                   You use it at your own risc.                  *)
(*                                                                 *)
(*******************************************************************)

unit RunExpert;

{$DEFINE RunSeparatelyItem}
{$DEFINE ShortCutForSyntaxCheck}
{$DEFINE ShortCutForBuildAll}

{$IFDEF VER120} // Delphi 4
  {$DEFINE VCL4}
{$ENDIF}

{$IFDEF VER125} // BCB 4
  {$DEFINE VCL4}
{$ENDIF}


interface

uses
  Windows, SysUtils,  Classes, Controls, Dialogs, Menus, ToolsApi, ExtCtrls;

type

  TRunExpert = class({$IFDEF VCL4}TInterfacedObject
    {$ELSE}TNotifierObject{$ENDIF}, IOTAWizard)
  private
    // IDE Menu Items
    RunMenuItem, ProjectMenuItem: TMenuItem;
{$IFDEF RunSeparatelyItem}
    RSItem: TMenuItem; // Run separatelly
{$ENDIF}
    SyntaxCheckItem, BuildAllItem: TMenuItem;
    FTimer: TTimer;
    procedure OnClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    // IOTAWizard methods
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  {$IFDEF VCL4}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  {$ENDIF}
  end;

procedure Register;

implementation

uses ShellAPI, Graphics;

resourcestring
  SMainMenuNotFound = 'Sorry, unable to find IDE main menu :-(';
  SRunMenuNotFound = 'Sorry, unable to find Run menu :-(';
  SProjectMenuNotFound = 'Sorry, unable to find Project menu :-(';
  SCurrentProjectNotFound = 'Sorry, current project not available. :-(';

  SRunSeparatelyCaption = 'Run separately';

const // do not localize

  SExpertIDString = 'Vadim Vinokur.RunExpert';
  SExpertName = 'RunExpert';

  SRunMenuName = 'RunMenu';
  SProjectMenuName = 'ProjectMenu';
  SSyntaxCheckItemName = 'ProjectSyntaxItem';
  SBuildAllItemName = 'ProjectBuildAllItem';

  SRunSeparatelyItemName = 'RunRunSeparatellyItem';

procedure Register;
begin
  RegisterPackageWizard(TRunExpert.Create);
end;

{ TRunExpert }

constructor TRunExpert.Create;
var
  IDEMenu: TMenu; // IDE Main Menu
  I: Integer;
begin
  inherited Create;
  IDEMenu := (BorlandIDEServices as INTAServices).GetMainMenu();
  if not Assigned(IDEMenu) then begin
    MessageDlg(SMainMenuNotFound, mtError, [mbOk], 0);
    Exit;
  end;
  for I := 0 to IDEMenu.Items.Count - 1 do begin
    if CompareText(IDEMenu.Items[I].Name, SRunMenuName) = 0 then
      RunMenuItem := IDEMenu.Items[I];
    if CompareText(IDEMenu.Items[I].Name, SProjectMenuName) = 0 then
      ProjectMenuItem := IDEMenu.Items[I];
  end;
  if not Assigned(RunMenuItem) then begin
    MessageDlg(SRunMenuNotFound, mtError, [mbOk], 0);
    Exit;
  end;
  if not Assigned(ProjectMenuItem) then begin
    MessageDlg(SProjectMenuNotFound, mtError, [mbOk], 0);
    Exit;
  end;

  for I := 0 to ProjectMenuItem.Count - 1 do begin
    if CompareText(ProjectMenuItem.Items[I].Name, SSyntaxCheckItemName) = 0 then
      SyntaxCheckItem := ProjectMenuItem.Items[I];
    if CompareText(ProjectMenuItem.Items[I].Name, SBuildAllItemName) = 0 then
      BuildAllItem := ProjectMenuItem.Items[I];
  end;
  if Assigned(SyntaxCheckItem) then
    SyntaxCheckItem.ShortCut := ShortCut(VK_F9, [ssAlt]);
  if Assigned(BuildAllItem) then
    BuildAllItem.ShortCut := ShortCut(VK_F9, [ssShift, ssCtrl]);

{$IFDEF RunSeparatelyItem}
  RSItem := TMenuItem.Create(nil);
  RSItem.Name := SRunSeparatelyItemName;
  RSItem.Caption := SRunSeparatelyCaption;
  RSItem.OnClick := OnClick;
  RunMenuItem.Insert(1, RSItem);
  RSItem.ShortCut := ShortCut(VK_F9, [ssShift]);
{$ENDIF}
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := OnTimer;
  FTimer.Enabled := True;
end;

destructor TRunExpert.Destroy;
{$IFDEF RunSeparatelyItem}
var
 Index: Integer;
{$ENDIF}
begin
{$IFDEF RunSeparatelyItem}
  if Assigned(RunMenuItem) then begin
    Index := RunMenuItem.IndexOf(RSItem);
    if Index >= 0 then
      RunMenuItem.Delete(Index);
  end;
  RSItem.Free;
{$ENDIF}
  FTimer.Free;
  inherited Destroy;
end;

function TRunExpert.GetIDString: string;
begin
  Result := SExpertIDString;
end;

function TRunExpert.GetName: string;
begin
  Result := SExpertName;
end;

procedure TRunExpert.OnClick(Sender: TObject);
begin
  Execute();
end;

procedure TRunExpert.Execute;
var
  Project: IOTAProject;
  ProjectOptions: IOTAProjectOptions;
  ProjectBuilder: IOTAProjectBuilder;
  OutputDir, ProjectFileName, ExeName, ProjectExt: string;

  function GetCurrentProject: IOTAProject;
  { This function uses code from Erik Berry's Open Tools FAQ,
    http://www.gexperts.org/opentools/  }
  var
    Services: IOTAModuleServices;
    Module: IOTAModule;
    Project: IOTAProject;
    ProjectGroup: IOTAProjectGroup;
    MultipleProjects: Boolean;
    I: Integer;
  begin
    Result := nil;
    MultipleProjects := False;
    Services := BorlandIDEServices as IOTAModuleServices;
    for I := 0 to Services.ModuleCount - 1 do
    begin
      Module := Services.Modules[I];
      if Module.QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK then
      begin
        Result := ProjectGroup.ActiveProject;
        Exit;
      end
      else if Module.QueryInterface(IOTAProject, Project) = S_OK then
      begin
        if Result = nil then
          // Found the first project, so save it
          Result := Project
        else
          MultipleProjects := True;
          // It doesn't look good, but keep searching for a project group
      end;
    end;
    if MultipleProjects then
      Result := nil;
  end;

begin
  Project := GetCurrentProject();
  if not Assigned(Project) then begin
    MessageDlg(SCurrentProjectNotFound, mtError, [mbOk], 0);
    Exit;
  end;
  // get project file name
  ProjectFileName := Project.GetFileName();
  ProjectExt := ExtractFileExt(ProjectFileName);
  // don't try run dpk, bpk, etc...
  if not ((CompareText(ProjectExt, '.dpr') = 0) or
     (CompareText(ProjectExt, '.cpp') = 0) or
     (CompareText(ProjectExt, '.bpr') = 0)) then Exit;
  // compile project
  ProjectBuilder := Project.GetProjectBuilder();
  // Exit if failure
  if not ProjectBuilder.BuildProject(cmOTAMake, False) then Exit;
  // get output directory
  ProjectOptions := Project.GetProjectOptions();

  OutputDir := ProjectOptions.GetOptionValue('OutputDir');
  if OutputDir = '' then
    ExeName := ChangeFileExt(ProjectFileName, '.exe')
  else begin
    if OutputDir[Length(OutputDir)] <> '\' then
      OutputDir := OutputDir + '\';
    ExeName := OutputDir + ChangeFileExt(ExtractFileName(ProjectFileName), '.exe');
  end;
  // try executing file
  if FileExists(ExeName) then
    ShellExecute(0, 'open', PChar(ExeName), '', '', SW_SHOWNORMAL);
end;

function TRunExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TRunExpert.OnTimer(Sender: TObject);
begin
{$IFDEF RunSeparatelyItem}
  if Assigned(RSItem) and (RSItem.ShortCut = 0) then
    RSItem.ShortCut := ShortCut(VK_F9, [ssShift]);
{$ENDIF}
{$IFDEF ShortCutForSyntaxCheck}
  if Assigned(SyntaxCheckItem) then
    SyntaxCheckItem.ShortCut := ShortCut(VK_F9, [ssAlt]);
{$ENDIF}
{$IFDEF ShortCutForBuildAll}
  if Assigned(BuildAllItem) then
    BuildAllItem.ShortCut := ShortCut(VK_F9, [ssShift, ssCtrl]);
{$ENDIF}
  FTimer.Enabled := False;
end;

{$IFDEF VCL4}

procedure TRunExpert.AfterSave;
begin
end;

procedure TRunExpert.BeforeSave;
begin
end;

procedure TRunExpert.Destroyed;
begin
end;

procedure TRunExpert.Modified;
begin
end;

{$ENDIF}

end.
