unit ToolBarExpert;

interface

uses
  Classes, SysUtils, Windows, Dialogs, ToolsAPI, Menus, comctrls, actnlist;

type
  TToolBarWizard = class(TNotifierObject,IOTAWizard)
  private
    procedure CreateToolBar;
    procedure ViewXYZCmdExecute(Sender: TObject);
    procedure ViewXYZCmdUpdate(Sender: TObject);
  protected
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TToolBarClass=class of TToolBar;

procedure Register;

implementation

uses
  forms;

procedure Register;
begin
  RegisterPackageWizard(TToolBarWizard.Create);
end;

{ TToolBarWizard }

constructor TToolBarWizard.Create;
begin
  inherited ;
  CreateToolBar;
end;

procedure TToolBarWizard.CreateToolBar;
var
  DockToolBar: TToolBarClass;
  StdToolBar,XYZToolBar: TToolBar;
  ViewStdCmd,ViewXYZCmd: TAction;
begin
  StdToolBar:=(BorlandIDEServices as INTAServices).ToolBar['StandardToolBar'];//TToolBar(Application.MainForm.FindComponent('StandardToolBar'));
  DockToolBar:=TToolBarClass(StdToolBar.ClassType);

	XYZToolBar:=DockToolBar.Create(Application.MainForm);
  XYZToolBar.Name:='XYZToolBar';
  XYZToolBar.Caption:='XYZ';
  XYZToolBar.OnGetSiteInfo:=StdToolBar.OnGetSiteInfo;
  XYZToolBar.Constraints.MinHeight:=StdToolBar.Constraints.MinHeight;
  XYZToolBar.Constraints.MinWidth:=StdToolBar.Constraints.MinWidth;
  XYZToolBar.DockSite:=StdToolBar.DockSite;
  XYZToolBar.DragKind:=StdToolBar.DragKind;
  XYZToolBar.DragMode:=StdToolBar.DragMode;
  XYZToolBar.PopupMenu:=StdToolBar.PopupMenu;
  XYZToolBar.OnStartDock:=StdToolBar.OnStartDock;
  XYZToolBar.OnEndDock:=StdToolBar.OnEndDock;
  XYZToolBar.ShowHint:=StdToolBar.ShowHint;
  XYZToolBar.ShowCaptions:=StdToolBar.ShowCaptions;
  XYZToolBar.Parent:=StdToolBar.Parent;
  XYZToolBar.Images:=StdToolBar.Images;
  XYZToolBar.Visible:=True;

  ViewStdCmd:=TAction(Application.MainForm.FindComponent('ViewStandardCommand'));
  if ViewStdCmd<>nil then
  begin
    ViewXYZCmd:=TAction.Create(Application.MainForm);
    ViewXYZCmd.ActionList:=ViewStdCmd.ActionList;
    ViewXYZCmd.Name:='ViewXYZCommand';
    ViewXYZCmd.Caption:='X&YZ';
    ViewXYZCmd.Category:='View';
    ViewXYZCmd.OnExecute:=ViewXYZCmdExecute;
    ViewXYZCmd.OnUpdate:=ViewXYZCmdUpdate;
  end;
end;

destructor TToolBarWizard.Destroy;
begin

  inherited;
end;

procedure TToolBarWizard.Execute;
begin
end;

function TToolBarWizard.GetIDString: string;
begin
  Result:='EP.ToolBarWizard';
end;

function TToolBarWizard.GetName: string;
begin
  Result:='Tool Bar Wizard';
end;

function TToolBarWizard.GetState: TWizardState;
begin
  Result:=[];
end;

procedure TToolBarWizard.ViewXYZCmdExecute(Sender: TObject);
var
  XYZToolBar: TToolBar;
begin
  XYZToolBar:=(BorlandIDEServices as INTAServices).ToolBar['XYZToolBar'];
  if XYZToolBar<>nil then
    XYZToolBar.Visible:=not XYZToolBar.Visible;
end;

procedure TToolBarWizard.ViewXYZCmdUpdate(Sender: TObject);
var
  XYZToolBar: TToolBar;
begin
  XYZToolBar:=(BorlandIDEServices as INTAServices).ToolBar['XYZToolBar'];
  if (XYZToolBar<>nil) and (Sender is TAction) then
    TAction(Sender).Checked:=XYZToolBar.Visible;
end;

end.

