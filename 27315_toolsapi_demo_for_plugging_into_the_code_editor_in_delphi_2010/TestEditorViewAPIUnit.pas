unit TestEditorViewAPIUnit;

interface

procedure Register;

implementation

uses
  SysUtils, Dialogs, Classes, Forms, Menus, ToolsAPI, DesignIntf,
  EditorTabFrame, FileInfoFrame;

type

  TFileInfoEditorSubView = class(TInterfacedObject, INTACustomEditorSubView)
  private
    FEditorViewServices: IOTAEditorViewServices;
    FModuleServices: IOTAModuleServices;
    function GetEditorViewServices: IOTAEditorViewServices;
    function GetModuleServices: IOTAModuleServices;
  public
    procedure Display(const AContext: IInterface; AViewObject: TObject);
    function EditAction(const AContext: IInterface; Action: TEditAction;
      AViewObject: TObject): Boolean;
    procedure FrameCreated(AFrame: TCustomFrame);
    function GetCanCloneView: Boolean;
    function GetCaption: string;
    function GetEditState(const AContext: IInterface;
      AViewObject: TObject): TEditState;
    function GetFrameClass: TCustomFrameClass;
    function GetPriority: Integer;
    function GetViewIdentifier: string;
    function Handles(const AContext: IInterface): Boolean;
    procedure Hide(const AContext: IInterface; AViewObject: TObject);
    procedure ViewClosed(const AContext: IInterface; AViewObject: TObject);
  end;

var
  MyEditorViewRef: Pointer;
  MenuItem: TMenuItem;

procedure AddViewMenuItem;
begin
  MenuItem := Menus.NewItem('Something Cool', 0, False, True, TEditorTabHandler.OnViewMenuClick, 0, 'ViewSomethingCoolItem');
  (BorlandIDEServices as INTAServices).AddActionMenu('DockEditWindow1', nil, MenuItem);
end;

procedure Register;
begin
  MyEditorViewRef :=
    (BorlandIDEServices as IOTAEditorViewServices).RegisterEditorSubView(TFileInfoEditorSubView.Create as INTACustomEditorSubView);
  (BorlandIDEServices as IOTAEditorViewServices).RegisterEditorView(cViewIdentifier, EditorTabFrame.RecreateTab);
  AddViewMenuItem;
end;

procedure UnregisterItems;
begin
  (BorlandIDEServices as IOTAEditorViewServices).UnregisterEditorSubView(MyEditorViewRef);
  (BorlandIDEServices as IOTAEditorViewServices).UnregisterEditorView(cViewIdentifier);
  FreeAndNil(MenuItem);
end;

{ TFileInfoEditorSubView }

procedure TFileInfoEditorSubView.Display(const AContext: IInterface;
  AViewObject: TObject);
var
  LModule: IOTAModule;
begin
  if (AViewObject is TFileInfoFrame) and
    GetEditorViewServices.ContextToModule(AContext, LModule) then
  begin
    TFileInfoFrame(AViewObject).Module := LModule;
  end;
end;

function TFileInfoEditorSubView.EditAction(const AContext: IInterface;
  Action: TEditAction; AViewObject: TObject): Boolean;
begin
  Result := False;
end;

procedure TFileInfoEditorSubView.FrameCreated(AFrame: TCustomFrame);
begin

end;

function TFileInfoEditorSubView.GetCanCloneView: Boolean;
begin
  Result := True;
end;

function TFileInfoEditorSubView.GetCaption: string;
begin
  Result := 'OS File Information';
end;

function TFileInfoEditorSubView.GetEditorViewServices: IOTAEditorViewServices;
begin
  if FEditorViewServices = nil then
    Supports(BorlandIDEServices, IOTAEditorViewServices, FEditorViewServices);
  Result := FEditorViewServices;
  assert(Result <> nil);
end;

function TFileInfoEditorSubView.GetEditState(const AContext: IInterface;
  AViewObject: TObject): TEditState;
begin
  Result := [];
end;

function TFileInfoEditorSubView.GetFrameClass: TCustomFrameClass;
begin
  Result := TFileInfoFrame;
end;

function TFileInfoEditorSubView.GetModuleServices: IOTAModuleServices;
begin
  if FModuleServices = nil then
    Supports(BorlandIDEServices, IOTAModuleServices, FModuleServices);
  Result := FModuleServices;
  assert(Result <> nil);
end;

function TFileInfoEditorSubView.GetPriority: Integer;
begin
  Result := svpNormal;
end;

function TFileInfoEditorSubView.GetViewIdentifier: string;
begin
  Result := 'OSFileInformation';
end;

function TFileInfoEditorSubView.Handles(const AContext: IInterface): Boolean;
var
  LModule: IOTAModule;
begin
  Result := GetEditorViewServices.ContextToModule(AContext, LModule) and (LModule.ModuleFileCount > 0) and
    ((LModule.FileSystem = '') or GetModuleServices.FindFileSystem(LModule.FileSystem).IsFileBased);
end;

procedure TFileInfoEditorSubView.Hide(const AContext: IInterface;
  AViewObject: TObject);
begin

end;

procedure TFileInfoEditorSubView.ViewClosed(const AContext: IInterface;
  AViewObject: TObject);
var
  LModule: IOTAModule;
begin
  if (AViewObject is TFileInfoFrame) and
    GetEditorViewServices.ContextToModule(AContext, LModule) then
  begin
    TFileInfoFrame(AViewObject).Module := nil;
  end;
end;

initialization
finalization
  UnregisterItems;
end.
