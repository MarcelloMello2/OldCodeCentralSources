unit EditorTabFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, DesignIntf, ComCtrls, ToolsAPI, IniFiles, Menus;

type
  TEditorTabHandler = class;

  TEditorTab = class(TFrame)
    Ball: TShape;
    MoveTimer: TTimer;
    FramePopupMenu: TPopupMenu;
    StopStartItem: TMenuItem;
    SetShapetoHereItem: TMenuItem;
    procedure MoveTimerTimer(Sender: TObject);
    procedure StopStartItemClick(Sender: TObject);
    procedure SetShapetoHereItemClick(Sender: TObject);
  private
    FVertMove, FHorzMove: Integer;
    FTimerCount, FChangeDirCount: Integer;
    FEditorTabHandler: TEditorTabHandler;
    class var FCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEditorTabHandler = class(TInterfacedObject, INTACustomEditorView,
    INTACustomEditorViewStatusPanel, INTACustomEditorViewState)
  private
    FEditorTab: TEditorTab;
    FFileName: string;
  public
    constructor Create(const Filename: string);
    class procedure OnViewMenuClick(Sender: TObject);
    { INTACustomEditorView }
    procedure CloseAllCalled(var ShouldClose: Boolean);
    procedure DeselectView;
    function EditAction(Action: TEditAction): Boolean;
    function GetCanCloneView: Boolean;
    function CloneEditorView: INTACustomEditorView;
    function GetCaption: string;
    function GetEditState: TEditState;
    function GetEditorWindowCaption: string;
    function GetViewIdentifier: string;
    procedure SelectView;
    function GetFrameClass: TCustomFrameClass;
    procedure FrameCreated(AFrame: TCustomFrame);
    function CreateFrameInstance(AOwner: TComponent): TCustomFrame;
    { INTACustomEditorTabStatusPanel }
    procedure ConfigurePanel(StatusBar: TStatusBar;
      Panel: TStatusPanel);
    procedure DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    function GetStatusPanelCount: Integer;
    { INTACustomEditorViewState }
    procedure LoadViewState(const Desktop: TCustomIniFile;
      const ViewDeskSection: string);
    procedure SaveViewState(const Desktop: TCustomIniFile;
      const IsProject: Boolean; const ViewDeskSection: string);
  end;

const
  cViewIdentifier = 'SomethingCoolView';

function RecreateTab: INTACustomEditorView;

implementation

{$R *.dfm}

function RecreateTab: INTACustomEditorView;
begin
  Result := TEditorTabHandler.Create('Something Cool') as INTACustomEditorView;
end;

{ TEditorTab }

constructor TEditorTab.Create(AOwner: TComponent);
begin
  FVertMove := 2;
  FHorzMove := 2;
  FTimerCount := 0;
  FChangeDirCount := 0;
  inherited;
  Name := Format('%s_%d', [Name, FCount]);
  Inc(FCount);
end;

procedure TEditorTab.MoveTimerTimer(Sender: TObject);

  procedure UpdateStatusBar(const NewText: string);
  var
    EditWindow: INTAEditWindow;
  begin
    EditWindow := (BorlandIDEServices as IOTAEditorViewServices).GetOwningEditWindow(FEditorTabHandler as INTACustomEditorView);
    EditWindow.StatusBar.Panels[0].Text := NewText;
  end;

begin
  Inc(FTimerCount);
  Ball.Top := Ball.Top + FVertMove;
  Ball.Left := Ball.Left + FHorzMove;

  if Ball.Top + Ball.Height > Height then
    Ball.Top := Height - Ball.Height;
  if Ball.Top < 0 then
    Ball.Top := 0;
  if (Ball.Top + Ball.Height >= Height) or (Ball.Top <= 0)  then
  begin
    FVertMove := -FVertMove;
    Inc(FChangeDirCount);
    UpdateStatusBar(IntToStr(FChangeDirCount));
  end;

  if Ball.Left + Ball.Width > Width then
    Ball.Left := Width - Ball.Width;
  if Ball.Left < 0 then
    Ball.Left := 0;
  if (Ball.Left + Ball.Width >= Width) or (Ball.Left <= 0) then
  begin
    FHorzMove := -FHorzMove;
    Inc(FChangeDirCount);
    UpdateStatusBar(IntToStr(FChangeDirCount));
  end;
  if FTimerCount mod 50 = 0 then
  begin
    FTimerCount := 0;
    if Ball.Brush.Color = clBlue then
      Ball.Brush.Color := clRed
    else if Ball.Brush.Color = clRed then
      Ball.Brush.Color := clYellow
    else if Ball.Brush.Color = clYellow then
      Ball.Brush.Color := clGreen
    else if Ball.Brush.Color = clGreen then
      Ball.Brush.Color := clWhite
    else if Ball.Brush.Color = clWhite then
      Ball.Brush.Color := clBlack
    else if Ball.Brush.Color = clBlack then
      Ball.Brush.Color := clPurple
    else if Ball.Brush.Color = clPurple then
      Ball.Brush.Color := clFuchsia
    else if Ball.Brush.Color = clFuchsia then
      Ball.Brush.Color := clLime
    else if Ball.Brush.Color = clLime then
      Ball.Brush.Color := clSilver
    else if Ball.Brush.Color = clSilver then
      Ball.Brush.Color := clNavy
    else if Ball.Brush.Color = clNavy then
      Ball.Brush.Color := clTeal
    else if Ball.Brush.Color = clTeal then
      Ball.Brush.Color := clBlue;
  end;
end;

procedure TEditorTab.SetShapetoHereItemClick(Sender: TObject);
var
  MyPoint: TPoint;
begin
  MyPoint := ScreenToClient(FramePopupMenu.PopupPoint);
  Ball.Left := MyPoint.X;
  Ball.Top := MyPoint.Y;
end;

procedure TEditorTab.StopStartItemClick(Sender: TObject);
begin
  MoveTimer.Enabled := not MoveTimer.Enabled;
end;

{ TEditorTabHandler }

constructor TEditorTabHandler.Create(const Filename: string);
begin
  inherited Create;
  FFileName := Filename;
end;

procedure TEditorTabHandler.CloseAllCalled(var ShouldClose: Boolean);
begin
  ShouldClose := True;
end;

procedure TEditorTabHandler.ConfigurePanel(StatusBar: TStatusBar;
  Panel: TStatusPanel);
begin
  Panel.Style := psText;
  Panel.Text := '';
end;

procedure TEditorTabHandler.DeselectView;
begin
  FEditorTab.MoveTimer.Enabled := False;
end;

procedure TEditorTabHandler.DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
   //no owner-drawn panels to deal with
end;

function TEditorTabHandler.EditAction(Action: TEditAction): Boolean;
begin
   //do nothing
   Result := True;
end;

function TEditorTabHandler.GetCanCloneView: Boolean;
begin
  Result := True;
end;

function TEditorTabHandler.CloneEditorView: INTACustomEditorView;
begin
  Result := TEditorTabHandler.Create(FFilename) as INTACustomEditorView;
end;

function TEditorTabHandler.GetCaption: string;
begin
  Result := FFileName;
end;

function TEditorTabHandler.GetEditorWindowCaption: string;
begin
  Result := FFileName;
end;

function TEditorTabHandler.GetEditState: TEditState;
begin
  Result := [];
end;

function TEditorTabHandler.GetFrameClass: TCustomFrameClass;
begin
  Result := TEditorTab;
end;

procedure TEditorTabHandler.FrameCreated(AFrame: TCustomFrame);
begin
  if AFrame is TEditorTab then
  begin
    FEditorTab := TEditorTab(AFrame);
    FEditorTab.FEditorTabHandler := Self;
  end;
end;

function TEditorTabHandler.CreateFrameInstance(AOwner: TComponent): TCustomFrame;
begin
  FEditorTab := TEditorTab.Create(AOwner);
  Result := FEditorTab;
end;

function TEditorTabHandler.GetStatusPanelCount: Integer;
begin
  Result := 1;
end;

function TEditorTabHandler.GetViewIdentifier: string;
begin
  Result := cViewIdentifier;
end;

procedure TEditorTabHandler.LoadViewState(const Desktop: TCustomIniFile;
  const ViewDeskSection: string);
begin
  FEditorTab.Ball.Brush.Color := StringToColor(Desktop.ReadString(ViewDeskSection, 'BallColor', ColorToString(FEditorTab.Ball.Brush.Color)));
  FEditorTab.Ball.Top  := Desktop.ReadInteger(ViewDeskSection, 'Top', FEditorTab.Ball.Top);
  FEditorTab.Ball.Left  := Desktop.ReadInteger(ViewDeskSection, 'Left', FEditorTab.Ball.Left);
  FEditorTab.FVertMove  := Desktop.ReadInteger(ViewDeskSection, 'VertMove', FEditorTab.FVertMove);
  FEditorTab.FHorzMove  := Desktop.ReadInteger(ViewDeskSection, 'HorzMove', FEditorTab.FHorzMove);
end;

class procedure TEditorTabHandler.OnViewMenuClick(Sender: TObject);
var
  EditorTabHandler: TEditorTabHandler;
begin
  EditorTabHandler := TEditorTabHandler.Create('Something Cool');
  (BorlandIDEServices as IOTAEditorViewServices).ShowEditorView(EditorTabHandler as INTACustomEditorView);
end;

procedure TEditorTabHandler.SaveViewState(const Desktop: TCustomIniFile;
  const IsProject: Boolean; const ViewDeskSection: string);
begin
  Desktop.WriteString(ViewDeskSection, 'BallColor', ColorToString(FEditorTab.Ball.Brush.Color));
  Desktop.WriteInteger(ViewDeskSection, 'Top', FEditorTab.Ball.Top);
  Desktop.WriteInteger(ViewDeskSection, 'Left', FEditorTab.Ball.Left);
  Desktop.WriteInteger(ViewDeskSection, 'VertMove', FEditorTab.FVertMove);
  Desktop.WriteInteger(ViewDeskSection, 'HorzMove', FEditorTab.FHorzMove);
end;

procedure TEditorTabHandler.SelectView;
begin
  FEditorTab.MoveTimer.Enabled := True;
end;

initialization
  TEditorTab.FCount := 0;
end.
