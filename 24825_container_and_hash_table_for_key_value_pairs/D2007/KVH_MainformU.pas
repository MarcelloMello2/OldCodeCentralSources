unit KVH_MainformU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ActnList, PBKeyValueStoreU;

type
  TMainform = class(TForm)
    ActionList1: TActionList;
    AddAction: TAction;
    AddButton: TButton;
    ClearAction: TAction;
    ClearButton: TButton;
    DeleteAction: TAction;
    DeleteButton: TButton;
    DisplayMemo: TMemo;
    EnumerateAction: TAction;
    EnumerateButton: TButton;
    FindAction: TAction;
    FindButton: TButton;
    KeyEdit: TEdit;
    KeyLabel: TLabel;
    Messagetimer: TTimer;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    RestoreAction: TAction;
    RestoreButton: TButton;
    SaveAction: TAction;
    SaveButton: TButton;
    StatusBar: TStatusBar;
    ValueEdit: TEdit;
    ValueLabel: TLabel;
    procedure AddActionExecute(Sender: TObject);
    procedure AddActionUpdate(Sender: TObject);
    procedure ClearActionExecute(Sender: TObject);
    procedure DeleteActionExecute(Sender: TObject);
    procedure EnumerateActionExecute(Sender: TObject);
    procedure FindActionExecute(Sender: TObject);
    procedure FindActionUpdate(Sender: TObject);
    procedure MessagetimerTimer(Sender: TObject);
    procedure RestoreActionExecute(Sender: TObject);
    procedure RestoreActionUpdate(Sender: TObject);
    procedure SaveActionExecute(Sender: TObject);
  private
    FStore: IKeyValueStore;
    FStream: TMemoryStream;
    procedure ClearDesigntimeCaptions;
    procedure RestoreFromLastSession;
    procedure SaveStoreForNextSession;
  protected
    procedure RefreshView;
    function GetKey: string;
    procedure SetKey(const Value: string);
    function GetValue: string;
    procedure SetValue(const aValue: string);
    property Key: string read GetKey write SetKey;
    property Store: IKeyValueStore read FStore;
    property Value: string read GetValue write SetValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Display(const S: string; Timed: Boolean = false); overload;
    procedure Display(const Fmt: string; const A: array of const; Timed:
      Boolean = false); overload;
    function CloseQuery: Boolean; override;
  end;

var
  Mainform: TMainform;

implementation

{$R *.dfm}

const
  MessageTimeout = 30000;  // 30 seconds
  Savefile = 'store.dat';

constructor TMainform.Create(AOwner: TComponent);
begin
  inherited;
  ClearDesigntimeCaptions;
  try
    FStore := CreateKeyValueStore;
  except
    on E: Exception do begin
      Application.ShowException(E);
      raise;
    end;
  end;
  RestoreFromLastSession;
end;

destructor TMainform.Destroy;
begin
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TMainform.AddActionExecute(Sender: TObject);
begin
  Store.Add(Key, Value);
  RefreshView;
  KeyEdit.SetFocus;
end;

procedure TMainform.AddActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Key <> '') and (Value <> '');
end;

procedure TMainform.ClearActionExecute(Sender: TObject);
begin
  Store.Clear;
  RefreshView;
end;

procedure TMainform.ClearDesigntimeCaptions;
var
  I: Integer;
  Comp: TComponent;
begin
  for I := 0 to ComponentCount - 1 do begin
    Comp := Components[I];
    if Comp is TPanel then
      TPanel(Comp).Caption := ''
    else if Comp is TCustomEdit  then
      TCustomEdit(Comp).Clear;
  end; {for}
end;

function TMainform.CloseQuery: Boolean;
begin
  Result := inherited CloseQuery;
  SaveStoreForNextSession;
end;

procedure TMainform.DeleteActionExecute(Sender: TObject);
begin
  Store.Delete(Key);
  RefreshView;
end;

procedure TMainform.Display(const S: string; Timed:
  Boolean = false);
begin
  if Statusbar.SimplePanel then 
    Statusbar.SimpleText := S
  else if Statusbar.Panels.Count > 0 then
    Statusbar.Panels[0].Text := S;
  if Timed then begin
    MessageTimer.Interval := MessageTimeout;
    MessageTimer.Enabled := true
  end; {if}
end;

procedure TMainform.Display(const Fmt: string; const A: array of const; Timed:
  Boolean = false);
begin
  Display(Format(Fmt, A), Timed);
end;

procedure TMainform.EnumerateActionExecute(Sender: TObject);
var
  Enum: IKeyValueEnumerator;
  Pair: IKeyValuePair;
begin
  Enum := Store.GetEnumerator;
  DisplayMemo.Clear;
  while Enum.MoveNext do begin
    Pair := Enum.Current;
    DisplayMemo.Lines.Add(Format('Key: %s; Value: %s',[Pair.Key, Pair.Value]));
  end; {while}
end;

procedure TMainform.FindActionExecute(Sender: TObject);
var
  aValue: string;
begin
  if Store.Find(Key, aValue) then
    Value := aValue
  else begin
    Value := '';
    Display('Key not found');
  end;  
end;

procedure TMainform.FindActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Key <> '';
end;

function TMainform.GetKey: string;
begin
  Result := Trim(KeyEdit.Text);
end;

function TMainform.GetValue: string;
begin
  Result := Trim(ValueEdit.Text);
end;

procedure TMainform.MessagetimerTimer(Sender: TObject);
begin
  Messagetimer.Enabled := false;
  Display('');
end;

procedure TMainform.RefreshView;
begin
  Store.AssignTo(DisplayMemo.Lines);
end;

procedure TMainform.RestoreActionExecute(Sender: TObject);
begin
  FStream.Position := 0;
  Store.LoadFromStream(FStream);
  FreeAndNil(FStream);
  RefreshView;
end;

procedure TMainform.RestoreActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FStream);
end;

procedure TMainform.RestoreFromLastSession;
var
  Filename: string;
begin
  Filename := ExtractFilePath(ParamStr(0)) +Savefile;
  if FileExists(Filename) then begin
    Store.LoadFromFile(Filename);
    RefreshView;
  end;
end;

procedure TMainform.SaveActionExecute(Sender: TObject);
begin
  if Assigned(FStream) then
    FStream.Clear
  else
    FStream := TMemoryStream.Create;
  Store.SaveToStream(FStream);
  Display('Store saved to memory stream', true);
end;

procedure TMainform.SaveStoreForNextSession;
var
  Filename: string;
begin
  Filename := ExtractFilePath(ParamStr(0)) +Savefile;
  Store.SaveToFile(Filename);
end;

procedure TMainform.SetKey(const Value: string);
begin
  KeyEdit.Text := Value
end;

procedure TMainform.SetValue(const aValue: string);
begin
  ValueEdit.Text := aValue;
end;

end.
