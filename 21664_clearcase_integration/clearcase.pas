unit clearcase;

interface

uses ToolsApi, ToolIntf;


implementation

uses
  Windows, Dialogs, Classes, ExptIntf, Menus, ShellApi, SysUtils;

type
  TDelphiClearcase = class
  private
    FClearcaseMenu,
    FDoCheckOut,
    FUndoCheckOut,
    FDoCheckOutPasDfm,
    FDoCheckIn,
    FDoCheckInPasDfm,
    FDoCompareWithPrev,
    FDoVersionTree,
    FDoViewCheckOuts  : TIMenuItemIntf;
    procedure ExecCommand(const command: string; path: PChar = nil);
  public
    destructor Destroy;override;
    procedure DoClick(Sender: TIMenuItemIntf);
    property ClearcaseMenu: TIMenuItemIntf read FClearcaseMenu write FClearcaseMenu;
    property DoCheckOut:TIMenuItemIntf write FDoCheckOut;
    property UndoCheckOut:TIMenuItemIntf write FUndoCheckOut;
    property DoCheckOutPasDfm:TIMenuItemIntf write FDoCheckOutPasDfm;
    property DoCheckIn:TIMenuItemIntf write FDoCheckIn;
    property DoCheckInPasDfm: TIMenuItemIntf write FDoCheckInPasDfm;
    property DoCompareWithPrev:TIMenuItemIntf write FDoCompareWithPrev;
    property DoVersionTree: TIMenuItemIntf write FDoVersionTree;
    property DoViewCheckOuts: TIMenuItemIntf write FDoViewCheckOuts;
  end;

var
  dcc: TDelphiClearcase = nil;



{ TDelphiClearcase }

destructor TDelphiClearcase.Destroy;
  procedure Remove(item: TIMenuItemIntf);
  begin
    if( item = nil )then
      Exit;
    item.DestroyMenuItem;
    if( item <> nil )then
      item.Free
  end;
begin
  Remove(FDoCheckOut);
  Remove(FUndoCheckOut);
  Remove(FDoCheckOutPasDfm);
  Remove(FDoCheckIn);
  Remove(FDoCheckInPasDfm);
  Remove(FDoCompareWithPrev);
  Remove(FDoVersionTree);
//  Remove(FDoViewCheckOuts);
  Remove(FClearcaseMenu);
  inherited;
end;

procedure TDelphiClearcase.DoClick(Sender: TIMenuItemIntf);
  function GetPasDfm(const f: string): string;
  var
    aux: string;
  begin
    aux := Copy(f, 1, Length(f) - 4);
    Result := aux + '.pas' + ' ' + aux + '.dfm'
  end;

var
  command,
  fileName  : string;
begin
  fileName := ToolServices.GetCurrentFile;

  if( Sender = FDoCheckOut )then
    command := 'cleartool co ' + fileName
  else if( Sender = FUndoCheckOut )then
    command := 'cleartool unco -rm ' + fileName
  else if( Sender = FDoCheckOutPasDfm )then
    command := 'cleartool co ' + GetPasDfm(fileName)
  else if( Sender = FDoCheckIn )then
    command := 'cleartool ci ' + fileName
  else if( Sender = FDoCheckInPasDfm )then
    command := 'cleartool ci ' + GetPasDfm(fileName)
  else if( Sender = FDoCompareWithPrev )then
    command := 'cleartool diff -g -pre ' + fileName
  else if( Sender = FDoVersionTree )then
    command := 'cleartool lsvtree -g ' + fileName;

  ExecCommand(command);

  ToolServices.ReloadFile(fileName);
  
end;

procedure TDelphiClearcase.ExecCommand(const command: string; path: PChar);
var
  pi  : TProcessInformation;
  stinfo : TStartupInfo;
begin
  FillChar(stinfo, SizeOf(stinfo), 0);
  stinfo.cb := SizeOf(stinfo);

  if( CreateProcess(nil, PChar(command), nil, nil, True, CREATE_NEW_CONSOLE,
      nil, path, stinfo, pi) )then begin
    WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hProcess)
  end
end;

procedure CreateMenus;
var
  services: TIToolServices;
begin
  if( BorlandIDEServices = nil )then
    Exit;
  services := ToolServices;

  if( services = nil )then
    Exit;

  dcc := TDelphiClearcase.Create;

  dcc.ClearcaseMenu := services.GetMainMenu.GetMenuItems.InsertItem(6,
    'C&learcase', 'ClearcaseMenu', 'ClearcaseTools', 0, 0, 0,
    [mfEnabled, mfVisible], nil);

  dcc.DoCheckOut := dcc.ClearcaseMenu.InsertItem(0,
    'Check &Out', 'DoCheckOut', 'Check out current file', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.UndoCheckOut := dcc.ClearcaseMenu.InsertItem(1,
    '&Undo Check Out', 'UndoCheckOut', 'Undo the Check out of the current file', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.DoCheckOutPasDfm := dcc.ClearcaseMenu.InsertItem(2,
    'Check Out pas and dfm', 'DoCheckOutPasDfm', 'Undo the check outs', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.DoCheckIn := dcc.ClearcaseMenu.InsertItem(3,
    'Check &In', 'DoCheckIn', 'Check in current file', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.DoCheckInPasDfm:= dcc.ClearcaseMenu.InsertItem(4,
    'Check In pas and dfm', 'DoCheckInPasDfm', 'Check in current files', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.DoCompareWithPrev:= dcc.ClearcaseMenu.InsertItem(5,
    'Compare with prev', 'DoCompareWithPrev', 'Compare with previous version', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);

  dcc.DoVersionTree:= dcc.ClearcaseMenu.InsertItem(6,
    'Version Tree', 'DoVersionTree', 'Show the version tree of the file', 0, 0, 2,
    [mfEnabled, mfVisible], dcc.DoClick);


end;

procedure DestroyMenus;
begin
  if( dcc <> nil )then
    dcc.Free
end;

initialization
  CreateMenus;

finalization
  DestroyMenus
end.
