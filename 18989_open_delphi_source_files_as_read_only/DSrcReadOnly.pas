unit DSrcReadOnly;

{ IDE notifier to automatically mark edit buffers of $(DELPHI)\Source files as read-only when opened }

interface

uses
  Classes, SysUtils, ToolsAPI;

type
  TIDENotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    FSourceDir: string;
  protected
    { IOTAIDENotifier }
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  public
    constructor Create;
  end;

procedure Register;

implementation

uses
  Windows, Registry;

var
  NotifierIndex: Integer = -1;

procedure Register;
var
  Services: IOTAServices;
begin
  if Supports(BorlandIDEServices, IOTAServices, Services) then
    NotifierIndex := Services.AddNotifier(TIDENotifier.Create);
end;

procedure RemoveNotifier;
var
  Services: IOTAServices;
begin
  if (NotifierIndex <> -1) and Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    Services.RemoveNotifier(NotifierIndex);
    NotifierIndex := -1;
  end;
end;

{ TIDENotifier protected: IOTAIDENotifier }

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
var
  ModuleServices: IOTAModuleServices;
  EditBuffer: IOTAEditBuffer;
  I, J: Integer;
begin
  if (NotifyCode = ofnFileOpened) and (FSourceDir <> '') and
    (StrLIComp(PChar(FileName), PChar(FSourceDir), Length(FSourceDir)) = 0) and
    Supports(BorlandIDEServices, IOTAModuleServices, ModuleServices) then
    with ModuleServices do
      for I := 0 to ModuleCount - 1 do
        if AnsiCompareText(FileName, Modules[I].FileName) = 0 then
          with Modules[I] do
          begin
            for J := 0 to ModuleFileCount - 1 do
            begin
              EditBuffer := nil;
              if Supports(ModuleFileEditors[J], IOTAEditBuffer, EditBuffer) then
                EditBuffer.IsReadOnly := True;
            end;
            Break;
          end;
end;

{ TIDENotifier public }

constructor TIDENotifier.Create;
var
  Services: IOTAServices;
  Reg: TRegistry;
begin
  inherited Create;
  if Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    Reg := TRegistry.Create(KEY_READ);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(Services.GetBaseRegistryKey, False) then
        FSourceDir := IncludeTrailingPathDelimiter(Reg.ReadString('RootDir')) + 'Source';
    finally
      Reg.Free;
    end;
  end;
end;

initialization

finalization
  RemoveNotifier;

end.
