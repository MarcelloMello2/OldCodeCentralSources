unit KbdMacroMod;

interface

uses
  SysUtils, Classes, Windows, Messages, Dialogs, ImgList, Menus, Forms, Graphics, Controls, ActnList, StdCtrls,
  ComCtrls, ExtCtrls, ToolsAPI;

type
  EMacroManager = class(Exception);

  TMacro = class
  private
    FBuffer: Pointer;
    FBufferSize: Integer;
    FName: string;

    procedure SetName(const Value: string);
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure ClearBuffer;
    procedure LoadFromStream(Stream: TStream);
    procedure ReadFromOTARecord(OTARecord: IOTARecord);
    procedure SaveToStream(Stream: TStream);
    procedure WriteToOTARecord(OTARecord: IOTARecord);

    property Buffer: Pointer read FBuffer;
    property BufferSize: Integer read FBufferSize;
    property Name: string read FName write SetName;
  end;

  TMacroManager = class(TDataModule, IInterface, IOTANotifier, IOTAKeyboardBinding)
    ActionList: TActionList;
    ImageList: TImageList;
    MacroManagerDelete: TAction;
    MacroManagerOpen: TAction;
    MacroManagerPlay: TAction;
    MacroManagerRecord: TAction;
    MacroManagerSave: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure MacroManagerDeleteExecute(Sender: TObject);
    procedure MacroManagerDeleteUpdate(Sender: TObject);
    procedure MacroManagerOpenExecute(Sender: TObject);
    procedure MacroManagerPlayExecute(Sender: TObject);
    procedure MacroManagerPlayUpdate(Sender: TObject);
    procedure MacroManagerRecordExecute(Sender: TObject);
    procedure MacroManagerRecordUpdate(Sender: TObject);
    procedure MacroManagerSaveExecute(Sender: TObject);
    procedure MacroManagerSaveUpdate(Sender: TObject);
  private
    FComboBox: TComboBox;
    FMacros: TList;
    FModified: Boolean;
    FRefCount: Integer;
    FToolBar: TToolBar;

    function IInterface._AddRef = _AddRef;
    function IInterface._Release = _Release;

    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // QueryInterface already implemented in TComponent

    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAKeyboardBinding }
    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);

    function CanPlayback: Boolean;
    procedure ClearCurrentMacro;
    function CurrentMacroName: string;
    procedure ClearMacros;
    procedure ComboBoxClick(Sender: TObject);
    procedure ComboBoxDropDown(Sender: TObject);
    procedure ComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure CreateToolBar;
    procedure FocusTopEditView;
    function IsEmpty: Boolean;
    function IsPlaying: Boolean;
    function IsRecording: Boolean;
    procedure PlaybackProc(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    function StartRecording: Boolean;
    function StopRecording: Boolean;
    procedure ToggleRecordProc(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    function MacroByName(const MacroName: string): TMacro;
    class function NewInstance: TObject; override;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);

    property RefCount: Integer read FRefCount;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  RTLConsts;

const
  KbdSignature: array[0..4] of Char = 'TOMM1';
  SInternalMacroDisplay = '<Internal>';
  SInternalMacroName = 'Internal';

resourcestring
  SBindingError = 'Unable to add keyboard binding';
  SClearInternalMacro = 'Clear internal macro?';
  SControlBarNotFound = 'ControlBar not found';
  SDeleteCaption1 = 'Clear';
  SDeleteCaption2 = 'Delete';
  SDeleteHint1 = 'Clear internal macro';
  SDeleteHint2 = 'Delete macro ''%s''';
  SDeleteMacro = 'Delete macro ''%s''?';
  SMacroExists = 'Macro ''%s'' already exists. Overwrite?';
  SMacroManagerCaption = 'Macro Manager';
  SRecordCaption1 = 'Start';
  SRecordCaption2 = 'Stop';
  SRecordHint1 = 'Start recording';
  SRecordHint2 = 'Stop recording';
  SSpeedSettingHint = 'Macro Manager speed setting';

function FindDelphiAction(const AName: string): TContainedAction;
var
  DelphiActions: TCustomActionList;
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as INTAServices40 do
    DelphiActions := ActionList;
  if DelphiActions = nil then
    Exit;

  with DelphiActions do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = AName then
      begin
        Result := Actions[I];
        Break;
      end;
end;

procedure GetBlockPositions(EditBlock: IOTAEditBlock; var PosStart, PosEnd: Integer);
var
  BlockStart, BlockEnd: TOTAEditPos;
  CharStart, CharEnd: TOTACharPos;
begin
  BlockStart.Col := EditBlock.StartingColumn;
  BlockStart.Line := EditBlock.StartingRow;
  BlockEnd.Col := EditBlock.EndingColumn;
  BlockEnd.Line := EditBlock.EndingRow;
  with EditBlock as IOTAEditView40 do
  begin
    ConvertPos(True, BlockStart, CharStart);
    ConvertPos(True, BlockEnd, CharEnd);

    PosStart := CharPosToPos(CharStart);
    PosEnd := CharPosToPos(CharEnd) - 2;
  end;
end;

procedure ReadLines(EditBuffer: IOTAEditBuffer; Lines: TStrings; PosStart, PosEnd: Integer);
var
  Reader: IOTAEditReader;
  Buf: PChar;
begin
  Lines.Clear;
  Reader := EditBuffer.CreateReader;
  Buf := AllocMem(PosEnd - PosStart + 2);
  try
    Reader.GetText(PosStart, Buf, PosEnd - PosStart + 1);
    Lines.Text := Buf;
  finally
    FreeMem(Buf);
  end;
end;

procedure Register;
begin
  with BorlandIDEServices as IOTAKeyboardServices do
    AddKeyboardBinding(TMacroManager.Create(nil));
end;

procedure RegisterActions(WizardActions: TCustomActionList);
var
  NTAServices: INTAServices;
  DelphiActions: TCustomActionList;
  NewImageIndex: Integer;
  Action: TCustomAction;
  Bitmap: TBitmap;
begin
  NTAServices := BorlandIDEServices as INTAServices;
  DelphiActions := NTAServices.ActionList;

  with WizardActions do
    if Assigned(Images) then
    begin
      while ActionCount > 0 do
      begin
        if Actions[0] is TCustomAction then
        begin
          Action := TCustomAction(Actions[0]);
          with Action do
          begin
            Bitmap := TBitmap.Create;
            try
              Bitmap.Height := Images.Height;
              Bitmap.Width := Images.Width;
              Images.GetBitmap(ImageIndex, Bitmap);
              NewImageIndex := NTAServices.AddMasked(Bitmap, clWhite, Name + 'Image');

              ActionList := DelphiActions;
              ImageIndex := NewImageIndex;
            finally
              Bitmap.Free;
            end;
          end;
        end;
      end;
    end
    else
      while ActionCount > 0 do
        Actions[0].ActionList := DelphiActions;
end;

function RegisterActionWithImageIndex(WizardAction: TCustomAction; const DelphiActionName: string): Boolean;
var
  DelphiAction: TContainedAction;
begin
  Result := False;
  DelphiAction := FindDelphiAction(DelphiActionName);
  if DelphiAction = nil then
    Exit;
  with WizardAction do
  begin
    ActionList := (BorlandIDEServices as INTAServices40).ActionList;
    if DelphiAction is TCustomAction then
    begin
      ImageIndex := TCustomAction(DelphiAction).ImageIndex;
      Result := True;
    end
    else
      ImageIndex := -1;
  end;
end;
                                                    
procedure WriteLines(EditBuffer: IOTAEditBuffer; Lines: TStrings; PosStart: Integer);
var
  Writer: IOTAEditWriter;
  I: Integer;
begin
  Writer := EditBuffer.CreateUndoableWriter;
  Writer.CopyTo(PosStart);
  Writer.DeleteTo(PosStart + Length(Lines.Text) - 1);
  for I := 0 to Lines.Count - 1 do
  begin
    Writer.Insert(PChar(Lines[I]));
    if I < Lines.Count - 1 then
      Writer.Insert(#13#10);
  end;
end;

{ TMacro private }

procedure TMacro.SetName(const Value: string);
begin
  if Value <> FName then
  begin
    FName := Value;
  end;
end;

{ TMacro public }

constructor TMacro.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FBuffer := nil;
  FBufferSize := 0;
end;

destructor TMacro.Destroy;
begin
  ClearBuffer;
  inherited Destroy;
end;

procedure TMacro.ClearBuffer;
begin
  if Assigned(FBuffer) then
    FreeMem(FBuffer);
  FBuffer := nil;
  FBufferSize := 0;
end;

procedure TMacro.LoadFromStream(Stream: TStream);
var
  L: Integer;
begin
  ClearBuffer;
  FName := '';
  with Stream do
  begin
    ReadBuffer(L, SizeOf(Integer));
    SetLength(FName, L);
    ReadBuffer(FName[1], L);
    ReadBuffer(FBufferSize, SizeOf(Integer));
    FBuffer := AllocMem(FBufferSize);
    try
      ReadBuffer(FBuffer^, FBufferSize);
    except
      ClearBuffer;
      raise;
    end;
  end;
end;

procedure TMacro.ReadFromOTARecord(OTARecord: IOTARecord);
var
  Stream: TStreamAdapter;
begin
  if OTARecord = nil then
    Exit;
  ClearBuffer;
  Stream := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
  try
    OTARecord.WriteToStream(Stream);
    FBufferSize := Stream.Stream.Size;
    FBuffer := AllocMem(FBufferSize);
    try
      Stream.Stream.Seek(0, soFromBeginning);
      Stream.Stream.ReadBuffer(FBuffer^, FBufferSize);
    except
      ClearBuffer;
      raise;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TMacro.SaveToStream(Stream: TStream);
var
  L: Integer;
begin
  L := Length(FName);
  with Stream do
  begin
    WriteBuffer(L, SizeOf(Integer));
    WriteBuffer(FName[1], L);
    WriteBuffer(FBufferSize, SizeOf(Integer));
    WriteBuffer(FBuffer^, FBufferSize);
  end;
end;

procedure TMacro.WriteToOTARecord(OTARecord: IOTARecord);
var
  Stream: TStreamAdapter;
begin
  if OTARecord = nil then
    Exit;
  Stream := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
  try
    Stream.Stream.WriteBuffer(FBuffer^, FBufferSize);
    Stream.Stream.Seek(0, soFromBeginning);
    OTARecord.Clear;
    OTARecord.ReadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TMacroManager private }

function TMacroManager.CanPlayback: Boolean;
begin
  with BorlandIDEServices as IOTAEditorServices do
    try
      Result := Assigned(TopView) and not IsPlaying and not IsEmpty;
    except
      Result := False;
    end;
end;

procedure TMacroManager.ClearCurrentMacro;
begin
  with BorlandIDEServices as IOTAKeyboardServices do
  begin
    if Assigned(CurrentRecord) then
    begin
      if CurrentRecord.IsRecording then
        ResumeRecord;
      CurrentRecord.Clear;
      CurrentRecord.Name := SInternalMacroName;              
    end;
  end;
end;

function TMacroManager.CurrentMacroName: string;
begin
  with BorlandIDEServices as IOTAKeyboardServices do            
    if Assigned(CurrentRecord) then
      Result := CurrentRecord.Name;
end;

procedure TMacroManager.ClearMacros;
var
  I: Integer;
begin
  for I := 0 to FMacros.Count - 1 do
    TMacro(FMacros[I]).Free;
  FMacros.Clear;
  if Assigned(FComboBox) then
    FComboBox.Clear;
end;

procedure TMacroManager.ComboBoxClick(Sender: TObject);
var
  Macro: TMacro;
begin
  try // suppress AV in coride when no file is open
    with BorlandIDEServices as IOTAKeyboardServices do
    begin
      if Assigned(CurrentRecord) then
      begin
        // stop if recording
        if CurrentRecord.IsRecording then
          ResumeRecord;
        with Sender as TComboBox do
          Macro := MacroByName(Text);
        if Assigned(Macro) then
        begin
          Macro.WriteToOTARecord(CurrentRecord);
          CurrentRecord.Name := Macro.Name;
        end;
      end;
    end;
    FocusTopEditView;
  except
  end;
end;

procedure TMacroManager.ComboBoxDropDown(Sender: TObject);
var
  I: Integer;
begin
  with Sender as TComboBox do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      for I := 0 to FMacros.Count - 1 do
        Items.AddObject(TMacro(FMacros[I]).Name, FMacros[I]);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TMacroManager.ComboBoxKeyPress(Sender: TObject; var Key: Char);
var
  Macro: TMacro;
  S: string;
begin
  case Key of
    #13:
      begin
        Key := #0; // prevent beep

        with Sender as TComboBox do
        begin
          if IsEmpty then
          begin
            Text := SInternalMacroDisplay;
            SelectAll;
            Exit;
          end;
          SelectAll;
          if Text <> '' then
          begin

            Macro := MacroByName(Text);
            if Assigned(Macro) then
            begin
              if MessageDlg(Format(SMacroExists, [Macro.Name]), mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
                with BorlandIDEServices as IOTAKeyboardServices do
                  if Assigned(CurrentRecord) then
                  begin
                    CurrentRecord.Name := Text;
                    Macro.Name := Text;
                    FModified := True;
                  end;
            end
            else
            begin
              with BorlandIDEServices as IOTAKeyboardServices do
                if Assigned(CurrentRecord) then
                begin
                  CurrentRecord.Name := Text;
                  Macro := TMacro.Create(Text);
                  try
                    Macro.ReadFromOTARecord(CurrentRecord);
                    FMacros.Add(Macro);
                    FModified := True;
                  except
                    Macro.Free;
                    raise;
                  end;
                end;
            end;
          end;
        end;
      end;
    #27:
      begin
        Key := #0;
        with Sender as TComboBox, BorlandIDEServices as IOTAKeyboardServices do
          if Assigned(CurrentRecord) then
          begin
            S := CurrentRecord.Name;
            if S = SInternalMacroName then
              Text := SInternalMacroDisplay
            else
              Text := S;
            SelectAll;
          end;
      end;
  end;
end;

procedure TMacroManager.CreateToolBar;
var
  AppBuilderForm: TCustomForm;
  ControlBar: TControlBar;
  I: Integer;
  LeftPos, ToolBarHeight: Integer;
  DockToolBarClass: TControlClass;
  NTAServices40: INTAServices40;
begin
  // find controlbar
  ControlBar := nil;
  AppBuilderForm := Application.MainForm;
  if Assigned(AppBuilderForm) then
    with AppBuilderForm do
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TControlBar then
        begin
          ControlBar := TControlBar(Controls[I]);
          Break;
        end;
  if ControlBar = nil then
    raise EMacroManager.Create(SControlBarNotFound);

  // find the rightmost position in the first row, find TDockToolBar class
  DockToolBarClass := nil;
  LeftPos := 0;
  ToolBarHeight := 0;
  with ControlBar do
    for I := 0 to ControlCount - 1 do
    begin
      if (DockToolBarClass = nil) and (Controls[I].ClassNameIs('TDockToolBar')) then
      begin
        DockToolBarClass := TControlClass(Controls[I].ClassType);
        ToolBarHeight := Controls[I].Height;
      end;
      with Controls[I] do
        if (Top < RowSize) and (Left + Width > LeftPos) then
          LeftPos := Left + Width;
    end;

  if DockToolBarClass = nil then
    DockToolBarClass := TToolBar;

  if ToolBarHeight = 0 then
    ToolBarHeight := 20; 

  FToolBar := DockToolBarClass.Create(nil) as TToolBar;
  try
    FToolBar.ManualDock(ControlBar);
    FToolBar.Name := 'MacroManagerToolBar';
    FToolBar.Caption := SMacroManagerCaption;
    FToolBar.Wrapable := False;
    FToolBar.AutoSize := False;
    FToolBar.Height := ToolBarHeight;
    FToolBar.ShowHint := True;
    FToolBar.Left := LeftPos;
    FToolBar.EdgeBorders := [];
    FToolBar.DragKind := dkDock;
    FToolBar.DragMode := dmAutomatic;
    NTAServices40 := BorlandIDEServices as INTAServices40;
    if Assigned(NTAServices40) then
      FToolBar.Images := NTAServices40.ImageList;

    // create combo
    FComboBox := TComboBox.Create(FToolBar);
    with FComboBox do
    begin
      Parent := FToolBar;
      Name := 'MacroManagerComboBox';
      Hint := SSpeedSettingHint;
      Text := '';
      AutoComplete := False;
      Sorted := True;
      OnClick := ComboBoxClick;
      OnDropDown := ComboBoxDropDown;
      OnKeyPress := ComboBoxKeyPress;
    end;

    with TToolButton.Create(FToolBar) do
    begin
      Parent := FToolBar;
      Style := tbsSeparator;
    end;

    // create buttons
    for I := ActionList.ActionCount - 1 downto 0 do
      with TToolButton.Create(FToolBar) do
      begin
        Parent := FToolBar;
        Style := tbsButton;
        Action := ActionList.Actions[I];
      end;
  except
    FToolBar.Free;
    raise;
  end;
end;

procedure TMacroManager.FocusTopEditView;
var
  EditorServices: IOTAEditorServices;
  EditWindow: INTAEditWindow;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
    if Assigned(EditorServices) then
      if Assigned(EditorServices.TopView) then
      begin
        EditWindow := EditorServices.TopView as INTAEditWindow;
        if Assigned(EditWindow) then
          if Assigned(EditWindow.Form) then
            EditWindow.Form.SetFocus;
      end;
end;

function TMacroManager.IsEmpty: Boolean;
const
  EmptyMacroSize = 5;
var
  Stream: TStreamAdapter;
begin
  Result := True;
  with BorlandIDEServices as IOTAKeyboardServices do
    if Assigned(CurrentPlayback) then
    begin
      Stream := TStreamAdapter.Create(TMemoryStream.Create, soOwned);
      try
        CurrentPlayback.WriteToStream(Stream);
        Result := Stream.Stream.Size = EmptyMacroSize;
      finally
        Stream.Free;
      end;
    end;
end;

function TMacroManager.IsPlaying: Boolean;
begin
  with BorlandIDEServices as IOTAKeyboardServices do
    Result := Assigned(CurrentPlayback) and CurrentPlayback.IsPlaying;
end;

function TMacroManager.IsRecording: Boolean;
begin
  with BorlandIDEServices as IOTAKeyboardServices do
    Result := Assigned(CurrentRecord) and CurrentRecord.IsRecording;
end;

procedure TMacroManager.PlaybackProc(const Context: IOTAKeyContext; KeyCode: TShortcut;
  var BindingResult: TKeyBindingResult);
begin
  BindingResult := krHandled;
  MacroManagerPlay.Execute;
end;

function TMacroManager.StartRecording: Boolean;
begin
  Result := False;
  if not IsPlaying and not IsRecording then
    with BorlandIDEServices as IOTAKeyboardServices do
    begin
      if Assigned(CurrentRecord) then
      begin
        CurrentRecord.Clear;
        CurrentRecord.Name := SInternalMacroName;
      end;
      ResumeRecord;
      Result := True;
    end;
end;

function TMacroManager.StopRecording: Boolean;
begin
  Result := False;
  if IsRecording and not IsPlaying then
    with BorlandIDEServices as IOTAKeyboardServices do
    begin
      ResumeRecord;
      Result := True;
    end;
end;

procedure TMacroManager.ToggleRecordProc(const Context: IOTAKeyContext; KeyCode: TShortcut;
  var BindingResult: TKeyBindingResult);
begin
  BindingResult := krHandled;
  MacroManagerRecord.Execute;
end;

{ TMacroManager private: IInterface }

function TMacroManager._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TMacroManager._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TMacroManager private: IOTANotifier }

procedure TMacroManager.AfterSave;
begin
  // do nothing
end;

procedure TMacroManager.BeforeSave;
begin
  // do nothing
end;

procedure TMacroManager.Destroyed;
begin
  // do nothing
end;

procedure TMacroManager.Modified;
begin
  // do nothing
end;

{ TMacroManager private: IOTAKeyboardBinding }

function TMacroManager.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TMacroManager.GetDisplayName: string;
begin
  Result := SMacroManagerCaption;
end;

function TMacroManager.GetName: string;
begin
  Result := 'TOndrej.MacroManager';
end;

procedure TMacroManager.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
begin
  with BindingServices do
  begin
    if not AddKeyBinding([ShortCut(Ord('R'), [ssShift, ssCtrl])], ToggleRecordProc, nil) then
      raise EMacroManager.Create(SBindingError);
    if not AddKeyBinding([ShortCut(Ord('P'), [ssShift, ssCtrl])], PlaybackProc, nil) then
      raise EMacroManager.Create(SBindingError);
  end;
end;

{ TMacroManager public }

procedure TMacroManager.AfterConstruction;
begin
  inherited AfterConstruction;
  // release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TMacroManager.BeforeDestruction;
begin
  if RefCount <> 0 then
    System.Error(reInvalidPtr);
  inherited BeforeDestruction;
end;

procedure TMacroManager.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMacroManager.LoadFromStream(Stream: TStream);
var
  Signature: array[0..SizeOf(KbdSignature) - 1] of Char;
  I: Integer;
  Macro: TMacro;
begin
  ClearMacros;
  with Stream do
  begin
    ReadBuffer(Signature[0], SizeOf(Signature));
    if StrLComp(Signature, KbdSignature, SizeOf(KbdSignature)) <> 0 then
      raise EReadError.CreateRes(@SReadError);
    ReadBuffer(I, SizeOf(Integer));
    while I > 0 do
    begin
      Macro := TMacro.Create('');
      try
        Macro.LoadFromStream(Stream);
        FMacros.Add(Macro);
        if Assigned(FComboBox) then
          FComboBox.Items.Add(Macro.Name);
      except
        Macro.Free;
        raise;
      end;

      Dec(I);
    end;
  end;
end;

function TMacroManager.MacroByName(const MacroName: string): TMacro;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FMacros.Count - 1 do
    if AnsiCompareText(TMacro(FMacros[I]).Name, MacroName) = 0 then
    begin
      Result := FMacros[I];
      Break;
    end;
end;

class function TMacroManager.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TMacroManager(Result).FRefCount := 1;
end;

procedure TMacroManager.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMacroManager.SaveToStream(Stream: TStream);
var
  I: Integer;
begin
  with Stream do
  begin
    WriteBuffer(KbdSignature[0], SizeOf(KbdSignature));
    I := FMacros.Count;
    WriteBuffer(I, SizeOf(Integer));
    for I := 0 to FMacros.Count - 1 do
      TMacro(FMacros[I]).SaveToStream(Stream);
  end;
end;

{ TMacroManager event handlers }

procedure TMacroManager.DataModuleCreate(Sender: TObject);
begin
  with BorlandIDEServices as IOTAKeyboardServices do
    if Assigned(CurrentRecord) then
      CurrentRecord.Name := SInternalMacroName;
  FMacros := TList.Create;
  FToolBar := nil;
  FComboBox := nil;
  CreateToolBar;
  RegisterActionWithImageIndex(MacroManagerDelete, 'EditDeleteCommand');
  RegisterActionWithImageIndex(MacroManagerSave, 'FileSaveCommand');
  RegisterActionWithImageIndex(MacroManagerOpen, 'FileOpenCommand');
  RegisterActions(ActionList);
end;

type
  THackContainedAction = class(TContainedAction);
  THackControlActionLink = class(TControlActionLink);

procedure TMacroManager.DataModuleDestroy(Sender: TObject);
var
  I, J: Integer;
begin
  // free toolbar & its owned components
  if Assigned(FToolBar) then
    FToolBar.Free;
  FToolBar := nil;
  FComboBox := nil;
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TContainedAction then
      with THackContainedAction(Components[I]) do
      begin
        // free any remaining action clients (custom buttons)
        for J := 0 to FClients.Count - 1 do
          if Assigned(FClients[J]) and (TObject(FClients[J]) is TControlActionLink) then
            THackControlActionLink(FClients[J]).FClient.Free;
        // remove from Delphi actionlist
        ActionList := Self.ActionList;
      end;

  ClearMacros;
  FMacros.Free;
end;

procedure TMacroManager.MacroManagerDeleteExecute(Sender: TObject);
var
  S: string;
  Macro: TMacro;
begin
  S := CurrentMacroName;
  if S = SInternalMacroName then
  begin
    if MessageDlg(Format(SClearInternalMacro, [S]), mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      ClearCurrentMacro;
  end
  else
  begin
    if MessageDlg(Format(SDeleteMacro, [S]), mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    begin
      Macro := MacroByName(CurrentMacroName);
      if Assigned(Macro) then
      begin
        FMacros.Remove(Macro);
        Macro.Free;
      end;
      ClearCurrentMacro;
      FModified := True;
      if Assigned(FComboBox) then
        FComboBox.Text := SInternalMacroDisplay;
    end;
  end;
  FocusTopEditView;
end;

procedure TMacroManager.MacroManagerDeleteUpdate(Sender: TObject);
var
  S: string;
begin
  with Sender as TCustomAction do
  begin
    Enabled := not IsEmpty;
    S := CurrentMacroName;
    if S = SInternalMacroName then
    begin
      Caption := SDeleteCaption1;
      Hint := SDeleteHint1;
    end
    else
    begin
      Caption := SDeleteCaption2;
      Hint := Format(SDeleteHint2, [S]);
    end;
  end;
end;

procedure TMacroManager.MacroManagerOpenExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    if InitialDir = '' then
      InitialDir := ExtractFilePath(Application.ExeName);
    if Execute then
    begin
      LoadFromFile(FileName);
      if Assigned(FComboBox) then
        FComboBox.Text := SInternalMacroDisplay;
      FModified := False;
    end;
  end;
end;

procedure TMacroManager.MacroManagerPlayExecute(Sender: TObject);
begin
  FocusTopEditView;
  with BorlandIDEServices as IOTAKeyboardServices do
    if Assigned(CurrentPlayback) and not CurrentPlayback.IsPlaying then
      ResumePlayback;
end;

procedure TMacroManager.MacroManagerPlayUpdate(Sender: TObject);
begin
  with Sender as TCustomAction do
    Enabled := CanPlayback;
end;

procedure TMacroManager.MacroManagerRecordExecute(Sender: TObject);
begin
  with Sender as TCustomAction do
    if IsRecording then
    begin
      if StopRecording then
      begin
      end;
      FocusTopEditView;
    end
    else
    begin
      StartRecording;
      FocusTopEditView;
    end;
end;

procedure TMacroManager.MacroManagerRecordUpdate(Sender: TObject);
var
  S: string;
begin
  if Assigned(Sender) and (Sender is TCustomAction) then
    with TCustomAction(Sender) do
    begin
      try // suppress AV in coride when no file is open
        with BorlandIDEServices as IOTAEditorServices do
          Enabled := Assigned(TopView);
      except
        Enabled := False;
      end;

      Checked := IsRecording;
      if Checked then
      begin
        Caption := SRecordCaption2;
        Hint := SRecordHint2;
      end
      else
      begin
        Caption := SRecordCaption1;
        Hint := SRecordHint1;
      end;
    end;

  if Assigned(FComboBox) then
    with FComboBox do
      if not Focused then
      begin
        S := CurrentMacroName;
        if S = SInternalMacroName then
          Text := SInternalMacroDisplay
        else
          Text := S;
      end;
end;

procedure TMacroManager.MacroManagerSaveExecute(Sender: TObject);
begin
  with SaveDialog do
  begin
    if InitialDir = '' then
      InitialDir := ExtractFilePath(Application.ExeName);
    if Execute then
    begin
      SaveToFile(FileName);
      FModified := False;
    end;
  end;
end;

procedure TMacroManager.MacroManagerSaveUpdate(Sender: TObject);
begin
  with Sender as TCustomAction do
    Enabled := (FMacros.Count > 0) and FModified;
end;

end.
