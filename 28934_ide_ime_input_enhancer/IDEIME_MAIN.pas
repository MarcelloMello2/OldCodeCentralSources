unit IDEIME_MAIN;

(*

  IDE 한글 입력 도우미. 만든이 : 김도완

  Undo 버퍼라던가 밑줄만으로 IME의 상태를 나타낼 수 있고
  문법강조 부분에서는 IME 조립 중에 캐럿이 보인다던가 하는
  풀기 힘든 문제들이 있습니다;

  델파이 2006/XP에서 만들어졌습니다. Notifier뼈대는
  codecentral의 17217의 예제의 소스를 참고해서 붙였습니다.

  구버전에서는 SetImeText를 수정하면 아마도 사용할 수 있으리라 봅니다.
  UTF-8을 버퍼로 사용하는 델파이등의 IDE에서 사용할 수 있습니다.

  라이센스 : 프리웨어.

   (수정1) 2개 이상의 창이 열렸을 때 IDE를 닫으면 AV가 발생하는 문제해결.
   (수정2) 간소화 해서 안정화.
   (수정3) DC를 필요할 때만 생성하게 해서 더 안정화.
   (수정4) WM_Destroy로 창이 닫힐 때 핸들러 복구하도록 추가.
   (수정5) IDEHandlers 제거 버그.
   (수정6) 델파이 7에서 사용가능할 수 있도록 버전 문자열 추가.
           특정 위치에서 한글 입력시 글자가 깨지는 문제 해결.
   (수정7) 득정 위치의 한글 깨짐 방법의 작은 수정.
   (수정8) 편집기를 닫을 때 리소스 해제(WM_Destroy가 메시지 호출이 없는 것을 확인)
   (수정9) 델파이 6에서 IDE자체 문제로 AV가 발생하는 부분의 exception처리.
   (수정10) 이미 있는 창에 핸들러를 확인해서 처리하도록 수정.
   (수정11) 보다 안정적인 종료를 위해 수정.

   델파이 2006 IDE의 버그로 특정조건에서 한글을 입력 위치보다 캐럿이
   멀리 보이는 문제가 있습니다. 이 도우미를 제거해도 마찬가지로 IDE의
   자체 버그입니다.

   델파이 6이상에서 사용 가능하리라 추정.
*)

interface

// 델파이 2005 이상이면 UTF8로 문자열 처리.
{$IFDEF CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=17}
    {$DEFINE IDE_UTF8}
  {$ifend}
{$ENDIF}

{.$DEFINE DEBUG_MSG}

uses
  Windows, Classes, SysUtils, ToolsAPI, Messages;

type
  TIMEHandler = class
  private
    FEditView: IOTAEditView;
    WHandle: THandle;
    oldWndProc: Pointer;
    NewWndProc: Pointer;
    LastImeLen: Integer;

    procedure WndProc(var Msg: TMessage);
    procedure WMIMEStartComposition(var Msg: TMessage);
    procedure WMIMEComposition(var Msg: TMessage);
    procedure DrawUnderLine;
  public
    constructor Create(const View:IOTAEditView);
    destructor Destroy; override;
    procedure ReleaseResources;
    property EditView:IOTAEditView read FEditView;
  end;
  
  TSourceEditorNotifier = class(TNotifierObject, IOTANotifier, IOTAEditorNotifier)
  private
    FEditor: IOTASourceEditor;
    FIndex: Integer;

    { IOTANotifier }
    procedure Destroyed;

    { IOTAEditorNotifier }
    procedure ViewActivated(const View: IOTAEditView);
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);

  public
    constructor Create(AEditor: IOTASourceEditor);
    destructor Destroy; override;
  end;

  TIDENotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  private
    { IOTAIDENotifier }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
  end;

procedure Register;

implementation

uses
  Controls, Graphics, Forms, Imm;

var
  SourceEditorNotifiers: TList = nil;
  IDEIMEHanders: TList = nil;
  IDENotifierIndex: Integer = -1;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF DEBUG_MSG}
Procedure OutputMessage(strText : String);

begin
  (BorlandIDEServices As IOTAMessageServices).AddTitleMessage(strText);
end;
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

procedure ClearSourceEditorNotifiers;

var
  I: Integer;

begin
  if Assigned(SourceEditorNotifiers) then
    if SourceEditorNotifiers.Count>0 then
    for I := SourceEditorNotifiers.Count - 1 downto 0 do
      // Destroyed calls RemoveNotifier which in turn releases the instance
      TSourceEditorNotifier(SourceEditorNotifiers[I]).Destroyed;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure ClearIDEIMEHandlers;
begin
  // 모든 핸들러를 제거
  if Assigned(IDEIMEHanders) then begin
    while IDEIMEHanders.Count>0 do
      TIMEHandler(IDEIMEHanders.Last).Free;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure RemoveIDEIMEHandler(EditView:IOTAEditView);
var
  ii : Integer;
  temp : TIMEHandler;
begin
  // EditView에 지정된 IDEIMEHandler를 해제.
  if Assigned(IDEIMEHanders) then begin
    if IDEIMEHanders.Count>0 then
    for ii:=IDEIMEHanders.Count-1 downto 0 do begin
      temp := TIMEHandler(IDEIMEHanders[ii]);
      if temp<>nil then
      if (temp.EditView as IUnknown)=(EditView as IUnknown) then begin
        temp.Free;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function IsExistIDEIMEHandler(EditView:IOTAEditView):Boolean;
var
  ii : Integer;
  temp : TIMEHandler;
begin
  Result := False;
  // EditView가 들어간 IDEIME 찾기.
  if Assigned(IDEIMEHanders) then begin
    if IDEIMEHanders.Count>0 then
    for ii:=IDEIMEHanders.Count-1 downto 0 do begin
      temp := TIMEHandler(IDEIMEHanders[ii]);
      if temp<>nil then
      if (temp.EditView as IUnknown)=(EditView as IUnknown) then begin
        Result:=True;
        Break;
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure InstallSourceEditorNotifiers(Module: IOTAModule);

var
  I: Integer;
  SourceEditor: IOTASourceEditor;

begin
  for I := 0 to Module.ModuleFileCount - 1 do
    if Supports(Module.ModuleFileEditors[I], IOTASourceEditor, SourceEditor) then
    begin
      SourceEditorNotifiers.Add(TSourceEditorNotifier.Create(SourceEditor));
      SourceEditor := nil;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

var
  Services: IOTAServices;
  ModuleServices: IOTAModuleServices;
  EditorServices: IOTAEditorServices;
  EditorTopView: IOTAEditView;
  I, J: Integer;

begin
  SourceEditorNotifiers := TList.Create;
  IDEIMEHanders := TList.Create;

  // install IDE notifier so that we can install editor notifiers for any newly opened module
  Services := BorlandIDEServices as IOTAServices;
  IDENotifierIndex := Services.AddNotifier(TIDENotifier.Create);

  // install editor notifiers for all currently open modules
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices.ModuleCount = 0 then 
    Exit;
  for I := 0 to ModuleServices.ModuleCount - 1 do
    InstallSourceEditorNotifiers(ModuleServices.Modules[I]);

  // hook currently active module
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  if not Assigned(EditorServices) then
    Exit;

  try
  // if no editors in delphi 6, AV occured IOTAEditorServices.TopView  
  EditorTopView := EditorServices.TopView;
  if not Assigned(EditorTopView) then
    Exit;

  for I := 0 to SourceEditorNotifiers.Count - 1 do
    with TSourceEditorNotifier(SourceEditorNotifiers[I]) do
      for J := 0 to FEditor.EditViewCount - 1 do
        if FEditor.EditViews[J] = EditorTopView then
        begin
          ViewActivated(EditorTopView);
          Exit;
        end;
  except
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RemoveIDENotifier;

var
  Services: IOTAServices;

begin
  Services := BorlandIDEServices as IOTAServices;
  if Assigned(Services) then
    Services.RemoveNotifier(IDENotifierIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSourceEditorNotifier private: IOTANotifier }

//----------------------------------------------------------------------------------------------------------------------

procedure TSourceEditorNotifier.Destroyed;

begin
  FEditor.RemoveNotifier(FIndex);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSourceEditorNotifier private: IOTAEditorNotifier }

//----------------------------------------------------------------------------------------------------------------------

procedure TSourceEditorNotifier.ViewActivated(const View: IOTAEditView);
var
  EditWindow: INTAEditWindow;
  EditWindowForm: TCustomForm;
begin
  EditWindow := View.GetEditWindow;
  if not Assigned(EditWindow) then
    Exit;

  EditWindowForm := EditWindow.Form;
  if not Assigned(EditWindowForm) then
    Exit;

  if not IsExistIDEIMEHandler(View) then begin
    ClearIDEIMEHandlers;
    TIMEHandler.Create(View);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TSourceEditorNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  if Operation=opRemove then
    RemoveIDEIMEHandler(View);
end;

//----------------------------------------------------------------------------------------------------------------------

{ TSourceEditorNotifier public }

//----------------------------------------------------------------------------------------------------------------------

constructor TSourceEditorNotifier.Create(AEditor: IOTASourceEditor);

begin
  inherited Create;
  FEditor := AEditor;
  FIndex := FEditor.AddNotifier(Self);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TSourceEditorNotifier.Destroy;

begin
  SourceEditorNotifiers.Remove(Self);
  FEditor := nil;
  inherited Destroy;
end;

//----------------------------------------------------------------------------------------------------------------------

{ TIDENotifier private: IOTAIDENotifier }

//----------------------------------------------------------------------------------------------------------------------

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);

begin
  // do nothing
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);

begin
  // do nothing
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TIDENotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);

var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;

begin
  case NotifyCode of
    ofnFileOpened:
      begin
        ModuleServices := BorlandIDEServices as IOTAModuleServices;
        Module := ModuleServices.FindModule(FileName);
        if Assigned(Module) then
          InstallSourceEditorNotifiers(Module);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
// 문자열을 입력하는 루틴.
procedure SetImeText(const EditView: IOTAEditView; const S: Widestring; LastImeLen, BufLen: Integer);
var
  EditPosition: IOTAEditPosition;
  EditBuffer: IOTAEditBuffer;
  tempstr : {$IFDEF IDE_UTF8}UTF8String{$ELSE}AnsiString{$ENDIF};
begin
  if Assigned(EditView) then begin
    EditPosition := EditView.Position;
    EditBuffer := EditView.Buffer;

    if not EditBuffer.IsReadOnly then begin
      //블럭이 지정되어있을 경우엔 블럭을 지운다.
      if EditView.Block.IsValid then begin
        EditView.Block.Delete;
      end else begin
        if LastImeLen>0 then begin
          // 이전에 입력받은 문자를 지웁니다.
          EditPosition.BackspaceDelete(LastImeLen);
          (*
          EditView.Block.EndBlock;
          EditPosition.MoveRelative(0,-LastImeLen);
          EditView.Block.BeginBlock;
          EditView.Block.Delete;
          *)
        end else
          // 덮어쓰기 상태에서 IME에 처음 진입한 후 첫번째 글자를 지웁니다.
          if BufLen>0 then begin
            // 줄의 마지막에서 다음 줄을 지우는 것을 방지.
            if not EditBuffer.BufferOptions.InsertMode then
              if not (EditPosition.Character in [#13,#10]) then
                EditPosition.Delete(1);
          end;
      end;
      EditView.Paint;
      // 가져온 문자열이 있으면 버퍼에 넣습니다.
      if BufLen>0 then begin
        EditView.MoveViewToCursor;      
        tempstr := {$IFDEF IDE_UTF8}UTF8Encode(S){$ELSE}AnsiString(S){$ENDIF};
        EditPosition.InsertText(tempstr);
        EditView.Paint;
      end;
    end;
  end;
end;

{ TIMEHandler }

constructor TIMEHandler.Create(const View: IOTAEditView);
var
  EditWindow: INTAEditWindow;
  EditWindowForm: TCustomForm;
  ii : Integer;
begin
  EditWindow := View.GetEditWindow;
  if not Assigned(EditWindow) then
    Exit;

  EditWindowForm := EditWindow.Form;
  if not Assigned(EditWindowForm) then
    Exit;

  WHandle := INVALID_HANDLE_VALUE;
  oldWndProc := nil;
  NewWndProc := Classes.MakeObjectInstance(WndProc);

  FEditView := View;
  for ii:=0 to EditWindowForm.ComponentCount-1 do
    if EditWindowForm.Components[ii].ClassNameIs('TEditControl') then begin
      // 핸들을 가져와서 바꿔치기 합니다.
      WHandle := TWinControl(EditWindowForm.Components[ii]).Handle;

      oldWndProc := Pointer(GetWindowLong(WHandle,GWL_WNDPROC));
      SetWindowLong(WHandle,GWL_WNDPROC,LONGINT(NewWndProc));
      {$IFDEF DEBUG_MSG}
      OutputMessage('Add WndProc');
      {$ENDIF}
      // 핸들러 리스트에 넣습니다.
      IDEIMEHanders.Add(Self);

      break;
    end;
end;

destructor TIMEHandler.Destroy;
begin
  ReleaseResources;
  IDEIMEHanders.Remove(Self);
  FEditView:=nil;  
  inherited;
end;

procedure TIMEHandler.DrawUnderLine;
var
  cp : TPoint;
  w, h, x : Integer;
  ACanvas : TCanvas;
  DC : HDC;
begin
  ACanvas := TCanvas.Create;
  try
    GetCaretPos(cp);
    DC := GetDC(WHandle);
    try
      ACanvas.Handle := DC;
      w := ACanvas.TextWidth('O')*2;
      h := ACanvas.TextHeight('W');
      Inc(cp.Y,h);
      x := cp.X;
      Dec(x,w);
      ACanvas.Pen.Mode := pmMaskPenNot;
      ACanvas.MoveTo(x,cp.Y);
      ACanvas.LineTo(cp.X,cp.Y);
    finally
      ReleaseDC(WHandle,DC);
    end;
  finally
    ACanvas.Free;
  end;
end;

procedure TIMEHandler.ReleaseResources;
begin
  if oldWndProc<>nil then begin
    SetWindowLong(WHandle,GWL_WNDPROC,LONGINT(oldWndProc));
    {$IFDEF DEBUG_MSG}
    OutputMessage('Remove WndProc');
    MessageBox(0,'','',MB_OK);
    {$ENDIF}
  end;
  Classes.FreeObjectInstance(NewWndProc);
end;

procedure TIMEHandler.WMIMEComposition(var Msg: TMessage);
var
  CompFlag, BufLen : Integer;
  Buf : PWideChar;
  imc : HIMC;
begin
  if Msg.LParam and GCS_COMPSTR <> 0 then
    CompFlag := GCS_COMPSTR
    else if Msg.LParam and GCS_RESULTSTR <> 0 then
      CompFlag := GCS_RESULTSTR
      else
        CompFlag := 0;
  if CompFlag<>0 then begin
    imc := ImmGetContext(WHandle);
    try
      BufLen := ImmGetCompositionStringW(imc,CompFlag,nil,0);
      GetMem(Buf,BufLen+sizeof(WideChar));
      try
        ImmGetCompositionStringW(imc,CompFlag,Buf,BufLen);
        BufLen:=BufLen shr 1;
        Buf[BufLen]:=#0;
        SetImeText(FEditView,Buf,LastImeLen,BufLen);
        LastImeLen := BufLen;
        if (BufLen=0) or (CompFlag=GCS_RESULTSTR) then begin
          LastImeLen:=0;
          ShowCaret(WHandle);
        end else begin
          DrawUnderLine;
          HideCaret(WHandle);
        end;
      finally
        FreeMem(Buf);
      end;
    finally
      ImmReleaseContext(WHandle,imc);
    end;
  end;
end;

procedure TIMEHandler.WMIMEStartComposition(var Msg: TMessage);
begin
  LastImeLen := 0;
end;

procedure TIMEHandler.WndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_IME_STARTCOMPOSITION : begin
                                WMIMEStartComposition(Msg);
                                exit;
                              end;
    WM_IME_COMPOSITION :  begin
                            WMIMEComposition(Msg);
                            exit;
                          end;
    (*
    // 호출이 안되는 것을 확인했는데 혹시 모르니 남겨둠.
    WM_DESTROY : begin
                    {$IFDEF DEBUG_MSG}
                    OutputMessage('Remove WndProc');
                    {$ENDIF}
                    SetWindowLong(WHandle,GWL_WNDPROC,Longint(oldWndProc));
                    Msg.Result := CallWindowProc(oldWndProc,WHandle,Msg.Msg,Msg.WParam,Msg.LParam);
                    oldWndProc := nil;
                    exit;
                 end;
    *)
  end;
  Msg.Result := CallWindowProc(oldWndProc,WHandle,Msg.Msg,Msg.WParam,Msg.LParam);
end;

initialization

finalization
  RemoveIDENotifier;
  ClearSourceEditorNotifiers;
  ClearIDEIMEHandlers;
  FreeAndNil(SourceEditorNotifiers);
  FreeAndNil(IDEIMEHanders);
//----------------------------------------------------------------------------------------------------------------------

end.

