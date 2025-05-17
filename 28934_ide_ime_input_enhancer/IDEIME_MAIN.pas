unit IDEIME_MAIN;

(*

  IDE �ѱ� �Է� �����. ������ : �赵��

  Undo ���۶���� ���ٸ����� IME�� ���¸� ��Ÿ�� �� �ְ�
  �������� �κп����� IME ���� �߿� ĳ���� ���δٴ��� �ϴ�
  Ǯ�� ���� �������� �ֽ��ϴ�;

  ������ 2006/XP���� ����������ϴ�. Notifier�����
  codecentral�� 17217�� ������ �ҽ��� �����ؼ� �ٿ����ϴ�.

  ������������ SetImeText�� �����ϸ� �Ƹ��� ����� �� �������� ���ϴ�.
  UTF-8�� ���۷� ����ϴ� �����̵��� IDE���� ����� �� �ֽ��ϴ�.

  ���̼��� : ��������.

   (����1) 2�� �̻��� â�� ������ �� IDE�� ������ AV�� �߻��ϴ� �����ذ�.
   (����2) ����ȭ �ؼ� ����ȭ.
   (����3) DC�� �ʿ��� ���� �����ϰ� �ؼ� �� ����ȭ.
   (����4) WM_Destroy�� â�� ���� �� �ڵ鷯 �����ϵ��� �߰�.
   (����5) IDEHandlers ���� ����.
   (����6) ������ 7���� ��밡���� �� �ֵ��� ���� ���ڿ� �߰�.
           Ư�� ��ġ���� �ѱ� �Է½� ���ڰ� ������ ���� �ذ�.
   (����7) ���� ��ġ�� �ѱ� ���� ����� ���� ����.
   (����8) �����⸦ ���� �� ���ҽ� ����(WM_Destroy�� �޽��� ȣ���� ���� ���� Ȯ��)
   (����9) ������ 6���� IDE��ü ������ AV�� �߻��ϴ� �κ��� exceptionó��.
   (����10) �̹� �ִ� â�� �ڵ鷯�� Ȯ���ؼ� ó���ϵ��� ����.
   (����11) ���� �������� ���Ḧ ���� ����.

   ������ 2006 IDE�� ���׷� Ư�����ǿ��� �ѱ��� �Է� ��ġ���� ĳ����
   �ָ� ���̴� ������ �ֽ��ϴ�. �� ����̸� �����ص� ���������� IDE��
   ��ü �����Դϴ�.

   ������ 6�̻󿡼� ��� �����ϸ��� ����.
*)

interface

// ������ 2005 �̻��̸� UTF8�� ���ڿ� ó��.
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
  // ��� �ڵ鷯�� ����
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
  // EditView�� ������ IDEIMEHandler�� ����.
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
  // EditView�� �� IDEIME ã��.
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
// ���ڿ��� �Է��ϴ� ��ƾ.
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
      //���� �����Ǿ����� ��쿣 ���� �����.
      if EditView.Block.IsValid then begin
        EditView.Block.Delete;
      end else begin
        if LastImeLen>0 then begin
          // ������ �Է¹��� ���ڸ� ����ϴ�.
          EditPosition.BackspaceDelete(LastImeLen);
          (*
          EditView.Block.EndBlock;
          EditPosition.MoveRelative(0,-LastImeLen);
          EditView.Block.BeginBlock;
          EditView.Block.Delete;
          *)
        end else
          // ����� ���¿��� IME�� ó�� ������ �� ù��° ���ڸ� ����ϴ�.
          if BufLen>0 then begin
            // ���� ���������� ���� ���� ����� ���� ����.
            if not EditBuffer.BufferOptions.InsertMode then
              if not (EditPosition.Character in [#13,#10]) then
                EditPosition.Delete(1);
          end;
      end;
      EditView.Paint;
      // ������ ���ڿ��� ������ ���ۿ� �ֽ��ϴ�.
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
      // �ڵ��� �����ͼ� �ٲ�ġ�� �մϴ�.
      WHandle := TWinControl(EditWindowForm.Components[ii]).Handle;

      oldWndProc := Pointer(GetWindowLong(WHandle,GWL_WNDPROC));
      SetWindowLong(WHandle,GWL_WNDPROC,LONGINT(NewWndProc));
      {$IFDEF DEBUG_MSG}
      OutputMessage('Add WndProc');
      {$ENDIF}
      // �ڵ鷯 ����Ʈ�� �ֽ��ϴ�.
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
    // ȣ���� �ȵǴ� ���� Ȯ���ߴµ� Ȥ�� �𸣴� ���ܵ�.
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

