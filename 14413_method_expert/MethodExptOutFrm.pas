unit MethodExptOutFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ToolIntf, ComCtrls, Menus, ImgList;

type
  TOutputForm = class(TForm)
    ilImageList: TImageList;
    lvListView: TListView;
    pmPopupMenu: TPopupMenu;
    mmiProcedures: TMenuItem;
    mmiFunctions: TMenuItem;
    mmiConstructors: TMenuItem;
    mmiDestructors: TMenuItem;
    mmiClasses: TMenuItem;
    mmiN1: TMenuItem;
    mmiImplementationOnly: TMenuItem;
    procedure FormActivate(Sender: TObject);
    procedure lvListViewDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure pmPopupMenuPopup(Sender: TObject);
    procedure SetPopupMenu(Sender: TObject);
    procedure lvListViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure lvListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FormCreate(Sender: TObject);
    procedure lvListViewKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    fMenuItem : TIMenuItemIntf;
    fShowProc : Boolean;
    fShowFunc : Boolean;
    fShowConstr : Boolean;
    fShowDestr : Boolean;
    fShowClass : Boolean;
    HintWin : THintWindow;
    FSortColumn : Integer;
    FImplementationOnly : Boolean;
    Procedure SetShowProc(Value : Boolean);
    Procedure SetShowFunc(Value : Boolean);
    Procedure SetShowConstr(Value : Boolean);
    Procedure SetShowDestr(Value : Boolean);
    Procedure SetShowClass(Value : Boolean);
    Procedure lvListViewCompareByLine(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    Procedure lvListViewCompareByScope(Sender: TObject; Item1,
      Item2: TListItem; Data: Integer; var Compare: Integer);
    Procedure CMMouseLeave(var Message : TMessage); Message CM_MOUSELEAVE;
    Procedure SetSortColumn(Value : Integer);
    Procedure SetImplementationOnly(Value : Boolean);
  public
    { Public declarations }
    Property ShowProcedures : Boolean Read fShowProc Write SetShowProc;
    Property ShowFunctions : Boolean  Read fShowFunc Write SetShowFunc;
    Property ShowConstructors : Boolean  Read fShowConstr Write SetShowConstr;
    Property ShowDestructors : Boolean  Read fShowDestr Write SetShowDestr;
    Property ShowClasses : Boolean  Read fShowClass Write SetShowClass;
    Procedure AddListItem(txt : TStrings; iLine, iType : Integer);
    Function IsItemToShow(Const txt : String; bImplement : Boolean) : Integer;
    Procedure SetMenuIntf(Menu : TIMenuItemIntf);
    Property SortColumn : Integer Read FSortColumn Write SetSortColumn;
    Property ImplementationOnly : Boolean Read FImplementationOnly
      Write SetImplementationOnly;
  end;


var
  OutputForm: TOutputForm;

implementation

{$R *.DFM}

Uses
  EditIntf, ExptIntf;

Procedure TOutputForm.AddListItem(txt : TStrings; iLine, iType : Integer);

Var
  strTemp : String;
  iPos : Integer;
  lviItem : TListItem;
  iParenCount : Integer;
  iChar : Integer;

begin
  With lvListView Do
    Begin
      strTemp := Trim(txt[iLine]);
      iParenCount := 0;
      If iType In [1..4] Then
        For iChar := 1 To Length(strTemp) Do
          Begin
            If strTemp[iChar] = '(' Then
              Inc(iParenCount);
            If strTemp[iChar] = ')' Then
              Dec(iParenCount);
          End;
      If (iParenCount <> 0) And (iLine <> txt.Count) Then
        strTemp := strTemp + #32 + Trim(txt[iLine + 1]);
      lviItem := Items.Add;
      lviItem.ImageIndex := iType - 1;
      If iType In [1..4, 7] Then
        Begin
          iPos := Pos(#32, strTemp);
          If iPos <> 0 Then
            strTemp := Copy(strTemp, iPos + 1, Length(strTemp) - iPos);
          iPos := Pos('.', strTemp);
          If iPos = 0 Then
            Begin
              lviItem.Caption := strTemp;
              lviItem.SubItems.Add('');
            End Else
            Begin
              lviItem.Caption := Copy(strTemp, iPos + 1, Length(strTemp) - iPos);
              lviItem.SubItems.Add(Copy(strTemp, 1, iPos - 1));
            End;
        End Else
        Begin
          lviItem.Caption := strTemp;
          lviItem.SubItems.Add('');
        End;
      lviItem.SubItems.Add(IntToStr(iLine));
    End;
End;

procedure TOutputForm.FormActivate(Sender: TObject);

Var
  ModIntf : TIModuleInterface;
  EditIntf : TIEditorInterface;
  EditReader : TIEditReader;
  Buf : String;
  Editor : String;
  iRead : Integer;
  strList : TStringList;
  iLine : Integer;
  bImplement : Boolean;
  iPos : Integer;
  iIndex : Integer;
  iType : Integer;
  strFile : String;

Const
  iSize = 31 * 1024;

begin
  bImplement := Not ImplementationOnly;
  If lvListView.Selected <> Nil Then
    iIndex := lvListView.Selected.Index
  Else
    iIndex := -1;
  lvListView.Items.BeginUpdate;
  Try
    lvListView.Items.Clear;
    With Toolservices Do
      Begin
        strFile := GetCurrentFile;
        If Uppercase(ExtractFileExt(strFile)) = '.DFM' Then
          strFile := ChangeFileExt(strFile, '.PAS');
        ModIntf := GetModuleInterface(strFile);
        If ModIntf <> Nil Then
          Caption := 'Methods : ' + ExtractFileName(GetCurrentFile)
        Else
          Caption := 'Methods';
      End;
    If ModIntf <> Nil Then
      Try
        EditIntf := ModIntf.GetEditorInterface;
        If EditIntf <> Nil Then
          Try
            EditReader := EditIntf.CreateReader;
            If EditReader <> Nil Then
              Try
                SetLength(Buf, iSize + 1);
                iPos := 0;
                Editor := '';
                iRead := EditReader.GetText(iPos, PChar(Buf), iSize);
                SetLength(Buf, iRead);
                Editor := Buf;
                While iRead = iSize Do
                  Begin
                    Inc(iPos, iRead);
                    SetLength(Buf, Length(Buf) + iSize + 1);
                    iRead := EditReader.GetText(iPos, PChar(Buf), iSize);
                    SetLength(Buf, iRead);
                    Editor := Editor + Buf;
                  End;
                strList := TStringList.Create;
                Try
                  strList.Text := Editor;
                  For iLine := 0 To strList.Count - 1 Do
                    Begin
                      If Lowercase(Trim(strList[iLine])) = 'implementation' Then
                        bImplement := True;
                      iType := IsItemToShow(strList[iLine], bImplement);
                      If iType <> 0 Then
                        AddListItem(strList, iLine, iType);
                    End;
                Finally
                  strList.Free;
                End;
              Finally
                EditReader.Free;
              End;
          Finally
            EditIntf.Free;
          End;
      Finally
        ModIntf.Free;
      End;
  Finally
    lvListView.Items.EndUpdate;
  End;
  If (iIndex > -1) And (lvListView.Items.Count > 0) Then
    Begin
      If iIndex < lvListView.Items.Count Then
        lvListView.Items[iIndex].Selected := True
      Else
        lvListView.Items[lvListView.Items.Count - 1].Selected := True;
      If lvListView.Selected <> Nil Then
        lvListView.Selected.MakeVisible(False);
    End;
end;

Function TOutputForm.IsItemToShow(Const txt : String; bImplement : Boolean) : Integer;

Var
  strTemp : String;

Const
  strFunction = 'function';
  strProcedure = 'procedure';
  strConstructor = 'constructor';
  strDestructor = 'destructor';

  Function IsClass(Const txt : String) : Boolean;

  Var
    iChar : Integer;
    iIdent : Integer;
    bStr : Boolean;
    strTemp : String;
    bStartComp : Boolean;

  Const
    ClassIdent = '=class(';

  Begin
    Result := False;
    bStr := False;
    strTemp := '';
    bStartComp := False;
    iIdent := 1;
    For iChar := 1 To Length(txt) Do
      Begin
        If txt[iChar] = '''' Then
          bStr := Not bStr;
        If Not bStr And (txt[iChar] = '=') Then
          bStartComp := True;
        If Not bStr And bStartComp Then
          Case txt[iChar] Of
            #32 : Continue;
          Else
            If txt[iChar] = ClassIdent[iIdent] Then
              Begin
                Inc(iIdent);
                If iIdent = 8 Then
                  Begin
                    Result := True;
                    Exit;
                  End;
              End Else
              Begin
                iIdent := 1;
                bStartComp := False;
              End;
          End;
      End;
  End;

  function IsMethod(Const txt : String; Const MethodIdent : String) : Boolean;

  Var
    iChar : Integer;
    iIdent : Integer;
    iLen : Integer;

  Begin
    Result := False;
    iIdent := 1;
    iLen := Length(MethodIdent);
    For iChar := 1 To Length(txt) Do
      Case txt[iChar] Of
        #32 : Continue
      Else
        If txt[iChar] = MethodIdent[iIdent] Then
          Begin
            Inc(iIdent);
            If iIdent = iLen + 1 Then
              Begin
                Result := True;
                Exit;
              End;
          End Else
            Exit;
      End;
  End;

Begin
  Result := 0;
  strTemp := Lowercase(txt);
  If ShowFunctions And bImplement And IsMethod(strTemp, strFunction) Then
    Result := 1;
  If ShowProcedures And bImplement And IsMethod(strTemp, strProcedure) Then
    Result := 2;
  If ShowConstructors And bImplement And IsMethod(strTemp, strConstructor) Then
    Result := 3;
  If ShowDestructors And bImplement And IsMethod(strTemp, strDestructor) Then
    Result := 4;
  If ShowClasses And IsClass(strTemp) Then
    Result := 5;
End;

procedure TOutputForm.lvListViewDblClick(Sender: TObject);

Var
  ModIntf : TIModuleInterface;
  EditIntf : TIEditorInterface;
  View : TIEditView;
  EditPos : TEditPos;
  iLine : Integer;
  iPos : Integer;
  strFile : String;
  i : Integer;

begin
  If lvListView.Selected <> Nil Then
    iLine := StrToInt(lvListView.Selected.SubItems[1]) + 1
  Else
    Exit;
  With Toolservices Do
    Begin
      strFile := GetCurrentFile;
      If Uppercase(ExtractFileExt(strFile)) = '.DFM' Then
        strFile := ChangeFileExt(strFile, '.PAS');
      ModIntf := GetModuleInterface(strFile);
    End;
  iPos := Pos(' : ', Caption);
  If (iPos = 0) Or (ExtractFileName(ToolServices.GetCurrentFile) <>
    Copy(Caption, iPos + 3, Length(Caption) - iPos - 2)) Then
    Begin
      MessageDlg('The active module has changed, methods references are now invalid',
        mtWarning, [mbOK],0);
      Exit;
    End;
  If ModIntf <> Nil Then
    Try
      EditIntf := ModIntf.GetEditorInterface;
      If EditIntf <> Nil Then
        Try
          View := EditIntf.GetView(0);
          Try
            EditPos.Col := 1;
            EditPos.Line := iLine;
            View.TopPos := EditPos;
            View.CursorPos := EditPos;
          Finally
            View.Free;
          End;
        Finally
          EditIntf.Free;
        End;
    Finally
      ModIntf.Free;
    End;
  For i := 0 To Screen.FormCount - 1 Do
    If (Screen.Forms[i] Is TForm) And (Screen.Forms[i].Name = 'EditWindow_0') Then
      TForm(Screen.Forms[i]).BringToFront;
end;

Procedure TOutputForm.SetMenuIntf(Menu : TIMenuItemIntf);

Begin
  fMenuItem := Menu;
End;

procedure TOutputForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fMenuItem.setFlags([mfChecked], []);
end;

Procedure TOutputForm.SetShowProc(Value : Boolean);

Begin
  If Value <> fShowProc Then
    fShowProc := Value;
End;

Procedure TOutputForm.SetShowFunc(Value : Boolean);

Begin
  If Value <> fShowFunc Then
    fShowFunc := Value;
End;

Procedure TOutputForm.SetShowConstr(Value : Boolean);

Begin
  If Value <> fShowConstr Then
    fShowConstr := Value;
End;

Procedure TOutputForm.SetShowDestr(Value : Boolean);

Begin
  If Value <> fShowDestr Then
    fShowDestr := Value;
End;

Procedure TOutputForm.SetShowClass(Value : Boolean);

Begin
  If Value <> fShowClass Then
    fShowClass := Value;
End;

procedure TOutputForm.pmPopupMenuPopup(Sender: TObject);
begin
  mmiProcedures.Checked := ShowProcedures;
  mmiFunctions.Checked := ShowFunctions;
  mmiConstructors.Checked := ShowConstructors;
  mmiDestructors.Checked := ShowDestructors;
  mmiClasses.Checked := ShowClasses;
  mmiImplementationOnly.Checked := ImplementationOnly;
end;

procedure TOutputForm.SetPopupMenu(Sender: TObject);
begin
  If Sender Is TMenuItem Then
    TMenuItem(Sender).Checked := Not TMenuItem(Sender).Checked;
  ShowProcedures := mmiProcedures.Checked;
  ShowFunctions := mmiFunctions.Checked;
  ShowConstructors := mmiConstructors.Checked;
  ShowDestructors := mmiDestructors.Checked;
  ShowClasses := mmiClasses.Checked;
  ImplementationOnly := mmiImplementationOnly.Checked;
  FormActivate(Self);
end;

procedure TOutputForm.lvListViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

Var
  liItem : TListItem;
  Pnt : TPoint;
  Rect : TRect;

Const
  iTopOffset = -3;
  iNameLeftOffset = 11;
  iScopeLeftOffset = 1;

  Function ClientToScreenRect(Cont : TControl; R : TRect) : TRect;

  Begin
    Result.TopLeft := Cont.ClientToScreen(R.TopLeft);
    Result.BottomRight := Cont.ClientToScreen(R.BottomRight);
  End;

  Function MoveRect(R : TRect; P : TPoint) : TRect;

  Begin
    Result := R;
    Inc(Result.Top, P.Y);
    Inc(Result.Left, P.X);
    Inc(Result.Bottom, P.Y);
    Inc(Result.Right, P.X);
  End;

begin
  liItem := lvListView.GetItemAt(X, Y);
  If liItem <> Nil Then
    Begin
      If (X < lvListView.Columns[0].Width) And (liItem.Top > 16) And
        (lvListView.StringWidth(liItem.Caption) > lvListView.Columns[0].Width - 18) Then
        Begin
          Pnt.Y := liItem.Top + iTopOffset;
          Pnt.X := liItem.Left + iNameLeftOffset;
          Rect := HintWin.CalcHintRect(Screen.Width, liItem.Caption, Nil);
          Rect := ClientToScreenRect(lvListView, Rect);
          Rect := MoveRect(Rect, Pnt);
          HintWin.ActivateHint(Rect, liItem.Caption);
        End Else If (X > lvListView.Columns[0].Width) And (liItem.Top > 16) And
          (X < lvListView.Columns[0].Width + lvListView.Columns[1].Width) And
          (lvListView.StringWidth(liItem.SubItems[0]) > lvListView.Columns[1].Width - 12) Then
        Begin
          Pnt.Y := liItem.Top + iTopOffset;
          Pnt.X := liItem.Left + lvListView.Columns[0].Width + iScopeLeftOffset;
          Rect := HintWin.CalcHintRect(Screen.Width, liItem.SubItems[0], Nil);
          Rect := ClientToScreenRect(lvListView, Rect);
          Rect := MoveRect(Rect, Pnt);
          HintWin.ActivateHint(Rect, liItem.SubItems[0]);
        End Else
          HintWin.ReleaseHandle;
    End Else
      HintWin.ReleaseHandle;
end;

procedure TOutputForm.FormDestroy(Sender: TObject);
begin
  HintWin.Free;
end;

procedure TOutputForm.lvListViewCompareByLine(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := StrToInt(Item1.SubItems[1]) - StrToInt(Item2.SubItems[1]);
end;

procedure TOutputForm.lvListViewCompareByScope(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := AnsiStrIComp(PChar(Item1.SubItems[0] + '.' + Item1.Caption),
    PChar(Item2.SubItems[0] + '.' + Item2.Caption));
end;

procedure TOutputForm.lvListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  SortColumn := Column.Index;
  Case Column.Index Of
    1 :
      Begin
        lvListView.OnCompare := lvListViewCompareByScope;
        lvListView.SortType := stData;
      End;
    2 :
      Begin
        lvListView.OnCompare := lvListViewCompareByLine;
        lvListView.SortType := stData;
      End;
  Else
    lvListView.OnCompare := Nil;
    lvListView.SortType := stText;
  End;
  lvListView.AlphaSort;
end;

procedure TOutputForm.FormCreate(Sender: TObject);
begin
  HintWin := THintWindow.Create(OutputForm);
  HintWin.Color := clInfoBk;
  HintWin.Canvas.Font.Assign(lvListView.Font);
end;

Procedure TOutputForm.CMMouseLeave(var Message : TMessage);

Begin
  HintWin.ReleaseHandle;
End;

Procedure TOutputForm.SetSortColumn(Value : Integer);

Begin
  If Value <> FSortColumn Then
    FSortColumn := Value;
End;


procedure TOutputForm.lvListViewKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 Then
    lvListViewDblClick(Self);
end;

Procedure TOutputForm.SetImplementationOnly(Value : Boolean);

Begin
  if Value <> FImplementationOnly Then
    FImplementationOnly := Value;
End;


end.
