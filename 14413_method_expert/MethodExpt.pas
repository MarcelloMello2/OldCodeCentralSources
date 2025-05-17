Unit MethodExpt;

Interface

Uses
  Windows, Dialogs, ExptIntf, EditIntf, Classes, ToolIntf, IStreams;

Type
  TDGHMethodExpt = Class(TIExpert)
  Private
    fMenuItem : TIMenuItemIntf;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure MenuClick(Sender : TIMenuItemIntf);
    Procedure Execute; Override;
    Function GetAuthor : String; Override;
    Function GetComment : String; Override;
    Function GetGlyph : HICON; Override;
    Function GetIDString : String; Override;
    Function GetMenuText : String; Override;
    Function GetName : String; Override;
    Function GetPage : String; Override;
    Function GetState : TExpertState; Override;
    Function GetStyle : TExpertStyle; Override;
  End;

Implementation

Uses
  SysUtils, Registry, MethodExptOutFrm;

Const
  TargetName = 'ViewWindowListItem';
  RegKey = '\Software\Seasons Fall';
  SectionName = 'DGH Method Expert';

  { TDGHMethodExpt Class Methods }

Constructor TDGHMethodExpt.Create;

Var
  Flags : TIMenuFlags;
  TargetItem : TIMenuItemIntf;
  ParentItem : TIMenuItemIntf;
  Index : Integer;
  Reg : TRegIniFile;
  ShowWin : Boolean;

Begin
  Inherited Create;
  OutputForm := TOutputForm.Create(Nil);
  Reg := TRegInifile.Create(RegKey);
  Try
    ShowWin := Reg.ReadBool(SectionName, 'Visible', True);
    With OutputForm Do
      Begin
        Top := Reg.ReadInteger(SectionName, 'Top', 200);
        Left := Reg.ReadInteger(SectionName, 'Left', 200);
        Width := Reg.ReadInteger(SectionName, 'Width', 300);
        Height := Reg.ReadInteger(SectionName, 'Height', 125);
        OutputForm.ShowProcedures := Reg.ReadBool(SectionName, 'ShowProc', True);
        OutputForm.ShowFunctions := Reg.ReadBool(SectionName, 'ShowFunc', True);
        OutputForm.ShowConstructors := Reg.ReadBool(SectionName, 'ShowConstr', True);
        OutputForm.ShowDestructors := Reg.ReadBool(SectionName, 'ShowDestr', True);
        OutputForm.ShowClasses := Reg.ReadBool(SectionName, 'ShowClass', True);
        lvListView.Columns[0].Width := Reg.ReadInteger(SectionName, 'Col1', 160);
        lvListView.Columns[1].Width := Reg.ReadInteger(SectionName, 'Col2', 100);
        lvListView.Columns[2].Width := Reg.ReadInteger(SectionName, 'Col3', 40);
        SortColumn := Reg.ReadInteger(SectionName, 'SortColumn', 0);
        OutputForm.ImplementationOnly := Boolean(Reg.ReadInteger(SectionName,
          'Implementation', Integer(True)));
        lvListViewColumnClick(Self, lvlistView.Columns[SortColumn]);
      End;
  Finally
    Reg.Free;
  End;
  If ShowWin Then
    Begin
      Flags := [mfEnabled, mfVisible, mfChecked];
      OutputForm.Show;
    End Else
      Flags := [mfEnabled, mfVisible];
  With ToolServices.GetMainMenu Do
    Begin
      TargetItem := FindMenuItem(TargetName);
      Try
        If TargetItem = Nil Then
          Raise Exception.CreateFmt('Can not find menu item, %s', [TargetName]);
        ParentItem := TargetItem.GetParent;
        Index := TargetItem.GetIndex;
        Inc(Index);
        Try
          fMenuItem := ParentItem.InsertItem(Index, 'Module Method Wizard',
            'DGHModMethodWiz', '', 0, 0, 0, Flags, MenuClick);
          OutputForm.SetMenuIntf(fMenuItem);
        Finally
          ParentItem.Free;
        End;
      Finally
        TargetItem.Free;
      End;
    End;
End;

Destructor TDGHMethodExpt.Destroy;

Var
  Reg : TRegIniFile;

Begin
  Reg := TRegInifile.Create(RegKey);
  Try
    With OutputForm Do
      Begin
        Reg.WriteBool(SectionName, 'Visible', Visible);
        Reg.WriteInteger(SectionName, 'Top', Top);
        Reg.WriteInteger(SectionName, 'Left', Left);
        Reg.WriteInteger(SectionName, 'Width', Width);
        Reg.WriteInteger(SectionName, 'Height', Height);
        Reg.WriteBool(SectionName, 'ShowProc', OutputForm.ShowProcedures);
        Reg.WriteBool(SectionName, 'ShowFunc', OutputForm.ShowFunctions);
        Reg.WriteBool(SectionName, 'ShowConstr', OutputForm.ShowConstructors);
        Reg.WriteBool(SectionName, 'ShowDestr', OutputForm.ShowDestructors);
        Reg.WriteBool(SectionName, 'ShowClass', OutputForm.ShowClasses);
        Reg.WriteInteger(SectionName, 'Col1', lvListView.Columns[0].Width);
        Reg.WriteInteger(SectionName, 'Col2', lvListView.Columns[1].Width);
        Reg.WriteInteger(SectionName, 'Col3', lvListView.Columns[2].Width);
        Reg.WriteInteger(SectionName, 'SortColumn', SortColumn);
        Reg.ReadInteger(SectionName, 'Implementation',
          Integer(OutputForm.ImplementationOnly));
      End;
  Finally
    Reg.Free;
  End;
  fMenuItem.Free;
  OutputForm.Free;
  Inherited Destroy;
End;

Procedure TDGHMethodExpt.Execute;

Begin
  { Your execution code goes here... }
End;

Function TDGHMethodExpt.GetAuthor : String;

Begin
  Result := '';
End;

Function TDGHMethodExpt.GetComment : String;

Begin
  Result := '';
End;

Function TDGHMethodExpt.GetGlyph : HICON;

Begin
  Result := 0;
End;

Function TDGHMethodExpt.GetIDString : String;

Begin
  Result := '.Module Method Wizard';
End;

Function TDGHMethodExpt.GetMenuText : String;

Begin
  Result := '';
End;

Function TDGHMethodExpt.GetName : String;

Begin
  Result := 'Module Method Wizard';
End;

Function TDGHMethodExpt.GetPage : String;

Begin
  Result := '';
End;

Function TDGHMethodExpt.GetState: TExpertState;

Begin
  Result := [];
End;

Function TDGHMethodExpt.GetStyle: TExpertStyle;

Begin
  Result := esAddin;
End;

Procedure TDGHMethodExpt.MenuClick(Sender : TIMenuItemIntf);

Var
  bMenuChecked : Boolean;

Begin
  bMenuChecked := Not (mfChecked In Sender.GetFlags);
  If bMenuChecked Then
    Sender.SetFlags([mfChecked], [mfChecked])
  Else
    Sender.SetFlags([mfChecked], []);
  If bMenuChecked Then
    OutputForm.Show
  Else
    OutputForm.Hide;
End;

End.
