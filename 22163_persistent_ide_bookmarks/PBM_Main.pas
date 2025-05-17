unit PBM_Main;

{==============================================================================}
{                                                                              }
{  Persistent IDE Bookmarks                                                    }
{  for Delphi 5, 6, 7                                                          }
{                                                                              }
{  Stores all IDE bookmarks in the project desktop file (.dsk) when the        }
{  project is closed. The bookmarks are restored when the project is reopened. }
{                                                                              }
{  How to install                                                              }
{    - compile and install the package                                         }
{    - make sure the option "Autosave project desktop" is enabled              }
{      (Delphi menu: Tools|Environment Options...|tab Preferences)             }
{  How to use                                                                  }
{    - all IDE bookmarks of all open units are now automatically stored when   }
{      the project(group) is closed                                            }
{  Uninstall                                                                   }
{    - remove the package from Delphi's installed packages list                }
{      (Delphi menu: Component|Install packages...                             }
{      select "Persistent IDE Bookmarks" and click Remove)                     }
{                                                                              }
{  Written by Sasan Adami - s.adami@gmx.net                                    }
{  Please contact me if you have found a bug or if you have suggestions for    }
{  improvement.                                                                }
{                                                                              }
{  Version 1.0 - 19 aug 2004                                                   }
{    - initial release                                                         }
{  Version 1.01 - 23 aug 2004                                                  }
{    - the column position of a bookmark is now also (re)stored                }
{      thanks to Anders Isaksson for reporting this                            }
{                                                                              }
{==============================================================================}

interface

{$I PBMVER.INC}

uses
  Windows, Classes, SysUtils, IniFiles, ToolsApi, DeskUtil;

implementation

procedure BuildViewList(Ini: TCustomIniFile; aList: TStrings);
var
  idx: integer;
  ModuleName, SectionName: string;
begin
  aList.Clear;
  idx := 0;
  SectionName := Format('View%d', [idx]);
  while Ini.SectionExists(SectionName) do
  begin
    ModuleName := Ini.ReadString(SectionName, 'Module', '');
    if ModuleName <> '' then
      aList.Add(Format('%s=%s', [SectionName, ModuleName]));
    Inc(idx);
    SectionName := Format('View%d', [idx]);
  end;
end;

{$IFDEF LOWERD7}
// Delphi7 has a function GetActiveProject in ToolsApi.pas
function GetActiveProject: IOTAProject;
var
  a: integer;
  ModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AProject: IOTAProject;
  AProjectGroup: IOTAProjectGroup;
begin
  Result := nil;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  Assert(ModuleServices <> nil, 'IDE error: Cannot obtain ModuleServices');

  for a := 0 to ModuleServices.ModuleCount - 1 do
  begin
    AModule := ModuleServices.Modules[a];
    if Supports(AModule, IOTAProjectGroup, AProjectGroup) then
    begin
      Result := AProjectGroup.ActiveProject;
      Break;
    end else
    if Supports(AModule, IOTAProject, AProject) then
    begin
      Result := AProject;
      Break;
    end;
  end;
end;
{$ENDIF}

function GetEditViewFromModuleName(ModuleName: string): IOTAEditView;
var
  a: integer;
  ModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  AEditor: IOTAEditor;
  ASourceEditor: IOTASourceEditor;
begin
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  Assert(ModuleServices <> nil, 'IDE error: Cannot obtain ModuleServices');

  AModule := ModuleServices.FindModule(ModuleName);
  if Assigned(AModule) then
  begin
    // enumerate all editors of the module
    for a := 0 to AModule.GetModuleFileCount - 1 do
    begin
      AEditor := AModule.GetModuleFileEditor(a);
      // is the editor a sourceeditor?
      if Supports(AEditor, IOTASourceEditor, ASourceEditor) then
      begin
        // note: no need to enumerate all the editors views; they have all the same bookmarks
        if ASourceEditor.EditViewCount > 0 then
        begin
          Result := ASourceEditor.EditViews[0];
          Break;
        end;
      end;
    end;
  end;
end;

procedure RestoreBookmark(ModuleName: string; BookmarkIndex: integer; BookmarkPos: TOTACharPos);
var
  AEditView: IOTAEditView;
  OldEditPos, NewEditPos: TOTAEditPos;
begin
  if BookmarkPos.Line > 0 then
  begin
    AEditView := GetEditViewFromModuleName(ModuleName);
    if Assigned(AEditView) then
    begin
      // remember the position in the editor
      OldEditPos := AEditView.CursorPos;

      // set the position in the editor for the bookmark
      NewEditPos.Col := BookmarkPos.CharIndex + 1; // TOTAEditPos.Col is 1-based, TOTACharPos.CharIndex is 0-based
      NewEditPos.Line := BookmarkPos.Line;
      AEditView.CursorPos := NewEditPos;

      // set the bookmark
      AEditView.BookmarkRecord(BookmarkIndex);

      // restore the position in the editor
      AEditView.CursorPos := OldEditPos;
    end;
  end;
end;

procedure BuildModuleBookmarkList(ModuleName: string; aList: TStrings);
var
  a: integer;
  AEditView: IOTAEditView;
  ACharPos: TOTACharPos;
begin
  aList.Clear;
  AEditView := GetEditViewFromModuleName(ModuleName);
  if Assigned(AEditView) then
  begin
    for a := 0 to 9 do
    begin
      ACharPos := AEditView.BookmarkPos[a];
      if ACharPos.Line > 0 then
        aList.Add(Format('%d=%d,%d', [a, ACharPos.Line, ACharPos.CharIndex + 1])); // TOTACharPos.CharIndex is zero-based
    end;
  end;
end;

function StringToCharPos(BookmarkStr: string): TOTACharPos;
var
  idx: integer;
  v1, v2: string;
begin
  BookmarkStr := Trim(BookmarkStr);  // LineNr, ColNr
  idx := Pos(',', BookmarkStr);
  v1 := Copy(BookmarkStr, 1, idx-1);
  v2 := Copy(BookmarkStr, idx+1, Length(BookmarkStr));
  if idx > 0 then
  begin
    Result.Line := StrToIntDef(v1, 0);
    Result.CharIndex := StrToIntDef(v2, 1) - 1;
  end else // no comma found: assume a linenumber only
  begin
    Result.Line := StrToIntDef(v2, 0);
    Result.CharIndex := 0;
  end;
end;

procedure PBM_DeskLoadProc(DeskFile: TCustomIniFile);
var
  a, b: integer;
  ViewList: TStrings;
  ModuleName, SectionName: string;
  BookmarkStr: string;
  BookmarkPos: TOTACharPos;
begin
  if DeskFile <> nil then
  begin
    ViewList := TStringList.Create;
    try
      BuildViewList(DeskFile, ViewList);
      for a := 0 to ViewList.Count - 1 do
      begin
        SectionName := ViewList.Names[a];
        ModuleName := ViewList.Values[SectionName];
        for b := 0 to 9 do
        begin
          BookmarkStr := DeskFile.ReadString(SectionName, Format('Bm%d', [b]), '');
          if BookmarkStr <> '' then
          begin
            BookmarkPos := StringToCharPos(BookmarkStr);
            RestoreBookmark(ModuleName, b, BookMarkPos);
          end;
        end;
      end;
    finally
      ViewList.Free;
    end;
  end;
end;

procedure PBM_DeskSaveProc(DeskFile: TCustomIniFile; IsProject: Boolean);
var
  AProject: IOTAProject;
  a, b: integer;
  ViewList, BookmarkList: TStrings;
  SectionName, ModuleName: string;
  BookmarkIndex, BookmarkPos: string;
begin
  if DeskFile <> nil then
  begin
    if IsProject then
    begin
      AProject := GetActiveProject;
      if Assigned(AProject) then
      begin
        // How to determine what module corresponds to what Editor View in the dsk file?
        // We build a list of Views from the DeskFile and find the corresponding module from there
        ViewList := TStringList.Create;
        try
          BuildViewList(DeskFile, ViewList);
          for a := 0 to ViewList.Count - 1 do
          begin
            SectionName := ViewList.Names[a];
            ModuleName := ViewList.Values[SectionName];
            BookmarkList := TStringList.Create;
            try
              BuildModuleBookmarkList(ModuleName, BookmarkList);
              for b := 0 to BookmarkList.Count - 1 do
              begin
                BookmarkIndex := BookmarkList.Names[b];
                BookmarkPos := BookmarkList.Values[BookmarkIndex];
                DeskFile.WriteString(SectionName, Format('Bm%s', [BookmarkIndex]), BookmarkPos);
              end;
            finally
              BookmarkList.Free;
            end;
          end;
        finally
          ViewList.Free;
        end;
      end;
    end;
  end;
end;

initialization
  DeskUtil.RegisterDesktopProcs(PBM_DeskLoadProc, PBM_DeskSaveProc);

finalization
  DeskUtil.UnregisterDesktopProcs(PBM_DeskLoadProc, PBM_DeskSaveProc);

end.
