unit ShowPath;
// ideas from Allen Bauer "Opening Doors" article

interface

uses Forms, SysUtils, ExptIntf, ToolsAPI;

procedure Register;

implementation
var
  Index: integer;
  OriginalTitle: string;  //revert back to this of no project loaded or when disabling behaviour

type
  TShowPath = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
    {== IOTANotifier ==}
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IOTAWizards }
    procedure AfterSave;
    { This function is called immediately before the item is saved. This is not
      called for IOTAWizard }
    procedure BeforeSave;
    { The associated item is being destroyed so all references should be dropped.
      Exceptions are ignored. }
    procedure Destroyed;
    { This associated item was modified in some way. This is not called for
      IOTAWizards }
    procedure Modified;
    {== IOTAIDENotifier ==}
    { This procedure is called for many various file operations within the
      IDE }
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;
    { This function is called immediatately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    {== IOTAIDENotifier50 ==}  //not needed
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean); overload;
    { Same as AfterCompile on IOTAIDENotifier except indicates if the compiler
      was invoked due to a CodeInsight compile }
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  end;


{ TShowPath }

procedure TShowPath.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
  { Not interested in AfterCompile at this time }
end;

procedure TShowPath.AfterCompile(Succeeded: Boolean);
begin
  { Not interested in AfterCompile at this time }
end;

procedure TShowPath.AfterSave;
begin
  { Not interested in AfterSave at this time }
end;

procedure TShowPath.BeforeCompile(const Project: IOTAProject;
  IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  { Not interested in BeforeCompile at this time }
end;

procedure TShowPath.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  { Not interested in BeforeCompile at this time }
end;

procedure TShowPath.BeforeSave;
begin
  { Not interested in BeforeSave at this time }
end;

procedure TShowPath.Destroyed;
begin
  { Not interested in Destroyed at this time }
end;

procedure TShowPath.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
   case NotifyCode  of
    ofnFileOpened: begin
//      (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
//         Format('%s Opened', [FileName]));
      if ToolServices.GetProjectName = '' then
         Application.Title := OriginalTitle
      else
         Application.Title := ExtractFilePath( ToolServices.GetProjectName );
    end;
    ofnFileClosing: begin
//    (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(
//        Format('%s Closed', [FileName]));
      if ToolServices.GetProjectName = '' then
         Application.Title := OriginalTitle
      else
         Application.Title := ExtractFilePath( ToolServices.GetProjectName );
    end;
  end;
end;

procedure TShowPath.Modified;
begin
  { Not interested in Modified at this time }
end;

procedure Register;
begin
  OriginalTitle := Application.Title;
  Index := (BorlandIDEServices as IOTAServices).AddNotifier( TShowPath.Create );
  Application.Title := ExtractFilePath( ToolServices.GetProjectName );
end;

initialization
finalization
  if Assigned( Application ) then
     Application.Title := OriginalTitle;
  (BorlandIDEServices as IOTAServices).RemoveNotifier(Index);
end.
 