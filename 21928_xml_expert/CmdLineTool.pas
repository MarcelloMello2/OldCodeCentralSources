unit CmdLineTool;

interface

uses
  System.Diagnostics, System.IO;

type
  TCommandLineToolOutput = class
  private
    FDirInfo: DirectoryInfo;
    FErrorOutput: string;
    FLocalDirectoryName: string;
    FOutput: string;
  protected
    procedure CreateLocalDirectory;
  public
    constructor Create;
    procedure Close;
    function ExpandFileName(const FileName: string): string;
    function FileExists(const FileName: string): Boolean;
    function ReadFile(const FileName: string): string;
    procedure Run(const FileName, Arguments: string);
    procedure SaveFile(const FileName: string; const Content: string);
    class function UniqueDirectoryName: string;
    property ErrorOutput: string read FErrorOutput;
    property LocalDirectoryName: string read FLocalDirectoryName;
    property Output: string read FOutput;
  end;

function DelphiProviderFullName: string;

function DotNetSdkPath: string;

function XsdToolPathName: string;

implementation

uses
  Microsoft.Win32, System.Reflection;

function DelphiProviderFullName: string;
var
  R: RegistryKey;
  SharedAssembliesPath: string;
begin
  R := Registry.CurrentUser.OpenSubKey('Software\Borland\BDS\3.0\AssemblyFolders\BorlandComponents');
  SharedAssembliesPath := R.GetValue('').ToString;
  R.Close;
  Result := 'Borland.Delphi.DelphiCodeProvider,' + AssemblyName.GetAssemblyName(Path.Combine(SharedAssembliesPath, 'DelphiProvider.dll')).FullName;
end;

function DotNetSdkPath: string;
var
  R: RegistryKey;
begin
  R := Registry.LocalMachine.OpenSubKey('SOFTWARE\Microsoft\.NETFramework');
  Result := R.GetValue('sdkInstallRootv1.1').ToString;
  R.Close;
end;

function XsdToolPathName: string;
begin
  Result := Path.Combine(DotNetSdkPath, 'Bin\xsd.exe');
end;

{ TCommandLineToolOutput }

procedure TCommandLineToolOutput.Close;
var
  DirFiles: array of FileInfo;
  I: Integer;
begin
  DirFiles := FDirInfo.GetFiles;
  for I := Low(DirFiles) to High(DirFiles) do
    DirFiles[I].Delete;
  FDirInfo.Delete;
end;

constructor TCommandLineToolOutput.Create;
begin
  inherited Create;
  CreateLocalDirectory;
end;

procedure TCommandLineToolOutput.CreateLocalDirectory;
begin
  FLocalDirectoryName := UniqueDirectoryName;
  FDirInfo := Directory.CreateDirectory(FLocalDirectoryName);
end;

function TCommandLineToolOutput.ExpandFileName(const FileName: string): string;
begin
  Result := Path.Combine(FLocalDirectoryName, FileName);
end;

function TCommandLineToolOutput.FileExists(const FileName: string): Boolean;
begin
  Result := &File.Exists(ExpandFileName(FileName));
end;

function TCommandLineToolOutput.ReadFile(const FileName: string): string;
var
  Sr: StreamReader;
begin
  Sr := &File.OpenText(ExpandFileName(FileName));
  try
    Result := Sr.ReadToEnd;
  finally
    Sr.Close;
  end;
end;

procedure TCommandLineToolOutput.Run(const FileName, Arguments: string);
var
  P: Process;
  SI: ProcessStartInfo;
begin
  SI := ProcessStartInfo.Create;
  SI.FileName := FileName;
  SI.Arguments := Arguments;
  SI.UseShellExecute := False;
  SI.CreateNoWindow := True;
  SI.WindowStyle := ProcessWindowStyle.Hidden;
  SI.RedirectStandardError := True;
  SI.RedirectStandardOutput := True;
  P := Process.Create;
  try
    P.StartInfo := SI;
    P.Start;
    P.WaitForExit;
    FErrorOutput := P.StandardError.ReadToEnd;
    FOutput := P.StandardOutput.ReadToEnd;
    if P.ExitCode <> 0 then
      raise InvalidOperationException.Create(System.String.Format('Exit code = {0}', [P.ExitCode]));
  finally
    P.Close;
  end;
end;

procedure TCommandLineToolOutput.SaveFile(const FileName, Content: string);
var
  Sw: StreamWriter;
begin
  Sw := &File.CreateText(ExpandFileName(FileName));
  try
    Sw.Write(Content);
  finally
    Sw.Close;
  end;
end;

class function TCommandLineToolOutput.UniqueDirectoryName: string;
begin
  Result := Path.Combine(Path.GetTempPath, Guid.NewGuid.ToString);
end;

end.
