{ Threaded File Appender
  Desenvolvimento iniciado em 27/07/2007 por
  Walter Frederico Bauchspiess

* Recebe dados a escrever em arquivos e escreve em uma thread separada,
  respondendo assim de forma imediata;

* Permite a escrita em diversos arquivos simultâneamente;

* Suporta ser utilizada simultaneamente por threads e programas diferentes;

* Faz cache das informações a serem escritas, de forma a escrever em pacotes
  para acelerar ainda mais;

* Suporta ser utilizada em DLLs de forma que escreve as informações pendentes
  na saída destas DLLs. O mesmo ocorre quando utilizada em outras aplicações
  (não DLLs);

* Na sua finalização (saída da aplicação ou DLL), somente tenta escrever
  nos arquivos por um determinado periodo, para evitar travar no caso de
  estes arquivos estarem bloqueados para escrita por qualquer outro processo
  por um longo tempo.

}

unit ThreadedFileAppender;

interface

uses
  Classes, SysUtils, Windows, SyncObjs;

procedure ThreadAppendFile(const pFileName, pData: string);

implementation

uses
  ThreadControl;

const
  cPoolingInterval            = 777;
  cTerminatingPoolingInterval = 27;
  cTerminatingWriteRetries    = 7;

type
  TBuffer = record
    fData: string;
  end;
  PBuffer = ^TBuffer;

  TWriterThread = class(TControlledThread)
  private
    fStopEvent: THandle;
    fLock: TCriticalSection;
    fFiles: TStringList;
    function AppendToFiles: boolean;
  protected
    procedure Execute; override;
    procedure Terminate; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(pFileName: string; const pData: string);
  end;

var
  WriterThread: TWriterThread = nil;

procedure ThreadAppendFile(const pFileName, pData: string);
begin
  if WriterThread = nil then
    WriterThread := TWriterThread.Create;
  WriterThread.Add(pFileName, pData);
end;

{ TWriterThread }

procedure TWriterThread.Add(pFileName: string; const pData: string);
var
  lIndex: integer;
  lBuffer: PBuffer;
begin
  if (pFileName = '') or (pData = '') then
    exit;
  pFileName := ExpandFileName(pFileName);
  fLock.Enter;
  try
    lIndex := fFiles.IndexOf(pFileName); // searches case insensitively
    if lIndex >= 0 then
      lBuffer := PBuffer(fFiles.Objects[lIndex])
    else
    begin
      New(lBuffer);
      fFiles.AddObject(pFileName, TObject(lBuffer));
    end;
    with lBuffer^ do
      fData := fData + pData;
  finally
    fLock.Leave;
  end;
end;

function TWriterThread.AppendToFiles: boolean;
var
  lFileIndex,
  lDataLen: integer;
  lFileName, lData: string;
  lBuffer: PBuffer;
  function WriteData: boolean;
  const
    cFileMode: array[boolean] of word = (fmCreate, fmOpenReadWrite);
  begin
    try
      with TFileStream.Create(lFileName,
            cFileMode[FileExists(lFileName)] or fmShareDenyWrite) do
      try
        Seek(0, soFromEnd);
        WriteBuffer(lData[1], lDataLen);
      finally
        Free;
      end;
      Result := True;
    except
      Result := False;
    end;
  end;
begin
  lFileIndex := 0;
  repeat
    // Get data to write
    fLock.Enter;
    try
      if lFileIndex > fFiles.Count - 1 then
        break;
      lFileName := fFiles[lFileIndex];
      lBuffer := PBuffer(fFiles.Objects[lFileIndex]);
      lData := lBuffer^.fData;
    finally
      fLock.Leave;
    end;
    lDataLen := Length(lData);
    if (lDataLen > 0) and WriteData then // Data is written outside of any lock
    begin
      fLock.Enter;
      try
        System.Delete(lBuffer^.fData, 1, lDataLen);
      finally
        fLock.Leave;
      end;
    end;
    Inc(lFileIndex);
  until false;
  // Remove empty files
  fLock.Enter;
  try
    for lFileIndex := fFiles.Count - 1 downto 0 do
    begin
      lBuffer := PBuffer(fFiles.Objects[lFileIndex]);
      if lBuffer^.fData = '' then
      begin
        Dispose(lBuffer);
        fFiles.Delete(lFileIndex);
      end;
    end;
    Result := fFiles.Count = 0; // All written
  finally
    fLock.Leave;
  end;
end;

constructor TWriterThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := true;
  //Priority := tpLowest;
  fStopEvent := CreateEvent(nil, false, false, nil);
  fLock := TCriticalSection.Create;
  fFiles := TStringList.Create;
  Resume;
end;

destructor TWriterThread.Destroy;
var
  I: integer;
begin
  for I := 0 to fFiles.Count - 1 do
    Dispose(PBuffer(fFiles.Objects[I]));
  fFiles.Free;
  CloseHandle(fStopEvent);
  fLock.Free;
  inherited;
end;

procedure TWriterThread.Execute;
const
  cInterval: array[boolean] of dword =
    (cPoolingInterval, cTerminatingPoolingInterval);
var
  lAllWritten: boolean;
  lTerminatingWriteRetryCount: integer;
begin
  lTerminatingWriteRetryCount := 0;
  repeat
    WaitForSingleObject(fStopEvent, cInterval[Terminated]);
    try
      lAllWritten := AppendToFiles;
    except
      lAllWritten := False;
    end;
    if not lAllWritten and Terminated then
      Inc(lTerminatingWriteRetryCount);
  until Terminated and
    (lAllWritten or (lTerminatingWriteRetryCount > cTerminatingWriteRetries));
end;

procedure TWriterThread.Terminate;
begin
  inherited Terminate;
  SetEvent(fStopEvent);
end;

end.
