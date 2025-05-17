{== NotificationDispatcherU ===========================================}
{! <summary>
 This unit implements a singleton that can be used to send text
 notifications to registered listeners.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2007-03-15</para>
<para>Version 1.01 created 2009-01-23, added </para>
<para>Last modified       2010-10-22</para>
</history>
<remarks>
Listeners are identified by (string) names. We support a default
listener, which would usually be the main form of an application. A
listener will always receive a notification in the thread that registered
the listener with the dispatcher, even if the dispatcher's Notify method
is called from another thread.</remarks>}
{======================================================================}

unit NotificationDispatcherU;
{$INCLUDE PBDEFINES.INC}
interface

uses SysUtils;

const
  DefaultListenerName = 'MainForm';

type
  {! Prototype for the callback a listener has to implement. }
  TListenerProc = procedure (const S: string) of object;

  {! Public interface of the notification dispatcher. The standard
    implementation offered by this unit is thread-safe.}
  INotificationDispatcher = interface(IInterface)
  ['{F9DE6C54-25EC-4330-9E4A-C333FF331AAB}']
    {! <summary>Send a string to a listener</summary>
      <param name='aMessage'>is the text to send.</param>
      <param name='aListenerName'>is the name of the listener(s) to send to. If this
      string is empty the default listener is the target.</param>}
    procedure Notify(const aMessage: string; const aListenerName: string =
        ''); overload;

    {! <summary>Send a formatted string to a listener</summary>
      <param name='aMessage'>is the format string to send.</param>
      <param name='Args'>contains the format parameters to use.</param>
      <param name='aListenerName'>is the name of the listener(s) to send to. If this
        string is empty the default listener is the target.</param>}
    procedure Notify(const aMessage: string; const Args: array of const;
      const aListenerName: string = ''); overload;

    {! <summary>Add a listener to the dispatcher.</summary>
      <param name='aListenerproc'>is the method to call with a message.</param>
      <param name='aListenerName'>is the name of the listener. If this
        string is empty the default listener is the target.</param>
      <precondition>aListenerProc not nil</precondition>
      <exception cref="ENotificationDispatcherError">
        if the listener is already registered.</exception>
      <remarks>Note that it is possible to register several listeners with
        the same Name, as long as they use different callbacks.</remarks>}
    procedure Subscribe(aListenerProc: TListenerProc;
      const aListenerName: string = '');

    {! <summary>Remove a listener from the dispatcher's list.</summary>
      <param name='aListenerproc'>is the listener's callback. This
        parameter can be nil, in which case all listeners with the same
        name will be removed.</param>
      <param name='aListenerName'>is the name of the listener. If this
        string is empty the default listener is the target.</param>
      <remarks>If the listener is not found no error is raised.</remarks>}
    procedure UnSubscribe(aListenerProc: TListenerProc;
      const aListenerName: string = '');
  end;

  {! The standard dispatcher implements this interface to allow the
    thread ID of the originator thread to be added to the notification
    message. }
  INotificationDispatcherDebug = interface(IInterface)
  ['{A8100E3F-68DE-4D1D-98B6-B2509B6C2761}']
    function GetAddCurrentThreadID: Boolean;
    procedure SetAddCurrentThreadID(const Value: Boolean);
    property AddCurrentThreadID: Boolean read GetAddCurrentThreadID write
        SetAddCurrentThreadID;
  end;

  {! Exception class used to report errors in
    <see cref="INotificationDispatcher">}
  ENotificationDispatcherError = class(Exception);

{! Returns the public interface of the dispatcher.}
function NotificationDispatcher: INotificationDispatcher;

implementation

uses Classes, Contnrs, StrUtils, InterlockedOpsU, Windows,
  SyncObjs, CrossThreadMessengerU, ExecutorIntfU;

type
  TListener = class
  private
    FCreatorThreadID: Cardinal;
    FName: string;
    FNotify: TListenerProc;
  protected
  public
    constructor Create(ANotify: TListenerProc; const AName: string);
    function Matches(ANotify: TListenerProc; const aName: string): Boolean;
    procedure Notify(const aMessage: string);
    property CreatorThreadID: Cardinal read FCreatorThreadID;
  end;

  TNotificationDispatcher = class(TInterfacedObject, INotificationDispatcher,
      INotificationDispatcherDebug)
  private
    FAddCurrentThreadID: Boolean;
    FGuardian: TCriticalSection;
    FListeners: TObjectlist;
    FMessengers: TObjectlist;
    function GetAddCurrentThreadID: Boolean;
    procedure SetAddCurrentThreadID(const Value: Boolean);
  protected
    procedure AddMessenger;
    function FindListener(aListenerProc: TListenerProc; const aName:
        string; out Index: Integer): Boolean;
    function FindMessenger(ThreadID: Cardinal; out aMessenger:
        TCrossThreadMessenger): Boolean;
    procedure Notify(const aMessage: string; const aListenerName: string =
        ''); overload;
    procedure Notify(const aMessage: string; const Args: array of const;
        const aListenerName: string = ''); overload;
    procedure NotifyViaMessenger(aListener: TListener; const aMessage:
        string);
    procedure Subscribe(aListenerProc: TListenerProc; const aListenerName:
        string = '');
    procedure UnSubscribe(aListenerProc: TListenerProc; const
        aListenerName: string = '');
    property Guardian: TCriticalSection read FGuardian;
    property Listeners: TObjectlist read FListeners;
    property Messengers: TObjectlist read FMessengers;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TListenerExecutor = class(TInterfacedObject, IStatelessExecutor)
  private
    FListener: TListener;
    FMessage: string;
  public
    constructor Create(aListener: TListener; const aMessage: string);
    procedure Execute;
  end;

var
  InternalNotificationDispatcher: INotificationDispatcher = nil;

function NotificationDispatcher: INotificationDispatcher;
var
  P: TObject;
begin
  if Assigned(InternalNotificationDispatcher) then
    Result := InternalNotificationDispatcher
  else begin
    Result := TNotificationDispatcher.Create;
    Result._AddRef; // the call below does not increment the refcount!
    P:= InterlockedCompareExchangeObject(InternalNotificationDispatcher,
      TObject(Pointer(Result)), nil);
    if P <> nil then begin
      Result._Release;
      Result := InternalNotificationDispatcher;
    end; {if}
  end; {else}
end; {NotificationDispatcher}

function ListenerName(const aName: string): string;
begin
  Result := aName;
  if Result = '' then
    Result := DefaultListenerName;
end;

{== TNotificationDispatcher ===========================================}

constructor TNotificationDispatcher.Create;
begin
  inherited Create;
  FListeners := TObjectList.Create();
  FGuardian := TCriticalSection.Create();
  FMessengers := TObjectlist.Create();
end;

destructor TNotificationDispatcher.Destroy;
begin
  FreeAndNil(FMessengers);
  FreeAndNil(FGuardian);
  FreeAndNil(FListeners);
  inherited Destroy;
end;

procedure TNotificationDispatcher.AddMessenger;
var
  Messenger: TCrossThreadMessenger;
begin
  if not FindMessenger(GetCurrentThreadID, Messenger) then begin
    Messenger:= TCrossThreadMessenger.Create;
    try
      Messengers.Add(Messenger);
    except
      Messenger.Free;
      raise;
    end; {except}
  end; {if}
end;

function TNotificationDispatcher.FindListener(aListenerProc:
    TListenerProc; const aName: string; out Index: Integer): Boolean;
var
  I: Integer;
begin
  Result := false;
  Index := -1;
  for I := 0 to FListeners.Count - 1 do
    if TListener(FListeners[I]).Matches(aListenerProc, aName) then begin
      Index := I;
      Result := true;
      Break;
    end; {if}
end;

function TNotificationDispatcher.FindMessenger(ThreadID: Cardinal; out
    aMessenger: TCrossThreadMessenger): Boolean;
var
  I: Integer;
begin
  Result := false;
  aMessenger := nil;
  for I := 0 to FMessengers.Count - 1 do
    if TCrossThreadMessenger(FMessengers[I]).CreatorThreadID = ThreadID
    then begin
      Result := true;
      aMessenger := TCrossThreadMessenger(FMessengers[I]);
      Break;
    end; {if}
end;

function TNotificationDispatcher.GetAddCurrentThreadID: Boolean;
begin
  Result := FAddCurrentThreadID;
end;

procedure TNotificationDispatcher.Notify(const aMessage: string; const
    aListenerName: string = '');
var
  Name: string;
  I: Integer;
  Listener: TListener;
  ThreadID: Cardinal;
  S: string;
  Templist: TList;
begin
  Name:= ListenerName(aListenerName);
  ThreadID := GetCurrentThreadID;
  if FAddCurrentThreadID then
    S:= Format('[%d] %s', [GetCurrentThreadId, aMessage])
  else
    S:= aMessage;
  Templist:= TList.Create;
  try
    Guardian.Acquire;
    try
      for I:= 0 to FListeners.Count - 1 do begin
        Listener := TListener(FListeners[I]);
        if Listener.Matches(nil, Name) then
          Templist.Add(Listener);
      end; {for}
    finally
      Guardian.Release;
    end; {finally}
    for I:= 0 to Templist.Count - 1 do begin
      Listener := TListener(Templist[I]);
      try
        if Listener.CreatorThreadID = ThreadID then
          Listener.Notify(S)
        else
          NotifyViaMessenger(Listener, S);
      except
        // Swallow all exceptions, even though that is not recommended
        // usually. The listener may have become invalid before we get
        // to use it, but using it while holding the Guardian entails
        // a risk of deadlock if the listener's creator thread
        // calls Notify itself.
      end;
    end; {for}
  finally
    Templist.Free;
  end;
end;

procedure TNotificationDispatcher.Notify(const aMessage: string; const
    Args: array of const; const aListenerName: string = '');
begin
  {TODO: Format is not thread-safe, although collisions are unlikely.}
  Notify(Format(aMessage, Args), aListenerName);
end;

procedure TNotificationDispatcher.NotifyViaMessenger(aListener:
    TListener; const aMessage: string);
var
  Messenger: TCrossThreadMessenger;
begin
  if FindMessenger(aListener.CreatorThreadID, Messenger) then
    Messenger.QueueCall(
      TListenerExecutor.Create(aListener, aMessage) as IStatelessExecutor)
  else
    Assert(false, 'No messenger for this thread');
end;

procedure TNotificationDispatcher.SetAddCurrentThreadID(const Value: Boolean);
begin
  FAddCurrentThreadID := Value;
end;

procedure TNotificationDispatcher.Subscribe(aListenerProc:
    TListenerProc; const aListenerName: string = '');
var
  I: Integer;
  Name: string;
  Listener: TListener;
begin
  Assert(Assigned(aListenerProc));
  Name:= ListenerName(aListenerName);
  Guardian.Acquire;
  try
    if FindListener(aListenerProc, Name, I) then
      raise ENotificationDispatcherError.CreateFmt(
        'A listener named %s is already registered with the same callback.',
        [Name]);
    Listener := TListener.Create(aListenerProc, Name);
    try
      FListeners.Add(Listener);
    except
      Listener.Free;
      raise;
    end;
    AddMessenger;
  finally
    Guardian.Release;
  end; {finally}
end;

procedure TNotificationDispatcher.UnSubscribe(aListenerProc:
    TListenerProc; const aListenerName: string = '');
var
  Name: string;
  I: Integer;
begin
  Name:= ListenerName(aListenerName);
  Guardian.Acquire;
  try
    while FindListener(aListenerProc, Name, I) do begin
      FListeners.Delete(I);
      if Assigned(aListenerProc) then
        Break;
    end; {while}
  finally
    Guardian.Release;
  end; {finally}
end;

function SameMethod( M1, M2: TMethod ): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

{== TListener =========================================================}

constructor TListener.Create(ANotify: TListenerProc; const AName:
    string);
begin
  inherited Create;
  FNotify := ANotify;
  FName := AName;
  FCreatorThreadID := GetCurrentThreadID;
end;

function TListener.Matches(ANotify: TListenerProc; const aName: string):
    Boolean;
begin
  Result := SameText(FName, aName);
  if Result and Assigned(ANotify) then
    Result := SameMethod(TMethod(FNotify), TMethod(aNotify));
end;

procedure TListener.Notify(const aMessage: string);
begin
  FNotify(aMessage);
end;

{== TListenerExecutor =================================================}

constructor TListenerExecutor.Create(aListener: TListener; const
    aMessage: string);
begin
  inherited Create;
  FListener:= aListener;
  FMessage := aMessage;
end;

procedure TListenerExecutor.Execute;
begin
  FListener.Notify(FMessage);
end;

initialization
finalization
{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Finalizing NotificationDispatcherU...');
{$ENDIF}

    InternalNotificationDispatcher := nil;

{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Done finalizing NotificationDispatcherU');
{$ENDIF}
end.
