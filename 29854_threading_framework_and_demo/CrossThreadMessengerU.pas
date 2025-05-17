{== CrossThreadMessengerU =============================================}
{!
<summary>
 This unit implements a class that can be used by another thread to
 send notifications to the thread that created an instance of the class.
</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2007-02-06</para>
<para>Version 1.1 created 2009-05-17, changed docs to XML, checked for
 Unicode issues, changed precondition checks to raise exceptions.</para>
<para>Last modified       2009-05-17</para>
</history>
<remarks>
 This class was designed to solve the problem that the implementation
 of AllocateHwnd and DeallocateHwnd in the Delphi RTL is not thread-safe.
 Basically we implement a way to create a window for messenging purposes
 in a thread-safe manner. For the messenger class to work the thread
 owning such an object needs to have an active message loop.</remarks>}
{======================================================================}
unit CrossThreadMessengerU;

{$INCLUDE PBDEFINES.INC}
interface

uses
  Messages, Windows, Classes, Sysutils, ExecutorIntfU;

const
  C15Seconds = 15*1000;  {Default message timeout in milliseconds}
  UM_CALLEXECUTOR = WM_USER;

type
  {!
   <remarks>
    The prototype for a method that can be used to handle exceptions
    raised during message processing in the TCrossThreadMessenger class.
    An example of a suitable hook method would be Application.ShowException.
   </remarks>
  }
  TExceptionHook = procedure (E: Exception) of Object;

  {!
   <remarks>
    Instances of this class can be used to execute code in the context
    of the instances owner thread from other threads.
   <para>
    The mechanism used is based on Windows messages, so the instances
    owner thread needs to have a message queue and an active message
    loop. The helper window required is created in a thread-safe
    manner. </para>
   <para>
    Like any code using message-handling this class also needs to
    handle exceptions that are raised by (external) code called from the
    message handler. Allowing those to propagate up the stack into
    Windows code is not a smart idea. The TCrossThreadMessenger class
    offers several ways for code using it to get aware of such
    exceptions.</para>
    <list type="bullet">
      <item>
      A hook method can be associated with a specific instance of the
      class through its ExceptionHook property. If present this method
      will be passed the exception object trapped, and it is supposed to
      handle it in a sensible manner, e.g. by logging it or showing it to
      the user. It must <b>not</b> raise it, though. Doing so may result
      in unpredictable behaviour!</item>
      <item>
      A "global" hook method can be set for all instances of the class
      by using the SetClassExceptionHook class method (I've not used a class
      property for backwards compatibility with older Delphi versions).
      If no handler has been assigned to an instances ExceptionHook
      property the global hook will be tried next. The considerations
      detailed in the last paragraph apply here as well, the hook must not
      raise the passed exception.</item>
      <item>
      If no hooks are set the object will take possession of the
      exception object and store it into its LastError property. </item>
    </list>
    </remarks>
  }
  TCrossThreadMessenger = class(TObject)
  private
    FCreatorThreadID: Cardinal;
    FErrorAddr: Pointer;
    FExceptionHook: TExceptionHook;
    FHandle: Hwnd;
    FLastError: Exception;
    procedure CallHook(Hook: TExceptionHook; E: Exception);
    procedure RegisterHelperWindowClass;
  protected
    {!
    <summary>
     Register the helper windowclass and create the helper window. </summary>
    }
    procedure CreateWindow; virtual;
    {!
    <summary>
     Destroy the helper window. </summary>
    }
    procedure DestroyWindow; virtual;
    {!
    <summary>
     Stores the passed exception object and the address it was raised
     at. Any previous stored exception object is destroyed and lost.</summary>
    <param name="Value">
     is the exception object to store. Nil can be passed to clear the
     fields used to store the data.</param>
    }
    procedure SetLastError(const Value: Exception);
    {!
    <summary>
     This is the message processing method for the helper window. </summary>
    <remarks>
     We only process the UM_CALLEXECUTOR message to call the passed
     executor, all other messages are left to the default processing
     done by HelperWindowProc, which just falls back on DefWindowProc.
     Derived classes can override this method to process other messages.
     They must always call this inherited method for messages they do not
     process, though! </remarks>
    }
    procedure WndProc(var Message: TMessage); virtual;
  public
    {!
    <summary>
     Create the helper window and remember which thread created this
     instance. </summary>
    }
    constructor Create; reintroduce; virtual;
    {!
    <summary>
     Destroy the helper window and any exception object we may have
     grabbed hold of.</summary>
    }
    destructor Destroy; override;
    {!
    <summary>
     Return the class-level exception hook. This may be nil if no hook
     was set previously. </summary>
    }
    class function GetClassExceptionHook: TExceptionHook;
    {!
    <summary>
     Post a message to the helper window. This method is usually called
     from a secondary thread. It returns after the message has been put
     into the target threads message queue.</summary>
    <returns>
     true if the message could be posted, false if the target
     thread has no message queue or the queue is full. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="lp">
     is the second message parameter. 0 should be used if the message does
     not require a parameter.</param>
    }
    function PostMessage(msg: cardinal; wp: WPARAM; lp: LPARAM): Boolean;
        overload;
    {!
    <summary>
     Post a message transporting an object to the helper window. This
     method is usually called from a secondary thread. It returns after
     the message has been put into the target threads message queue.</summary>
    <returns>
     true if the message could be posted, false if the target
     thread has no message queue or the queue is full. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="Obj">
     is the object to pass. The sender relinquishes control of this object
     to the receiver of the message. It must only free the object if the
     PostMessage call failed! A derived classes WndProc needs to be
     able to handle the received object, including freeing it when that
     is appropriate.</param>
    }
    function PostMessage(msg: cardinal; wp: WPARAM; Obj: TObject): Boolean;
        overload;
    {!
    <summary>
     Post a message transporting an interface reference to the helper
     window. This method is usually called from a secondary thread.
     It returns after the message has been put into the target threads
     message queue.</summary>
    <returns>
     true if the message could be posted, false if the target
     thread has no message queue or the queue is full. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="Intf">
     is the interface to pass. Since interface references cannot be used
     as message parameters safely due to reference counting issues we use
     a helper object to hold the reference. The receiver of the message
     has to free this object or return it to the pool it came from.</param>
    }
    function PostMessage(msg: cardinal; wp: WPARAM; const Intf:
        IInterface): Boolean; overload;
    {!
    <summary>
     Queue a method call for execution in the objects creator thread.</summary>
    <param name="aMethod">
     is the parameterless method to call. If nil is passed nothing will
     be done.</param>
    <remarks>
     The method returns after the passed method pointer has been queued
     but usually before it has been called (but that can happen as well,
     depending on thread scheduling). aMethod is thus called asynchronously.
    </remarks>
    }
    procedure QueueCall(aMethod: TThreadMethod); overload;
    {!
    <summary>
     Queue an executor for execution in the objects creator thread. </summary>
    <param name="aExecutor">
     is the executor to call. If nil is passed nothing will be done.</param>
    <remarks>
     The method returns after the passed executor has been queued
     but usually before it has been called (but that can happen as well,
     depending on thread scheduling). The executor is thus called
     asynchronously. </remarks>
    }
    procedure QueueCall(aExecutor: IStatelessExecutor); overload;
    {!
    <summary>
     If we have a last error stored, raise it in the context of the
     current thread but at the old address. If we do not have a last
     error stored the method does nothing, so don't count on it to
     not return! </summary>
    }
    procedure RaiseLastError;
    {!
    <summary>
     Send a message to the helper window and wait for it to be processed. </summary>
    <returns>
     the message result returned by the targets window proc. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="lp">
     is the second message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="timeout">
     is the time to wait for the message to be processed, in milliseconds.
     The default timeout is 15 seconds.
    <exception cref="EThreadMessageTimeoutError">
     is raised if the message timed out, usually because the receiver
     windows thread is blocked. If the call fails for other reasons a
     EWin32Error may also be raised. </exception>
    }
    function SendMessage(msg: cardinal; wp: WPARAM; lp: LPARAM; timeout:
        Cardinal = C15Seconds): LRESULT; overload;
    {!
    <summary>
     Send a message carrying an object reference to the helper window
     and wait for it to be processed. </summary>
    <returns>
     the message result returned by the targets window proc. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="Obj">
     is the object to pass. Since the operation is synchronous the sender
     can retain ownership of the object.</param>
    <param name="timeout">
     is the time to wait for the message to be processed, in milliseconds.
     The default timeout is 15 seconds.
    <exception cref="EThreadMessageTimeoutError">
     is raised if the message timed out, usually because the receiver
     windows thread is blocked. If the call fails for other reasons a
     EWin32Error may also be raised. </exception>
    }
    function SendMessage(msg: cardinal; wp: WPARAM; Obj: TObject; timeout:
        Cardinal = C15Seconds): LRESULT; overload;
    {!
    <summary>
     Send a message carrying an interface reference to the helper window
     and wait for it to be processed. </summary>
    <returns>
     the message result returned by the targets window proc. </returns>
    <param name="msg">
     identifies the message. This will typically be a user-defined message
     code.</param>
    <param name="wp">
     is the first message parameter. 0 should be used if the message does
     not require a parameter.</param>
    <param name="Intf">
     is the interface to pass. </param>
    <param name="timeout">
     is the time to wait for the message to be processed, in milliseconds.
     The default timeout is 15 seconds.
    <exception cref="EThreadMessageTimeoutError">
     is raised if the message timed out, usually because the receiver
     windows thread is blocked. If the call fails for other reasons a
     EWin32Error may also be raised. </exception>
    <remarks>
     Since interface references cannot be used as message parameters
     safely due to reference counting issues we use a helper object to
     hold the reference. The receiver of the message has to free this
     object or return it to the pool it came from. In principle we
     could also do this after the message has been send, but since the
     same message may also be posted and the receiver has no good way to
     figure out whether the message was sent or posted it is better to
     handle both cases the same.</remarks>
    }
    function SendMessage(msg: cardinal; wp: WPARAM; const Intf: IInterface;
        timeout: Cardinal = C15Seconds): LRESULT; overload;
    {!
    <summary>
     Set the class-level exception hook. </summary>
    }
    class procedure SetClassExceptionHook(const Value: TExceptionHook);
    {!
    <summary>
     Call a parameterless method in the context of the thread that
     created this instance and wait for it to return.</summary>
    <param name="aMethod">
     is the method to execute. Nothing is done if nil is passed here.</param>
    <exception cref="EThreadMessageTimeoutError">
     is raised if the call did not complete inside 15 seconds, usually
     because the receiver windows thread is blocked. If the call fails
     for other reasons a  EWin32Error may also be raised. </exception>
    }
    procedure Synchronize(aMethod: TThreadMethod); overload;
    {!
    <summary>
     Call an executor in the context of the thread that
     created this instance and wait for it to return.</summary>
    <param name="aExecutor">
     is the executor to call. Nothing is done if nil is passed here.</param>
    <exception cref="EThreadMessageTimeoutError">
     is raised if the call did not complete inside 15 seconds, usually
     because the receiver windows thread is blocked. If the call fails
     for other reasons a  EWin32Error may also be raised. </exception>
    }
    procedure Synchronize(const aExecutor: IStatelessExecutor); overload;
    {!
    <summary>
     Pass the Message record to the WndProc method and deal with any
      exception that may be raised by that method. </summary>
    <remarks>
     If there is an exception we will pass it to the instances
     ExceptionHook, if there is one, otherwise to the class exception hook,
     if there is one, otherwise just grab hold of the exception object
     and store it for later inspection.</remarks>
    }
    procedure WindowProc(var Message: TMessage);

    {!
    <value>
     Returns or sets the instance-specific hook for exception trapping.</value>
    }
    property ExceptionHook: TExceptionHook read FExceptionHook write
        FExceptionHook;
    {!
    <value>
     Returns the ID of the thread that created this instance and that
     will thus receive all messages send to the objects helper window.</value>
    }
    property CreatorThreadID: Cardinal read FCreatorThreadID;

    {!
    <value>
     The helper windows handle. Use with care, do not destroy this
     window!</value>
    }
    property Handle: Hwnd read FHandle;

    {!
    <value>
     Returns the last error trapped if no exception hooks are in place.
     You can examine the returned instance, but do not raise it or
     destroy it, it is property of this object. You can call the
     <see cref="RaiseLastError"> method to raise the exception in the
     context of the current thread.</value>
    }
    property LastError: Exception read FLastError;
  end;

  {!
  <remarks>
   Helper class used by the SendMessage and PostMessage overloads of the
   <see cref="TCrossThreadMessenger"/> class that passes an interface
   reference as parameter. The messages receiver will get a TInterfacePorter
   instance as lparam and has to either free this instance or return it
   to the pool through the ReturnInstance or ReturnToPool method.
  <para>
   This class suffers from exhibitionism, it exposes some things in its
   public interface that are not intended for use outside this unit.
   However, we have to make the class available to descendents of
   <see cref="TCrossThreadMessenger"/>, so there is no good way around this
   problem. We can only hope for programmer discipline... </para>
  </remarks>
  }
  TInterfacePorter = class
  private
    FIntf: IInterface;
    FNext: TInterfacePorter;
  public
    {!
    <summary>
     Destroy all instances in the internal pool. This method is called
     automatically as part of the unit finalization and should not be
     called directly. The method is thread-safe.</summary>
    }
    class procedure ClearPool;
    {!
    <summary>
     Create the internal pool This method is called
     automatically as part of the unit initialization and should not be
     called directly. The method is thread-safe.</summary>
    }
    class procedure CreatePool;
    {!
    <summary>
     Get an instance from the pool. If the pool is empty a new instance
     is created and returned. This method is thread-safe. </summary>
    <returns>
     a instance for the exclusive use of the caller. He can do whatever
     he wants with it until it is returned to the pool, including freeing
     it.</returns>
    }
    class function GetInstance: TInterfacePorter;
    {!
    <summary>
     Return the passed instance to the pool. The method is thread-safe.</summary>
    <param name="Obj">
     is the object to return to the pool, cannot be nil. The caller
     relinquishes ownership of the object to the pool.</param>
    <exception cref="EParameterCannotBeNil">
     is raised if Obj is nil.</exception>
    <remarks>
     Note that returning the object sets the interface reference it may
     hold to nil, which may result in the interfaces implementor to be
     destroyed.</remarks>
    }
    class procedure ReturnInstance(Obj: TInterfacePorter);
    {!
    <summary>
     Return this instance to the pool.</summary>
    }
    procedure ReturnToPool;
    {!
    <value>
     Returns or sets the interface reference to port.</value>
    }
    property Intf: IInterface read FIntf write FIntf;
    {!
    <value>
     Used to tie an object to the pool. Will always be nil while the
     object is not in the pool and should not be used by code outside
     this unit.</value>
    }
    property Next: TInterfacePorter read FNext write FNext;
  end;

  {!
  <remarks>
   Exception class used to report SendMessage timeouts. </remarks>
  }
  EThreadMessageTimeoutError = class(Exception);

implementation

uses InterlockedOpsU, SyncObjs, CommonTypesU;

resourcestring
  SMessageTimedOut = 'TCrossThreadMessenger.SendMessage: The target thread did not process the message in time (timeout: %d milliseconds).';

{== TInterfacePorter ==================================================}

const
  {! Number of TInterfacePorter objects put into the pool when it is
    created. More objects are created as needed, if required. }
  DefPoolSize = 5;
  {! Maximum number of objects to retain in the pool.}
  MaxPoolSize = 25;
var
  {! "Head" node of the TInterfacePorter pool, which is implemented as
    singly linked list. }
  Pool: TInterfacePorter = nil;
  {! Critical section used to make access to the pool thread-safe.}
  PoolLock: TCriticalSection = nil;
  {! Number of objects currently in the pool}
  PoolSize: Cardinal = 0;

class procedure TInterfacePorter.ClearPool;
var
  Inst: TInterfacePorter;
begin
  while Assigned(Pool) do begin
    Inst := GetInstance;
    Inst.Free;
  end; {while}
end;

class procedure TInterfacePorter.CreatePool;
var
  I: Integer;
begin
  for I := 1 to DefPoolSize do
    ReturnInstance(TInterfacePorter.Create);
end;

class function TInterfacePorter.GetInstance: TInterfacePorter;
begin
  PoolLock.Acquire;
  try
    Result := Pool;
    if not Assigned(Result) then
      Result := TInterfacePorter.Create
    else begin
      Pool := Result.Next;
      Result.Next := nil;
      Dec(Poolsize);
    end; {else}
  finally
    PoolLock.Release;
  end; {finally}
end;

class procedure TInterfacePorter.ReturnInstance(Obj: TInterfacePorter);
begin
  if not Assigned(Obj) then
    raise EParameterCannotBeNil.Create('TInterfacePorter.ReturnInstance','Obj');

  Obj.Intf := nil;
  {The if condition is not absolutely thread-safe, but that does not
   matter here. All that might happen is that we free an object that
   should have gone back to the pool, or add one back that should have
   been destroyed. Since the actual pool size does not matter that is
   not critical. }
  if Poolsize >= MaxPoolSize then
    Obj.Free
  else begin
    PoolLock.Acquire;
    try
      Obj.Next := Pool;
      Pool := Obj;
      Inc(PoolSize);
    finally
      PoolLock.Release;
    end; {finally}
  end; {else}
end;

procedure TInterfacePorter.ReturnToPool;
begin
  ReturnInstance(Self);
end;

{== TThreadMethodPorter ===============================================}

type
  {!
  <remarks>
   This is a helper class used to transport a message pointer
   in the form of an IStatelessExecutor interface with a Windows
   message. It's only used internally, so the error checking is
   minimal.</remarks>
  }
  TThreadMethodPorter = class(TInterfacedObject, IStatelessExecutor)
  private
    FCall: TThreadMethod;
  protected
    procedure Execute;
  public
    constructor Create(aMethod: TThreadMethod);
  end;

constructor TThreadMethodPorter.Create(aMethod: TThreadMethod);
begin
  inherited Create;
  FCall := aMethod;
end;

procedure TThreadMethodPorter.Execute;
begin
  FCall;
end;


{== TCrossThreadMessenger =============================================}


var
  {! This variable will hold the class-level exception hook. }
  ClassExceptionHook: TExceptionHook = nil;

{$IFDEF WIN64}
{$MESSAGE Fatal 'This function needs to be adapted for 64 bit Windows'}
{$ENDIF}

{!
<summary>
 Window function used by the helper windowclass.   </summary>
<remarks>
 <para>
  We expect to get an object reference for a TCrossThreadMessenger
  objects with the WM_CREATE message. This reference is stored into
  the GWL_USERDATA field of the window structure, from which we get it
  back on subsequent messages. For those the message parameters are
  packaged into a standard TMessage record and handed to the objects
  WindowProc.</para>
 <para>
  There is one crucial difference to the way a TWndMethod needs to be
  implemented for a helper window created via AllocateHwnd: If the
  called WindowProc does not set the Message.Result we automatically
  hand the message to DefWindowProc as the last step.</para>
 <para>
  <b>Win64 compatibility issue:</b> We assume sizeof(pointer) =
  sizeof(integer), in Win64 it will no longer be possible to store
  an object reference into GWL_USERDATA! </para>
</remarks>
}
function HelperWndProc(Wnd: HWND; msg: Cardinal; wparam: WPARAM;
  lparam: LPARAM): LRESULT; stdcall;
var
  Messenger: TCrossThreadMessenger;
  Message: TMessage;
begin
  if msg = WM_CREATE then begin
    SetWindowLong(Wnd, GWL_USERDATA,
      Integer(PCreateStruct(lparam)^.lpCreateParams));
    Result := 0;
  end {if}
  else begin
    Messenger := TCrossThreadMessenger(GetWindowLong(Wnd, GWL_USERDATA));
    Message.Result := MaxInt;
    if Assigned(Messenger) then begin
      Message.Msg := Msg;
      Message.WParam := wparam;
      Message.LParam := lparam;
      try
        Messenger.WindowProc(Message);
      except
        {Exceptions when calling the Messengers WíndowProc only happen
         if the Messenger was destroyed while a message was on the way.
         Rare but not impossible. We can do little here, just make sure
         the exception does not cause any damage. }
      end;
    end; {if}
    if Message.Result = MaxInt then
      Result := DefWindowProc(Wnd, Msg, wparam, lparam)
    else
      Result := Message.Result;
  end; {else}
end;

var
  HelperWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @HelperWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPBCrossThreadMessengerWindow');


constructor TCrossThreadMessenger.Create;
begin
  inherited Create;
  FCreatorThreadID := GetCurrentThreadID;
  CreateWindow;
end;

destructor TCrossThreadMessenger.Destroy;
begin
  SetWindowLong(Handle, GWL_USERDATA, 0);
  {Note: the above statement makes sure the helper window proc does not
   try to call the messengers WindowProc while we are in the process of
   destroying the messenger. }

  DestroyWindow;
  FLastError.Free;
  inherited Destroy;
end;

{! Call the passed exception hook method, passing it the exception
  object E. If the call causes an exception we grab hold of it and
  store it. However, if the hook raises the object it was passed
  (E2 = E) that may have unforseen consequences, since this method
  is called from inside an except block already...}
procedure TCrossThreadMessenger.CallHook(Hook: TExceptionHook; E:
    Exception);
begin
  try
    SetLastError(nil);
    Hook(E);
  except on E2: Exception do
    SetLastError(AcquireExceptionObject);
  end; {except}
end;

procedure TCrossThreadMessenger.CreateWindow;
begin
  RegisterHelperWindowClass;
  FHandle := CreateWindowEx(
    WS_EX_TOOLWINDOW,
    HelperWindowClass.lpszClassName,
    '', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, Self);
end;

procedure TCrossThreadMessenger.DestroyWindow;
var
  H: HWND;
begin
  H:= Handle;
  FHandle := 0;
  if IsWindow(H) then
    Windows.DestroyWindow(H);
end;

class function TCrossThreadMessenger.GetClassExceptionHook:
    TExceptionHook;
begin
  Result := ClassExceptionHook;
end;

function TCrossThreadMessenger.PostMessage(msg: cardinal; wp: WPARAM;
    lp: LPARAM): Boolean;
begin
  Result := Windows.PostMessage(Handle, msg, wp, lp);
end;

function TCrossThreadMessenger.PostMessage(msg: cardinal; wp: WPARAM;
    Obj: TObject): Boolean;
begin
  Result := PostMessage(msg, wp, LPARAM(Obj));
end;

function TCrossThreadMessenger.PostMessage(msg: cardinal; wp: WPARAM;
    const Intf: IInterface): Boolean;
var
  Porter: TinterfacePorter;
begin
  Porter := TInterfacePorter.GetInstance;
  Porter.Intf := Intf;
  Result := PostMessage(msg, wp, Porter);
  {Note: the messages receiver has to return the porter to the pool!}
  if not Result then
    Porter.ReturnToPool;
end;

procedure TCrossThreadMessenger.QueueCall(aMethod: TThreadMethod);
begin
  if Assigned(aMethod) then
    QueueCall(TThreadMethodPorter.Create(aMethod) as IStatelessExecutor);
end;

procedure TCrossThreadMessenger.QueueCall(aExecutor:
    IStatelessExecutor);
begin
  if Assigned(aExecutor) then
    PostMessage(UM_CALLEXECUTOR, 0, aExecutor as IInterface);
end;

procedure TCrossThreadMessenger.RaiseLastError;
var
  Temp: Exception;
begin
  if Assigned(FLastError) then begin
    Temp := FLastError;
    FLastError := nil;
    raise Temp at FErrorAddr;
  end; {if}
end;

{! Register the helper windowclass, if that has not been done already. }
procedure TCrossThreadMessenger.RegisterHelperWindowClass;
var
  ClassRegistered: Boolean;
  TempClass: TWndClass;
begin
  ClassRegistered :=
    GetClassInfo(HInstance, HelperWindowClass.lpszClassName, TempClass);
  if not ClassRegistered then begin
    HelperWindowClass.hInstance := HInstance;
    Windows.RegisterClass(HelperWindowClass);
  end; {if}
end;

function TCrossThreadMessenger.SendMessage(msg: cardinal; wp: WPARAM;
    lp: LPARAM; timeout: Cardinal = C15Seconds): LRESULT;
var
  C: Cardinal;
begin
  if
  {$IFDEF CPUX86}
    Windows.SendMessageTimeout(Handle, msg, wp, lp, SMTO_BLOCK,
    timeout, @C) = 0
  {$ELSE}
    Windows.SendMessageTimeout(Handle, msg, wp, lp, SMTO_BLOCK,
    timeout, C) = 0
  {$ENDIF}
  then begin
    if GetLastError = 0 then
      raise EThreadMessageTimeoutError.CreateFmt(
        SMessageTimedOut, [timeout])
    else
      RaiseLastOSError;
    Result := 0;  // to remove hint
  end {if}
  else
    Result := LRESULT(C);
end;

function TCrossThreadMessenger.SendMessage(msg: cardinal; wp: WPARAM;
    Obj: TObject; timeout: Cardinal = C15Seconds): LRESULT;
begin
  Result := SendMessage(msg, wp, LPARAM(Obj), timeout);
end;

function TCrossThreadMessenger.SendMessage(msg: cardinal; wp: WPARAM;
    const Intf: IInterface; timeout: Cardinal = C15Seconds): LRESULT;
var
  Porter: TinterfacePorter;
begin
  Porter := TInterfacePorter.GetInstance;
  Porter.Intf := Intf;
  try
    Result := SendMessage(msg, wp, Porter, timeout);
    {Note: the messages receiver has to return the porter to the pool!}
  except
    Porter.ReturnToPool;
    raise;
  end; {finally}
end;

class procedure TCrossThreadMessenger.SetClassExceptionHook(const
    Value: TExceptionHook);
begin
  ClassExceptionHook := Value;
end;

procedure TCrossThreadMessenger.SetLastError(const Value: Exception);
begin
  if Assigned(FLastError) then
    FLastError.Free;
  FLastError := Value;
  if Assigned(Value) then
    FErrorAddr := ErrorAddr
  else
    FErrorAddr := nil;
end;

procedure TCrossThreadMessenger.Synchronize(aMethod: TThreadMethod);
begin
  if Assigned(aMethod) then
    Synchronize(TThreadMethodPorter.Create(aMethod) as IStatelessExecutor);
end;

procedure TCrossThreadMessenger.Synchronize(const aExecutor:
    IStatelessExecutor);
begin
  if Assigned(aExecutor) then
    SendMessage(UM_CALLEXECUTOR, 0, aExecutor as IInterface);
end;

procedure TCrossThreadMessenger.WindowProc(var Message: TMessage);
begin
  try
    WndProc(Message);
  except on E: Exception do
    if Assigned(ExceptionHook) then
      CallHook(Exceptionhook, E)
    else if Assigned(ClassExceptionHook) then
      CallHook(ClassExceptionHook, E)
    else
      SetLastError(AcquireExceptionObject);
  end; {except}
end;

procedure TCrossThreadMessenger.WndProc(var Message: TMessage);
var
  Porter: TObject;
  Intf: IStatelessExecutor;
begin
  if Message.Msg = UM_CALLEXECUTOR then begin
    Message.Result := 0;
    Porter := TObject(Message.lparam);
    if Assigned(Porter) and (Porter Is TInterfacePorter) then
    begin
      try
        if Supports(TInterfacePorter(Porter).Intf,
          IStatelessExecutor, Intf)
        then
          Intf.Execute;
      finally
        TInterfacePorter(Porter).ReturnToPool;
      end; {finally}
    end; {if}
  end; {if}
end;

initialization
  PoolLock:= TCriticalSection.Create;
  TInterfacePorter.CreatePool;
finalization
{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Finalizing CrossThreadMessengerU...');
{$ENDIF}

    TInterfacePorter.ClearPool;
    FreeAndNil(PoolLock);

{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Done finalizing CrossThreadMessengerU');
{$ENDIF}
end.
