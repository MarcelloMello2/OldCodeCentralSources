{== AsyncTasksU =======================================================}
{! <summary>
  This unit implements a mechanism to execute a method of an arbitrary
  object in a secondary thread.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2007-01-15</para>
<para>Version 1.1 created 2007-07-24, modified the pool to allow a
  limit on the number of created threads to be used and extra
  tasks to be queued until a pool thread becomes idle.</para>
<para>Version 1.2 created 2009-03-14, updated docs, checked for Unicode
  issues, changed preconditions to raise exceptions.</para>
<para>Version 1.3 created 2013-10-23, added support for asnychronous
 execution of anonymous methods.</para>
<para>Last modified       2013-10-23</para>
</history>
<remarks>
<para>The mechanism is modelled on the asynchronous processing model of the
Microsoft .NET framework. It uses an internal thread pool. Since we
don't have a reflection-like framework available for Win32 the
implementation offers direct support for only two plain method pointer
types, and an extensible "executor" model that can be used to call
methods with arbitrary parameter lists. See the comments in the
<see cref="ExecutorIntfU"/> unit for some hints on how to implement an
executor. </para>
<para>The model allows polling, waiting, and callbacks to be used to get
aware that an asynchronous task has completed. The classes and
interfaces used do not map 1:1 to the equivalent .NET classes since in
Win32 Delphi method pointers are not classes, so they cannot have
methods like BeginInvoke and EndInvoke. Instead we implement these
methods on the thread pool. </para>
<para>As it stands now the pool does not allow a task to be bound to a
particular thread. If you need that use the CreateWorkerthread function
to obtain an interface for a dedicated worker thread under your control.
</para></remarks>
<copyright>Copyright 2009 by Dr. Peter Below</copyright>
<licence> The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
unit AsyncTasksU;
interface

{$INCLUDE PBDEFINES.INC}
uses SysUtils, Windows, SyncObjs, Classes, ExecutorIntfU;

type
  {! <remarks>
    This is the equivalent of the poorly named .NET WaitCallback
    delegate. </remarks>
  }
  TAsyncCallback = procedure (State: TObject) of Object;

  {! <remarks>
    Use a callback method of this type if no additional data needs
    to be passed.</remarks>
  }
  TSimpleAsyncCallback = procedure of Object;

  {! <remarks>
    We only support parameterless stand-alone functions for
    asynchronous execution directly. </remarks>
  }
  TSimpleAsyncProc = procedure;

  {! <summary>
    This interface is the direct equivalent of the .NET AsyncResult.
    </summary> <remarks>
    It is a bit too limited for our purposes, however, so thread pool
    methods starting an asynchronous call with feedback will return a
    derived <see cref="IAsyncCall"/> interface instead.</remarks>
  }
  IAsyncResult = interface(IInterface)
  ['{3A317E41-D6F7-4632-B6F8-4EE7BDBC9A88}']
    {! <summary>
      Getter for the <see cref="IAsyncResult.AsyncState"/> property.</summary>
    }
    function GetAsyncState: TObject;
    {! <summary>
      Getter for the <see cref="IAsyncResult.AsyncWaitHandle"/> property.</summary>
    }
    function GetAsyncWaitHandle: THandle;
    {! <summary>
      Getter for the <see cref="IAsyncResult.CompletedSynchronously"/> property.</summary>
    }
    function GetCompletedSynchronously: Boolean;
    {! <summary>
      Getter for the <see cref="IAsyncResult.IsCompleted"/> property</summary>
    }
    function GetIsCompleted: Boolean;

    {! <value>
      Returns the state object passed to the thread pool when the async
      operation was started. Can be nil.</value>
    }
    property AsyncState: TObject read GetAsyncState;

    {! <value>
      Returns the API handle of a synchronization object the caller can
      wait on until the async operation has completed. Do not use,
      instead use one of the Wait methods of the IAsyncCall interface.</value>
    }
    property AsyncWaitHandle: THandle read GetAsyncWaitHandle;

    {! <value>
      After the task completed this properly can be used to detect
      whether it was actually executed asynchronouly or not. Will always
      return false in the current implementation.</value>
    }
    property CompletedSynchronously: Boolean read GetCompletedSynchronously;

    {! <value>
      Can be used to check whether the async task has completed yet.</value>
    }
    property IsCompleted: Boolean read GetIsCompleted;
  end;

  IAsyncCall = interface;  //forward declaration

  {! <remarks>
    An object submitting a call for asynchronous execution can use
    a callback of this type to get notified when the call has completed.</remarks>
  }
  TAsyncNotification = procedure (aResult: IAsyncCall) of object;

  {! <summary>
    Extended interface for a task that is executed asynchronously. </summary>
   <remarks>
    This interface adds some methods and properties to allow the state of the
    task to be checked, to wait for the task to complete, or get notified
    when it is done, and to handle errors encountered during the tasks
    execution. </remarks>
  }
  IAsyncCall = interface(IAsyncResult)
  ['{ACCFC06F-CAE8-464C-8B78-2ACFE5066069}']
    {! <summary>
      Getter for the <see cref="IAsyncCall.Failed"/> property.</summary>
    }
    function GetFailed: Boolean;
    {! <summary>
      Getter for the <see cref="IAsyncCall.LastError"/> property.</summary>
    }
    function GetLastError: Exception;
    {! <summary>
      Getter for the <see cref="IAsyncCall.Succeeded"/> property.</summary>
    }
    function GetSucceeded: Boolean;

    {! <summary>
      Define a method to call when the async task has completed.  </summary>
     <param name="aCallback">The method to execute when the task is done.
       Cannot be nil. </param>
     <exception cref="EAsyncCallError">Raises EAsyncCallError if a different
      thread has already installed a notification callback.</exception>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
     <remarks>
      Note that the method will execute in the thread context of the caller.
      The callback may be called before this method has returned if the task
      has already completed. The calling thread must have a message loop!
      If it does not the worker thread trying to call the callback will
      be blocked. </remarks> }
    procedure NotifyWhenDone(aCallback: TAsyncNotification);

    {! <summary>
      Re-raise the exception that terminated the async task prematurely
      in the thread context of the caller. Note that it will be raised
      at the original address.</summary>
     <exception cref="EPreconditionViolation">is raised if the call has not
      completed or we have no trapped exception. </exception>
    }
    procedure RaiseLastError;

    {! <summary>
      Wait for the async task to complete. </summary>
     <returns> A value indicating what caused the method to return. The only
      values expected here are wrSignalled (the wait completed because the
      task completed) or wrTimeout (the timeout expired before the task
      completed).   </returns>
     <param name="TimeoutMSecs">
      The time to wait, in milliseconds, before
      the wait is abandonned. The default is INFINITE (no timeout).</param>
     <param name="ProcessMessages">
      Determines how the wait is implemented. See the remarks section.</param>
     <remarks>
      <para>
      If the ProcessMessages parameter is true the wait will process paint
      messages, non-client area messages with the exception of
      WM_SYSCOMMAND/SC_CLOSE, and messages send from other threads, but
      discard user input messages. It will also process queued synchronize
      requests. If the parameter is false (the default) the wait will
      block until the task completes.
      </para><para>
      True should only be used if the wait is called in the main thread of
      a GUI app, since the waiting thread must have a message loop. If it
      does not have one the wait will block as if false had been passed
      for this parameter.</para> </remarks>
    }
    function WaitForCompletion(TimeoutMSecs: Cardinal = INFINITE;
      ProcessMessages: Boolean = false): TWaitResult;

    {! <value>
      Returns true if the task completed due to an exception, false
      if it was successful or is still executing.</value> }
    property Failed: Boolean read GetFailed;

    {! <value>
      Returns a reference to the exception object that caused the
      task to fail, or nil, if the task succeeded or has not completed
      yet. Note that the returned object remains property of the
      interfaces implementor, the caller must not free or raise it!</value>}
    property LastError: Exception read GetLastError;

    {! <value>
      Returns true if the task completed successfully, false if it
      failed due to an exception or is still executing.</value>}
    property Succeeded: Boolean read GetSucceeded;
  end;

  {! <remarks>
    This defines the public interface for a worker thread. It has methods
    to queue calls for execution in the threads context. A thread pool
    will also implement this interface.</remarks>}
  IThreadManager = interface(IInterface)
  ['{7366F58D-FC8E-4B4F-B71C-CCA69F6796E2}']
    {! <summary>
      Submit a parameterless method for asynchronous execution.   </summary>
     <returns>
      an interface that can be used to check the state of the queued
      task and wait for its completion.      </returns>
     <param name="aCallback">
      aCallback is the method to execute in a secondary thread.
      It cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    function BeginInvoke(aCallback: TSimpleAsyncCallback): IAsyncCall;
      overload;

    {! <summary>
      Submit a method for asynchronous execution.   </summary>
     <returns>
      An interface that can be used to check the state of the queued
      task and wait for its completion.   </returns>
     <param name="aCallback">
      The method to execute in a secondary thread, cannot be nil.</param>
     <param name="aState">
      A data object to pass to the callback, can be nil.  </param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    function BeginInvoke(aCallback: TAsyncCallback; aState: TObject = nil):
      IAsyncCall; overload;

    {! <summary>
      Submit a stand-alone procedure for asynchronous execution. </summary>
     <returns>
      An interface that can be used to check the state of the queued
      task and wait for its completion.   </returns>
     <param name="aCallback">
      The procedure to execute in a secondary thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    function BeginInvoke(aCallback: TSimpleAsyncProc): IAsyncCall; overload;

    {! <summary>
      Submit an executor for asynchronous execution. </summary>
     <returns>
      An interface that can be used to check the state of the
      queued task and wait for its completion.    </returns>
     <param name="aExecutor">
      Represents the object that should have its Execute method called
      in a secondary thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aExecutor is nil.
      </exception>
      }
    function BeginInvoke(aExecutor: IExecutor): IAsyncCall; overload;

    {$IFDEF SUPPORTS_ANONYMOUS_METHODS}
    {! <summary>
      Submit a parameterless closure for asynchronous execution. </summary>
     <returns>
      An interface that can be used to check the state of the
      queued task and wait for its completion.    </returns>
     <param name="aClosure">
      holds the anonymous method that should be executed in a secondary
      thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aClosure is nil.
      </exception>
     <remarks>
      Make sure any data the closure has captured is not tempered with
      until the closure has completed! Remember that Delphi captures
      variables by reference, not by value!</remarks>
      }
    function BeginInvoke(aClosure: TProc): IAsyncCall; overload;
    procedure QueueUserWorkitem(aClosure: TProc); overload;
    {$ENDIF}

    {! <summary>
      Waits for the passed call to complete.</summary>
     <returns>
      The object passed to BeginInvoke, or nil, if none was passed. </returns>
     <param name="aCall">
      Is the pending call to wait for, cannot be nil.  </param>
     <param name="aTimeoutMSecs">
      Is an optional timeout in milliseconds. The default is INFINITE
      (no timeout).</param>
     <exception cref="EAsyncTimeoutError">
      Raises EAsyncTimeoutError if the wait times out.</exception>
     <exception cref="EParameterCannotBeNil">is raised if aCall is nil.
      </exception>
     <remarks>
      Unless the application is a console application EndInvoke will
      wait in a way that does not block message processing if called from
      the applications main thread. It will discard user input messages,
      though.
      </remarks> }
    function EndInvoke(aCall: IAsyncCall; aTimeoutMSecs: Cardinal =
      INFINITE): TObject;

    {! <summary>
      Submit a method for asynchronous execution. The caller is not
      notified in any way when the task completes.  </summary>
     <param name="aCallback">
      The method to execute in a secondary thread, cannot be nil.</param>
     <param name="aState">
      A data object to pass to the callback, can be nil.  </param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    procedure QueueUserWorkitem( aCallback: TAsyncCallback;
      aState: TObject = nil); overload;

    {! <summary>
      Submit a parameterless method for asynchronous execution. The caller
      is not  notified in any way when the task completes.  </summary>
     <param name="aCallback">
      The method to execute in a secondary thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    procedure QueueUserWorkitem(aCallback: TSimpleAsyncCallback
      ); overload;

    {! <summary>
      Submit a parameterless procedure for asynchronous execution. The caller
      is not  notified in any way when the task completes.  </summary>
     <param name="aCallback">
      The procedure to execute in a secondary thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
      </exception>
    }
    procedure QueueUserWorkitem(aCallback: TSimpleAsyncProc
      ); overload;

    {! <summary>
      Submit an executor for asynchronous execution. The caller
      is not  notified in any way when the task completes. </summary>
      <param name="aExecutor">
      Represents the object that should have its Execute method called
      in a secondary thread, cannot be nil.</param>
     <exception cref="EParameterCannotBeNil">is raised if aExecutor is nil.
      </exception>
    }
    procedure QueueUserWorkitem( aExecutor: IExecutor); overload;
  end;

  {! <remarks>
    This defines the public interface of a thread pool. It inherits
    the methods to queue calls for execution in worker threads from
    <see cref="IThreadManager"/> and adds a number of properties
    that influence the way the thread pool works.</remarks> }
  IThreadPool = interface(IThreadmanager)
  ['{54CC4013-97E4-4034-83A3-A03C01D17ACE}']
    {! <summary>
      Read accessor for the <see cref="IThreadPool.IdleThreads"/> property</summary>.
    }
    function GetIdleThreads: Integer;
    {! <summary>
      Read accessor for the <see cref="IThreadPool.LimitToMaxThreads"/> property</summary>.
    }
    function GetLimitToMaxThreads: Boolean;
    {! <summary>
      Read accessor for the <see cref="IThreadPool.MaxThreads"/> property</summary>.
    }
    function GetMaxThreads: Integer;
    {! <summary>
      Read accessor for the <see cref="IThreadPool.Threadcount"/> property</summary>.
    }
    function GetThreadcount: Integer;

    {! <summary>
      Write accessor for the <see cref="IThreadPool.LimitToMaxThreads"/> property</summary>.
    }
    procedure SetLimitToMaxThreads(const Value: Boolean);
    {! <summary>
      Write accessor for the <see cref="IThreadPool.MaxThreads"/> property</summary>.
    }
    procedure SetMaxThreads(const Value: Integer);

    {! <value>
      Returns the number of idle threads currently in the pool. Note this
      number may be already out of date when you read it.</value>.
    }
    property IdleThreads: Integer read GetIdleThreads;

    {! <value>
      If set to true the pool will never create more than
      <see cref="IThreadPool.MaxThreads"/>
      threads. If a new task is submitted while all threads are busy it
      will get added to a queue and only be executed when one of the other
      threads becomes idle. If this never happens the task may never
      execute. This property is false by default. </value>
    }
    property LimitToMaxThreads: Boolean read GetLimitToMaxThreads write
        SetLimitToMaxThreads;

    {! <value>
      Get or set the maximum number of threads to keep in the pool.
      The pool is able to create more threads as required, unless
      <see cref="LimitToMaxThreads"/> has been set to true, but it will
      destroy extras returned to the pool again if the pool already has
      MaxThreads idle threads in it.</value>
    }
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;

    {! <value>
      Returns the number of threads currently in the pool. Note this
      number may be already out of date when you read it.</value>
    }
    property Threadcount: Integer read GetThreadcount;
  end;

  {!
  <remarks>
   The standard thread manager implements this interface to allow for
   controlled termination of a worker thread.</remarks>
  }
  IThreadManagerEx = interface(IInterface)
  ['{63DADC37-7FA0-4791-8101-662B9C42E5CA}']
    procedure PrepareForClose(aTimeoutMSecs: Cardinal = INFINITE);
  end;

  {! <remarks>
    Exception class used to report errors in worker threads or thread
    pools.</remarks>
  }
  EAsyncCallError = class(Exception);
  {! <remarks>
    Exception raised if a wait for an async call times out.</remarks>
  }
  EAsyncTimeoutError = Class(EAsyncCallError);

{! <summary>
  Returns the public interface of the thread pool singleton. The pool
  is created on first call and will be destroyed during unit finalization.
  This function is thread-safe.</summary>
}
function ThreadPool: IThreadPool;

{! <summary>
 Create and return a dedicated worker thread for the callers exclusive
 use. The threads lifetime is managed via interface reference counting.</summary>
}
function CreateWorkerthread: IThreadManager;

{! <summary>
 Create and return a dedicated worker thread for the callers exclusive
 use. The threads lifetime is managed via interface reference counting.
 The thread initializes the COM libraries using the single-threaded
 appartment model.</summary>
}
function CreateComWorkerthread: IThreadManager;

implementation

uses Contnrs, InterlockedOpsU, Messages,
  CrossThreadMessengerU, CommonTypesU, ActiveX, DebugHelpers;

const
  NOneSecond = 1000;
  NMaxThreadsDefault = 5;

resourcestring
  SEndInvokeTimeout =
    'TThreadPool.EndInvoke: Operation did not complete after %d milliseconds';
  SNotificationAlreadyInstalled =
    'TAsyncCall.NotifyWhenDone: Another thread has already installed a '+
    'notification method!';


type
  {! <remarks>
    This class implements a thread that can be managed by the thread
    pool. It can also be used outside the pool, however, if it is managed
    by an instance of the TThreadManager class.</remarks>
  }
  TPoolThread = class(TThread)
  strict private
    FDying: boolean;
    FQueueEmptySignal: TSimpleEvent;
  private
    FIdle: Boolean;
    FNext: TPoolThread;
    FOnThreadDying: TNotifyEvent;
    FQueue: IInterfaceList;
    FSignal: TSimpleEvent;
    FStandalone: Boolean;
    procedure RepoolThread;
    procedure RunTasks;
    procedure WaitForWork;
  protected
    procedure DoThreadDying;
    procedure Execute; override;
    procedure FinalizeThread; virtual;
    procedure InitializeThread; virtual;
    property Dying: boolean read FDying write FDying;
    property QueueEmptySignal: TSimpleEvent read FQueueEmptySignal;
    property Standalone: Boolean read FStandalone;
  public
    constructor Create(AStandalone: Boolean = false); reintroduce;
    destructor Destroy; override;
    procedure Die;
    function Hook(NewNext: TPoolThread): TPoolThread;
    procedure PrepareForClose(aTimeoutMSecs: Cardinal = INFINITE);
    procedure SubmitTask(const aExecutor: IExecutor);
    function Unhook: TPoolThread;
    property Idle: Boolean read FIdle;
    property Next: TPoolThread read FNext;
    property OnThreadDying: TNotifyEvent read FOnThreadDying write
      FOnThreadDying;
  end;

  {! <remarks>
    The thread pool implements this additional interface to allow a
    thread to be added to the pool from outside. </remarks>
  }
  IThreadPoolManager = interface(IInterface)
  ['{8C43FF60-0F2B-4E16-BB13-C97BC004231C}']
    {! <summary>
      Add a worker thread to the thread pool.</summary>
     <param name="aThread">is the thread to add, cannot be nil.</param>
     <param name="Repool">is true if a worker obtained from the pool
      should be returned to it, false if this is a newly hired worker
      to go into the pool.</param>
     <exception cref="EParameterCannotBeNil">is raised if aThread is nil.</exception>
    }
    procedure AddThread(aThread: TPoolThread; Repool: Boolean = true);
  end;

  {! <summary>
    This class implements the methods of the <see cref="IThreadManager"/>
    interface.</summary>
    <remarks>
    It basically creates a helper object that implements the
    <see cref="IExecutor"/> interface and hands it to the
    <see cref="TBaseThreadHandler.DoBeginInvoke"/> method. That method
     is abstract and has to be implemented by descendents. </remarks>
  }
  TBaseThreadHandler = class {$IFDEF SUPPORTS_CLASS_ABSTRACT}abstract{$ENDIF}
    (TInterfacedObject, IThreadManager)
  private
    procedure CallbackMissing;
  protected
    {! <summary>
     Descendents must override this method to queue the passed executor
     for later execution in a secondary thread.</summary>
    }
    function DoBeginInvoke(aExecutor: IExecutor): IAsyncCall; virtual; abstract;
    function BeginInvoke(aCallback: TAsyncCallback; aState: TObject = nil):
        IAsyncCall; overload;
    function BeginInvoke(aExecutor: IExecutor): IAsyncCall; overload;
    function BeginInvoke(aCallback: TSimpleAsyncCallback): IAsyncCall; overload;
    function BeginInvoke(aCallback: TSimpleAsyncProc): IAsyncCall; overload;
    {$IFDEF SUPPORTS_ANONYMOUS_METHODS}
    function BeginInvoke(aClosure: TProc): IAsyncCall; overload;
    procedure QueueUserWorkitem(aClosure: TProc); overload;
    {$ENDIF}
    function EndInvoke(aCall: IAsyncCall; aTimeoutMSecs: Cardinal =
        INFINITE): TObject;
    procedure QueueUserWorkitem(aExecutor: IExecutor); overload;
    procedure QueueUserWorkitem(aCallback: TAsyncCallback; aState: TObject
        = nil); overload;
    procedure QueueUserWorkitem(aCallback: TSimpleAsyncCallback); overload;
    procedure QueueUserWorkitem(aCallback: TSimpleAsyncProc); overload;
  end;

  {! <summary>
    This class implements the thread pool.</summary>
   <remarks><para>
    The class maintains a linked list of idle threads and a task queue. The pool
    is of course itself thread-safe. The class inherits the implementation
    of the <see cref="IThreadManager"/> interface from its ancestor and
    adds implementations for the <see cref="IThreadPool"/> and
    <see cref="IThreadPoolManager"/> interfaces. </para>
    <para>
    The pools starts out empty. As calls are passed to the pool via the
    <see cref="DoBeginInvoke"/> method the pool may create new worker
    threads and add them to the pool. It only adds a new thread if it
    cannot find an idle one already in the pool. If the
    <see cref="LimitToMaxThreads"/> property has been set to true
    the pool will not create more than <see cref="MaxThreads"/>
    threads. </para>
    <para>
    If no worker thread is available for a call the call will
    be queued until a worker threads becomes available. To get aware of
    this the pool relies on the worker threads to "repool" themselves
    by calling the <see cref="AddThread"/> method of the pool. In this
    method the pool passes queued calls to the free thread and also
    implements the mechanism to limit the number of threads in the pool
    by removing and killing the extras when they repool themselves.
    </para></remarks>
    }
  TThreadPool = class(TBaseThreadHandler, IThreadPool, IThreadPoolManager)
  private
    FLimitToMaxThreads: Boolean;
    FMaxThreads: Integer;
    FNumThreads: Integer;
    FPool: TPoolThread;
    FPoolGuard: TCriticalSection;
    FTaskQueue: IInterfaceList;
    function FindWorker: TPoolThread;
    procedure RemoveThread(aThread: TPoolThread);
  protected
    function GetIdleThreads: Integer;
    function GetMaxThreads: Integer;
    function GetThreadcount: Integer;
    function GetLimitToMaxThreads: Boolean;
    procedure SetLimitToMaxThreads(const Value: Boolean);
    procedure SetMaxThreads(const Value: Integer);
  protected
    procedure AddThread(aThread: TPoolThread; Repool: Boolean = true);
    function DoBeginInvoke(aExecutor: IExecutor): IAsyncCall; override;
    procedure ThreadDied(sender: TObject);
    property LimitToMaxThreads: Boolean read GetLimitToMaxThreads write
        SetLimitToMaxThreads;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property PoolGuard: TCriticalSection read FPoolGuard;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  {! <remarks>
    This class provides the <see interface="IThreadManager"/> interface
    for a single dedicated worker thread that is not part of a thread
    pool. The <see cref="CreateWorkerThread"/> function returns the
    interface for an instance of this class.</remarks>
  }
  TThreadManager = class(TBaseThreadHandler, IThreadManagerEx)
  private
    FWorker: TPoolThread;
    procedure CreateWorker;
    procedure PrepareForClose(aTimeoutMSecs: Cardinal = INFINITE);
    procedure ThreadDied(sender: TObject);
  protected
    property Worker: TPoolThread read FWorker;
    function DoBeginInvoke(aExecutor: IExecutor): IAsyncCall; override;
    function NewWorkerthread: TPoolThread; virtual;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  {!
  <remarks>
   This class uses a worker thread that initializes the COM
   libraries, so can be used to execute code that requires COM
   stuff.</remarks>
  }
  TComThreadManager = class(TThreadManager)
  protected
    function NewWorkerthread: TPoolThread; override;
  end;



  {! <remarks>
    This class implements the <see cref="IExecutor"/> and
    <see cref="IStatelessExecutor"/> interfaces for a method that needs
    no parameters.</remarks>
  }
  TSimpleExecutor = class(TInterfacedObject, IExecutor, IStatelessExecutor)
  private
    FCallback: TSimpleAsyncCallback;
  protected
    procedure Execute;
    function GetState: TObject;
    property Callback: TSimpleAsyncCallback read FCallback;
  public
    constructor Create(ACallback: TSimpleAsyncCallback);
  end;

  {! <remarks>
  This class implements the <see cref="IExecutor"/> and
  <see cref="IStatelessExecutor"/> interfaces for a call of
  a parameterless stand-alone procedure.</remarks>
  }
  TStandAloneExecutor = class(TInterfacedObject, IExecutor, IStatelessExecutor)
  private
    FCallback: TSimpleAsyncProc;
  protected
    procedure Execute;
    function GetState: TObject;
    property Callback: TSimpleAsyncProc read FCallback;
  public
    constructor Create(ACallback: TSimpleAsyncProc);
  end;

{$IFDEF SUPPORTS_ANONYMOUS_METHODS}
  {! <remarks>
  This class implements the <see cref="IExecutor"/> and
  <see cref="IStatelessExecutor"/> interfaces for a call of
  a parameterless anonymous method.</remarks>
  }
  TClosureExecutor = class(TInterfacedObject, IExecutor, IStatelessExecutor)
  private
    FCallback: TProc;
  protected
    procedure Execute;
    function GetState: TObject;
    property Callback: TProc read FCallback;
  public
    constructor Create(ACallback: TProc);
  end;
{$ENDIF}


  {! <remarks>
    This class implements the <see cref="IExecutor"/> and
    <see cref="IStatelessExecutor"/> interfaces for a method that
    takes an object as the single parameter. </remarks>
  }
  TExecutorWithState = class(TInterfacedObject, IExecutor, IStatelessExecutor)
  private
    FCallback: TAsyncCallback;
    FState: TObject;
  protected
    procedure Execute;
    function GetState: TObject;
    property Callback: TAsyncCallback read FCallback;
  public
    constructor Create(ACallback: TAsyncCallback; AState: TObject);
  end;

  {! <summary>  This class is used to wrap an asynchronous call. </summary>
    <remarks><para>
    An instance will be created by a worker thread object that is handed
    a call to execute, the thread will call the Execute method inside the
    threads context. The originator of the call will receive an IAsyncCall
    interface for the created instance and can use it to check the state
    of the call, install a notification callback, or get the cause of
    failure if the call runs into an exception.</para>
    <para> The classes Execute method traps all
    exceptions, the worker thread itself is not involved in the details.
    The IAsyncCall interface also allows the caller to wait on the task
    to complete. The wait can be done in a way that does not block
    message processing in the waiting thread, although user input messages
    will be discarded. </para></remarks>
  }
  TAsyncCall = class(TInterfacedObject, IAsyncCall, IAsyncResult, IExecutor, IStatelessExecutor)
  private
    FCompleted: Boolean;
    FErrorAddress: Pointer;
    FExecutor: IExecutor;
    FLastError: Exception;
    FNotificationProc: TAsyncNotification;
    FMessenger: TCrossThreadMessenger;
    FSignal: TSimpleEvent;
    FSucceeded: Boolean;
    procedure CallNotificationProc;
    procedure EnsureSignalIsCreated;
    procedure SetCompleted;
  public
    constructor Create(AExecutor: IExecutor);
    destructor Destroy; override;
    procedure Execute;
  protected
    function GetFailed: Boolean;
    function GetLastError: Exception;
    function GetState: TObject;
    function GetSucceeded: Boolean;
    procedure NotifyWhenDone(aCallback: TAsyncNotification);
    procedure RaiseLastError;
    function WaitForCompletion(TimeoutMSecs: Cardinal = 4294967295;
      ProcessMessages: Boolean = False): TWaitResult;
    function GetAsyncState: TObject;
    function GetAsyncWaitHandle: THandle;
    function GetCompletedSynchronously: Boolean;
    function GetIsCompleted: Boolean;
  end;

  TComPoolThread = class(TPoolThread)
  protected
    procedure FinalizeThread; override;
    procedure InitializeThread; override;
  end;

var
  {Holds the reference to the thread pool singleton}
  InternalThreadPool: IThreadPool = nil;
  InternalShutDownInProgress: Boolean = false;

function ThreadPool: IThreadPool;
var
  P: TObject;
begin
  if InternalShutDownInProgress then begin
    Result := nil;
    Assert(false, 'ThreadPool referenced during shutdown!');
    Exit;
  end; {if}
  Result := InternalThreadPool;
  if not Assigned(Result) then begin
    Result := TThreadPool.Create as IThreadPool;
    Result._AddRef; // the call below does not increment the refcount!
    P:= InterlockedCompareExchangeObject(InternalThreadPool,
      TObject(Pointer(Result)), nil);
    if P <> nil then begin
      {Some other threads got there first, release the pool we just
       created and use the other one.}
      Result._Release;
      Result := InternalThreadPool;
    end; {if}
  end {if}
end; {ThreadPool}

function CreateWorkerthread: IThreadManager;
begin
  Result := TThreadManager.Create as IThreadManager;
end;

function CreateComWorkerthread: IThreadManager;
begin
  Result := TComThreadManager.Create as IThreadManager;
end;

{== TThreadPool =======================================================}

constructor TThreadPool.Create;
begin
  inherited Create;
  FPoolGuard := TCriticalSection.Create();
  FMaxThreads := NMaxThreadsDefault;
  FTaskQueue := TInterfaceList.Create;
end;

{! <remarks><para>
  Destroying the pool will remove and terminate all threads in the
  pool. The pool threads self-immolate when they are terminated.
  This destructor will wait for one second per thread and then mercilessly
  kill the thread if it has not terminated until then.</para>
  <para>
  Any tasks remaining in the pool's queue will be abandonned!</para>
 </remarks>
}
destructor TThreadPool.Destroy;
var
  Temp: TPoolThread;
  Handles: array of THandle;
  I: Integer;
begin
  if Assigned(PoolGuard) then begin
    PoolGuard.Acquire;
    while Assigned(FPool) do begin
      Temp:= FPool;
      FPool := Temp.Unhook;
      Temp.OnThreadDying := nil;
      SetLength(Handles, Length(Handles)+1);
      Handles[High(Handles)] := Temp.Handle;
      Temp.Die;
    end;
    FreeAndNil(FPoolGuard);
    for I := 0 to High(Handles) do begin
      if WaitForSingleObject(Handles[I], NOneSecond) = WAIT_TIMEOUT then
        TerminateThread(Handles[I], WAIT_TIMEOUT);
    end; {for}
  end; {if}
  FTaskQueue := nil;
  inherited Destroy;
end;

{! <summary>
  This method implements <see cref="IThreadPoolManager.AddThread"/>.
  It is used to add new worker threads to the pool (Repool = false)
  and to flag worker threads as available when they become idle
  (Repool = true).</summary>
 <remarks><para>
  If an idle worker returns to the workforce we check if there are
  calls in the pools queue, if that is the case the worker is handed the
  first task from the queue. Otherwise we check whether the maximum
  number of idle threads is already in the pool. If this is the case
  the worker is removed from the pool and layed off, Mafia-style.</para>
  <para>
  This method can be called from diverse threads, the pool will be locked
  while it executes.
 </remarks> </para>
}
procedure TThreadPool.AddThread(aThread: TPoolThread; Repool: Boolean =
    true);
var
  Task: IExecutor;
begin
  if not Assigned(aThread) then
    raise EParameterCannotBeNil.Create('TThreadPool.AddThread','aThread');
  PoolGuard.Acquire;
  try
    if Repool then begin
      {This is supposed to be an old worker to recycle. }
      if FTaskQueue.Count > 0 then begin
        Task := FTaskQueue.First as IExecutor;
        FTaskQueue.Delete(0);
        aThread.SubmitTask(Task);
      end {if}
      else if FNumThreads > FMaxThreads then begin
        RemoveThread(aThread);
        aThread.Die;
      end; {if}
    end {if}
    else begin
      {This is supposed to be a new worker to introduce to the pool. }
      aThread.OnThreadDying := ThreadDied;
      aThread.Hook(FPool);
      FPool := aThread;
      Inc(FNumThreads);
    end; {else}
  finally
    PoolGuard.Release;
  end; {finally}
end;

{! <summary>
  Submits an executor for execution in a worker thread.</summary>
 <remarks><para>
  This method is usually called from the main thread but can be called
  from other threads as well. It will be called by one of the methods of
  the <see cref="IThreadManager"/> interface. </para>
  <para>
  We create a wrapper for the call and return an IAsyncCall interface
  for it. The wrappers executor interface is then passed to a free
  worker thread. If there is none available the interface is added to
  the pools queue. </remarks> </para>
}
function TThreadPool.DoBeginInvoke(aExecutor: IExecutor): IAsyncCall;
var
  Worker: TPoolThread;
begin
  Result := TAsyncCall.Create(aExecutor);
  PoolGuard.Acquire;
  try
    Worker := FindWorker;
    if Assigned(Worker) then
      Worker.SubmitTask(Result as IExecutor)
    else
      FTaskQueue.Add(Result as IInterface);
  finally
    PoolGuard.Release;
  end;
end;

{! <summary>
  Find or create an idle worker thread and return its reference.</summary>
 <remarks> <para>
  If no idle thread is in the queue a new one is created unless the
  maximum number of threads has been reached already and we are not
  allowed to create more. In this case the method will return nil.</para>
  <para>
  Note: since the method is only called internally we know that the pool
  will be already locked, so no need to do that here again.</remarks></para>
}
function TThreadPool.FindWorker: TPoolThread;
begin
  Result := FPool;
  while Assigned(Result) and not Result.Idle do
    Result := Result.Next;
  if not Assigned(Result) then
    if not LimitToMaxThreads or (FNumThreads < FMaxThreads) then begin
      Result := TPoolThread.Create;
      AddThread(Result, false);
    end; {if}
end;

{! <summary>
  Count the number of idle threads in the pool and return that number.</summary>
}
function TThreadPool.GetIdleThreads: Integer;
var
  Walker: TPoolThread;
begin
  Result := 0;
  Poolguard.Acquire;
  try
    Walker := FPool;
    while Assigned(Walker) do begin
      if Walker.Idle then
        Inc(Result);
      Walker := Walker.Next;
    end; {while}
  finally
    Poolguard.Release;
  end;
end;

function TThreadPool.GetLimitToMaxThreads: Boolean;
begin
  Result := FLimitToMaxThreads;
end;

function TThreadPool.GetMaxThreads: Integer;
begin
  Result := FMaxThreads;
end;

function TThreadPool.GetThreadcount: Integer;
begin
  Result := FNumThreads;
end;

{! <summary>Remove a thread from the pool. </summary>
 <remarks>
  Note: since the method is only called internally we know that the pool
  will be already locked, so no need to do that here again.</remarks>
}
procedure TThreadPool.RemoveThread(aThread: TPoolThread);
var
  Walker: TPoolThread;
begin
  if FPool = aThread then begin
    FPool := aThread.Unhook;
    Dec(FNumThreads);
  end {if}
  else begin
    Walker := FPool;
    while Assigned(Walker) and (Walker.next <> aThread) do
      Walker := Walker.Next;
    if Assigned(Walker) then begin
      Walker.Hook(aThread.Next);
      Dec(FNumThreads);
    end; {if}
  end; {else}
  aThread.OnThreadDying := nil;
end;

procedure TThreadPool.SetLimitToMaxThreads(const Value: Boolean);
begin
  FLimitToMaxThreads := Value;
end;

procedure TThreadPool.SetMaxThreads(const Value: Integer);
begin
  FMaxThreads := Value;
end;

{! <remarks>
 A pool thread is dying unexpectedly, remove it from the pool. This
 method will execute in the dying threads context, it handles the
 threads OnThreadDying event. </remarks>}
procedure TThreadPool.ThreadDied(sender: TObject);
begin
  PoolGuard.Acquire;
  try
    RemoveThread(sender as TPoolThread);
  finally
    PoolGuard.Release;
  end; {finally}
end;

{== TSimpleExecutor ===================================================}

constructor TSimpleExecutor.Create(ACallback: TSimpleAsyncCallback);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TSimpleExecutor.Execute;
begin
  Callback;
end;

function TSimpleExecutor.GetState: TObject;
begin
  Result := nil;
end;

{== TStandAloneExecutor ===============================================}

constructor TStandAloneExecutor.Create(ACallback: TSimpleAsyncProc);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TStandAloneExecutor.Execute;
begin
  Callback;
end;

function TStandAloneExecutor.GetState: TObject;
begin
  Result := nil;
end;

{$IFDEF SUPPORTS_ANONYMOUS_METHODS}
{== TClosureExecutor ===============================================}

constructor TClosureExecutor.Create(ACallback: TProc);
begin
  inherited Create;
  FCallback := ACallback;
end;

procedure TClosureExecutor.Execute;
begin
  Callback;
end;

function TClosureExecutor.GetState: TObject;
begin
  Result := nil;
end;
{$ENDIF}

{== TExecutorWithState ================================================}

constructor TExecutorWithState.Create(ACallback: TAsyncCallback;
    AState: TObject);
begin
  inherited Create;
  FCallback := ACallback;
  FState := AState;
end;

procedure TExecutorWithState.Execute;
begin
  Callback(FState);
end;

function TExecutorWithState.GetState: TObject;
begin
  Result := FState;
end;

{== TPoolThread =======================================================}

{! <remarks><para>
  A worker thread will usually be part of a thread pool (aStandalone =
  false) but it can also be used in isolation (aStandalone = true). In
  the latter case it will not repool itself when a task completes.</para>
  <para>
  The worker thread has an internal task queue and a signal on which it
  will wait while the queue is empty. The thread will self-destruct when
  it terminates.</para></remarks>
}
constructor TPoolThread.Create(AStandalone: Boolean = false);
begin
  FSignal := TSimpleEvent.Create;
  FQueue := TInterfaceList.Create;
  FQueueEmptySignal := TSimpleEvent.Create;
  FIdle := true;
  FStandalone := AStandalone;
  inherited Create(false);
  FreeOnTerminate := true;
end;

destructor TPoolThread.Destroy;
begin
  FQueueEmptySignal.Free;
  FreeAndNil(FSignal);
  FQueue := nil;
  inherited Destroy;
end;

{! <summary>
  Terminates and sets the thread up for destruction</summary>
}
procedure TPoolThread.Die;
begin
  Terminate;
  try
    FSignal.SetEvent;
  except on Exception do
    {There is a very slight chance that the thread has already self-
     destructed when we try to set the signal. That could result in
     an access violation or invalid handle error. Ignore that here. }
  end;
end;

{! <summary>
  Fire the OnThreadDying event.</summary>
}
procedure TPoolThread.DoThreadDying;
begin
  if Assigned(FOnThreadDying) then FOnThreadDying(Self);
end;

{! <summary>
  Wait for tasks and execute them until the thread is terminated.</summary>
 <remarks>
  Any exceptions will be trapped and logged, but they will terminate the
  thread. A notification event is fired before the thread ends up in the
  Great Bit Bucket Beyond. Note that the tasks are responsible to trap
  and handle execptions in their execution, so the thread itself will not
  get terminated by those errors.</remarks>
}
procedure TPoolThread.Execute;
const
  SSeparator = ': ';
begin
  InitializeThread;
  try
    try
      while not Terminated do begin
        WaitForWork;
        if not Terminated then
          RunTasks;
        if not (Terminated or StandAlone) then
          RepoolThread;
      end; {if}
    except
      on E: Exception do
        ODS(E.ClassName + SSeparator+E.Message);
        {We cannot log to the standard log manager here since that
         uses the threading framework itself! ODS uses OutputDebugString,
         which is safe. }
    end;
  finally
    FinalizeThread;
  end;
  DoThreadDying;
end;

{!
<summary>
 Called from the end of the threads Execute object, can be used by
 descendents to perform some thread finalization.</summary>
}
procedure TPoolThread.FinalizeThread;
begin
  // Does nothing, descendents can override it as needed.
end;

{!
  Sets the Next property to the passed value and returns the old
  value of Next. This method is threadsafe.</summary>
}
function TPoolThread.Hook(NewNext: TPoolThread): TPoolThread;
begin
  Result := TPoolThread(InterlockedExchangeObject(FNext, NewNext));
end;

{!
<summary>
 Called from the start of the threads Execute object, can be used by
 descendents to perform some thread initialization.</summary>
}
procedure TPoolThread.InitializeThread;
begin
  // Does nothing, descendents can override it as needed.
end;

procedure TPoolThread.PrepareForClose(aTimeoutMSecs: Cardinal =
    INFINITE);
begin
  Dying := true;
  if not (Terminated or Idle) then begin
    QueueEmptySignal.ResetEvent;
    QueueEmptySignal.Waitfor(aTimeoutMSecs);
  end;
end;

{! <summary>
  Set the threads state to idle and tell the pool manager that the
  thread is available for work.</summary>
}
procedure TPoolThread.RepoolThread;
var
  Temp: IThreadPool;
begin
  FIdle := true;
  Temp := InternalThreadPool;
  if Assigned(Temp) and not Terminated then
    (Temp as IThreadPoolManager).AddThread(Self);
end;

{! <summary>
  Execute tasks from the queue until the queue runs dry or the thread
 is terminated. </summary>
}
procedure TPoolThread.RunTasks;
var
  Intf: IInterface;
begin
  if FQueue.Count > 0 then
  repeat
    Intf := FQueue.First;
    FQueue.Delete(0);
    (Intf as IExecutor).Execute;
  until (FQueue.Count = 0) or Terminated;
  QueueEmptySignal.SetEvent;
end;

{! <summary>
  Adds the passed task to the threads queue and wakes the thread.</summary>
}
procedure TPoolThread.SubmitTask(const aExecutor: IExecutor);
begin
  if Terminated or Dying then Exit;

  Assert(Assigned(aExecutor));
  FIdle := false;
  FQueue.Add(aExecutor as IInterface);
  FSignal.SetEvent;
end;

{! <summary>
  Set the Next property to nil and returns its old value</summary>
}
function TPoolThread.Unhook: TPoolThread;
begin
  Result := TPoolThread(InterlockedExchangeObject(FNext, nil));
end;

{! <summary>
  Block the thread until there is something to do.</summary>
}
procedure TPoolThread.WaitForWork;
begin
  FIdle := true;
  FSignal.WaitFor(INFINITE);
  FSignal.ResetEvent;
end;

{== TAsyncCall ========================================================}

constructor TAsyncCall.Create(AExecutor: IExecutor);
begin
  Assert(Assigned(AExecutor));
  inherited Create;
  FExecutor := AExecutor;
  {Note: we do not create the wait event here because it may never be
   needed. The FSignal object is created when needed. The same applies
   to the cross-thread messenger, this is only needed when a notification
   callback is installed and it has to be created in the thread that
   installs the callback! }
end;

destructor TAsyncCall.Destroy;
begin
  FSignal.Free;
  FMessenger.Free;
  inherited Destroy;
end;

{! <summary>
  Create the wait event if it's not already there.</summary>
}
procedure TAsyncCall.EnsureSignalIsCreated;
var
  Temp: TSimpleEvent;
  Ret: TObject;
begin
  if not Assigned(FSignal) then begin
    Temp := TSimpleEvent.Create;
    Ret := InterlockedCompareExchangeObject(FSignal, Temp, nil);
    if Ret <> nil then
      Temp.Free;
  end; {if}
end;

{! <summary>
  Execute the call, trap and store any exception for later inspection.</summary>
}
procedure TAsyncCall.Execute;
begin
  try
    FExecutor.Execute;
    FSucceeded := true;
  except
    on E: Exception do begin
      FErrorAddress:= ErrorAddr;
      FLastError := AcquireExceptionObject;
      FSucceeded := false;
    end;
  end; {except}
  SetCompleted;
end;

function TAsyncCall.GetAsyncState: TObject;
begin
  Result := FExecutor.State;
end;

{! Return the wait events handle. This method is provided only for
 compatability with the .NET IAsynResult interface. }
function TAsyncCall.GetAsyncWaitHandle: THandle;
begin
  EnsureSignalIsCreated;
  if FCompleted then
    FSignal.SetEvent;
  Result := FSignal.Handle;
end;

{! We do not support synchronous execution. }
function TAsyncCall.GetCompletedSynchronously: Boolean;
begin
  Result := false;
end;

function TAsyncCall.GetFailed: Boolean;
begin
  Result := not FSucceeded;
end;

function TAsyncCall.GetIsCompleted: Boolean;
begin
  Result := FCompleted;
end;

function TAsyncCall.GetLastError: Exception;
begin
  Result := FLastError;
end;

function TAsyncCall.GetState: TObject;
begin
  Result := FExecutor.State;
end;

function TAsyncCall.GetSucceeded: Boolean;
begin
  Result := FSucceeded;
end;

{! <summary>
  Define a method to call when the async task has completed.  </summary>
 <param name="aCallback">The method to execute when the task is done.
   Cannot be nil. </param>
 <exception cref="EAsyncCallError">Raises EAsyncCallError if a different
  thread has already installed a notification callback.</exception>
 <exception cref="EParameterCannotBeNil">is raised if aCallback is nil.
  </exception>
 <remarks>
  Note that the method will execute in the thread context of the caller.
  The callback may be called before this method has returned if the task
  has already completed. The calling thread must have a message loop!
  If it does not the worker thread trying to call the callback will
  be blocked, since it tries to execute the callback in the callers
  thread context. </remarks> ,
}
procedure TAsyncCall.NotifyWhenDone(aCallback: TAsyncNotification);
var
  Temp: TCrossThreadMessenger;
  Ret: TObject;
begin
  if not Assigned(aCallback) then
    raise EParameterCannotBeNil.Create('TAsyncCall.NotifyWhenDone','aCallback');
  if not Assigned(FMessenger) then begin
    Temp := TCrossThreadMessenger.Create;
    Ret := InterlockedCompareExchangeObject(FMessenger, Temp, nil);
    if Ret <> nil then begin
      Temp.Free;
      raise EAsyncCallError.Create(SNotificationAlreadyInstalled);
    end; {if}
  end {if}
  else
    if GetCurrentThreadID <> FMessenger.CreatorThreadID then
      raise EAsyncCallError.Create(SNotificationAlreadyInstalled);
  FNotificationProc := aCallback;
  if FCompleted then begin
    aCallback(Self as IAsyncCall);
    FNotificationProc := nil;
    {This prevents a duplicate call to the notification proc if a
     call was queued to the messenger by the worker thread while
     we had not reached the if statement yet due to a thread switch.
     The messenger cannot process the queued call until we get back
     to the message loop.}
  end; {if}
end;

{! <summary>
  Raise the exception that caused the call to fail in the callers thread.</summary>
 <exception cref="EPreconditionViolation">is raised if the call has not
  completed or we have no trapped exception. </exception>
}
procedure TAsyncCall.RaiseLastError;
const
  Procname = 'TAsyncCall.RaiseLastError';
var
  Temp: Exception;
begin
  if not FCompleted then
    raise EPreconditionViolation.Create(Procname, 'Call still running!');
  Temp := FLastError;
  if not Assigned(Temp) then
    raise EPreconditionViolation.Create(Procname, 'No last error to raise');
  if Assigned(Temp) then begin
    FLastError := nil;
    raise Temp at FErrorAddress;
  end; {if}
end;

{! <remarks>
  Mark the call as completed, signal the wait event, if we have one,
  and fire the notification callback, if one was installed. Since the
  notification will be executed in a separate thread we have to make
  sure this object is not destroyed before the notification has been
  executed. We increment the reference count to ensure this.</remarks>
}
procedure TAsyncCall.SetCompleted;
begin
  FCompleted := true;
  if Assigned(FSignal) then
    FSignal.SetEvent;
  if Assigned(FMessenger) then begin
    _AddRef;  // paired with a _Release in CallNotificationProc
    try
      FMessenger.QueueCall(CallNotificationProc);
    except
      _Release;
      raise;
    end; {except}
  end; {if}
end;

{$STACKFRAMES OFF}
{in: value in eax
 out: result in edx:eax }
function ToInt64( Value: DWORD ): Int64;
asm
  xor edx, edx
end;
{$STACKFRAMES ON}

procedure TAsyncCall.CallNotificationProc;
begin
  if Assigned(FNotificationProc) then
    FNotificationProc(Self as IAsyncCall);
  _Release;  // paired with a _AddRef in SetCompleted
end;

{! Wait for the call to complete. Optionally we can process a subset
  of messages during the wait. That complicates the timeout handling,
  though, since the wait can be interrupted by messages.}
function TAsyncCall.WaitForCompletion(TimeoutMSecs: Cardinal;
  ProcessMessages: Boolean): TWaitResult;
var
  TargetTime, CurrentTime: int64;
  H: THandle;
  Ret: DWORD;

  function TimeRemaining: DWORD;
  begin
    if TimeoutMSecs = INFINITE then
      Result := INFINITE
    else begin
      CurrentTime := ToInt64(GetTickCount());
      if CurrentTime > TargetTime then
        Result := 0
      else begin
        Result := TargetTime - CurrentTime;
        if Result > TimeoutMSecs then
          Result := 0;  // GetTickCount rolled over
      end;
    end;
  end;

  {! We want to block most user input here but allow paint messages
    to be processed and also non-client messages that relate to moving
    or minimizing the window to be acted upon. Timer messages are also
    processed! }
  procedure ProcessPendingMessages;
  var
    Msg: TMsg;
  const
    NSCMask = $FFF0;
  begin
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
      case Msg.message of
        WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST: ; // swallow
        WM_NCMOUSEMOVE..WM_NCLBUTTONDBLCLK:
          case Msg.wParam of
            HTCAPTION, HTMINBUTTON: DispatchMessage(Msg);
          end; {case}
        WM_SYSCOMMAND:
          case Msg.wParam and NSCMask of
            SC_MINIMIZE, SC_RESTORE: DispatchMessage(Msg);
          end; {case}
      else
        DispatchMessage(Msg);
      end; {case}
  end;

begin
  EnsureSignalIsCreated;
  if FCompleted then
    Result := wrSignaled
  else
    if ProcessMessages then begin
      TargetTime := ToInt64(GetTickCount()) + TimeoutMSecs;
      H:= FSignal.Handle;
      Result := wrAbandoned;
      repeat
        Ret := MsgWaitForMultipleObjectsEx(
          1, H, TimeRemaining, QS_ALLINPUT, 0);
        case Ret of
          WAIT_ABANDONED : Exit;
          WAIT_FAILED    : Result := wrError;
          WAIT_TIMEOUT   : Result := wrTimeout;
          WAIT_OBJECT_0  : Result := wrSignaled;
          WAIT_OBJECT_0+1: ProcessPendingMessages;
        end; {case }
      until Result <> wrAbandoned;
    end {if}
    else
      Result := FSignal.WaitFor(TimeoutMSecs);
end;

{== TThreadManager ====================================================}

constructor TThreadManager.Create;
begin
  inherited;
  CreateWorker;
end;

destructor TThreadManager.Destroy;
var
  H: THandle;
  Res: Cardinal;
begin
  if Assigned(Worker) then begin
    H:= Worker.Handle;
    Worker.OnThreadDying := nil;
    Worker.Die;
    Res:= WaitForSingleObject(H, NOneSecond);
    if Res = WAIT_TIMEOUT then
      TerminateThread(H, WAIT_TIMEOUT);
  end; {if}
  inherited Destroy;
end;

procedure TThreadManager.CreateWorker;
begin
  FWorker := NewWorkerthread;
  FWorker.OnThreadDying := ThreadDied;
end;

function TThreadManager.DoBeginInvoke(aExecutor: IExecutor): IAsyncCall;
begin
  Result := TAsyncCall.Create(aExecutor);
  if not Assigned(Worker) then
    CreateWorker;
  Worker.SubmitTask(Result as IExecutor)
end;

function TThreadManager.NewWorkerthread: TPoolThread;
begin
  Result := TPoolThread.Create(true);
end;

procedure TThreadManager.PrepareForClose(aTimeoutMSecs: Cardinal =
    INFINITE);
begin
  if Assigned(Worker) then
    Worker.PrepareForClose(aTimeoutMSecs);
end;

procedure TThreadManager.ThreadDied(sender: TObject);
begin
  FWorker := nil;
end;

{== TBaseThreadHandler ================================================}

function TBaseThreadHandler.BeginInvoke(aCallback: TAsyncCallback;
    aState: TObject = nil): IAsyncCall;
begin
  if not Assigned(aCallback) then
    CallbackMissing;
  Result := BeginInvoke(
    TExecutorWithState.Create(aCallback, aState) as IExecutor);
end;

function TBaseThreadHandler.BeginInvoke(aExecutor: IExecutor):
    IAsyncCall;
begin
  if not Assigned(aExecutor) then
    raise EParameterCannotBeNil.Create('TBaseThreadHandler.BeginInvoke','aExecutor');
  Result := DoBeginInvoke(aExecutor);
end;

function TBaseThreadHandler.BeginInvoke(
  aCallback: TSimpleAsyncCallback): IAsyncCall;
begin
  if not Assigned(aCallback) then
    CallbackMissing;
  Result := BeginInvoke(
    TSimpleExecutor.Create(aCallback) as IExecutor);
end;

function TBaseThreadHandler.BeginInvoke(
  aCallback: TSimpleAsyncProc): IAsyncCall;
begin
  if not Assigned(aCallback) then
    CallbackMissing;
  Result := BeginInvoke(
    TStandAloneExecutor.Create(aCallback) as IExecutor);
end;

{$IFDEF SUPPORTS_ANONYMOUS_METHODS}
function TBaseThreadHandler.BeginInvoke(aClosure: TProc): IAsyncCall;
begin
  if not Assigned(aClosure) then
    CallbackMissing;
  Result := BeginInvoke(
    TClosureExecutor.Create(aClosure) as IExecutor);
end;

procedure TBaseThreadHandler.QueueUserWorkitem(aClosure: TProc);
begin
  BeginInvoke(aClosure);
end;
{$ENDIF}

procedure TBaseThreadHandler.CallbackMissing;
begin
  raise EParameterCannotBeNil.Create('TBaseThreadHandler.BeginInvoke','aCallback');
end;

function TBaseThreadHandler.EndInvoke(aCall: IAsyncCall; aTimeoutMSecs:
    Cardinal = INFINITE): TObject;
begin
  if not Assigned(aCall) then
    raise EParameterCannotBeNil.Create('TBaseThreadHandler.EndInvoke','aCall');
  if wrTimeout = aCall.WaitForCompletion(aTimeoutMSecs,
    (MainThreadID = GetCurrentThreadID) and not IsConsole)
  then
    raise EAsyncTimeoutError.CreateFmt(SEndInvokeTimeout, [aTimeoutMSecs]);
  Result := aCall.AsyncState;
end;

procedure TBaseThreadHandler.QueueUserWorkitem(aExecutor: IExecutor);
begin
  BeginInvoke(aExecutor);
end;

procedure TBaseThreadHandler.QueueUserWorkitem(aCallback:
    TAsyncCallback; aState: TObject = nil);
begin
  BeginInvoke(aCallback, aState);
end;

procedure TBaseThreadHandler.QueueUserWorkitem(aCallback:
    TSimpleAsyncCallback);
begin
  BeginInvoke(aCallback);
end;

procedure TBaseThreadHandler.QueueUserWorkitem(
  aCallback: TSimpleAsyncProc);
begin
  BeginInvoke(aCallback);
end;

function TComThreadManager.NewWorkerthread: TPoolThread;
begin
  Result := TComPoolThread.Create(true);
end;

procedure TComPoolThread.FinalizeThread;
begin
  CoUninitialize;
end;

procedure TComPoolThread.InitializeThread;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
end;

initialization
finalization
{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Finalizing AsyncTasksU...');
{$ENDIF}

    InternalShutDownInProgress := true;
    InternalThreadPool := nil;

{$IFDEF DEBUG_FINALIZATION}
OutputDebugString('Done finalizing AsyncTasksU');
{$ENDIF}
end.


