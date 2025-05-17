{== ExecutorIntfU =====================================================}
{! <summary>
 This unit defines the executor interfaces used by the asynchronous
 processing framework.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2007-02-06</para>
<para>Last modified       2008-08-17</para>
</history>
<remarks>
The purpose of these interfaces is to offer a generic way to allow
methods or even plain functions with arbitrary parameter lists to be
executed in the context of another thread. For this purpose the caller
has to construct a helper object that contains the parameter values
to pass to the method/function, as well as the required method or
function pointer. The helper has to implement the required executor
interface, who's Execute method will be called in the context of the
target thread. This method then does the actual method or function call.
Some simple implementors can be found in the <see cref="AsyncTasksU"/>
unit.</remarks>
<copyright>Copyright 2007-2010 by Dr. Peter Below</copyright>
<licence>
The code in this unit is released to the public domain without
restrictions for use or redistribution. Just leave the copyright note
above intact. The code carries no warranties whatsoever, use at your
own risk!</licence>}
unit ExecutorIntfU;

interface

type
  {!
  <remarks>
   IStatelessExecutor is the executor interface required by the
   <see cref="TCrossThreadMessenger"/>
   class. It allows us to implement functionality like the
   TThread.Synchronize method for methods with parameters.</remarks>
  }
  IStatelessExecutor = interface(IInterface)
  ['{FA34D800-9942-4E1B-B370-F5443F5A2E6C}']
    procedure Execute;
  end;

  {!
  <remarks>
   The IExecutor interface has to be implemented by helper classes that wrap
   methods with parameters that cannot be directly passed to the
   thread pools methods for asynchronous execution.
   The parameters to use have to be loaded into the helper object first.
   The thread pool will then call the Execute method in the context of a
   secondary thread and that method can call the actual callback with
   the required parameters. </remarks>
  }
  IExecutor = interface(IInterface)
  ['{23DD04FE-790E-4001-B81A-DD293E9CA4AC}']
    procedure Execute;
    function GetState: TObject;
    {!
    <value>
     Returns the state object passed to the thread or threadpool
     when the executor was queued for execution. May be nil if no state
     object was used.</value>
    }
    property State: TObject read GetState;
  end;

implementation

end.
