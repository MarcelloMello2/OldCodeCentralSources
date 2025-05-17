using System;
using Borland.Studio.ToolsAPI;

//These routines are unsafe in StandAlone mode

namespace MarcRohloff.BDS.Utilities
{
	public class BDSServices
	{
       public static IOTAService Service
         { get { return (IOTAService)GetService(typeof(IOTAService)); } }

       public static IOTAMainMenuService MenuService
         { get { return (IOTAMainMenuService)GetService(typeof(IOTAMainMenuService)); } }

       public static IOTAAddInService AddInService
         { get { return (IOTAAddInService)GetService(typeof(IOTAAddInService)); } }

       public static IOTAWizardService WizardService
         { get { return (IOTAWizardService)GetService(typeof(IOTAWizardService)); } }

       public static IOTAModuleServices ModuleService
         { get { return (IOTAModuleServices)GetService(typeof(IOTAModuleServices)); } }

       public static IOTAActionService ActionService
         { get { return (IOTAActionService)GetService(typeof(IOTAActionService)); } }

       public static IOTAAboutBoxService AboutBox
         { get { return (IOTAAboutBoxService)GetService(typeof(IOTAAboutBoxService)); } }

       #region private fields and methods

       private static object GetService(Type t)
       {
         if (BorlandIDE.Services==null)
           throw new BDSException("BDS Services not initialized");

         object o = BorlandIDE.GetService(t);

         if (o==null)
           throw new BDSException("BDS Service of type " + t.Name + " not found");

         if (!t.IsInstanceOfType(o))
           throw new BDSException("BDS Service of type " + t.Name + " is invalid");

         return o;
       }

	   private BDSServices() {} //Static class
       #endregion private fields and methods
	}
}
