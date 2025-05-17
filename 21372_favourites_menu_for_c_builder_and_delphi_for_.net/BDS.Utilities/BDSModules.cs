using System;
using Borland.Studio.ToolsAPI;

namespace MarcRohloff.BDS.Utilities
{
	public class BDSModules
	{
        public static IOTAModule CurrentModule
          { get { return Service.CurrentModule; } }

		public static IOTAProject CurrentProject
  		  { get
            { IOTAProjectGroup g = CurrentProjectGroup;
              return (g!=null) ? g.ActiveProject : null;   }
          }

		public static IOTAProjectGroup CurrentProjectGroup
  		  { get { return Service.MainProjectGroup; } }

        public static IOTAModule GetTopLevelModule(out bool isGroup)
        {
           IOTAModule module;

           module = BDSModules.CurrentProjectGroup;
           if ( (module != null) && System.IO.File.Exists(module.FileName) )
           {
             isGroup = true;
           }
           else
           {
             module = BDSModules.CurrentProject;
             isGroup = false;
           };

           return module;
        }

        public static IOTAModuleServices Service
           { get { return BDSServices.ModuleService; } }

        #region private methods and fields

		private BDSModules() {} /*static class*/

        #endregion private methods and fields

    } /* class BDSModules */

}   /* namespace */

