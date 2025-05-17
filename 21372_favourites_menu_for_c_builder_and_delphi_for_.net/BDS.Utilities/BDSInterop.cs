//#define ForceStandalone

using System;
using Borland.Studio.ToolsAPI;
using Microsoft.Win32;

/////////////////////////////////////////////////
// Utilities for accessing the BDS functions
///////////////////
// The following functions/properties can be used from a
// standalone project running outside of the BDS
//   IsStandalone
//   BaseRegistryPath
//   RootDir
//   IsAssemblyLoaded()
//   LoadAssembly()
//   OpenKey()
//   ExpandEnvironmentStrings
//   ContractEnvironementStrings

namespace MarcRohloff.BDS.Utilities
{

    #region BDSException
    public class BDSException:Exception
    {
      public BDSException(string message):base(message) {}
      public BDSException(string message, Exception inner):base(message, inner) {}
    }
    #endregion BDSException

	public class BDSInterop
	{
        static public bool StandAlone
        { get { if (!standAloneDetected)
                   DetectStandAlone();
                return standAlone;   }  }

        static public void HandleException(Exception ex)
        {
          System.Windows.Forms.Application.OnThreadException(ex);
        }

        static public void AddWizard(IOTAWizard wizard)
        {
          int id = BDSServices.WizardService.AddWizard( wizard);
          wizards.Add(wizard, id);
        }

        static public void RemoveWizard(IOTAWizard wizard)
        {
          object id = wizards[wizard];
          if (id != null)
          {
            wizards.Remove(wizard);
            BDSServices.WizardService.RemoveWizard( (int)id );
          }
        }


        static public void AddAboutBox(IOTAWizard wizard,
                                       string title,
                                       string description,
                                       System.Drawing.Bitmap bitmap )
        {
          IntPtr hBmp = IntPtr.Zero;
          if (bitmap != null)
          {
            using (System.Drawing.Bitmap b = BDSGraphics.MakeBitmapSize(bitmap, 32))
            {
              b.MakeTransparent();
              hBmp = b.GetHbitmap();
            }
          };

          int id = BDSServices.AboutBox.AddPluginInfo
                     (title, description, hBmp,
                      false, "", "");

          aboutboxes.Add(wizard, id);
        }

        static public void RemoveAboutBox(IOTAWizard wizard)
        {
          object id = aboutboxes[wizard];
          if (id != null)
          {
            aboutboxes.Remove(wizard);
            BDSServices.AboutBox.RemovePluginInfo( (int) id );
          }
        }

        static public string MakeValidName(string original)
          { return original.Replace('.','_'); }

        #region private methods and fields

 		private BDSInterop()	{	} /* Static Class */


        static private void DetectStandAlone()
        {
          #if ForceStandalone
            standAlone = true;
          #else
            Type t = Type.GetType("Borland.Studio.ToolsAPI.BorlandIDE, Borland.Studio.ToolsAPI", false, true);
            standAlone = (t==null);
          #endif

          standAloneDetected = true;
        }

        static private bool standAloneDetected;
        static private bool standAlone;

        static private System.Collections.Specialized.HybridDictionary wizards =
          new System.Collections.Specialized.HybridDictionary();
        static private System.Collections.Specialized.HybridDictionary aboutboxes =
          new System.Collections.Specialized.HybridDictionary();

        #endregion Private methods and fields
	}
}
