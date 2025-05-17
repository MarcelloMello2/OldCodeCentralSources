using System;
using Microsoft.Win32;

//All functions are safe to use standalone

namespace MarcRohloff.BDS.Utilities
{
	/// <summary>
	/// Summary description for Class.
	/// </summary>
	public class BDSRegistry
	{

        #region registry keys
        public const string BDSRootRegPath = @"Software\Borland\BDS\";

	    public const string KnownAssemKey = @"Known IDE Assemblies";
        #endregion registry keys


        static public string BaseRegistryPath
        { get
          {
            if (!BDSInterop.StandAlone)
              return UnsafeBaseRegistryPath;

            BDSVersion v = BDSVersions.CurrentVersion;
            if (v==null)
              return null;
            else
              return v.RegPath;
           }
        }


        public static string FixPath(string regPath)
        {
          if ( (regPath!=null) && (regPath.Length>0) && (regPath[0]=='\\') )
            regPath = regPath.Remove(0,1);
          return regPath;
        }

        public static RegistryKey OpenKey(RegistryKey root, string subkey, bool writeable)
        {
          if (root==null)
            root = Registry.CurrentUser;

          string key = FixPath(BaseRegistryPath);

          if ( (subkey != null) && (subkey!="") )
          {
            if (subkey[0]!='\\')
               subkey = subkey.Insert(0, "\\");
            key = key + subkey;
          }

          if (writeable)
             return root.CreateSubKey(key);
          else
             return root.OpenSubKey(key);
        }


        #region private methods and fields

		private BDSRegistry() {}  //Static class

        static private string UnsafeBaseRegistryPath
          { get { return  BDSServices.Service.BaseRegistryKey; } }

        #endregion private methods and fields

	}
}
