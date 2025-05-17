using System;

//All functions are safe to use in StandAlone mode

namespace MarcRohloff.BDS.Utilities
{

	public class BDSFiles
	{
        public static string RemoveTrailingDirectorySeperators(string s)
        {
          return s.TrimEnd( new char[] {System.IO.Path.DirectorySeparatorChar,
                                        System.IO.Path.AltDirectorySeparatorChar } );
        }

        public static string ContractEnvironmentStrings(string str)
        {

          string root = RootDir;

          if (root != null)
            if (String.Compare(root, 0, str, 0, root.Length, true)==0)
              str = BDSEnvironmentString + str.Remove(0, root.Length);

          return str;
        }


        public static string ExpandEnvironmentStrings(string str)
        {

          if (String.Compare(BDSEnvironmentString, 0, str, 0,
                             BDSEnvironmentString.Length, true)==0)
          {
            string root = RootDir;
            str = root + str.Remove(0, BDSEnvironmentString.Length);
          };

          return str;
        }


        static public string RootDir
        {
          get
          {
            Microsoft.Win32.RegistryKey r = BDSRegistry.OpenKey(Microsoft.Win32.Registry.CurrentUser, null, false);
            if (r==null)
              return "";

            using (r)
            {
               string res = (string)r.GetValue("RootDir");
               res = RemoveTrailingDirectorySeperators(res);
               return res;
            }
          }
        }


        public static bool IsAssemblyLoaded(string path)
        {
          if (BDSInterop.StandAlone)
             return false;

          path = ExpandEnvironmentStrings(path);

          foreach (System.Reflection.Assembly a
                     in AppDomain.CurrentDomain.GetAssemblies() )
          {
            if ( !(a is System.Reflection.Emit.AssemblyBuilder)
              &&   (String.Compare(a.Location, path, true)==0)    )
               return true;
          };

          return false;
        }


        public static void LoadAddIn(string path)
        {
          if (!BDSInterop.StandAlone)
             UnsafeLoadAddIn(path);
        }

        #region public fields

        public  const string BDSEnvironmentString    = @"$(BDS)";

        #endregion public fields

        #region private methods and fields

        private static void UnsafeLoadAddIn(string path)
        {
          path = ExpandEnvironmentStrings(path);
          Borland.Studio.ToolsAPI.IOTAAddInService svc = BDSServices.AddInService;

          //Using reflection because C# Builder defines LoadAddIn as:
          //  void LoadAddIn(string) while D8 defines it as bool LoadAddIn(string)
          //  This way we don;t need to rebuild
          if (loadAddInMI==null)
          {
            loadAddInMI = svc.GetType().GetMethod("LoadAddIn", new Type[] { typeof(string) } );
            if (loadAddInMI==null)
              throw new BDSException("Method IOTAAddInService.LoadAddIn(string) not found");
          }

          loadAddInMI.Invoke(svc, new object[] {path} );
        }

        private string StringReplace(string orig, string from, string to)
        {
          int s = 0;
          string work = orig;
          while (true)
          {
            s = System.Globalization.CultureInfo.InvariantCulture.CompareInfo.IndexOf(work,
                 from,  s, System.Globalization.CompareOptions.IgnoreCase);
            if (s<0) break;
            string temp;
            temp = work.Remove(s, from.Length);
            work = temp.Insert(s, to);
            s += to.Length;
          };

          return work;
        }

		private BDSFiles() {} //Static class

        private static System.Reflection.MethodInfo loadAddInMI;

        #endregion private methods and fields

	}
}
