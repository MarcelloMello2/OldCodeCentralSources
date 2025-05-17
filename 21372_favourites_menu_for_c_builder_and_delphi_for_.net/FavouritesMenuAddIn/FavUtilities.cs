using System;
using System.IO;
using System.Text;
using System.Runtime.InteropServices;

namespace MarcRohloff.FavouritesMenuAddIn
{
	public class FavUtilities
	{
	  private FavUtilities() {} //Static Class

      #region Public Methods
      static public string GetLongFilename(string orig, out bool exists)
      {
        char sep = Path.DirectorySeparatorChar;
        orig = Path.GetFullPath(orig); 

        string[] paths = orig.Split(sep);
        if (paths.Length==0) {exists = false; return orig;}

        string lastName=null;

        //We don't access any slow drives
        if ( (paths[0].Length==2) && (paths[0][1]==Path.VolumeSeparatorChar) )
        {
          paths[0] = paths[0].ToUpper();
          if (GetDriveType(paths[0])==DRIVE_FIXED)
            lastName = paths[0];
        }

        StringBuilder fn = new StringBuilder();

        for (int i = 1; i<paths.Length; i++)
        {
          fn.Append(lastName!=null ? lastName : paths[i-1]);
          fn.Append(Path.DirectorySeparatorChar);

          if (lastName!=null)
            lastName = FindLongName(fn.ToString(), paths[i]);
        }

        exists = (lastName != null);
        fn.Append(lastName!=null ? lastName : paths[ paths.Length-1 ] );

        return fn.ToString();
      }

      static public Microsoft.Win32.RegistryKey Registry()
        { return BDS.Utilities.BDSRegistry.OpenKey(null, Constants.sFavRegKey, true); }

      #endregion Public Methods

      #region Private Methods
      private static string FindLongName(string dir, string fn)
      {
        WIN32_FIND_DATA fd = new WIN32_FIND_DATA();
        IntPtr h = FindFirstFile(dir + fn,
                                  out fd);
        if (h.ToInt32()!=-1)
        {
          FindClose(h);
          return fd.cFileName;
        }

        return null;
      }

      #endregion Private Methods

      #region DLL Imports
      private struct WIN32_FIND_DATA
      {
        internal int dwFileAttributes;
        internal int ftCreationTime_dwHighDateTime;
        internal int ftCreationTime_dwLowDateTime;
        internal int ftLastAccessTime_dwHighDateTime;
        internal int ftLastAccessTime_dwLowDateTime;
        internal int ftLastWriteTime_dwHighDateTime;
        internal int ftLastWriteTime_dwLowDateTime;
        internal int nFileSizeHigh;
        internal int nFileSizeLow;
        internal int dwReserved0;
        internal int dwReserved1;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=MAX_PATH)]
        internal string cFileName;
        [MarshalAs(UnmanagedType.ByValTStr, SizeConst=14)]
        internal string cAlternateFileName;
      }

      [DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true) ]
      private static extern
        IntPtr FindFirstFile(string pFileName,
                             out WIN32_FIND_DATA pFindFileData);

      [DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true) ]
      private static extern
        bool FindClose(IntPtr hndFindFile);


      [DllImport("kernel32.dll", CharSet=CharSet.Ansi, SetLastError=true) ]
      private static extern
        uint GetDriveType(string lpRootPathName);

      const int DRIVE_FIXED	 = 3;
      const int MAX_PATH     = 260;

      #endregion DLL Imports
	}
}
