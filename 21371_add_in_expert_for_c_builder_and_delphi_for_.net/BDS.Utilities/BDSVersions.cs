using System;

namespace MarcRohloff.BDS.Utilities
{
	public class BDSVersions
	{
      #region Public Properties
      public static readonly BDSVersion CSB1 =
        new BDSVersion(1.0, Product.CSB, 1.0,
                             null,
                             Language.CSharp);

      public static readonly BDSVersion Delphi8 =
        new BDSVersion(2.0, Product.Delphi, 8.0,
                             null,
                             Language.Pascal);


      public static readonly BDSVersion[] KnownVersions =
      {
        CSB1,
        Delphi8
      };

      public static BDSVersion[] InstalledVersions
      { get {
        CheckInstalledVersions();
        return installedVersions;
      } }

      public static BDSVersion CurrentVersion
      { get
        {
          CheckCurrentVersion();
          return currentVersion;
        }

        set
        {
          if ( (!BDSInterop.StandAlone) && (value != currentVersion) )
            throw new BDSException("Cannot set version when not running standalone");

          currentVersion = value;
        }
      }

      #endregion Public Properties

      #region Public Methods
      public static BDSVersion GetVersion(Product product, double version)
      {
        if (installedVersions==null)
          foreach (BDSVersion v in KnownVersions)
            if (v.Matches(product, version))
              return v;

        CheckInstalledVersions();

        if (installedVersions!=null)
          foreach (BDSVersion v in installedVersions)
            if (v.Matches(product, version))
              return v;

        return null;
      }

      public static BDSVersion GetVersion(Product product)
      { return GetVersion(product, 0); }
      #endregion Public Methods

      #region Lifetime Methods
      private BDSVersions() {} /* static class */
      #endregion Lifetime Methods

      #region Private Methods
      static private void CheckInstalledVersions()
      {
        if (installedVersions != null) return;

        Microsoft.Win32.RegistryKey reg =
          Microsoft.Win32.Registry.LocalMachine.OpenSubKey(BDSRegistry.BDSRootRegPath);
        if (reg==null) return;

        string[] subkeys = reg.GetSubKeyNames();
        reg.Close();
        if (subkeys==null) return;

        System.Collections.ArrayList list = new System.Collections.ArrayList();

        for(int i = subkeys.Length-1; i>=0; i--)
        {
          double ver;
          if (!double.TryParse(subkeys[i],
                               System.Globalization.NumberStyles.Any,
                               System.Globalization.NumberFormatInfo.InvariantInfo,
                               out ver) )
              continue;

          BDSVersion  v = GetKnownVersion( ver );
          if (v==null)
            v = new BDSVersion( ver );
          list.Add(v);

        } /*for*/

        installedVersions = (BDSVersion[])
                            list.ToArray(typeof(BDSVersion) );
      }

      private static BDSVersion GetKnownVersion(double bdsVer)
      {
        foreach (BDSVersion v in KnownVersions)
          if (v.Matches(bdsVer))
            return v;

        return null;
      }

      private static void CheckCurrentVersion()
      {
        if (currentVersion != null) return;
        if (BDSInterop.StandAlone) return;

        UnsafeCheckCurrentVersion();
      }

      private static void UnsafeCheckCurrentVersion()
      {
        string reg = BDSServices.Service.BaseRegistryKey;
        currentVersion = GetVersionForRegistryPath(reg);
      }

      private static BDSVersion GetVersionForRegistryPath(string path)
      {
        path = BDSRegistry.FixPath(path);

        if (installedVersions==null)
          foreach (BDSVersion v in KnownVersions)
            if (v.Matches(path))
              return v;

        CheckInstalledVersions();

        if (installedVersions!=null)
          foreach (BDSVersion v in installedVersions)
            if (v.Matches(path))
              return v;

        return null;
      }
      #endregion Private Methods

      #region Private Fields
      private static BDSVersion[] installedVersions = null;
      private static BDSVersion   currentVersion    = null;
      #endregion Private Fields

	}
}
