using System;
using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.BDS.AddInManager
{

   //This is a proxy for real add-ins
   public class AddIn : IComparable
   {
      public AddIn(string name, string path)
      {
        path = BDSFiles.ContractEnvironmentStrings(path);
        this.path     = path;
        this.name     = name;
        this.loadType = DefaultLoadType;

        if (this.name=="")
        {
          this.name = System.IO.Path.GetFileNameWithoutExtension(path);
        }
      }

      public void MarkAsChanged() {changed=true;}
      public void ClearChanges()  {changed=false;}
      public void MarkAsNew()     {MarkAsChanged();}


	  private  bool     changed;
	  private  string   name;
	  private  string   path;
	  private  LoadType loadType;

      public bool     Changed { get {return changed;} }
      public string   Path    { get {return path;   } }
      public bool     Loaded  { get {return BDSFiles.IsAssemblyLoaded(path);  } }
      public bool     Exists  { get {return System.IO.File.Exists( BDSFiles.ExpandEnvironmentStrings(path) ); } }


      public string   Name
      {
        get {return name; }
        set
        {
          if (value!=this.name)
          {
            this.name=value;
            MarkAsChanged();
          };
        }
      }


      public LoadType LoadType
      {
        get {return loadType; }
        set
        {
          if (value!=this.loadType)
          {
            this.loadType=value;
            MarkAsChanged();
          };
        }
      }


      public void Load()
        { BDSFiles.LoadAddIn(path); }


      public int CompareTo(object o)
      {
        int res;
        AddIn other = (AddIn) o;

        res = loadType - other.loadType;
        if (res!=0) return res;

        return name.CompareTo( other.name );
      }

      #region private fields and methods
      private const LoadType   DefaultLoadType = LoadType.AutoExpert;
      #endregion private fields and methods

   }

}
