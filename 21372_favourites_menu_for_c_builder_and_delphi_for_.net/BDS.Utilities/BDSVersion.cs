using System;

namespace MarcRohloff.BDS.Utilities
{
    #region Enumerations
    public enum Product {
      Unknown = 0,
      CSB     = 1,
      Delphi  = 2
    }

    [Flags]
    public enum Language {
      Unknown  = 0,
      CSharp   = 1,
      Pascal   = 2,
      VB       = 4
    }

    [Flags]
    public enum Capability {
      Personal     =  1,
      Professional =  2,
      Architect    =  4,
      Enterprise   =  8,
      ECO          = 16
    }

    #endregion Enumerations


    //Detail on a single available BDS Version
	public sealed class BDSVersion
	{
      #region Public properties
      /// BDS Version (1.0=csb1, 2.0=D8)
      public double     Version        { get { return version; } }
      public Product    Product        { get { return product; } }
      public double     ProductVersion { get { return productVersion; } }
      public string     RegPath        { get { return regPath; } }
      public Language   Languages      { get { return languages; } }

      public string     Description
      { get {
        string fmt;
        switch (product) {
          case Product.Unknown : fmt = "BDS Version {1:f1}"; break;
          case Product.CSB     : fmt = "C# Builder {0:f1}"; break;
          case Product.Delphi  : fmt = "Delphi {0:f1}"; break;
          default              : fmt = "Undetected BDS {1:f1}"; break;
        };

        return String.Format(fmt, productVersion, version);
      } }

      public Capability Capabilities
        { get { CheckCapabilities(); return capabilities; } }
      #endregion Public Properties

      #region Public Methods
      public override string ToString()
        { return Description; }

      public override bool Equals(object o)
        {  return ((BDSVersion)o).version == this.version;     }

      public override int GetHashCode()
        {  return version.GetHashCode(); }

      public  bool Matches(double version)
      { return (System.Math.Abs(version-this.version) < 0.0001);    }

      public bool Matches(string regPath)
      { return (String.Compare(this.regPath, regPath, true)==0); }

      public bool Matches(Product product, double productVersion)
      {
        if (product != this.product) return false;
        if (System.Math.Abs(productVersion) < 0.0001) return true;

        return (System.Math.Abs(productVersion-this.productVersion) < 0.0001);
      }


      #endregion Public Methods

      #region Lifetime Methods

      /*Create a version with known information*/
      internal BDSVersion (double        version,
                           Product       product,
                           double        productVersion,
                           string        regPath,
                           Language      languages )
      {
        this.version        = version;
        this.product        = product;
        this.productVersion = productVersion;
        this.regPath        = BDSRegistry.FixPath(regPath);
        this.languages      = languages;

        if (this.regPath==null)
          this.regPath = BDSRegistry.BDSRootRegPath +
                         version.ToString("f1",
                                System.Globalization.NumberFormatInfo.InvariantInfo);
      }

      /*Create a version with known information*/
      internal BDSVersion (double version) :
         this(version,
              Product.Unknown,
              0,
              null,
              Language.Unknown
             )
      { }

      #endregion Lifetime Methods

      #region Private Methods
      //ToDo : I still have to figure out how to get these values
      private void CheckCapabilities()
      {
        if (capabilities != 0) return; //Already Checked
        capabilities = 0;
      }
      #endregion Private Methods

      #region Private Fields
      private Product    product;
      private double     productVersion;
      private double     version;
      private string     regPath;
      private Language   languages;
      private Capability capabilities;
      #endregion PRivate Fields
	}
}
