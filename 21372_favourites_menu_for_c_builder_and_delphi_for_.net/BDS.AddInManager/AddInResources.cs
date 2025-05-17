using System;

namespace MarcRohloff.BDS.AddInManager
{
   #region Types
   public class AddInException:Exception
   {
     public AddInException(string message) : base(message) {}
     public AddInException(string message, Exception inner) : base(message, inner) {}
   }

   public delegate void AddInEvent(AddIn data);

   public enum LoadType
   {
     AutoBDS    ,
     AutoExpert ,
     Manual     ,
     RunOnce    ,
     Removed
   };
   #endregion Types

   public class AddInResources
   {
     #region Constants
     public const string KnownAddInsKey  = @"Known AddIn Assemblies";

     public const char   SeperatorChar = '|';
     #endregion Constants


     #region Private Methods and Fields
     private AddInResources() {} /*static class*/
     #endregion Private Methods and Fields

   }

}
