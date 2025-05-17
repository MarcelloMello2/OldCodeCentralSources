using System;
using System.Collections;
using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.BDS.AddInManager
{

   public class AddInCollection
   {

     public IEnumerator GetEnumerator() { return list.GetEnumerator(); }

     public AddIn Find(string path)
     {
       path = BDSFiles.ContractEnvironmentStrings(path);

       foreach (AddIn a in list)
       {
         if (string.Compare(a.Path, path, true)==0)
           return a;
       }

       return null;
     }


     public void Add(AddIn a) {list.Add(a);}
     public int  Count { get {return list.Count; } }

     public AddIn this[int i] { get { return (AddIn)list[i]; } }

     #region private fields and methods

     private ArrayList list = new ArrayList();

     #endregion private fields and methods

   }

}
