using System;
using Microsoft.Win32;
using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.BDS.AddInManager
{
	public class AddInStorage
	{

	   static public AddInCollection FetchAddIns()
	   {
         AddInCollection list = new AddInCollection();
         ReadAddInKey(list);
         ReadBDSKey(list);
         return list;
	   }


	   static public void StoreAddIns(AddInCollection list)
	   {
         foreach (AddIn d in list)
         {
           if (d.Changed)
           {
             WriteBDSKey(d);
             WriteAddInKey(d);
             d.ClearChanges();
           }
         }
	   }


     #region private fields and methods

	   private AddInStorage() {} /*static class*/

       static private void ReadAddInKey(AddInCollection list)
       {
		  RegistryKey expreg = BDSRegistry.OpenKey(null, AddInResources.KnownAddInsKey, false);
		  if (expreg==null) return;
          using (expreg)
          {
            RegistryKey bdsreg = BDSRegistry.OpenKey(null, BDSRegistry.KnownAssemKey, false);
            if (bdsreg==null) return;

            using (bdsreg)
            {

              foreach (string path in expreg.GetValueNames())
              {
                if (path=="") continue;

                string  val  = (string)expreg.GetValue(path);
                string  name = null;
                string  type = null;

                if (val!="")
                {
                  string[] s = val.Split( AddInResources.SeperatorChar );
                  if (s.Length>0) name = s[0];
                  if (s.Length>1) type = s[1];
                  name = name.Trim();
                  type = type.Trim();
                };

                AddIn d = new AddIn(name, path.Trim() );

                switch (type[0])
                {
                  case  'B' :  d.LoadType = LoadType.AutoBDS;      break;
                  case  'E' :  d.LoadType = LoadType.AutoExpert;   break;
                  case  'M' :  d.LoadType = LoadType.Manual;       break;
                  case  'R' :  d.LoadType = LoadType.Removed;      break;
                };

                if ( (d.LoadType==LoadType.AutoBDS) && (bdsreg.GetValue(path)==null) )
                   d.LoadType = LoadType.Manual;

                d.ClearChanges();
                list.Add(d);

              } /*foreach*/ ;

            } /* using bdsreg */
          } /*using expreg */
       } /* ReadAddInKey*/


       static private void ReadBDSKey(AddInCollection list)
       {
		  RegistryKey r = BDSRegistry.OpenKey(null, BDSRegistry.KnownAssemKey, false);
		  if (r==null) return;
          using (r)
          {

            foreach (string path in r.GetValueNames())
            {
              string  val  = (string)r.GetValue(path);
              AddIn a = list.Find(path);

              if (a==null)
              {
                if (String.Compare(val, "RunOnce", true)==0)
                {
                  a = new AddIn("", path);
                  a.LoadType = LoadType.RunOnce;
                }
                else
                {
                  a = new AddIn(val, path);
                  if (val=="")
                    a.LoadType = LoadType.Manual;
                }

                list.Add(a);
              }

              if ( (val!="") && (a.LoadType != LoadType.RunOnce) )
                a.LoadType = LoadType.AutoBDS;

              a.ClearChanges();
            } /*for*/ ;

          } /* using r */
       }

	   static private  void WriteBDSKey(AddIn data)
       {
         RegistryKey bdskey = BDSRegistry.OpenKey(null, BDSRegistry.KnownAssemKey, true);
         using (bdskey)
         {

           if (data.LoadType==LoadType.Removed)
           {
             if (bdskey.GetValue(data.Path)!=null)
               bdskey.DeleteValue(data.Path);
           }

           else
           {
             string bdsval = (string)bdskey.GetValue(data.Path, "");
             string reqval = (data.LoadType==LoadType.AutoBDS)
                           ? data.Name :
                             (data.LoadType==LoadType.RunOnce)
                           ? "RunOnce"  : "";
             if (bdsval!=reqval)
               bdskey.SetValue(data.Path, reqval);
           };

         } /*using bdskey*/

       }


	   static private  void WriteAddInKey(AddIn data)
       {
         RegistryKey expkey = BDSRegistry.OpenKey(null, AddInResources.KnownAddInsKey, true);
         using (expkey)
         {

           if (  (data.LoadType==LoadType.Removed)
               ||(data.LoadType==LoadType.RunOnce) )
           {
             if (expkey.GetValue(data.Path) != null )
               expkey.DeleteValue(data.Path);
           }

           else
           {
             string ext;
             switch (data.LoadType)
             {
               case LoadType.AutoBDS :
                    ext = "BDS";       break;
               case LoadType.AutoExpert :
                    ext = "Expert";    break;
               case LoadType.Manual :
                    ext = "Manual";    break;
               default :
                    ext = "Removed";   break;
             };

             expkey.SetValue(data.Path, data.Name + AddInResources.SeperatorChar + ext);
           }
         } /*using expkey*/
       }

     #endregion private fields and methods

   } /*AddInManager*/

} /*namespace*/
