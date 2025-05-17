using System;
using System.Windows.Forms;
using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.BDS.AddInManager
{
	public class AddInInstaller
	{

       static public void CheckInstall(out bool canInstall, out bool canUninstall,
                                       Type implementor,
                                       AddInCollection addIns)
       {
         string path = implementor.Assembly.Location;
         path = BDSFiles.ContractEnvironmentStrings(path);

         foreach (AddIn a in addIns)
         {
           if (String.Compare(a.Path, path, true)==0)
           {
             canUninstall =  (a.LoadType != LoadType.Removed);
             canInstall   =  (a.LoadType != LoadType.AutoBDS)
                          && (a.LoadType != LoadType.AutoExpert);
             return;
           }
         }

         foreach (AddIn a in addIns)
         {
           if (String.Compare(a.Path, path, true)==0)
           {
             canUninstall = (a.LoadType != LoadType.Removed);
             canInstall   =  (a.LoadType != LoadType.AutoBDS)
                          && (a.LoadType != LoadType.AutoExpert);
             return;
           }
         }

         //No entry found
         canInstall   = true;
         canUninstall = false;
       }


       static public void AutoInstall(Type         implementor,
                                      string       addInName,
                                      LoadType     loadType)
       {
         AutoInstall(implementor, addInName, null, loadType,
                     BDSVersions.InstalledVersions);
       }

       static public void AutoInstall(Type         implementor,
                                      string       addInName,
                                      string[]     obsoleteNames,
                                      LoadType     loadType)
       {
         AutoInstall(implementor, addInName, obsoleteNames, loadType,
                     BDSVersions.InstalledVersions);
       }

       static public void AutoInstall(Type         implementor,
                                      string       addInName,
                                      string[]     obsoleteNames,
                                      LoadType     loadType,
                                      BDSVersion[] versions)
       {
         if ( (versions==null) || (versions.Length==0) )
         {
           MessageBox.Show("No BDS Versions detected",
                           "Installing " + addInName,
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Warning);
           return;
         }

         AddInInstallerForm f = new AddInInstallerForm();
         f.Initialize(implementor, addInName, obsoleteNames,
                      loadType, versions);
         f.ShowDialog();
       }


       /*Simple Install into CurrentVersion*/
       static public void SimpleInstall(Type     implementor,
                                      string   addInName,
                                      LoadType loadType)
       {
         AddInCollection list = AddInStorage.FetchAddIns();

         bool canInstall;
         bool canUninstall;

         CheckInstall(out canInstall, out canUninstall,
                     implementor, list);

         string auxText=null;
         if (canUninstall)
         {
           auxText = "Uninstall";
           Uninstall(implementor, list, null);
         }
         else if (canInstall)
         {
           auxText = "Install";
           Install(implementor, null, null,
                   loadType, list, null);
         }
         else
         {
           MessageBox.Show("No install action taken.",
                            "Install BDS Add-In",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Information);
           return;
         }

         DialogResult res;
         res = MessageBox.Show(auxText + " " + addInName + "?",
                                auxText + " BDS Add-In",
                                MessageBoxButtons.YesNo,
                                MessageBoxIcon.Question,
                                MessageBoxDefaultButton.Button1 );

         if (res==DialogResult.Yes)
         {
           AddInStorage.StoreAddIns(list);
           MessageBox.Show(auxText + " completed successfully.",
                            auxText + " BDS Add-In",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Information);

         }

       } /*AutoInstall*/

       
       static public void Install(Type implementor,
                                  string    addInName, /* can be null*/
                                  string[]  obsoleteNames,
                                  LoadType  loadType,
                                  AddInCollection addIns,
                                  AddInEvent notification )
       {
         bool      found = false;

         string    path  = implementor.Assembly.Location;
         path = BDSFiles.ContractEnvironmentStrings(path);

         addInName = GetAddInName(implementor, addInName);

         foreach (AddIn a in addIns)
         {
           if (String.Compare(a.Path, path, true)==0)
           {
             a.LoadType = loadType;
             a.Name     = addInName;
             if (notification!=null) notification(a);
             found = true;
           }
           //Uninstall if same version and different path
           else if (String.Compare(a.Name, addInName, true)==0)
           {
             a.LoadType = LoadType.Removed;
             if (notification!=null) notification(a);
           }
           //Check for previous versions
           else if (IsInStrings(a.Name, obsoleteNames))
           {
             a.LoadType = LoadType.Removed;
             if (notification!=null) notification(a);
           };

         } /*foreach*/

         if (!found)
         {
           AddIn a = new AddIn(addInName, path);
           addIns.Add(a);
           a.LoadType   = loadType;
           a.MarkAsNew();
           if (notification!=null) notification(a);
         }

       }


       static public void Uninstall( Type implementor,
                                     AddInCollection addIns,
                                     AddInEvent notification)
       {
         string path = implementor.Assembly.Location;
         path = BDSFiles.ContractEnvironmentStrings(path);

         foreach (AddIn d in addIns)
           if (String.Compare(d.Path, path, true)==0)
           {
             d.LoadType = LoadType.Removed;
             if (notification != null)
               notification(d);
           }
       }



      #region Private Methods and Fields
 	   private AddInInstaller() {} /*static class*/

       static private string GetAddInName(Type implementor, string proposed)
       {
         if ( (proposed==null) || (proposed=="") )
            proposed = implementor.Namespace;

         return proposed;
       }


       static private bool IsInStrings(string s, string[] list)
       {
         if (list!=null)
           foreach (string ss in list)
             if (String.Compare(s, ss, true)==0)
               return true;

         return false;
       }

      #endregion Private Methods and Fields
    }
}
