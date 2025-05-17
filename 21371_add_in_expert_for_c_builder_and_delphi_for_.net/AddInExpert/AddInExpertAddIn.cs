using System;
using System.Windows.Forms;

namespace MarcRohloff.AddInExpert
{
	public class AddInExpertAddIn:BDS.Utilities.BDSAddInBase
	{
		public AddInExpertAddIn() {}


        #region Overridden inherited methods
        protected override string Name
          { get { return "Add-In Expert"; } }

        protected override string Description
          { get { return "A master expert for managing Add-ins to the BDS.\r\n(c)2004 Marc Rohloff"; } }

        protected override System.Drawing.Bitmap Bitmap
          { get { return frmAddInExpert.GetMenuBitmap(); } }

        protected override void RunStandalone()
        {
          base.RunStandalone();

          /*Select Version
          BDSUtilities.BDSVersion v = BDSUtilities.BDSVersionSelectForm.SelectVersion();
          if (v==null) return;
          BDSUtilities.BDSVersions.CurrentVersion = v; */

          (new frmAddInExpert()).ShowDialog();
        }

        protected override void Register()
        {
          base.Register();

          AddMenuItem(BDS.Utilities.BDSMenus.MenuComponent,
                      BDS.Utilities.BDSMenus.Position.Child,
                      "MarcRohloff_AddInsExpert",
                      "&Add-Ins Expert",
                      new EventHandler(ExpertMenuClick),
                      frmAddInExpert.GetMenuBitmap(),
                      Keys.Control | Keys.Alt | Keys.W );

          LoadAutoExpertAddIns();
        }

        #endregion Overridden inherited methods

        #region Private Methods
        static private void LoadAutoExpertAddIns()
        {
          BDS.AddInManager.AddInCollection list = BDS.AddInManager.AddInStorage.FetchAddIns();
          foreach (BDS.AddInManager.AddIn a in list)
          {
            if (a.LoadType==BDS.AddInManager.LoadType.AutoExpert)
            {
              try
              {
                a.Load();
              }
              catch (Exception e)
              {
                BDS.Utilities.BDSInterop.HandleException(e);
              }
            }
          }
        }

        private static void ExpertMenuClick(object o, EventArgs e)
        {
          frmAddInExpert.ShowExpert();
        }

        #endregion Private Methods
	}
}
