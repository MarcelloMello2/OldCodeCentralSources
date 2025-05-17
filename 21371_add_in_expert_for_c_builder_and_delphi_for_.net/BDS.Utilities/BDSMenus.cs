using System;
using Borland.Studio.ToolsAPI;

//These routines are unsafe in StandAlone mode

namespace MarcRohloff.BDS.Utilities
{

	public class BDSMenus
	{
        #region nested types
        public enum Position
        {
          Before = OTAMenuItemLocation.otamlBefore,
          After  = OTAMenuItemLocation.otamlAfter,
          Child  = OTAMenuItemLocation.otamlChild
        }
        #endregion nested types

        #region menu identifiers
        /*File Menu identifiers*/
        public const string MenuFile               = "FileMenu";
		public const string MenuFileOpenProject    = "FileOpenProjectItem";
		/*Component Menu identifiers */
		public const string MenuComponent          = "ComponentMenu";
		public const string MenuComponentInstall   = "mnuInstalledComponents";
		/*Tools Menu identifiers */
		public const string MenuTools              = "ToolsMenu";
		#endregion menu identifiers

        public static int KeyToShortCut(System.Windows.Forms.Keys k)
        {
          int mod = (int)(k & System.Windows.Forms.Keys.Modifiers);
          int key = (int)(k & System.Windows.Forms.Keys.KeyCode);
          return key  | ( mod >> 3 );
       }

        public static IOTAMenuItem GetMenuItem(string name)
          { return Service.GetMenuItem(name); }

		public static IOTAMenuItem AddMenuSeperator(string relativeTo,
                                                    Position locn)
        {
          return AddMenuItem(relativeTo, locn, "", "-", null, null, 0);
        }

		public static IOTAMenuItem AddMenuItem(string relativeTo,
                                               Position locn,
                                               string menuName,
                                               string menuCaption,
                                               EventHandler action,
                                               System.Drawing.Bitmap     bitmap,
                                               System.Windows.Forms.Keys shortCut)
		{
          IntPtr hBmp = IntPtr.Zero;
          if (bitmap != null)
          {
            bitmap.MakeTransparent();
            hBmp = bitmap.GetHbitmap();
          };

          IOTAMenuItem mni;

		  mni = Service.AddMenuItem (relativeTo,
                                     (OTAMenuItemLocation)locn,
                                     menuName, menuCaption,
                                     hBmp);

          IOTAMenuItem parent = mni.ParentMenuItem;
          if (parent != null)
            mni.Category = parent.Category;
          mni.Executed += action;

          mni.Shortcut = KeyToShortCut(shortCut);

          AddMenuShortcut(mni);

          return mni;
		}

		public static IOTAMenuItem AddMenuItem(IOTAMenuItem relativeTo,
                                               Position locn,
                                               string menuName,
                                               string menuCaption,
                                               EventHandler action,
                                               System.Drawing.Bitmap     bitmap,
                                               System.Windows.Forms.Keys shortCut)
		{
          return AddMenuItem(relativeTo.Name, locn, menuName, menuCaption, action, bitmap, shortCut);
		}

        public static void RemoveMenuItem(IOTAMenuItem item)
        {
          Service.RemoveMenuItem(item.Name);
          RemoveMenuShortcut(item);
        }

        public static void RemoveMenuItem(string name)
        {
          IOTAMenuItem mni = Service.GetMenuItem(name);
          if (mni != null)
            RemoveMenuItem(mni);
        }

        public static IOTAMainMenuService Service
           { get { return BDSServices.MenuService; } }


        #region private methods and fields

        private static void AddMenuShortcut(IOTAMenuItem item)
		{
          if (item.Shortcut==0) return;

          if (menuItems==null)
          {
            menuItems = new System.Collections.Specialized.HybridDictionary();
            BDSServices.Service.FileNotification += new FileNotificationHandler(UpdateMenuShortcuts);
          }

          menuItems.Add(item, item.Shortcut);
		}

        private static void RemoveMenuShortcut(IOTAMenuItem item)
        {
          if (menuItems!=null)
            menuItems.Remove(item);
        }

		private static void UpdateMenuShortcuts(object sender, FileNotificationEventArgs args)
		{
            if (menuItems==null) return;

			if ((args.NotifyCode == OTAFileNotification.ofnPackageInstalled) ||
				(args.NotifyCode == OTAFileNotification.ofnPackageUninstalled)  ||
				(args.NotifyCode == OTAFileNotification.ofnFileOpened))
			{
				foreach (IOTAMenuItem i in menuItems.Keys)
                {
                  if (i!=null)
                    i.Shortcut = (int)menuItems[i];
                }
			}
		}


        //installed menu items
        private static System.Collections.Specialized.HybridDictionary menuItems;

        #endregion private methods and fields

	}
}
