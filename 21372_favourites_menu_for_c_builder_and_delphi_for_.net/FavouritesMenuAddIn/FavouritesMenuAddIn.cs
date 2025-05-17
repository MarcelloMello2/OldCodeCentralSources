using System;
using Borland.Studio.ToolsAPI;
using MarcRohloff.BDS.Utilities;


namespace MarcRohloff.FavouritesMenuAddIn
{
	internal class FavouritesMenuAddIn : BDSAddInBase
	{
	  internal FavouritesMenuAddIn() {}

      #region Overridden inherited methods
      protected override string Name
        { get { return Constants.sFavourites + " Menu"; } }

      protected override string Description
        { get { return "Adds a sub-menu to store your " + Constants.sFavourite + " projects and files\r\n(c)2004 Marc Rohloff"; } }

      protected override System.Drawing.Bitmap Bitmap
        { get { return Constants.MainBitmap; } }

      protected override void RunStandalone()
      {
         BDS.AddInManager.AddInInstaller.AutoInstall( GetType(),
                                                     Name,
                                                     BDS.AddInManager.LoadType.AutoBDS);
      }

      protected override void Register()
      {
        base.Register();
        BuildMenu();
        favourites.Load();
        RefreshMenu();
      }

      #endregion Overridden inherited methods

      #region private methods
      private void BuildMenu()
      {
        favMenu = AddMenuItem(BDSMenus.MenuFileOpenProject,
                              BDSMenus.Position.After,
                              IDString,
                              Constants.sMenuCaption,
                              new EventHandler(UpdateMenu),
                              Constants.MainBitmap,
                              0 );
        favMenu.Category = "File";

        AddMenuItem(favMenu.Name, BDSMenus.Position.Child,
                    "FavMenu_Config", "Edit " + Constants.sFavourites + " ...",
                    new EventHandler(ConfigureFavourites),
                    Constants.MainBitmap,  0);

        favMenuAddProj = AddMenuItem(favMenu.Name, BDSMenus.Position.Child,
                                     "FavMenu_AddProj", "FavMenu_AddProj",
                                     new EventHandler(AddProjectClick),
                                     Constants.GetBitmap("AddProj"), 0);

        favMenuAddFile = AddMenuItem(favMenu.Name, BDSMenus.Position.Child,
                                     "FavMenu_AddFile", "FavMenu_AddFile",
                                     new EventHandler(AddFileClick),
                                     Constants.GetBitmap("AddFile"), 0);

        favMenuSeperator = AddMenuSeperator(favMenu.Name, BDSMenus.Position.Child);
      }

      private void UpdateMenuAddTexts()
      {
        string fn;
        IOTAModule module;

        module  = BDSModules.CurrentModule;
        fn = (module != null ? module.FileName : "");
        favMenuAddFile.Text = "Add file " + System.IO.Path.GetFileName(fn);
        favMenuAddFile.Enabled = favourites.CanAddFilename(fn);

        bool isGroup;
        module = BDSModules.GetTopLevelModule(out isGroup);
        if (isGroup)
          favMenuAddProj.Text = "Add group ";
        else
          favMenuAddProj.Text = "Add project ";

        fn = (module != null ? module.FileName : "");
        favMenuAddProj.Text    += System.IO.Path.GetFileName(fn);
        favMenuAddProj.Enabled =  favourites.CanAddFilename(fn);
      }

      private void UpdateMenuRelativePaths()
      /*Update menu entries when there are relative paths */
      {
        if (favourites.DisplayMode!=DisplayMode.Relative) return;
        
        bool isGroup;
        Borland.Studio.ToolsAPI.IOTAModule m = BDS.Utilities.BDSModules.GetTopLevelModule(out isGroup);
        string root = ( (m!=null)
                      ? (new System.IO.FileInfo(m.FileName)).DirectoryName+System.IO.Path.DirectorySeparatorChar
                      : "");

        for(int i=0; i<favourites.Count; i++)
        {
          IOTAMenuItem mni = favMenu.ChildMenuItem(i);
          mni.Text = favourites[i].GetMenuText(favourites, root);
        }
      }

      private void UpdateMenu(object o, EventArgs e)
      {
        try
        {
          UpdateMenuRelativePaths();
          UpdateMenuAddTexts();
        }
        catch(Exception ex)
        { BDSInterop.HandleException(ex); }

      }

      private void AddModule(IOTAModule module)
      {
        try
        {
          if (module==null) return;
          string fn = module.FileName;
          if (fn=="") return;

          Favourite fav = favourites.Add(fn);

          if (fav != null)
            AddMenuItem(fav);

        }
        catch (Exception ex)
        {
          BDSInterop.HandleException(ex);
        }
      }

      private void AddProjectClick(object o, EventArgs e)
      {
        bool       isGroup;
        IOTAModule module = BDSModules.GetTopLevelModule(out isGroup);
        AddModule(module);
      }

      private void AddFileClick(object o, EventArgs e)
        { AddModule(BDSModules.CurrentModule); }


      private void ClearMenu()
      {
        while (favMenu.ChildCount>0)
        {
          IOTAMenuItem m = favMenu.ChildMenuItem(0);
          if (m.Name==favMenuSeperator.Name) break;
          RemoveMenuItem(m);
        }
      }

      private void AddMenuItem(Favourite f)
      {
        int    cnt = favMenu.ChildCount;

        AddMenuItem(favMenuSeperator.Name,
                    BDSMenus.Position.Before,
                    "",
                    f.GetMenuText(favourites, ""),
                    new EventHandler( f.Execute ),
                    f.GetBitmap(),
                    f.Shortcut );
      }

      private void RefreshMenu()
      {
        ClearMenu();

        foreach (Favourite f in favourites)
          AddMenuItem(f);
      }

      private void ConfigureFavourites(object o, EventArgs e)
      {
        try
        {
          frmConfigureFavourites f = new frmConfigureFavourites();
          f.Read(favourites);
          if (f.ShowDialog() == System.Windows.Forms.DialogResult.OK)
          {
            f.Write(favourites);
            RefreshMenu();
            favourites.Save();
          }


        } catch (Exception ex) {
          BDSInterop.HandleException(ex);
        }

      }
      #endregion private methods

      #region private fields
      private IOTAMenuItem favMenu;
      private IOTAMenuItem favMenuAddFile;
      private IOTAMenuItem favMenuAddProj;
      private IOTAMenuItem favMenuSeperator;

      private Favourites favourites = new Favourites();
      #endregion private fields
	}
}
