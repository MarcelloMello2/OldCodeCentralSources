using System;
using System.Collections;
using Microsoft.Win32;

namespace MarcRohloff.FavouritesMenuAddIn
{
	internal class Favourites
	{

	  internal Favourites() {}

      public IEnumerator GetEnumerator()
        { return list.GetEnumerator(); }

      internal DisplayMode DisplayMode   { get {return displayMode;} set {displayMode=value;} }
      internal bool        ShowExtension { get {return showExtension;} set {showExtension=value;} }

      internal int Count { get { return list.Count; } }

      internal Favourite this[int i] { get { return (Favourite)list[i]; } }

      internal Favourite ForFilename(string filename)
      {
        foreach (Favourite f in list)
          if (f.Matches(filename))
            return f;

        return null;
      }

      internal void Clear()
        { list.Clear(); }

      internal bool CanAddFilename(string filename)
      {
        if (Favourite.IsSeperatorName(filename))
          return true;
        else
          return (ForFilename(filename) == null)
              && (System.IO.File.Exists(filename) );
      }

      internal void Add(Favourite f)
        { list.Add(f); }

      internal Favourite Add(string filename)
      {
        if (!CanAddFilename(filename))
          return null;

        Favourite f = InternalAdd(filename);
        Save();
        return f;
      }

      internal void Save()
      {
        using (RegistryKey r = FavUtilities.Registry())
        {
          System.ComponentModel.TypeConverter t =
              System.ComponentModel.TypeDescriptor.GetConverter(typeof(System.Windows.Forms.Keys));

          r.SetValue("DisplayMode", (int)displayMode);
          r.SetValue("ShowExtension", showExtension);

          r.SetValue("count", list.Count);
          for (int i = 0; i<list.Count; i++)
          {
            Favourite f = (Favourite)list[i];
            string    s = f.Filename;
            if ( (!f.IsSeperator) && (f.Shortcut!= System.Windows.Forms.Keys.None) )
              s += Constants.sRegSeperator
                 + t.ConvertToString(f.Shortcut);
            r.SetValue(i.ToString(), s);
          }
        } /* using r*/
      }

      internal void Load()
      {
        list.Clear();

        try
        {
          using (RegistryKey r = FavUtilities.Registry())
          {
            if (r==null) return;
            System.ComponentModel.TypeConverter t =
              System.ComponentModel.TypeDescriptor.GetConverter(typeof(System.Windows.Forms.Keys));

            int count = (int)r.GetValue("count", 0);
            displayMode = (DisplayMode)r.GetValue("DisplayMode", (int)displayMode);
            showExtension = bool.Parse( (string) r.GetValue("ShowExtension", showExtension));

            for (int i = 0; i < count; i++)
            {
              LoadFavourite(t, (string)r.GetValue(i.ToString(), "") );
            }
          } /*using r*/
        }
        catch
        { /*Ignore*/ }
      }

      internal void LoadFavourite(System.ComponentModel.TypeConverter t, string val)
      {
        if ( (val==null) || (val=="")) return;

        string[] fn  = val.Split(new char[] {Constants.sRegSeperator}, 2);
        Favourite f= InternalAdd( fn[0] );
        if (fn.Length>1)
        {
          try {
            System.Windows.Forms.Keys k = (System.Windows.Forms.Keys)
                                          t.ConvertFromString(fn[1]);
            f.Shortcut = k;
          }
          catch (ArgumentException)   { /*Ignore*/ }
        } /*if*/  
      }

      #region Private Fields and Methods

      private Favourite InternalAdd(string filename)
      {
        Favourite f = new Favourite(filename);
        list.Add(f);
        return f;
      }

      private DisplayMode displayMode   = DisplayMode.Fullname;
      private bool        showExtension = true;

      private ArrayList list = new ArrayList();

      #endregion private fields and methods

	} /* class Favourites */

} /* namespace */
