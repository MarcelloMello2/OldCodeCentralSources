using System;
using System.Windows.Forms;
using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.FavouritesMenuAddIn
{
    public enum DisplayMode
    {
      Fullname,
      FilenameOnly,
      Relative
    }

	public class Favourite
	{
        #region Internal Methods and Properties
        internal Favourite(string filename)
          {
            if  (IsSeperatorName(filename))
            {
              this.filename = Constants.sSeperatorText;
              exists = true;
            }
            else
            {
              this.filename = FavUtilities.GetLongFilename(filename, out exists);
            }
          }

        /*made public for datagrid*/
        public   string Filename { get { return filename; } }

        /*made public for datagrid*/
        public   Keys   Shortcut { get { return shortcut; } set {shortcut=value;} }

        internal bool   IsSeperator { get { return IsSeperatorName(filename); } }

        internal bool   Exists { get { return exists; } }

        internal string GetMenuText(Favourites f, string relativePathRoot)
        {
          string res;
          if (IsSeperator)
            res = "-";
          else if (f.DisplayMode==DisplayMode.FilenameOnly)
            res = System.IO.Path.GetFileName(filename);
          else if (f.DisplayMode==DisplayMode.Relative)
            res = GetRelativePath(relativePathRoot);
          else
            res = filename;

          if (!f.ShowExtension)
          {
            res = System.IO.Path.ChangeExtension(res, "");
            if (res.Length>0)
              res = res.Substring(0, res.Length-1);
          }

          return res;
        }

        internal void   UpdateExists()
        {
          if (!IsSeperator)
            exists = System.IO.File.Exists(filename);
        }

        internal void   Execute(object o, EventArgs e)
        {
           try
           {
             if (System.IO.File.Exists(filename))
             {
               if (IsProject)
                 BDSServices.ActionService.OpenProject(filename, true);
               else
                 BDSServices.ActionService.OpenFile(filename);
             }
             else
             {
               System.Windows.Forms.MessageBox.Show("File not found");
             }
           }
           catch(Exception ex)
           {
             BDSInterop.HandleException(ex);
           }
        }

        internal bool Matches(string filename)
        {
          return (String.Compare(filename, this.filename, true)==0);
        }

        internal bool IsProject { get
        {
           string ext = System.IO.Path.GetExtension(filename).ToLower();
           return Constants.sProjectExtensions.IndexOf(ext)>=0;
        } }

        internal System.Drawing.Bitmap GetBitmap()
        {
          string ext = System.IO.Path.GetExtension(filename).ToLower();
          if (ext.Length>0)
            ext = ext.Remove(0,1); //Remove '.'

          System.Drawing.Bitmap b = Constants.GetBitmap(ext);

          if (b==null)
          {
            b = Constants.GetBitmap(
                      IsProject ? Constants.DefaultProjectBitmapName
                                : Constants.DefaultFileBitmapName);
                                  }

          return b;
        }

        internal static bool IsSeperatorName(string text)
          { return ( String.Compare(text, Constants.sSeperatorText, true)==0)
                || ( String.Compare(text, Constants.sAltSeperatorText, true)==0) ; }

        #endregion Internal Methods and Properties

        #region Private Methods
        private string GetRelativePath(string root)
        {
          string res = filename;
          if (  (res.Length >= root.Length)
             && (String.Compare(res,0, root,0, root.Length, true)==0) )
             res = res.Remove(0, root.Length);

          return res;
        }
        #endregion Private MEthods

        #region Private Fields
        private string       filename;
        private Keys         shortcut;
        private bool         exists;
        #endregion private fields


    }  /* class Favourite */

} /* namespace */
