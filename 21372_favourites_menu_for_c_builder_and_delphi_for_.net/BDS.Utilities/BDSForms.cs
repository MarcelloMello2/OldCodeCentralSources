using System;
using System.Drawing;
using System.Windows.Forms;

namespace MarcRohloff.BDS.Utilities
{
	public class BDSForms
	{

        public static void FixIDEForm(Form f)
        {
          if (BDSInterop.StandAlone) return;

          Borland.Studio.ToolsAPI.IOTAService svc =
            (Borland.Studio.ToolsAPI.IOTAService)
               Borland.Studio.ToolsAPI.BorlandIDE.GetService(
                 typeof(Borland.Studio.ToolsAPI.IOTAService) );

          WindowsAPI.SetWindowLong(f.Handle ,WindowsAPI.GWL_HWNDPARENT, BDSServices.Service.ParentHandle );
        }

        public static string RectToString(Rectangle r)
        {
          return String.Format( System.Globalization.NumberFormatInfo.InvariantInfo,
                                "{0},{1},{2},{3}",
                                r.X, r.Y, r.Width, r.Height );
        }

        public static Rectangle StringToRect(string s, Rectangle current)
        {
          if (s==null) return current;
          Rectangle r = current;

          string[] ss = s.Split(',');
          try
          {
            if (ss.Length>0)
              r.X      = Int32.Parse(ss[0], System.Globalization.NumberFormatInfo.InvariantInfo);
            if (ss.Length>1)
              r.Y      = Int32.Parse(ss[1], System.Globalization.NumberFormatInfo.InvariantInfo);
            if (ss.Length>2)
              r.Width  = Int32.Parse(ss[2], System.Globalization.NumberFormatInfo.InvariantInfo);
            if (ss.Length>3)
              r.Height = Int32.Parse(ss[3], System.Globalization.NumberFormatInfo.InvariantInfo);

            r.Intersect( Screen.GetWorkingArea(r) );
            if (r.Width<=0)  r.Width  = current.Width;
            if (r.Height<=0) r.Height = current.Height;

          }
          catch (FormatException)
          {};

          return r;
        }


        #region private methods and fields
		private BDSForms() {} //static class
        #endregion private methods and fields

	}
}
