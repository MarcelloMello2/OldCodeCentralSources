using System;
using System.Drawing;

namespace MarcRohloff.BDS.Utilities
{
	public class BDSGraphics
	{

      public static Bitmap ScaleBitmap(Bitmap src, int scale)
      {
        if (src==null)
          return null;

        Bitmap b = new Bitmap(scale*src.Width, scale*src.Height);
        using (Graphics g = Graphics.FromImage(b))
          g.DrawImage(src, 0, 0, b.Width, b.Height);

        return b;
      }

      public static Bitmap MakeBitmapSize(Bitmap src, int width)
      {
        if (src==null) return null;

        int scale;
        if (width > src.Width)
          scale = (int)(width/src.Width);
        else
          scale = 1;

        return ScaleBitmap(src, scale);
      }

      public static Bitmap CreateOverlay(Bitmap src, Bitmap overlay)
      {
        if ( (src==null) && (overlay==null) )
          return null;

        if (src==null)
        {
          Bitmap o = new Bitmap(overlay);
          o.MakeTransparent();
          return o;
        }

        Bitmap b = new Bitmap(src);
        if (overlay!=null)
        {
          using( Bitmap o = new Bitmap(overlay))
          using( Graphics g = Graphics.FromImage(b))
          {
            o.MakeTransparent();
            g.DrawImage(o,0, 0, b.Width, b.Height);
          }
        }
        
        return b;
      }

      #region Private fields and methods
	  private BDSGraphics() {} /*static class*/
      #endregion Private fields and methods
	}
}
