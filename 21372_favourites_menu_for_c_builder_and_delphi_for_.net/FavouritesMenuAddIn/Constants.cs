//Use this define to enable American english
//#define AMERICAN

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

namespace MarcRohloff.FavouritesMenuAddIn
{
	public class Constants : System.Windows.Forms.Control
	{
        #region String Resources
        #if AMERICAN
          internal const string sFavourite = "Favorite";
        #else
          internal const string sFavourite = "Favourite";
        #endif
        internal const string sFavourites = sFavourite + "s";
        internal const string sMenuCaption = "&" + sFavourites + " ...";

        internal const string sFavRegKey     = "FavouritesMenu";
        internal const string sSeperatorText = "- seperator - ";
        internal const string sAltSeperatorText = "-";

        internal const char   sRegSeperator = '/';

        internal const string sProjectExtensions = ".bdsproj.bdsgroup.dpr.dpk";
        internal const string DefaultProjectBitmapName  = "bdsproj";
        internal const string DefaultFileBitmapName     = "any";
        #endregion String Resources

        #region Other Resources
        static internal Bitmap GetBitmap(string name)
          { return resmgr.GetObject(name + ".Image") as Bitmap;  }

        static internal Bitmap MainBitmap
          { get {return GetBitmap("Main"); } }
        #endregion

        #region Designer Created Code
		private System.ComponentModel.Container components = null;
        private System.Windows.Forms.PictureBox any;
        private System.Windows.Forms.PictureBox txt;
        private System.Windows.Forms.PictureBox asmx;
        private System.Windows.Forms.PictureBox aspx;
        private System.Windows.Forms.PictureBox cs;
        private System.Windows.Forms.PictureBox bdsgroup;
        private System.Windows.Forms.PictureBox bdsproj;
        private System.Windows.Forms.PictureBox Main;
        private System.Windows.Forms.PictureBox AddProj;
        private System.Windows.Forms.PictureBox AddFile;

		private Constants()
		{
			InitializeComponent();
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing)
			{
				if (components != null)
					components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Component Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Constants));
            this.any = new System.Windows.Forms.PictureBox();
            this.txt = new System.Windows.Forms.PictureBox();
            this.asmx = new System.Windows.Forms.PictureBox();
            this.aspx = new System.Windows.Forms.PictureBox();
            this.cs = new System.Windows.Forms.PictureBox();
            this.bdsgroup = new System.Windows.Forms.PictureBox();
            this.bdsproj = new System.Windows.Forms.PictureBox();
            this.Main = new System.Windows.Forms.PictureBox();
            this.AddProj = new System.Windows.Forms.PictureBox();
            this.AddFile = new System.Windows.Forms.PictureBox();
            this.SuspendLayout();
            // 
            // any
            // 
            this.any.Image = ((System.Drawing.Image)(resources.GetObject("any.Image")));
            this.any.Location = new System.Drawing.Point(17, 17);
            this.any.Name = "any";
            this.any.Size = new System.Drawing.Size(16, 16);
            this.any.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.any.TabIndex = 21;
            this.any.TabStop = false;
            // 
            // txt
            // 
            this.txt.Image = ((System.Drawing.Image)(resources.GetObject("txt.Image")));
            this.txt.Location = new System.Drawing.Point(86, 17);
            this.txt.Name = "txt";
            this.txt.Size = new System.Drawing.Size(16, 16);
            this.txt.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.txt.TabIndex = 20;
            this.txt.TabStop = false;
            // 
            // asmx
            // 
            this.asmx.Image = ((System.Drawing.Image)(resources.GetObject("asmx.Image")));
            this.asmx.Location = new System.Drawing.Point(149, 17);
            this.asmx.Name = "asmx";
            this.asmx.Size = new System.Drawing.Size(16, 16);
            this.asmx.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.asmx.TabIndex = 19;
            this.asmx.TabStop = false;
            // 
            // aspx
            // 
            this.aspx.Image = ((System.Drawing.Image)(resources.GetObject("aspx.Image")));
            this.aspx.Location = new System.Drawing.Point(227, 17);
            this.aspx.Name = "aspx";
            this.aspx.Size = new System.Drawing.Size(16, 16);
            this.aspx.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.aspx.TabIndex = 18;
            this.aspx.TabStop = false;
            // 
            // cs
            // 
            this.cs.Image = ((System.Drawing.Image)(resources.GetObject("cs.Image")));
            this.cs.Location = new System.Drawing.Point(302, 17);
            this.cs.Name = "cs";
            this.cs.Size = new System.Drawing.Size(16, 16);
            this.cs.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.cs.TabIndex = 17;
            this.cs.TabStop = false;
            // 
            // bdsgroup
            // 
            this.bdsgroup.Image = ((System.Drawing.Image)(resources.GetObject("bdsgroup.Image")));
            this.bdsgroup.Location = new System.Drawing.Point(364, 17);
            this.bdsgroup.Name = "bdsgroup";
            this.bdsgroup.Size = new System.Drawing.Size(16, 16);
            this.bdsgroup.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.bdsgroup.TabIndex = 16;
            this.bdsgroup.TabStop = false;
            // 
            // bdsproj
            // 
            this.bdsproj.Image = ((System.Drawing.Image)(resources.GetObject("bdsproj.Image")));
            this.bdsproj.Location = new System.Drawing.Point(462, 17);
            this.bdsproj.Name = "bdsproj";
            this.bdsproj.Size = new System.Drawing.Size(16, 16);
            this.bdsproj.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.bdsproj.TabIndex = 15;
            this.bdsproj.TabStop = false;
            // 
            // Main
            // 
            this.Main.Image = ((System.Drawing.Image)(resources.GetObject("Main.Image")));
            this.Main.Location = new System.Drawing.Point(550, 17);
            this.Main.Name = "Main";
            this.Main.Size = new System.Drawing.Size(16, 16);
            this.Main.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.Main.TabIndex = 14;
            this.Main.TabStop = false;
            // 
            // AddProj
            // 
            this.AddProj.Image = ((System.Drawing.Image)(resources.GetObject("AddProj.Image")));
            this.AddProj.Location = new System.Drawing.Point(550, 17);
            this.AddProj.Name = "AddProj";
            this.AddProj.Size = new System.Drawing.Size(16, 16);
            this.AddProj.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.AddProj.TabIndex = 13;
            this.AddProj.TabStop = false;
            // 
            // AddFile
            // 
            this.AddFile.Image = ((System.Drawing.Image)(resources.GetObject("AddFile.Image")));
            this.AddFile.Location = new System.Drawing.Point(17, 54);
            this.AddFile.Name = "AddFile";
            this.AddFile.Size = new System.Drawing.Size(16, 16);
            this.AddFile.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.AddFile.TabIndex = 12;
            this.AddFile.TabStop = false;
            // 
            // Constants
            // 
            this.Controls.Add(this.any);
            this.Controls.Add(this.txt);
            this.Controls.Add(this.asmx);
            this.Controls.Add(this.aspx);
            this.Controls.Add(this.cs);
            this.Controls.Add(this.bdsgroup);
            this.Controls.Add(this.bdsproj);
            this.Controls.Add(this.Main);
            this.Controls.Add(this.AddProj);
            this.Controls.Add(this.AddFile);
            this.Name = "Constants";
            this.ResumeLayout(false);
        }
		#endregion

        #endregion Designer Created Code

        #region Private Methods and Fields
        private static System.Resources.ResourceManager 
           resmgr = new System.Resources.ResourceManager(typeof(Constants));
        #endregion Private MEthods and Fields
	}
}
