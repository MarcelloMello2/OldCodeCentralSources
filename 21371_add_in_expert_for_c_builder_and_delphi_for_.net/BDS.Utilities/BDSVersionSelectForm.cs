using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace MarcRohloff.BDS.Utilities
{
	public class BDSVersionSelectForm : System.Windows.Forms.Form
	{
		private System.ComponentModel.Container components = null;
        protected System.Windows.Forms.ListView listView;
        protected System.Windows.Forms.ColumnHeader colVersion;
        protected System.Windows.Forms.Panel pnlBottom;
        protected System.Windows.Forms.Button btnOK;
        protected System.Windows.Forms.Button btnCancel;

        public static BDSVersion SelectVersion()
          { return SelectVersion(BDSVersions.InstalledVersions, BDSVersions.CurrentVersion); }

        public static BDSVersion SelectVersion(BDSVersion[] versions,
                                               BDSVersion   defaultVersion)
        {
          if (versions.Length==0)
            throw new BDSException("No BDS Versions are installed");

          if (versions.Length==1) return versions[0];

          BDSVersionSelectForm f = new BDSVersionSelectForm();
          f.LoadVersions(versions, defaultVersion);
          if (f.ShowDialog()==DialogResult.OK)
            return (BDSVersion)f.listView.SelectedItems[0].Tag;
          else
            return null;
        }

		public BDSVersionSelectForm()
		{
		  InitializeComponent();
		}

        private ListViewItem GetNewItem()
        {
          ListViewItem item = listView.Items.Add("");
          for (int i = 1; i<listView.Columns.Count; i++)
            item.SubItems.Add("");
          return item;
        }

        protected virtual void UpdateItem(ListViewItem item, BDSVersion ver)
        {
          item.SubItems[0].Text = ver.Description;
        }

        protected void LoadVersions(BDSVersion[] versions, BDSVersion defaultVersion)
        {
          listView.Items.Clear();
          foreach(BDSVersion v in versions)
          {
            ListViewItem i = GetNewItem();
            i.Tag = v;
            UpdateItem(i, v);
            if ( (v==defaultVersion) || (i.Index==0) )
              i.Selected = true;
          }

          btnOK.Enabled =  (listView.SelectedItems.Count>0); 
        }

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose (bool disposing)
		{
			if (disposing)
			{
				if (components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
        {
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(BDSVersionSelectForm));
            this.listView = new System.Windows.Forms.ListView();
            this.colVersion = new System.Windows.Forms.ColumnHeader();
            this.pnlBottom = new System.Windows.Forms.Panel();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOK = new System.Windows.Forms.Button();
            this.pnlBottom.SuspendLayout();
            this.SuspendLayout();
            // 
            // listView
            // 
            this.listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                        this.colVersion});
            this.listView.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listView.FullRowSelect = true;
            this.listView.HeaderStyle = System.Windows.Forms.ColumnHeaderStyle.Nonclickable;
            this.listView.HideSelection = false;
            this.listView.LabelWrap = false;
            this.listView.Location = new System.Drawing.Point(0, 0);
            this.listView.MultiSelect = false;
            this.listView.Name = "listView";
            this.listView.Size = new System.Drawing.Size(360, 117);
            this.listView.TabIndex = 1;
            this.listView.View = System.Windows.Forms.View.Details;
            this.listView.DoubleClick += new System.EventHandler(this.listView_DoubleClick);
            this.listView.Layout += new System.Windows.Forms.LayoutEventHandler(this.listView_Layout);
            this.listView.SelectedIndexChanged += new System.EventHandler(this.listView_SelectedIndexChanged);
            // 
            // colVersion
            // 
            this.colVersion.Text = "Version";
            this.colVersion.Width = 100;
            // 
            // pnlBottom
            // 
            this.pnlBottom.Controls.Add(this.btnCancel);
            this.pnlBottom.Controls.Add(this.btnOK);
            this.pnlBottom.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.pnlBottom.Location = new System.Drawing.Point(0, 85);
            this.pnlBottom.Name = "pnlBottom";
            this.pnlBottom.Size = new System.Drawing.Size(360, 32);
            this.pnlBottom.TabIndex = 2;
            // 
            // btnCancel
            // 
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.CausesValidation = false;
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(280, 6);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.TabIndex = 1;
            this.btnCancel.Text = "   &Cancel";
            // 
            // btnOK
            // 
            this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnOK.Image = ((System.Drawing.Image)(resources.GetObject("btnOK.Image")));
            this.btnOK.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnOK.Location = new System.Drawing.Point(200, 6);
            this.btnOK.Name = "btnOK";
            this.btnOK.TabIndex = 0;
            this.btnOK.Text = "&OK";
            // 
            // BDSVersionSelectForm
            // 
            this.AcceptButton = this.btnOK;
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(360, 117);
            this.Controls.Add(this.pnlBottom);
            this.Controls.Add(this.listView);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "BDSVersionSelectForm";
            this.ShowInTaskbar = false;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Select BDS Version ...";
            this.Load += new System.EventHandler(this.BDSVersionSelectForm_Load);
            this.pnlBottom.ResumeLayout(false);
            this.ResumeLayout(false);
        }
		#endregion

        private void listView_Layout(object sender, System.Windows.Forms.LayoutEventArgs e)
        {
          int w = 0;
          for (int i=1; i<listView.Columns.Count; i++)
            w += listView.Columns[i].Width;

          listView.Columns[0].Width = listView.ClientSize.Width - w;
        }

        private void BDSVersionSelectForm_Load(object sender, System.EventArgs e)
        {
          ((Bitmap)btnOK.Image).MakeTransparent();
          ((Bitmap)btnCancel.Image).MakeTransparent();
          listView_Layout(null, null);
          this.ActiveControl = listView;
        }

        private void listView_SelectedIndexChanged(object sender, System.EventArgs e)
        {
          btnOK.Enabled = (listView.SelectedItems.Count>0);
        }

        private void listView_DoubleClick(object sender, System.EventArgs e)
        {
            if (btnOK.Enabled)
              btnOK.PerformClick();
        }
	}
}
