using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

using MarcRohloff.BDS.Utilities;

namespace MarcRohloff.BDS.AddInManager
{
	internal class AddInInstallerForm : Utilities.BDSVersionSelectForm
	{
        private const int CanInstallFlag   = 1;
        private const int CanUninstallFlag = 2;

        private Type     implementor   = null;
        private string   addInName     = "";
        private string[] obsoleteNames = null;
        private LoadType loadType      = LoadType.AutoBDS;


        internal void Initialize(Type         implementor,
                                 string       addInName,
                                 string[]     obsoleteNames,
                                 LoadType     loadType,
                                 BDSVersion[] versions)
        {
          this.implementor   = implementor;
          this.addInName     = addInName;
          this.obsoleteNames = obsoleteNames;
          this.loadType      = loadType;

          this.Text = "Install " + addInName;
          LoadVersions(versions, null);
          BDSVersions.CurrentVersion = null;

          foreach(ListViewItem i in listView.Items)
            i.Selected = true;
        }

        protected override void UpdateItem(ListViewItem item, BDSVersion version)
        {
          base.UpdateItem(item, version);

          bool canInstall;
          bool canUninstall;

          BDSVersions.CurrentVersion = version;
          AddInCollection addIns = AddInStorage.FetchAddIns();
          AddInInstaller.CheckInstall(out canInstall, out canUninstall,
                                      implementor, addIns);

          item.ImageIndex = 0;
          if (canInstall)   item.ImageIndex |= CanInstallFlag;
          if (canUninstall) item.ImageIndex |= CanUninstallFlag;

          switch (item.ImageIndex)
          {
            case 0 : item.SubItems[1].Text = "Error reading status"; break;
            case 1 : item.SubItems[1].Text = "Not Installed";        break;
            case 2 : item.SubItems[1].Text = "Installed";            break;
            case 3 : item.SubItems[1].Text = "Manual Installation";  break;
          }
        }

        private void InstallVersion(BDSVersion ver)
        {
          try {
            BDSVersions.CurrentVersion = ver;
            AddInCollection addIns = AddInStorage.FetchAddIns();

            AddInInstaller.Install(implementor, addInName, obsoleteNames,
                                   loadType, addIns, null);

            AddInStorage.StoreAddIns(addIns);
          } catch(Exception ex) {
            BDSInterop.HandleException(ex);
          }
        }

        private void UninstallVersion(BDSVersion ver)
        {
          try {
            BDSVersions.CurrentVersion = ver;
            AddInCollection addIns = AddInStorage.FetchAddIns();

            AddInInstaller.Uninstall(implementor, addIns, null);

            AddInStorage.StoreAddIns(addIns);
          } catch(Exception ex) {
            BDSInterop.HandleException(ex);
          }
        }

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components;
        private System.Windows.Forms.Button btnInstall;
        private System.Windows.Forms.Button btnUninstall;
        private System.Windows.Forms.ImageList imageList;
        private System.Windows.Forms.ColumnHeader colStatus;


		public AddInInstallerForm()
		{
			InitializeComponent();
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
            this.components = new System.ComponentModel.Container();
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(AddInInstallerForm));
            this.btnInstall = new System.Windows.Forms.Button();
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.btnUninstall = new System.Windows.Forms.Button();
            this.colStatus = new System.Windows.Forms.ColumnHeader();
            this.pnlBottom.SuspendLayout();
            // 
            // listView
            // 
            this.listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                        this.colStatus});
            this.listView.MultiSelect = true;
            this.listView.Name = "listView";
            this.listView.SmallImageList = this.imageList;
            this.listView.SelectedIndexChanged += new System.EventHandler(this.listView_SelectedIndexChanged);
            // 
            // colVersion
            // 
            this.colVersion.Width = 236;
            // 
            // pnlBottom
            // 
            this.pnlBottom.Controls.Add(this.btnUninstall);
            this.pnlBottom.Controls.Add(this.btnInstall);
            this.pnlBottom.Name = "pnlBottom";
            this.pnlBottom.TabIndex = 0;
            this.pnlBottom.Controls.SetChildIndex(this.btnInstall, 0);
            this.pnlBottom.Controls.SetChildIndex(this.btnUninstall, 0);
            this.pnlBottom.Controls.SetChildIndex(this.btnOK, 0);
            this.pnlBottom.Controls.SetChildIndex(this.btnCancel, 0);
            // 
            // btnOK
            // 
            this.btnOK.Name = "btnOK";
            this.btnOK.TabIndex = 2;
            this.btnOK.Visible = false;
            // 
            // btnCancel
            // 
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.TabIndex = 3;
            this.btnCancel.Text = "   &Close";
            // 
            // btnInstall
            // 
            this.btnInstall.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnInstall.ImageIndex = 2;
            this.btnInstall.ImageList = this.imageList;
            this.btnInstall.Location = new System.Drawing.Point(5, 6);
            this.btnInstall.Name = "btnInstall";
            this.btnInstall.TabIndex = 0;
            this.btnInstall.Text = "&Install";
            this.btnInstall.Click += new System.EventHandler(this.btnInstall_Click);
            // 
            // imageList
            // 
            this.imageList.ImageSize = new System.Drawing.Size(16, 16);
            this.imageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
            this.imageList.TransparentColor = System.Drawing.Color.Lime;
            // 
            // btnUninstall
            // 
            this.btnUninstall.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnUninstall.ImageIndex = 1;
            this.btnUninstall.ImageList = this.imageList;
            this.btnUninstall.Location = new System.Drawing.Point(87, 6);
            this.btnUninstall.Name = "btnUninstall";
            this.btnUninstall.TabIndex = 1;
            this.btnUninstall.Text = "    &Uninstall";
            this.btnUninstall.Click += new System.EventHandler(this.btnUninstall_Click);
            // 
            // colStatus
            // 
            this.colStatus.Text = "Install Status";
            this.colStatus.Width = 120;
            // 
            // AddInInstallerForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(360, 117);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "AddInInstallerForm";
            this.Text = "{In-Code}";
            this.pnlBottom.ResumeLayout(false);
        }
		#endregion
        
        private void listView_SelectedIndexChanged(object sender, System.EventArgs e)
        {
          btnInstall.Enabled   = false;
          btnUninstall.Enabled = false;
          foreach (ListViewItem i in listView.SelectedItems)
          {
            if ( (i.ImageIndex & CanInstallFlag)   != 0)
               btnInstall.Enabled = true;
            if ( (i.ImageIndex & CanUninstallFlag) != 0)
               btnUninstall.Enabled = true;
          }
        }
        
        private void btnInstall_Click(object sender, System.EventArgs e)
        {
          foreach (ListViewItem i in listView.SelectedItems)
            if ( (i.ImageIndex & CanInstallFlag) != 0)
            {
              InstallVersion( (BDSVersion)i.Tag );
              UpdateItem(i, (BDSVersion)i.Tag );
            }

          BDSVersions.CurrentVersion = null;
          listView.Focus();
          listView_SelectedIndexChanged(null, null);
        }

        private void btnUninstall_Click(object sender, System.EventArgs e)
        {
          foreach (ListViewItem i in listView.SelectedItems)
            if ( (i.ImageIndex & CanUninstallFlag) != 0)
            {
              UninstallVersion( (BDSVersion)i.Tag );
              UpdateItem(i, (BDSVersion)i.Tag );
            }

          BDSVersions.CurrentVersion = null;
          listView_SelectedIndexChanged(null, null);
        }

	}
}
