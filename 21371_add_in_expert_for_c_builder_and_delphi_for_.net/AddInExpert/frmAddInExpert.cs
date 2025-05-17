using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

using MarcRohloff.BDS.AddInManager;

using BDSVersion = MarcRohloff.BDS.Utilities.BDSVersion;
using BDSInterop = MarcRohloff.BDS.Utilities.BDSInterop;

namespace MarcRohloff.AddInExpert
{
	/// <summary>
	/// Summary description for WinForm.
	/// </summary>
	public class frmAddInExpert : System.Windows.Forms.Form, IComparer
	{

        #region public methods
        internal static void ShowExpert()
        {
          if (form==null)
            form = new frmAddInExpert();

          form.Show();
          form.BringToFront();
        }

        internal static Bitmap GetMenuBitmap()
        {
          System.Resources.ResourceManager rm = new System.Resources.ResourceManager(typeof(frmAddInExpert));
          return ((Icon)rm.GetObject("$this.Icon")).ToBitmap();
        }


		internal frmAddInExpert()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

            listView.ListViewItemSorter = this;
		}
        #endregion public methods

        #region private fields
        static private frmAddInExpert form;

        private int  sortColumn = 0;
        private bool sortAsc    = true;

        private AddInCollection addIns;
        #endregion private fields

        #region protected methods
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
        #endregion protected methods


        #region IComparer implementation
        int IComparer.Compare(object lhs, object rhs)
        {
          string sl = ((ListViewItem)lhs).SubItems[sortColumn].Text;
          string sr = ((ListViewItem)rhs).SubItems[sortColumn].Text;

          int res = string.Compare(sl, sr, true);
          if (!sortAsc)
            res = -res;

          return res;
        }
        #endregion IComparer implementation


        #region Private Methods
        private ListViewItem ListItem(AddIn d)
        {
          foreach (ListViewItem i in listView.Items)
            if (d==DataItem(i))
              return i;

          return null;
        }

        private void LoadVersions()
        {
          BDSVersion[] versions;

          if (BDS.Utilities.BDSVersions.CurrentVersion != null)
            //Either running in BDS or pre-configured
            versions = new BDSVersion[] { BDS.Utilities.BDSVersions.CurrentVersion };
          else
            versions  = BDS.Utilities.BDSVersions.InstalledVersions;

          if ( (versions==null) || (versions.Length==0) ) return;

          edtVersion.Items.Clear();
          foreach (BDSVersion v in versions)
             edtVersion.Items.Add(v);
          edtVersion.SelectedIndex = 0;

          if (versions.Length<=1) pnlTop.Visible = false;
        }

        private AddIn DataItem(ListViewItem i)
        { return (AddIn)i.Tag; }


        private void RefreshAll()
        {
          foreach (ListViewItem i in listView.Items)
            LoadItem( i, DataItem(i) );
        }


		private void DataChanged(AddIn d)
		{
          if (d.Changed)
          {
		    LoadItem( ListItem(d), d);
            btnOK.Enabled  = true;
            btnCancel.Text = @"C&ancel";
          }
		}


		private void InitItem(ListViewItem item)
		{
		  for (int i=1; i<item.ListView.Columns.Count; i++)
  			  item.SubItems.Add("");
		}


		private void LoadItem(ListViewItem item, AddIn data)
		{
            if (item==null) return;

			item.Tag              = data;
			item.StateImageIndex  = (data.Loaded)? 1:0;
			item.SubItems[0].Text = data.Name;
			item.ImageIndex       = (int)data.LoadType + 2;
			item.SubItems[1].Text = data.Path;
			item.Font             = new Font(item.Font,
									         data.Changed ? FontStyle.Italic
                                                          : FontStyle.Regular );
		}


        private bool CheckSave()
        {
          if (!btnOK.Enabled) return true;

          DialogResult r = MessageBox.Show("Do you wan't to save tha changes you made?",
                                           "Save Changes",
                                           MessageBoxButtons.YesNoCancel,
                                           MessageBoxIcon.Question,
                                           MessageBoxDefaultButton.Button1);
           if (r==DialogResult.Yes)
           {
             SaveList();
             return true;
           }
           else if (r==DialogResult.No)
           {
             btnOK.Enabled = false;
             return true;
           }
           else
           {
             return false;
           }
        }

        private void SaveList()
        {
          if (edtVersion.SelectedItem==null) return;

          AddInStorage.StoreAddIns(addIns);
          btnOK.Enabled = false;
        }

		private void LoadList()
		{
          listView.Items.Clear();
          btnOK.Enabled = false;
          btnOptions.Enabled = false;

          if (edtVersion.SelectedItem != null)
          {
            btnOptions.Enabled = true;
            
            BDS.Utilities.BDSVersions.CurrentVersion =
              (BDSVersion)edtVersion.SelectedItem;

            addIns = AddInStorage.FetchAddIns();

            foreach (AddIn a in addIns)
            {
              ListViewItem i = listView.Items.Add("");
              InitItem(i);
              LoadItem(i, a);
              listView.Sort();
            }
          };

		}


		private AddInCollection SelectedItems()
		{
		  AddInCollection list = new AddInCollection();

		  for (int i=0; i<listView.SelectedItems.Count; i++)
			 list.Add( DataItem( listView.SelectedItems[i] ) );

		  return list;
		}


        private void ChangeLoadType(object sender, System.EventArgs e)
        {
            LoadType lt;
            if (sender == mniAutoBDS)
				 lt = LoadType.AutoBDS;
			else if (sender == mniAutoExpert)
				 lt = LoadType.AutoExpert;
			else if (sender == mniManual)
                 lt = LoadType.Manual;
            else
                 lt = LoadType.Removed;

		   AddInCollection sel = SelectedItems();
		   foreach (AddIn d in sel)
		   {
			  d.LoadType = lt;
			  DataChanged(d);
		   };

		}


        private void LoadState()
        {
          try {
            Microsoft.Win32.RegistryKey reg =
            Microsoft.Win32.Registry.CurrentUser.OpenSubKey(BDS.Utilities.BDSRegistry.BDSRootRegPath + "AddInsExpert");
            if (reg==null) return;

            using (reg)
            {
               DesktopBounds = BDS.Utilities.BDSForms.StringToRect(
                               (string)reg.GetValue("Position"),
                               DesktopBounds);

               string vers  = (string)reg.GetValue("BDSVersion", "");
               double verd  = double.Parse(vers, System.Globalization.NumberFormatInfo.InvariantInfo);
               for (int i = 0; i<edtVersion.Items.Count; i++)
                 if (((BDSVersion)edtVersion.Items[i]).Matches(verd))
                 {
                    edtVersion.SelectedIndex = i;
                    break;
                 }
            }
          } catch { /*ignore*/ }
        }


        private void SaveState()
        {
          Microsoft.Win32.RegistryKey reg =
          Microsoft.Win32.Registry.CurrentUser.CreateSubKey(BDS.Utilities.BDSRegistry.BDSRootRegPath + "AddInsExpert");
          if (reg==null) return;

          using (reg)
          {
            reg.SetValue("Position",
                         BDS.Utilities.BDSForms.RectToString(DesktopBounds) );
            if ( (pnlTop.Visible) && (edtVersion.Items.Count>1)
                 && (edtVersion.SelectedIndex>=0) )
            {
              reg.SetValue("BDSVersion", ((BDSVersion)edtVersion.SelectedItem).Version.ToString("f1") );
            }
          }
        }

        private void InstallingExpert(AddIn d)
        {
          ListViewItem i = ListItem(d);
          if (i==null)
          {
            i = listView.Items.Add("");
            InitItem(i);
            LoadItem(i, d);
            listView.SelectedItems.Clear();
            i.Selected = true;
            i.Focused  = true;
          }

          DataChanged(d);
        }
        #endregion private methods


        #region Required Designer variables
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components;
        private System.Windows.Forms.ListView listView;
        private System.Windows.Forms.Button btnOK;
        private System.Windows.Forms.ImageList imageList;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Label label1;
		private System.Windows.Forms.ColumnHeader colName;
		private System.Windows.Forms.ColumnHeader colPath;
        private System.Windows.Forms.ContextMenu mnuContext;
        private System.Windows.Forms.MenuItem mniLoad;
        private System.Windows.Forms.MenuItem mnsLoad;
        private System.Windows.Forms.MenuItem mniAutoBDS;
        private System.Windows.Forms.MenuItem mniAutoExpert;
        private System.Windows.Forms.MenuItem mniManual;
		private System.Windows.Forms.MenuItem menuItem6;
		private System.Windows.Forms.MenuItem mniRemove;
        private System.Windows.Forms.MenuItem mniUnload;
        private System.Windows.Forms.Button btnOptions;
        private System.Windows.Forms.OpenFileDialog dlgOpenFile;
        private System.Windows.Forms.MenuItem menuItem1;
        private System.Windows.Forms.MenuItem mniRename;
        private System.Windows.Forms.MenuItem menuItem4;
        private System.Windows.Forms.ContextMenu mnuOptions;
        private System.Windows.Forms.MenuItem mniOptAdd;
        private System.Windows.Forms.MenuItem mniOptLoad;
        private System.Windows.Forms.MenuItem menuItem5;
        private System.Windows.Forms.MenuItem mniOptInstall;
        private System.Windows.Forms.MenuItem mniOptUninstall;
        private System.Windows.Forms.MenuItem mniOptClean;
        private System.Windows.Forms.Panel pnlTop;
        private System.Windows.Forms.Panel pnlClient;
        private System.Windows.Forms.Panel pnlBottom;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox edtVersion;

        #endregion Required Designer variables


		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(frmAddInExpert));
            this.imageList = new System.Windows.Forms.ImageList(this.components);
            this.mnuContext = new System.Windows.Forms.ContextMenu();
            this.mniRename = new System.Windows.Forms.MenuItem();
            this.menuItem4 = new System.Windows.Forms.MenuItem();
            this.mniLoad = new System.Windows.Forms.MenuItem();
            this.mniUnload = new System.Windows.Forms.MenuItem();
            this.mnsLoad = new System.Windows.Forms.MenuItem();
            this.mniAutoBDS = new System.Windows.Forms.MenuItem();
            this.mniAutoExpert = new System.Windows.Forms.MenuItem();
            this.mniManual = new System.Windows.Forms.MenuItem();
            this.menuItem6 = new System.Windows.Forms.MenuItem();
            this.mniRemove = new System.Windows.Forms.MenuItem();
            this.dlgOpenFile = new System.Windows.Forms.OpenFileDialog();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.mnuOptions = new System.Windows.Forms.ContextMenu();
            this.mniOptAdd = new System.Windows.Forms.MenuItem();
            this.mniOptLoad = new System.Windows.Forms.MenuItem();
            this.menuItem5 = new System.Windows.Forms.MenuItem();
            this.mniOptClean = new System.Windows.Forms.MenuItem();
            this.mniOptInstall = new System.Windows.Forms.MenuItem();
            this.mniOptUninstall = new System.Windows.Forms.MenuItem();
            this.pnlTop = new System.Windows.Forms.Panel();
            this.edtVersion = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.pnlClient = new System.Windows.Forms.Panel();
            this.listView = new System.Windows.Forms.ListView();
            this.colName = new System.Windows.Forms.ColumnHeader();
            this.colPath = new System.Windows.Forms.ColumnHeader();
            this.label1 = new System.Windows.Forms.Label();
            this.pnlBottom = new System.Windows.Forms.Panel();
            this.btnOptions = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOK = new System.Windows.Forms.Button();
            this.pnlTop.SuspendLayout();
            this.pnlClient.SuspendLayout();
            this.pnlBottom.SuspendLayout();
            this.SuspendLayout();
            // 
            // imageList
            // 
            this.imageList.ImageSize = new System.Drawing.Size(16, 16);
            this.imageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
            this.imageList.TransparentColor = System.Drawing.Color.Magenta;
            // 
            // mnuContext
            // 
            this.mnuContext.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                        this.mniRename,
                        this.menuItem4,
                        this.mniLoad,
                        this.mniUnload,
                        this.mnsLoad,
                        this.mniAutoBDS,
                        this.mniAutoExpert,
                        this.mniManual,
                        this.menuItem6,
                        this.mniRemove});
            this.mnuContext.Popup += new System.EventHandler(this.mnuContext_Popup);
            // 
            // mniRename
            // 
            this.mniRename.Index = 0;
            this.mniRename.Text = "Re&name";
            this.mniRename.Click += new System.EventHandler(this.mniRename_Click);
            // 
            // menuItem4
            // 
            this.menuItem4.Index = 1;
            this.menuItem4.Text = "-";
            // 
            // mniLoad
            // 
            this.mniLoad.DefaultItem = true;
            this.mniLoad.Index = 2;
            this.mniLoad.Text = "&Load Now";
            this.mniLoad.Click += new System.EventHandler(this.mniLoad_Click);
            // 
            // mniUnload
            // 
            this.mniUnload.Enabled = false;
            this.mniUnload.Index = 3;
            this.mniUnload.Text = "&Unload";
            // 
            // mnsLoad
            // 
            this.mnsLoad.Index = 4;
            this.mnsLoad.Text = "-";
            // 
            // mniAutoBDS
            // 
            this.mniAutoBDS.Index = 5;
            this.mniAutoBDS.RadioCheck = true;
            this.mniAutoBDS.Text = "Automatic (&BDS)";
            this.mniAutoBDS.Click += new System.EventHandler(this.ChangeLoadType);
            // 
            // mniAutoExpert
            // 
            this.mniAutoExpert.Index = 6;
            this.mniAutoExpert.RadioCheck = true;
            this.mniAutoExpert.Text = "Automatic (E&xpert)";
            this.mniAutoExpert.Click += new System.EventHandler(this.ChangeLoadType);
            // 
            // mniManual
            // 
            this.mniManual.Index = 7;
            this.mniManual.RadioCheck = true;
            this.mniManual.Text = "&Manual";
            this.mniManual.Click += new System.EventHandler(this.ChangeLoadType);
            // 
            // menuItem6
            // 
            this.menuItem6.Index = 8;
            this.menuItem6.Text = "-";
            // 
            // mniRemove
            // 
            this.mniRemove.Index = 9;
            this.mniRemove.Text = "&Remove";
            this.mniRemove.Click += new System.EventHandler(this.ChangeLoadType);
            // 
            // dlgOpenFile
            // 
            this.dlgOpenFile.DefaultExt = "dll";
            this.dlgOpenFile.Filter = "Experts (*.exe,*.dll)|*.exe;*.dll|All files (*.*)|*.*";
            this.dlgOpenFile.FilterIndex = 0;
            // 
            // menuItem1
            // 
            this.menuItem1.Index = -1;
            this.menuItem1.Text = "-";
            // 
            // mnuOptions
            // 
            this.mnuOptions.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                        this.mniOptAdd,
                        this.mniOptLoad,
                        this.menuItem5,
                        this.mniOptClean,
                        this.mniOptInstall,
                        this.mniOptUninstall});
            this.mnuOptions.Popup += new System.EventHandler(this.mnuOptions_Popup);
            // 
            // mniOptAdd
            // 
            this.mniOptAdd.Index = 0;
            this.mniOptAdd.Text = "&Add ...";
            this.mniOptAdd.Click += new System.EventHandler(this.mniOptAdd_Click);
            // 
            // mniOptLoad
            // 
            this.mniOptLoad.Index = 1;
            this.mniOptLoad.Text = "&Load Once ...";
            this.mniOptLoad.Click += new System.EventHandler(this.mniOptLoad_Click);
            // 
            // menuItem5
            // 
            this.menuItem5.Index = 2;
            this.menuItem5.Text = "-";
            // 
            // mniOptClean
            // 
            this.mniOptClean.Index = 3;
            this.mniOptClean.Text = "&Clean";
            this.mniOptClean.Click += new System.EventHandler(this.mniOptClean_Click);
            // 
            // mniOptInstall
            // 
            this.mniOptInstall.Index = 4;
            this.mniOptInstall.Text = "&Install";
            this.mniOptInstall.Click += new System.EventHandler(this.mniOptInstall_Click);
            // 
            // mniOptUninstall
            // 
            this.mniOptUninstall.Index = 5;
            this.mniOptUninstall.Text = "&Uninstall";
            this.mniOptUninstall.Click += new System.EventHandler(this.mniOptUninstall_Click);
            // 
            // pnlTop
            // 
            this.pnlTop.Controls.Add(this.edtVersion);
            this.pnlTop.Controls.Add(this.label2);
            this.pnlTop.Dock = System.Windows.Forms.DockStyle.Top;
            this.pnlTop.Location = new System.Drawing.Point(0, 0);
            this.pnlTop.Name = "pnlTop";
            this.pnlTop.Size = new System.Drawing.Size(528, 32);
            this.pnlTop.TabIndex = 5;
            // 
            // edtVersion
            // 
            this.edtVersion.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.edtVersion.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.edtVersion.Location = new System.Drawing.Point(100, 5);
            this.edtVersion.Name = "edtVersion";
            this.edtVersion.Size = new System.Drawing.Size(420, 21);
            this.edtVersion.TabIndex = 4;
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(8, 8);
            this.label2.Name = "label2";
            this.label2.TabIndex = 3;
            this.label2.Text = "Installed Versions :";
            // 
            // pnlClient
            // 
            this.pnlClient.Controls.Add(this.listView);
            this.pnlClient.Controls.Add(this.label1);
            this.pnlClient.Dock = System.Windows.Forms.DockStyle.Fill;
            this.pnlClient.Location = new System.Drawing.Point(0, 32);
            this.pnlClient.Name = "pnlClient";
            this.pnlClient.Size = new System.Drawing.Size(528, 181);
            this.pnlClient.TabIndex = 6;
            // 
            // listView
            // 
            this.listView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
                        | System.Windows.Forms.AnchorStyles.Left) 
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.listView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
                        this.colName,
                        this.colPath});
            this.listView.FullRowSelect = true;
            this.listView.HideSelection = false;
            this.listView.LabelEdit = true;
            this.listView.Location = new System.Drawing.Point(8, 16);
            this.listView.Name = "listView";
            this.listView.Size = new System.Drawing.Size(512, 134);
            this.listView.SmallImageList = this.imageList;
            this.listView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.listView.StateImageList = this.imageList;
            this.listView.TabIndex = 3;
            this.listView.View = System.Windows.Forms.View.Details;
            this.listView.DoubleClick += new System.EventHandler(this.listView_DoubleClick);
            this.listView.MouseUp += new System.Windows.Forms.MouseEventHandler(this.listView_MouseUp);
            this.listView.Layout += new System.Windows.Forms.LayoutEventHandler(this.listView_Layout);
            this.listView.KeyUp += new System.Windows.Forms.KeyEventHandler(this.listView_KeyUp);
            this.listView.AfterLabelEdit += new System.Windows.Forms.LabelEditEventHandler(this.listView_AfterLabelEdit);
            this.listView.ColumnClick += new System.Windows.Forms.ColumnClickEventHandler(this.listView_ColumnClick);
            // 
            // colName
            // 
            this.colName.Text = "Name";
            this.colName.Width = 180;
            // 
            // colPath
            // 
            this.colPath.Text = "Path";
            this.colPath.Width = 150;
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 0);
            this.label1.Name = "label1";
            this.label1.TabIndex = 2;
            this.label1.Text = "Available E&xperts:";
            // 
            // pnlBottom
            // 
            this.pnlBottom.Controls.Add(this.btnOptions);
            this.pnlBottom.Controls.Add(this.btnCancel);
            this.pnlBottom.Controls.Add(this.btnOK);
            this.pnlBottom.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.pnlBottom.Location = new System.Drawing.Point(0, 187);
            this.pnlBottom.Name = "pnlBottom";
            this.pnlBottom.Size = new System.Drawing.Size(528, 26);
            this.pnlBottom.TabIndex = 7;
            // 
            // btnOptions
            // 
            this.btnOptions.AccessibleRole = System.Windows.Forms.AccessibleRole.ButtonMenu;
            this.btnOptions.BackColor = System.Drawing.SystemColors.Control;
            this.btnOptions.ImageAlign = System.Drawing.ContentAlignment.MiddleRight;
            this.btnOptions.Location = new System.Drawing.Point(8, 2);
            this.btnOptions.Name = "btnOptions";
            this.btnOptions.TabIndex = 5;
            this.btnOptions.Text = "&Options";
            this.btnOptions.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnOptions.Click += new System.EventHandler(this.btnOptions_Click);
            // 
            // btnCancel
            // 
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.CausesValidation = false;
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Location = new System.Drawing.Point(448, 2);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.TabIndex = 7;
            this.btnCancel.Text = "C&lose";
            this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
            // 
            // btnOK
            // 
            this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnOK.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnOK.Location = new System.Drawing.Point(368, 2);
            this.btnOK.Name = "btnOK";
            this.btnOK.TabIndex = 6;
            this.btnOK.Text = "&OK";
            this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
            // 
            // frmAddInExpert
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(528, 213);
            this.Controls.Add(this.pnlBottom);
            this.Controls.Add(this.pnlClient);
            this.Controls.Add(this.pnlTop);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "frmAddInExpert";
            this.ShowInTaskbar = false;
            this.Text = "Add-Ins Expert";
            this.Closing += new System.ComponentModel.CancelEventHandler(this.frmExperts_Closing);
            this.Load += new System.EventHandler(this.frmExperts_Load);
            this.Closed += new System.EventHandler(this.frmExperts_Closed);
            this.pnlTop.ResumeLayout(false);
            this.pnlClient.ResumeLayout(false);
            this.pnlBottom.ResumeLayout(false);
            this.ResumeLayout(false);
        }
		#endregion


        #region Event Handlers
        private void btnCancel_Click(object sender, System.EventArgs e)
        {
            Close();
        }


		private void listView_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			if (e.Button == MouseButtons.Right)
			{
			  if (listView.SelectedItems.Count>0)
				mnuContext.Show(listView,
								 new Point(e.X, e.Y) );
			}
		}


		private void mnuContext_Popup(object sender, System.EventArgs e)
		{
		   AddInCollection sel = SelectedItems();

		   LoadType     lt      = sel[0].LoadType;
		   bool         canLoad = false;

		   foreach (AddIn d in sel)
		   {
			  canLoad |= (!d.Loaded);

			  if (d.LoadType != lt)
				 lt = LoadType.Removed;
		   };

           mniRename.Enabled     = (sel.Count==1);

		   mniAutoBDS.Checked    = (lt == LoadType.AutoBDS);
		   mniAutoExpert.Checked = (lt == LoadType.AutoExpert);
		   mniManual.Checked     = (lt == LoadType.Manual);

		   mniLoad.Enabled       = canLoad;
		}

        private void frmExperts_Load(object sender, System.EventArgs e)
        {
           LoadVersions();
           LoadState();

           if (BDSInterop.StandAlone)
           {
             listView.StateImageList = null;
             mniLoad.Visible    = false;
             mniUnload.Visible  = false;
             mnsLoad.Visible    = false;
             mniOptLoad.Visible = false;
          };

          LoadList();

          edtVersion.SelectedIndexChanged += new System.EventHandler(edtVersion_SelectedIndexChanged);
          listView_Layout(null, null);

          if (!BDSInterop.StandAlone)
            BDS.Utilities.BDSForms.FixIDEForm(form);
        }


        private void listView_AfterLabelEdit(object sender, System.Windows.Forms.LabelEditEventArgs e)
        {
          e.CancelEdit=true;

          if (e.Label==null) return;

          string text = e.Label;
          text = text.Replace( AddInResources.SeperatorChar.ToString(), "");
          text = text.Trim();

          if (text=="") return;

          AddIn d   = DataItem( listView.Items[e.Item] );
          d.Name         = text;
          DataChanged(d);

          listView.Sort();
        }


        private void mniRename_Click(object sender, System.EventArgs e)
        {
            listView.SelectedItems[0].BeginEdit();
        }


        private void listView_ColumnClick(object sender, System.Windows.Forms.ColumnClickEventArgs e)
        {
          if (e.Column==sortColumn)
          {
              sortAsc = (!sortAsc);
          }
          else
          {
              sortColumn = e.Column;
              sortAsc = true;
          }

          listView.Sort();
        }

        private void btnOK_Click(object sender, System.EventArgs e)
        {
           SaveList();
           Close();
        }


        private void listView_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
        {
          if (e.KeyData==Keys.F2)
          {
            if (listView.FocusedItem != null)
            {
              listView.FocusedItem.BeginEdit();
              e.Handled = true;
            };
          };
        }

        private void mniOptClean_Click(object sender, System.EventArgs e)
        {
           foreach (AddIn a in addIns)
             if (!a.Exists)
             {
               a.LoadType = LoadType.Removed;
               DataChanged(a);
             }
        }

        
        private void btnOptions_Click(object sender, System.EventArgs e)
        {
          mnuOptions.Show(btnOptions, new Point(0, btnOptions.Height) );
        }


        private void mniOptLoad_Click(object sender, System.EventArgs e)
        {
          dlgOpenFile.Title = @"Load Add-In";

		  try {
			  if (dlgOpenFile.ShowDialog(this) == DialogResult.OK)
			  {
				BDS.Utilities.BDSFiles.LoadAddIn(dlgOpenFile.FileName);
				System.Threading.Thread.Sleep(200);
				RefreshAll();
			  }
		   }
		   catch (Exception ex)
		   {
			  Application.OnThreadException(ex);
		   }

		}


		private void mniLoad_Click(object sender, System.EventArgs e)
		{
		  AddInCollection list = SelectedItems();
		  foreach (AddIn d in list)
		  {
			 try
			 {
			   d.Load();
			 }
			 catch (Exception ex)
			 {
			   Application.OnThreadException(ex);
			 }
		  }

          System.Threading.Thread.Sleep(200);
          RefreshAll();
        }


        private void frmExperts_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            SaveState();
        }


        private void listView_Layout(object sender, System.Windows.Forms.LayoutEventArgs e)
        {
           colPath.Width = listView.ClientSize.Width - colName.Width;
        }


        private void mniOptUninstall_Click(object sender, System.EventArgs e)
        {
           AddInInstaller.Uninstall(GetType(),
                                    addIns,
                                    new AddInEvent(DataChanged) );
        }


        private void mnuOptions_Popup(object sender, System.EventArgs e)
        {
           bool canInstall;
           bool canUninstall;
           AddInInstaller.CheckInstall(out canInstall, out canUninstall,
                                       GetType(), addIns);

           mniOptUninstall.Visible = canUninstall;
           mniOptInstall.Visible   = canInstall;
        }


        private AddIn AddExpert(string name, string path, out bool existed)
        {
          existed = true;
          AddIn a = addIns.Find( path );
          if (a==null)
          {
            a = new AddIn(name, path);

            /* Force AutoBDS LoadType if not installed */
            bool canInstall;
            bool canUninstall;
            AddInInstaller.CheckInstall(out canInstall, out canUninstall,
                                        GetType(), addIns);
            if (canInstall)
              a.LoadType = LoadType.AutoBDS;


            addIns.Add(a);
            a.MarkAsNew();

            ListViewItem i = listView.Items.Add("");
            InitItem(i);
            LoadItem(i, a);
            DataChanged(a);
            existed = false;
          }

          return a;
        }


        private void mniOptAdd_Click(object sender, System.EventArgs e)
        {
          dlgOpenFile.Title = @"Add Add-In";

          if (dlgOpenFile.ShowDialog(this) == DialogResult.OK)
          {
            string path = dlgOpenFile.FileName;
            bool   existed;

            AddIn d = AddExpert("", path, out existed);
            ListViewItem i = ListItem(d);

            listView.SelectedItems.Clear();
            i.Selected = true;
            i.Focused  = true;

            if (!existed)
            {
              listView.Sort();
              i.BeginEdit();
            }

          }

        }

        private void mniOptInstall_Click(object sender, System.EventArgs e)
        {
          AddInInstaller.Install(GetType(), null, null,
                                 LoadType.AutoBDS, addIns,
                                 new AddInEvent(InstallingExpert) );
          listView.Sort();
        }

        private void frmExperts_Closed(object sender, System.EventArgs e)
        {
          form = null;
        }

        /*linked manually*/
        private void edtVersion_SelectedIndexChanged(object sender, System.EventArgs e)
        {
          if (edtVersion.SelectedItem==BDS.Utilities.BDSVersions.CurrentVersion)
            return;

          if (CheckSave() )
            LoadList();

          edtVersion.SelectedItem = BDS.Utilities.BDSVersions.CurrentVersion;
        }

        private void listView_DoubleClick(object sender, System.EventArgs e)
        {
          mniLoad.PerformClick();
        }

        #endregion Event Handlers

     } /* frmExperts */

} /*namespace*/
