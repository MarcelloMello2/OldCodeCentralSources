using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace MarcRohloff.FavouritesMenuAddIn
{
	internal class frmConfigureFavourites : System.Windows.Forms.Form
	{
        const int ShortcutDisplayWidth = 100;

        #region Public methods
        public void Read(Favourites from)
        {
          listBox.Items.Clear();
          foreach (Favourite f in from)
          {
            listBox.Items.Add(f);
            f.UpdateExists();
          }

          edtDisplayMode.SelectedIndex = (int)from.DisplayMode;
          chkShowExtensions.Checked    = from.ShowExtension;

          if (listBox.Items.Count>0)
            listBox.SelectedIndex = listBox.Items.Count-1;

          CancelChanges();
          UpdateButtons();
        }

        public void Write(Favourites to)
        {
          to.Clear();

          to.DisplayMode = (DisplayMode)edtDisplayMode.SelectedIndex;
          to.ShowExtension = chkShowExtensions.Checked;

          foreach (Favourite f in listBox.Items)
            to.Add(f);
        }
        #endregion Public methods

        #region Private Fields
        TypeConverter keysConverter = null;
        #endregion Private Fields

        #region Private methods
        private void CancelChanges()
        {
          btnOK.Enabled = false;
          btnCancel.Text = "   &Close";
        }

        private void UpdateButtons()
        {
          btnCleanFiles.Enabled = (listBox.Items.Count>0);
          btnDeleteFile.Enabled = (listBox.SelectedIndex>=0);
          btnMoveUp.Enabled     = (listBox.SelectedIndex>0);
          btnMoveDown.Enabled   = (listBox.SelectedIndex>=0) &&
                                  (listBox.SelectedIndex<listBox.Items.Count-1);
        }

        private void MakeButtonImagesTransparent(Control root)
        {
          foreach(Control c in root.Controls)
          {
            MakeButtonImagesTransparent(c);

            Button b = c as Button;
            if ( (b!=null) && (b.Image as Bitmap != null) )
              ((Bitmap)b.Image).MakeTransparent();
          };
        }

        private void DataChanged(object sender, System.EventArgs e)
        {
           btnOK.Enabled  = true;
           btnCancel.Text = "   &Cancel";
           UpdateButtons();
        }

        private int IndexOfFile(string fn)
        {
          for(int i=0; i<listBox.Items.Count; i++)
            if (String.Compare(fn, ((Favourite)listBox.Items[i]).Filename, true) ==0)
              return i;

          return -1;
        }

        private void HandleFormLevelShortcuts(KeyEventArgs e)
        {
          switch (e.KeyData)
          {
            case Keys.Insert :
               if (btnAddFile.Enabled)
               {
                 btnAddFile.PerformClick();
                 e.Handled = true;
               }
               break;

            case Keys.Subtract :
            case Keys.OemMinus :
               if (btnAddSeperator.Enabled)
               {
                 btnAddSeperator.PerformClick();
                 e.Handled = true;
               }
               break;

            case Keys.Delete :
               if (btnDeleteFile.Enabled)
               {
                 btnDeleteFile.PerformClick();
                 e.Handled = true;
               }
               break;

            case Keys.Up | Keys.Shift:
               if (btnMoveUp.Enabled)
               {
                 btnMoveUp.PerformClick();
                 e.Handled = true;
               }
               break;

            case Keys.Down | Keys.Shift:
               if (btnMoveDown.Enabled)
               {
                 btnMoveDown.PerformClick();
                 e.Handled = true;
               }
               break;

            case Keys.Down :
               if ( (listBox.SelectedIndex<listBox.Items.Count-1)
                  && (listBox.SelectedIndex>=0) )
               {
                 listBox.SelectedIndex++;
                 e.Handled = true;
               }
               break;

            case Keys.Up :
               if (listBox.SelectedIndex>0)
               {
                 listBox.SelectedIndex--;
                 e.Handled = true;
               }
               break;

           } /*switch*/
        }

        private void MakeFormKeypressIntoFavouriteShortcut(KeyEventArgs e)
        {
          if (e.Handled) return;
          if (listBox.SelectedIndex<0) return;

          if ((e.KeyData & Keys.Modifiers) != 0)
          {
            if ( (e.KeyCode==Keys.ControlKey)
              || (e.KeyCode==Keys.LControlKey)
              || (e.KeyCode==Keys.RControlKey)
              || (e.KeyCode==Keys.Menu)
              || (e.KeyCode==Keys.LMenu)
              || (e.KeyCode==Keys.RMenu)
              || (e.KeyCode==Keys.LWin)
              || (e.KeyCode==Keys.RWin)
              || (e.KeyCode==Keys.ShiftKey)
              || (e.KeyCode==Keys.LShiftKey)
              || (e.KeyCode==Keys.LShiftKey) )
              return;

            Favourite f = (Favourite)listBox.SelectedItem;
            if (f.IsSeperator) return;

            e.Handled = true;
            f.Shortcut  = e.KeyData;
            listBox.Invalidate( listBox.GetItemRectangle(listBox.SelectedIndex) );
          }

          if ( (e.KeyData == Keys.Escape) | (e.KeyData==Keys.Space) )
          {
            Favourite f = (Favourite)listBox.SelectedItem;
            e.Handled   = true;
            f.Shortcut  = Keys.None;
            listBox.Invalidate( listBox.GetItemRectangle(listBox.SelectedIndex) );
          }
        }

        private void MoveItem(int from, int to)
        {
          Object o = listBox.Items[from];
          listBox.Items.RemoveAt(from);
          listBox.Items.Insert(to, o);
          listBox.SelectedIndex = to;
          DataChanged(null, null);
        }

        private void DropFiles(string[] files, int row)
        {
          foreach (string fn in files)
          {
            if (System.IO.File.Exists(fn))
            {
              int i = IndexOfFile(fn);
              if (i>=0)
              {
                listBox.SelectedIndex = i;
              }
              else
              {
                row++;
                Favourite f = new Favourite(fn);
                listBox.Items.Insert(row, f );
                listBox.SelectedIndex = row;
                DataChanged(null, null);
              }
            }
          } /* foreach */   
        }

        #endregion Private methods


        #region Designer Generated Code
		private System.ComponentModel.IContainer components;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox edtDisplayMode;
        private System.Windows.Forms.Button btnOK;
        private System.Windows.Forms.Button btnCancel;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Button btnMoveUp;
        private System.Windows.Forms.Button btnMoveDown;
        private System.Windows.Forms.Button btnCleanFiles;
        private System.Windows.Forms.Button btnAddFile;
        private System.Windows.Forms.Button btnAddSeperator;
        private System.Windows.Forms.Button btnDeleteFile;
        private System.Windows.Forms.OpenFileDialog openFileDialog;
        private System.Windows.Forms.ToolTip toolTip;
        private System.Windows.Forms.CheckBox chkShowExtensions;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.ListBox listBox;

		public frmConfigureFavourites()
		{
		  InitializeComponent();
          keysConverter = TypeDescriptor.GetConverter(typeof(Keys));
		}

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
		private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(frmConfigureFavourites));
            this.panel1 = new System.Windows.Forms.Panel();
            this.chkShowExtensions = new System.Windows.Forms.CheckBox();
            this.btnCancel = new System.Windows.Forms.Button();
            this.btnOK = new System.Windows.Forms.Button();
            this.edtDisplayMode = new System.Windows.Forms.ComboBox();
            this.label1 = new System.Windows.Forms.Label();
            this.panel2 = new System.Windows.Forms.Panel();
            this.btnDeleteFile = new System.Windows.Forms.Button();
            this.btnAddSeperator = new System.Windows.Forms.Button();
            this.btnAddFile = new System.Windows.Forms.Button();
            this.btnCleanFiles = new System.Windows.Forms.Button();
            this.btnMoveDown = new System.Windows.Forms.Button();
            this.btnMoveUp = new System.Windows.Forms.Button();
            this.openFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.toolTip = new System.Windows.Forms.ToolTip(this.components);
            this.panel3 = new System.Windows.Forms.Panel();
            this.listBox = new System.Windows.Forms.ListBox();
            this.panel1.SuspendLayout();
            this.panel2.SuspendLayout();
            this.panel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.chkShowExtensions);
            this.panel1.Controls.Add(this.btnCancel);
            this.panel1.Controls.Add(this.btnOK);
            this.panel1.Controls.Add(this.edtDisplayMode);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 209);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(328, 64);
            this.panel1.TabIndex = 0;
            // 
            // chkShowExtensions
            // 
            this.chkShowExtensions.Location = new System.Drawing.Point(4, 40);
            this.chkShowExtensions.Name = "chkShowExtensions";
            this.chkShowExtensions.Size = new System.Drawing.Size(152, 24);
            this.chkShowExtensions.TabIndex = 1;
            this.chkShowExtensions.Text = "Show Extensions";
            this.chkShowExtensions.Click += new System.EventHandler(this.DataChanged);
            // 
            // btnCancel
            // 
            this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Image = ((System.Drawing.Image)(resources.GetObject("btnCancel.Image")));
            this.btnCancel.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnCancel.Location = new System.Drawing.Point(248, 37);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.TabIndex = 3;
            this.btnCancel.Text = "  &Close";
            // 
            // btnOK
            // 
            this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnOK.Enabled = false;
            this.btnOK.Image = ((System.Drawing.Image)(resources.GetObject("btnOK.Image")));
            this.btnOK.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.btnOK.Location = new System.Drawing.Point(168, 37);
            this.btnOK.Name = "btnOK";
            this.btnOK.TabIndex = 2;
            this.btnOK.Text = "&OK";
            // 
            // edtDisplayMode
            // 
            this.edtDisplayMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.edtDisplayMode.Items.AddRange(new object[] {
                        "Fullname",
                        "Filename",
                        "Relative "});
            this.edtDisplayMode.Location = new System.Drawing.Point(4, 18);
            this.edtDisplayMode.Name = "edtDisplayMode";
            this.edtDisplayMode.Size = new System.Drawing.Size(121, 21);
            this.edtDisplayMode.TabIndex = 0;
            this.edtDisplayMode.SelectedIndexChanged += new System.EventHandler(this.DataChanged);
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(4, 2);
            this.label1.Name = "label1";
            this.label1.TabIndex = 0;
            this.label1.Text = "Display Mode:";
            // 
            // panel2
            // 
            this.panel2.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panel2.Controls.Add(this.btnDeleteFile);
            this.panel2.Controls.Add(this.btnAddSeperator);
            this.panel2.Controls.Add(this.btnAddFile);
            this.panel2.Controls.Add(this.btnCleanFiles);
            this.panel2.Controls.Add(this.btnMoveDown);
            this.panel2.Controls.Add(this.btnMoveUp);
            this.panel2.Dock = System.Windows.Forms.DockStyle.Right;
            this.panel2.Location = new System.Drawing.Point(296, 0);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(32, 209);
            this.panel2.TabIndex = 1;
            // 
            // btnDeleteFile
            // 
            this.btnDeleteFile.Image = ((System.Drawing.Image)(resources.GetObject("btnDeleteFile.Image")));
            this.btnDeleteFile.Location = new System.Drawing.Point(5, 99);
            this.btnDeleteFile.Name = "btnDeleteFile";
            this.btnDeleteFile.Size = new System.Drawing.Size(23, 23);
            this.btnDeleteFile.TabIndex = 5;
            this.btnDeleteFile.TabStop = false;
            this.toolTip.SetToolTip(this.btnDeleteFile, "Delete an item");
            this.btnDeleteFile.Click += new System.EventHandler(this.btnDeleteFile_Click);
            // 
            // btnAddSeperator
            // 
            this.btnAddSeperator.Image = ((System.Drawing.Image)(resources.GetObject("btnAddSeperator.Image")));
            this.btnAddSeperator.Location = new System.Drawing.Point(5, 76);
            this.btnAddSeperator.Name = "btnAddSeperator";
            this.btnAddSeperator.Size = new System.Drawing.Size(23, 23);
            this.btnAddSeperator.TabIndex = 4;
            this.btnAddSeperator.TabStop = false;
            this.toolTip.SetToolTip(this.btnAddSeperator, "Add a seperator");
            this.btnAddSeperator.Click += new System.EventHandler(this.btnAddSeperator_Click);
            // 
            // btnAddFile
            // 
            this.btnAddFile.Image = ((System.Drawing.Image)(resources.GetObject("btnAddFile.Image")));
            this.btnAddFile.Location = new System.Drawing.Point(5, 53);
            this.btnAddFile.Name = "btnAddFile";
            this.btnAddFile.Size = new System.Drawing.Size(23, 23);
            this.btnAddFile.TabIndex = 3;
            this.btnAddFile.TabStop = false;
            this.toolTip.SetToolTip(this.btnAddFile, "Add an item");
            this.btnAddFile.Click += new System.EventHandler(this.btnAddFile_Click);
            // 
            // btnCleanFiles
            // 
            this.btnCleanFiles.Image = ((System.Drawing.Image)(resources.GetObject("btnCleanFiles.Image")));
            this.btnCleanFiles.Location = new System.Drawing.Point(5, 125);
            this.btnCleanFiles.Name = "btnCleanFiles";
            this.btnCleanFiles.Size = new System.Drawing.Size(23, 23);
            this.btnCleanFiles.TabIndex = 2;
            this.btnCleanFiles.TabStop = false;
            this.toolTip.SetToolTip(this.btnCleanFiles, "Clean non-existant items");
            this.btnCleanFiles.Click += new System.EventHandler(this.btnCleanFiles_Click);
            // 
            // btnMoveDown
            // 
            this.btnMoveDown.Image = ((System.Drawing.Image)(resources.GetObject("btnMoveDown.Image")));
            this.btnMoveDown.Location = new System.Drawing.Point(5, 27);
            this.btnMoveDown.Name = "btnMoveDown";
            this.btnMoveDown.Size = new System.Drawing.Size(23, 23);
            this.btnMoveDown.TabIndex = 1;
            this.btnMoveDown.TabStop = false;
            this.toolTip.SetToolTip(this.btnMoveDown, "Move item down");
            this.btnMoveDown.Click += new System.EventHandler(this.btnMoveDown_Click);
            // 
            // btnMoveUp
            // 
            this.btnMoveUp.Image = ((System.Drawing.Image)(resources.GetObject("btnMoveUp.Image")));
            this.btnMoveUp.Location = new System.Drawing.Point(5, 4);
            this.btnMoveUp.Name = "btnMoveUp";
            this.btnMoveUp.Size = new System.Drawing.Size(23, 23);
            this.btnMoveUp.TabIndex = 0;
            this.btnMoveUp.TabStop = false;
            this.toolTip.SetToolTip(this.btnMoveUp, "Move item up");
            this.btnMoveUp.Click += new System.EventHandler(this.btnMoveUp_Click);
            // 
            // openFileDialog
            // 
            this.openFileDialog.DefaultExt = "bdsproj";
            this.openFileDialog.Filter = "Projects (*.bdsproj,*.bdsgroup,*.dpr,*.dpk)|*.bdsproj;*.bdsgroup;*" +  
                ".dpr;*.dpk|Source Files (*.cs,*.pas)|*.cs;*.pas|Web files (*.asmx" +  
                ",*.aspx,*.asax,*.config)|*.asmx;*.aspx;*.asax;*.config|Text files" +  
                " (*.txt)|*.txt|All FIles (*.*)|*.*";
            this.openFileDialog.Title = "Select file to add ...";
            // 
            // panel3
            // 
            this.panel3.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.panel3.Controls.Add(this.listBox);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(296, 209);
            this.panel3.TabIndex = 0;
            // 
            // listBox
            // 
            this.listBox.AllowDrop = true;
            this.listBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.listBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.listBox.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.listBox.IntegralHeight = false;
            this.listBox.Location = new System.Drawing.Point(0, 0);
            this.listBox.Name = "listBox";
            this.listBox.Size = new System.Drawing.Size(292, 205);
            this.listBox.TabIndex = 0;
            this.listBox.Resize += new System.EventHandler(this.listBox_Resize);
            this.listBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.listBox_MouseDown);
            this.listBox.DragOver += new System.Windows.Forms.DragEventHandler(this.listBox_DragOver);
            this.listBox.DragDrop += new System.Windows.Forms.DragEventHandler(this.listBox_DragDrop);
            this.listBox.QueryContinueDrag += new System.Windows.Forms.QueryContinueDragEventHandler(this.listBox_QueryContinueDrag);
            this.listBox.MouseUp += new System.Windows.Forms.MouseEventHandler(this.listBox_MouseUp);
            this.listBox.MouseMove += new System.Windows.Forms.MouseEventHandler(this.listBox_MouseMove);
            this.listBox.DrawItem += new System.Windows.Forms.DrawItemEventHandler(this.listBox_DrawItem);
            this.listBox.SelectedIndexChanged += new System.EventHandler(this.listBox_SelectedIndexChanged);
            // 
            // frmConfigureFavourites
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(328, 273);
            this.Controls.Add(this.panel3);
            this.Controls.Add(this.panel2);
            this.Controls.Add(this.panel1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.KeyPreview = true;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "frmConfigureFavourites";
            this.ShowInTaskbar = false;
            this.Text = "{In code}";
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.frmConfigureFavourites_KeyDown);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.frmConfigureFavourites_Closing);
            this.Load += new System.EventHandler(this.frmConfigureFavourites_Load);
            this.Deactivate += new System.EventHandler(this.frmConfigureFavourites_Deactivate);
            this.panel1.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.ResumeLayout(false);
        }
		#endregion
        #endregion Designer Generated Code


        #region Event Handlers
        private void frmConfigureFavourites_Load(object sender, System.EventArgs e)
        {
          Text = "Edit " + Constants.sFavourites + " Menu";
          MakeButtonImagesTransparent(this);

          using (Microsoft.Win32.RegistryKey r = FavUtilities.Registry() )
          {
            string s = (string)r.GetValue("Position");
            DesktopBounds = BDS.Utilities.BDSForms.StringToRect(s, DesktopBounds);
          }  
        }

        private void btnAddSeperator_Click(object sender, System.EventArgs e)
        {
          int i = listBox.SelectedIndex+1;
          listBox.Items.Insert(i, new Favourite(Constants.sSeperatorText) );
          listBox.SelectedIndex = i;
          DataChanged(null, null);
        }

        private void btnMoveUp_Click(object sender, System.EventArgs e)
        {
          int       newI = listBox.SelectedIndex-1;
          Favourite f    = (Favourite)listBox.Items[listBox.SelectedIndex];
          listBox.Items.Remove(f);
          listBox.Items.Insert(newI, f);
          listBox.SelectedIndex = newI;
          DataChanged(null, null);
        }

        private void btnMoveDown_Click(object sender, System.EventArgs e)
        {
          int       newI = listBox.SelectedIndex+1;
          Favourite f    = (Favourite)listBox.Items[listBox.SelectedIndex];
          listBox.Items.Remove(f);
          listBox.Items.Insert(newI, f);
          listBox.SelectedIndex = newI;
          DataChanged(null, null);
        }

        private void btnDeleteFile_Click(object sender, System.EventArgs e)
        {
          int i = listBox.SelectedIndex;
          listBox.Items.RemoveAt(i);

          DataChanged(null, null);

          if (i>=listBox.Items.Count)
            i = listBox.Items.Count-1;
          if (i>=0)
            listBox.SelectedIndex = i;
        }

        private void btnCleanFiles_Click(object sender, System.EventArgs e)
        {
          bool changed = false;
          int i = 0;
          while (i<listBox.Items.Count)
          {
            Favourite f = (Favourite)listBox.Items[i];
            if (f.IsSeperator)
            {
              if (  (i==0) || (i>=listBox.Items.Count-1)
                 || ((Favourite)listBox.Items[i-1]).IsSeperator )
              {
                listBox.Items.Remove(f);
                changed = true;
                i--;
              }
            }
            else if (!f.Exists)
            {
              listBox.Items.Remove(f);
              changed = true;
              i--;
            }

            i++;
          } /*while*/

          if (changed)
          {
            i = listBox.SelectedIndex;
            if (listBox.Items.Count>0)
            {
              if ( (i<0) || ( i>=listBox.Items.Count) )
                i = listBox.Items.Count-1;
              listBox.SelectedIndex = i;
            }
            DataChanged(null, null);
          }

        }

        private void frmConfigureFavourites_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
          try
          {
            string s = BDS.Utilities.BDSForms.RectToString(DesktopBounds);

            using (Microsoft.Win32.RegistryKey r = FavUtilities.Registry() )
              r.SetValue("Position", s);
          }
          catch {}
        }

        private void btnAddFile_Click(object sender, System.EventArgs e)
        {
            if (openFileDialog.ShowDialog()==DialogResult.OK)
            {
              int i = IndexOfFile(openFileDialog.FileName);
              if (i>=0)
              {
                listBox.SelectedIndex = i;
              }
              else
              {
                i = listBox.SelectedIndex+1;

                Favourite f = new Favourite(openFileDialog.FileName);
                listBox.Items.Insert(i, f);
                listBox.SelectedIndex = i;
                DataChanged(null, null);
              }
           }
        }


        private void frmConfigureFavourites_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
        {
          HandleFormLevelShortcuts(e);
          MakeFormKeypressIntoFavouriteShortcut(e);
        }

        private void listBox_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
        {
           dragStart = Point.Empty;
        }

        private Point dragStart = Point.Empty;

        private void listBox_MouseMove(object sender, System.Windows.Forms.MouseEventArgs e)
        {
          if (dragStart==Point.Empty) return;
          if(e.Button != MouseButtons.Left) return;

          Size dragSize = SystemInformation.DragSize;
          if ( Math.Abs(e.Y - dragStart.Y)*2 < dragSize.Height)
             return;

          int i = listBox.IndexFromPoint(e.X, e.Y);

          DragDropEffects dropEffect = listBox.DoDragDrop(i, DragDropEffects.Move);
          if (dropEffect == DragDropEffects.None)
          {
            listBox.SelectedIndex = i;
          }

          dragStart = Point.Empty;
        }

        private void listBox_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
        {
          int i = listBox.IndexFromPoint(e.X, e.Y);
          if ( ((e.Button & MouseButtons.Left) != 0)
               && (i>=0) && (i<listBox.Items.Count) )
            dragStart = new Point(e.X, e.Y);
          else
            dragStart = Point.Empty;
        }

        private void listBox_QueryContinueDrag(object sender, System.Windows.Forms.QueryContinueDragEventArgs e)
        {
           Point p = this.PointToClient( Control.MousePosition );
           if (!this.ClientRectangle.Contains(p))
             dragStart = Point.Empty;

           if (this.dragStart==Point.Empty)
             e.Action=DragAction.Cancel;
        }


        private void listBox_DragOver(object sender, System.Windows.Forms.DragEventArgs e)
        {
          e.Effect = DragDropEffects.None;

          if (  (e.Data.GetDataPresent(typeof(int)))
             || (e.Data.GetDataPresent(DataFormats.FileDrop)) )
          {
            int i = e.Data.GetDataPresent(typeof(int))
                  ? (int)e.Data.GetData(typeof(int))
                  : -1;

            Point p = listBox.PointToClient( new Point(e.X, e.Y));
            int hi = listBox.IndexFromPoint(p);

            if ( (hi!=i) ||(e.Data.GetDataPresent(DataFormats.FileDrop)) )
            {
              e.Effect = DragDropEffects.Move;
              if ( (hi>=0) && (hi<listBox.Items.Count) )
                listBox.SelectedIndex = hi;
            }
          } /* if GetDataPresent */

        }

        private void listBox_DragDrop(object sender, System.Windows.Forms.DragEventArgs e)
        {
          Point p = listBox.PointToClient( new Point(e.X, e.Y));
          int newRow = listBox.IndexFromPoint(p);

          if ( (newRow<0) || (newRow >= listBox.Items.Count) )
             newRow = listBox.Items.Count-1;

          if (e.Data.GetDataPresent(typeof(int)))
            MoveItem( (int)e.Data.GetData( typeof(int)),
                      newRow );
          else if (e.Data.GetDataPresent(DataFormats.FileDrop))
            DropFiles( (string[])e.Data.GetData( DataFormats.FileDrop),
                      newRow );

        }

        private void frmConfigureFavourites_Deactivate(object sender, System.EventArgs e)
        {
          dragStart = Point.Empty;
        }

        private void listBox_DrawItem(object sender, System.Windows.Forms.DrawItemEventArgs e)
        {
          e.DrawBackground();
          if (e.Index<0) return;
          
          Favourite f = (Favourite)listBox.Items[e.Index];

          //Draw filename
          Color fc;
          if ((e.State & DrawItemState.Selected) != 0)
            fc  = e.ForeColor;
          else if (f.IsSeperator)
            fc  = SystemColors.GrayText;
          else if (!f.Exists)
            fc  = Color.DarkRed;
          else
            fc  = e.ForeColor;

          Rectangle fnBounds = new Rectangle(e.Bounds.X, e.Bounds.Y,
                                             e.Bounds.Width - ShortcutDisplayWidth, e.Bounds.Height);
          using (Brush bFn = new SolidBrush(fc))
            e.Graphics.DrawString(f.Filename, e.Font, bFn, fnBounds);

          //Draw shortcut
          if (f.Shortcut!=Keys.None)
          {
            Rectangle scBounds = new Rectangle(e.Bounds.Width - ShortcutDisplayWidth, e.Bounds.Y,
                                             ShortcutDisplayWidth, e.Bounds.Height);
            using (Brush bSc = new SolidBrush(fc))
              e.Graphics.DrawString(keysConverter.ConvertToString(f.Shortcut), e.Font, bSc, scBounds);
          };


          e.DrawFocusRectangle();
        }

        private void listBox_SelectedIndexChanged(object sender, System.EventArgs e)
        {
          UpdateButtons();
        }


        private void listBox_Resize(object sender, System.EventArgs e)
        {
          listBox.Invalidate();
        }

        #endregion Event Handlers

	} /* frmConfigureFavourites */

} /*Namespace*/

