namespace FormsDsg
{
    partial class TForm1Dsg
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
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
            this.BtnClickMe = new System.Windows.Forms.Button();
            this.BtnClear = new System.Windows.Forms.Button();
            this.BtnClose = new System.Windows.Forms.Button();
            this.ListEvents = new System.Windows.Forms.ListBox();
            this.SuspendLayout();
            // 
            // BtnClickMe
            // 
            this.BtnClickMe.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnClickMe.Location = new System.Drawing.Point(3, 6);
            this.BtnClickMe.Margin = new System.Windows.Forms.Padding(2);
            this.BtnClickMe.Name = "BtnClickMe";
            this.BtnClickMe.Size = new System.Drawing.Size(75, 23);
            this.BtnClickMe.TabIndex = 0;
            this.BtnClickMe.Text = "Click Me!";
            this.BtnClickMe.UseVisualStyleBackColor = true;
            // 
            // BtnClear
            // 
            this.BtnClear.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnClear.Location = new System.Drawing.Point(81, 6);
            this.BtnClear.Margin = new System.Windows.Forms.Padding(2);
            this.BtnClear.Name = "BtnClear";
            this.BtnClear.Size = new System.Drawing.Size(75, 23);
            this.BtnClear.TabIndex = 1;
            this.BtnClear.Text = "Clear";
            this.BtnClear.UseVisualStyleBackColor = true;
            // 
            // BtnClose
            // 
            this.BtnClose.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.BtnClose.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.BtnClose.Location = new System.Drawing.Point(271, 6);
            this.BtnClose.Margin = new System.Windows.Forms.Padding(2);
            this.BtnClose.Name = "BtnClose";
            this.BtnClose.Size = new System.Drawing.Size(75, 23);
            this.BtnClose.TabIndex = 2;
            this.BtnClose.Text = "Close";
            this.BtnClose.UseVisualStyleBackColor = true;
            // 
            // ListEvents
            // 
            this.ListEvents.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.ListEvents.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.ListEvents.FormattingEnabled = true;
            this.ListEvents.IntegralHeight = false;
            this.ListEvents.Location = new System.Drawing.Point(3, 33);
            this.ListEvents.Margin = new System.Windows.Forms.Padding(2);
            this.ListEvents.Name = "ListEvents";
            this.ListEvents.Size = new System.Drawing.Size(343, 121);
            this.ListEvents.TabIndex = 3;
            // 
            // TForm1Dsg
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(349, 159);
            this.Controls.Add(this.ListEvents);
            this.Controls.Add(this.BtnClose);
            this.Controls.Add(this.BtnClear);
            this.Controls.Add(this.BtnClickMe);
            this.MaximizeBox = false;
            this.Name = "TForm1Dsg";
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
            this.Text = "Form1";
            this.ResumeLayout(false);

        }

        #endregion

        protected System.Windows.Forms.Button BtnClickMe;
        protected System.Windows.Forms.Button BtnClear;
        protected System.Windows.Forms.Button BtnClose;
        protected System.Windows.Forms.ListBox ListEvents;

    }
}