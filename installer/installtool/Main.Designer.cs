namespace installtool
{
    partial class MainWindow
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainWindow));
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.welcomeLabel_ = new System.Windows.Forms.Label();
            this.welcomeText_ = new System.Windows.Forms.Label();
            this.closeButton_ = new System.Windows.Forms.Button();
            this.nextButton_ = new System.Windows.Forms.Button();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.Location = new System.Drawing.Point(0, 0);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(164, 314);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.pictureBox1.TabIndex = 0;
            this.pictureBox1.TabStop = false;
            // 
            // panel1
            // 
            this.panel1.BackColor = System.Drawing.SystemColors.ControlLightLight;
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.welcomeText_);
            this.panel1.Controls.Add(this.welcomeLabel_);
            this.panel1.Controls.Add(this.pictureBox1);
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Margin = new System.Windows.Forms.Padding(0);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(560, 315);
            this.panel1.TabIndex = 1;
            // 
            // welcomeLabel_
            // 
            this.welcomeLabel_.Font = new System.Drawing.Font("Segoe UI Semibold", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.welcomeLabel_.Location = new System.Drawing.Point(170, 3);
            this.welcomeLabel_.Name = "welcomeLabel_";
            this.welcomeLabel_.Size = new System.Drawing.Size(380, 103);
            this.welcomeLabel_.TabIndex = 2;
            this.welcomeLabel_.Text = "Welcome to the AMX Mod X 1.8.3 Setup Wizard";
            // 
            // welcomeText_
            // 
            this.welcomeText_.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.welcomeText_.Location = new System.Drawing.Point(172, 87);
            this.welcomeText_.Name = "welcomeText_";
            this.welcomeText_.Size = new System.Drawing.Size(378, 211);
            this.welcomeText_.TabIndex = 3;
            this.welcomeText_.Text = resources.GetString("welcomeText_.Text");
            // 
            // closeButton_
            // 
            this.closeButton_.Location = new System.Drawing.Point(12, 321);
            this.closeButton_.Name = "closeButton_";
            this.closeButton_.Size = new System.Drawing.Size(88, 30);
            this.closeButton_.TabIndex = 6;
            this.closeButton_.Text = "&Close";
            this.closeButton_.UseVisualStyleBackColor = true;
            this.closeButton_.Click += new System.EventHandler(this.closeButton__Click);
            // 
            // nextButton_
            // 
            this.nextButton_.Location = new System.Drawing.Point(454, 321);
            this.nextButton_.Name = "nextButton_";
            this.nextButton_.Size = new System.Drawing.Size(88, 30);
            this.nextButton_.TabIndex = 5;
            this.nextButton_.Text = "&Next";
            this.nextButton_.UseVisualStyleBackColor = true;
            // 
            // MainWindow
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(554, 358);
            this.Controls.Add(this.nextButton_);
            this.Controls.Add(this.closeButton_);
            this.Controls.Add(this.panel1);
            this.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "MainWindow";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Form1";
            this.Load += new System.EventHandler(this.MainWindow_Load);
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label welcomeLabel_;
        private System.Windows.Forms.Label welcomeText_;
        private System.Windows.Forms.Button closeButton_;
        private System.Windows.Forms.Button nextButton_;
    }
}

