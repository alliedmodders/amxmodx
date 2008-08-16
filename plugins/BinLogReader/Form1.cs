using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.Text;

namespace BinLogReader
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class Form1 : System.Windows.Forms.Form
	{
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem menuItem3;
		private System.Windows.Forms.MenuItem menuItem4;
		private System.Windows.Forms.MenuItem menuItem5;
		private System.Windows.Forms.MenuItem menuItem6;
		private System.Windows.Forms.OpenFileDialog ofd;
		private System.Windows.Forms.MenuItem MenuFileOpen;
		private System.Windows.Forms.TabPage PluginsTab;
		private System.Windows.Forms.ListView PluginList;
		private System.Windows.Forms.TabPage LogTextTab;
		private System.Windows.Forms.RichTextBox TextLog;
		private System.Windows.Forms.TabPage LogListTab;
        private System.Windows.Forms.TabControl MainTab;
        private IContainer components;

		public Form1()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		private BinLog binlog;

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.components = new System.ComponentModel.Container();
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.MenuFileOpen = new System.Windows.Forms.MenuItem();
            this.menuItem3 = new System.Windows.Forms.MenuItem();
            this.menuItem4 = new System.Windows.Forms.MenuItem();
            this.menuItem5 = new System.Windows.Forms.MenuItem();
            this.menuItem6 = new System.Windows.Forms.MenuItem();
            this.ofd = new System.Windows.Forms.OpenFileDialog();
            this.PluginsTab = new System.Windows.Forms.TabPage();
            this.PluginList = new System.Windows.Forms.ListView();
            this.LogTextTab = new System.Windows.Forms.TabPage();
            this.TextLog = new System.Windows.Forms.RichTextBox();
            this.LogListTab = new System.Windows.Forms.TabPage();
            this.MainTab = new System.Windows.Forms.TabControl();
            this.PluginsTab.SuspendLayout();
            this.LogTextTab.SuspendLayout();
            this.MainTab.SuspendLayout();
            this.SuspendLayout();
            // 
            // mainMenu1
            // 
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItem1,
            this.menuItem5});
            // 
            // menuItem1
            // 
            this.menuItem1.Index = 0;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.MenuFileOpen,
            this.menuItem3,
            this.menuItem4});
            this.menuItem1.Text = "&File";
            // 
            // MenuFileOpen
            // 
            this.MenuFileOpen.Index = 0;
            this.MenuFileOpen.Text = "&Open";
            this.MenuFileOpen.Click += new System.EventHandler(this.MenuFileOpen_Click);
            // 
            // menuItem3
            // 
            this.menuItem3.Index = 1;
            this.menuItem3.Text = "-";
            // 
            // menuItem4
            // 
            this.menuItem4.Index = 2;
            this.menuItem4.Text = "E&xit";
            this.menuItem4.Click += new System.EventHandler(this.menuItem4_Click);
            // 
            // menuItem5
            // 
            this.menuItem5.Index = 1;
            this.menuItem5.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItem6});
            this.menuItem5.Text = "&Help";
            // 
            // menuItem6
            // 
            this.menuItem6.Index = 0;
            this.menuItem6.Text = "&About";
            // 
            // ofd
            // 
            this.ofd.Filter = "Binary Log Files|*.blg";
            // 
            // PluginsTab
            // 
            this.PluginsTab.Controls.Add(this.PluginList);
            this.PluginsTab.Location = new System.Drawing.Point(4, 22);
            this.PluginsTab.Name = "PluginsTab";
            this.PluginsTab.Size = new System.Drawing.Size(648, 346);
            this.PluginsTab.TabIndex = 0;
            this.PluginsTab.Text = "Plugins";
            // 
            // PluginList
            // 
            this.PluginList.AllowColumnReorder = true;
            this.PluginList.AutoArrange = false;
            this.PluginList.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PluginList.FullRowSelect = true;
            this.PluginList.Location = new System.Drawing.Point(0, 0);
            this.PluginList.MultiSelect = false;
            this.PluginList.Name = "PluginList";
            this.PluginList.Size = new System.Drawing.Size(648, 346);
            this.PluginList.TabIndex = 0;
            this.PluginList.UseCompatibleStateImageBehavior = false;
            this.PluginList.View = System.Windows.Forms.View.Details;
            // 
            // LogTextTab
            // 
            this.LogTextTab.Controls.Add(this.TextLog);
            this.LogTextTab.Location = new System.Drawing.Point(4, 22);
            this.LogTextTab.Name = "LogTextTab";
            this.LogTextTab.Size = new System.Drawing.Size(648, 346);
            this.LogTextTab.TabIndex = 1;
            this.LogTextTab.Text = "Event Log (Text)";
            // 
            // TextLog
            // 
            this.TextLog.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TextLog.Font = new System.Drawing.Font("Lucida Console", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.TextLog.Location = new System.Drawing.Point(0, 0);
            this.TextLog.Name = "TextLog";
            this.TextLog.Size = new System.Drawing.Size(648, 346);
            this.TextLog.TabIndex = 0;
            this.TextLog.Text = "";
            // 
            // LogListTab
            // 
            this.LogListTab.Location = new System.Drawing.Point(4, 22);
            this.LogListTab.Name = "LogListTab";
            this.LogListTab.Size = new System.Drawing.Size(648, 346);
            this.LogListTab.TabIndex = 3;
            this.LogListTab.Text = "Event Log (List)";
            // 
            // MainTab
            // 
            this.MainTab.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.MainTab.Controls.Add(this.PluginsTab);
            this.MainTab.Controls.Add(this.LogTextTab);
            this.MainTab.Controls.Add(this.LogListTab);
            this.MainTab.ItemSize = new System.Drawing.Size(46, 18);
            this.MainTab.Location = new System.Drawing.Point(8, 12);
            this.MainTab.Name = "MainTab";
            this.MainTab.SelectedIndex = 0;
            this.MainTab.Size = new System.Drawing.Size(656, 372);
            this.MainTab.TabIndex = 0;
            this.MainTab.SelectedIndexChanged += new System.EventHandler(this.MainTab_SelectedIndexChanged);
            // 
            // Form1
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(676, 393);
            this.Controls.Add(this.MainTab);
            this.Menu = this.mainMenu1;
            this.Name = "Form1";
            this.Text = "AMX Mod X BinLog Reader";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.PluginsTab.ResumeLayout(false);
            this.LogTextTab.ResumeLayout(false);
            this.MainTab.ResumeLayout(false);
            this.ResumeLayout(false);

		}
		#endregion


		public enum ViewAreas
		{
			Update_All=0,
			Update_Plugins=1,
			Update_Text=2,
		}

		private ViewAreas g_UpdateViews;

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
            Application.EnableVisualStyles();
            Application.DoEvents();
			Application.Run(new Form1());
		}

		private void Form1_Load(object sender, System.EventArgs e)
		{
			binlog = null;
			g_UpdateViews = ViewAreas.Update_All;
		}

		private void menuItem4_Click(object sender, System.EventArgs e)
		{
			Application.Exit();
		}

		private void ClearViews(ViewAreas v)
		{
			if (v == ViewAreas.Update_All || 
				((v & ViewAreas.Update_Plugins) == ViewAreas.Update_Plugins))
			{
				PluginList.Clear();	
				PluginList.Columns.Clear();
			}
			if (v == ViewAreas.Update_Text || 
				((v & ViewAreas.Update_Text) == ViewAreas.Update_Text))
			{
				TextLog.Clear();
			}
		}

		private void UpdateViews(ViewAreas v, BinLogFlags b)
		{
			ClearViews(v);

			if (v == ViewAreas.Update_All || 
				((v & ViewAreas.Update_Plugins) == ViewAreas.Update_Plugins)
				&& (binlog.GetPluginDB() != null))
			{
				PluginDb plugdb = binlog.GetPluginDB();
				PluginList.View = View.Details;
				PluginList.Columns.Add("Number", 60, HorizontalAlignment.Left);
				PluginList.Columns.Add("File", 100, HorizontalAlignment.Left);
				PluginList.Columns.Add("Title", 120, HorizontalAlignment.Left);
				PluginList.Columns.Add("Version", 60, HorizontalAlignment.Left);
				PluginList.Columns.Add("Status", 60, HorizontalAlignment.Left);

				int num = plugdb.Count;
				ListViewItem[] items = new ListViewItem[num];
				for (int i=0; i<num; i++)
				{
					Plugin pl = plugdb.GetPluginById(i);
					ListViewItem item = new ListViewItem(i.ToString());
					item.SubItems.Add(pl.File);
					item.SubItems.Add(pl.Title);
					item.SubItems.Add(pl.Version);
					item.SubItems.Add(pl.Status);
					items[i] = item;
				}
				PluginList.Items.AddRange(items);
			}
			if (v == ViewAreas.Update_Text || 
				((v & ViewAreas.Update_Text) == ViewAreas.Update_Text)
				&& (binlog != null))
			{
				ArrayList al = binlog.OpList;
				BinLogEntry ble;
				StringBuilder sb = new StringBuilder(al.Count * 10);
				BinLogFlags flags = (BinLogFlags.Show_GameTime | BinLogFlags.Show_PlugFile | BinLogFlags.Show_PlugId);
				for (int i=0; i<al.Count; i++)
				{
					ble = (BinLogEntry)al[i];
					BinLogEntry.BinLogString(sb, ble, flags);
					sb.Append("\n");
				}
				TextLog.Text = sb.ToString();
			}
		}

		private void MenuFileOpen_Click(object sender, System.EventArgs e)
		{
			DialogResult res = ofd.ShowDialog(this);

			if (res != DialogResult.OK)
			{
				return;
			}

			try
			{
				binlog = BinLog.FromFile(ofd.FileName);
				if (binlog == null)
				{
					throw new Exception("Stream failure in log file");
				}

				MainTab.SelectedTab = MainTab.TabPages[0];
				ClearViews(ViewAreas.Update_All);
				UpdateViews(ViewAreas.Update_Plugins, BinLogFlags.Show_PlugFile | BinLogFlags.Show_GameTime);
				g_UpdateViews = ViewAreas.Update_Text;
			} 
			catch (Exception ex)
			{
				string msg = "Failed to open binary log";
				if (ex.Message != null)
					msg += ": " + ex.Message;
				MessageBox.Show(this, 
					msg, 
					"AMX Mod X Binary Log Reader", 
					MessageBoxButtons.OK, 
					MessageBoxIcon.Error);
			}
		}

		private void MainTab_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			if ((g_UpdateViews & ViewAreas.Update_Text) == ViewAreas.Update_Text)
			{
				UpdateViews(ViewAreas.Update_Text, BinLogFlags.Show_PlugFile|BinLogFlags.Show_GameTime);
				g_UpdateViews &= ~(ViewAreas.Update_Text);	
			}		
		}
	}
}
