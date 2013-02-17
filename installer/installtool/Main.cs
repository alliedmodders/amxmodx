using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace installtool
{
    public partial class MainWindow : Form
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void MainWindow_Load(object sender, EventArgs e)
        {
            this.Text = "AMX Mod X " + Program.VersionString + " Setup Wizard";
            welcomeLabel_.Text = "Welcome to the AMX Mod X " + Program.VersionString + " Setup Wizard";
            welcomeText_.Text = welcomeText_.Text.Replace("{Version}", Program.VersionString);
        }

        private void closeButton__Click(object sender, EventArgs e)
        {
            Application.Exit();
        }
    }
}
