using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace installtool
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private UIElement currentPanel_;
        private LicenseAccept licensePanel_;


        public MainWindow()
        {
            InitializeComponent();
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            welcomeLabel_.Text = welcomeLabel_.Text.Replace("{Version}", Program.VersionString);
            welcomeText_.Text = welcomeText_.Text.Replace("{Version}", Program.VersionString);
            currentPanel_ = welcomePanel_;
        }

        private void closeButton__Click(object sender, RoutedEventArgs e)
        {
            Application.Current.Shutdown();
        }

        private void swap(UIElement to)
        {
            currentPanel_.Visibility = Visibility.Hidden;
            to.Visibility = Visibility.Visible;
            currentPanel_ = to;
        }

        private void backButton__Click(object sender, RoutedEventArgs e)
        {
            if (currentPanel_ == licensePanel_)
            {
                swap(welcomePanel_);
            }

            backButton_.IsEnabled = currentPanel_ != welcomePanel_;
            nextButton_.IsEnabled = currentPanel_ != licensePanel_;
        }

        private void nextButton__Click(object sender, RoutedEventArgs e)
        {
            if (licensePanel_ == null || currentPanel_ == welcomePanel_)
            {
                if (licensePanel_ == null)
                {
                    licensePanel_ = new LicenseAccept();
                    contentPanel_.Children.Add(licensePanel_);
                    licensePanel_.AgreementStateChanged += new RoutedEventHandler(licensePanel__AgreementStateChanged);
                }
                nextButton_.IsEnabled = licensePanel_.Accepted;
                swap(licensePanel_);
            }

            backButton_.IsEnabled = true;
        }

        void licensePanel__AgreementStateChanged(object sender, RoutedEventArgs e)
        {
            nextButton_.IsEnabled = licensePanel_.Accepted;
        }
    }
}
