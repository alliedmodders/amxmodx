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
    /// Interaction logic for LicenseAccept.xaml
    /// </summary>
    public partial class LicenseAccept : UserControl
    {
        public bool Accepted { get; private set; }

        public static readonly RoutedEvent AgreementStateChangedEvent =
            EventManager.RegisterRoutedEvent("AgreementStateChanged",
            RoutingStrategy.Bubble, typeof(RoutedEventHandler), typeof(LicenseAccept));

        public LicenseAccept()
        {
            InitializeComponent();
            license_.Text = license_.Text.Replace("&#10;&#13;", "");
        }

        public event RoutedEventHandler AgreementStateChanged
        {
            add { AddHandler(AgreementStateChangedEvent, value); }
            remove { RemoveHandler(AgreementStateChangedEvent, value); }
        }

        private void agreeOption_Checked(object sender, RoutedEventArgs e)
        {
            RoutedEventArgs args = new RoutedEventArgs(AgreementStateChangedEvent);

            Accepted = true;
            RaiseEvent(args);
        }

        private void disagreeOption_Checked(object sender, RoutedEventArgs e)
        {
            RoutedEventArgs args = new RoutedEventArgs(AgreementStateChangedEvent);

            Accepted = false;
            RaiseEvent(args);
        }
    }
}
