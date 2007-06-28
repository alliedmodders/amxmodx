program AMXInstaller;

{ AMXX Installer for AMX Mod X
  by the AMXX Development Team

  Used components:

  - Indy 9 (www.indyproject.org)
  - FlatStyle Components (www.torry.net)
  - mxFlatPack Component Pack (http://www.maxcomponents.net/index.php?id=2&page=1)
  - JVCL Lib Pack 3.0 (jvcl.sourceforge.net)

  AMXX Installer for AMX Mod X is developed under GNU Public License
  and comes WITH ABSOLUTELY NO WARRANTY!
}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  UnitfrmMain in 'UnitfrmMain.pas' {frmMain},
  UnitFunctions in 'UnitFunctions.pas',
  UnitScanMods in 'UnitScanMods.pas',
  UnitfrmProxy in 'UnitfrmProxy.pas' {frmProxy},
  UnitInstall in 'UnitInstall.pas',
  UnitSelectModPath in 'UnitSelectModPath.pas' {frmSelectModPath};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'AMX Mod X Installer';
  Application.CreateForm(TfrmMain, frmMain);
  frmMain.lblWelcome.Caption := 'Welcome to the AMX Mod X Installer ' + VERSION + ' Setup Wizard';
  frmMain.lblInfo1.Caption := 'This wizard will guide you through the installation of AMX Mod X ' + VERSION + '.';
  frmMain.lblSubTitle1.Caption := 'Please review the following license terms before installing AMX Mod X ' + VERSION + '.';
  frmMain.lblSelectModInfo.Caption := 'Please select the mod AMX Mod X ' + VERSION + ' shall be installed to.';
  frmMain.lblTitle3.Caption := 'Installing AMX Mod X ' + VERSION + ' via FTP';
  frmMain.lblTitle5.Caption := 'Installing AMX Mod X ' + VERSION;
  frmMain.lblSubTitle5.Caption := 'Please wait while AMX Mod X ' + VERSION + ' is being installed.';
  Application.CreateForm(TfrmProxy, frmProxy);
  Application.CreateForm(TfrmSelectModPath, frmSelectModPath);
  Application.Run;
end.
