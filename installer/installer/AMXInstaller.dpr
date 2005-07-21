program AMXInstaller;

{ AMXX Installer 1.1
  by the AMXX Development Team

  Used components:

  - Indy 9 (www.indyproject.org)
  - FlatStyle Components (www.torry.net)
  - FlatPack Component Pack (www.torry.net)
  - JVCL Lib Pack 3.0 (jvcl.sourceforge.net)

  AMXX Installer 1.1 is developed under GNU Public License
  and comes WITH ABSOLUTELY NO WARRANTY!
}

uses
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
  Application.CreateForm(TfrmProxy, frmProxy);
  Application.CreateForm(TfrmSelectModPath, frmSelectModPath);
  Application.Run;
end.
