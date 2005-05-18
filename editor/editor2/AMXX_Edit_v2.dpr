program AMXX_Edit_v2;

{
  AMXX-Edit v2

  Editor for AMX Mod X scripts, coded by Basic-Master
  © by AMX Mod X Development Team

  This application uses the following components:

  DelphiSci: delphisci.sourceforge.net (based on Scintilla library: scintilla.sourceforge.net)
  FlatStyle by Maik Porkert (found on www.torry.net)
  GlyFX Icons: www.glyfx.com (using GlyFX Icon Pack of Delphi 2005 PE)
  Modified CorelButton (see CorelButton.pas, original by ConquerWare)
  Indy 9 Socket Components: www.indyproject.org

  AMXX-Edit v2 is published under GNU General Public License and comes
  with ABSOLUTELY NO WARRANTY (see GPL.txt for more information)
}


uses
  Forms,
  Windows,
  Classes,
  SysUtils,
  UnitfrmMain in 'UnitfrmMain.pas' {frmMain},
  UnitfrmOptions in 'UnitfrmOptions.pas' {frmSettings},
  UnitfrmDebug in 'UnitfrmDebug.pas' {frmDebug},
  UnitFunc in 'UnitFunc.pas',
  UnitfrmAbout in 'UnitfrmAbout.pas' {frmAbout},
  UnitfrmGoToLine in 'UnitfrmGoToLine.pas' {frmGoToLine},
  UnitfrmSaveDialog in 'UnitfrmSaveDialog.pas' {frmSaveDialog},
  UnitTextAnalyze in 'UnitTextAnalyze.pas',
  UnitfrmMenuMaker in 'UnitfrmMenuMaker.pas' {frmMenuMaker},
  UnitAddMenu in 'UnitAddMenu.pas',
  UnitfrmSelectMenu in 'UnitfrmSelectMenu.pas' {frmSelectMenu},
  UnitHowToMakePlayerMenu in 'UnitHowToMakePlayerMenu.pas' {frmHowToMakePlayerMenu},
  UnitfrmSockets in 'UnitfrmSockets.pas' {frmSocketTerminal},
  UnitReadThread in 'UnitReadThread.pas',
  UnitfrmLoopGenerator in 'UnitfrmLoopGenerator.pas' {frmLoopGenerator};

{$R *.res}

var i: integer;
begin
  if (FindWindow('TfrmMain', 'AMXX-Edit v2') <> 0) then begin // Don't allow 2 starts...
    for i := 1 to ParamCount do begin
      if FileExists(ParamStr(i)) then
        SendOpen(ParamStr(i));         // ... and send open message to the other app
    end;
    ShowWindow(FindWindow('TfrmMain', 'AMXX-Edit v2'), SW_SHOW);
    SetForegroundWindow(FindWindow('TfrmMain', 'AMXX-Edit v2'));
    exit;
  end;
  Application.Initialize;
  Application.Title := 'AMXX-Edit v2';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmGoToLine, frmGoToLine);
  Application.CreateForm(TfrmSaveDialog, frmSaveDialog);
  Application.CreateForm(TfrmMenuMaker, frmMenuMaker);
  Application.CreateForm(TfrmSelectMenu, frmSelectMenu);
  Application.CreateForm(TfrmHowToMakePlayerMenu, frmHowToMakePlayerMenu);
  Application.CreateForm(TfrmSocketTerminal, frmSocketTerminal);
  Application.CreateForm(TfrmLoopGenerator, frmLoopGenerator);
  Load;
  Apply;
  Application.Run;
end.
