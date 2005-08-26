program AMXX_Studio;

uses
  Forms,
  Windows,
  Messages,
  UnitfrmMain in 'UnitfrmMain.pas' {frmMain},
  UnitMainTools in 'UnitMainTools.pas',
  UnitfrmSettings in 'UnitfrmSettings.pas' {frmSettings},
  UnitLanguages in 'UnitLanguages.pas',
  UnitfrmSelectColor in 'UnitfrmSelectColor.pas' {frmSelectColor},
  UnitfrmInfo in 'UnitfrmInfo.pas' {frmInfo},
  UnitCodeSnippets in 'UnitCodeSnippets.pas',
  UnitCodeUtils in 'UnitCodeUtils.pas',
  UnitfrmSplashscreen in 'UnitfrmSplashscreen.pas' {frmSplashscreen},
  UnitfrmSearch in 'UnitfrmSearch.pas' {frmSearch},
  UnitfrmReplace in 'UnitfrmReplace.pas' {frmReplace},
  UnitfrmAllFilesForm in 'UnitfrmAllFilesForm.pas' {frmAllFilesForm},
  UnitfrmGoToLine in 'UnitfrmGoToLine.pas' {frmGoToLine},
  UnitfrmPluginsIniEditor in 'UnitfrmPluginsIniEditor.pas' {frmPluginsIniEditor},
  UnitfrmSocketsTerminal in 'UnitfrmSocketsTerminal.pas' {frmSocketsTerminal},
  UnitReadThread in 'UnitReadThread.pas',
  UnitCodeExplorerUpdater in 'UnitCodeExplorerUpdater.pas',
  UnitTextAnalyze in 'UnitTextAnalyze.pas',
  UnitfrmHudMsgGenerator in 'UnitfrmHudMsgGenerator.pas' {frmHudMsgGenerator},
  UnitCompile in 'UnitCompile.pas',
  UnitfrmAutoIndent in 'UnitfrmAutoIndent.pas' {frmAutoIndent},
  UnitfrmHTMLPreview in 'UnitfrmHTMLPreview.pas' {frmHTMLPreview},
  UnitfrmMenuGenerator in 'UnitfrmMenuGenerator.pas' {frmMenuGenerator},
  UnitCodeInspector in 'UnitCodeInspector.pas',
  UnitfrmMOTDGen in 'UnitfrmMOTDGen.pas' {frmMOTDGen},
  UnitMenuGenerators in 'UnitMenuGenerators.pas',
  UnitfrmClose in 'UnitfrmClose.pas' {frmClose},
  UnitfrmConnGen in 'UnitfrmConnGen.pas' {frmConnGen},
  UnitPlugins in 'UnitPlugins.pas';

{ Used components:
  - JVCL 3.0
  - FlatPack
  - FlatStyle
  - Toolbar2000, TBX, SpTBX, mbTBX Lib
  - Scintilla and DelphiSci
  - madExcept
}

{$R *.res}

var i: integer;
begin
  if FindWindow(nil, 'AMXX-Studio') <> 0 then begin
    if ParamCount > 0 then begin
      for i := 1 to ParamCount  do
        SendToMainApp('LoadFile' + ParamStr(i));
    end;
    //exit;
  end;
  Application.Initialize;
  Application.Title := 'AMXX-Studio';
  Application.CreateForm(TfrmSplashscreen, frmSplashscreen);
  Application.Run;
end.
