program AMXX_Studio;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  Windows,
  Classes,
  Messages,
  SysUtils,
  SciLexerMemo,
  JvInspector,
  UnitfrmMain in 'UnitfrmMain.pas' {frmMain},
  UnitMainTools in 'UnitMainTools.pas',
  UnitfrmSettings in 'UnitfrmSettings.pas' {frmSettings},
  UnitLanguages in 'UnitLanguages.pas',
  UnitfrmSelectColor in 'UnitfrmSelectColor.pas' {frmSelectColor},
  UnitfrmInfo in 'UnitfrmInfo.pas' {frmInfo},
  UnitCodeSnippets in 'UnitCodeSnippets.pas',
  UnitCodeUtils in 'UnitCodeUtils.pas',
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
  UnitPlugins in 'UnitPlugins.pas',
  UnitfrmIRCPaster in 'UnitfrmIRCPaster.pas' {frmIRCPaster},
  MyEditFileClasses in 'MyEditFileClasses.pas',
  UnitfrmParamEdit in 'UnitfrmParamEdit.pas' {frmParamEdit},
  UnitACCheck in 'UnitACCheck.pas';

{ Used components:
  - JVCL 3.0
  - FlatPack
  - FlatStyle
  - Toolbar2000, TBX, SpTBX, mbTBX Lib
  - Scintilla and DelphiSci
}

{$R *.res}

var eCache: TStringList;
    i: integer;
    eExt: String;
begin
  if (FindWindow('TfrmMain', 'AMXX-Studio') <> 0) and (FindWindow(nil, 'Delphi 7') = 0) then begin
    if ParamCount > 0 then begin
      for i := 1 to ParamCount  do
        SendStudioMsg(SCM_LOADFILE, ParamStr(i), 0);
    end;
    SetForegroundWindow(FindWindow('TfrmMain', 'AMXX-Studio'));
    exit;
  end;

  Application.Initialize;
  Application.Title := 'AMXX-Studio';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmAutoIndent, frmAutoIndent);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmParamEdit, frmParamEdit);
  Application.OnMessage := frmMain.OnMessage;
  Application.OnShortCut := frmMain.OnShortCut;
  frmMain.sciEditor.Lines[5] := '#define PLUGIN "' + frmSettings.txtDefaultName.Text + '"';
  frmMain.sciEditor.Lines[6] := '#define VERSION "' + frmSettings.txtDefaultVersion.Text + '"';
  frmMain.sciEditor.Lines[7] := '#define AUTHOR "' + frmSettings.txtDefaultAuthor.Text + '"';

  frmMain.sciPropertyLoader.FileName := ExtractFilePath(ParamStr(0)) + 'config\Editor.sci';
  if FileExists(frmMain.sciPropertyLoader.FileName) then
    frmMain.sciPropertyLoader.Load
  else
    frmMain.sciPropertyLoader.Save; // create new if it doesnt exist...

  frmMain.sciEditor.Gutter1.Width := 40;
  frmMain.sciEditor.Gutter1.MarginType := gutLineNumber;
  LoadCodeSnippets('Pawn');
  ResetToEnglish;
  TJvCustomInspectorData.ItemRegister.Add(TJvInspectorTypeInfoRegItem.Create(TJvInspectorSelectionTextListItem, TypeInfo(TSelectionTextList)));

  eCache := TStringList.Create;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'config\Cache.cfg') then
    eCache.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\Cache.cfg');
  for i := 1 to ParamCount do begin
    if eCache.IndexOf(ParamStr(i)) = -1 then
      eCache.Add(ParamStr(i));
  end;

  for i := 0 to eCache.Count -1 do begin
    if FileExists(eCache[i]) then begin
      eExt := ExtractFileExt(eCache[i]);
      eExt := LowerCase(eExt);
      if (eExt = '.sma') or (eExt = '.inc') or (eExt = '.inl') then // Pawn files
        PAWNProjects.Open(eCache[i])
      else if (eExt = '.cpp') or (eExt = '.h') then // C++ files
        CPPProjects.Open(eCache[i])
      else if (eExt = '.htm') or (eExt = '.html') then // HTML files
        OtherProjects.Open(eCache[i], 'HTML')
      else if (eExt = '.sql') then // SQL databases
        OtherProjects.Open(eCache[i], 'SQL')
      else if (eExt = '.xml') then // XML files
        OtherProjects.Open(eCache[i], 'XML')
      else // Other files and/or Textfiles
        OtherProjects.Open(eCache[i], 'null');
    end;
  end;
  eCache.Free;

  i := 0;
  if PawnProjects.Count > 1 then begin
    PawnProjects.Close(0, True);
    i := 1;
  end;
  if CPPProjects.Count > 1 then begin
    CPPProjects.Close(0, True);
    i := 1;
  end;
  if OtherProjects.Count > 1 then begin
    OtherProjects.Close(0, True);
    i := 1;
  end;

  if i = 1 then begin
    ActivateProjects(0, False); // Started := True is already set here
    PAWNProjects.Activate(PawnProjects.Count -1, False, False);
  end;
  UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);

  Application.CreateForm(TfrmSelectColor, frmSelectColor);
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.CreateForm(TfrmReplace, frmReplace);
  Application.CreateForm(TfrmAllFilesForm, frmAllFilesForm);
  Application.CreateForm(TfrmGoToLine, frmGoToLine);
  Application.CreateForm(TfrmPluginsIniEditor, frmPluginsIniEditor);
  Application.CreateForm(TfrmSocketsTerminal, frmSocketsTerminal);
  Application.CreateForm(TfrmHudMsgGenerator, frmHudMsgGenerator);
  Application.CreateForm(TfrmMenuGenerator, frmMenuGenerator);
  Application.CreateForm(TfrmMOTDGen, frmMOTDGen);
  Application.CreateForm(TfrmClose, frmClose);
  Application.CreateForm(TfrmConnGen, frmConnGen);
  Application.CreateForm(TfrmIRCPaster, frmIRCPaster);
  if IEInstalled then
    Application.CreateForm(TfrmHTMLPreview, frmHTMLPreview)
  else
    frmMain.mnuMOTDGenerator.Enabled := False;
  LoadPlugins;
  Application.Run;
end.
