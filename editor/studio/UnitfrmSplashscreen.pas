unit UnitfrmSplashscreen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, SciLexerMemo, JvInspector,
  UnitfrmMain, UnitfrmSettings, UnitfrmSelectColor, UnitfrmSearch,
  UnitfrmReplace, UnitfrmAllFilesForm, UnitfrmGoToLine,
  UnitfrmPluginsIniEditor, UnitfrmSocketsTerminal, UnitfrmInfo, TBX,
  TB2Item, SpTBXItem, Dialogs, menus, SciKeyBindings;

type
  TfrmSplashscreen = class(TForm)
    imgSplashscreen: TImage;
    lblStudio: TLabel;
    tmrHide: TTimer;
    procedure FormShow(Sender: TObject);
    procedure tmrHideTimer(Sender: TObject);
  public
    procedure OnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
  end;

var
  frmSplashscreen: TfrmSplashscreen;

implementation

uses UnitCodeExplorerUpdater, UnitCodeSnippets, UnitCodeUtils,
  UnitLanguages, UnitMainTools, UnitReadThread, UnitfrmHudMsgGenerator,
  UnitfrmAutoIndent, UnitfrmHTMLPreview, UnitCodeInspector, UnitPlugins,
  UnitfrmMenuGenerator, UnitfrmMOTDGen, UnitfrmClose, UnitfrmConnGen,
  UnitfrmIRCPaster;


{$R *.DFM}

procedure TfrmSplashscreen.FormShow(Sender: TObject);
var eCache: TStringList;
    i: integer;
    eExt: String;
begin
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmMain, frmMain);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmAutoIndent, frmAutoIndent);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmSelectColor, frmSelectColor);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmInfo, frmInfo);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmSearch, frmSearch);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmReplace, frmReplace);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmAllFilesForm, frmAllFilesForm);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmGoToLine, frmGoToLine);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmPluginsIniEditor, frmPluginsIniEditor);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmSocketsTerminal, frmSocketsTerminal);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmHudMsgGenerator, frmHudMsgGenerator);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmMenuGenerator, frmMenuGenerator);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmMOTDGen, frmMOTDGen);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmClose, frmClose);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmConnGen, frmConnGen);
  Application.ProcessMessages;
  Repaint;
  Application.CreateForm(TfrmIRCPaster, frmIRCPaster);
  Application.ProcessMessages;
  Repaint;

  if IEInstalled then begin
    Application.CreateForm(TfrmHTMLPreview, frmHTMLPreview);
    Application.ProcessMessages;
    Repaint;
  end
  else
    frmMain.mnuMOTDGenerator.Enabled := False;

  Application.OnMessage := OnMessage;
  Application.OnShortCut := OnShortCut;

  with frmMain do begin
    sciPropertyLoader.FileName := ExtractFilePath(ParamStr(0)) + 'config\Editor.sci';
    if FileExists(sciPropertyLoader.FileName) then
      sciPropertyLoader.Load
    else
      sciPropertyLoader.Save; // create new if it doesnt exist...

    sciEditor.Gutter1.Width := 40;
    sciEditor.Gutter1.MarginType := gutLineNumber;
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
    if PAWNProjects.Count > 1 then begin
      PAWNProjects.Close(0);
      i := 1;
    end;
    if CPPProjects.Count > 1 then begin
      CPPProjects.Close(0);
      i := 1;
    end;
    if OtherProjects.Count > 1 then begin
      OtherProjects.Close(0);
      i := 1;
    end;

    if i = 1 then begin
      ActivateProjects(0, False); // Started := True is already set here
      PAWNProjects.Activate(PAWNProjects.Count -1, False, False);
    end;
    UpdateCI;
    LoadPlugins;
  end;

  tmrHide.Enabled := True;
end;

procedure TfrmSplashscreen.OnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  Handled := not Plugin_AppMsg(Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam, Msg.time, Msg.pt);
end;

procedure TfrmSplashscreen.OnShortCut(var Msg: TWMKey;
  var Handled: Boolean);
function TriggerMenuShortcut(eShortcut: TShortcut; Item: TTBCustomItem): Boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to Item.Count -1 do begin
    if Item.Items[i].ShortCut = eShortcut then begin
      Item.Items[i].OnClick(Self);
      Result := True;
      exit;
    end
    else
      TriggerMenuShortcut(eShortcut, Item.Items[i]);
  end;
end;

var i: integer;
    eShortcut: TShortcut;
begin
  if not Started then exit;

  // Check frmSettings shortcut
  if (frmSettings.Visible) and (frmSettings.txtShortcut.Focused) then begin
    if (Msg.CharCode = VK_CONTROL) or (Msg.CharCode = VK_MENU) or (Msg.CharCode = VK_SHIFT) then begin
      frmSettings.txtShortcut.Clear;
      if ssShift in KeyDataToShiftState(Msg.KeyData) then
        frmSettings.txtShortcut.Text := frmSettings.txtShortcut.Text + 'Shift+';
      if ssCtrl in KeyDataToShiftState(Msg.KeyData) then
        frmSettings.txtShortcut.Text := frmSettings.txtShortcut.Text + 'Ctrl+';
      if ssAlt in KeyDataToShiftState(Msg.KeyData) then
        frmSettings.txtShortcut.Text := frmSettings.txtShortcut.Text + 'Alt+';
    end
    else
      frmSettings.txtShortcut.Text := ShortcutToText(Shortcut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData)));
    Handled := True;
  end;

  if GetForegroundWindow <> frmMain.Handle then exit;
  
  // stop IRC Paster if escape is pressed
  if (Msg.CharCode = VK_ESCAPE) then begin
    frmMain.IRCPasterStop := True;
    if frmMain.sciEditor.CallTipActive then
      frmMain.sciEditor.CallTipCancel;
    if frmMain.sciEditor.AutoCActive then
      frmMain.sciEditor.AutoCCancel;
    exit;
  end;

  eShortcut := Shortcut(Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
  // Some menu commands are suppressed by the controlchars thingy, so they will be triggered manually
  for i := 0 to frmMain.tbxMenu.Items.Count -1 do begin
    if TriggerMenuShortcut(eShortcut, frmMain.tbxMenu.Items[i]) then begin
      Handled := True;
      exit;
    end;
  end;
  for i := 0 to frmMain.tbxToolbar.Items.Count -1 do begin
    if frmMain.tbxToolbar.Items[i].ShortCut = eShortcut then begin
      Handled := True;
      frmMain.tbxToolbar.Items[i].OnClick(Self);
      exit;
    end;
  end;
  for i := 0 to frmMain.tbxEdit.Items.Count -1 do begin
    if frmMain.tbxEdit.Items[i].ShortCut = eShortcut then begin
      Handled := True;
      frmMain.tbxEdit.Items[i].OnClick(Self);
      exit;
    end;
  end;
  Application.ProcessMessages;
  // Control chars
  if (eShortcut = Shortcut(Ord('E'), [ssCtrl])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('H'), [ssCtrl])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('K'), [ssCtrl])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('S'), [ssCtrl])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('B'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('C'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('D'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('E'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('F'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('G'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('H'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('K'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('N'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('O'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('P'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('Q'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('R'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('V'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('W'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('X'), [ssCtrl, ssShift])) then
    Handled := True;
  if (eShortcut = Shortcut(Ord('Y'), [ssCtrl, ssShift])) then
    Handled := True;

  if Handled then begin
    for i := 0 to frmMain.sciEditor.KeyCommands.Count -1 do begin
      if TSciKeyCommand(frmMain.sciEditor.KeyCommands.Items[i]).ShortCut = eShortcut then
        Handled := False;
    end;
  end;
end;

procedure TfrmSplashscreen.tmrHideTimer(Sender: TObject);
begin
  Hide;
  frmMain.Show;

  tmrHide.Enabled := False;
end;

end.
