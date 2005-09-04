unit UnitfrmSplashscreen;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, SciLexerMemo, JvInspector,
  UnitfrmMain, UnitfrmSettings, UnitfrmSelectColor, UnitfrmSearch,
  UnitfrmReplace, UnitfrmAllFilesForm, UnitfrmGoToLine,
  UnitfrmPluginsIniEditor, UnitfrmSocketsTerminal, UnitfrmInfo, TBX,
  TB2Item, SpTBXItem, Dialogs;

type
  TfrmSplashscreen = class(TForm)
    imgSplashscreen: TImage;
    lblStudio: TLabel;
    tmrHide: TTimer;
    procedure FormShow(Sender: TObject);
    procedure tmrHideTimer(Sender: TObject);
  public
    procedure OnMessage(var Msg: TMsg; var Handled: Boolean);
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
        if (eExt = '.sma') or (eExt = '.inc') then // PAWN files
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

procedure TfrmSplashscreen.tmrHideTimer(Sender: TObject);
begin
  Hide;
  frmMain.Show;

  tmrHide.Enabled := False;
end;

end.
