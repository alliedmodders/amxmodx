unit UnitfrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  ToolWin, ActnMan, ActnCtrls, ActnMenus, ActnList,
  ImgList, ComCtrls, SciDocuments, SciLexer,
  SciLexerMod, SciAutoComplete, SciCallTips, SciLexerOptionsDlg,
  SciSearchReplace, StdCtrls, Tabs, Menus, ShellAPI, ScintillaLanguageManager,
  SciLexerMemo, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdFTP, ExtCtrls, TFlatHintUnit, Dialogs, XPStyleActnCtrls, CorelButton;

type
  TfrmMain = class(TForm)
    atbToolBar: TActionToolBar;
    ilMenu: TImageList;
    amMenu: TActionManager;
    acNew: TAction;
    acSave: TAction;
    acSaveAs: TAction;
    acOpen: TAction;
    acClose: TAction;
    acCloseAllFiles: TAction;
    acExit: TAction;
    acUndo: TAction;
    acRedo: TAction;
    acCut: TAction;
    acCopy: TAction;
    acPaste: TAction;
    acSearch: TAction;
    acFindNext: TAction;
    acReplace: TAction;
    acCompile: TAction;
    acCompileAndUpload: TAction;
    acDoc: TAction;
    acForum: TAction;
    acAbout: TAction;
    mmbMenu: TActionMainMenuBar;
    acCompileAndStart: TAction;
    acOptions: TAction;
    sciEditor: TScintilla;
    dtcEditor: TSciDocumentTabControl;
    sacComplete: TSciAutoComplete;
    cltEditor: TSciCallTips;
    srpSearch: TSciSearchReplace;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    ppmEditor: TPopupMenu;
    mnuClose: TMenuItem;
    acGoTo: TAction;
    IdFTP: TIdFTP;
    sbInfo: TStatusBar;
    acSaveAll: TAction;
    pnlFunctions: TPanel;
    pnlSpacerTop: TPanel;
    trvFunctions: TTreeView;
    pnlSpacerLeft: TPanel;
    splFunctions: TSplitter;
    pnlSpacerBottom: TPanel;
    acEdit: TAction;
    lvDebug: TListView;
    acIdenter: TAction;
    acMenuMaker: TAction;
    acPMM: TAction;
    acRemoveMissingPlugins: TAction;
    acSocketTerminal: TAction;
    acSelectAll: TAction;
    acUnidenter: TAction;
    acLoopGenerator: TAction;
    procedure FormCreate(Sender: TObject);
    procedure acNewExecute(Sender: TObject);
    procedure acOpenExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acSaveAsExecute(Sender: TObject);
    procedure acCloseExecute(Sender: TObject);
    procedure dtcEditorChange(Sender: TObject);
    procedure acOptionsExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acUndoExecute(Sender: TObject);
    procedure acRedoExecute(Sender: TObject);
    procedure acCutExecute(Sender: TObject);
    procedure acCopyExecute(Sender: TObject);
    procedure acPasteExecute(Sender: TObject);
    procedure acSearchExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure acReplaceExecute(Sender: TObject);
    procedure acCloseAllFilesExecute(Sender: TObject);
    procedure dtcEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuCloseClick(Sender: TObject);
    procedure acDocExecute(Sender: TObject);
    procedure acForumExecute(Sender: TObject);
    procedure acAboutExecute(Sender: TObject);
    procedure acCompileExecute(Sender: TObject);
    procedure sciEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sciEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sciEditorKeyPress(Sender: TObject; var Key: Char);
    procedure FormActivate(Sender: TObject);
    procedure acGoToExecute(Sender: TObject);
    procedure acCompileAndUploadExecute(Sender: TObject);
    procedure acCompileAndStartExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure amMenuExecute(Action: TBasicAction; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure sciEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sciEditorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
    procedure acSaveAllExecute(Sender: TObject);
    procedure sciEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure dtcEditorChanging(Sender: TObject; var AllowChange: Boolean);
    procedure trvFunctionsEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure trvFunctionsDblClick(Sender: TObject);
    procedure trvFunctionsCollapsed(Sender: TObject; Node: TTreeNode);
    procedure trvFunctionsExpanded(Sender: TObject; Node: TTreeNode);
    procedure trvFunctionsEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure trvFunctionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure trvFunctionsClick(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure pnlSpacerLeftClick(Sender: TObject);
    procedure lvDebugEnter(Sender: TObject);
    procedure lvDebugDblClick(Sender: TObject);
    procedure acIdenterExecute(Sender: TObject);
    procedure acMenuMakerExecute(Sender: TObject);
    procedure acPMMExecute(Sender: TObject);
    procedure acRemoveMissingPluginsExecute(Sender: TObject);
    procedure acSocketTerminalExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acUnidenterExecute(Sender: TObject);
    procedure acLoopGeneratorExecute(Sender: TObject);
    procedure sciEditorModified(Sender: TObject; const position,
      modificationType: Integer; text: PAnsiChar; const length, linesAdded,
      line, foldLevelNow, foldLevelPrev: Integer);
  private
    eSelectedTab: Integer;
    eCurrentLine: Integer;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
  public
    StdAutoComplete: String; // Save this because we add special functions
    StdCallTips: String;     // etc. dynamically for each file
    FunctionType: TStringList;
    function StrLength(eStr: String): Integer;
    procedure OnExceptionHandler(Sender: TObject; E: Exception);
    procedure DeleteNode(Node: TTreeNode);
    procedure RenameNode(Node: TTreeNode; New: String);
    procedure SetModified;
    procedure SetSaved;
    procedure ShowErrorLine;
  end;

var
  frmMain: TfrmMain;

implementation

uses UnitfrmOptions, UnitfrmDebug, UnitFunc,
  UnitfrmAbout, UnitfrmGoToLine, UnitTextAnalyze, UnitfrmMenuMaker,
  UnitfrmSockets, UnitfrmLoopGenerator;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  eErrorLine := -1;
  Caption := 'AMXX-Edit v2';
  StdAutoComplete := sacComplete.AStrings.Text;
  StdCallTips := cltEditor.ApiStrings.Text;
  Application.OnException := OnExceptionHandler;
  FunctionType := TStringList.Create;
  DoubleBuffered := True;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
begin
  dtcEditor.NewDocument;
  acClose.Enabled := True;
  mnuClose.Enabled := True;
  acCloseAllFiles.Enabled := True;
end;

procedure TfrmMain.acOpenExecute(Sender: TObject);
begin
  if DirectoryExists(frmSettings.txtSave.Text) then
    odOpen.InitialDir := frmSettings.txtSave.Text;

  try
    if odOpen.Execute then begin
      dtcEditor.Open(odOpen.FileName);
      SetSaved;
      UpdateList(sciEditor.Lines.Text);
    end;
    atbToolbar.RecreateControls;
  except
    // :F
  end;
end;

procedure TfrmMain.acSaveExecute(Sender: TObject);
begin
  try
    if dtcEditor.ActiveDocument.IsUntitled then
      acSaveAs.Execute
    else begin
      dtcEditor.Editor.SaveToFile(dtcEditor.ActiveDocument.FileName);
      Delay(100);
      SetSaved;
    end;
    atbToolbar.RecreateControls;
  except
    // :F
  end;
end;

procedure TfrmMain.acSaveAsExecute(Sender: TObject);
begin
  if DirectoryExists(frmSettings.txtSave.Text) then
    sdSave.InitialDir := frmSettings.txtSave.Text;

  if sdSave.Execute then begin
    AppendFileExt;
    dtcEditor.ActiveDocument.FileName := sdSave.FileName;
    sciEditor.SaveToFile(dtcEditor.ActiveDocument.FileName);
    SetSaved;
  end;
end;

procedure TfrmMain.acCloseExecute(Sender: TObject);
begin
  if dtcEditor.ActiveDocument.Modified then begin
    case MessageBox(Handle, PChar('Do you want to save "' + ExtractFileName(dtcEditor.ActiveDocument.FileName) + '" before closing?'), 'Question', MB_ICONQUESTION + MB_YESNOCANCEL) of
      mrYes: acSave.Execute;
      mrCancel: exit;
    end;
  end;
  dtcEditor.Close(dtcEditor.ActiveDocument.Index, False);
  acClose.Enabled := (dtcEditor.Tabs.Count <> 1) or (not dtcEditor.ActiveDocument.IsUntitled);
  mnuClose.Enabled := acClose.Enabled;
  acCloseAllFiles.Enabled := (dtcEditor.Tabs.Count <> 1) or (dtcEditor.Tabs.Count <> 1) or (not dtcEditor.ActiveDocument.IsUntitled);
end;

procedure TfrmMain.dtcEditorChange(Sender: TObject);
begin
  if frmSettings.chkHighlighting.Checked then
    sciEditor.LanguageManager.SelectedLanguage := 'SMALL'
  else
    sciEditor.LanguageManager.SelectedLanguage := 'null';

  if sbInfo.Panels[0].Width > Canvas.TextWidth(dtcEditor.ActiveDocument.FileName) then
    sbInfo.Panels[0].Text := dtcEditor.ActiveDocument.FileName
  else
    sbInfo.Panels[0].Text := ExtractFileName(dtcEditor.ActiveDocument.FileName);

  if dtcEditor.ActiveDocument.Modified then
    sbInfo.Panels[1].Text := 'Modified'
  else
    sbInfo.Panels[1].Text := '';
  UpdateList(sciEditor.Lines.Text);
  eCurrentLine := sciEditor.GetCurrentLineNumber;
end;

procedure TfrmMain.acOptionsExecute(Sender: TObject);
begin
  Load;
  if frmSettings.ShowModal = mrOk then begin
    Apply;
    Save;
  end;
end;

procedure TfrmMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acUndoExecute(Sender: TObject);
begin
  sciEditor.Undo;
end;

procedure TfrmMain.acRedoExecute(Sender: TObject);
begin
  sciEditor.Redo;
end;

procedure TfrmMain.acCutExecute(Sender: TObject);
begin
  sciEditor.Cut;
end;

procedure TfrmMain.acCopyExecute(Sender: TObject);
begin
  sciEditor.Copy; 
end;

procedure TfrmMain.acPasteExecute(Sender: TObject);
begin
  sciEditor.Paste;
end;

procedure TfrmMain.acSearchExecute(Sender: TObject);
begin
  try
    srpSearch.ShowSearchReplaceDialog(False);
    atbToolBar.RecreateControls;
  except
    // :F
  end;
end;

procedure TfrmMain.acFindNextExecute(Sender: TObject);
begin
  srpSearch.DoSearchReplaceText(False, False);
end;

procedure TfrmMain.acReplaceExecute(Sender: TObject);
begin
  srpSearch.ShowSearchReplaceDialog(True); 
end;

procedure TfrmMain.acCloseAllFilesExecute(Sender: TObject);
var i: integer;
begin
  if ShowSaveDialog('Close all files', 'Save files', 'Close') then begin
    for i := dtcEditor.Tabs.Count -1 downto 0 do
      dtcEditor.Close(i, False);
    acClose.Enabled := False;
    mnuClose.Enabled := False;
    acCloseAllFiles.Enabled := False;
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'Files.ini');
  end;
end;

procedure TfrmMain.dtcEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  eSelectedTab := dtcEditor.IndexOfTabAt(X, Y);
  if (eSelectedTab <> -1) and (Y < 20) and (Y > 0) and (Button = mbRight) then
    ppmEditor.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y); 
end;

procedure TfrmMain.mnuCloseClick(Sender: TObject);
begin
  if dtcEditor.ActiveDocument.Modified then begin
    case MessageBox(Handle, PChar('Do you want to save "' + ExtractFileName(dtcEditor.Document[eSelectedTab].FileName) + '" before closing?'), 'Question', MB_ICONQUESTION + MB_YESNOCANCEL) of
      mrYes: begin
        if dtcEditor.ActiveDocument.IsUntitled then begin
          if DirectoryExists(frmSettings.txtSave.Text) then
            sdSave.InitialDir := frmSettings.txtSave.Text;

          if sdSave.Execute then begin
            AppendFileExt;
            dtcEditor.Document[eSelectedTab].FileName := sdSave.FileName;
            dtcEditor.Document[eSelectedTab].Modified := False;
            sciEditor.SaveToFile(dtcEditor.Document[eSelectedTab].FileName);
            if eSelectedTab = dtcEditor.ActiveDocument.Index then
              sbInfo.Panels[1].Text := ''
          end;
        end
        else begin
          dtcEditor.Editor.SaveToFile(dtcEditor.ActiveDocument.FileName);
          dtcEditor.ActiveDocument.Modified := False;
          if eSelectedTab = dtcEditor.ActiveDocument.Index then
            sbInfo.Panels[1].Text := '';
        end;
      end;
      mrCancel: exit;
    end;
  end;

  dtcEditor.Close(eSelectedTab, False);
  acClose.Enabled := (dtcEditor.Tabs.Count <> 1) or (not dtcEditor.ActiveDocument.IsUntitled);
  mnuClose.Enabled := acClose.Enabled;
  acCloseAllFiles.Enabled := (dtcEditor.Tabs.Count <> 1) or (not dtcEditor.ActiveDocument.IsUntitled);
end;

procedure TfrmMain.acDocExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.amxmodx.org/doc/', nil, nil, SW_SHOW);
end;

procedure TfrmMain.acForumExecute(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://www.amxmodx.org/forums/viewforum.php?f=8', nil, nil, SW_SHOW);
end;

procedure TfrmMain.acAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.acCompileExecute(Sender: TObject);
begin
  DoCompile;
end;

procedure TfrmMain.sciEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  sbInfo.Panels.Items[2].Text := Format('Ln %s Ch %s', [IntToStr(sciEditor.GetCurrentLineNumber +1), IntToStr(sciEditor.GetCaretInLine +1)]);
  sciEditor.ReadOnly := False;
  eCurrentLine := sciEditor.GetCurrentLineNumber;
  if sciEditor.LineFromPosition(sciEditor.SelStart) <> eErrorLine then begin
    sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
    sciEditor.Caret.ForeColor := clDefault;
    sciEditor.Colors.SelBack := clHighlight;
  end;
end;

procedure TfrmMain.sciEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  sbInfo.Panels.Items[2].Text := Format('Ln %s Ch %s', [IntToStr(sciEditor.GetCurrentLineNumber +1), IntToStr(sciEditor.GetCaretInLine +1)]);
end;

procedure TfrmMain.sciEditorKeyPress(Sender: TObject; var Key: Char);
var eStr: String;
begin
  if (Key = #13) and (Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) = '') and (frmSettings.chkAutoIndent.Checked) then begin
    if sciEditor.GetCurrentLineNumber <> 0 then begin
      eStr := Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber -1]);
      Delete(eStr, 1, Length(eStr) -1);
      if eStr = '{' then
        sciEditor.SelText := '	';

      if sciEditor.GetCurrentLineNumber <> 0 then begin // if we are on line 0 we would access line -1 otherwise
        if (Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) <> '') and (Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber -1]) = '') and (frmSettings.chkAutoIndent.Checked) then // if the prevorious line isn't empty, the line contains only spaces and the auto-identer is enabled then...  
          sciEditor.Lines[sciEditor.GetCurrentLineNumber] := Copy(sciEditor.Lines[sciEditor.GetCurrentLineNumber -1], 1, Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber -1]) -2); // remove the last char
      end;
    end;
  end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
  sciEditor.Font.Name := 'Courier New';
  if eErrorLine <> -1 then
    ShowErrorLine;
end;

procedure TfrmMain.acGoToExecute(Sender: TObject);
begin
  frmGoToLine.txtLine.SelectAll;
  if frmGoToLine.ShowModal = mrOk then
    sciEditor.GotoLineEnsureVisible(StrToInt(frmGoToLine.txtLine.Text) -1);
end;

procedure TfrmMain.acCompileAndUploadExecute(Sender: TObject);
begin
  DoCompile;

  if eErrors = 0 then begin
    if frmSettings.optWindow.Checked then begin
      frmDebug.lblStatus.Font.Style := [];
      frmDebug.lblStatus.Caption := ' Uploading file...';
      frmDebug.lstOutput.Items.Add('');
      frmDebug.lstOutput.Items.Add('Connecting to FTP server...');
      frmDebug.lstOutput.ItemIndex := frmDebug.lstOutput.Items.Count -1;
    end
    else begin
      lvDebug.Items.Add.Caption := 'Connecting to FTP server...';
      lvDebug.ItemIndex := lvDebug.Items.Count -1;
    end;
    IdFTP.Host := frmSettings.txtHost.Text;
    IdFTP.Port := StrToInt(frmSettings.txtPort.Text);
    IdFTP.Username := frmSettings.txtUser.Text;
    IdFTP.Password := frmSettings.txtPassword.Text;
    try
      try
        IdFTP.Connect;
      finally
        if frmSettings.optWindow.Checked then begin
          frmDebug.lstOutput.Items.Add('Connected, uploading file...');
          frmDebug.lstOutput.ItemIndex := frmDebug.lstOutput.Items.Count -1;
        end
        else begin
          lvDebug.Items.Add.Caption := 'Connected, uploading file...';
          lvDebug.ItemIndex := lvDebug.Items.Count -1;
        end;
        IdFTP.ChangeDir(frmSettings.txtStandardDir.Text);
        IdFTP.Put(frmSettings.txtAMXXPath.Text + 'plugins\' + ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx'), ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx'));
        if frmSettings.optWindow.Checked then begin
          frmDebug.lstOutput.Items.Add('Done.');
          frmDebug.lstOutput.ItemIndex := frmDebug.lstOutput.Items.Count -1;
          frmDebug.lblStatus.Font.Style := [fsBold];
          frmDebug.lblStatus.Caption := ' Done.';
        end
        else begin
          lvDebug.Items.Add.Caption := 'Done.';
          lvDebug.ItemIndex := lvDebug.Items.Count -1;
        end;
        IdFTP.Disconnect;
      end;
    except
      if frmSettings.optWindow.Checked then begin
        frmDebug.lblStatus.Caption := ' Error: Couldn''t connect to server.';
        frmDebug.lstOutput.Items.Add('Error: Couldn''t connect to server.');
        frmDebug.lstOutput.Items.Add('Check your settings and try again.');
        frmDebug.lstOutput.ItemIndex := frmDebug.lstOutput.Items.Count -1;
      end
      else begin
        lvDebug.Items.Add.Caption := 'Error: Coudln''t connect to server.';
        lvDebug.Items.Add.Caption := 'Check your settings and try again.';
        lvDebug.ItemIndex := lvDebug.Items.Count -1;
      end;
    end;
  end;
end;

procedure TfrmMain.acCompileAndStartExecute(Sender: TObject);
begin
  if FileExists(frmSettings.txtHalfLife.Text) then begin
    if FileExists(frmSettings.txtAMXXPath.Text + 'plugins\' + ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx')) then
      DeleteFile(frmSettings.txtAMXXPath.Text + 'plugins\' + ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx'));
    DoCompile;
    if FileExists(frmSettings.txtAMXXPath.Text + 'plugins\' + ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx')) then begin
      if frmSettings.optWindow.Checked then
        frmDebug.lblStatus.Caption := ' Done: Starting Half-Life'
      else
        lvDebug.Items.Add.Caption := 'Starting Half-Life...';
      ShellExecute(Handle, 'open', PChar(frmSettings.txtHalfLife.Text), '', PChar(ExtractFilePath(frmSettings.txtHalfLife.Text)), SW_SHOW);
    end;
  end
  else
    MessageBox(Handle, 'Couldn''t find Half-Life exe. Check your settings and try again.', 'Error', MB_ICONERROR);
end;

procedure TfrmMain.WMCopyData(var Msg: TWMCopyData);
var
  sText: array[0..1500] of Char;
begin
  try
    StrLCopy(sText, Msg.CopyDataStruct.lpData, Msg.CopyDataStruct.cbData);
    dtcEditor.Open(sText);
    SetSaved;
    UpdateList(sciEditor.Lines.Text);
  except
    // :F
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not ShowSaveDialog('Save files before close', 'Save and close', 'Close') then
    Action := caNone;
end;

procedure TfrmMain.amMenuExecute(Action: TBasicAction;
  var Handled: Boolean);
begin
  sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
  sciEditor.Caret.ForeColor := clDefault;
  sciEditor.Colors.SelBack := clHighlight;
  if lvDebug.Visible then
    lvDebug.Visible := False;
end;

procedure TfrmMain.FormShow(Sender: TObject);
var eStr: TStringList;
    i: integer; 
begin
  if Tag = 0 then begin
    Tag := 1;
    if frmSettings.chkReload.Checked then begin
      eStr := TStringList.Create;
      if FileExists(ExtractFilePath(ParamStr(0)) + 'Files.ini') then begin
        eStr.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Files.ini');
        with frmMain do begin
          for i := 0 to eStr.Count -1 do begin
            if FileExists(eStr[i]) then begin
              dtcEditor.Open(eStr[i]);
              SetSaved;
            end;
          end;
          acClose.Enabled := True;
          mnuClose.Enabled := True;
          acCloseAllFiles.Enabled := True;
        end;
      end;
      eStr.Free;
      UpdateList(sciEditor.Lines.Text);
    end;

    for i := 1 to ParamCount do begin
      if FileExists(ParamStr(i)) then begin
        dtcEditor.Open(ParamStr(i));
        SetSaved;
      end;
    end;
    UpdateList(sciEditor.Lines.Text);
    acClose.Enabled := True;
    mnuClose.Enabled := True;
    acCloseAllFiles.Enabled := True;
  end;
end;

procedure TfrmMain.sciEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  sbInfo.Panels.Items[2].Text := Format('Ln %s Ch %s', [IntToStr(sciEditor.GetCurrentLineNumber +1), IntToStr(sciEditor.GetCaretInLine +1)]);
end;

procedure TfrmMain.sciEditorMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  sbInfo.Panels.Items[2].Text := Format('Ln %s Ch %s', [IntToStr(sciEditor.GetCurrentLineNumber +1), IntToStr(sciEditor.GetCaretInLine +1)]);
end;

procedure TfrmMain.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  sbInfo.Panels[0].Width := sbInfo.Width - 162;
  if sbInfo.Panels[0].Width > Canvas.TextWidth(dtcEditor.ActiveDocument.FileName) then
    sbInfo.Panels[0].Text := dtcEditor.ActiveDocument.FileName
  else
    sbInfo.Panels[0].Text := ExtractFileName(dtcEditor.ActiveDocument.FileName);
  lvDebug.Column[0].Width := lvDebug.Width - 20;
end;

procedure TfrmMain.acSaveAllExecute(Sender: TObject);
begin
  if ShowSaveDialog('Save all files', 'Save files', 'Close') then
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'Files.ini');
end;

procedure TfrmMain.sciEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if sciEditor.Caret.LineBackColor = clMaroon then begin
    sciEditor.SelLength := Length(sciEditor.Lines[eErrorLine -1]);
    sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
    sciEditor.Caret.ForeColor := clDefault;
    sciEditor.Colors.SelBack := clHighlight;
  end;
  sciEditor.ReadOnly := False;
  if (Key = 13) then begin // Return
    if lvDebug.Visible then
      lvDebug.Hide;
    SetModified;
  end
  else if (Key = 9) and (not (ssCtrl in Shift)) then  // Tab
    SetModified
  else if (Key >= 65) and (Key <= 90) and (not (ssCtrl in Shift)) then // a..z
    SetModified;

  if (sciEditor.GetCurrentLineNumber <> eCurrentLine) or (RemoveSpaces(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) = '') then begin
    UpdateList(sciEditor.Lines.Text);
    eCurrentLine := sciEditor.GetCurrentLineNumber;
  end;
end;

procedure TfrmMain.dtcEditorChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  if sciEditor.Caret.LineBackColor = clMaroon then begin
    sciEditor.SelLength := Length(sciEditor.Lines[eErrorLine -1]);
    sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
    sciEditor.Caret.ForeColor := clDefault;
    sciEditor.Colors.SelBack := clHighlight;
  end;
  sciEditor.ReadOnly := False;
  lvDebug.Hide;
end;

procedure TfrmMain.trvFunctionsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := Assigned(Node.Parent);
end;

procedure TfrmMain.trvFunctionsDblClick(Sender: TObject);
function CheckAndSelect(ePath: String): Boolean;
var i: integer;
begin
  Result := False;
  ePath := LowerCase(ePath);
  for i := 0 to dtcEditor.Tabs.Count -1 do begin
    if LowerCase(dtcEditor.Document[i].FileName) = ePath then begin
      dtcEditor.Activate(i);
      Result := True;
      exit; 
    end;
  end;
end;

var Node: TTreeNode;
    i: integer;
begin
  Node := trvFunctions.Selected;
  if not Assigned(Node) then
    exit;
  if not Assigned(Node.Parent) then
    exit;
    
  if Node.Parent.Text = 'Included' then begin
    if FileExists(frmSettings.txtAMXXPath.Text + 'scripting\include\' + Node.Text) then begin
      if CheckAndSelect(frmSettings.txtAMXXPath.Text + 'scripting\include\' + Node.Text) then
        exit;
      dtcEditor.Open(frmSettings.txtAMXXPath.Text + 'scripting\include\' + Node.Text);
      SetSaved;
      UpdateList(sciEditor.Lines.Text);
    end
    else if FileExists(frmSettings.txtAMXXPath.Text + 'scripting\' + Node.Text) then begin
      if CheckAndSelect(frmSettings.txtAMXXPath.Text + 'scripting\' + Node.Text) then
        exit;
      dtcEditor.Open(frmSettings.txtAMXXPath.Text + 'scripting\' + Node.Text);
      SetSaved;
      UpdateList(sciEditor.Lines.Text);
    end
    else if (FileExists(ExtractFilePath(dtcEditor.ActiveDocument.FileName) + Node.Text)) and (not dtcEditor.ActiveDocument.IsUntitled) then begin
      if CheckAndSelect(ExtractFilePath(dtcEditor.ActiveDocument.FileName) + Node.Text) then
        exit;
      dtcEditor.Open(ExtractFilePath(dtcEditor.ActiveDocument.FileName) + Node.Text);
      SetSaved;
      UpdateList(sciEditor.Lines.Text);
    end
    else
      MessageBox(Handle, PChar('Couldn''t find "' + Node.Text + '". Maybe you''re still working on it on another (not saved) document or you entered an invalid value.'), 'Information', MB_ICONINFORMATION);
  end;
  if Node.Parent.Text = 'Defined' then begin
    for i := 0 to sciEditor.Lines.Count -1 do begin
      if Pos('#define ' + Node.Text, TrimLeft(sciEditor.Lines[i])) = 1 then begin
        sciEditor.SetFocus;
        sciEditor.GotoLine(i);
        exit;
      end;
    end;
  end;
  if (Node.Parent.Text = 'Variables') or (Node.Parent.Text = 'Constants') then begin
    for i := 0 to sciEditor.Lines.Count -1 do begin
      if Pos('new ' + Node.Text, TrimLeft(sciEditor.Lines[i])) = 1 then begin
        sciEditor.SetFocus;
        sciEditor.GotoLine(i);
        exit;
      end;
    end;
  end;
  if (Node.Parent.Text = 'Functions') then begin
    for i := 0 to sciEditor.Lines.Count -1 do begin
      if Pos(FunctionType[Node.Index] + Node.Text, TrimLeft(sciEditor.Lines[i])) = 1 then begin
        sciEditor.SetFocus;
        sciEditor.GotoLine(i);
        exit;
      end;
    end;
  end;
end;

procedure TfrmMain.OnExceptionHandler(Sender: TObject; E: Exception);
begin
  // nothing :F
end;

procedure TfrmMain.trvFunctionsCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 18;
  Node.SelectedIndex := 18;
end;

procedure TfrmMain.trvFunctionsExpanded(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 19;
  Node.SelectedIndex := 19;
end;

procedure TfrmMain.trvFunctionsEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
begin
  if S = '' then begin
    DeleteNode(Node);
    trvFunctions.Items.Delete(Node);
    SetModified;
  end
  else
    RenameNode(Node, S);
end;

procedure TfrmMain.DeleteNode(Node: TTreeNode);
procedure DeleteLine(eFormat: String);
var i: Integer;
begin
  eFormat := Trim(Format(eFormat, [Node.Text]));
  for i := 0 to sciEditor.Lines.Count -1 do begin
    if Pos(eFormat, Trim(sciEditor.Lines[i])) = 1 then begin
      sciEditor.Lines.Delete(i);
      exit;
    end;
  end;
end;

var i, a, b, c: integer;
    eStr: TStringList;
begin
  if Node.Parent.Text = 'Included' then
    DeleteLine('#include <%s>');
  if Node.Parent.Text = 'Defined' then
    DeleteLine('#define %s');
  if Node.Parent.Text = 'Variables' then
    DeleteLine('new %s');
  if Node.Parent.Text = 'Constants' then
    DeleteLine('new %s');
  if Node.Parent.Text = 'Functions' then begin
    eStr := TStringList.Create;
    eStr.Text := sciEditor.Lines.Text;
    for i := 0 to eStr.Count -1 do begin
      if Pos(Node.Text, TrimLeft(eStr[i])) = 1 then begin
        if CountChars(eStr[i], '{') <> CountChars(eStr[i], '}') then begin
          b := 0; // open brackets
          for a := i to eStr.Count -1 do begin
            // Remove strings because they could include brackets
            while CountChars(eStr[a], '"') > 1 do
              eStr[a] := StringReplace(eStr[a], '"' + Between(eStr[i], '"', '"') + '"', '', [rfReplaceAll]);
            if (Pos('/*', eStr[a]) = 1) or (Pos('*', eStr[a]) = 1) or (Pos('*/', eStr[a]) = 1) then
              eStr[a] := '';
            if Pos('//', eStr[a]) <> 0 then
              eStr[a] := Copy(eStr[a], 1, Pos('//', eStr[a]) -2);
            // Find end of the function
            b := b + CountChars(eStr[a], '{');
            b := b - CountChars(eStr[a], '}');
            if b = 0 then begin
              // Delete function
              for c := i + a + 1 downto i do
                sciEditor.Lines.Delete(c);
              SetModified;
              exit;
            end;
          end;
        end
        else begin
          sciEditor.Lines.Delete(i);
          SetModified;
        end;
      end;
    end;
    eStr.Free;
  end;
end;

procedure TfrmMain.SetModified;
begin
  sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
  sciEditor.Caret.ForeColor := clDefault;
  sciEditor.Colors.SelBack := clHighlight;
  sbInfo.Panels[1].Text := 'Modified';
  dtcEditor.ActiveDocument.Modified := True;
  acClose.Enabled := True;
  mnuClose.Enabled := True;
  acCloseAllFiles.Enabled := True;
end;

procedure TfrmMain.trvFunctionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 46) and (Assigned(trvFunctions.Selected)) and (not trvFunctions.IsEditing) then begin
    DeleteNode(trvFunctions.Selected);
    trvFunctions.Items.Delete(trvFunctions.Selected); 
  end;
end;

procedure TfrmMain.trvFunctionsClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to trvFunctions.Items.Count -1 do begin
    if not Assigned(trvFunctions.Items[i].Parent) then begin
      if trvFunctions.Items[i].Text = 'Included' then
        eExpand1 := trvFunctions.Items[i].Expanded;
      if trvFunctions.Items[i].Text = 'Defined' then
        eExpand2 := trvFunctions.Items[i].Expanded;
      if trvFunctions.Items[i].Text = 'Variables' then
        eExpand3 := trvFunctions.Items[i].Expanded;
      if trvFunctions.Items[i].Text = 'Constants' then
        eExpand4 := trvFunctions.Items[i].Expanded;
      if trvFunctions.Items[i].Text = 'Functions' then
        eExpand5 := trvFunctions.Items[i].Expanded;
    end;
  end;
end;

procedure TfrmMain.RenameNode(Node: TTreeNode; New: String);
procedure ChangeLineTo(eOldFormat, eNewFormat: String);
var i: integer;
begin
  eOldFormat := Format(eOldFormat, [Node.Text]);
  eNewFormat := Format(eNewFormat, [New]);
  for i := 0 to sciEditor.Lines.Count -1 do begin
    if Pos(Trim(eOldFormat), Trim(sciEditor.Lines[i])) = 1 then begin
      sciEditor.Lines[i] := eNewFormat;
      exit;
    end;
  end;
end;
begin
  if Node.Parent.Text = 'Included' then
    ChangeLineTo('#include <%s>', '#include <%s>');
  if Node.Parent.Text = 'Defined' then
    ChangeLineTo('#define %s', '#define %s');
  if Node.Parent.Text = 'Variables' then
    ChangeLineTo('new %s', 'new %s');
  if Node.Parent.Text = 'Constants' then
    ChangeLineTo('new %s', 'new %s');
  if Node.Parent.Text = 'Functions' then
    ChangeLineTo('%s', '%s');
  SetModified;
end;

procedure TfrmMain.acEditExecute(Sender: TObject);
begin
  if (trvFunctions.Focused) and (Assigned(trvFunctions.Selected)) then begin
    if Assigned(trvFunctions.Selected.Parent) then
      trvFunctions.Selected.EditText;
  end;
end;

procedure TfrmMain.pnlSpacerLeftClick(Sender: TObject);
var i: integer;
begin
  if frmSettings.cboCodeExplorer.ItemIndex = 1 then begin
    if pnlFunctions.Width <> 150 then begin
      for i := 1 to 30 do begin
        Delay(5);
        pnlFunctions.Width := i * 5;
      end;
    end
    else begin
      for i := 30 downto 1 do begin
        Delay(5);
        pnlFunctions.Width := i * 5;
      end;
    end;
  end;
end;

procedure TfrmMain.ShowErrorLine;
begin
  BringToFront;
  SetFocus;
  sciEditor.SetFocus;
  sciEditor.GotoLineEnsureVisible(eErrorLine -1);
  sciEditor.SelLength := Length(sciEditor.Lines[eErrorLine -1]);
  sciEditor.ReadOnly := True;
  sciEditor.Colors.SelBack := clMaroon;
  sciEditor.Caret.LineBackColor := clMaroon;
  sciEditor.Caret.ForeColor := clWhite;
  eErrorLine := -1;
end;

procedure TfrmMain.lvDebugEnter(Sender: TObject);
begin
  if sciEditor.Caret.LineBackColor = clMaroon then begin
    sciEditor.SelLength := Length(sciEditor.Lines[eErrorLine -1]);
    sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;
    sciEditor.Caret.ForeColor := clDefault;
    sciEditor.Colors.SelBack := clHighlight;
  end;
  sciEditor.ReadOnly := False;
end;

procedure TfrmMain.lvDebugDblClick(Sender: TObject);
var eStr: String;
begin
  if Assigned(lvDebug.Selected) then begin
    eStr := lvDebug.Selected.Caption;
    while Pos(#32, eStr) <> 0 do
      Delete(eStr, 1, 1);
    try
      StrToInt(eStr);
    finally
      eErrorLine := StrToInt(eStr);
      ShowErrorLine;
    end;
  end;
end;

procedure TfrmMain.acIdenterExecute(Sender: TObject);
var eStr: TStringList;
    i, k: integer;
    eIdent, eTempIdent: Integer;
begin
  Screen.Cursor := crHourGlass;
  sciEditor.Enabled := False;
  eStr := TStringList.Create;
  eIdent := 0;
  eTempIdent := 0;
  for i := 0 to sciEditor.Lines.Count -1 do begin
    eStr.Add(TrimLeft(sciEditor.Lines[i]));
    // Remove strings and comments virtually because they could include brackets
    if CountChars(eStr[i], '"') <> 0 then begin
      Caption := Format('AMXX-Edit v2 - Preparing (%s of %s lines)', [IntToStr(i+1), IntToStr(sciEditor.Lines.Count)]);
      while CountChars(eStr[i], '"') > 1 do
        eStr[i] := StringReplace(eStr[i], '"' + Between(eStr[i], '"', '"') + '"', '', [rfReplaceAll]);
      if (Pos('/*', eStr[i]) = 1) or (Pos('*', eStr[i]) = 1) or (Pos('*/', eStr[i]) = 1) then
        eStr[i] := '';
      if Pos('//', eStr[i]) <> 0 then
        eStr[i] := Copy(eStr[i], 1, Pos('//', eStr[i]) -2);
    end;
  end;

  for i := 0 to eStr.Count -1 do begin
    if CountChars(eStr[i], '{') <> CountChars(eStr[i], '}') then
      eIdent := eIdent - CountChars(eStr[i], '}');
    sciEditor.Lines[i] := TrimLeft(sciEditor.Lines[i]);
    for k := 1 to eIdent + eTempIdent do
      sciEditor.Lines[i] := '	' + sciEditor.Lines[i];
    if eTempIdent <> 0 then
      eTempIdent := eTempIdent -1;
    if CountChars(eStr[i], '{') <> CountChars(eStr[i], '}') then
      eIdent := eIdent + CountChars(eStr[i], '{');

    if (Pos('if', eStr[i]) = 1) and (Pos('{', eStr[i]) = 0) then
      eTempIdent := eTempIdent +1
    else if (Pos('else', eStr[i]) = 1) and (Pos('{', eStr[i]) = 0) then
      eTempIdent := eTempIdent +1;
    Caption := Format('AMXX-Edit v2 - Setting indents (%s of %s lines)', [IntToStr(i+1), IntToStr(sciEditor.Lines.Count)]);
  end;
  Sleep(350);
  SetModified;
  Caption := 'AMXX-Edit v2';
  
  sciEditor.Enabled := True;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.SetSaved;
begin
  dtcEditor.ActiveDocument.Modified := False;
  sbInfo.Panels[1].Text := '';
  acClose.Enabled := True;
  mnuClose.Enabled := True;
  acCloseAllFiles.Enabled := True;
end;

procedure TfrmMain.acMenuMakerExecute(Sender: TObject);
begin
  with frmMenuMaker do begin
    DefaultMenu := True;
    nbkPages.PageIndex := 0;
    cmdCancel.Caption := 'Cancel';
    cmdNext.Caption := '&Next >';
    Reset;
    ShowModal;
  end;
  if sbInfo.Panels[0].Width > Canvas.TextWidth(dtcEditor.ActiveDocument.FileName) then
    sbInfo.Panels[0].Text := dtcEditor.ActiveDocument.FileName
  else
    sbInfo.Panels[0].Text := ExtractFileName(dtcEditor.ActiveDocument.FileName);
end;

procedure TfrmMain.acPMMExecute(Sender: TObject);
begin
  with frmMenuMaker do begin
    DefaultMenu := False;
    nbkPages.PageIndex := 0;
    cmdCancel.Caption := 'Cancel';
    cmdNext.Caption := '&Next >';
    Reset;
    ShowModal;
  end;
  if sbInfo.Panels[0].Width > Canvas.TextWidth(dtcEditor.ActiveDocument.FileName) then
    sbInfo.Panels[0].Text := dtcEditor.ActiveDocument.FileName
  else
    sbInfo.Panels[0].Text := ExtractFileName(dtcEditor.ActiveDocument.FileName);
end;

procedure TfrmMain.acRemoveMissingPluginsExecute(Sender: TObject);
var eStr: TStringList;
    i, eRemoved: Integer;
    ePlugin: String;
begin
  eStr := TStringList.Create;
  if FileExists(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini') then begin
    eStr.LoadFromFile(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini');
    eRemoved := 0;
    for i := eStr.Count -1 downto 0 do begin // 0 to eStr.Count -1 won't work
      if (Copy(eStr[i], 1, 1) <> ';') and (Copy(eStr[i], 1, 2) <> '//') then begin
        ePlugin := frmSettings.txtAMXXPath.Text + 'plugins\' + eStr[i];
        if Pos(';', ePlugin) > 0 then
          ePlugin := Copy(ePlugin, 1, Pos(';', ePlugin) -1);
        if Pos('//', ePlugin) > 0 then
          ePlugin := Copy(ePlugin, 1, Pos('//', ePlugin) -1);
        ePlugin := Trim(ePlugin);
        if not FileExists(ePlugin) then begin
          eStr.Delete(i);
          Inc(eRemoved, 1);
        end;
      end;
    end;
    eStr.SaveToFile(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini');
    if eRemoved = 0 then
      MessageBox(Handle, 'Done! No plugin has been removed.', 'Information', MB_ICONINFORMATION)
    else
      MessageBox(Handle, PChar('Done! Removed ' + IntToStr(eRemoved) + ' missing plugins.'), 'Information', MB_ICONINFORMATION);
  end
  else
    MessageBox(Handle, 'Couldn''t find plugins.ini. Check your settings and try again.', 'Warning', MB_ICONWARNING);
  eStr.Free;
end;

procedure TfrmMain.acSocketTerminalExecute(Sender: TObject);
begin
  frmSocketTerminal.Show;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FunctionType.Free;
end;

procedure TfrmMain.acSelectAllExecute(Sender: TObject);
begin
  sciEditor.SelectAll;
end;

procedure TfrmMain.acUnidenterExecute(Sender: TObject);
var i: integer;
begin
  Screen.Cursor := crHourGlass;
  for i := 0 to sciEditor.Lines.Count -1 do begin
    Caption := Format('AMXX-Edit v2 - Removing indents (%s of %s lines)', [IntToStr(i +1), IntToStr(sciEditor.Lines.Count)]);
    sciEditor.Lines[i] := TrimLeft(sciEditor.Lines[i]);
  end;
  SetModified;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.acLoopGeneratorExecute(Sender: TObject);
begin
  frmLoopGenerator.optWhile.Checked := True;
  frmLoopGenerator.txtWhileCondition.Clear;
  frmLoopGenerator.txtVariable.Text := 'i=0';
  frmLoopGenerator.txtForCondition.Text := 'i<10';
  frmLoopGenerator.txtForAction.Text := 'i++';
  if frmLoopGenerator.ShowModal = mrOk then begin
    if frmLoopGenerator.optWhile.Checked then
      sciEditor.SelText := 'while (' + frmLoopGenerator.txtWhileCondition.Text + ') { /* Add code here */ }'
    else
      sciEditor.SelText := 'for (' + frmLoopGenerator.txtVariable.Text + ', ' + frmLoopGenerator.txtForCondition.Text + ', ' + frmLoopGenerator.txtForAction.Text + ') { /* Add code here */ }';
  end;
end;

procedure TfrmMain.sciEditorModified(Sender: TObject; const position,
  modificationType: Integer; text: PAnsiChar; const length, linesAdded,
  line, foldLevelNow, foldLevelPrev: Integer);
var i: integer;
    eStr: String;
begin
  eStr := sciEditor.Lines.Text;
  if StrLength(eStr) <> 0 then begin
    for i := StrLength(eStr) -1 downto 1 do begin
      if Ord(eStr[i]) < 9 then
        Delete(eStr, i, 1);
    end; 
  end;
  sciEditor.Lines.Text := eStr;
end;

function TfrmMain.StrLength(eStr: String): Integer;
begin
  Result := Length(eStr);
end;

end.
