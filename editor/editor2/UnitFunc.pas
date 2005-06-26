unit UnitFunc;

interface

uses SysUtils, Classes, IniFiles, Graphics, ScintillaLanguageManager,
     Windows, Messages, SciLexerMod, tlhelp32, Controls, Forms, SciDocuments;

procedure Delay(eTime: Integer);
function CountChars(eIn: String; eChar: Char): Integer;
function Between(eText, eFirst, eSecond: String): String;
procedure SendOpen(eFile: String);
procedure Load;
procedure Save;
procedure Apply;
function GetConsoleOutput(const Command: String): Boolean;
procedure KillIt(dwProcID: DWORD);
function GetProcID(sProcName: String): Integer;
procedure DoCompile;
function ShowSaveDialog(Caption, SaveCaption, CloseCaption: String): Boolean;
procedure AppendFileExt;
function RemoveSpaces(eInput: String): String;

var eErrorLine: Integer;
    eHints, eWarnings, eErrors: Integer;

implementation

uses UnitfrmMain, UnitfrmOptions, UnitfrmAbout,
  UnitfrmDebug, UnitfrmSaveDialog;

procedure Delay(eTime: Integer);
var i: integer;
begin
  for i := 1 to eTime do begin
    Sleep(1);
    Application.ProcessMessages;
  end;
end;

function CountChars(eIn: String; eChar: Char): Integer;
var i: integer;
begin
  Result := 0;
  if Length(eIn) <> 0 then begin
    for i := 1 to Length(eIn) do begin
      if eIn[i] = eChar then
        Inc(Result, 1);
    end;
  end;
end;

function Between(eText, eFirst, eSecond: String): String;
var eTemp: String;
begin
if (Pos(eFirst, eText) = 0) or (Pos(eSecond, eText) = 0) then
  Result := ''
else begin
  eTemp := eText;
  Delete(eTemp, 1, Pos(eFirst, eText) + Length(eFirst) - 1);
  Delete(eTemp, Pos(eSecond, eTemp), Length(eTemp));
  Result := eTemp;
end;
end;

procedure SendOpen(eFile: String);
var HTargetWnd: HWND;
    ACopyDataStruct: TCopyDataStruct;  
begin
  with ACopyDataStruct do  
  begin  
    dwData := 0;  
    cbData := Length(eFile) + 1;
    lpData := PChar(eFile);  
  end;  

  HTargetWnd := FindWindow('TfrmMain', 'AMXX-Edit v2');  

  if HTargetWnd <> 0 then  
    SendMessage(HTargetWnd, WM_COPYDATA, 0, LongInt(@ACopyDataStruct));
end;

procedure Load;
var eFile: TIniFile;
begin
  if not FileExists(ExtractFilePath(ParamStr(0)) + 'Config.ini') then
    exit;
    
  eFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Config.ini');
  with frmSettings do begin
    { Editor }
    cboFoldingStyle.ItemIndex := eFile.ReadInteger('Editor', 'FoldingStyle', 0);
    chkAutoComplete.Checked := eFile.ReadBool('Editor', 'Auto-Complete', True);
    chkHints.Checked := eFile.ReadBool('Editor', 'Hints', True);
    chkHighlighting.Checked := eFile.ReadBool('Editor', 'Highlighting', True);
    chkAutoIndent.Checked := eFile.ReadBool('Editor', 'AutoIndent', True);
    chkBrackets.Checked := eFile.ReadBool('Editor', 'HighlightBrackets', True); 
    { Directories }
    txtAMXXPath.Text := eFile.ReadString('Directories', 'AMXX', '');
    txtHalfLife.Text := eFile.ReadString('Directories', 'Half-Life', '');
    txtSave.Text := eFile.ReadString('Directories', 'SaveTarget', '');
    { Colors }
    cboComments.Selected := eFile.ReadInteger('Colors', 'Comments', clGreen);
    cboDirectives.Selected := eFile.ReadInteger('Colors', 'Directives', clTeal);
    cboOperators.Selected := eFile.ReadInteger('Colors', 'Operators', clNavy);
    cboStrings.Selected := eFile.ReadInteger('Colors', 'Strings', clBlue);
    cboKeywords.Selected := eFile.ReadInteger('Colors', 'Keywords', clRed);
    cboActiveLine.Selected := eFile.ReadInteger('Colors', 'ActiveLine', $00FFE6E6);
    { FTP }
    txtHost.Text := eFile.ReadString('FTP', 'Host', '');
    txtPort.Text := IntToStr(eFile.ReadInteger('FTP', 'Port', 21));
    txtUser.Text := eFile.ReadString('FTP', 'Username', '');
    txtPassword.Text := eFile.ReadString('FTP', 'Password', '');
    txtStandardDir.Text := eFile.ReadString('FTP', 'DefaultDir', '\');
    { View }
    cboCodeExplorer.ItemIndex := eFile.ReadInteger('View', 'ShowCodeExplorer', 0);
    chkStatusbar.Checked := eFile.ReadBool('View', 'Statusbar', True); 
    { Char completer }
    frmSettings.chkAutoCloseBrackets.Checked := eFile.ReadBool('CharCompleter', 'AutoCloseBrackets', False);
    frmSettings.chkAutoCloseQuotes.Checked := eFile.ReadBool('CharCompleter', 'AutoCloseQuotes', False);
    { Compiler Output }
    if eFile.ReadBool('View', 'CompileOutputInWindow', True) then
      frmSettings.optWindow.Checked := True
    else
      frmSettings.optList.Checked := True;
    { Misc }
    chkReload.Checked := eFile.ReadBool('Misc', 'AutoReload', True);
    chkAutoAddPlugins.Checked := eFile.ReadBool('Misc', 'AutoAdd', True);
  end;
  eFile.Free;
end;

procedure Save;
var eFile: TIniFile;
begin
  eFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Config.ini');
  with frmSettings do begin
    { Editor }
    eFile.WriteInteger('Editor', 'FoldingStyle', cboFoldingStyle.ItemIndex);
    eFile.WriteBool('Editor', 'Auto-Complete', chkAutoComplete.Checked);
    eFile.WriteBool('Editor', 'Hints', chkHints.Checked);
    eFile.WriteBool('Editor', 'Highlighting', chkHighlighting.Checked);
    eFile.WriteBool('Editor', 'AutoIndent', chkAutoIndent.Checked);
    eFile.WriteBool('Editor', 'HighlightBrackets', chkBrackets.Checked); 
    { Directories }
    eFile.WriteString('Directories', 'AMXX', txtAMXXPath.Text);
    eFile.WriteString('Directories', 'Half-Life', txtHalfLife.Text);
    eFile.WriteString('Directories', 'SaveTarget', txtSave.Text);
    { Colors }
    eFile.WriteInteger('Colors', 'Comments', cboComments.Selected);
    eFile.WriteInteger('Colors', 'Directives', cboDirectives.Selected);
    eFile.WriteInteger('Colors', 'Operators', cboOperators.Selected);
    eFile.WriteInteger('Colors', 'Strings', cboStrings.Selected);
    eFile.WriteInteger('Colors', 'Keywords', cboKeywords.Selected);
    eFile.WriteInteger('Colors', 'ActiveLine', cboActiveLine.Selected); 
    { FTP }
    eFile.WriteString('FTP', 'Host', txtHost.Text);
    eFile.WriteString('FTP', 'Port', txtPort.Text);
    eFile.WriteString('FTP', 'Username', txtUser.Text);
    eFile.WriteString('FTP', 'Password', txtPassword.Text);
    eFile.WriteString('FTP', 'DefaultDir', txtStandardDir.Text);
    { View }
    eFile.WriteInteger('View', 'ShowCodeExplorer', cboCodeExplorer.ItemIndex);
    eFile.WriteBool('View', 'Statusbar', chkStatusbar.Checked); 
    { Char completer }
    eFile.WriteBool('CharCompleter', 'AutoCloseBrackets', frmSettings.chkAutoCloseBrackets.Checked);
    eFile.WriteBool('CharCompleter', 'AutoCloseQuotes', frmSettings.chkAutoCloseQuotes.Checked);
    { Compiler Output }
    eFile.WriteBool('View', 'CompileOutputInWindow', frmSettings.optWindow.Checked);
    { Misc }
    eFile.WriteBool('Misc', 'AutoReload', chkReload.Checked);
    eFile.WriteBool('Misc', 'AutoAdd', chkAutoAddPlugins.Checked);
  end;
  eFile.Free;
end;

procedure Apply;
function PathComplete(eInput: String): String;
var eBackup: String;
begin
  if Trim(eInput) = '' then
    exit;
    
  eBackup := eInput;
  while Length(eInput) > 1 do
    Delete(eInput, 1, 1);
  if eInput <> '\' then
    Result := eBackup + '\'
  else
    Result := eBackup;
end;

begin
  with frmMain do begin
    { Folding }
    case frmSettings.cboFoldingStyle.ItemIndex of
      0: sciEditor.FoldMarkerType := sciMarkArrows;
      1: sciEditor.FoldMarkerType := sciMarkBox;
      2: sciEditor.FoldMarkerType := sciMarkCircle;
      3: sciEditor.FoldMarkerType := sciMarkPlusMinus;
    end;
    if frmSettings.cboFoldingStyle.ItemIndex = 4 then
      sciEditor.Folding := sciEditor.Folding - [foldFold]
    else
      sciEditor.Folding := sciEditor.Folding + [foldFold];
    { Auto Complete }
    sacComplete.Disabled := not frmSettings.chkAutoComplete.Checked;
    { Hints }
    cltEditor.Disabled := not frmSettings.chkHints.Checked;
    { Colors }
    with sciEditor.LanguageManager.LanguageList.Find('SMALL').Styles do begin
      TSciStyle(Items[0]).ForeColor := frmSettings.cboComments.Selected;
      TSciStyle(Items[1]).ForeColor := frmSettings.cboDirectives.Selected;
      TSciStyle(Items[2]).ForeColor := frmSettings.cboOperators.Selected;
      TSciStyle(Items[3]).ForeColor := frmSettings.cboStrings.Selected;
      TSciStyle(Items[4]).ForeColor := frmSettings.cboKeywords.Selected;
      TSciStyle(Items[5]).ForeColor := frmSettings.cboComments.Selected;
      TSciStyle(Items[6]).ForeColor := TSciStyle(Items[2]).ForeColor;
    end;
    frmMain.sciEditor.Caret.LineBackColor := frmSettings.cboActiveLine.Selected;

    if frmSettings.chkHighlighting.Checked then
      frmMain.sciEditor.LanguageManager.SelectedLanguage := 'SMALL'
    else
      frmMain.sciEditor.LanguageManager.SelectedLanguage := 'null';
    { Check directories }
    frmSettings.txtAMXXPath.Text := PathComplete(frmSettings.txtAMXXPath.Text);
    frmSettings.txtSave.Text := PathComplete(frmSettings.txtSave.Text);
    { View }
    case frmSettings.cboCodeExplorer.ItemIndex of
      0: begin
        frmMain.splFunctions.Visible := True;
        frmMain.pnlFunctions.Visible := True;
        frmMain.pnlFunctions.Width := 150;
        frmMain.pnlSpacerLeft.Cursor := crDefault;
      end;
      1: begin
        frmMain.splFunctions.Visible := False;
        frmMain.pnlFunctions.Visible := True;
        frmMain.pnlFunctions.Width := 5;
        frmMain.pnlSpacerLeft.Cursor := crHSplit;
      end;
      2: begin
        frmMain.pnlFunctions.Visible := False;
        frmMain.splFunctions.Visible := False;
        frmMain.pnlSpacerLeft.Cursor := crDefault;
      end;
    end;
    frmMain.sbInfo.Visible := frmSettings.chkStatusbar.Checked;
    frmMain.sciEditor.BraceHilite := frmSettings.chkBrackets.Checked;
    { Char completer }
    frmMain.sciEditor.AutoCloseBraces := frmSettings.chkAutoCloseBrackets.Checked;
    frmMain.sciEditor.AutoCloseQuotes := frmSettings.chkAutoCloseQuotes.Checked;
  end;
end;

procedure DoAdd(eStream: TMemoryStream);
var eStr: TStringList;
    i: integer;
    eErrLine: Integer;
    eType, eBackup, eTemp: String;
begin
  eStr := TStringList.Create;
  eStr.LoadFromStream(eStream);
  
  eHints := 0;
  eWarnings := 0;
  eErrors := 0;
  eType := '';
  eErrorLine := -1;
  
  for i := 2 to eStr.Count -1 do begin
    try
      if eStr[i] <> '' then begin
        if (Pos(': fatal error', eStr[i]) <> 0) or (Pos(': error', eStr[i]) <> 0) or (Pos(': warning', eStr[i]) <> 0) or (Pos(': hint', eStr[i]) <> 0) then begin
          eBackup := eStr[i];
          if (Pos(': fatal error', eStr[i]) <> 0) or (Pos(': error', eStr[i]) <> 0) then begin
            Inc(eErrors, 1);
            eType := 'Error';
          end
          else if Pos(': warning', eStr[i]) <> 0 then begin
            Inc(eWarnings, 1);
            eType := 'Warning';
          end
          else if Pos(': hint', eStr[i]) <> 0 then begin
            Inc(eHints, 1);
            eType := 'Hint';
          end;

          eErrLine := -1;
          while (Pos('(', eStr[i]) <> 1) and (Length(eStr[i]) > 0) do
            eStr[i] := Copy(eStr[i], 2, Length(eStr[i]));
          try
            eTemp := Copy(eStr[i], 2, Pos(')', eStr[i]) -2);
            if Pos(#32, eTemp) <> 0 then
              eTemp := Copy(eTemp, 1, Pos(#32, eTemp) -1);
            eErrLine := StrToInt(eTemp);
            if (eErrorLine = -1) and (eType = 'Error') then
              eErrorLine := eErrLine;
          except
            if (eErrorLine = -1) and (eType = 'Error') then
              eErrorLine := frmMain.sciEditor.Lines.Count -1;
          end;
          eStr[i] := Copy(eStr[i], 3, Length(eStr[i]));
          while (Pos(':', eStr[i]) > 3) and (Length(eStr[i]) > 0) do
            eStr[i] := Copy(eStr[i], 2, Length(eStr[i]));
          eStr[i] := Copy(eStr[i], 4, Length(eStr[i]));
          eStr[i] := Copy(eStr[i], Pos(':', eStr[i]) +2, Length(eStr[i]));
          if Pos(': fatal error', eBackup) <> 0 then
            eStr[i] := 'Fatal error: ' + eStr[i]
          else if eStr[i] = '' then
            eStr[i] := eBackup
          else
            eStr[i] := eType + ': ' + eStr[i] + ' on line ' + IntToStr(eErrLine);
          if frmSettings.optWindow.Checked then begin
            frmDebug.lblErrors.Caption := ' Errors: ' + IntToStr(eErrors);
            frmDebug.lblWarnings.Caption := ' Warnings: ' + IntToStr(eWarnings);
            frmDebug.lblHints.Caption := ' Hints: ' + IntToStr(eHints);
          end;
        end
        else if (eStr[i] = 'Done.') or (Pos(' Error', eStr[i]) <> 0) or (Pos(' Warning', eStr[i]) <> 0) or (Pos(' Hint', eStr[i]) <> 0) then begin
          if frmSettings.optWindow.Checked then begin
            if eErrors <> 0 then
              frmDebug.lblStatus.Caption := ' Done. There are errors.'
            else if eWarnings <> 0 then
              frmDebug.lblStatus.Caption := ' Done. There are warnings.'
            else if eHints <> 0 then
              frmDebug.lblStatus.Caption := ' Done. There are hints.'
            else
              frmDebug.lblStatus.Caption := ' Done.';
            frmDebug.lblStatus.Font.Style := [fsBold];
          end
          else begin
            if eErrors <> 0 then
              frmMain.lvDebug.Items.Add.Caption := 'Done. There are errors.'
            else if eWarnings <> 0 then
              frmMain.lvDebug.Items.Add.Caption := 'Done. There are warnings.'
            else if eHints <> 0 then
              frmMain.lvDebug.Items.Add.Caption := 'Done. There are hints.'
            else
              frmMain.lvDebug.Items.Add.Caption := 'Done.';

            if eErrorLine <> -1 then
              frmMain.ShowErrorLine;
            eStr.Free;
            exit;
          end;
        end;
        
        if frmSettings.optWindow.Checked then begin
          frmDebug.lstOutput.Items.Add(eStr[i]);
          frmDebug.lstOutput.ItemIndex := frmDebug.lstOutput.Items.Count -1;
          frmDebug.Repaint;
        end
        else begin
          frmMain.lvDebug.Items.Add.Caption := eStr[i];
          frmMain.lvDebug.ItemIndex := frmDebug.lstOutput.Items.Count -1;
          frmMain.Repaint;
        end;
      end;
    except
      // nothing
    end;
  end;
  eStr.Free;
end;

function GetConsoleOutput(const Command: String): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeOutputRead: THandle;
  PipeOutputWrite: THandle;
  PipeErrorsRead: THandle;
  PipeErrorsWrite: THandle;
  Succeed: Boolean;
  Buffer: array [0..255] of Char;
  NumberOfBytesRead: DWORD;
  Stream: TMemoryStream;
begin
  frmDebug.Compiling := True;
  FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);

  FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;

  CreatePipe(PipeOutputRead, PipeOutputWrite, @SecurityAttr, 0);
  CreatePipe(PipeErrorsRead, PipeErrorsWrite, @SecurityAttr, 0);

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb:=SizeOf(StartupInfo);
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := PipeOutputWrite;
  StartupInfo.hStdError := PipeErrorsWrite;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  if  CreateProcess(nil, PChar(command), nil, nil, true,
  CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil,
  StartupInfo, ProcessInfo) then begin
    Result := True;
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsWrite);

    Stream := TMemoryStream.Create;
    try
      while True do begin
        Succeed := ReadFile(PipeOutputRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then Break;
        Stream.Write(Buffer, NumberOfBytesRead);
      end;
      Stream.Position := 0;
      DoAdd(Stream);
    finally
      Stream.Free;
    end;
    CloseHandle(PipeOutputRead);

    try
      while True do
      begin
        Succeed := ReadFile(PipeErrorsRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then Break;
      end;
    finally
    end;
    CloseHandle(PipeErrorsRead);

    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
  begin
    Result := False;
    CloseHandle(PipeOutputRead);
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsRead);
    CloseHandle(PipeErrorsWrite);
  end;
  frmDebug.Compiling := False;
end;


procedure KillIt(dwProcID: DWORD);
var 
  hProcess : Cardinal; 
begin 
  hProcess := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, dwProcID);
  TerminateProcess(hProcess, 0); 
end;

function GetProcID(sProcName: String): Integer;
var
  hProcSnap: THandle; 
  pe32: TProcessEntry32; 
begin 
  result := -1; 
  hProcSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0); 
  if hProcSnap = INVALID_HANDLE_VALUE then exit; 

  pe32.dwSize := SizeOf(ProcessEntry32); 

  if Process32First(hProcSnap, pe32) = true then
    while Process32Next(hProcSnap, pe32) = true do 
    begin 
      if pos(sProcName, pe32.szExeFile) <> 0 then 
        result := pe32.th32ProcessID; 
    end; 
end;

procedure DoCompile;
var eStr: TStringList;
    i: integer;
    eFound: Boolean;
begin
  if (FileExists(frmSettings.txtAMXXPath.Text + 'scripting\amxxsc.exe')) and (FileExists(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini')) then begin
    if frmMain.dtcEditor.ActiveDocument.IsUntitled then begin
      if DirectoryExists(frmSettings.txtSave.Text) then
        frmMain.sdSave.InitialDir := frmSettings.txtSave.Text;

      if frmMain.sdSave.Execute then begin
        try
          AppendFileExt;
          frmMain.dtcEditor.ActiveDocument.Modified := False;
          frmMain.dtcEditor.ActiveDocument.FileName := frmMain.sdSave.FileName;
          frmMain.sciEditor.SaveToFile(frmMain.dtcEditor.ActiveDocument.FileName);
        except
          // :F
        end;
      end
      else
        exit;
    end
    else
      frmMain.acSave.Execute;
    Screen.Cursor := crHourGlass;
    if frmSettings.chkAutoAddPlugins.Checked then begin
      eStr := TStringList.Create;
      eStr.LoadFromFile(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini');
      eFound := False;
      for i := 0 to eStr.Count -1 do begin
        if Pos(ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx'), TrimLeft(eStr[i])) = 1 then
          eFound := True;
      end;
      if not eFound then begin
        eStr.Add(ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx'));
        eStr.SaveToFile(frmSettings.txtAMXXPath.Text + 'configs\plugins.ini');
      end;
      eStr.Free;
    end;
    try
      frmMain.atbToolBar.RecreateControls;
    except
      // :F
    end;
    
    if frmSettings.optWindow.Checked then begin
      frmDebug.lblFile.Caption :=     ' File: ' + ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName);
      frmDebug.lblStatus.Font.Style := [];
      frmDebug.lblStatus.Caption :=   ' Status: Compiling...';
      frmDebug.lblHints.Caption :=    ' Hints: 0';
      frmDebug.lblWarnings.Caption := ' Warnings: 0';
      frmDebug.lblErrors.Caption :=   ' Errors: 0';
      frmDebug.lstOutput.Items.Text := 'Compiler Output:';
      frmDebug.Show;
      frmDebug.Repaint;
    end
    else begin
      frmMain.lvDebug.Clear;
      frmMain.lvDebug.Items.Add.Caption := 'Compiling ' + ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName) + '...';
      frmMain.lvDebug.Visible := True;
      frmMain.Repaint;
    end;
    GetConsoleOutput(frmSettings.txtAMXXPath.Text + 'scripting\amxxsc.exe ' +
                                                     '"' + frmMain.dtcEditor.ActiveDocument.FileName + '" ' +
                                                     '"-o' + frmSettings.txtAMXXPath.Text + 'plugins\' + ChangeFileExt(ExtractFileName(frmMain.dtcEditor.ActiveDocument.FileName), '.amxx') + '"');
    Screen.Cursor := crDefault;
  end
  else
    MessageBox(frmMain.Handle, 'Couldn''t find amxxsc.exe or plugins.ini. Check your settings and try again.', 'Error', MB_ICONERROR);
end;

function ShowSaveDialog(Caption, SaveCaption, CloseCaption: String): Boolean;
function TabByName(eName: String): TSciDoc;
var i: integer;
begin
  Result := nil;
  for i := 0 to frmMain.dtcEditor.Count -1 do begin
    if frmMain.dtcEditor.Tabs[i] = eName then
      Result := frmMain.dtcEditor.Document[i];
  end;
end;

var i: integer;
    eStr: TStringList;
begin
  eStr := TStringList.Create;
  frmSaveDialog.Caption := Caption;
  frmSaveDialog.SaveCaption := SaveCaption;
  frmSaveDialog.CloseCaption := CloseCaption;
  frmSaveDialog.cmdSave.Caption := CloseCaption;
  frmSaveDialog.lstFiles.Clear;
  for i := 0 to frmMain.dtcEditor.Tabs.Count -1 do begin
    if (frmMain.dtcEditor.Document[i].Modified) then
      frmSaveDialog.lstFiles.Items.Add(frmMain.dtcEditor.Tabs[i])
    else if (frmMain.dtcEditor.Document[i].IsUntitled) then
      frmSaveDialog.lstFiles.Items.Add(frmMain.dtcEditor.Tabs[i]);
  end;

  if (frmMain.dtcEditor.Tabs.Count = 1) and (frmMain.sciEditor.Lines.Text = '') then
    frmSaveDialog.lstFiles.Clear;

  if frmSaveDialog.lstFiles.Items.Count = 0 then begin
    Result := True;
    if frmSettings.chkReload.Checked then begin
      for i := 0 to frmMain.dtcEditor.Tabs.Count -1 do begin
        if not frmMain.dtcEditor.Document[i].IsUntitled then
          eStr.Add(frmMain.dtcEditor.Document[i].FileName); 
      end;
      eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Files.ini');
    end;
    eStr.Free;
    exit;
  end;

  for i := 0 to frmSaveDialog.lstFiles.Items.Count -1 do begin
    if Pos('&', frmSaveDialog.lstFiles.Items[i]) = 1 then
      frmSaveDialog.lstFiles.Items[i] := Copy(frmSaveDialog.lstFiles.Items[i], 2, Length(frmSaveDialog.lstFiles.Items[i]));
  end;

  if frmSaveDialog.ShowModal = mrOk then begin
    for i := 0 to frmSaveDialog.lstFiles.Items.Count -1 do begin
      if frmSaveDialog.lstFiles.Checked[i] then begin
        if (TabByName(frmSaveDialog.lstFiles.Items[i]).IsUntitled) then begin
          frmMain.sdSave.Title := Format('Save %s (Tab %s)', [TabByName(frmSaveDialog.lstFiles.Items[i]).FileName, IntToStr(TabByName(frmSaveDialog.lstFiles.Items[i]).Index +1)]);
          if frmMain.sdSave.Execute then begin
            AppendFileExt;
            frmMain.dtcEditor.Activate(TabByName(frmSaveDialog.lstFiles.Items[i]).Index);
            frmMain.sciEditor.SaveToFile(frmMain.sdSave.FileName);
            frmMain.dtcEditor.ActiveDocument.FileName := frmMain.sdSave.FileName;
            frmMain.SetSaved;
            frmMain.sbInfo.Panels[1].Text := '';
          end
          else begin
            frmMain.sdSave.Title := 'Save...';
            eStr.Free;
            Result := False;
            exit;
          end;
          frmMain.sdSave.Title := 'Save...';
        end
        else begin
          frmMain.dtcEditor.Activate(TabByName(frmSaveDialog.lstFiles.Items[i]).Index);
          frmMain.sciEditor.SaveToFile(TabByName(frmSaveDialog.lstFiles.Items[i]).FileName);
          frmMain.dtcEditor.ActiveDocument.Modified := False;
          frmMain.SetSaved;
          frmMain.sbInfo.Panels[1].Text := '';
        end;
      end;
    end;
    for i := 0 to frmMain.dtcEditor.Tabs.Count -1 do begin
      if (not frmMain.dtcEditor.Document[i].IsUntitled) and (not frmMain.dtcEditor.Document[i].Modified) then
        eStr.Add(frmMain.dtcEditor.Document[i].FileName);
    end;
    
    if frmSettings.chkReload.Checked then
      eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Files.ini'); 
    Result := True;
  end
  else
    Result := False;
  eStr.Free;
end;

procedure AppendFileExt;
var eExt: String;
begin
  eExt := LowerCase(ExtractFileExt(frmMain.sdSave.FileName));
  if (frmMain.sdSave.FilterIndex = 1) and (eExt <> '.sma') then
    frmMain.sdSave.FileName := frmMain.sdSave.FileName + '.sma'
  else if (frmMain.sdSave.FilterIndex = 2) and (eExt <> '.inc') then
    frmMain.sdSave.FileName := frmMain.sdSave.FileName + '.inc';
end;

function RemoveSpaces(eInput: String): String;
begin
  eInput := StringReplace(eInput, ' ', '', [rfReplaceAll]);
  eInput := StringReplace(eInput, '	', '', [rfReplaceAll]);
  Result := eInput;
end;

end.
