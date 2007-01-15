unit UnitCompile;

interface

uses SysUtils, Classes, Windows, Forms, Controls, ShellAPI, Messages, IdFTP,
  IdFTPCommon;

type TPAWNCompileThread = class(TThread)
  protected
    Stream: TStringStream;

    Output: TStringList;
    Finished: Boolean;
    procedure Execute; override;
    procedure ProcessItem(eLineStr: String);
    procedure AddOutput;
    procedure StartHL;
    procedure Upload;
  public
    FileName: string;
    Compiler: string;
    Args: string;
    Target: string;
    Flags: Integer;
  end;

function DoCompilePAWN(eFlags: Integer): Boolean;

var Compiling: Boolean;

implementation

uses UnitfrmSettings, UnitLanguages, UnitMainTools, UnitfrmMain,
  UnitCodeUtils, UnitPlugins;

function DoCompilePAWN(eFlags: Integer): Boolean;
var eFile: string;
begin
  Result := False;
  if (Compiling) then exit;
  if not FileExists(frmSettings.txtPAWNCompilerPath.Text) then begin
    MessageBox(frmMain.Handle, PChar(lPAWNCompilerNotFound), PChar(Application.Title), MB_ICONERROR);
    exit;
  end;

  Screen.Cursor := crHourGlass;
  Compiling := True;
  if (ActiveDoc.Untitled) then
    eFile := ExtractFilePath(ParamStr(0)) + 'Untitled.sma'
  else
    eFile := ActiveDoc.FileName;
  frmMain.sciEditor.Lines.SaveToFile(eFile);

  if Plugin_VisibleControlChange(CTRL_OUTPUT, True) then begin
    frmMain.lstOutput.Clear;
    frmMain.splOutput.Show;
    frmMain.lstOutput.Show;
    Plugin_VisibleControlChange(CTRL_OUTPUT, True);
  end;

  with TPawnCompileThread.Create(True) do begin
    FileName := eFile;
    Compiler := frmSettings.txtPAWNCompilerPath.Text;
    if DirectoryExists(frmSettings.txtPAWNOutput.Text) then
      Target := IncludeTrailingPathDelimiter(frmSettings.txtPAWNOutput.Text) + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')
    else
      Target := ChangeFileExt(eFile, '.amxx');

    Args := frmSettings.txtPAWNArgs.Text;
    if Args <> '' then
      Args := Args + #32;
    Flags := eFlags;
      
    Resume;
  end;
end;

{ TPAWNCompileThread }

procedure TPAWNCompileThread.ProcessItem(eLineStr: String);
var eLine: Integer;
    eTemp: String;
begin
  eLine := -1;
  if Pos(LowerCase(FileName), LowerCase(eLineStr)) = 1 then begin
    Delete(eLineStr, 1, Length(FileName));
    if IsNumeric(Between(eLineStr, '(', ')')) then
      eLine := StrToInt(Between(eLineStr, '(', ')'))
    else begin
      eTemp := Between(eLineStr, '(', ')');
      eTemp := Copy(eTemp, 1, Pos(#32, eTemp) -1);
      eLine := StrToInt(eTemp)
    end;

    eTemp := Between(eLineStr, ':', ':');

    Delete(eLineStr, 1, Pos(':', eLineStr) +1);
    Delete(eLineStr, 1, Pos(':', eLineStr) +1);
    if eLineStr <> '' then
      eLineStr[1] := UpperCase(eLineStr[1])[1];
    if Pos('error', eTemp) <> 0 then
      eLineStr := Format(lError, [Trim(eLineStr), eLine])
    else if Pos('warning', eTemp) <> 0 then
      eLineStr := Format(lWarning, [Trim(eLineStr), eLine])
    else
      eLineStr := Format(lOther, [Trim(eLineStr), eLine]);
  end;
  
  if frmMain.lstOutput.ItemIndex = -1 then begin
    if Pos('error', eTemp) <> 0 then begin
      frmMain.lstOutput.SetFocus;
      frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add(eLineStr);
      frmMain.SetErrorLine(eLine);
    end
    else if eLineStr = 'Done.' then begin
      if (DirectoryExists(GetAMXXDir(True) + 'plugins\')) and (GetAMXXDir(True) <> '') then begin
        if LowerCase(IncludeTrailingPathDelimiter(frmSettings.txtPAWNOutput.Text)) <> LowerCase(GetAMXXDir(True) + 'plugins\') then begin
          if FileExists(GetAMXXDir(True) + 'plugins\' + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')) then
            DeleteFile(PChar(GetAMXXDir(True) + 'plugins\' + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')));
          if frmSettings.txtPAWNOutput.Text = '' then
            CopyFile(PChar(ChangeFileExt(ActiveDoc.FileName, '.amxx')), PChar(GetAMXXDir(True) + 'plugins\' + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')), False)
          else
            CopyFile(PChar(frmSettings.txtPAWNOutput.Text + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')), PChar(GetAMXXDir(True) + 'plugins\' + ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')), False);
          frmMain.lstOutput.Items.Add('Copied output file to: ' + GetAMXXDir(True)+ 'plugins\');
        end;
      end;

      if Flags = COMP_STARTHL then // Start HL
        Synchronize(StartHL)
      else if Flags = COMP_UPLOAD then
        Synchronize(Upload)
      else begin
        frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add('Done.');
        frmMain.lstOutput.Perform(WM_VSCROLL, SB_BOTTOM, 0);
      end;
      Plugin_Compile(Flags, GetCurrLang.Name, ActiveDoc.FileName, False);
    end
    else begin
      frmMain.lstOutput.Items.Add(eLineStr);
      frmMain.lstOutput.Perform(WM_VSCROLL, SB_BOTTOM, 0);
    end;
  end
  else
    frmMain.lstOutput.Items.Add(eLineStr);
end;

procedure TPAWNCompileThread.AddOutput;
var i, eIndex: integer;
begin
 if Output.Count > 1 then begin
    eIndex := frmMain.lstOutput.ItemIndex;
    frmMain.lstOutput.Items.Clear;
    if Finished then begin
      for i := 0 to Output.Count -1 do
        ProcessItem(Output[i]);
    end
    else begin
      for i := 0 to Output.Count -2 do
        ProcessItem(Output[i]);
    end;
    frmMain.lstOutput.ItemIndex := eIndex;
    frmMain.Repaint;
    Application.ProcessMessages;
  end;
end;

procedure TPAWNCompileThread.Execute;
var StartupInfo: TStartupInfo;
    ProcessInfo: TProcessInformation;
    SecurityAttr: TSecurityAttributes;
    PipeOutputRead: THandle;
    PipeOutputWrite: THandle;
    PipeErrorsRead: THandle;
    PipeErrorsWrite: THandle;
    Succeed: Boolean;
    Buffer: array[0..255] of Char;
    NumberOfBytesRead: DWORD;
begin
  Output := TStringList.Create;
  FillChar(ProcessInfo, SizeOf(TProcessInformation), 0);
  FillChar(SecurityAttr, SizeOf(TSecurityAttributes), 0);
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.bInheritHandle := True;
  SecurityAttr.lpSecurityDescriptor := nil;
  CreatePipe(PipeOutputRead, PipeOutputWrite, @SecurityAttr, 0);
  CreatePipe(PipeErrorsRead, PipeErrorsWrite, @SecurityAttr, 0);
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := PipeOutputWrite;
  StartupInfo.hStdError := PipeErrorsWrite;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  if CreateProcess(nil, PChar(Compiler + ' "' + FileName + '" ' + Args + '"-o' + Target + '"'), nil, nil, True, CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo) then begin
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsWrite);

    Stream := TStringStream.Create('');
    try
      Finished := False;
      while True do begin
        Succeed := ReadFile(PipeOutputRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then break;
        Stream.Write(Buffer, NumberOfBytesRead);
        Output.Text := Stream.DataString;
        Synchronize(AddOutput);
      end;
      Finished := True;
      Synchronize(AddOutput);
    finally
      Stream.Free;
    end;
    CloseHandle(PipeOutputRead);
    try
      while True do begin
        Succeed := ReadFile(PipeErrorsRead, Buffer, 255, NumberOfBytesRead, nil);
        if not Succeed then Break;
        { and here the errors }
      end;
    finally
    end;
    CloseHandle(PipeErrorsRead);
    WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hProcess);
  end
  else begin
    CloseHandle(PipeOutputRead);
    CloseHandle(PipeOutputWrite);
    CloseHandle(PipeErrorsRead);
    CloseHandle(PipeErrorsWrite);
  end;
  Screen.Cursor := crDefault;
  Compiling := False;
  Output.Free;
end;

procedure TPAWNCompileThread.StartHL;
begin
  frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add('Done.');
  frmMain.lstOutput.Items.Add('');
  frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add(lStartingHalfLife);
  if (FileExists(frmSettings.txtHLExec.Text)) and (frmSettings.txtHLExec.Text <> '') then begin
    ShellExecute(frmMain.Handle, 'open', PChar(frmSettings.txtHLExec.Text), PChar(frmSettings.txtCustomParameters.Text), PChar(ExtractFilePath(frmSettings.txtHLExec.Text)), SW_SHOW);
    frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add('Done.');
  end
  else begin
    frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add(lHLNotFound);
    frmMain.lstOutput.ItemIndex := frmMain.lstOutput.Items.Add(lCheckSettingsTryAgain);
    MessageBeep(MB_ICONWARNING);
  end;
end;

procedure TPAWNCompileThread.Upload;
procedure AddOutput(eItem: String);
var eAddedIndex: Integer;
begin
  eAddedIndex := frmMain.lstOutput.Items.Add(eItem);

  frmMain.lstOutput.ItemIndex := eAddedIndex;
  repeat
    Delay(50);
    frmMain.lstOutput.Repaint;
  until frmMain.lstOutput.ItemIndex = eAddedIndex;
end;

begin
  AddOutput('Done.');
  if frmMain.IdFTP.Connected then
    frmMain.IdFTP.Disconnect;

  AddOutput('');
  AddOutput(lConnecting);

  if TryConnect = 0 then begin
    AddOutput(lChangingDir);

    try
      frmMain.IdFTP.ChangeDir(frmSettings.txtDefaultDir.Text + 'plugins/');
      AddOutput(lUploadingFile);
    except
      MessageBox(frmMain.Handle, PChar(lInvalidDirectory), PChar(Application.Title), MB_ICONERROR);
      AddOutput(lUploadFailed);

      if frmMain.IdFTP.Connected then
        frmMain.IdFTP.Disconnect;
      exit;
    end;

    try
      frmMain.IdFTP.TransferType := ftBinary;
      frmMain.IdFTP.Put(Target, ExtractFileName(Target));
      AddOutput(lDone);
    except
      on E: Exception do begin
        MessageBox(frmMain.Handle, PChar(lErrorUpload + #13 + E.Message), PChar(Application.Title), MB_ICONERROR);
        AddOutput(lUploadFailed);
      end;
    end;
    
    if frmMain.IdFTP.Connected then
      frmMain.IdFTP.Disconnect;
  end
  else
    AddOutput(lUploadFailed);
end;

end.

