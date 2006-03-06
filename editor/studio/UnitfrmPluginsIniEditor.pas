unit UnitfrmPluginsIniEditor;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, TBXDkPanels, SpTBXDkPanels, mbTBXMemo, Dialogs,
  IdFTPCommon, SpTBXControls;

type
  TfrmPluginsIniEditor = class(TForm)
    txtFile: TmbTBXMemo;
    cmdOK: TSpTBXButton;
    cmdCancel: TSpTBXButton;
    pnlEditType: TPanel;
    chkEditFTP: TSpTBXCheckBox;
    chkEditLocal: TSpTBXCheckBox;
    cmdRemove: TSpTBXButton;
    odOpen: TOpenDialog;
    procedure chkEditFTPClick(Sender: TObject);
    procedure chkEditLocalClick(Sender: TObject);
    procedure cmdRemoveClick(Sender: TObject);
  end;

var
  frmPluginsIniEditor: TfrmPluginsIniEditor;

implementation

uses UnitfrmMain, UnitfrmSettings, UnitMainTools, UnitLanguages;

{$R *.DFM}

procedure TfrmPluginsIniEditor.chkEditFTPClick(Sender: TObject);
begin
  if Screen.Cursor = crHourGlass then exit;

  Screen.Cursor := crHourGlass;

  if not frmMain.IdFTP.Connected then begin
    if TryConnect <> 0 then begin
      cmdRemove.Enabled := False;
      exit;
    end;
  end;

  try
    frmMain.IdFTP.ChangeDir(frmSettings.txtDefaultDir.Text + 'configs/');
  except
    frmMain.IdFTP.Disconnect;
    MessageBox(Application.Handle, PChar(lInvalidDirectory), PChar(Application.Title), MB_ICONERROR);
    chkEditFTP.Checked := False;
    chkEditLocal.Checked := True;
    cmdRemove.Enabled := False;
    Screen.Cursor := crDefault;
    exit;
  end;

  frmMain.IdFTP.TransferType := ftASCII;
  frmMain.IdFTP.Get('plugins.ini', ExtractFilePath(ParamStr(0)) + 'plugins.ini', True);
  frmPluginsIniEditor.txtFile.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'plugins.ini');
  DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'plugins.ini'));

  chkEditFTP.Checked := True;
  chkEditLocal.Checked := False;
  cmdRemove.Enabled := True;
  Screen.Cursor := crDefault;
end;

procedure TfrmPluginsIniEditor.chkEditLocalClick(Sender: TObject);
begin
  if Screen.Cursor = crHourGlass then exit;

  Screen.Cursor := crHourGlass;
  if not FileExists(GetAMXXDir(True) + 'configs\plugins.ini') then begin
    if odOpen.Execute then begin
      txtFile.Lines.LoadFromFile(odOpen.FileName);
      cmdRemove.Enabled := Pos('amxmodx', LowerCase(odOpen.FileName)) <> 0;
    end
    else
      cmdRemove.Enabled := False;
  end
  else begin
    txtFile.Lines.LoadFromFile(GetAMXXDir(True) + 'configs\plugins.ini');
    odOpen.FileName := GetAMXXDir(True) + 'configs\plugins.ini';
    cmdRemove.Enabled := True;
  end;
  chkEditFTP.Checked := False;
  chkEditLocal.Checked := True;
  Screen.Cursor := crDefault;
end;

procedure TfrmPluginsIniEditor.cmdRemoveClick(Sender: TObject);
function RemComments(eLine: String): String;
var a, b: integer;
begin
  if Length(eLine) > 0 then begin
    b := Length(eLine) +1;
    for a := 1 to Length(eLine) -1 do begin
      if (eLine[a] = ';') or (eLine[a] = '/') then begin
        b := a;
        break;
      end;
    end;

    if (b = 0) and (Pos(' debug', LowerCase(eLine)) <> 0) then
      b := Pos(' debug', LowerCase(eLine));
    if b <> 0 then
      eLine := Trim(Copy(eLine, 1, b -1));
  end;
  Result := Trim(eLine);
end;

var eStr: TStringList;
    a,b: integer;
    eFound: Boolean;
begin
  Screen.Cursor := crHourGlass;
  if chkEditFTP.Checked then begin
    if not frmMain.IdFTP.Connected then begin
      if TryConnect <> 0 then begin
        Screen.Cursor := crDefault;
        exit;
      end;
    end;

    eStr := TStringList.Create;
    try
      frmMain.IdFTP.ChangeDir(frmSettings.txtDefaultDir.Text + 'plugins/');
      frmMain.IdFTP.List(eStr, '', False);
      for a := txtFile.Lines.Count -1 downto 0 do begin
        if (Copy(txtFile.Lines[a], 1, 1) <> ';') and (Copy(txtFile.Lines[a], 1, 1) <> '/') and (Trim(txtFile.Lines[a]) <> '') then begin
          eFound := False;
          for b := 0 to eStr.Count -1 do begin
            if RemComments(txtFile.Lines[a]) = eStr[b] then
              eFound := True;
          end;

          if not eFound then
            txtFile.Lines.Delete(a); 
        end;
      end;
    except
      MessageBox(Application.Handle, PChar(lInvalidDirectory), PChar(Application.Title), MB_ICONERROR);
    end;
    
    eStr.Free;
  end
  else begin
    for a := txtFile.Lines.Count -1 downto 0 do begin
      if (Copy(txtFile.Lines[a], 1, 1) <> ';') and (Copy(txtFile.Lines[a], 1, 1) <> '/') then begin
        if (not FileExists(Copy(ExtractFilePath(odOpen.FileName), 1, Length(ExtractFilePath(odOpen.FileName)) -8) + 'plugins\' + RemComments(txtFile.Lines[a]))) and (Trim(txtFile.Lines[a]) <> '') then
          txtFile.Lines.Delete(a);
      end;
    end;
  end;
  Screen.Cursor := crDefault;
end;

end.
