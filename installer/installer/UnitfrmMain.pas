unit UnitfrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, mxFlatControls, JvPageList,
  ExtCtrls, JvExControls, JvComponent, TFlatButtonUnit, jpeg, TFlatEditUnit,
  TFlatGaugeUnit, ImgList, FileCtrl, Registry, CheckLst, TFlatComboBoxUnit,
  TFlatCheckBoxUnit, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdFTP, IdException, IdAntiFreezeBase, IdAntiFreeze,
  IdIntercept, IdLogBase, IdLogFile, JclFileUtils, TFlatRadioButtonUnit;

type
  TfrmMain = class(TForm)
    jplWizard: TJvPageList;
    jspWelcome: TJvStandardPage;
    pnlButtons: TPanel;
    bvlSpace: TBevel;
    cmdNext: TFlatButton;
    cmdCancel: TFlatButton;
    imgInstall: TImage;
    lblWelcome: TLabel;
    lblInfo1: TLabel;
    lblInfo2: TLabel;
    lblInfo3: TLabel;
    jspLicense: TJvStandardPage;
    pnlLicense: TPanel;
    imgIcon1: TImage;
    lblTitle1: TLabel;
    lblSubTitle1: TLabel;
    freLicense: TmxFlatRichEdit;
    frbAgree: TFlatRadioButton;
    ftbDontAgree: TFlatRadioButton;
    jspInstallMethod: TJvStandardPage;
    pnlHeader2: TPanel;
    imgIcon2: TImage;
    lblTitle2: TLabel;
    lblSubTitle2: TLabel;
    cmdBack: TFlatButton;
    jspFTP: TJvStandardPage;
    pnlHeader3: TPanel;
    imgIcon3: TImage;
    lblTitle3: TLabel;
    lblSubTitle3: TLabel;
    lblStep1: TLabel;
    pnlFTPData: TPanel;
    lblHost: TLabel;
    txtHost: TFlatEdit;
    lblUserName: TLabel;
    txtUserName: TFlatEdit;
    txtPassword: TFlatEdit;
    lblPassword: TLabel;
    txtPort: TFlatEdit;
    lblPort: TLabel;
    lblStep2: TLabel;
    cmdConnect: TFlatButton;
    pnlDirectory: TPanel;
    trvDirectories: TTreeView;
    jspInstallProgress: TJvStandardPage;
    pnlHeader5: TPanel;
    imgIcon5: TImage;
    lblTitle5: TLabel;
    lblSubTitle5: TLabel;
    ggeAll: TFlatGauge;
    lblProgress: TLabel;
    ggeItem: TFlatGauge;
    rtfDetails: TmxFlatRichEdit;
    lblDetails: TLabel;
    bvlSpace2: TBevel;
    ilImages: TImageList;
    bvlSpacer1: TBevel;
    bvlSpacer2: TBevel;
    bvlSpacer3: TBevel;
    bvlSpacer5: TBevel;
    jspSelectMod: TJvStandardPage;
    pnlSelectMod: TPanel;
    imgIcon6: TImage;
    lblSelectMod: TLabel;
    lblSelectModInfo: TLabel;
    bvlSelectMod: TBevel;
    lblInfo: TLabel;
    chkPassive: TFlatCheckBox;
    IdFTP: TIdFTP;
    cmdProxySettings: TFlatButton;
    IdAntiFreeze: TIdAntiFreeze;
    lblStep4: TLabel;
    cboGameAddon: TFlatComboBox;
    tmrSpeed: TTimer;
    IdLogFile: TIdLogFile;
    lblInfo4: TLabel;
    trvMods: TTreeView;
    lblRemoteInstallation: TLabel;
    lblRemoteHint: TLabel;
    pnlRemote: TPanel;
    frbFTP: TFlatRadioButton;
    lblLocalHint: TLabel;
    lblLocalInstallation: TLabel;
    lblLocalHintItalic: TLabel;
    pnlLocal: TPanel;
    frbDedicatedServer: TFlatRadioButton;
    frbListenServer: TFlatRadioButton;
    frbStandaloneServer: TFlatRadioButton;
    frbSelectMod: TFlatRadioButton;
    shpMods: TShape;
    lblSelectModNote: TLabel;
    lblStep3: TLabel;
    pnlOS: TPanel;
    optWindows: TFlatRadioButton;
    optLinux: TFlatRadioButton;
    lblStep5: TLabel;
    lblFTP: TLabel;
    optMac: TFlatRadioButton;
    procedure jvwStepsCancelButtonClick(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure CheckNext(Sender: TObject);
    procedure cmdBackClick(Sender: TObject);
    procedure cmdConnectClick(Sender: TObject);
    procedure jplWizardChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdProxySettingsClick(Sender: TObject);
    procedure txtPortChange(Sender: TObject);
    procedure trvDirectoriesExpanded(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure IdFTPWork(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCount: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrSpeedTimer(Sender: TObject);
    procedure trvDirectoriesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure trvDirectoriesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure jspFTPShow(Sender: TObject);
    procedure frbFTPClick(Sender: TObject);
    procedure frbLocalClick(Sender: TObject);
    procedure trvModsClick(Sender: TObject);
    procedure trvDirectoriesMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure trvDirectoriesChange(Sender: TObject; Node: TTreeNode);
  private
    OldProgress: Integer;
    CurrProgress: Integer;
  public
    procedure ExceptionHandler(Sender: TObject; E: Exception);
  end;

var
  frmMain: TfrmMain;
  gMultiAccount: Boolean;

const VERSION = '1.8.2';

implementation

uses UnitFunctions, UnitScanMods, UnitfrmProxy, UnitInstall,
  UnitSelectModPath;

{$R *.dfm}

procedure TfrmMain.jvwStepsCancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cmdCancelClick(Sender: TObject);
begin
  if (jplWizard.ActivePage = jspFTP) and (cmdConnect.Caption = 'Connecting...') then begin
    Screen.Cursor := crDefault;
    Cancel := True;
    try
      IdFTP.Disconnect;
    except
      // oh, hello BAILOPAN!
    end;
    cmdCancel.Caption := 'Close';
  end
  else if (jplWizard.ActivePage = jspInstallProgress) then begin
   if Cancel then
     Close
   else if MessageBox(Handle, 'Do you really want to cancel the installation?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
      Screen.Cursor := crDefault;
      Application.OnException := ExceptionHandler;
      Cancel := True;
      if IdFTP.Connected then
        IdFTP.Quit;
    end;
    cmdCancel.Caption := 'Close';
  end
  else
    Close;
end;

procedure TfrmMain.cmdNextClick(Sender: TObject);
var ePath: String;
    eRegistry: TRegistry;
    ChosenMod: TMod;
    eStr: TStringList;
    CurNode: TTreeNode;
    eOS: TOS;
    i, k: integer;
begin
  if jplWizard.ActivePage = jspFTP then begin
    Screen.Cursor := crHourGlass;
    try
      if not IdFTP.Connected then
        IdFTP.Connect;
    except
      MessageBox(Handle, 'Cannot connect to server. Please check your connection and try again.', 'Error', MB_ICONWARNING);
      Screen.Cursor := crDefault;
      exit;
    end;

    { FTP }
    eStr := TStringList.Create;
    ePath := '/';
    CurNode := trvDirectories.Selected;
    if (Assigned(CurNode)) then begin
      repeat
        ePath := '/' + CurNode.Text + ePath;
        CurNode := CurNode.Parent;
      until (not Assigned(CurNode));
    end;
    
    try
      IdFTP.ChangeDir(ePath);
    except
      MessageBox(Handle, PChar('Cannot change directory to "' + ePath + '". Please check your settings and try again.'), 'Error', MB_ICONWARNING);
      Screen.Cursor := crDefault;
      exit;
    end;

    try
      IdFTP.List(eStr, '', False);
    except
      // worst "exception" ever. bad indy!
    end;

    if eStr.IndexOf('liblist.gam') = -1 then begin
      MessageBox(Handle, 'Invalid directory. Please select your mod directory and try again.', PChar(Application.Title), MB_ICONWARNING);
      eStr.Free;
      Screen.Cursor := crDefault;
      exit;
    end
    else
      eStr.Free;

    // design stuff
    trvDirectories.Enabled := False;
    cmdConnect.Enabled := False;
    optWindows.Enabled := False;
    optLinux.Enabled := False;
    optMac.Enabled := False;
    cboGameAddon.Enabled := False;
    // preinstall...
    MakeDir(ExtractFilePath(Application.ExeName) + 'temp');
    DownloadFile('liblist.gam', ExtractFilePath(Application.ExeName) + 'temp\liblist.gam');
    try
      IdFTP.ChangeDir(ePath + 'addons/metamod/');
      ForceDirectories(ExtractFilePath(Application.ExeName) + 'temp\addons\metamod\');
      DownloadFile('plugins.ini', ExtractFilePath(Application.ExeName) + 'temp\addons\metamod\plugins.ini');
    except
      try
        IdFTP.ChangeDir(ePath);
      except
        MessageBox(Handle, PChar('Cannot change directory to "' + ePath + '". Please check your settings and try again.'), 'Error', MB_ICONWARNING);
        Screen.Cursor := crDefault;
        exit;
      end;
    end;
    ChosenMod := modNone;
    case cboGameAddon.ItemIndex of
      1: ChosenMod := modCS;
      2: ChosenMod := modDoD;
      3: ChosenMod := modNS;
      4: ChosenMod := modTFC;
      5: ChosenMod := modTS;
      6: ChosenMod := modCS;
      7: ChosenMod := modESF;
    end;

    if optWindows.Checked then
      eOS := osWindows
    else if optLinux.Checked then
      eOS := osLinux
    else
      eOS := osMac;

    jspInstallProgress.Show;
    frmMain.Height := 382;
    rtfDetails.Lines.Text := 'Starting Pre-Installation, this may take a few minutes...';
    rtfDetails.Lines.Add('');
    Sleep(1500);
    ggeAll.Progress := 0;
    ggeItem.Progress := 0;
    cmdNext.Hide;
    InstallCustom(ExtractFilePath(Application.ExeName) + 'temp\', ChosenMod, eOS);
    if Cancel then
      exit;
    AddStatus('', clBlack, False);
    AddStatus('', clBlack, False);
    AddStatus('- - - - -', clBlack, False);
    AddStatus('Uploading all files...', clBlack, False);
    AddStatus('', clBlack, False);
    Sleep(1500);
    // ... then upload ...
    ggeAll.Progress := 0;
    ggeItem.Progress := 0;
    InstallFTP(ChosenMod, eOS);
  end
  else if jplWizard.ActivePage = jspInstallProgress then
    Close
  else if jplWizard.ActivePage = jspSelectMod then begin
    { Dedicated Server }
    if (frbDedicatedServer.Checked) or (frbStandaloneServer.Checked) then begin
      jspInstallProgress.Show;
      ChosenMod := modNone;
      ePath := LowerCase(GetModPathName(trvMods.Selected.Text));
      if gMultiAccount then
        SteamPath := GetSteamAppsDir + trvMods.Selected.Parent.Text + '\dedicated server\'; // setting this path for a user with only one account is not necessary
      // ask for additional mods...
      if (ePath = 'cstrike') or (ePath = 'czero') then begin
        if MessageBox(Handle, 'Install Counter-Strike addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modCS;
      end;
      if ePath = 'dod' then begin
        if MessageBox(Handle, 'Install Day of Defeat addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modDoD;
      end;
      if ePath = 'ns' then begin
        if MessageBox(Handle, 'Install Natural Selection addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modNS;
      end;
      if ePath = 'tfc' then begin
        if MessageBox(Handle, 'Install Team Fortress Classic addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modTFC;
      end;
      if ePath = 'ts' then begin
        if MessageBox(Handle, 'Install The Specialists addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modTS;
      end;
      if ePath = 'esf' then begin
        if MessageBox(Handle, 'Install Earth''s Special Forces addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
          ChosenMod := modESF;
      end;
      // install it
      if frbDedicatedServer.Checked then begin
        if DirectoryExists(SteamPath + ePath) then
          InstallDedicated(SteamPath + ePath + '\', ChosenMod, True)
        else begin
          MessageBox(Handle, 'Error: The directory of the mod you selected doesn''t exist any more. Run Dedicated Server with the chosen mod and try again.', PChar(Application.Title), MB_ICONERROR);
          Application.Terminate;
          exit;
        end;
      end
      else begin
        if DirectoryExists(StandaloneServer + ePath) then
          InstallDedicated(StandaloneServer + ePath + '\', ChosenMod, False)
        else begin
          MessageBox(Handle, 'Error: The directory of the mod you selected doesn''t exist (any more). Run Half-Life Dedicated Server with the chosen mod again and restart.', PChar(Application.Title), MB_ICONERROR);
          Application.Terminate;
          exit;
        end;
      end;
    end;
    { Listen Server }
    if frbListenServer.Checked then begin
      ChosenMod := modNone;
      if gMultiAccount then
        SteamPath := GetSteamAppsDir + trvMods.Selected.Parent.Text + '\'; // setting this path for a user with only one account is not necessary
      ePath := trvMods.Selected.Text;
      if DirectoryExists(SteamPath + ePath + '\' + GetModPathName(ePath)) then
        ePath := SteamPath + ePath + '\' + GetModPathName(ePath)
      else if DirectoryExists(SteamPath + 'half-life\' + ePath) then
        ePath := SteamPath + 'half-life\' + ePath
      else if DirectoryExists(SteamPath + 'half-life\' + GetModPathName(ePath)) then
        ePath := SteamPath + 'half-life\' + GetModPathName(ePath)
      else if DirectoryExists(SteamPath + GetModPathName(ePath)) then
        ePath := SteamPath + GetModPathName(ePath);

      if Pos(SteamPath, ePath) = 0 then
        MessageBox(Handle, 'An error occured. Please report this bug to the AMX Mod X team and post a new thread on the forums of www.amxmodx.org.', PChar(Application.Title), MB_ICONSTOP)
      else begin
        ePath := LowerCase(ePath); // fixes case-sensivity bug

        if not FileExists(ePath + '\liblist.gam') then begin
          // added for HLDM
          if FileExists(ExtractFilePath(ePath) + 'liblist.gam') then
            ePath := ExtractFilePath(ePath)
          else begin
            MessageBox(Handle, 'You have to play this game once before installing AMX Mod X. Do that and try again.', PChar(Application.Title), MB_ICONWARNING);
            exit;
          end;
        end;

        jspInstallProgress.Show;
        if (Pos('\cstrike', ePath) <> Pos('\counter-strike', ePath)) or (Pos('\condition zero', ePath) <> Pos('czero', ePath)) then begin // Counter-Strike & Condition Zero
          if MessageBox(Handle, 'Install Counter-Strike addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modCS;
        end
        else if Pos('\day of defeat', ePath) <> Pos('\dod', ePath) then begin // Day of Defeat
          if MessageBox(Handle, 'Install Day of Defeat addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modDoD;
        end
        else if Pos('\team fortress classic', ePath) <> Pos('\tfc', ePath) then begin // Team Fortress Classic
          if MessageBox(Handle, 'Install Team Fortress Classic addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modTFC;
        end
        else if Pos('half-life\ts', ePath) <> 0 then begin // The Specialists
          if MessageBox(Handle, 'Install The Specialists addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modTS;
        end
        else if Pos('half-life\ns', ePath) <> 0 then begin // Natural Selection
          if MessageBox(Handle, 'Install Natural Selection addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modNS;
        end
        else if Pos('half-life\esf', ePath) <> 0 then begin // Natural Selection
          if MessageBox(Handle, 'Install Earth''s Special Forces addon?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
            ChosenMod := modESF;
        end;

        ePath := ePath + '\';
        InstallListen(ePath, ChosenMod);
      end;
    end;
    { Custom mod below }      
  end
  else if jplWizard.ActivePage <> jspInstallMethod then
    jplWizard.NextPage
  else begin
    if frbDedicatedServer.Checked then begin    // Dedicated Server
      ePath := GetSteamAppsDir;
      if ePath = '' then
        MessageBox(Handle, 'You haven''t installed Steam yet! Download it at www.steampowered.com, install Dedicated Server and try again.', 'Error', MB_ICONWARNING)
      else begin
        trvMods.Items.Clear;

        with GetSteamAccounts do begin
          if Count = 1 then begin
            gMultiAccount := False;
            SteamPath := ePath + Strings[0] + '\dedicated server\';
            
            eStr := GetAllMods(SteamPath, False);
            for i := 0 to eStr.Count -1 do
              trvMods.Items.Add(nil, eStr[i]);
            eStr.Free;
          end
          else begin
            gMultiAccount := True;
            for i := 0 to Count -1 do begin
              SteamPath := ePath + Strings[i] + '\dedicated server\';
              if DirectoryExists(SteamPath) then begin
                eStr := GetAllMods(SteamPath, False);
                if eStr.Count <> 0 then begin
                  CurNode := trvMods.Items.Add(nil, Strings[i]);

                  for k := 0 to eStr.Count -1 do
                    trvMods.Items.AddChild(CurNode, eStr[k]);
                  eStr.Free;

                  CurNode.Expand(False);
                end;
              end;
            end;
          end;
          
          Free;
        end;

        if trvMods.Items.Count = 0 then
          MessageBox(Handle, 'You haven''t used dedicated server yet. Please start it once before installing AMX Mod X.', 'Error', MB_ICONERROR)
        else begin
          jspSelectMod.Show;
          trvMods.Selected := nil;
          cmdNext.Enabled := False;
        end;
      end;
    end
    else if frbListenServer.Checked then begin  // Listen Server
      ePath := GetSteamAppsDir;
      if ePath = '' then
        MessageBox(Handle, 'You haven''t installed Steam yet! Download it at www.steampowered.com, install Dedicated Server and try again.', 'Error', MB_ICONWARNING)
      else begin
        trvMods.Items.Clear;

        with GetSteamAccounts do begin
          if Count = 1 then begin
            gMultiAccount := False;
            SteamPath := ePath + Strings[0] + '\';
            eStr := GetAllMods(SteamPath, True);
            for i := 0 to eStr.Count -1 do
              trvMods.Items.Add(nil, eStr[i]);
            eStr.Free;

            if DirectoryExists(SteamPath + 'half-life') then begin
              eStr := GetAllMods(SteamPath + 'half-life\', False);
              for i := 0 to eStr.Count -1 do
                trvMods.Items.Add(nil, eStr[i]);
              eStr.Free;
            end;
          end
          else begin
            gMultiAccount := True;
            for i := 0 to Count -1 do begin
              SteamPath := ePath + Strings[i] + '\';
              if DirectoryExists(SteamPath) then begin
                eStr := GetAllMods(SteamPath, False);
                CurNode := trvMods.Items.Add(nil, Strings[i]);

                for k := 0 to eStr.Count -1 do
                  trvMods.Items.AddChild(CurNode, eStr[k]);
                eStr.Free;

               CurNode.Expand(False);

                if DirectoryExists(SteamPath + 'half-life') then begin
                  eStr := GetAllMods(SteamPath + 'half-life', False);
                  for k := 0 to eStr.Count -1 do
                    trvMods.Items.AddChild(CurNode, eStr[k]);
                  eStr.Free;
                end;

                if CurNode.Count = 0 then
                  CurNode.Free;
              end;
            end;
          end;
          
          Free;
        end;

        if trvMods.Items.Count = 0 then
          MessageBox(Handle, 'You haven''t installed any Steam games yet. It is necessary to do that if you want to install AMX Mod X on a listen server.', 'Error', MB_ICONERROR)
        else begin
          jspSelectMod.Show;
          trvMods.Selected := nil;
          cmdNext.Enabled := False;
        end;
      end;
    end
    else if frbStandaloneServer.Checked then begin // Standalone Server
      eRegistry := TRegistry.Create;
      try
        eRegistry.RootKey := HKEY_CURRENT_USER;
        if eRegistry.OpenKey('Software\Valve\HLServer', False) then begin
          StandaloneServer := IncludeTrailingPathDelimiter(eRegistry.ReadString('InstallPath'));
          if DirectoryExists(StandaloneServer) then begin
            with GetAllMods(StandaloneServer, False) do begin
              gMultiAccount := False;
              for i := 0 to Count -1 do
                trvMods.Items.Add(nil, Strings[i]);
            end;
            jspSelectMod.Show;
          end
          else
            MessageBox(Handle, 'You haven''t installed Half-Life Dedicated Server yet!',  'Error', MB_ICONWARNING);
        end
        else
          MessageBox(Handle, 'You haven''t installed Half-Life Dedicated Server yet!',  'Error', MB_ICONWARNING);
      finally
        eRegistry.Free;
      end;
    end
    else if frbSelectMod.Checked then begin 
      { Custom mod }
      if frmSelectModPath.ShowModal = mrOk then begin
        jspInstallProgress.Show;
        ChosenMod := modNone;
        case frmSelectModPath.cboGameAddon.ItemIndex of
          1: ChosenMod := modCS;
          2: ChosenMod := modDoD;
          3: ChosenMod := modNS;
          4: ChosenMod := modTFC;
          5: ChosenMod := modTS;
          6: ChosenMod := modCS;
          7: ChosenMod := modESF;
        end;

        InstallCustom(frmSelectModPath.trvDirectory.SelectedFolder.PathName + '\', ChosenMod, osWindows);
      end;
    end
    else if frbFTP.Checked then begin // FTP
      frmMain.Height := 421;
      jspFTP.Show;
    end;
  end;
end;

procedure TfrmMain.CheckNext(Sender: TObject);
begin
  cmdNext.Enabled := frbAgree.Checked;
end;

procedure TfrmMain.cmdBackClick(Sender: TObject);
begin
  if jplWizard.ActivePage = jspFTP then begin
    frmMain.Height := 382;
    jspInstallMethod.Show;
  end
  else begin
    jplWizard.PrevPage;
    cmdBack.Visible := jplWizard.ActivePageIndex <> 0;
  end;
end;

procedure TfrmMain.cmdConnectClick(Sender: TObject);
var i: integer;
    eStr: TStringList;
    CurNode, HomeNode, OldNode: TTreeNode;
    Path: String;
begin
  if (Trim(txtHost.Text) = '') or (Trim(txtUsername.Text) = '') then
    MessageBox(Handle, 'Please fill in each field!', PChar(Application.Title), MB_ICONWARNING)
  else if cmdConnect.Caption = 'Connect' then begin
    // ... design stuff ...
    Screen.Cursor := crHourGlass;
    cmdConnect.Enabled := False;
    cmdProxySettings.Enabled := False;
    txtHost.Enabled := False;
    txtPort.Enabled := False;
    txtUsername.Enabled := False;
    txtPassword.Enabled := False;
    chkPassive.Enabled := False;
    cmdConnect.Caption := 'Connecting...';
    cmdCancel.Caption := '&Cancel';
    // ... set values ...
    IdFTP.Host := txtHost.Text;
    IdFTP.Port := StrToInt(txtPort.Text);
    IdFTP.Username := txtUsername.Text;
    IdFTP.Passive := chkPassive.Checked;
    IdFTP.Password := txtPassword.Text;
    // ... connect and check values etc ...
    try
      IdFTP.Connect(True, 15000);
      // ... get initial directory ...
      Path := IdFTP.RetrieveCurrentDir;
      // ... "fix" path ...
      eStr := TStringList.Create;
      eStr.Text := StringReplace(Path, '/', #13, [rfReplaceAll]);
      for i := eStr.Count -1 downto 0 do begin
        if eStr[i] = '' then
          eStr.Delete(i);
      end;
      if (Copy(Path, Length(Path) -1, 1) <> '/') then
        Path := Path + '/';
      // ... connect successful, change captions ...
      trvDirectories.Enabled := True;
      cmdConnect.Enabled := True;
      cmdConnect.Caption := 'Disconnect';
      cmdCancel.Caption := '&Close';
      cmdNext.Enabled := True;
      // ... change to / and create all the directories ...
      HomeNode := nil;
      try
        if (Path <> '/') then
          IdFTP.ChangeDir('/');
        
        trvDirectories.Items.BeginUpdate;
        with GetAllDirs do begin
          for i := 0 to Count -1 do begin
            CurNode := trvDirectories.Items.Add(nil, Strings[i]);
            if (Pos('/' + CurNode.Text + '/', Path) = 0) then begin
              trvDirectories.Items.AddChild(CurNode, 'Scanning...');
              CurNode.Data := Pointer(2);
            end
            else begin
              HomeNode := CurNode;
              CurNode.Data := Pointer(1);
              Repaint;
            end;
          end;
          Free;
        end;
        trvDirectories.Items.EndUpdate;
        trvDirectories.TopItem := HomeNode;
        IdFTP.ChangeDir(Path);
      except
        trvDirectories.Items.EndUpdate;
        if (IdFTP.Connected) then
          IdFTP.ChangeDir(Path)
        else
          IdFTP.Connect;
      end;
      // ... find directories in start path ...
      CurNode := HomeNode;
      OldNode := nil;
      for i := 1 to eStr.Count -1 do begin
        if (Assigned(CurNode)) then begin
          CurNode := trvDirectories.Items.AddChild(CurNode, eStr[i]);
          if (Assigned(OldNode)) then
            OldNode.Expand(False);
          CurNode.Data := Pointer(1);
          OldNode := CurNode;
        end;
      end;

      if (Assigned(CurNode)) then begin
        trvDirectories.Items.AddChild(CurNode, 'Scanning...');
        CurNode.Data := Pointer(2);
      end;
      // ... expand home node ...
      if (Assigned(HomeNode)) and (HomeNode <> CurNode) then begin
        HomeNode.Data := Pointer(0);
        trvDirectories.TopItem := HomeNode;
        HomeNode.Expand(False);
        HomeNode.Data := Pointer(1);
      end;
      eStr.Free;
      // ... scan for directories in home dir ...
      if Assigned(CurNode) then
        CurNode.Expand(False);
      trvDirectories.TopItem := HomeNode;
    except
      on E: Exception do begin
        Screen.Cursor := crDefault;
        // reset button properties
        cmdConnect.Enabled := True;
        txtHost.Enabled := True;
        txtPort.Enabled := True;
        txtUsername.Enabled := True;
        txtPassword.Enabled := True;
        chkPassive.Enabled := True;
        cmdProxySettings.Enabled := True;
        cmdNext.Enabled := False;
        cmdConnect.Caption := 'Connect';
        cmdCancel.Caption := '&Cancel';
        if Cancel then begin
          Cancel := False;
          exit;
        end;
        // analyze messages
        if Pos('Login incorrect.', E.Message) <> 0 then begin // login failed
          MessageBox(Handle, 'Login incorrect. Check your FTP settings and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtUsername.SetFocus;
          txtUsername.SelectAll;
        end
        else if Pos('Host not found.', E.Message) <> 0 then begin // host not found
          MessageBox(Handle, 'The entered host couldn''t be found. Check your settings and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtHost.SetFocus;
          txtHost.SelectAll;
        end
        else if Pos('Connection refused.', E.Message) <> 0 then begin // wrong port (?)
          MessageBox(Handle, 'The host refused the connection. Check your port and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtPort.SetFocus;
          txtPort.SelectAll;
        end
        else if E is EIdProtocolReplyError then begin // wrong port
          MessageBox(Handle, 'The port you entered is definitely wrong. Check it and try again.', PChar(Application.Title), MB_ICONWARNING);
          txtPort.SetFocus;
          txtPort.SelectAll;
        end
        else
          MessageBox(Handle, PChar(E.Message), PChar(Application.Title), MB_ICONWARNING); // unknown error

        // ... connect failed, leave procedure ...
        exit;
      end;
    end;
    Screen.Cursor := crDefault;
  end
  else begin
    Screen.Cursor := crHourGlass;
    IdFTP.Quit;
    trvDirectories.Items.Clear;
    trvDirectories.Enabled := False;
    cmdConnect.Enabled := True;
    cmdProxySettings.Enabled := True;
    txtHost.Enabled := True;
    txtPort.Enabled := True;
    txtUsername.Enabled := True;
    txtPassword.Enabled := True;
    chkPassive.Enabled := True;
    cmdConnect.Caption := 'Connect';
    cmdCancel.Caption := '&Close';
    cmdNext.Enabled := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.jplWizardChange(Sender: TObject);
begin
  if (jplWizard.ActivePage = jspInstallProgress) then begin
    cmdNext.Caption := '&Finish';
    cmdNext.Enabled := False;
    cmdBack.Visible := False;
  end
  else begin
    cmdNext.Caption := '&Next >';
    cmdNext.Enabled := True;
    cmdBack.Visible := jplWizard.ActivePageIndex <> 0;
  end;

  if (jplWizard.ActivePage = jspLicense) then
    cmdNext.Enabled := frbAgree.Checked;

  if (jplWizard.ActivePage = jspFTP) then
    cmdNext.Enabled := False;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if LowerCase(ParamStr(1)) = '-logftp' then begin
    MessageBox(Handle, 'FTP installation will be logged to FTP.log!', PChar(Application.Title), MB_ICONINFORMATION);
    IdLogFile.Filename := ExtractFilePath(Application.ExeName) + 'FTP.log';
    IdLogFile.Active := True;
  end;

  if not DirectoryExists(ExtractFilePath(Application.ExeName) + 'files\base') then begin
    MessageBox(Handle, 'You cannot copy this program and then run it to install AMX Mod X. Please download and install the full AMX Mod X package and start this installer again from the start menu.', 'Error', MB_ICONERROR);
    Application.Terminate;
  end
  else begin
    FileList := TStringList.Create;
    DirList := TStringList.Create;
    rtfDetails.Clear;
  end;

  // delete files, then directories
  if (DirectoryExists(ExtractFilePath(Application.ExeName) + 'temp')) then
    DelTree(ExtractFilePath(Application.ExeName) + 'temp');
end;

procedure TfrmMain.cmdProxySettingsClick(Sender: TObject);
begin
  frmProxy.ShowModal;
  // Apply Proxy Settings
  case frmProxy.cboProxy.ItemIndex of
    0: IdFTP.ProxySettings.ProxyType := fpcmNone; // none
    1: IdFTP.ProxySettings.ProxyType := fpcmHttpProxyWithFtp; // HTTP Proxy with FTP
    2: IdFTP.ProxySettings.ProxyType := fpcmOpen; // Open
    3: IdFTP.ProxySettings.ProxyType := fpcmSite; // Site
    4: IdFTP.ProxySettings.ProxyType := fpcmTransparent; // Transparent
    5: IdFTP.ProxySettings.ProxyType := fpcmUserPass; // User (Password)
    6: IdFTP.ProxySettings.ProxyType := fpcmUserSite; // User (Site)
  end;

  IdFTP.ProxySettings.Host := frmProxy.txtHost.Text;
  IdFTP.ProxySettings.UserName := frmProxy.txtPort.Text;
  IdFTP.ProxySettings.Password := frmProxy.txtPassword.Text;
  IdFTP.ProxySettings.Port := StrToInt(frmProxy.txtPort.Text);
end;

procedure TfrmMain.txtPortChange(Sender: TObject);
var i: integer;
begin
  if txtPort.Text = '' then
    txtPort.Text := '21'
  else begin
    // check if value is numeric...
    for i := Length(txtPort.Text) downto 1 do begin
      if Pos(txtPort.Text[i], '0123456789') = 0 then begin
        txtPort.Text := '21';
        txtPort.SelStart := 4;
        exit;
      end;
    end;
  end;
end;

procedure TfrmMain.trvDirectoriesExpanded(Sender: TObject;
  Node: TTreeNode);

function NodeExists(const SNode: TTreeNode; const Text: String): Boolean;
var i: integer;
begin
  Result := False;
  for i := 0 to SNode.Count -1 do begin
    if (SNode.Item[i].Text = Text) then begin
      Result := True;
      break;
    end;
  end;
end;

var ePath: String;
    CurNode: TTreeNode;
    i: integer;
begin
  if (Integer(Node.Data) <> 0) then begin // no directories added yet
    Screen.Cursor := crHourGlass;
    // get complete path
    ePath := '/';
    CurNode := Node;
    if (Assigned(CurNode)) then begin
      repeat
        ePath := '/' + CurNode.Text + ePath;
        CurNode := CurNode.Parent;
      until (not Assigned(CurNode));
    end;
    // change dir and add directories in it
    try
      Repaint;
      IdFTP.ChangeDir(ePath);
      with GetAllDirs do begin
        if (Integer(Node.Data) = 2) and (Node.Count > 0) then
          Node.Item[0].Free;
        for i := 0 to Count -1 do begin
          if (not NodeExists(Node, Strings[i])) then begin
            CurNode := trvDirectories.Items.AddChild(Node, Strings[i]);
            trvDirectories.Items.AddChild(CurNode, 'Scanning...');
            CurNode.Data := Pointer(2);
          end;
        end;
        Free;
      end;
    except
      if (Integer(Node.Data) = 2) and (Node.Count > 0) then
        Node.Item[0].Free;
      Application.ProcessMessages;
    end;
    Node.Data := Pointer(0); // scan done
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FileList.Free;
  DirList.Free;
end;

procedure TfrmMain.IdFTPWork(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
  if AWorkCount > 15 then begin
    ggeItem.Progress := AWorkCount;
    CurrProgress := AWorkCount;
  end;

  if Cancel then
    IdFTP.Abort;
    
  Application.ProcessMessages;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if (jplWizard.ActivePage = jspFTP) and (IdFTP.Connected) then
    IdFTP.Quit;
  if (jplWizard.ActivePage = jspInstallProgress) and (ggeAll.Progress <> ggeAll.MaxValue) and (not Cancel) then begin
    if MessageBox(Handle, 'Do you really want to cancel the installation?', PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
      Screen.Cursor := crDefault;
      Application.OnException := ExceptionHandler;
      Cancel := True;
      if IdFTP.Connected then
        IdFTP.Quit;
    end
    else begin
      Action := caNone;
      exit;
    end;
  end;

  if (DirectoryExists(ExtractFilePath(Application.ExeName) + 'temp')) then
    DelTree(ExtractFilePath(Application.ExeName) + 'temp');
end;

procedure TfrmMain.ExceptionHandler(Sender: TObject; E: Exception);
begin
  // we don't want any exceptions after close, so leave this empty
end;

procedure TfrmMain.tmrSpeedTimer(Sender: TObject);
begin
  Caption := CalcSpeed(OldProgress, CurrProgress);
  OldProgress := CurrProgress;
end;

procedure TfrmMain.trvDirectoriesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  Node.ImageIndex := 1;
  Node.SelectedIndex := 1;
end;

procedure TfrmMain.trvDirectoriesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Node.ImageIndex := 0;
  Node.SelectedIndex := 0;
end;

procedure TfrmMain.jspFTPShow(Sender: TObject);
begin
  Cancel := False;
end;

procedure TfrmMain.frbFTPClick(Sender: TObject);
begin
  frbDedicatedServer.Checked := False;
  frbListenServer.Checked := False;
  frbStandaloneServer.Checked := False;
  frbSelectMod.Checked := False;
end;

procedure TfrmMain.frbLocalClick(Sender: TObject);
begin
  frbFTP.Checked := False;
end;

procedure TfrmMain.trvModsClick(Sender: TObject);
begin
  if gMultiAccount then
    cmdNext.Enabled := (Assigned(trvMods.Selected)) and (Assigned(trvMods.Selected.Parent))
  else
    cmdNext.Enabled := (Assigned(trvMods.Selected));
end;

procedure TfrmMain.trvDirectoriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Node: TTreeNode;
begin
  Node := trvDirectories.GetNodeAt(X, Y);
  if (Assigned(Node)) then begin
    if (Node.DisplayRect(True).Right < X) then
      trvDirectories.Selected := nil;
  end;
end;

procedure TfrmMain.trvDirectoriesChange(Sender: TObject; Node: TTreeNode);
begin
  if (Screen.Cursor <> crHourGlass) and (Assigned(Node)) and (Integer(Node.Data) = 1) then
    Node.Expand(False);
end;

end.
