unit UnitfrmSettings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, JvExControls, JvComponent, JvPageList,
  ComCtrls, JvExComCtrls, JvPageListTreeView, TBXDkPanels, SpTBXDkPanels,
  SpTBXEditors, IniFiles, TFlatButtonUnit, TFlatEditUnit, TFlatCheckBoxUnit,
  TFlatListBoxUnit, TFlatComboBoxUnit, mbXPFontCombo, ScintillaLanguageManager,
  SciKeyBindings, menus, TFlatTabControlUnit, TFlatMemoUnit,
  TFlatRadioButtonUnit, sciLexer, sciLexerMod, sciLexerMemo, Dialogs,
  FileCtrl, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdFTP, IdException, ImgList, JvxSlider;

type
  TfrmSettings = class(TForm)
    trvSettings: TJvSettingsTreeView;
    jplSettings: TJvPageList;
    jspHighlighter: TJvStandardPage;
    jspTools: TJvStandardPage;
    jspShortcuts: TJvStandardPage;
    jspCompiler: TJvStandardPage;
    jspFTP: TJvStandardPage;
    jspPlugIns: TJvStandardPage;
    jspMisc: TJvStandardPage;
    jspCodeSnippets: TJvStandardPage;
    lblCurrSetting: TLabel;
    pnlControls: TPanel;
    bvlControls: TBevel;
    cmdOK: TFlatButton;
    cmdCancel: TFlatButton;
    lblStyles: TLabel;
    cboLanguage: TFlatComboBox;
    lblLanguage: TLabel;
    pnlHighlighter: TPanel;
    lblProperties: TLabel;
    cboFont: TmbXPFontCombo;
    lblFont: TLabel;
    chkBold: TFlatCheckBox;
    chkItalic: TFlatCheckBox;
    chkUnderlined: TFlatCheckBox;
    chkVisible: TFlatCheckBox;
    pnlColors: TPanel;
    cmdSelectBackground: TFlatButton;
    imgBackground: TImage;
    lblBackground: TLabel;
    cmdSelectForeground: TFlatButton;
    imgForeground: TImage;
    lblForeground: TLabel;
    lblFontSize: TLabel;
    txtFontSize: TFlatEdit;
    chkUseDefaultFont: TFlatCheckBox;
    cmdReset: TFlatButton;
    pnlDefaultNewPluginValues: TPanel;
    lblDefaultInfo: TLabel;
    lblDefaultAuthor: TLabel;
    txtDefaultAuthor: TFlatEdit;
    lblDefaultVersion: TLabel;
    lblDefaultName: TLabel;
    txtDefaultName: TFlatEdit;
    txtDefaultVersion: TFlatEdit;
    shpStyles: TShape;
    lstStyles: TListBox;
    chkHighlightBraces: TFlatCheckBox;
    chkAutoCloseBraces: TFlatCheckBox;
    chkAutoCloseQuotes: TFlatCheckBox;
    chkClearUndoAfterSave: TFlatCheckBox;
    bvlTools1: TBevel;
    chkWordWrap: TFlatCheckBox;
    lblCodeFolding: TLabel;
    pnlCodeFolding: TPanel;
    cboCodeFolding: TFlatComboBox;
    lblCodeFoldingStyle: TLabel;
    lvShortcuts: TListView;
    cmdApply: TFlatButton;
    shpShortcuts: TShape;
    ftcCodeSnippets: TFlatTabControl;
    shpCodeSnippets: TShape;
    shpCodeSnippet: TShape;
    lstCodeSnippets: TListBox;
    txtCodeSnippet: TMemo;
    cmdCSAdd: TFlatButton;
    cmdCSRemove: TFlatButton;
    chkShowStatusbar: TFlatCheckBox;
    lblCaret: TLabel;
    pnlCaret: TPanel;
    cmdSelectCaretFore: TFlatButton;
    imgCaretFore: TImage;
    lblSelectCaretFore: TLabel;
    cmdSelectCaretBack: TFlatButton;
    imgCaretBack: TImage;
    lblSelectCaretBack: TLabel;
    bvlCaret1: TBevel;
    chkShowCaret: TFlatCheckBox;
    lblCaretPeriod: TLabel;
    txtPeriod: TFlatEdit;
    pnlNotes: TPanel;
    lblSaveNotesTo: TLabel;
    optFileComment: TFlatRadioButton;
    optConfig: TFlatRadioButton;
    shpPlugins: TShape;
    lvPlugins: TListView;
    cmdReload: TFlatButton;
    cmdLoad: TFlatButton;
    cmdUnload: TFlatButton;
    cmdRemove: TFlatButton;
    optDontSave: TFlatRadioButton;
    lblPAWN: TLabel;
    pnlSMALLCompiler: TPanel;
    lblCPPCompiler: TLabel;
    pnlCPPCompiler: TPanel;
    lblPAWNCompilerPath: TLabel;
    txtPAWNCompilerPath: TFlatEdit;
    cmdBrowsePAWNCompiler: TFlatButton;
    txtCPPCompilerPath: TFlatEdit;
    lblCPPCompilerPath: TLabel;
    cmdBrowseCPPCompiler: TFlatButton;
    lblCPPHint: TLabel;
    lblCPPCompilerArgs: TLabel;
    txtCPPCompilerArguments: TFlatEdit;
    txtCPPOutput: TFlatEdit;
    lblCPPOutput: TLabel;
    txtPAWNOutput: TFlatEdit;
    txtPAWNArgs: TFlatEdit;
    lblPAWNArgs: TLabel;
    lblSPAWNOutput: TLabel;
    cmdBrowseOutputPAWN: TFlatButton;
    cmdBrowseOutputCPP: TFlatButton;
    pnlFTPData: TPanel;
    lblFTPData: TLabel;
    chkPassive: TFlatCheckBox;
    txtHost: TFlatEdit;
    lblHost: TLabel;
    txtPort: TFlatEdit;
    txtUsername: TFlatEdit;
    txtPassword: TFlatEdit;
    lblPassword: TLabel;
    lblUsername: TLabel;
    lblPort: TLabel;
    pnlDefaultPath: TPanel;
    pnlDirectory: TPanel;
    trvDirectories: TTreeView;
    txtDefaultDir: TFlatEdit;
    cmdConnect: TFlatButton;
    lblDefaultDir: TLabel;
    lblDefaultDirectory: TLabel;
    odBrowse: TOpenDialog;
    ilImages: TImageList;
    jspProxy: TJvStandardPage;
    pnlProxy: TPanel;
    txtProxyPassword: TFlatEdit;
    txtProxyUsername: TFlatEdit;
    lblProxyPassword: TLabel;
    lblProxyUsername: TLabel;
    txtProxyHost: TFlatEdit;
    txtProxyPort: TFlatEdit;
    lblProxyPort: TLabel;
    lblProxyHost: TLabel;
    cboProxy: TFlatComboBox;
    lblProxy: TLabel;
    jspHalfLife: TJvStandardPage;
    pnlHLExecutable: TPanel;
    lblHLExec: TLabel;
    txtHLExec: TFlatEdit;
    cmdBrowseHL: TFlatButton;
    lblCustomParameters: TLabel;
    txtCustomParameters: TFlatEdit;
    chkIndentGuides: TFlatCheckBox;
    lblAutoIndent: TLabel;
    pnlAutoIndent: TPanel;
    chkAutoIndent: TFlatCheckBox;
    cmdAdvancedAutoIndent: TFlatButton;
    txtAMXXDir: TFlatEdit;
    lblAMXXDir: TLabel;
    cmdBrowseAMXXDir: TFlatButton;
    cmdResetShortcuts: TFlatButton;
    txtShortcut: TFlatEdit;
    jspCTSettings: TJvStandardPage;
    pnlPCSpeed: TPanel;
    lblCPUSpeed: TLabel;
    lblSlow: TLabel;
    lblAverage: TLabel;
    lblFast: TLabel;
    sldSpeed: TJvxSlider;
    lblCodeExplorer: TLabel;
    lblCodeInspector: TLabel;
    pnlCodeInspector: TPanel;
    cmdBrowseLangDir: TFlatButton;
    txtLangDir: TFlatEdit;
    lblLangDir: TLabel;
    chkDontLoadFilesTwice: TFlatCheckBox;
    chkMakeBaks: TFlatCheckBox;
    chkDisableAC: TFlatCheckBox;
    chkDisableCT: TFlatCheckBox;
    procedure jplSettingsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtFontSizeChange(Sender: TObject);
    procedure cboLanguageChange(Sender: TObject);
    procedure chkUseDefaultFontClick(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkUnderlinedClick(Sender: TObject);
    procedure chkVisibleClick(Sender: TObject);
    procedure cmdResetClick(Sender: TObject);
    procedure lstStylesClick(Sender: TObject);
    procedure lvShortcutsClick(Sender: TObject);
    procedure trvSettingsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure txtPeriodChange(Sender: TObject);
    procedure txtPortChange(Sender: TObject);
    procedure lvShortcutsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure cmdApplyClick(Sender: TObject);
    procedure cmdSelectForegroundClick(Sender: TObject);
    procedure cmdSelectBackgroundClick(Sender: TObject);
    procedure cmdSelectCaretForeClick(Sender: TObject);
    procedure cmdSelectCaretBackClick(Sender: TObject);
    procedure cmdBrowsePAWNCompilerClick(Sender: TObject);
    procedure cmdBrowseCPPCompilerClick(Sender: TObject);
    procedure cmdBrowseOutputPAWNClick(Sender: TObject);
    procedure cmdBrowseOutputCPPClick(Sender: TObject);
    procedure txtPAWNOutputExit(Sender: TObject);
    procedure txtCPPOutputChange(Sender: TObject);
    procedure cmdCSAddClick(Sender: TObject);
    procedure cmdCSRemoveClick(Sender: TObject);
    procedure lstCodeSnippetsClick(Sender: TObject);
    procedure ftcCodeSnippetsTabChanged(Sender: TObject);
    procedure txtCodeSnippetKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cmdConnectClick(Sender: TObject);
    procedure trvDirectoriesExpanded(Sender: TObject; Node: TTreeNode);
    procedure trvDirectoriesChange(Sender: TObject; Node: TTreeNode);
    procedure trvDirectoriesExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure trvDirectoriesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure cboProxyChange(Sender: TObject);
    procedure txtProxyPortChange(Sender: TObject);
    procedure jplSettingsChanging(Sender: TObject; PageIndex: Integer;
      var AllowChange: Boolean);
    procedure txtProxyHostChange(Sender: TObject);
    procedure cmdBrowseHLClick(Sender: TObject);
    procedure cmdAdvancedAutoIndentClick(Sender: TObject);
    procedure cmdReloadClick(Sender: TObject);
    procedure cmdUnloadClick(Sender: TObject);
    procedure cmdLoadClick(Sender: TObject);
    procedure cmdRemoveClick(Sender: TObject);
    procedure cmdBrowseAMXXDirClick(Sender: TObject);
    procedure txtShortcutKeyPress(Sender: TObject; var Key: Char);
    procedure cmdResetShortcutsClick(Sender: TObject);
    procedure txtShortcutKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cboFontChange(Sender: TObject);
    procedure cmdBrowseLangDirClick(Sender: TObject);
  public
    Foreground, Background: TColor;
    CaretFore, CaretBack: TColor;
    procedure UpdateItemIndex;
    procedure PaintForeground(eColor: TColor);
    procedure PaintBackground(eColor: TColor);
    procedure PaintCaretFore(eColor: TColor);
    procedure PaintCaretBack(eColor: TColor);
    procedure EnableControls(eEnable: Boolean); // For Proxy
  end;

var
  frmSettings: TfrmSettings;
  eConfig: TIniFile;

implementation

uses UnitMainTools, UnitfrmMain, UnitfrmSelectColor, UnitLanguages,
  UnitCodeSnippets, UnitfrmAutoIndent, UnitPlugins;

{$R *.DFM}

procedure TfrmSettings.jplSettingsChange(Sender: TObject);
begin
  if not Started then exit;

  if Assigned(trvSettings.Selected.Parent) then
    lblCurrSetting.Caption := trvSettings.Selected.Parent.Text + ' - ' + (jplSettings.ActivePage as TJvStandardPage).Caption
  else
    lblCurrSetting.Caption := (jplSettings.ActivePage as TJvStandardPage).Caption;

  txtPAWNOutputExit(Sender);
  txtCPPOutputChange(Sender);
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  eConfig := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'config\Settings.ini');
  ReloadIni;
  case eConfig.ReadInteger('Misc', 'WindowState', 0) of
    0: frmMain.WindowState := wsNormal;
    1: frmMain.WindowState := wsMaximized;
    2: frmMain.WindowState := wsMinimized;
  end;

  PaintForeground(clBlack);
  PaintBackground(clBlack);
  PaintCaretFore(clBlack);
  PaintCaretBack(clBlack);
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
begin
  case frmMain.WindowState of
    wsNormal   : eConfig.WriteInteger('Misc', 'WindowState', 0);
    wsMaximized: eConfig.WriteInteger('Misc', 'WindowState', 1);
    else         eConfig.WriteInteger('Misc', 'WindowState', 2);
  end;

  if frmMain.IdFTP.Connected then
    frmMain.IdFTP.Disconnect;
  
  eConfig.Free;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
var i: integer;
begin
  cboLanguage.OnChange(Self);
  for i := 0 to trvSettings.Items.Count -1 do
    trvSettings.Items[i].Expand(True);
end;

procedure TfrmSettings.txtFontSizeChange(Sender: TObject);
begin
  if not IsNumeric(txtFontSize.Text) then begin
    txtFontSize.Text := '0';
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontSize := 0;
  end
  else
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontSize := StrToInt(txtFontSize.Text);
end;

{ For Highlighter Section }

procedure TfrmSettings.PaintBackground(eColor: TColor);
begin
  imgBackground.Canvas.Pen.Color := $008396A0;
  imgBackground.Canvas.Brush.Color := eColor;
  imgBackground.Canvas.Rectangle(0, 0, 19, 19);
end;

procedure TfrmSettings.PaintForeground(eColor: TColor);
begin
  imgForeground.Canvas.Pen.Color := $008396A0;
  imgForeground.Canvas.Brush.Color := eColor;
  imgForeground.Canvas.Rectangle(0, 0, 19, 19);
end;

procedure TfrmSettings.cboLanguageChange(Sender: TObject);
var i: integer;
begin
  lstStyles.Items.Clear;
  for i := 0 to TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Count -1 do
    lstStyles.Items.Add(TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[i]).Name);

  if lstStyles.Items.Count > 0 then
    lstStyles.ItemIndex := 0;
  cboFont.Enabled := lstStyles.Items.Count <> 0;
  chkUseDefaultFont.Enabled := lstStyles.Items.Count <> 0;
  chkBold.Enabled := lstStyles.Items.Count <> 0;
  chkItalic.Enabled := lstStyles.Items.Count <> 0;
  chkUnderlined.Enabled := lstStyles.Items.Count <> 0;
  chkVisible.Enabled := lstStyles.Items.Count <> 0;
  cmdSelectForeground.Enabled := lstStyles.Items.Count <> 0;
  cmdSelectBackground.Enabled := lstStyles.Items.Count <> 0;
  
  UpdateItemIndex;
end;

procedure TfrmSettings.chkUseDefaultFontClick(Sender: TObject);
begin
  cboFont.Enabled := not chkUseDefaultFont.Checked;
  if chkUseDefaultFont.Checked then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontName := ''
  else
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontName := cboFont.Selected;
end;

procedure TfrmSettings.chkBoldClick(Sender: TObject);
begin
  if chkBold.Checked then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles + [fsBold]
  else
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles - [fsBold];
end;

procedure TfrmSettings.chkItalicClick(Sender: TObject);
begin
  if chkItalic.Checked then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles + [fsItalic]
  else
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles - [fsItalic];
end;

procedure TfrmSettings.chkUnderlinedClick(Sender: TObject);
begin
  if chkUnderlined.Checked then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles + [fsUnderline]
  else
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles := TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontStyles - [fsUnderline];
end;

procedure TfrmSettings.chkVisibleClick(Sender: TObject);
begin
  TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).Visible := chkVisible.Checked;
end;

procedure TfrmSettings.cmdResetClick(Sender: TObject);
begin
  if MessageBox(Handle, PChar(lWarnHighlighterReset), 'AMXX-Studio', MB_ICONWARNING + MB_YESNO) = mrYes then begin
    DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\Editor.sci'));
    cmdReset.Enabled := False;
    cboLanguage.Enabled := False;
    lstStyles.Enabled := False;
    lblFont.Enabled := False;
    cboFont.Enabled := False;
    chkUseDefaultFont.Enabled := False;
    chkBold.Enabled := False;
    chkItalic.Enabled := False;
    chkUnderlined.Enabled := False;
    chkVisible.Enabled := False;
    lblFontSize.Enabled := False;
    txtFontSize.Enabled := False;
    lblForeground.Enabled := False;
    cmdSelectForeground.Enabled := False;
    lblBackground.Enabled := False;
    cmdSelectBackground.Enabled := False;
    MessageBox(Handle, PChar(lHighlighterResetDone), 'AMXX-Studio', MB_ICONINFORMATION);
  end;
end;

procedure TfrmSettings.UpdateItemIndex;
begin
  lblFont.Enabled := lstStyles.ItemIndex <> -1;
  cboFont.Enabled := lstStyles.ItemIndex <> -1;
  chkUseDefaultFont.Enabled := lstStyles.ItemIndex <> -1;
  chkBold.Enabled := lstStyles.ItemIndex <> -1;
  chkItalic.Enabled := lstStyles.ItemIndex <> -1;
  chkUnderlined.Enabled := lstStyles.ItemIndex <> -1;
  chkVisible.Enabled := lstStyles.ItemIndex <> -1;
  lblFontSize.Enabled := lstStyles.ItemIndex <> -1;
  txtFontSize.Enabled := lstStyles.ItemIndex <> -1;
  lblForeground.Enabled := lstStyles.ItemIndex <> -1;
  cmdSelectForeground.Enabled := lstStyles.ItemIndex <> -1;
  lblBackground.Enabled := lstStyles.ItemIndex <> -1;
  cmdSelectBackground.Enabled := lstStyles.ItemIndex <> -1;

  if lstStyles.ItemIndex <> -1 then begin
    with TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]) do begin
      chkUseDefaultFont.Checked := FontName = '';
      if FontName <> '' then
        cboFont.Selected := FontName;
      cboFont.Enabled := FontName <> '';
      chkBold.Checked := fsBold in FontStyles;
      chkItalic.Checked := fsItalic in FontStyles;
      chkUnderlined.Checked := fsUnderline in FontStyles;
      chkVisible.Checked := Visible;
      txtFontSize.Text := IntToStr(TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontSize);

      Foreground := ForeColor;
      Background := BackColor;
      
      PaintForeground(ForeColor);
      PaintBackground(BackColor);
    end;
  end;
end;

procedure TfrmSettings.lstStylesClick(Sender: TObject);
begin
  UpdateItemIndex;
end;

procedure TfrmSettings.lvShortcutsClick(Sender: TObject);
begin
  cmdApply.Enabled := Assigned(lvShortcuts.Selected);
  txtShortcut.Enabled := cmdApply.Enabled;
  if cmdApply.Enabled then
    txtShortcut.Text := lvShortcuts.Selected.Subitems[0];
end;

procedure TfrmSettings.trvSettingsChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
begin
  if (not Started) then exit;

  if (Assigned(Node.Parent)) and (not cmdReset.Enabled) then
    AllowChange := Node.Parent.Index <> 0
  else
    AllowChange := True;
end;

procedure TfrmSettings.txtPeriodChange(Sender: TObject);
begin
  if not IsNumeric(txtPeriod.Text) then
    txtPeriod.Text := '1024';
end;

procedure TfrmSettings.txtPortChange(Sender: TObject);
begin
  if not IsNumeric(txtPort.Text) then
    txtPort.Text := '21';
end;

procedure TfrmSettings.PaintCaretBack(eColor: TColor);
begin
  imgCaretBack.Canvas.Pen.Color := $008396A0;
  imgCaretBack.Canvas.Brush.Color := eColor;
  imgCaretBack.Canvas.Rectangle(0, 0, 19, 19);
end;

procedure TfrmSettings.PaintCaretFore(eColor: TColor);
begin
  imgCaretFore.Canvas.Pen.Color := $008396A0;
  imgCaretFore.Canvas.Brush.Color := eColor;
  imgCaretFore.Canvas.Rectangle(0, 0, 19, 19);
end;

procedure TfrmSettings.lvShortcutsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  lvShortcuts.OnClick(Sender);
end;

procedure TfrmSettings.cmdApplyClick(Sender: TObject);
begin
  if Assigned(lvShortcuts.Selected) then
    lvShortcuts.Selected.SubItems[0] := txtShortcut.Text;
end;

procedure TfrmSettings.cmdSelectForegroundClick(Sender: TObject);
begin
  if ShowColorDialog(Foreground, imgForeground) then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).ForeColor := Foreground;
end;

procedure TfrmSettings.cmdSelectBackgroundClick(Sender: TObject);
begin
  if ShowColorDialog(Background, imgBackground) then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).BackColor := Background;
end;

procedure TfrmSettings.cmdSelectCaretForeClick(Sender: TObject);
begin
  ShowColorDialog(CaretFore, imgCaretFore);
end;

procedure TfrmSettings.cmdSelectCaretBackClick(Sender: TObject);
begin
  ShowColorDialog(CaretBack, imgCaretBack);
end;

procedure TfrmSettings.cmdBrowsePAWNCompilerClick(Sender: TObject);
begin
  if odBrowse.Execute then
    txtPAWNCompilerPath.Text := odBrowse.FileName;
end;

procedure TfrmSettings.cmdBrowseCPPCompilerClick(Sender: TObject);
begin
  if odBrowse.Execute then
    txtCPPCompilerPath.Text := odBrowse.FileName;
end;

procedure TfrmSettings.cmdBrowseOutputPAWNClick(Sender: TObject);
var eDir: String;
begin
  if SelectDirectory(lSelectOutputPAWN, txtPAWNOutput.Text, eDir) then
    txtPAWNOutput.Text := eDir;
end;

procedure TfrmSettings.cmdBrowseOutputCPPClick(Sender: TObject);
var eDir: String;
begin
  if SelectDirectory(lSelectOutputCPP, txtCPPOutput.Text, eDir) then
    txtCPPOutput.Text := eDir;
end;

procedure TfrmSettings.txtPAWNOutputExit(Sender: TObject);
var eHDC: HDC;
    eCanvas: TCanvas;
begin
  if (txtPAWNOutput.Text = '') and (not txtPAWNOutput.Focused) then begin
    eHDC := GetDC(txtPAWNOutput.Handle);
    eCanvas := TCanvas.Create;
    eCanvas.Handle := eHDC;
    eCanvas.Font.Name := 'Tahoma';
    eCanvas.Font.Color := clBtnShadow;
    eCanvas.Font.Size := 7;
    eCanvas.TextOut(1, 1, lDynamic);
    eCanvas.Free; 
    txtPAWNOutput.Hint := lOutputHint;
  end
  else
    txtPAWNOutput.Hint := '';
end;

procedure TfrmSettings.txtCPPOutputChange(Sender: TObject);
var eHDC: HDC;
    eCanvas: TCanvas;
begin
  if (txtCPPOutput.Text = '') and (not txtCPPOutput.Focused) then begin
    eHDC := GetDC(txtCPPOutput.Handle);
    eCanvas := TCanvas.Create;
    eCanvas.Handle := eHDC;
    eCanvas.Font.Name := 'Tahoma';
    eCanvas.Font.Color := clBtnShadow;
    eCanvas.Font.Size := 7;
    eCanvas.TextOut(1, 1, lDynamic);
    eCanvas.Free;
    txtCPPOutput.Hint := lOutputHint;
  end
  else
    txtCPPOutput.Hint := '';
end;

procedure TfrmSettings.cmdCSAddClick(Sender: TObject);
var eStr: String;
begin
  if InputQuery(lAddCodeSnippetCaption, lAddCodeSnippetPrompt, eStr) then begin
    eStr := StringReplace(eStr, '=', '', [rfReplaceAll]);
    if eStr = '' then begin
      MessageBox(Handle, PChar(lEmptyCodeSnippetTitle), PChar(Application.Title), MB_ICONWARNING);
      cmdCSAdd.Click;
    end
    else begin
      if lstCodeSnippets.Items.IndexOf(eStr) = -1 then begin
        lstCodeSnippets.ItemIndex := lstCodeSnippets.Items.Add(eStr);
        AddSnippet(ftcCodeSnippets.Tabs[ftcCodeSnippets.ActiveTab], eStr, '');
        txtCodeSnippet.Enabled := True;
        lstCodeSnippetsClick(Sender);
      end
      else
        MessageBox(Handle, PChar(lCodeSnippetExists), PChar(Application.Title), MB_ICONWARNING);
    end;
  end;
end;

procedure TfrmSettings.cmdCSRemoveClick(Sender: TObject);
begin
  if lstCodeSnippets.ItemIndex <> -1 then begin
    DelSnippet(ftcCodeSnippets.Tabs[ftcCodeSnippets.ActiveTab], lstCodeSnippets.Items[lstCodeSnippets.ItemIndex]);
    lstCodeSnippets.Items.Delete(lstCodeSnippets.ItemIndex); 
    if lstCodeSnippets.Items.Count > 0 then
      lstCodeSnippets.ItemIndex := 0
    else
      txtCodeSnippet.Clear;
    lstCodeSnippetsClick(Sender);
  end;
  cmdCSRemove.Enabled := lstCodeSnippets.ItemIndex <> -1;
  txtCodeSnippet.Enabled := lstCodeSnippets.Items.Count > 0;
end;

procedure TfrmSettings.lstCodeSnippetsClick(Sender: TObject);
begin
  cmdCSRemove.Enabled := lstCodeSnippets.ItemIndex <> -1;
  if cmdCSRemove.Enabled then
    txtCodeSnippet.Lines.Text := GetSnippet(ftcCodeSnippets.Tabs[ftcCodeSnippets.ActiveTab], lstCodeSnippets.Items[lstCodeSnippets.ItemIndex]);
end;

procedure TfrmSettings.ftcCodeSnippetsTabChanged(Sender: TObject);
begin
  lstCodeSnippets.Items.Text := GetSnippetList(ftcCodeSnippets.Tabs[ftcCodeSnippets.ActiveTab]).Text;
  if lstCodeSnippets.Items.Count > 0 then
    lstCodeSnippets.ItemIndex := 0
  else
    txtCodeSnippet.Clear;
  lstCodeSnippetsClick(Sender);
  txtCodeSnippet.Enabled := lstCodeSnippets.Items.Count > 0;
  cmdCSRemove.Enabled := lstCodeSnippets.ItemIndex <> -1;
end;

procedure TfrmSettings.txtCodeSnippetKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  SetSnippet(ftcCodeSnippets.Tabs[ftcCodeSnippets.ActiveTab], lstCodeSnippets.Items[lstCodeSnippets.ItemIndex], txtCodeSnippet.Lines.Text);
end;

procedure TfrmSettings.cmdConnectClick(Sender: TObject);
var i: integer;
    eStr: TStringList;
    CurNode: TTreeNode;
begin
  { The following functions are copied from my installer }

  if (Trim(txtHost.Text) = '') or (Trim(txtUsername.Text) = '') or (Trim(txtPassword.Text) = '') then
    MessageBox(Handle, PChar(lFillInEachField), PChar(Application.Title), MB_ICONWARNING)
  else if cmdConnect.Caption = lConnect then begin
    // ... design stuff ...
    Screen.Cursor := crHourGlass;
    cmdConnect.Enabled := False;
    txtHost.Enabled := False;
    txtPort.Enabled := False;
    txtUsername.Enabled := False;
    txtPassword.Enabled := False;
    chkPassive.Enabled := False;
    cmdConnect.Caption := lConnecting;
    // ... disconnect if already connected ...
    if frmMain.IdFTP.Connected then
      frmMain.IdFTP.Disconnect;
    // ... set values, connect and check errors etc ...
    i := TryConnect;
    case i of
      1: begin
        txtUsername.SetFocus;
        txtUsername.SelectAll;
      end;
      2: begin
        txtHost.SetFocus;
        txtHost.SelectAll;
      end;
      3, 4: begin
        txtPort.SetFocus;
        txtPort.SelectAll;
      end;
    end;

    if i <> 0 then begin
      // reset button properties
      cmdConnect.Enabled := True;
      txtHost.Enabled := True;
      txtPort.Enabled := True;
      txtUsername.Enabled := True;
      txtPassword.Enabled := True;
      chkPassive.Enabled := True;
      cmdConnect.Caption := lConnect;
      Screen.Cursor := crDefault;
      // ... connect failed, leave procedure ...
      exit;
    end;
    // ... connect successful, change captions ...
    trvDirectories.Enabled := True;
    cmdConnect.Enabled := True;
    cmdConnect.Caption := lDisconnect;
    // ... scan for initial directory ...
    eStr := TStringList.Create;
    eStr.Text := StringReplace(frmMain.IdFTP.RetrieveCurrentDir, '/', #13, [rfReplaceAll]);
    for i := eStr.Count -1 downto 0 do begin
      if eStr[i] = '' then
        eStr.Delete(i);
    end;

    CurNode := nil;
    if eStr.Count <> 0 then begin
      for i := 0 to eStr.Count -1 do
        CurNode := trvDirectories.Items.AddChild(CurNode, eStr[i]);
    end;
    if trvDirectories.Items.Count <> 0 then
      trvDirectories.Items.Item[0].Expand(True); 
    eStr.Destroy;
    
    // ... scan for directories ...
    with GetAllDirs do begin
      for i := 0 to Count -1 do
        trvDirectories.Items.AddChild(trvDirectories.Items.AddChild(CurNode, Strings[i]), 'Scanning...');
      Free;
    end;

    if Assigned(CurNode) then
      CurNode.Expand(False);
    Screen.Cursor := crDefault;
  end
  else begin
    Screen.Cursor := crHourGlass;
    frmMain.IdFTP.Quit;
    trvDirectories.Items.Clear;
    cmdConnect.Enabled := True;
    trvSettings.Enabled := True;
    txtHost.Enabled := True;
    txtPort.Enabled := True;
    txtUsername.Enabled := True;
    txtPassword.Enabled := True;
    chkPassive.Enabled := True;
    cmdConnect.Caption := lConnect;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSettings.trvDirectoriesExpanded(Sender: TObject;
  Node: TTreeNode);
var ePath: String;
    CurNode: TTreeNode;
    i: integer;
begin
  if Node.Item[0].Text = lScanning then begin // no directories have been added yet
    Screen.Cursor := crHourGlass;
    // get complete path
    ePath := '/';
    CurNode := Node;
    repeat
      ePath := '/' + CurNode.Text + ePath;
      CurNode := CurNode.Parent;
    until (not Assigned(CurNode));
    // change dir and add directories in it
    try
      Repaint;
      frmMain.IdFTP.ChangeDir(ePath);
      with GetAllDirs do begin
        Node.Item[0].Free;
        for i := 0 to Count -1 do begin
          trvDirectories.Items.AddChild(trvDirectories.Items.AddChild(Node, Strings[i]), lScanning);
        end;
        Free;
      end;
    finally
      Application.ProcessMessages;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmSettings.trvDirectoriesChange(Sender: TObject;
  Node: TTreeNode);
var ePath: String;
    CurNode: TTreeNode;
begin
  if Screen.Cursor <> crDefault then exit; // on disconnect this event is also raised

  // get complete path
  ePath := '/';
  CurNode := Node;
  repeat
    ePath := '/' + CurNode.Text + ePath;
    CurNode := CurNode.Parent;
  until (not Assigned(CurNode));
  // change path
  txtDefaultDir.Text := ePath;
end;

procedure TfrmSettings.trvDirectoriesExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  Node.ImageIndex := 1;
  Node.SelectedIndex := 1;
end;

procedure TfrmSettings.trvDirectoriesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
begin
  Node.ImageIndex := 0;
  Node.SelectedIndex := 0;
end;

procedure TfrmSettings.cboProxyChange(Sender: TObject);
begin
  EnableControls(cboProxy.ItemIndex <> 0); // 0 = None
  SetProxySettings;
end;

procedure TfrmSettings.EnableControls(eEnable: Boolean);
begin
  lblProxyHost.Enabled := eEnable;
  lblProxyPassword.Enabled := eEnable;
  lblProxyPort.Enabled := eEnable;
  lblProxyUsername.Enabled := eEnable;
  txtProxyHost.Enabled := eEnable;
  txtProxyPort.Enabled := eEnable;
  txtProxyUsername.Enabled := eEnable;
  txtProxyPassword.Enabled := eEnable;
end;

procedure TfrmSettings.txtProxyPortChange(Sender: TObject);
begin
  if not IsNumeric(txtProxyPort.Text) then
    txtProxyPort.Text := '8080';
  SetProxySettings;
end;

procedure TfrmSettings.jplSettingsChanging(Sender: TObject;
  PageIndex: Integer; var AllowChange: Boolean);
begin
  if (frmMain.IdFTP.Connected) and (jplSettings.Pages[PageIndex] = jspProxy) then
    AllowChange := False;
end;

procedure TfrmSettings.txtProxyHostChange(Sender: TObject);
begin
  SetProxySettings;
end;

procedure TfrmSettings.cmdBrowseHLClick(Sender: TObject);
begin
  if odBrowse.Execute then
    txtHLExec.Text := odBrowse.FileName;
end;

procedure TfrmSettings.cmdAdvancedAutoIndentClick(Sender: TObject);
begin
  frmAutoIndent.ShowModal;
end;

procedure TfrmSettings.cmdReloadClick(Sender: TObject);
begin
  if Assigned(lvPlugins.Selected) then begin
    if lvPlugins.Selected.SubItems[2] = 'Loaded' then
      cmdUnload.Click;
    cmdLoad.Click;
  end;
end;

procedure TfrmSettings.cmdUnloadClick(Sender: TObject);
begin
  if Assigned(lvPlugins.Selected) then begin
    if lvPlugins.Selected.SubItems[2] = 'Unloaded' then
      MessageBox(Handle, PChar(lAlreadyUnLoaded), PChar(Application.Title), MB_ICONERROR)
    else
      UnloadPlugin(lvPlugins.Selected);
  end;
end;

procedure TfrmSettings.cmdLoadClick(Sender: TObject);
begin
  if Assigned(lvPlugins.Selected) then begin
    if lvPlugins.Selected.SubItems[2] = 'Loaded' then
      MessageBox(Handle, PChar(lAlreadyLoaded), PChar(Application.Title), MB_ICONERROR)
    else
      LoadPlugin(lvPlugins.Selected);
  end;
end;

procedure TfrmSettings.cmdRemoveClick(Sender: TObject);
begin
  if Assigned(lvPlugins.Selected) then begin
    if lvPlugins.Selected.SubItems[2] = 'Loaded' then
      cmdUnload.Click;
    DeleteFile(PChar(ExtractFilePath(ParamStr(0)) + 'plugins\' + lvPlugins.Selected.SubItems[0]));
    lvPlugins.DeleteSelected;
  end;
end;

procedure TfrmSettings.cmdBrowseAMXXDirClick(Sender: TObject);
var eStr: String;
begin
  if SelectDirectory(lSelectAMXXCaption, ExtractFilePath(txtHLExec.Text), eStr) then
    txtAMXXDir.Text := eStr;
end;

procedure TfrmSettings.txtShortcutKeyPress(Sender: TObject; var Key: Char);
begin
  Key := #0;
end;

procedure TfrmSettings.cmdResetShortcutsClick(Sender: TObject);
var i: integer;
    Item: TListItem;
    KeyCommand: TSciKeyCommand;
    Ident: String;
begin
  if MessageBox(Handle, PChar(lResetShortcuts), PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then begin
    frmMain.sciEditor.KeyCommands.ResetDefaultCommands;
   	frmSettings.lvShortcuts.Items.BeginUpdate;
   	try
   		frmSettings.lvShortcuts.Clear;
   		for i := 0 to frmMain.sciEditor.KeyCommands.Count - 1 do begin
	  		KeyCommand := frmMain.sciEditor.KeyCommands.Items[i] as TSciKeyCommand;
        Ident := 'Unknown';
	  		IntToIdent(KeyCommand.Command, Ident, Sci_KeyboardCommandMap);
        if Ident <> 'No Command' then begin // Important for Control Chars, the user mustn't change the values for it...
        	Item := frmSettings.lvShortcuts.Items.Add;
 	        Item.Caption:= Ident;
          Item.SubItems.Add(ShortCutToText(KeyCommand.ShortCut));
 	       	Item.Data := KeyCommand;
        end;
	   end;
	  finally
   		frmSettings.lvShortcuts.Items.EndUpdate;
   	end;
  end;
end;

procedure TfrmSettings.txtShortcutKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SHIFT) or (Key = VK_CONTROL) or (Key = VK_MENU) then begin
    if txtShortcut.Text[Length(txtShortcut.Text)] = '+' then
      txtShortcut.Text := 'None';
  end;
end;

procedure TfrmSettings.cboFontChange(Sender: TObject);
begin
  if (not chkUseDefaultFont.Checked) then
    TSciStyle(TSciLangItem(frmMain.sciEditor.LanguageManager.LanguageList.Items[cboLanguage.ItemIndex]).Styles.Items[lstStyles.ItemIndex]).FontName := cboFont.Text;
end;

procedure TfrmSettings.cmdBrowseLangDirClick(Sender: TObject);
var eStr: String;
begin
  if SelectDirectory(lSelectLanguageDir, ExtractFilePath(txtLangDir.Text), eStr) then
    txtLangDir.Text := eStr;
end;

end.
