unit UnitfrmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpTBXItem, TB2Item, TBX, TB2Dock, TB2Toolbar, Menus, ImgList,
  TBXSwitcher, { Themes -> } TBXAluminumTheme, TBXDreamTheme, TBXEosTheme,
  TBXMonaiTheme, TBXNexos2Theme, TBXNexos3Theme, TBXNexos4Theme, TBXNexos5Theme,
  TBXOffice11AdaptiveTheme, TBXOfficeCTheme, TBXOfficeKTheme, TBXOfficeXPTheme,
  TBXReliferTheme, TBXSentimoXTheme, TBXTristanTheme, TBXTristan2Theme,
  TBXXitoTheme, TBXMonaiXPTheme, TBXZezioTheme, TBXWhidbeyTheme,
  TBXRomaTheme, TBXMirandaTheme, { <- Themes }
  SpTBXTabs, ExtCtrls, SpTBXDkPanels, TFlatSplitterUnit,
  SciLexer, SciLexerMemo, SciLexerMod, SciCallTips, ComCtrls, 
  StdCtrls, TBXDkPanels, TBXToolPals, SciPropertyMgr,
  SciAutoComplete, sciKeyBindings,
  sciPrint, ClipBrd, ActnList, SciSearchReplace,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdFTP,
  ShellAPI, IdFTPCommon, IdAntiFreezeBase, IdAntiFreeze, JvComponent,
  JvInspector, JvExControls, JvPluginManager, JvgLanguageLoader,
  JvWndProcHook, CommCtrl, JvPageList, JvPageListTreeView,
  SciSearchReplaceBase, SpTBXControls, JvTabBar, TB2ExtItems, SpTBXEditors,
  TBXLists, SpTBXLists, JvComponentBase;

type
  TfrmMain = class(TForm)
    TBXSwitcher: TTBXSwitcher;
    ilImages: TImageList;
    ppmOutput: TSpTBXPopupMenu;
    tbxTopDock: TSpTBXDock;
    tbxMenu: TSpTBXToolbar;
    mnuFile: TSpTBXSubmenuItem;
    mnuNew: TSpTBXSubmenuItem;
    mnuEmptyPlugin: TSpTBXItem;
    mnuNewPlugin: TSpTBXItem;
    mnuHeaderPawn: TSpTBXItem;
    mnuNewModule: TSpTBXItem;
    mnuNewUnit: TSpTBXItem;
    mnuNewHeaderCPP: TSpTBXItem;
    sepFile1: TSpTBXSeparatorItem;
    mnuOpen: TSpTBXItem;
    sepFile2: TSpTBXSeparatorItem;
    mnuSave: TSpTBXItem;
    mnuSaveAs: TSpTBXItem;
    mnuSaveAllFiles: TSpTBXItem;
    sepFile3: TSpTBXSeparatorItem;
    mnuClose: TSpTBXItem;
    mnuCloseAllFiles: TSpTBXItem;
    sepFile4: TSpTBXSeparatorItem;
    mnuPrint: TSpTBXItem;
    sepFile5: TSpTBXSeparatorItem;
    mnuExit: TSpTBXItem;
    mnuEdit: TSpTBXSubmenuItem;
    mnuUndo: TSpTBXItem;
    mnuRedo: TSpTBXItem;
    sepEdit1: TSpTBXSeparatorItem;
    mnuCut: TSpTBXItem;
    mnuCopy: TSpTBXItem;
    mnuPaste: TSpTBXItem;
    sepEdit2: TSpTBXSeparatorItem;
    mnuSelectAll: TSpTBXItem;
    mnuSearch: TSpTBXSubmenuItem;
    mnuSearchDialog: TSpTBXItem;
    mnuSearchAgain: TSpTBXItem;
    sepSearch1: TSpTBXSeparatorItem;
    mnuReplace: TSpTBXItem;
    sepSearch2: TSpTBXSeparatorItem;
    mnuGoToLine: TSpTBXItem;
    mnuView: TSpTBXSubmenuItem;
    mnuChangeTheme: TSpTBXSubmenuItem;
    mnuThemes: TSpTBXThemeGroupItem;
    mnuSelectHighlighter: TSpTBXSubmenuItem;
    mnuHPawn: TSpTBXItem;
    mnuHCPP: TSpTBXItem;
    mnuHHTML: TSpTBXItem;
    mnuHSQL: TSpTBXItem;
    mnuHXML: TSpTBXItem;
    sepHighlighter: TSpTBXSeparatorItem;
    mnuHNone: TSpTBXItem;
    sepView2: TSpTBXSeparatorItem;
    mnuShowFileTB: TSpTBXItem;
    mnuShowEditTB: TSpTBXItem;
    mnuShowCodeSnippets: TSpTBXItem;
    mnuShowCodeToolsWindow: TSpTBXItem;
    mnuCompile: TSpTBXSubmenuItem;
    mnuDoCompile: TSpTBXItem;
    sepCompile1: TSpTBXSeparatorItem;
    mnuCompileAndStartHL: TSpTBXItem;
    mnuCompileAndUpload: TSpTBXItem;
    sepCompile2: TSpTBXSeparatorItem;
    mnuRegisterPluginsIniLocal: TSpTBXItem;
    mnuRegisterPluginsIniWeb: TSpTBXItem;
    mnuTools: TSpTBXSubmenuItem;
    mnuIndenter: TSpTBXItem;
    mnuUnindenter: TSpTBXItem;
    sepTools1: TSpTBXSeparatorItem;
    mnuSocketTerminal: TSpTBXItem;
    sepTools2: TSpTBXSeparatorItem;
    mnuPluginsIniEditor: TSpTBXItem;
    sepTools3: TSpTBXSeparatorItem;
    mnuSettings: TSpTBXItem;
    mnuGenerators: TSpTBXSubmenuItem;
    mnuHudmessage: TSpTBXItem;
    mnuMOTDGenerator: TSpTBXItem;
    mnuHelp: TSpTBXSubmenuItem;
    mnuOpenHelp: TSpTBXItem;
    sepHelp1: TSpTBXSeparatorItem;
    mnuSearchForums: TSpTBXItem;
    mnuOpenScriptingForum: TSpTBXItem;
    tbxToolbar: TTBXToolbar;
    mnuTNew: TSpTBXItem;
    mnuTOpen: TSpTBXItem;
    mnuTSave: TSpTBXItem;
    sepToolbar1: TSpTBXSeparatorItem;
    mnuTSearch: TSpTBXItem;
    sepToolbar2: TSpTBXSeparatorItem;
    mnuTCompile: TSpTBXItem;
    tbxCodeSnippets: TSpTBXToolbar;
    mnuCodeSnippets: TSpTBXRightAlignSpacerItem;
    mnuPawn: TSpTBXItem;
    mnuCPP: TSpTBXItem;
    sepCodeSnippets: TSpTBXSeparatorItem;
    tbxEdit: TSpTBXToolbar;
    mnuTUndo: TSpTBXItem;
    mnuTRedo: TSpTBXItem;
    sepTEdit1: TSpTBXSeparatorItem;
    mnuTCopy: TSpTBXItem;
    mnuTCut: TSpTBXItem;
    mnuTPaste: TSpTBXItem;
    sepTEdit2: TSpTBXSeparatorItem;
    mnuTSelectAll: TSpTBXItem;
    sciCallTips: TSciCallTips;
    sbStatus: TSpTBXStatusBar;
    sepGenerators1: TSpTBXSeparatorItem;
    infoNewAMXX: TSpTBXRightAlignSpacerItem;
    infoNewCPP: TSpTBXRightAlignSpacerItem;
    sepNew1: TSpTBXSeparatorItem;
    mnuPaster: TSpTBXItem;
    sciPropertyLoader: TSciPropertyLoader;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    sepNew3: TSpTBXSeparatorItem;
    infoNewOther: TSpTBXRightAlignSpacerItem;
    mnuNewTextfile: TSpTBXItem;
    mnuNewHTML: TSpTBXItem;
    mnuNewSQL: TSpTBXItem;
    mnuNewXML: TSpTBXItem;
    sepHelp2: TSpTBXSeparatorItem;
    mnuInfo: TSpTBXItem;
    mnuFilename: TSpTBXRightAlignSpacerItem;
    sepStatus2: TSpTBXSeparatorItem;
    mnuCaret: TSpTBXRightAlignSpacerItem;
    sepStatus3: TSpTBXSeparatorItem;
    mnuModified: TSpTBXRightAlignSpacerItem;
    sepStatus1: TSpTBXSeparatorItem;
    mnuShowCodeTools: TSpTBXItem;
    mnuOther: TSpTBXItem;
    mnuFoldAll: TSpTBXItem;
    sepView1: TSpTBXSeparatorItem;
    sciPrinter: TSciPrinter;
    mnuCopyMessage: TSpTBXItem;
    mnuCopyAll: TSpTBXItem;
    mnuSaveToFile: TSpTBXItem;
    sepOutput: TSpTBXSeparatorItem;
    ppmEditor: TSpTBXPopupMenu;
    mnuEditorCopy: TSpTBXItem;
    mnuEditorPaste: TSpTBXItem;
    mnuEditorCut: TSpTBXItem;
    sepEditorMenu2: TSpTBXSeparatorItem;
    mnuEditorUndo: TSpTBXItem;
    mnuEditorRedo: TSpTBXItem;
    sepEditorMenu3: TSpTBXSeparatorItem;
    mnuToogleBookmark: TSpTBXItem;
    mnuGoToBookmark: TSpTBXItem;
    mnuEditorDelete: TSpTBXItem;
    sepEditorMenu1: TSpTBXSeparatorItem;
    mnuEditorSelectAll: TSpTBXItem;
    sciSearchReplace: TSciSearchReplace;
    IdFTP: TIdFTP;
    sciAutoComplete: TSciAutoComplete;
    mnuHTML: TSpTBXItem;
    ppmDocuments: TSpTBXPopupMenu;
    mnuPClose: TSpTBXItem;
    mnuPSave: TSpTBXItem;
    sepDocuments: TSpTBXSeparatorItem;
    mnuPCloseAllFiles: TSpTBXItem;
    mnuMenuGenerator: TSpTBXItem;
    JvInspectorDotNETPainter: TJvInspectorDotNETPainter;
    mnuConnectionGen: TSpTBXItem;
    sepView3: TSpTBXSeparatorItem;
    mnuShowCodeExplorer: TSpTBXItem;
    mnuShowCodeInspector: TSpTBXItem;
    mnuRestoreBackup: TSpTBXItem;
    tbDocs: TJvTabBar;
    mtpDocuments: TJvModernTabBarPainter;
    tcTools: TSpTBXTabControl;
    tiTools: TSpTBXTabItem;
    tiNotes: TSpTBXTabItem;
    tsNotes: TSpTBXTabSheet;
    imgRight4: TImage;
    imgBottom4: TImage;
    imgTop4: TImage;
    imgLeft4: TImage;
    tbxNotes: TSpTBXToolbar;
    mnuBold: TSpTBXItem;
    mnuItalic: TSpTBXItem;
    mnuUnderline: TSpTBXItem;
    sepNotes: TSpTBXSeparatorItem;
    mnuSelectColor: TSpTBXSubmenuItem;
    cpNotes: TTBXColorPalette;
    rtfNotes: TRichEdit;
    tsExplorer: TSpTBXTabSheet;
    spcBottom2: TImage;
    spcLeft2: TImage;
    spcRight2: TImage;
    pnlDock: TSpTBXMultiDock;
    pnlCodeExplorer: TSpTBXDockablePanel;
    trvExplorer: TTreeView;
    pnlCodeInspector: TSpTBXDockablePanel;
    jviCode: TJvInspector;
    splRight: TSplitter;
    spcRight1: TImage;
    spcLeft1: TImage;
    pnlParent: TPanel;
    splOutput: TSplitter;
    sciEditor: TScintilla;
    lstOutput: TListBox;
    pnlLoading: TSpTBXPanel;
    pbLoading: TSpTBXProgressBar;
    cmdCancel: TSpTBXButton;
    sepStatus0: TSpTBXSeparatorItem;
    cboCurrentIDE: TSpTBXDropDownItem;
    stlIDEs: TSpTBXStringList;
    procedure FormConstrainedResize(Sender: TObject; var MinWidth,
      MinHeight, MaxWidth, MaxHeight: Integer);
    procedure mnuExitClick(Sender: TObject);
    procedure OnCodeSnippetSelect(Sender: TObject);
    procedure cpNotesChange(Sender: TObject);
    procedure mnuBoldClick(Sender: TObject);
    procedure mnuItalicClick(Sender: TObject);
    procedure mnuUnderlineClick(Sender: TObject);
    procedure rtfNotesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rtfNotesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure trvExplorerCollapsed(Sender: TObject; Node: TTreeNode);
    procedure trvExplorerExpanded(Sender: TObject; Node: TTreeNode);
    procedure mnuSettingsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mnuTOpenClick(Sender: TObject);
    procedure mnuTSaveClick(Sender: TObject);
    procedure mnuTSearchClick(Sender: TObject);
    procedure mnuTCompileClick(Sender: TObject);
    procedure mnuTUndoClick(Sender: TObject);
    procedure mnuTRedoClick(Sender: TObject);
    procedure mnuTCopyClick(Sender: TObject);
    procedure mnuTCutClick(Sender: TObject);
    procedure mnuTPasteClick(Sender: TObject);
    procedure mnuTSelectAllClick(Sender: TObject);
    procedure mnuEmptyPluginClick(Sender: TObject);
    procedure mnuNewPluginClick(Sender: TObject);
    procedure mnuHXMLClick(Sender: TObject);
    procedure mnuHNoneClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuNewTextfileClick(Sender: TObject);
    procedure mnuNewHTMLClick(Sender: TObject);
    procedure mnuNewSQLClick(Sender: TObject);
    procedure mnuNewXMLClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuThemesClick(Sender: TObject);
    procedure mnuInfoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuCloseClick(Sender: TObject);
    procedure mnuShowCodeToolsClick(Sender: TObject);
    procedure sciEditorClick(Sender: TObject);
    procedure sciEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sciEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure mnuFoldAllClick(Sender: TObject);
    procedure tbxToolbarVisibleChanged(Sender: TObject);
    procedure mnuShowFileTBClick(Sender: TObject);
    procedure tbxEditVisibleChanged(Sender: TObject);
    procedure mnuShowEditTBClick(Sender: TObject);
    procedure tbxCodeSnippetsVisibleChanged(Sender: TObject);
    procedure mnuShowCodeSnippetsClick(Sender: TObject);
    procedure mnuCopyMessageClick(Sender: TObject);
    procedure mnuCopyAllClick(Sender: TObject);
    procedure mnuSaveToFileClick(Sender: TObject);
    procedure ppmOutputInitPopup(Sender: TObject; PopupView: TTBView);
    procedure mnuSearchForumsClick(Sender: TObject);
    procedure mnuOpenScriptingForumClick(Sender: TObject);
    procedure mnuEditorUndoClick(Sender: TObject);
    procedure mnuEditorRedoClick(Sender: TObject);
    procedure mnuEditorCopyClick(Sender: TObject);
    procedure mnuEditorCutClick(Sender: TObject);
    procedure mnuEditorPasteClick(Sender: TObject);
    procedure mnuToogleBookmarkClick(Sender: TObject);
    procedure mnuEditorDeleteClick(Sender: TObject);
    procedure mnuEditorSelectAllClick(Sender: TObject);
    procedure mnuGoToBookmarkClick(Sender: TObject);
    procedure mnuSearchDialogClick(Sender: TObject);
    procedure mnuReplaceClick(Sender: TObject);
    procedure mnuSearchAgainClick(Sender: TObject);
    procedure mnuGoToLineClick(Sender: TObject);
    procedure mnuPrintClick(Sender: TObject);
    procedure mnuSaveAllFilesClick(Sender: TObject);
    procedure sciEditorKeyPress(Sender: TObject; var Key: Char);
    procedure mnuCloseAllFilesClick(Sender: TObject);
    procedure mnuIndenterClick(Sender: TObject);
    procedure mnuUnindenterClick(Sender: TObject);
    procedure mnuPasterClick(Sender: TObject);
    procedure mnuPluginsIniEditorClick(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure mnuSocketTerminalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure trvExplorerDblClick(Sender: TObject);
    procedure tiPawnClick(Sender: TObject);
    procedure tiCPPClick(Sender: TObject);
    procedure tiOtherClick(Sender: TObject);
    procedure mnuOpenHelpClick(Sender: TObject);
    procedure mnuHudmessageClick(Sender: TObject);
    procedure mnuDoCompileClick(Sender: TObject);
    procedure lstOutputMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lstOutputDblClick(Sender: TObject);
    procedure lstOutputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sciEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lstOutputEnter(Sender: TObject);
    procedure mnuCompileAndStartHLClick(Sender: TObject);
    procedure mnuCompileAndUploadClick(Sender: TObject);
    procedure mnuRegisterPluginsIniLocalClick(Sender: TObject);
    procedure mnuRegisterPluginsIniWebClick(Sender: TObject);
    procedure mnuMOTDGeneratorClick(Sender: TObject);
    procedure mnuHeaderPawnClick(Sender: TObject);
    procedure mnuPCloseClick(Sender: TObject);
    procedure mnuPCloseAllFilesClick(Sender: TObject);
    procedure mnuTNewClick(Sender: TObject);
    procedure mnuMenuGeneratorClick(Sender: TObject);
    procedure JvPluginManagerPlugInError(Sender: TObject;
      AError: Exception);
    procedure mnuNewModuleClick(Sender: TObject);
    procedure mnuNewUnitClick(Sender: TObject);
    procedure mnuNewHeaderCPPClick(Sender: TObject);
    procedure tcToolsActiveTabChange(Sender: TObject; ItemIndex: Integer);
    procedure sciEditorModified(Sender: TObject; const position,
      modificationType: Integer; text: PAnsiChar; const len, linesAdded,
      line, foldLevelNow, foldLevelPrev: Integer);
    procedure sciEditorDblClick(Sender: TObject);
    procedure sciCallTipsBeforeShow(Sender: TObject;
      const Position: Integer; ListToDisplay: TStrings;
      var CancelDisplay: Boolean);
    procedure sciEditorCallTipClick(Sender: TObject;
      const position: Integer);
    procedure sciEditorAutoCSelection(Sender: TObject; text: PAnsiChar);
    procedure pnlCodeInspectorVisibleChanged(Sender: TObject);
    procedure pnlCodeExplorerVisibleChanged(Sender: TObject);
    procedure mnuShowCodeExplorerClick(Sender: TObject);
    procedure mnuShowCodeInspectorClick(Sender: TObject);
    procedure mnuConnectionGenClick(Sender: TObject);
    procedure trvExplorerClick(Sender: TObject);
    procedure jviCodeItemValueChanged(Sender: TObject;
      Item: TJvCustomInspectorItem);
    procedure mnuRestoreBackupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure stlIDEsClick(Sender: TObject);
    procedure tbDocsTabSelected(Sender: TObject; Item: TJvTabBarItem);
    procedure tbDocsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tbDocsTabClosing(Sender: TObject; Item: TJvTabBarItem;
      var AllowClose: Boolean);
    procedure sciAutoCompleteBeforeShow(Sender: TObject; const Position,
      TypedLen: Integer; ListToDisplay: TStrings;
      var CancelDisplay: Boolean);
  private
    procedure UpdateNotes;
  public
    IRCPasterStop: Boolean;
    eErrorLine: Integer;
    SelectedTab: TJvTabBarItem;
    procedure OnCodeSnippetClick(Sender: TObject);
    procedure OnCustomClick(Sender: TObject);
    procedure SetErrorLine(eLine: Integer);
    procedure OnCopyData(var Msg: TWMCopyData); message WM_COPYDATA;    
    procedure OnMessage(var Msg: TMsg; var Handled: Boolean);
    procedure OnShortCut(var Msg: TWMKey; var Handled: Boolean);
  end;

var
  frmMain: TfrmMain;
  eCPP: Boolean;

implementation

uses UnitfrmSettings, UnitMainTools, UnitLanguages, UnitfrmInfo,
  UnitCodeSnippets, UnitfrmSearch, UnitfrmReplace, UnitfrmGoToLine,
  UnitfrmAllFilesForm, UnitCodeUtils, UnitfrmPluginsIniEditor,
  UnitfrmSocketsTerminal, UnitCodeExplorerUpdater, UnitTextAnalyze,
  UnitfrmHudMsgGenerator, UnitCompile, UnitfrmAutoIndent,
  UnitfrmHTMLPreview, UnitCodeInspector, UnitfrmMOTDGen,
  UnitfrmMenuGenerator, UnitfrmClose, UnitPlugins, UnitfrmConnGen,
  UnitMenuGenerators, UnitfrmIRCPaster, MyEditFileClasses, UnitACCheck;

{$R *.dfm}

procedure TfrmMain.FormConstrainedResize(Sender: TObject; var MinWidth,
  MinHeight, MaxWidth, MaxHeight: Integer);
begin
  pnlLoading.Left := sciEditor.Left + 3 + (sciEditor.Width div 2) - (pnlLoading.Width div 2);
  pnlLoading.Top := tbDocs.Top + sciEditor.Top + ((sciEditor.Height * 5) div 6) - (pnlLoading.Height div 2);
  pnlLoading.BringToFront;

  if not Assigned(ActiveDoc) then exit;

  if (Canvas.TextWidth(ActiveDoc.FileName) + 10 > mnuFilename.CustomWidth) then
    mnuFilename.Caption := ExtractFileName(ActiveDoc.FileName)
  else
    mnuFilename.Caption := ActiveDoc.FileName;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

{ Notes -> }

procedure TfrmMain.OnCodeSnippetSelect(Sender: TObject);
begin
  mnuPawn.Checked := Sender = mnuPawn;
  mnuCPP.Checked := Sender = mnuCPP;
  mnuHTML.Checked := Sender = mnuHTML;
  mnuOther.Checked := Sender = mnuOther;
  LoadCodeSnippets((Sender as TSpTBXItem).Caption);
end;

procedure TfrmMain.cpNotesChange(Sender: TObject);
begin
  rtfNotes.SelAttributes.Color := cpNotes.Color;
end;

procedure TfrmMain.UpdateNotes;
begin
  cpNotes.Color := rtfNotes.SelAttributes.Color;
  mnuBold.Checked := fsBold in rtfNotes.SelAttributes.Style;
  mnuItalic.Checked := fsItalic in rtfNotes.SelAttributes.Style;
  mnuUnderline.Checked := fsUnderline in rtfNotes.SelAttributes.Style;
end;

procedure TfrmMain.mnuBoldClick(Sender: TObject);
begin
  if fsBold in rtfNotes.SelAttributes.Style then
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style - [fsBold]
  else
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style + [fsBold];
end;

procedure TfrmMain.mnuItalicClick(Sender: TObject);
begin
  if fsItalic in rtfNotes.SelAttributes.Style then
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style - [fsItalic]
  else
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style + [fsItalic];
end;

procedure TfrmMain.mnuUnderlineClick(Sender: TObject);
begin
  if fsUnderline in rtfNotes.SelAttributes.Style then
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style - [fsUnderline]
  else
    rtfNotes.SelAttributes.Style := rtfNotes.SelAttributes.Style + [fsUnderline];
end;

procedure TfrmMain.rtfNotesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateNotes;
end;

procedure TfrmMain.rtfNotesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateNotes;
  
  mnuModified.Caption := lModified;
  ActiveDoc.Modified := True;
end;

{ <- Notes - Code-Explorer -> }

procedure TfrmMain.trvExplorerCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 42;
  Node.SelectedIndex := 42;
end;

procedure TfrmMain.trvExplorerExpanded(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 43;
  Node.SelectedIndex := 43;
end;

{ <- Code Explorer }

procedure TfrmMain.mnuSettingsClick(Sender: TObject);
var i: integer;
  eModified: Boolean;
begin
  CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\Pawn.csl'), PChar(ExtractFilePath(ParamStr(0)) + 'config\Pawn.bak'), False);
  CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\C++.csl'), PChar(ExtractFilePath(ParamStr(0)) + 'config\C++.bak'), False);
  CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\Other.csl'), PChar(ExtractFilePath(ParamStr(0)) + 'config\Other.bak'), False);
  eModified := ActiveDoc.Modified;
  frmSettings.lstFunctions.Clear;
  for i := 0 to eACList.Count -1 do
    frmSettings.lstFunctions.Items.Add(TACFunction(eACList.Items[i]).Name);

  if frmSettings.ShowModal = mrOk then begin
    Screen.Cursor := crHourGlass;
    { Shortcuts }
    for i := 0 to frmSettings.lvShortcuts.Items.Count - 1 do
      TSciKeyCommand(frmSettings.lvShortcuts.Items[i].Data).ShortCut := (TextToShortCut(frmSettings.lvShortcuts.Items[i].SubItems[0]));
    { Tools }
    if frmSettings.chkIndentGuides.Checked then
      sciEditor.Indentation := sciEditor.Indentation + [IndentationGuides]
    else
      sciEditor.Indentation := sciEditor.Indentation - [IndentationGuides];
    sciEditor.BraceHilite := frmSettings.chkHighlightBraces.Checked;
    sciEditor.ClearUndoAfterSave := frmSettings.chkClearUndoAfterSave.Checked;
    sciEditor.AutoCloseBraces := frmSettings.chkAutoCloseBraces.Checked;
    sciEditor.AutoCloseQuotes := frmSettings.chkAutoCloseQuotes.Checked;
    if frmSettings.chkWordWrap.Checked then
      sciEditor.WordWrap := sciWrap
    else
      sciEditor.WordWrap := sciNoWrap;

    case frmSettings.cboCodeFolding.ItemIndex of
      0: sciEditor.FoldMarkers.MarkerType := sciMarkArrows;
      1: sciEditor.FoldMarkers.MarkerType := sciMarkBox;
      2: sciEditor.FoldMarkers.MarkerType := sciMarkCircle;
      3: sciEditor.FoldMarkers.MarkerType := sciMarkPlusMinus;
    end;
    if frmSettings.cboCodeFolding.ItemIndex = 4 then
      sciEditor.Folding := sciEditor.Folding - [foldFold]
    else
      sciEditor.Folding := sciEditor.Folding + [foldFold];

    sciEditor.Caret.ForeColor := frmSettings.CaretFore;
    sciEditor.Caret.LineBackColor := frmSettings.CaretBack;
    sciEditor.Caret.LineVisible := frmSettings.chkShowCaret.Checked;
    sciEditor.Caret.Period := StrToInt(frmSettings.txtPeriod.Text);
    eConfig.WriteBool('Editor', 'MakeBaks', frmSettings.chkMakeBaks.Checked);
    eConfig.WriteBool('Editor', 'DontLoadFilesTwice', frmSettings.chkDontLoadFilesTwice.Checked);
    eConfig.WriteBool('Editor', 'Auto-Indent', frmSettings.chkAutoIndent.Checked);
    eConfig.WriteBool('Editor', 'IndentOpeningBrace', frmAutoIndent.chkIndentOpeningBrace.Checked);
    eConfig.WriteBool('Editor', 'UnindentClosingBrace', frmAutoIndent.chkUnindentPressingClosingBrace.Checked);
    eConfig.WriteBool('Editor', 'UnindentEmptyLine', frmAutoIndent.chkUnindentLine.Checked);
    if (frmAutoIndent.optTabs.Checked) then
      eConfig.WriteInteger('Editor', 'IndentStyle', 0)
    else if (frmAutoIndent.optTwoSpaces.Checked) then
      eConfig.WriteInteger('Editor', 'IndentStyle', 1)
    else
      eConfig.WriteInteger('Editor', 'IndentStyle', 2);
    eConfig.WriteString('Editor', 'IndentSomethingElse', frmAutoIndent.txtSomethingElse.Text);
    eConfig.WriteBool('Editor', 'Disable_AC', frmSettings.chkDisableAC.Checked);
    eConfig.WriteBool('Editor', 'Disable_CT', frmSettings.chkDisableCT.Checked);
    eConfig.WriteBool('Editor', 'AutoHideCT', frmSettings.chkAutoHideCT.Checked);
    if frmSettings.chkAUDisable.Checked then
      eConfig.WriteString('Editor', 'AutoDisable', frmSettings.txtAUDisable.Text)
    else
      eConfig.WriteString('Editor', 'AutoDisable', '-1');

    { Editor }
    if FileExists(sciPropertyLoader.FileName) then
      sciPropertyLoader.Save;
    { Compiler }
    eConfig.WriteString('Pawn-Compiler', 'Path', frmSettings.txtPawnCompilerPath.Text);
    eConfig.WriteString('Pawn-Compiler', 'Args', frmSettings.txtPawnArgs.Text);
    eConfig.WriteString('Pawn-Compiler', 'DefaultOutput', frmSettings.txtPawnOutput.Text);
    eConfig.WriteString('CPP-Compiler', 'Path', frmSettings.txtCPPCompilerPath.Text);
    eConfig.WriteString('CPP-Compiler', 'Args', frmSettings.txtCPPCompilerArguments.Text);
    eConfig.WriteString('CPP-Compiler', 'DefaultOutput', frmSettings.txtCPPOutput.Text);
    { HL }
    eConfig.WriteString('Half-Life', 'Filename', frmSettings.txtHLExec.Text);
    eConfig.WriteString('Half-Life', 'Params', frmSettings.txtCustomParameters.Text);
    eConfig.WriteString('Half-Life', 'AMXXListen', frmSettings.txtAMXXDir.Text);
    { FTP Settings }
    eConfig.WriteString('FTP', 'Host', frmSettings.txtHost.Text);
    eConfig.WriteString('FTP', 'Port', frmSettings.txtPort.Text);
    eConfig.WriteString('FTP', 'Username', frmSettings.txtUsername.Text);
    eConfig.WriteString('FTP', 'Password', frmSettings.txtPassword.Text);
    eConfig.WriteString('FTP', 'DefaultDir', frmSettings.txtDefaultDir.Text);
    eConfig.WriteBool('FTP', 'Passive', frmSettings.chkPassive.Checked);
    { FTP Proxy }
    eConfig.WriteInteger('Proxy', 'ProxyType', frmSettings.cboProxy.ItemIndex);
    eConfig.WriteString('Proxy', 'Host', frmSettings.txtProxyHost.Text);
    eConfig.WriteString('Proxy', 'Port', frmSettings.txtProxyPort.Text);
    eConfig.WriteString('Proxy', 'Username', frmSettings.txtUsername.Text);
    eConfig.WriteString('Proxy', 'Password', frmSettings.txtProxyPassword.Text);
    { Misc }
    eConfig.WriteString('Misc', 'DefaultPluginName', frmSettings.txtDefaultName.Text);
    eConfig.WriteString('Misc', 'DefaultPluginVersion', frmSettings.txtDefaultVersion.Text);
    eConfig.WriteString('Misc', 'DefaultPluginAuthor', frmSettings.txtDefaultAuthor.Text);
    if frmSettings.optFileComment.Checked then
      eConfig.WriteInteger('Misc', 'SaveNotesTo', 0)
    else if frmSettings.optConfig.Checked then
      eConfig.WriteInteger('Misc', 'SaveNotesTo', 1)
    else
      eConfig.WriteInteger('Misc', 'SaveNotesTo', 3);
    eConfig.WriteInteger('Misc', 'CPUSpeed', frmSettings.sldSpeed.Value);
    eConfig.WriteString('Misc', 'LangDir', frmSettings.txtLangDir.Text);
    eConfig.WriteBool('Misc', 'ShowStatusbar', frmSettings.chkShowStatusbar.Checked);
    eACList.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\ACList.cfg');
    Screen.Cursor := crDefault;
  end
  else begin
    { Restore Code-Snippets }
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\Pawn.csl');
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\C++.csl');
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\Other.csl');
    CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\Pawn.bak'), PChar(ExtractFilePath(ParamStr(0)) + 'config\Pawn.csl'), False);
    CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\C++.bak'), PChar(ExtractFilePath(ParamStr(0)) + 'config\C++.csl'), False);
    CopyFile(PChar(ExtractFilePath(ParamStr(0)) + 'config\Other.bak'), PChar(ExtractFilePath(ParamStr(0)) + 'config\Other.csl'), False);
    eACList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\ACList.cfg');
  end;

  if mnuPawn.Checked then
    LoadCodeSnippets('Pawn')
  else if mnuCPP.Checked then
    LoadCodeSnippets('C++')
  else if mnuHTML.Checked then
    LoadCodeSnippets('HTML')
  else
    LoadCodeSnippets('Other');

  DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\Pawn.bak');
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\C++.bak');
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'config\Other.bak');

  if FileExists(sciPropertyLoader.FileName) then
    sciPropertyLoader.Load;
  ReloadIni;

  ActiveDoc.Modified := eModified;
  if eModified then
    mnuModified.Caption := lModified
  else
    mnuModified.Caption := '';

  sciEditor.Gutter1.MarginType := gutLineNumber;
  sciEditor.Gutter1.Width := 40;
end;

{ Settings -> }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if (Screen.Cursor = crDefault) and (not pnlLoading.Visible) then begin
    Started := True;
    TCodeExplorerUpdater.Create(False);
    ReloadIni;
  end;
end;

{ <- Settings | Toolbars -> }

procedure TfrmMain.mnuTOpenClick(Sender: TObject);
begin
  mnuOpen.Click;
end;

procedure TfrmMain.mnuTSaveClick(Sender: TObject);
begin
  mnuSave.Click;
end;

procedure TfrmMain.mnuTSearchClick(Sender: TObject);
begin
  mnuSearch.Click;
end;

procedure TfrmMain.mnuTCompileClick(Sender: TObject);
begin
  mnuDoCompile.Click;
end;

procedure TfrmMain.mnuTUndoClick(Sender: TObject);
begin
  mnuUndo.Click;
end;

procedure TfrmMain.mnuTRedoClick(Sender: TObject);
begin
  mnuRedo.Click;
end;

procedure TfrmMain.mnuTCopyClick(Sender: TObject);
begin
  mnuCopy.Click;
end;

procedure TfrmMain.mnuTCutClick(Sender: TObject);
begin
  mnuCut.Click;
end;

procedure TfrmMain.mnuTPasteClick(Sender: TObject);
begin
  mnuPaste.Click;
end;

procedure TfrmMain.mnuTSelectAllClick(Sender: TObject);
begin
  mnuSelectAll.Click;
end;

procedure TfrmMain.mnuEmptyPluginClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_Pawn_EMPTYPLUGIN, True) then exit;

  if stlIDEs.ItemIndex <> 0 then
    ActivateProjects(0, False);

  PawnProjects.Activate(PawnProjects.Add(''), True);
  Plugin_CreateNewFile(NEW_Pawn_EMPTYPLUGIN, False);
end;

procedure TfrmMain.mnuNewPluginClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_Pawn_PLUGIN, True) then exit;

  if stlIDEs.ItemIndex <> 0 then
    ActivateProjects(0, False);

  PawnProjects.Activate(PawnProjects.Add(''), False);
  sciEditor.Lines.Add('/* Plugin generated by AMXX-Studio */');
  sciEditor.Lines.Add('');
  sciEditor.Lines.Add('#include <amxmodx>');
  sciEditor.Lines.Add('#include <amxmisc>');
  sciEditor.Lines.Add('');
  sciEditor.Lines.Add('#define PLUGIN "' + eConfig.ReadString('Misc', 'DefaultPluginName', 'New Plug-In') + '"');
  sciEditor.Lines.Add('#define VERSION "' + eConfig.ReadString('Misc', 'DefaultPluginVersion', '1.0') + '"');
  sciEditor.Lines.Add('#define AUTHOR "' + eConfig.ReadString('Misc', 'DefaultPluginAuthor', 'Your Name') + '"');
  sciEditor.Lines.Add('');
  sciEditor.Lines.Add('');
  sciEditor.Lines.Add('public plugin_init() {');
  sciEditor.Lines.Add('	register_plugin(PLUGIN, VERSION, AUTHOR)');
  sciEditor.Lines.Add('	');
  sciEditor.Lines.Add('	// Add your code here...');
  sciEditor.Lines.Add('}');
  ActiveDoc.Modified := False;

  Plugin_CreateNewFile(NEW_Pawn_PLUGIN, False);
end;

procedure TfrmMain.mnuHXMLClick(Sender: TObject);
begin
  SelectLanguage((Sender as TSpTBXItem).Caption);
end;

procedure TfrmMain.mnuHNoneClick(Sender: TObject);
begin
  SelectLanguage('null');
end;

procedure TfrmMain.mnuOpenClick(Sender: TObject);
var eExt: string;
begin
  if Assigned(Sender) then begin
    if not odOpen.Execute then
      exit;
  end;

  eExt := ExtractFileExt(odOpen.FileName);
  eExt := LowerCase(eExt);
  if (eExt = '.sma') or (eExt = '.inc') or (eExt = '.inl') then begin // Pawn files
    if stlIDEs.ItemIndex <> 0 then
      ActivateProjects(0, False);
    PawnProjects.Open(odOpen.FileName);
  end
  else if (eExt = '.cpp') or (eExt = '.h') then begin // C++ files
    if not eCPP then
      MessageBox(Handle, PChar(lNoCPP), PChar(Application.Title), MB_ICONWARNING)
    else begin
      if stlIDEs.ItemIndex <> 1 then
        ActivateProjects(1, False);
      CPPProjects.Open(odOpen.FileName);
    end;
  end
  else if (eExt = '.htm') or (eExt = '.html') then begin // HTML files
    if stlIDEs.ItemIndex <> 2 then
      ActivateProjects(2, False);
    OtherProjects.Open(odOpen.FileName, 'HTML');
  end
  else if (eExt = '.sql') then begin // SQL databases
    if stlIDEs.ItemIndex <> 2 then
      ActivateProjects(2, False);
    OtherProjects.Open(odOpen.FileName, 'SQL');
  end
  else if (eExt = '.xml') then begin // XML files
    if stlIDEs.ItemIndex <> 2 then
      ActivateProjects(2, False);
    OtherProjects.Open(odOpen.FileName, 'XML');
  end
  else begin // Other files and/or Textfiles
    if stlIDEs.ItemIndex <> 2 then
      ActivateProjects(2, False);
    OtherProjects.Open(odOpen.FileName, 'null');
  end;
end;

procedure TfrmMain.mnuNewTextfileClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_OTHER_TEXTFILE, True) then exit;

  if stlIDEs.ItemIndex <> 2 then
    ActivateProjects(2, False);

  OtherProjects.Activate(OtherProjects.Add(''), True);

  Plugin_CreateNewFile(NEW_OTHER_TEXTFILE, False);
end;

procedure TfrmMain.mnuNewHTMLClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_OTHER_HTML, True) then exit;

  if stlIDEs.ItemIndex <> 2 then
    ActivateProjects(2, True);

  OtherProjects.Activate(OtherProjects.Add('', 'HTML'), True);
  sciEditor.Lines.Add('<html>');
  sciEditor.Lines.Add(#9'<head>');
  sciEditor.Lines.Add(#9#9'<title>Your Title</title>');
  sciEditor.Lines.Add(#9'</head>');
  sciEditor.Lines.Add(#9'<body>');
  sciEditor.Lines.Add(#9#9'<-- Your text here -->');
  sciEditor.Lines.Add(#9'</body>');
  sciEditor.Lines.Add('</html>');

  Plugin_CreateNewFile(NEW_OTHER_HTML, False);
end;

procedure TfrmMain.mnuNewSQLClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_OTHER_SQL, True) then exit;

  if stlIDEs.ItemIndex <> 2 then
    ActivateProjects(2, False);

  OtherProjects.Activate(OtherProjects.Add('', 'SQL'), True);

  Plugin_CreateNewFile(NEW_OTHER_SQL, False);
end;

procedure TfrmMain.mnuNewXMLClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_OTHER_XML, True) then exit;

  if stlIDEs.ItemIndex <> 2 then
    ActivateProjects(2, False);

  OtherProjects.Activate(OtherProjects.Add('', 'XML'), True);

  Plugin_CreateNewFile(NEW_OTHER_XML, False);
end;

procedure TfrmMain.mnuUndoClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.Undo
  else if ActiveControl = rtfNotes then
    rtfNotes.Undo;
end;

procedure TfrmMain.mnuSaveClick(Sender: TObject);
begin
  if ActiveDoc.Untitled then
    mnuSaveAs.Click
  else begin
    ActiveDoc.Save;
    mnuModified.Caption := '';
  end;
end;

procedure TfrmMain.mnuSaveAsClick(Sender: TObject);
begin
  if sdSave.Execute then begin
    ActiveDoc.FileName := AddExtension(sdSave.FileName, ActiveDoc.Highlighter, ActiveDoc);
    ActiveDoc.Save;
    tbDocs.Tabs[ActiveDoc.Index].Caption := ActiveDoc.Title;
  end;
end;

procedure TfrmMain.mnuThemesClick(Sender: TObject);
begin
  if Started then
    eConfig.WriteString('Misc', 'Theme', TBXCurrentTheme);
  Plugin_ThemeChange(TBXCurrentTheme);
end;

procedure TfrmMain.mnuInfoClick(Sender: TObject);
begin
  if Plugin_ShowHelp(HELP_ABOUT) then
    frmInfo.ShowModal;
end;

procedure TfrmMain.mnuRedoClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.Redo
  else if ActiveControl = rtfNotes then
    rtfNotes.Undo;
end;

procedure TfrmMain.mnuCutClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.Cut
  else if ActiveControl = rtfNotes then
    rtfNotes.CutToClipboard;
end;

procedure TfrmMain.mnuCopyClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.Copy
  else if ActiveControl = rtfNotes then
    rtfNotes.CopyToClipboard;
end;

procedure TfrmMain.mnuPasteClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.Paste
  else if ActiveControl = rtfNotes then
    rtfNotes.PasteFromClipboard;
end;

procedure TfrmMain.mnuSelectAllClick(Sender: TObject);
begin
  if ActiveControl = sciEditor then
    sciEditor.SelectAll
  else if ActiveControl = rtfNotes then
    rtfNotes.SelectAll;
end;

procedure TfrmMain.mnuCloseClick(Sender: TObject);
begin
  CloseDocument(ActiveDoc, True, True);
end;

procedure TfrmMain.mnuShowCodeToolsClick(Sender: TObject);
begin
  if not Plugin_VisibleControlChange(CTRL_CODETOOLS_MAIN, not tcTools.Visible) then exit;

  tcTools.Visible := (Sender as TSpTBXItem).Checked;
  splRight.Visible := (Sender as TSpTBXItem).Checked;
  mnuShowCodeTools.Checked := (Sender as TSpTBXItem).Checked;
  mnuShowCodeToolsWindow.Checked := (Sender as TSpTBXItem).Checked;
  Application.ProcessMessages;

  Plugin_VisibleControlChange(CTRL_CODETOOLS_MAIN, tcTools.Visible);
end;

procedure TfrmMain.sciEditorClick(Sender: TObject);
begin
  mnuCaret.Caption := Format(lLnCh, [sciEditor.GetCurrentLineNumber + 1, sciEditor.GetCaretInLine + 1]);
  Plugin_EditorClick(False);
  Plugin_UpdateSel(sciEditor.SelStart, sciEditor.SelLength, sciEditor.GetFirstVisibleLine);
end;

procedure TfrmMain.sciEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  mnuCaret.Caption := Format(lLnCh, [sciEditor.GetCurrentLineNumber + 1, sciEditor.GetCaretInLine + 1]);
  Plugin_UpdateSel(sciEditor.SelStart, sciEditor.SelLength, sciEditor.GetFirstVisibleLine);

  if Key = 46 then begin
    mnuModified.Caption := lModified;
    ActiveDoc.Modified := True;
  end;

  if sciEditor.Caret.LineVisible <> frmSettings.chkShowCaret.Checked then
    sciEditor.Caret.LineVisible := frmSettings.chkShowCaret.Checked;
  if sciEditor.Caret.LineBackColor <> frmSettings.CaretBack then begin
    sciEditor.Caret.LineBackColor := frmSettings.CaretBack;
    sciEditor.Colors.SelBack := clHighlight;

    i := sciEditor.SelLength;
    sciEditor.SelLength := 0;
    sciEditor.SelStart := sciEditor.SelStart + i;
  end;
end;

procedure TfrmMain.sciEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var eStr: string;
begin
  if not Started then exit;

  mnuCaret.Caption := Format(lLnCh, [sciEditor.GetCurrentLineNumber + 1, sciEditor.GetCaretInLine + 1]);
  Plugin_UpdateSel(sciEditor.SelStart, sciEditor.SelLength, sciEditor.GetFirstVisibleLine);

  if (Key = 13) and (frmSettings.chkAutoIndent.Checked) and (Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) = '') then begin
    if (sciEditor.LanguageManager.SelectedLanguage = 'Pawn') or (sciEditor.LanguageManager.SelectedLanguage = 'C++') then begin
      eStr := Trim(RemoveStringsAndComments(sciEditor.Lines[sciEditor.GetCurrentLineNumber - 1], True, True));
      if (Copy(eStr, Length(eStr), 1) = '{') and (frmAutoIndent.chkIndentOpeningBrace.Checked) then begin
        if (frmAutoIndent.optTabs.Checked) then
          sciEditor.SelText := #9
        else if (frmAutoIndent.optTwoSpaces.Checked) then
          sciEditor.SelText := '  '
        else
          sciEditor.SelText := frmAutoIndent.txtSomethingElse.Text;
      end;
      if (eStr = '') and (frmAutoIndent.chkUnindentLine.Checked) then begin
        sciEditor.Lines[sciEditor.GetCurrentLineNumber] := Copy(sciEditor.Lines[sciEditor.GetCurrentLineNumber], 1, Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) - 1); // remove last indent..
        sciEditor.SelStart := sciEditor.SelStart + Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]); // and jump to last position
      end;
    end;
  end;

  Application.ProcessMessages;
  UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);
end;

procedure TfrmMain.mnuFoldAllClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  sciEditor.FoldAll;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.tbxToolbarVisibleChanged(Sender: TObject);
begin
  mnuShowFileTB.Checked := tbxToolbar.Visible;
end;

procedure TfrmMain.mnuShowFileTBClick(Sender: TObject);
begin
  tbxToolbar.Visible := mnuShowFileTB.Checked;
end;

procedure TfrmMain.tbxEditVisibleChanged(Sender: TObject);
begin
  mnuShowEditTB.Checked := tbxEdit.Visible;
end;

procedure TfrmMain.mnuShowEditTBClick(Sender: TObject);
begin
  tbxEdit.Visible := mnuShowEditTB.Checked;
end;

procedure TfrmMain.tbxCodeSnippetsVisibleChanged(Sender: TObject);
begin
  mnuShowCodeSnippets.Checked := tbxCodeSnippets.Visible;
end;

procedure TfrmMain.mnuShowCodeSnippetsClick(Sender: TObject);
begin
  tbxCodeSnippets.Visible := mnuShowCodeSnippets.Checked;
end;

procedure TfrmMain.OnCodeSnippetClick(Sender: TObject);
var Snippet, Indentation: String;
    Line: Integer;
begin
  if Plugin_CodeSnippetClick(TSpTBXItem(Sender).Caption, GetCat, GetSnippet(GetCat, (Sender as TSpTBXItem).Caption)) then begin
    Snippet := GetSnippet(GetCat, (Sender as TSpTBXItem).Caption);
    if (Pos('!APPEND!' + #13, Snippet) = 1) then begin
      Snippet := Copy(Snippet, Pos(#10, Snippet)+1, Length(Snippet));
      Line := sciEditor.Lines.Add(Snippet);
    end
    else if (Pos('!INSERT!' + #13, Snippet) = 1) then begin
      Indentation := sciEditor.Lines[sciEditor.GetCurrentLineNumber];
      if (Trim(Indentation) <> '') then
        Indentation := Copy(Indentation, 1, Pos(Copy(TrimLeft(Indentation), 1, 1), Indentation)-1);
      Snippet := StringReplace(Snippet, #10, #10 + Indentation, [rfReplaceAll]);
      Line := sciEditor.GetCurrentLineNumber;
      sciEditor.Lines.Insert(Line, Copy(Snippet, Pos(#10, Snippet)+1, Length(Snippet)));
    end
    else begin
      sciEditor.SelText := Snippet;
      Line := sciEditor.GetCurrentLineNumber;
    end;

    sciEditor.GoToLine(Line);
  end;
end;

procedure TfrmMain.mnuCopyMessageClick(Sender: TObject);
begin
  if lstOutput.ItemIndex <> -1 then
    Clipboard.SetTextBuf(PChar(lstOutput.Items[lstOutput.ItemIndex]));
end;

procedure TfrmMain.mnuCopyAllClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(lstOutput.Items.Text));
end;

procedure TfrmMain.mnuSaveToFileClick(Sender: TObject);
begin
  sdSave.FilterIndex := 7;
  if sdSave.Execute then
    lstOutput.Items.SaveToFile(sdSave.FileName);
end;

procedure TfrmMain.ppmOutputInitPopup(Sender: TObject; PopupView: TTBView);
begin
  mnuCopyMessage.Enabled := lstOutput.ItemIndex <> -1;
end;

procedure TfrmMain.mnuSearchForumsClick(Sender: TObject);
begin
  if Plugin_ShowHelp(HELP_SEARCH) then
    ShellExecute(Handle, 'open', 'http://www.amxmodx.org/forums/search.php', nil, nil, SW_SHOW);
end;

procedure TfrmMain.mnuOpenScriptingForumClick(Sender: TObject);
begin
  if Plugin_ShowHelp(HELP_FORUMS) then
    ShellExecute(Handle, 'open', 'http://www.amxmodx.org/forums/viewforum.php?f=8', nil, nil, SW_SHOW);
end;

procedure TfrmMain.mnuEditorUndoClick(Sender: TObject);
begin
  mnuUndo.Click;
end;

procedure TfrmMain.mnuEditorRedoClick(Sender: TObject);
begin
  mnuRedo.Click;
end;

procedure TfrmMain.mnuEditorCopyClick(Sender: TObject);
begin
  mnuCopy.Click;
end;

procedure TfrmMain.mnuEditorCutClick(Sender: TObject);
begin
  mnuCut.Click;
end;

procedure TfrmMain.mnuEditorPasteClick(Sender: TObject);
begin
  mnuPaste.Click;
end;

procedure TfrmMain.mnuToogleBookmarkClick(Sender: TObject);
begin
  sciEditor.Bookmark.Toggle(sciEditor.GetCurrentLineNumber);
end;

procedure TfrmMain.mnuEditorDeleteClick(Sender: TObject);
begin
  sciEditor.Clear;
end;

procedure TfrmMain.mnuEditorSelectAllClick(Sender: TObject);
begin
  sciEditor.SelectAll;
end;

procedure TfrmMain.mnuGoToBookmarkClick(Sender: TObject);
begin
  sciEditor.Bookmark.Next(True);
end;

procedure TfrmMain.mnuSearchDialogClick(Sender: TObject);
begin
  Plugin_Search(frmSearch.cboSearchFor.Items.Text, frmSearch.cboSearchFor.Text, True, False, frmSearch.chkCaseSensivity.Checked, frmSearch.chkWholeWordsOnly.Checked, frmSearch.chkSearchFromCaret.Checked, frmSearch.chkSelectedTextOnly.Checked, frmSearch.chkRegularExpression.Checked, frmSearch.chkForward.Checked);
  if frmSearch.ShowModal = mrOk then begin
    if not Plugin_Search(frmSearch.cboSearchFor.Items.Text, frmSearch.cboSearchFor.Text, False, False, frmSearch.chkCaseSensivity.Checked, frmSearch.chkWholeWordsOnly.Checked, frmSearch.chkSearchFromCaret.Checked, frmSearch.chkSelectedTextOnly.Checked, frmSearch.chkRegularExpression.Checked, frmSearch.chkForward.Checked) then
      exit;

    with sciSearchReplace do begin
      SearchBackwards := frmSearch.chkBackward.Checked;
      SearchCaseSensitive := frmSearch.chkCaseSensivity.Checked;
      SearchFromCaret := frmSearch.chkSearchFromCaret.Checked;
      SearchSelectionOnly := frmSearch.chkSelectedTextOnly.Checked;
      SearchWholeWords := frmSearch.chkWholeWordsOnly.Checked;
      SearchRegex := frmSearch.chkRegularExpression.Checked;
      SearchText := frmSearch.cboSearchFor.Text;
      Screen.Cursor := crHourGlass;
      DoSearchReplaceText(False, frmSearch.chkBackward.Checked);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.mnuReplaceClick(Sender: TObject);
var Stop: Boolean;
begin
  if frmReplace.ShowModal = mrOk then begin
    if not Plugin_SearchReplace(frmReplace.cboSearchFor.Text, frmReplace.cboReplaceWith.Text, frmReplace.cboSearchFor.Items.Text, frmReplace.cboReplaceWith.Items.Text, frmSearch.chkCaseSensivity.Checked, frmSearch.chkWholeWordsOnly.Checked, frmSearch.chkSearchFromCaret.Checked, frmSearch.chkSelectedTextOnly.Checked, frmSearch.chkRegularExpression.Checked, frmSearch.chkForward.Checked) then
      exit;

    with sciSearchReplace do begin
      SearchBackwards := frmReplace.chkBackward.Checked;
      SearchCaseSensitive := frmReplace.chkCaseSensivity.Checked;
      // I don't like this but it works so far
      if (frmReplace.chkReplaceAll.Checked) and (Pos(LowerCase(frmReplace.cboSearchFor.Text), LowerCase(frmReplace.cboReplaceWith.Text)) <> 0) then
        SearchFromCaret := True
      else
        SearchFromCaret := frmReplace.chkSearchFromCaret.Checked;
      SearchSelectionOnly := frmReplace.chkSelectedTextOnly.Checked;
      SearchWholeWords := frmReplace.chkWholeWordsOnly.Checked;
      SearchRegex := frmReplace.chkRegularExpression.Checked;
      SearchText := frmReplace.cboSearchFor.Text;

      Stop := False;
      Screen.Cursor := crHourGlass;
      repeat
        DoSearchReplaceText(False, frmReplace.chkBackward.Checked);
        if sciEditor.SelLength = 0 then
          Stop := True
        else
          sciEditor.SelText := frmReplace.cboReplaceWith.Text;
      until (not frmReplace.chkReplaceAll.Checked) or (Stop);
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.mnuSearchAgainClick(Sender: TObject);
begin
  if not Plugin_Search(frmSearch.cboSearchFor.Items.Text, frmSearch.cboSearchFor.Text, False, True, frmSearch.chkCaseSensivity.Checked, frmSearch.chkWholeWordsOnly.Checked, frmSearch.chkSearchFromCaret.Checked, frmSearch.chkSelectedTextOnly.Checked, frmSearch.chkRegularExpression.Checked, frmSearch.chkForward.Checked) then
    exit;

  sciSearchReplace.SearchText := frmSearch.cboSearchFor.Text;
  sciSearchReplace.SearchFromCaret := True;
  Screen.Cursor := crHourGlass;
  sciSearchReplace.DoSearchReplaceText(False, frmSearch.chkBackward.Checked);
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.mnuGoToLineClick(Sender: TObject);
begin
  if frmGoToLine.ShowModal = mrOk then begin
    sciEditor.GotoLineEnsureVisible(StrToInt(frmGoToLine.txtGoToLine.Text) - 1);
    UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);
  end;
end;

procedure TfrmMain.mnuPrintClick(Sender: TObject);
begin
  sciPrinter.Title := ExtractFileName(ActiveDoc.FileName);
  sciPrinter.PrintRange := prAllPages;
  if sciEditor.SelLength <> 0 then begin
    case MessageBox(Handle, PChar(lPrintSelection), PChar(Application.Title), MB_ICONQUESTION + MB_YESNOCANCEL) of
      mrYes: sciPrinter.PrintRange := prSelection;
      mrCancel: exit;
    end;
  end;
  sciPrinter.Print;
end;

procedure TfrmMain.mnuSaveAllFilesClick(Sender: TObject);
var a, b: integer;
  Collection: TDocCollection;
begin
  case stlIDEs.ItemIndex of
    0: Collection := PawnProjects;
    1: Collection := CPPProjects;
    else Collection := OtherProjects;
  end;

  frmAllFilesForm.Caption := lSaveAllCaption1;
  frmAllFilesForm.lblCaption.Caption := lSaveAllCaption2;

  frmAllFilesForm.lstFiles.Clear;
  for a := 0 to Collection.Count - 1 do begin
    if TDocument(Collection.Items[a]).Modified then
      frmAllFilesForm.lstFiles.Checked[frmAllFilesForm.lstFiles.Items.AddObject(IntToStr(a + 1) + ') ' + ExtractFileName(TDocument(Collection.Items[a]).FileName), TObject(a))] := True;
  end;

  if frmAllFilesForm.lstFiles.Items.Count = 0 then begin
    MessageBox(Handle, PChar(lNoFilesToSave), PChar(Application.Title), MB_ICONINFORMATION);
    exit;
  end;

  if frmAllFilesForm.ShowModal = mrOk then begin
    for a := 0 to frmAllFilesForm.lstFiles.Items.Count - 1 do begin
      if frmAllFilesForm.lstFiles.Checked[a] then begin
        b := Integer(frmAllFilesForm.lstFiles.Items.Objects[a]);
        if TDocument(Collection.Items[b]).Untitled then begin
          if sdSave.Execute then begin
            TDocument(Collection.Items[b]).FileName := AddExtension(sdSave.FileName, TDocument(Collection.Items[b]).Highlighter, TDocument(Collection.Items[b]));
            TDocument(Collection.Items[b]).Save;
            TJvTabBarItem(tbDocs.Tabs[b]).Caption := TDocument(Collection.Items[b]).Title;
          end
          else
            exit;
        end
        else
          TDocument(Collection.Items[b]).Save;
        if ActiveDoc = Collection.Items[b] then
          mnuModified.Caption := '';
      end;
    end;
  end;
end;

procedure TfrmMain.sciEditorKeyPress(Sender: TObject; var Key: Char);
begin
  if Started then begin
    if not Plugin_KeyPress(Key) then begin
      Key := #0;
      exit;
    end;

    mnuModified.Caption := lModified;
    ActiveDoc.Modified := True;

    if (Key = '}') and (frmSettings.chkAutoIndent.Checked) then begin
      if (Trim(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) = '') and (frmAutoIndent.chkUnindentPressingClosingBrace.Checked) then begin
        if (sciEditor.LanguageManager.SelectedLanguage = 'Pawn') or (sciEditor.LanguageManager.SelectedLanguage = 'C++') then begin
          // remove last indentation..
          if (frmAutoIndent.optTabs.Checked)then
            sciEditor.Lines[sciEditor.GetCurrentLineNumber] := Copy(sciEditor.Lines[sciEditor.GetCurrentLineNumber], 1, Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) - 1)
          else if (frmAutoIndent.optTwoSpaces.Checked) then
            sciEditor.Lines[sciEditor.GetCurrentLineNumber] := Copy(sciEditor.Lines[sciEditor.GetCurrentLineNumber], 1, Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) - 2)
          else
            sciEditor.Lines[sciEditor.GetCurrentLineNumber] := Copy(sciEditor.Lines[sciEditor.GetCurrentLineNumber], 1, Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]) - Length(frmAutoIndent.txtSomethingElse.Text));
          sciEditor.SelStart := sciEditor.SelStart + Length(sciEditor.Lines[sciEditor.GetCurrentLineNumber]); // and jump to last position
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.mnuCloseAllFilesClick(Sender: TObject);
var i: integer;
  Collection: TDocCollection;
begin
  case stlIDEs.ItemIndex of
    0: Collection := PawnProjects;
    1: Collection := CPPProjects;
    else Collection := OtherProjects;
  end;

  frmAllFilesForm.Caption := lCloseAllCaption1;
  frmAllFilesForm.lblCaption.Caption := lCloseAllCaption2;

  frmAllFilesForm.lstFiles.Clear;
  for i := 0 to Collection.Count - 1 do
    frmAllFilesForm.lstFiles.Checked[frmAllFilesForm.lstFiles.Items.Add(IntToStr(i + 1) + ') ' + ExtractFileName(TDocument(Collection.Items[i]).FileName))] := True;

  if frmAllFilesForm.ShowModal = mrOk then begin
    for i := Collection.Count - 1 downto 0 do begin
      if not CloseDocument(TDocument(Collection.Items[i]), True, True) then
        exit;
    end;
  end;
end;

procedure TfrmMain.mnuIndenterClick(Sender: TObject);
begin
  IndentCode;
end;

procedure TfrmMain.mnuUnindenterClick(Sender: TObject);
begin
  UnindentCode;
end;

procedure TfrmMain.mnuPasterClick(Sender: TObject);
var i: integer;
  eTo, eFrom: Integer;
  eLine: string;
begin
  if FindWindow('mirc', nil) = 0 then begin
    MessageBox(Handle, PChar(lNoMIRCWindowOpen), PChar(Application.Title), MB_ICONERROR);
    exit;
  end;

  frmIRCPaster.chkDelay.Checked := Pos('gamesurge', LowerCase(mIRCGet('mIRC', 'SERVER', 'SERVER'))) = 0;
  if frmIRCPaster.ShowModal = mrOk then begin
    { All }
    if frmIRCPaster.optAll.Checked then begin
      eFrom := 0;
      eTo := sciEditor.Lines.Count - 1;
    end
    { Special Lines }
    else if frmIRCPaster.optLines.Checked then begin
      eFrom := StrToInt(frmIRCPaster.txtFrom.Text) - 1;
      eTo := StrToInt(frmIRCPaster.txtTo.Text) - 1;
    end
    { Selected }
    else begin
      eFrom := LineFromPos(sciEditor.SelStart);
      eTo := LineFromPos(sciEditor.SelStart + sciEditor.SelLength);
    end;
    pbLoading.Max := eTo - eFrom;
    pbLoading.Position := 0;
    ShowProgress(True);
    for i := eFrom to eTo do begin
      if (FindWindow('mirc', nil) = 0) or (Application.Terminated) or (IRCPasterStop) then
        break;

      pbLoading.Position := i;
      SetProgressStatus('Pasting code...');

      eLine := sciEditor.Lines[i];
      eLine := Trim(eLine);
      eLine := StringReplace(eLine, #9, #32, [rfReplaceAll]);
      if frmIRCPaster.chkColors.Checked then
        mIRCDDE('mIRC', 'COMMAND', '/msg ' + frmIRCPaster.txtChannel.Text + #32 + GetColoredLine(i))
      else
        mIRCDDE('mIRC', 'COMMAND', '/msg ' + frmIRCPaster.txtChannel.Text + #32 + eLine);

      if frmIRCPaster.chkDelay.Checked then
        Delay(eConfig.ReadInteger('Misc', 'IRCPasteDelay', 2500));
    end;
    HideProgress;
  end;
end;

procedure TfrmMain.mnuPluginsIniEditorClick(Sender: TObject);
begin
  frmPluginsIniEditor.txtFile.Clear;

  if FileExists(GetAMXXDir(True) + 'configs\plugins.ini') then
    frmPluginsIniEditor.chkEditLocal.Click
  else if (frmSettings.txtHost.Text <> '') and (frmPluginsIniEditor.chkEditFTP.Checked) then
    frmPluginsIniEditor.chkEditFTP.Click
  else
    frmPluginsIniEditor.chkEditLocal.Click;

  if frmPluginsIniEditor.txtFile.Text = '' then exit;

  if frmPluginsIniEditor.ShowModal = mrOk then begin
    Screen.Cursor := crHourGlass;
    if frmPluginsIniEditor.chkEditFTP.Checked then begin
      if not IdFTP.Connected then begin
        if TryConnect <> 0 then
          exit;
      end;

      try
        IdFTP.ChangeDir(frmSettings.txtDefaultDir.Text + 'configs/');
      except
        IdFTP.Disconnect;
        MessageBox(Handle, PChar(lInvalidDirectory), PChar(Application.Title), MB_ICONERROR);
        Screen.Cursor := crDefault;
        exit;
      end;

      frmPluginsIniEditor.txtFile.Lines.SaveToFile(ExtractFilePath(ParamStr(0)) + 'plugins.ini');
      IdFTP.TransferType := ftASCII;
      IdFTP.Put(ExtractFilePath(ParamStr(0)) + 'plugins.ini', 'plugins.ini');
      DeleteFile(ExtractFilePath(ParamStr(0)) + 'plugins.ini');
    end
    else
      frmPluginsIniEditor.txtFile.Lines.SaveToFile(frmPluginsIniEditor.odOpen.FileName);
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.cmdCancelClick(Sender: TObject);
begin
  Cancel := True;
  HideProgress;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.mnuSocketTerminalClick(Sender: TObject);
begin
  frmSocketsTerminal.ShowModal;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var i, k: integer;
  eRoot: TTreeNode;
  eItem: TDocument;
  eSavedFiles: TStringList;
begin
  ActiveDoc.Code := sciEditor.Lines.Text;
  frmClose.trvFiles.Items.Clear;
  { Pawn Projects }
  eRoot := frmClose.trvFiles.Items.Add(nil, stlIDEs.Strings[0]);
  for i := 0 to PawnProjects.Count - 1 do begin
    if TDocument(PawnProjects.Items[i]).Modified then
      frmClose.trvFiles.Items.AddChild(eRoot, IntToStr(i + 1) + '. ' + ExtractFileName(TDocument(PawnProjects.Items[i]).FileName));
  end;
  if eRoot.Count = 0 then
    eRoot.Destroy
  else
    eRoot.Expand(False);
  { C++ Projects }
  eRoot := frmClose.trvFiles.Items.Add(nil, stlIDEs.Strings[1]);
  for i := 0 to CPPProjects.Count - 1 do begin
    if TDocument(CPPProjects.Items[i]).Modified then
      frmClose.trvFiles.Items.AddChild(eRoot, IntToStr(i + 1) + '. ' + ExtractFileName(TDocument(CPPProjects.Items[i]).FileName));
  end;
  if eRoot.Count = 0 then
    eRoot.Destroy
  else
    eRoot.Expand(False);
  { Other Projects }
  eRoot := frmClose.trvFiles.Items.Add(nil, stlIDEs.Strings[2]);
  for i := 0 to OtherProjects.Count - 1 do begin
    if TDocument(OtherProjects.Items[i]).Modified then
      frmClose.trvFiles.Items.AddChild(eRoot, IntToStr(i + 1) + '. ' + ExtractFileName(TDocument(OtherProjects.Items[i]).FileName));
  end;
  if eRoot.Count = 0 then
    eRoot.Destroy
  else
    eRoot.Expand(False);

  eSavedFiles := TStringList.Create;
  eSavedFiles.Clear;
  for i := 0 to frmSettings.lvPlugins.Items.Count - 1 do begin
    if frmSettings.lvPlugins.Items[i].Data <> nil then
      eSavedFiles.Add('LOADED ' + frmSettings.lvPlugins.Items[i].SubItems[0])
    else
      eSavedFiles.Add('UNLOADED ' + frmSettings.lvPlugins.Items[i].SubItems[0]);
  end;
  eSavedFiles.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\Plugins.cfg');
  eSavedFiles.Clear;

  if frmClose.trvFiles.Items.Count <> 0 then begin
    frmClose.cmdSave.Caption := lCloseCaption;
    if (frmClose.ShowModal = mrOk) then begin
      if frmClose.cmdSave.Caption = lSaveCaption then begin
        for i := 0 to frmClose.trvFiles.Items.Count - 1 do begin
          { Pawn Projects }
          if frmClose.trvFiles.Items[i].Text = stlIDEs.Strings[0] then begin
            with frmClose.trvFiles.Items[i] do begin
              for k := 0 to Count - 1 do begin
                if frmClose.trvFiles.Checked[Item[k]] then begin
                  eItem := TDocument(PawnProjects.Items[StrToInt(Copy(Item[k].Text, 1, Pos('.', Item[k].Text) - 1)) - 1]);
                  // Process item here
                  if not eItem.Untitled then
                    eItem.Save
                  else begin
                    frmMain.sdSave.FilterIndex := 1;
                    if frmMain.sdSave.Execute then begin
                      eItem.FileName := AddExtension(frmMain.sdSave.FileName, eItem.Highlighter, eItem);
                      eItem.Save;
                    end
                    else begin
                      Action := caNone;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
          end;
          { C++ Projects }
          if frmClose.trvFiles.Items[i].Text = stlIDEs.Strings[1] then begin
            with frmClose.trvFiles.Items[i] do begin
              for k := 0 to Count - 1 do begin
                if frmClose.trvFiles.Checked[Item[k]] then begin
                  eItem := TDocument(CPPProjects.Items[StrToInt(Copy(Item[k].Text, 1, Pos('.', Item[k].Text) - 1)) - 1]);
                  // Process item here
                  if not eItem.Untitled then
                    eItem.Save
                  else begin
                    frmMain.sdSave.FilterIndex := 2;
                    if frmMain.sdSave.Execute then begin
                      eItem.FileName := AddExtension(frmMain.sdSave.FileName, eItem.Highlighter, eItem);
                      eItem.Save;
                    end
                    else begin
                      Action := caNone;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
          end;
          { Other Projects }
          if frmClose.trvFiles.Items[i].Text = stlIDEs.Strings[2] then begin
            with frmClose.trvFiles.Items[i] do begin
              for k := 0 to Count - 1 do begin
                if frmClose.trvFiles.Checked[Item[k]] then begin
                  eItem := TDocument(OtherProjects.Items[StrToInt(Copy(Item[k].Text, 1, Pos('.', Item[k].Text) - 1)) - 1]);
                  // Process item here
                  if not eItem.Untitled then
                    eItem.Save
                  else begin
                    frmMain.sdSave.FilterIndex := 0;
                    if frmMain.sdSave.Execute then begin
                      eItem.FileName := AddExtension(frmMain.sdSave.FileName, eItem.Highlighter, eItem);
                      eItem.Save;
                    end
                    else begin
                      Action := caNone;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
      Application.Terminate;
    end
    else begin
      Action := caNone;
      exit;
    end;
  end
  else
    Application.Terminate;
    
  for i := 0 to PawnProjects.Count - 1 do begin
    if (not TDocument(PawnProjects.Items[i]).Untitled) then
      eSavedFiles.Add(TDocument(PawnProjects.Items[i]).FileName);
  end;
  for i := 0 to CPPProjects.Count - 1 do begin
    if (not TDocument(CPPProjects.Items[i]).Untitled) then
      eSavedFiles.Add(TDocument(CPPProjects.Items[i]).FileName);
  end;
  for i := 0 to OtherProjects.Count - 1 do begin
    if (not TDocument(OtherProjects.Items[i]).Untitled) then
      eSavedFiles.Add(TDocument(OtherProjects.Items[i]).FileName);
  end;

  eSavedFiles.SaveToFile(ExtractFilePath(ParamStr(0)) + 'config\Cache.cfg');
  eSavedFiles.Free;
  eACList.Free;

  Started := False;
end;

procedure TfrmMain.trvExplorerDblClick(Sender: TObject);
var eFile, eTemp: String;
begin
  if Assigned(trvExplorer.Selected) then begin
    if (Assigned(trvExplorer.Selected.Parent)) and (trvExplorer.Selected.Parent.Text = 'Included') then begin
      eFile := UpdateIncPath(trvExplorer.Selected.Text);
      eTemp := odOpen.FileName;
      odOpen.FileName := eFile;
      mnuOpenClick(nil);
      odOpen.FileName := eTemp;
    end
    else if (trvExplorer.Selected.ImageIndex <> 42) and (trvExplorer.Selected.ImageIndex <> 43) then begin
      sciEditor.GotoLineEnsureVisible(Integer(trvExplorer.Selected.Data));
      sciEditor.SetFocus;
      UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);
    end;
  end;
end;

procedure TfrmMain.tiPawnClick(Sender: TObject);
begin
  trvExplorer.Enabled := True;
  jviCode.Enabled := True;
  mnuGenerators.Visible := True;
  mnuIndenter.Enabled := True;
  mnuCompile.Visible := True;
  mnuRegisterPluginsIniLocal.Enabled := True;
  mnuRegisterPluginsIniWeb.Enabled := True;
  UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);
end;

procedure TfrmMain.tiCPPClick(Sender: TObject);
begin
  trvExplorer.Enabled := False;
  jviCode.Clear;
  AddField('', 'This language is not supported.', '');
  jviCode.Enabled := False;
  mnuGenerators.Visible := False;
  mnuIndenter.Enabled := True;
  mnuCompile.Visible := True;
  mnuRegisterPluginsIniLocal.Enabled := False;
  mnuRegisterPluginsIniWeb.Enabled := False;
end;

procedure TfrmMain.tiOtherClick(Sender: TObject);
begin
  trvExplorer.Enabled := False;
  jviCode.Clear;
  AddField('', 'This language is not supported.', '');
  jviCode.Enabled := False;
  mnuGenerators.Visible := False;
  mnuIndenter.Enabled := False;
  mnuCompile.Visible := False;
  mnuRegisterPluginsIniLocal.Enabled := False;
  mnuRegisterPluginsIniWeb.Enabled := False;
end;

procedure TfrmMain.mnuOpenHelpClick(Sender: TObject);
begin
  if Plugin_ShowHelp(HELP_DEFAULT) then
    ShellExecute(Handle, 'open', 'http://www.amxmodx.org/doc/', nil, nil, SW_SHOW);
end;

procedure TfrmMain.mnuHudmessageClick(Sender: TObject);
  function Dot(eIn: string): string;
  begin
    Result := StringReplace(eIn, ',', '.', [rfReplaceAll]);
    Result := StringReplace(Result, '.00', '.0', [rfReplaceAll]);
  end;

var eStr: string;
begin
  frmHudMsgGenerator.chkXCenter.Checked := False;
  frmHudMsgGenerator.chkYCenter.Checked := False;
  frmHudMsgGenerator.txtXPos.Text := '0,00';
  frmHudMsgGenerator.txtYPos.Text := '0,00';
  frmHudMsgGenerator.lblHudMsg.Left := 0;
  frmHudMsgGenerator.lblHudMsg.Top := 0;
  frmHudMsgGenerator.lblHudMsg.Font.Color := clRed;
  frmHudMsgGenerator.txtText.Text := '';
  frmHudMsgGenerator.CurrColor := clRed;

  if frmHudMsgGenerator.ShowModal = mrOk then begin
    eStr := Format(GetIndents + 'set_hudmessage(%u, %u, %u, %s, %s, 0, 6.0, %s)', [GetRValue(frmHudMsgGenerator.CurrColor), GetGValue(frmHudMsgGenerator.CurrColor), GetBValue(frmHudMsgGenerator.CurrColor), Dot(frmHudMsgGenerator.txtXPos.Text), Dot(frmHudMsgGenerator.txtYPos.Text), Dot(frmHudMsgGenerator.txtTimeToShow.Text)]);
    eStr := eStr + #13#10 + GetIndents + 'show_hudmessage(id, "' + frmHudMsgGenerator.txtText.Text + '")';
    sciEditor.Lines.Insert(sciEditor.GetCurrentLineNumber, eStr);
    mnuModified.Caption := lModified;
    ActiveDoc.Modified := True;
  end;
end;

procedure TfrmMain.mnuDoCompileClick(Sender: TObject);
begin
  if not Plugin_Compile(COMP_DEFAULT, GetCurrLang.Name, ActiveDoc.FileName, True) then
    exit;

  if (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.inl') or (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.inc') or (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.h') then exit;

  if stlIDEs.ItemIndex = 0 then
    DoCompilePawn(COMP_DEFAULT)
  else if (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.htm') or (LowerCase(ExtractFileExt(ActiveDoc.FileName)) = '.html') then begin
    if IEInstalled then
      frmHTMLPreview.Show
    else
      MessageBox(Handle, PChar(lInternetExplorerRequired), PChar(Application.Title), MB_ICONINFORMATION);
  end;
end;

procedure TfrmMain.lstOutputMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var eItem: Integer;
begin
  eItem := lstOutput.ItemAtPos(Point(X, Y), True);
  if eItem = -1 then
    lstOutput.Hint := ''
  else begin
    if (lstOutput.Hint <> lstOutput.Items[eItem]) and (Canvas.TextWidth(lstOutput.Items[eItem]) > lstOutput.Width) then
      lstOutput.Hint := lstOutput.Items[eItem];
  end;
end;

procedure TfrmMain.lstOutputDblClick(Sender: TObject);
var eLine: Integer;
  eStr: string;
begin
  if not Plugin_OutputDblClick(lstOutput.ItemIndex) then
    exit;

  if (lstOutput.ItemIndex <> -1) then begin
    eStr := lstOutput.Items[lstOutput.ItemIndex];
    while Pos(#32, eStr) <> 0 do
      Delete(eStr, 1, 1);
    if IsNumeric(eStr) then begin
      eLine := StrToInt(eStr);
      SetErrorLine(eLine);
    end;
  end;
end;

procedure TfrmMain.lstOutputMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (Plugin_OutputPopup(lstOutput.ItemIndex)) then begin
    lstOutput.ItemIndex := lstOutput.ItemAtPos(Point(X, Y), True);
    mnuCopyMessage.Enabled := lstOutput.ItemAtPos(Point(X, Y), True) <> -1;
    ppmOutput.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TfrmMain.SetErrorLine(eLine: Integer);
begin
  eLine := eLine - 1;
  sciEditor.SetFocus;
  sciEditor.SelLength := 0;
  sciEditor.GotoLineEnsureVisible(eLine);
  sciEditor.SelLength := Length(sciEditor.Lines[eLine]);
  sciEditor.Caret.LineVisible := True;
  sciEditor.Caret.LineBackColor := clMaroon;
  sciEditor.Colors.SelBack := clMaroon;
  eErrorLine := eLine;
end;

procedure TfrmMain.sciEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if not Started then exit;

  if sciEditor.Caret.LineVisible <> frmSettings.chkShowCaret.Checked then
    sciEditor.Caret.LineVisible := frmSettings.chkShowCaret.Checked;
  if sciEditor.Caret.LineBackColor <> frmSettings.CaretBack then begin
    sciEditor.Caret.LineBackColor := frmSettings.CaretBack;
    sciEditor.Colors.SelBack := clHighlight;

    i := sciEditor.SelLength;
    sciEditor.SelLength := 0;
    sciEditor.SelStart := sciEditor.SelStart + i;
  end;
  UpdateCI(frmMain.sciEditor.GetCurrentLineNumber);
end;

procedure TfrmMain.lstOutputEnter(Sender: TObject);
var i: integer;
begin
  if sciEditor.Caret.LineVisible <> frmSettings.chkShowCaret.Checked then
    sciEditor.Caret.LineVisible := frmSettings.chkShowCaret.Checked;
  if sciEditor.Caret.LineBackColor <> frmSettings.CaretBack then begin
    sciEditor.Caret.LineBackColor := frmSettings.CaretBack;
    sciEditor.Colors.SelBack := clHighlight;

    i := sciEditor.SelLength;
    sciEditor.SelLength := 0;
    sciEditor.SelStart := sciEditor.SelStart + i;
  end;
end;

procedure TfrmMain.mnuCompileAndStartHLClick(Sender: TObject);
begin
  if not Plugin_Compile(COMP_STARTHL, GetCurrLang.Name, ActiveDoc.FileName, True) then
    exit;

  if stlIDEs.ItemIndex = 0 then
    DoCompilePawn(COMP_STARTHL);
end;

procedure TfrmMain.mnuCompileAndUploadClick(Sender: TObject);
begin
  if not Plugin_Compile(COMP_UPLOAD, GetCurrLang.Name, ActiveDoc.FileName, True) then
    exit;

  if stlIDEs.ItemIndex = 0 then
    DoCompilePawn(COMP_UPLOAD);
end;

procedure TfrmMain.mnuRegisterPluginsIniLocalClick(Sender: TObject);
  function RemComments(eLine: string): string;
  var a, b: integer;
  begin
    if Length(eLine) > 0 then begin
      b := 0;
      for a := 1 to Length(eLine) - 1 do begin
        if (eLine[a] = ';') or (eLine[a] = '/') then begin
          b := a;
          break;
        end;
      end;

      if (b = 0) and (Pos(' debug', LowerCase(eLine)) <> 0) then
        b := Pos(' debug', LowerCase(eLine));
      if b <> 0 then
        eLine := Trim(Copy(eLine, 1, b - 1));
    end;
    Result := Trim(eLine);
  end;

var eStr: TStringList;
  i: integer;
  eFound: Boolean;
begin
  if ActiveDoc.Untitled then begin
    MessageBox(Handle, PChar(lNoUntitledRegister), PChar(Application.Title), MB_ICONINFORMATION);
    exit;
  end;

  if FileExists(GetAMXXDir(True) + 'configs\plugins.ini') then begin
    eFound := False;

    eStr := TStringList.Create;
    eStr.LoadFromFile(GetAMXXDir(True) + 'configs\plugins.ini');
    for i := 0 to eStr.Count - 1 do begin
      if (Copy(eStr[i], 1, 1) <> ';') and (Copy(eStr[i], 1, 2) <> '//') then begin
        if LowerCase(RemComments(eStr[i])) = LowerCase(ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')) then begin
          eFound := True;
          break;
        end;
      end;
    end;

    if eFound then
      MessageBox(Handle, PChar(lAlreadyRegistered), PChar(Application.Title), MB_ICONINFORMATION)
    else begin
      eStr.Add(ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx'));
      eStr.SaveToFile(GetAMXXDir(True) + 'configs\plugins.ini');
      MessageBox(Handle, PChar(lSuccessfulRegistered), PChar(Application.Title), MB_ICONINFORMATION);
    end;

    eStr.Destroy;
  end;
end;

procedure TfrmMain.mnuRegisterPluginsIniWebClick(Sender: TObject);
  function RemComments(eLine: string): string;
  var a, b: integer;
  begin
    if Length(eLine) > 0 then begin
      b := Length(eLine) +1;
      for a := 1 to Length(eLine) - 1 do begin
        if (eLine[a] = ';') or (eLine[a] = '/') then begin
          b := a;
          break;
        end;
      end;
      eLine := Trim(Copy(eLine, 1, b - 1));
    end;
    Result := eLine;
  end;

var eStr: TStringList;
  i: integer;
  eFound: Boolean;
begin
  if ActiveDoc.Untitled then begin
    MessageBox(Handle, PChar(lNoUntitledRegister), PChar(Application.Title), MB_ICONINFORMATION);
    exit;
  end;

  Screen.Cursor := crHourGlass;
  if not IdFTP.Connected then begin
    if TryConnect <> 0 then
      exit;
  end;

  try
    IdFTP.ChangeDir(frmSettings.txtDefaultDir.Text + 'configs/');
    IdFTP.TransferType := ftASCII;
    IdFTP.Get('plugins.ini', ExtractFilePath(ParamStr(0)) + 'plugins.ini', True);
  except
    Screen.Cursor := crDefault;
    MessageBox(Handle, PChar(lFailedUpdatePluginsIni), PChar(Application.Title), MB_ICONERROR);
    exit;
  end;

  eFound := False;

  eStr := TStringList.Create;
  eStr.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'plugins.ini');
  for i := 0 to eStr.Count - 1 do begin
    if (Copy(eStr[i], 1, 1) <> ';') and (Copy(eStr[i], 1, 2) <> '//') then begin
      if LowerCase(RemComments(eStr[i])) = LowerCase(ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx')) then begin
        eFound := True;
        break;
      end;
    end;
  end;

  if eFound then begin
    Screen.Cursor := crDefault;
    MessageBox(Handle, PChar(lAlreadyRegistered), PChar(Application.Title), MB_ICONINFORMATION);
    IdFTP.Disconnect;
    exit;
  end
  else begin
    eStr.Add(ChangeFileExt(ExtractFileName(ActiveDoc.FileName), '.amxx'));
    eStr.SaveToFile(ExtractFilePath(ParamStr(0)) + 'plugins.ini');
  end;
  eStr.Free;

  try
    IdFTP.Put(ExtractFilePath(ParamStr(0)) + 'plugins.ini', 'plugins.ini');
    IdFTP.Disconnect;
    MessageBox(Handle, PChar(lSuccessfulRegistered), PChar(Application.Title), MB_ICONINFORMATION);
  except
    Screen.Cursor := crDefault;
    MessageBox(Handle, PChar(lFailedUpdatePluginsIni), PChar(Application.Title), MB_ICONERROR);
    exit;
  end;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.mnuMOTDGeneratorClick(Sender: TObject);
var eStr: TStringList;
  i: integer;
begin
  if (GetCurrLang.Name = 'HTML') then begin
    eStr := TStringList.Create;
    eStr.Text := StringReplace(sciEditor.Lines.Text, #9, '', [rfReplaceAll]);
    eStr.Text := StringReplace(sciEditor.Lines.Text, '"', '^"', [rfReplaceAll]);
    for i := 0 to eStr.Count -1 do begin
      if i = eStr.Count -1 then
        eStr[i] := '"' + eStr[i] + '"'
      else
        eStr[i] := '"' + eStr[i] + '^n" +';
    end;
    frmMOTDGen.txtMOTD.Lines.Assign(eStr);
    frmMOTDGen.lblLength.Caption := 'Length: ' + IntToStr(Length(eStr.Text));
    eStr.Destroy;

    frmMOTDGen.ShowModal;
  end
  else begin
    mnuNewHTML.Click;
    if IEInstalled then
      frmHTMLPreview.Show;
    MessageBox(Handle, PChar(lUseMOTDAgain), PChar(Application.Title), MB_ICONINFORMATION);
  end;
end;

procedure TfrmMain.mnuHeaderPawnClick(Sender: TObject);
begin
  if not Plugin_CreateNewFile(NEW_Pawn_HEADER, True) then exit;

  if stlIDEs.ItemIndex <> 0 then
    ActivateProjects(0, False);

  PawnProjects.Activate(PawnProjects.Add('Untitled.inc'), False);
  sciEditor.Lines.Add('/* Header generated by AMXX-Studio*/');
  sciEditor.Lines.Add('');
  Plugin_CreateNewFile(NEW_Pawn_HEADER, False);
end;

procedure TfrmMain.mnuPCloseClick(Sender: TObject);
var Collection: TDocCollection;
begin
  try
    case stlIDEs.ItemIndex of
      0: Collection := PawnProjects; // Pawn
      1: Collection := CPPProjects; // C++
      else Collection := OtherProjects; // Other
    end;

    if tbDocs.Tabs.IndexOf(SelectedTab) <> -1 then
      CloseDocument(TDocument(Collection.Items[tbDocs.Tabs.IndexOf(SelectedTab)]), True, True);
  except
    Application.ProcessMessages; // :o
  end;
end;

procedure TfrmMain.mnuPCloseAllFilesClick(Sender: TObject);
begin
  mnuCloseAllFiles.Click;
end;

procedure TfrmMain.mnuTNewClick(Sender: TObject);
begin
  case stlIDEs.ItemIndex of
    0: mnuNewPlugin.Click;
    1: mnuNewUnit.Click;
    2: mnuNewTextfile.Click;
  end;
end;

procedure TfrmMain.mnuMenuGeneratorClick(Sender: TObject);
begin
  frmMenuGenerator.jplMain.ActivePage := frmMenuGenerator.jspSelectType;
  frmMenuGenerator.ShowModal;
end;

procedure TfrmMain.JvPluginManagerPlugInError(Sender: TObject;
  AError: Exception);
begin
  MessageBox(Handle, PChar(lPluginError + #13 + AError.Message), PChar(Application.Title), MB_ICONERROR);
end;

procedure TfrmMain.mnuNewModuleClick(Sender: TObject);
begin
  Plugin_CreateNewFile(NEW_CPP_MODULE, True);
end;

procedure TfrmMain.mnuNewUnitClick(Sender: TObject);
begin
  Plugin_CreateNewFile(NEW_CPP_UNIT, True);
end;

procedure TfrmMain.mnuNewHeaderCPPClick(Sender: TObject);
begin
  Plugin_CreateNewFile(NEW_CPP_HEADER, True);
end;

procedure TfrmMain.tcToolsActiveTabChange(Sender: TObject;
  ItemIndex: Integer);
begin
  Plugin_VisibleControlChange(CTRL_CODETOOLS_ITEM, ItemIndex = 0);
  Plugin_VisibleControlChange(CTRL_NOTES, ItemIndex = 1);
end;

procedure TfrmMain.sciEditorModified(Sender: TObject; const position,
  modificationType: Integer; text: PAnsiChar; const len, linesAdded, line,
  foldLevelNow, foldLevelPrev: Integer);
begin
  if Started then
    Plugin_Modified(text);
end;

procedure TfrmMain.sciEditorDblClick(Sender: TObject);
begin
  Plugin_EditorClick(True);
end;

procedure TfrmMain.sciCallTipsBeforeShow(Sender: TObject;
  const Position: Integer; ListToDisplay: TStrings;
  var CancelDisplay: Boolean);
var i: integer;
    eFunction: String;
begin
  CancelDisplay := not Plugin_CallTipShow(ListToDisplay.GetText);
  eFunction := GetCurrFunc;
  if (frmSettings.chkAutoHideCT.Checked) and (not CancelDisplay) and (eFunction <> '') then begin
    eFunction := LowerCase(eFunction);

    for i := 0 to eACList.Count -1 do begin
      if eFunction = LowerCase(Trim(TACFunction(eACList.Items[i]).Name)) then begin
        if TACFunction(eACList.Items[i]).Items.Count > GetFunctionPos then begin
          CancelDisplay := True;
          break;
        end;
      end;
    end;
  end;
end;

procedure TfrmMain.sciEditorCallTipClick(Sender: TObject;
  const position: Integer);
var i: integer;
    eFunc: String;
begin
  if not Plugin_CallTipClick(position) then begin
    sciEditor.CallTipCancel;
    exit;
  end;

  eFunc := LowerCase(GetCurrFunc);
  for i := 0 to sciCallTips.ApiStrings.Count -1 do begin
    if Pos(eFunc, LowerCase(sciCallTips.ApiStrings[i])) = 1 then begin
      eFunc := UpdateIncPath(Between(sciCallTips.ApiStrings[i], '-> ', ','));
      if eFunc <> '' then begin
        sciEditor.CallTipCancel;
        PawnProjects.Open(eFunc);
      end;
      
      break;
    end;
  end;
end;

procedure TfrmMain.sciEditorAutoCSelection(Sender: TObject;
  text: PAnsiChar);
begin
  Plugin_AutoCompleteSelect(text);
end;

procedure TfrmMain.OnCustomClick(Sender: TObject);
begin
  Plugin_CustomItemClick((Sender as TTBXCustomItem).Caption);
end;

procedure TfrmMain.pnlCodeInspectorVisibleChanged(Sender: TObject);
begin
  mnuShowCodeInspector.Checked := pnlCodeInspector.Visible;
end;

procedure TfrmMain.pnlCodeExplorerVisibleChanged(Sender: TObject);
begin
  mnuShowCodeExplorer.Checked := pnlCodeExplorer.Visible;
end;

procedure TfrmMain.mnuShowCodeExplorerClick(Sender: TObject);
begin
  pnlCodeExplorer.Visible := mnuShowCodeExplorer.Checked;
end;

procedure TfrmMain.mnuShowCodeInspectorClick(Sender: TObject);
begin
  pnlCodeInspector.Visible := mnuShowCodeInspector.Checked;
end;

procedure TfrmMain.mnuConnectionGenClick(Sender: TObject);
var eIncluded: Integer;
  eRegLine: Integer;
  eIndents: string;
begin
  eRegLine := PluginInitLine;
  if (frmConnGen.ShowModal = mrOk) and (eRegLine <> -1) then begin
    // Add include and add var
    eIncluded := GetLast('#include', True);
    if eIncluded = -1 then
      eIncluded := 0;
    sciEditor.Lines.Insert(eIncluded + 1, 'new sck' + frmConnGen.txtName.Text);
    eRegLine := eRegLine + 1;
    AddIfDoesntExist('sockets');
    // CVar stuff
    eIndents := GetIndents(eRegLine + 1);
    sciEditor.Lines.Insert(eRegLine + 2, eIndents + '/* Init CVars for the socket "' + frmConnGen.txtName.Text + '" */');
    sciEditor.Lines.Insert(eRegLine + 3, eIndents + 'if (cvar_exists("amx_' + frmConnGen.txtName.Text + '_socket")) {');
    sciEditor.Lines.Insert(eRegLine + 4, eIndents + #9 + 'sck' + frmConnGen.txtName.Text + ' = get_cvar_num("amx_' + frmConnGen.txtName.Text + '_socket")');
    sciEditor.Lines.Insert(eRegLine + 5, eIndents + #9 + 'read_' + frmConnGen.txtName.Text + '()');
    sciEditor.Lines.Insert(eRegLine + 6, eIndents + '}');
    sciEditor.Lines.Insert(eRegLine + 7, eIndents + 'else');
    sciEditor.Lines.Insert(eRegLine + 8, eIndents + #9 + 'register_cvar("amx_' + frmConnGen.txtName.Text + '_socket", "0", FCVAR_PROTECTED&FCVAR_UNLOGGED)');
    sciEditor.Lines.Insert(eRegLine + 9, eIndents + '/* End */');
    // Functions
    sciEditor.Lines.Add('');
    sciEditor.Lines.Add('/* Socket ' + frmConnGen.txtName.Text + ' */');
    sciEditor.Lines.Add('');
    sciEditor.Lines.Add('public connect_' + frmConnGen.txtName.Text + '() {');
    sciEditor.Lines.Add(#9 + 'new error = 0');
    sciEditor.Lines.Add(#9 + 'sck' + frmConnGen.txtName.Text + ' = socket_open("' + frmConnGen.txtHost.Text + '", ' + frmConnGen.txtPort.Text + ', ' + frmConnGen.cboProtocol.Text + ', error)');
    sciEditor.Lines.Add(#9 + 'if (sck' + frmConnGen.txtName.Text + ' > 0) {');
    sciEditor.Lines.Add(#9 + #9 + '/* Connect successful */');
    sciEditor.Lines.Add(#9 + #9 + 'read_' + frmConnGen.txtName.Text + '()');
    sciEditor.Lines.Add(#9 + '}');
    sciEditor.Lines.Add(#9 + 'else {');
    sciEditor.Lines.Add(#9 + #9 + 'switch (error) {');
    sciEditor.Lines.Add(#9 + #9 + #9 + 'case 1: { /* Error creating socket */ }');
    sciEditor.Lines.Add(#9 + #9 + #9 + 'case 2: { /* Could not resolve hostname */ }');
    sciEditor.Lines.Add(#9 + #9 + #9 + 'case 3: { /* Could not connect to given host:port */ }');
    sciEditor.Lines.Add(#9 + #9 + '}');
    sciEditor.Lines.Add(#9 + '}');
    sciEditor.Lines.Add('}');
    sciEditor.Lines.Add('');
    sciEditor.Lines.Add('public read_' + frmConnGen.txtName.Text + '() {');
    sciEditor.Lines.Add(#9 + 'if (socket_change(sck' + frmConnGen.txtName.Text + ', 100)) {');
    sciEditor.Lines.Add(#9 + #9 + 'new buf[512], lines[30][100], count = 0');
    sciEditor.Lines.Add(#9 + #9 + 'socket_recv(sck' + frmConnGen.txtName.Text + ', buf, 511)');
    sciEditor.Lines.Add(#9 + #9 + 'count = ExplodeString(lines, 29, 99, buf, 13)');
    sciEditor.Lines.Add(#9 + #9 + 'for(new i=0;i<count;i++) {');
    sciEditor.Lines.Add(#9 + #9 + #9 + '/* Process items here */');
    sciEditor.Lines.Add(#9 + #9 + '}');
    sciEditor.Lines.Add(#9 + '}');
    sciEditor.Lines.Add(#9 + '');
    sciEditor.Lines.Add(#9 + 'if (sck' + frmConnGen.txtName.Text + ' != 0)');
    sciEditor.Lines.Add(#9 + #9 + 'set_task(0.5, "read_' + frmConnGen.txtName.Text + '")');
    sciEditor.Lines.Add(#9 + 'else {');
    sciEditor.Lines.Add(#9 + #9 + 'set_cvar_num("amx_' + frmConnGen.txtName.Text + '_socket", 0)');
    sciEditor.Lines.Add(#9 + #9 + 'disconnect_' + frmConnGen.txtName.Text + '()');
    sciEditor.Lines.Add(#9 + '}');
    sciEditor.Lines.Add('}');
    sciEditor.Lines.Add('');
    sciEditor.Lines.Add('public write_' + frmConnGen.txtName.Text + '(text[512]) {');
    sciEditor.Lines.Add(#9 + 'socket_send(sck' + frmConnGen.txtName.Text + ', text, 511)');
    sciEditor.Lines.Add('}');
    sciEditor.Lines.Add('');
    sciEditor.Lines.Add('public disconnect_' + frmConnGen.txtName.Text + '() {');
    sciEditor.Lines.Add(#9 + '/* Disconnected */');
    sciEditor.Lines.Add('}');

    if GetLine('stock ExplodeString', True, True) = -1 then begin
      sciEditor.Lines.Add('');
      sciEditor.Lines.Add('stock ExplodeString( p_szOutput[][], p_nMax, p_nSize, p_szInput[], p_szDelimiter ) { // Function by xeroblood');
      sciEditor.Lines.Add(#9 + 'new nIdx = 0, l = strlen(p_szInput)');
      sciEditor.Lines.Add(#9 + 'new nLen = (1 + copyc( p_szOutput[nIdx], p_nSize, p_szInput, p_szDelimiter ))');
      sciEditor.Lines.Add(#9 + 'while( (nLen < l) && (++nIdx < p_nMax) )');
      sciEditor.Lines.Add(#9 + #9 + 'nLen += (1 + copyc( p_szOutput[nIdx], p_nSize, p_szInput[nLen], p_szDelimiter ))');
      sciEditor.Lines.Add(#9 + 'return nIdx');
      sciEditor.Lines.Add('}');
    end;
    
    mnuModified.Caption := lModified;
    ActiveDoc.Modified := True;
  end;
end;

procedure TfrmMain.OnCopyData(var Msg: TWMCopyData);
procedure RemoveItemFromTreeView(eCaption: String; eParent: TTreeNode = nil);
var i: integer;
begin
  if Assigned(eParent) then begin
    for i := 0 to eParent.Count -1 do begin
      if eParent.Item[i].Text = eCaption then begin
        eParent.Item[i].Free;
        break;
      end
      else if eParent.Item[i].Count > 0 then
        RemoveItemFromTreeView(eCaption, eParent.Item[i]);
    end;
  end
  else begin
    for i := 0 to frmSettings.trvSettings.Items.Count -1 do begin
      if frmSettings.trvSettings.Items[i].Text = eCaption then begin
        frmSettings.trvSettings.Items[i].Free;
        break;
      end
      else if frmSettings.trvSettings.Items[i].Count > 0 then
        RemoveItemFromTreeView(eCaption, frmSettings.trvSettings.Items[i]);
    end;
  end;
end;

var eData: string;
    eIntData: Integer;
    eMessage: Integer;

    eBMP: TBitmap;
    eTemp: string;
    eItem: TSpTBXItem;
    ePage: TJvStandardPage;
    ePanel: TPanel;
    eStr: TStringList;
    eValues: array of string;
    i: integer;
begin
  eData := string(PChar(Msg.CopyDataStruct.lpData));
  eIntData := Msg.CopyDataStruct.dwData;
  eMessage := Msg.From;
  try
    Msg.Result := 1;
    case eMessage of
      SCM_SHOWPROGRESS: ShowProgress(eIntData = 1);
      SCM_HIDEPROGRESS: HideProgress;
      SCM_UPDATEPROGRESS: begin
          pbLoading.Position := eIntData;
          SetProgressStatus(eData);
        end;
      SCM_LOADCODESNIPPETS: LoadCodeSnippets(eData);
      SCM_CODESNIPPETCLICK: begin
          if Plugin_CodeSnippetClick(eData, GetCat, GetSnippet(GetCat, eData)) then
            sciEditor.SelText := GetSnippet(GetCat, eData);
        end;
      SCM_MIRC_CMD: mIRCDDE('mIRC', 'COMMAND', eData);
      SCM_RELOADINI: ReloadIni;
      SCM_SELECTLANGUAGE: SelectLanguage(eData);
      SCM_LOADFILE: begin
          eTemp := odOpen.FileName;
          odOpen.FileName := eData;
          mnuOpenClick(nil);
          odOpen.FileName := eTemp;
        end;
      SCM_CURRPROJECTS: Msg.Result := stlIDEs.ItemIndex;
      SCM_COMPILE: mnuDoCompile.Click;
      SCM_COMPILE_UPLOAD: mnuCompileAndUpload.Click;
      SCM_COMPILE_STARTHL: mnuCompileAndStartHL.Click;
      SCM_MENU_LOADIMAGE: begin
          eBMP := TBitmap.Create;
          eBMP.LoadFromFile(eData);
          if eIntData = -1 then
            Msg.Result := ilImages.Add(eBMP, nil)
          else
            Msg.Result := ilImages.AddMasked(eBMP, eIntData);
          eBMP.Destroy;
        end;
      SCM_MENU_ADDITEM: begin
          if Pos('->', eData) <> 0 then begin
            eTemp := Copy(eData, 1, Pos('->', eData) - 1);
            eData := Copy(eData, Pos('->', eData) + 2, Length(eData));
          end
          else
            eTemp := '';

          eItem := TSpTBXItem.Create(tbxMenu.Items);
          with eItem do begin
            Caption := eData;
            Images := ilImages;
            ImageIndex := eIntData;
            OnClick := OnCustomClick;
          end;

          if Assigned(GetMenuItem(eTemp)) then
            GetMenuItem(eTemp).Add(eItem)
          else
            tbxMenu.Items.Add(eItem);
        end;
      SCM_MENU_ADDSUBITEM: begin
          if Pos('->', eData) <> 0 then begin
            eTemp := Copy(eData, 1, Pos('->', eData) - 1);
            eData := Copy(eData, Pos('->', eData) + 2, Length(eData));
          end
          else
            eTemp := '';

          eItem := TSpTBXSubMenuItem.Create(tbxMenu.Items);
          with eItem do begin
            Caption := eData;
            Images := ilImages;
            ImageIndex := eIntData;
            OnClick := OnCustomClick;
          end;

          if Assigned(GetMenuItem(eTemp)) then
            GetMenuItem(eTemp).Add(eItem)
          else
            tbxMenu.Items.Add(eItem);
        end;
      SCM_MENU_FAKECLICK: begin
          if Assigned(GetMenuItem(eData)) then
            GetMenuItem(eData).Click
          else
            Msg.Result := 0;
        end;
      SCM_MENU_SHOWITEM: begin
          if Assigned(GetMenuItem(eData)) then
            GetMenuItem(eData).Visible := True
          else
            Msg.Result := 0;
        end;
      SCM_MENU_HIDEITEM: begin
          if Assigned(GetMenuItem(eData)) then
            GetMenuItem(eData).Visible := False
          else
            Msg.Result := 0;
        end;
      SCM_PLUGIN_LOAD: begin
          if eData <> '' then begin
            Msg.Result := 0;
            for i := 0 to frmSettings.lvPlugins.Items.Count - 1 do begin
              if LowerCase(frmSettings.lvPlugins.Items[i].SubItems[0]) = LowerCase(eData) then begin
                LoadPlugin(frmSettings.lvPlugins.Items[i]);
                Msg.Result := 1;
                break;
              end;
            end;
          end
          else
            LoadPlugin(frmSettings.lvPlugins.Items[eIntData]);
        end;
      SCM_PLUGIN_UNLOAD: begin
          if eData <> '' then begin
            Msg.Result := 0;
            for i := 0 to frmSettings.lvPlugins.Items.Count - 1 do begin
              if LowerCase(frmSettings.lvPlugins.Items[i].SubItems[0]) = LowerCase(eData) then begin
                UnloadPlugin(frmSettings.lvPlugins.Items[i]);
                Msg.Result := 1;
                break;
              end;
            end;
          end
          else
            UnloadPlugin(frmSettings.lvPlugins.Items[eIntData]);
        end;
      SCM_SETTINGS_CREATEPAGE: begin
          if Pos('->', eData) <> 0 then begin
            eTemp := Copy(eData, 1, Pos('->', eData) - 1);
            eData := Copy(eData, Pos('->', eData) + 2, Length(eData));
          end
          else
            eTemp := '';

          ePage := TJvStandardPage.Create(frmSettings.jplSettings);
          ePage.Caption := eData;
          TJvPageIndexNode(frmSettings.trvSettings.Items.AddChild(FindSettingsNode(eTemp), eData)).PageIndex := ePage.PageIndex;
          ePanel := TPanel.Create(ePage);
          ePanel.BevelInner := bvNone;
          ePanel.BevelOuter := bvNone;
          ePanel.Align := alClient;
          
          Msg.Result := ePanel.Handle;
        end;
      SCM_SETTINGS_REMOVEPAGE: begin
        Msg.Result := 0;
        for i := 0 to frmSettings.jplSettings.PageCount -1 do begin
          if TJvStandardPage(frmSettings.jplSettings.Pages[i]).Caption = eData then begin
            TJvStandardPage(frmSettings.jplSettings.Pages[i]).Free;
            Msg.Result := 1;
            break;
          end;
        end;
        
        if Msg.Result = 1 then
          RemoveItemFromTreeView(eData);
      end;
      SCM_CODEINSPECTOR_CLEAR: jviCode.Root.Clear;
      SCM_CODEINSPECTOR_ADD: begin
          eStr := TStringList.Create;
          eStr.Text := eData;
          if eStr.Count = 3 then
            AddField(eStr[0], eStr[1], eStr[2])
          else
            Msg.Result := 0;
          eStr.Destroy;
        end;
      SCM_CODEINSPECTOR_ADDCOMBO: begin
          eStr := TStringList.Create;
          eStr.Text := eData;
          if eStr.Count > 3 then begin
            SetLength(eValues, eStr.Count - 2);
            for i := 0 to eStr.Count - 4 do
              eValues[i] := eStr[i + 3];
            AddCombo(eStr[0], eStr[1], eStr[2], eValues);
          end
          else
            Msg.Result := 0;
          eStr.Destroy;
        end;
      SCM_CODEINSPECTOR_SETVALUE: begin
          eStr := TStringList.Create;
          eStr.Text := eData;
          if eStr.Count = 2 then begin
            if Assigned(GetCIItem(eStr[0])) then
              GetCIItem(eStr[0]).DisplayValue := eStr[1]
            else
              Msg.Result := 0;
          end
          else
            Msg.Result := 0;
        end;
      SCM_CODEINSPECTOR_SETNAME: begin
          eStr := TStringList.Create;
          eStr.Text := eData;
          if eStr.Count = 2 then begin
            if Assigned(GetCIItem(eStr[0])) then
              GetCIItem(eStr[0]).DisplayName := eStr[1]
            else
              Msg.Result := 0;
          end
          else
            Msg.Result := 0;
        end;
      SCM_CODEINSPECTOR_GETVALUE: begin
          if Assigned(GetCIItem(eData)) then
            Msg.Result := Integer(PChar(GetCIItem(eData).DisplayValue))
          else
            Msg.Result := Integer(PChar(''));
        end;
      SCM_CODEINSPECTOR_GETNAME: begin
          if Assigned(GetCIItemByValue(eData)) then
            Msg.Result := Integer(PChar(GetCIItemByValue(eData).DisplayName))
          else
            Msg.Result := Integer(PChar(''));
        end;
      SCM_CODEINSPECTOR_COUNT: Msg.Result := jviCode.Root.Count;
      SCM_CODEINSPECTOR_BEGINUPDATE: jviCode.BeginUpdate;
      SCM_CODEINSPECTOR_ENDUPDATE: jviCode.EndUpdate;
      SCM_CODEINSPECTOR_DELETE: begin
          if Assigned(GETCIItem(eData)) then
            jviCode.Root.Delete(GETCIItem(eData))
          else
            Msg.Result := 0;
        end;
      SCM_Pawn_NEWFILE: PawnProjects.Add(eData, '');
      SCM_Pawn_SAVEFILE: begin
          if (eData = '') and (TDocument(PawnProjects.Items[eIntData]).Untitled) then
            Msg.Result := 0
          else
            PawnProjects.Save(eIntData, eData);
        end;
      SCM_Pawn_CLOSEFILE: PawnProjects.Close(eIntData, True);
      SCM_Pawn_ISUNTITLED: begin
          try
            if TDocument(PawnProjects.Items[eIntData]).Untitled then
              Msg.Result := 1
            else
              Msg.Result := 0;
          except
            Msg.Result := -1;
          end;
        end;
      SCM_Pawn_ACTIVATE: begin
          if stlIDEs.ItemIndex <> 0 then
            ActivateProjects(0, eIntData = 1)
          else
            Msg.Result := 0;
        end;
      SCM_Pawn_ACTIVATEDOC: PawnProjects.Activate(eIntData, Pos('r', eData) <> 0, Pos('s', eData) <> 0);
      SCM_Pawn_GETNOTES: begin
          if (stlIDEs.ItemIndex = 0) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(PChar(GetRTFText(rtfNotes)))
          else
            Msg.Result := Integer(PChar(TDocument(PawnProjects.Items[eIntData]).NotesText));
        end;
      SCM_Pawn_SETNOTES: begin
          if (stlIDEs.ItemIndex = 0) and (tbDocs.SelectedTab.Index = eIntData) then
            SetRTFText(rtfNotes, eData)
          else
            TDocument(PawnProjects.Items[eIntData]).NotesText := eData;
        end;  
      SCM_Pawn_GETFILENAME: Msg.Result := Integer(PChar(TDocument(PawnProjects.Items[eIntData]).FileName));
      SCM_Pawn_FILECOUNT: Msg.Result := PawnProjects.Count;
      SCM_Pawn_GETTEXT: begin
          if (stlIDEs.ItemIndex = 0) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(sciEditor.Lines.GetText)
          else
            Msg.Result := Integer(PChar(TDocument(PawnProjects.Items[eIntData]).Code));
        end;
      SCM_CPP_NEWFILE: begin
          if eCPP then
            CPPProjects.Add(eData)
          else
            Msg.Result := 0;
        end;
      SCM_CPP_SAVEFILE: begin
          if eCPP then begin
            if (eData = '') and (TDocument(CPPProjects.Items[eIntData]).Untitled) then
              Msg.Result := 0
            else
              CPPProjects.Save(eIntData, eData);
          end;
        end;
      SCM_CPP_CLOSEFILE: begin
          if eCPP then
            CPPProjects.Close(eIntData, True)
          else
            Msg.Result := 0;
        end;
      SCM_CPP_ISUNTITLED: begin
          try
            if TDocument(CPPProjects.Items[eIntData]).Untitled then
              Msg.Result := 1
            else
              Msg.Result := 0;
          except
            Msg.Result := -1;
          end;
        end;
      SCM_CPP_ACTIVATE: begin
          if (eCPP) and (stlIDEs.ItemIndex <> 1) then
            ActivateProjects(1, eIntData = 1)
          else
            Msg.Result := 0;
        end;      
      SCM_CPP_ACTIVATEDOC: begin
          if eCPP then
            CPPProjects.Activate(eIntData, Pos('r', eData) <> 0, Pos('s', eData) <> 0)
          else
            Msg.Result := 0;
        end;
      SCM_CPP_ACTIVATEIDE: begin
          eCPP := eIntData = 1;
          if eCPP then begin
            mnuNewHeaderCPP.Enabled := True;
            mnuNewModule.Enabled := True;
            mnuNewUnit.Enabled := True;
          end
          else begin
            mnuNewHeaderCPP.Enabled := False;
            mnuNewModule.Enabled := False;
            mnuNewUnit.Enabled := False;
          end;
        end;
      SCM_CPP_GETNOTES: begin
          if (stlIDEs.ItemIndex = 1) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(PChar(GetRTFText(rtfNotes)))
          else
            Msg.Result := Integer(PChar(TDocument(CPPProjects.Items[eIntData]).NotesText));
        end;
      SCM_CPP_SETNOTES: begin
          if (stlIDEs.ItemIndex = 1) and (tbDocs.SelectedTab.Index = eIntData) then
            SetRTFText(rtfNotes, eData)
          else
            TDocument(CPPProjects.Items[eIntData]).NotesText := eData;
        end;
      SCM_CPP_GETFILENAME: Msg.Result := Integer(PChar(TDocument(CPPProjects.Items[eIntData]).FileName));
      SCM_CPP_FILECOUNT: Msg.Result := CPPProjects.Count;
      SCM_CPP_GETTEXT: begin
          if (stlIDEs.ItemIndex = 1) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(sciEditor.Lines.GetText)
          else
            Msg.Result := Integer(PChar(TDocument(CPPProjects.Items[eIntData]).Code));
        end;
      SCM_OTHER_NEWFILE: OtherProjects.Add(eData);
      SCM_OTHER_SAVEFILE: begin
          if (eData = '') and (TDocument(CPPProjects.Items[eIntData]).Untitled) then
            Msg.Result := 0
          else
            OtherProjects.Save(eIntData, eData);
        end;
      SCM_OTHER_CLOSEFILE: OtherProjects.Delete(eIntData);
      SCM_OTHER_ISUNTITLED: begin
          try
            if TDocument(OtherProjects.Items[eIntData]).Untitled then
              Msg.Result := 1
            else
              Msg.Result := 0;
          except
            Msg.Result := -1;
          end;
        end;
      SCM_OTHER_ACTIVATE: begin
          if stlIDEs.ItemIndex <> 2 then
            ActivateProjects(2, eIntData = 1)
          else
            Msg.Result := 0;
        end;
      SCM_OTHER_ACTIVATEDOC: OtherProjects.Activate(eIntData, Pos('r', eData) <> 0, Pos('s', eData) <> 0);
      SCM_OTHER_GETNOTES: begin
          if (stlIDEs.ItemIndex = 2) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(PChar(GetRTFText(rtfNotes)))
          else
            Msg.Result := Integer(PChar(TDocument(OtherProjects.Items[eIntData]).NotesText));
        end;
      SCM_OTHER_SETNOTES: begin
          if (stlIDEs.ItemIndex = 2) and (tbDocs.SelectedTab.Index = eIntData) then
            SetRTFText(rtfNotes, eData)
          else
            TDocument(OtherProjects.Items[eIntData]).NotesText := eData;
        end;
      SCM_OTHER_GETFILENAME: Msg.Result := Integer(PChar(TDocument(OtherProjects.Items[eIntData]).FileName));
      SCM_OTHER_FILECOUNT: Msg.Result := OtherProjects.Count;
      SCM_OTHER_GETTEXT: begin
          if (stlIDEs.ItemIndex = 2) and (tbDocs.SelectedTab.Index = eIntData) then
            Msg.Result := Integer(sciEditor.Lines.GetText)
          else
            Msg.Result := Integer(PChar(TDocument(OtherProjects.Items[eIntData]).Code));
        end;
      SCM_OUTPUT_SHOW: begin
          splOutput.Show;
          lstOutput.Show;
        end;
      SCM_OUTPUT_HIDE: begin
          splOutput.Hide;
          lstOutput.Hide;
        end;
      SCM_OUTPUT_ADD: Msg.Result := lstOutput.Items.Add(eData);
      SCM_OUTPUT_CLEAR: lstOutput.Items.Clear;
      SCM_OUTPUT_DELETE: lstOutput.Items.Delete(eIntData);
      SCM_OUTPUT_GETTEXT: Msg.Result := Integer(lstOutput.Items.GetText);
      SCM_OUTPUT_GETITEM: begin
          try
            Msg.Result := Integer(PChar(lstOutput.Items[eIntData]));
          except
            Msg.Result := Integer(PChar(''));
          end;
        end;
      SCM_OUTPUT_INDEXOF: Msg.Result := lstOutput.Items.IndexOf(eData);
      SCM_ACTIVE_DOCUMENT: Msg.Result := tbDocs.SelectedTab.Index;
      SCM_ACTIVE_PROJECTS: Msg.Result := stlIDEs.ItemIndex;
      SCM_EDITOR_SETTEXT: sciEditor.Lines.SetText(Msg.CopyDataStruct.lpData);
      SCM_EDITOR_GETTEXT: Msg.Result := Integer(sciEditor.Lines.GetText);
      SCM_EDTIOR_SETCALLTIPS: sciCallTips.ApiStrings.Text := eData;
      SCM_EDITOR_SHOWCALLTIP: sciEditor.CallTipShow(eIntData, Msg.CopyDataStruct.lpData);
      SCM_EDITOR_SETAUTOCOMPLETE: sciAutoComplete.AStrings.Text := eData;
      SCM_EDITOR_SHOWAUTOCOMPLETE: sciEditor.AutoCShow(eIntData, Msg.CopyDataStruct.lpData);
      SCM_EDITOR_GETSELSTART: Msg.Result := sciEditor.SelStart;
      SCM_EDITOR_GETSELLENGTH: Msg.Result := sciEditor.SelLength;
      SCM_EDITOR_SETSELSTART: sciEditor.SelStart := eIntData;
      SCM_EDITOR_SETSELLENGH: sciEditor.SelLength := eIntData;
      SCM_REMOVE_MENUITEM: begin
          if Assigned(GetMenuItem(eData)) then begin
            if Assigned(GetMenuItem(eData).Parent) then
              GetMenuItem(eData).Parent.Remove(GetMenuItem(eData))
            else
              tbxMenu.Items.Remove(GetMenuItem(eData));
            Msg.Result := 1;
          end
          else
            Msg.Result := 0;
        end;
      SCM_REMOVE_IMAGE: ilImages.Delete(eIntData);
      SCM_SETTHEME: TBXSetTheme(eData);
      SCM_GETTHEME: Msg.Result := Integer(PChar(TBXCurrentTheme));
    end;
  except
    Msg.Result := 0;
  end;
end;

procedure TfrmMain.OnMessage(var Msg: TMsg; var Handled: Boolean);
begin
  Handled := not Plugin_AppMsg(Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam, Msg.time, Msg.pt);
end;

procedure TfrmMain.OnShortCut(var Msg: TWMKey;
  var Handled: Boolean);
  function TriggerMenuShortcut(eShortcut: TShortcut; Item: TTBCustomItem): Boolean;
  var i: integer;
  begin
    Result := False;
    for i := 0 to Item.Count - 1 do begin
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
  if not Started then
    exit;
  if not Plugin_Shortcut(Msg.CharCode, Msg.KeyData) then begin
    Handled := True;
    exit;
  end;

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

  if GetActiveWindow <> frmMain.Handle then exit;

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
  for i := 0 to tbxMenu.Items.Count - 1 do begin
    if TriggerMenuShortcut(eShortcut, tbxMenu.Items[i]) then
      Handled := True;
  end;
  for i := 0 to tbxToolbar.Items.Count - 1 do begin
    if tbxToolbar.Items[i].ShortCut = eShortcut then begin
      Handled := True;
      tbxToolbar.Items[i].OnClick(Self);
    end;
  end;
  for i := 0 to tbxEdit.Items.Count - 1 do begin
    if tbxEdit.Items[i].ShortCut = eShortcut then begin
      Handled := True;
      tbxEdit.Items[i].OnClick(Self);
    end;
  end;
  for i := 0 to ppmEditor.Items.Count - 1 do begin
    if ppmEditor.Items[i].ShortCut = eShortcut then begin
      Handled := True;
      ppmEditor.Items[i].OnClick(Self);
    end;
  end;
  Application.ProcessMessages;
  if Handled then exit;
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
    for i := 0 to frmMain.sciEditor.KeyCommands.Count - 1 do begin
      if TSciKeyCommand(frmMain.sciEditor.KeyCommands.Items[i]).ShortCut = eShortcut then
        Handled := False;
    end;
  end;
end;


procedure TfrmMain.trvExplorerClick(Sender: TObject);
begin
  if Assigned(trvExplorer.Selected) then begin
    if trvExplorer.Selected.ImageIndex >= 42 then exit;

    UpdateCI(Integer(trvExplorer.Selected.Data));
  end;
end;

procedure TfrmMain.jviCodeItemValueChanged(Sender: TObject;
  Item: TJvCustomInspectorItem);
begin
  if GetCurrLang.Name = 'Pawn' then
    RebuildLine;
end;

procedure TfrmMain.mnuRestoreBackupClick(Sender: TObject);
begin
  if MessageBox(Handle, PChar(lAskRestore), PChar(Application.Title), MB_ICONQUESTION + MB_YESNO) = mrYes then
    sciEditor.Lines.LoadFromFile(ActiveDoc.FileName + '.bak');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  stlIDEs.ItemIndex := 0;
  cboCurrentIDE.Text := stlIDEs.Strings[0];

  sciEditor.StreamClass := TSciMyStream;
  sciEditor.OnCallTipClick := sciEditorCallTipClick; // god why doesn't delphi save this
  eACList := TmxJsCollection.Create(TACFunction);
  eACList.Collectionname := 'Autocomplete_List';
  eACList.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'config\ACList.cfg');
end;

procedure TfrmMain.stlIDEsClick(Sender: TObject);
begin
  if (stlIDEs.Strings[stlIDEs.ItemIndex] = 'C++') and (not eCPP) then begin
    stlIDEs.ItemIndex := CurrProjects;
    cboCurrentIDE.Text := stlIDEs.Strings[CurrProjects];
    MessageBox(Handle, 'C++ IDE is currenty not implemented!', 'Error', MB_ICONERROR);
    exit;
  end;

  cboCurrentIDE.Text := stlIDEs.Strings[stlIDEs.ItemIndex];
  if (not Started) or (frmMain.pnlLoading.Visible) then exit;

  if (lstOutput.Visible) then begin
    if Plugin_VisibleControlChange(CTRL_OUTPUT, False) then begin
      splOutput.Hide;
      lstOutput.Hide;
      Plugin_VisibleControlChange(CTRL_OUTPUT, False);
    end;
  end;

  case stlIDEs.ItemIndex of
    0: begin
        mnuTNew.Caption := 'New Plugin';
      end;
    1: begin
        mnuTNew.Caption := 'New Unit';
        sciCallTips.ApiStrings.Clear;
        sciAutoComplete.AStrings.Clear;
      end;
    2: begin
        mnuTNew.Caption := 'New Textfile';
        sciCallTips.ApiStrings.Clear;
        sciAutoComplete.AStrings.Clear;
      end;
  end;
  ActivateProjects(stlIDEs.ItemIndex, True);

  trvExplorer.Items.Clear;
  FillCodeExplorer(GetCurrLang.Name);
end;

procedure TfrmMain.tbDocsTabSelected(Sender: TObject; Item: TJvTabBarItem);
var Collection: TDocCollection;
begin
  if (not Started) or (pnlLoading.Visible) or (not Assigned(Item)) or (Screen.Cursor <> crDefault) then exit;

  if (lstOutput.Visible) then begin
    if Plugin_VisibleControlChange(CTRL_OUTPUT, False) then begin
      splOutput.Hide;
      lstOutput.Hide;
      Plugin_VisibleControlChange(CTRL_OUTPUT, False);
    end;
  end;

  case stlIDEs.ItemIndex of
    0: Collection := PawnProjects; // Pawn
    1: Collection := CPPProjects; // C++
    else Collection := OtherProjects; // Other
  end;
  Collection.Activate(Item.Index, True);
end;

procedure TfrmMain.tbDocsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then begin
    SelectedTab := tbDocs.TabAt(X, Y);
    if Assigned(SelectedTab) then
      ppmDocuments.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TfrmMain.tbDocsTabClosing(Sender: TObject; Item: TJvTabBarItem;
  var AllowClose: Boolean);
var Collection: TDocCollection;
    i: integer;
begin
  AllowClose := False;
  if (Started) and (Screen.Cursor = crDefault) and (not pnlLoading.Visible) then begin
    Screen.Cursor := crHourGlass;
    case stlIDEs.ItemIndex of
      0: Collection := PawnProjects; // Pawn
      1: Collection := CPPProjects; // C++
      else Collection := OtherProjects; // Other
    end;

    if tbDocs.Tabs.IndexOf(Item) <> -1 then begin
      if CloseDocument(TDocument(Collection.Items[tbDocs.Tabs.IndexOf(Item)]), True, False) then begin
        tbDocs.Tabs.Delete(Item.Index);
        
        for i := 0 to tbDocs.Tabs.Count -1 do
          tbDocs.Tabs[i].Caption := TDocument(Collection.Items[i]).Title;
      end;
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.sciAutoCompleteBeforeShow(Sender: TObject;
  const Position, TypedLen: Integer; ListToDisplay: TStrings;
  var CancelDisplay: Boolean);
function Matchstrings(Source, pattern: string): Boolean;
var pSource: array [0..255] of Char;
    pPattern: array [0..255] of Char;

function MatchPattern(element, pattern: PChar): Boolean;
  function IsPatternWild(pattern: PChar): Boolean;
  begin
    Result := StrScan(pattern, '*') <> nil;
    if not Result then Result := StrScan(pattern, '?') <> nil;
  end;
begin
  if 0 = StrComp(pattern, '*') then
    Result := True
  else if (element^ = Chr(0)) and (pattern^ <> Chr(0)) then
    Result := False
  else if element^ = Chr(0) then
    Result := True
  else
  begin
    case pattern^ of
      '*': if MatchPattern(element, @pattern[1]) then
          Result := True
        else
          Result := MatchPattern(@element[1], pattern);
        '?': Result := MatchPattern(@element[1], @pattern[1]);
      else
        if element^ = pattern^ then
          Result := MatchPattern(@element[1], @pattern[1])
        else
          Result := False;
    end;
  end;
end;

begin
  StrPCopy(pSource, Source);
  StrPCopy(pPattern, pattern);
  Result := MatchPattern(pSource, pPattern);
end;

var eCurrStyle: Integer;
    eFunction: String;
    eCmpList: TStringList;
    i, k, j: integer;
begin
  if not Plugin_AutoCompleteShow(ListToDisplay.GetText) then begin
    CancelDisplay := True;
    exit;
  end;

  if (Started) and (Assigned(GetStyleAt(sciEditor.SelStart))) then begin
    eCurrStyle := GetStyleAt(sciEditor.SelStart).StyleNumber;

    if (ActiveDoc.Highlighter = 'Pawn') then begin
      eFunction := LowerCase(GetCurrFunc);
      if eFunction <> '' then begin
        for i := 0 to eACList.Count -1 do begin
          if eFunction = LowerCase(Trim(TACFunction(eACList.Items[i]).Name)) then begin
            if TACFunction(eACList.Items[i]).Items.Count > GetFunctionPos then begin
              if (Trim(TACFunction(eACList.Items[i]).Items[GetFunctionPos]) <> '') then begin
                if (Pos('*', TACFunction(eACList.Items[i]).Items[GetFunctionPos]) = 0) and (Pos('?', TACFunction(eACList.Items[i]).Items[GetFunctionPos]) = 0) then
                  ListToDisplay.Text := StringReplace(TACFunction(eACList.Items[i]).Items[GetFunctionPos], '; ', #13, [rfReplaceAll])
                else begin
                  eCmpList := TStringList.Create;
                  eCmpList.Text := StringReplace(TACFunction(eACList.Items[i]).Items[GetFunctionPos], '; ', #13, [rfReplaceAll]);
                  for k := eCmpList.Count -1 downto 0 do begin
                    if (Pos('*', eCmpList[k]) <> 0) or (Pos('?', eCmpList[k]) <> 0) then begin
                      for j := 0 to ListToDisplay.Count -1 do begin
                        if Trim(ListToDisplay[j]) <> '' then begin
                          if (LowerCase(ListToDisplay[j][1]) = LowerCase(eCmpList[k][1])) then begin
                            if (MatchStrings(LowerCase(ListToDisplay[j]), LowerCase(eCmpList[k]))) then
                              eCmpList.Add(ListToDisplay[j]);
                          end;
                        end;
                      end;
                      eCmpList.Delete(k);
                    end;
                  end;
                  ListToDisplay.Assign(eCmpList);
                  eCmpList.Free;
                end;
              end;
              break;
            end;
          end;
        end;
      end;
      
      if (eCurrStyle = 11) or (eCurrStyle = 10) or (eCurrStyle = 9) or (eCurrStyle = 8) or (eCurrStyle = 5) or (eCurrStyle = 4) or (eCurrStyle = 0) or (eCurrStyle >= 34) then
        CancelDisplay := False
      else
        CancelDisplay := True;
    end;
  end;
end;

end.

