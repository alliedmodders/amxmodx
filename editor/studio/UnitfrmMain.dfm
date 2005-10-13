object frmMain: TfrmMain
  Left = 257
  Top = 302
  Width = 888
  Height = 641
  Caption = 'AMXX-Studio'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001002020040000000000E80200001600000028000000200000004000
    0000010004000000000000020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF004444
    4444444444444444444444444444444444444444444444444444444444444444
    4444444444444444444444444444444444444444444444444444444444444444
    44446664444444444666644444444444444CCCC644444444CCCC644444444444
    444C88CC6444444CC88C644444444444444C888CC64444CC888C644444444444
    444CC888CC644CC888CC4444444444444444CC888CC6CC888CC4444444444444
    444444C888CCC888CC44444444444444444444CC888C888CC444444444444444
    4444444CC88888CC4444444444444444444444444C888C644444444444444444
    44444444CC888CC644444444444444444444444CC88888CC6444444444444444
    444444CC888C888CC64444444444444444444CC888CCC888CC64444444444444
    4444CC888CC4CC888CC6444444444444444CC888CC444CC888CC644444444444
    444C888CC44444CC888C644444444444444C88CC4444444CC88C644444444444
    444CCCC444444444CCCC44444444444444444444444444444444444444444444
    4444444444444444444444444444444444446664444444446664444444444444
    44444663BB000BB3664444444444444444444466333333366444444444444444
    4444444466666664444444444444444444444444444444444444444444444444
    4444444444444444444444444444444444444444444444444444444444440000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000380000000000000000000000000000000000000000000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbxTopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 880
    Height = 72
    object tbxMenu: TSpTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Menu'
      CloseButton = False
      DefaultDock = tbxTopDock
      DockPos = 0
      Images = ilImages
      TabOrder = 0
      ChevronVertical = True
      object mnuFile: TSpTBXSubmenuItem
        CaptionW = 'File'
        object mnuNew: TSpTBXSubmenuItem
          ImageIndex = 32
          Images = ilImages
          CaptionW = 'New'
          object infoNewAMXX: TSpTBXRightAlignSpacerItem
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
            CaptionW = 'AMXX Scripts / Pawn'
          end
          object mnuEmptyPlugin: TSpTBXItem
            ImageIndex = 0
            Images = ilImages
            OnClick = mnuEmptyPluginClick
            CaptionW = 'Empty Plugin'
          end
          object mnuNewPlugin: TSpTBXItem
            ImageIndex = 18
            Images = ilImages
            OnClick = mnuNewPluginClick
            CaptionW = 'Plugin'
          end
          object mnuHeaderPAWN: TSpTBXItem
            ImageIndex = 33
            Images = ilImages
            OnClick = mnuHeaderPAWNClick
            CaptionW = 'Header'
          end
          object sepNew1: TSpTBXSeparatorItem
            Blank = True
          end
          object infoNewCPP: TSpTBXRightAlignSpacerItem
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
            CaptionW = 'C++'
          end
          object mnuNewModule: TSpTBXItem
            Enabled = False
            ImageIndex = 35
            Images = ilImages
            OnClick = mnuNewModuleClick
            CaptionW = 'Module'
          end
          object mnuNewUnit: TSpTBXItem
            Enabled = False
            ImageIndex = 34
            Images = ilImages
            OnClick = mnuNewUnitClick
            CaptionW = 'Unit'
          end
          object mnuNewHeaderCPP: TSpTBXItem
            Enabled = False
            ImageIndex = 48
            Images = ilImages
            OnClick = mnuNewHeaderCPPClick
            CaptionW = 'Header'
          end
          object sepNew3: TSpTBXSeparatorItem
            Blank = True
          end
          object infoNewOther: TSpTBXRightAlignSpacerItem
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
            CaptionW = 'Other'
          end
          object mnuNewTextfile: TSpTBXItem
            ImageIndex = 34
            Images = ilImages
            OnClick = mnuNewTextfileClick
            CaptionW = 'Textfile'
          end
          object mnuNewHTML: TSpTBXItem
            ImageIndex = 25
            Images = ilImages
            OnClick = mnuNewHTMLClick
            CaptionW = 'HTML Page'
          end
          object mnuNewSQL: TSpTBXItem
            ImageIndex = 44
            Images = ilImages
            OnClick = mnuNewSQLClick
            CaptionW = 'SQL Database'
          end
          object mnuNewXML: TSpTBXItem
            ImageIndex = 34
            OnClick = mnuNewXMLClick
            CaptionW = 'XML File'
          end
        end
        object sepFile1: TSpTBXSeparatorItem
        end
        object mnuOpen: TSpTBXItem
          ImageIndex = 3
          Images = ilImages
          ShortCut = 16463
          OnClick = mnuOpenClick
          CaptionW = 'Open...'
        end
        object sepFile2: TSpTBXSeparatorItem
        end
        object mnuSave: TSpTBXItem
          ImageIndex = 1
          Images = ilImages
          ShortCut = 16467
          OnClick = mnuSaveClick
          CaptionW = 'Save'
        end
        object mnuSaveAs: TSpTBXItem
          OnClick = mnuSaveAsClick
          CaptionW = 'Save as...'
        end
        object mnuSaveAllFiles: TSpTBXItem
          ImageIndex = 2
          Images = ilImages
          ShortCut = 24659
          OnClick = mnuSaveAllFilesClick
          CaptionW = 'Save all files'
        end
        object sepFile3: TSpTBXSeparatorItem
        end
        object mnuClose: TSpTBXItem
          ImageIndex = 46
          ShortCut = 16430
          OnClick = mnuCloseClick
          CaptionW = 'Close'
        end
        object mnuCloseAllFiles: TSpTBXItem
          OnClick = mnuCloseAllFilesClick
          CaptionW = 'Close all files'
        end
        object sepFile4: TSpTBXSeparatorItem
        end
        object mnuPrint: TSpTBXItem
          ImageIndex = 4
          Images = ilImages
          ShortCut = 16464
          OnClick = mnuPrintClick
          CaptionW = 'Print'
        end
        object sepFile5: TSpTBXSeparatorItem
        end
        object mnuExit: TSpTBXItem
          ImageIndex = 45
          Images = ilImages
          ShortCut = 32883
          OnClick = mnuExitClick
          CaptionW = 'Exit'
        end
      end
      object mnuEdit: TSpTBXSubmenuItem
        CaptionW = 'Edit'
        object mnuUndo: TSpTBXItem
          ImageIndex = 6
          Images = ilImages
          ShortCut = 16474
          OnClick = mnuUndoClick
          CaptionW = 'Undo'
        end
        object mnuRedo: TSpTBXItem
          ImageIndex = 7
          Images = ilImages
          ShortCut = 24666
          OnClick = mnuRedoClick
          CaptionW = 'Redo'
        end
        object sepEdit1: TSpTBXSeparatorItem
        end
        object mnuCut: TSpTBXItem
          ImageIndex = 8
          Images = ilImages
          ShortCut = 16472
          OnClick = mnuCutClick
          CaptionW = 'Cut'
        end
        object mnuCopy: TSpTBXItem
          ImageIndex = 9
          Images = ilImages
          ShortCut = 16451
          OnClick = mnuCopyClick
          CaptionW = 'Copy'
        end
        object mnuPaste: TSpTBXItem
          ImageIndex = 10
          Images = ilImages
          ShortCut = 16470
          OnClick = mnuPasteClick
          CaptionW = 'Paste'
        end
        object sepEdit2: TSpTBXSeparatorItem
        end
        object mnuSelectAll: TSpTBXItem
          ImageIndex = 11
          Images = ilImages
          ShortCut = 16449
          OnClick = mnuSelectAllClick
          CaptionW = 'Select all'
        end
      end
      object mnuSearch: TSpTBXSubmenuItem
        CaptionW = 'Search'
        object mnuSearchDialog: TSpTBXItem
          ImageIndex = 13
          Images = ilImages
          ShortCut = 16454
          OnClick = mnuSearchDialogClick
          CaptionW = 'Search'
        end
        object mnuSearchAgain: TSpTBXItem
          ShortCut = 114
          OnClick = mnuSearchAgainClick
          CaptionW = 'Search again'
        end
        object sepSearch1: TSpTBXSeparatorItem
        end
        object mnuReplace: TSpTBXItem
          ImageIndex = 39
          Images = ilImages
          ShortCut = 16466
          OnClick = mnuReplaceClick
          CaptionW = 'Replace'
        end
        object sepSearch2: TSpTBXSeparatorItem
        end
        object mnuGoToLine: TSpTBXItem
          ImageIndex = 14
          Images = ilImages
          ShortCut = 16455
          OnClick = mnuGoToLineClick
          CaptionW = 'Go to line...'
        end
      end
      object mnuView: TSpTBXSubmenuItem
        CaptionW = 'View'
        object mnuChangeTheme: TSpTBXSubmenuItem
          CaptionW = 'Change Theme'
          object mnuThemes: TSpTBXThemeGroupItem
            OnClick = mnuThemesClick
          end
        end
        object mnuSelectHighlighter: TSpTBXSubmenuItem
          ImageIndex = 15
          Images = ilImages
          CaptionW = 'Set Highlighter'
          object mnuHPAWN: TSpTBXItem
            AutoCheck = True
            Checked = True
            OnClick = mnuHXMLClick
            CaptionW = 'Pawn'
          end
          object mnuHCPP: TSpTBXItem
            AutoCheck = True
            OnClick = mnuHXMLClick
            CaptionW = 'C++'
          end
          object mnuHHTML: TSpTBXItem
            AutoCheck = True
            OnClick = mnuHXMLClick
            CaptionW = 'HTML'
          end
          object mnuHSQL: TSpTBXItem
            AutoCheck = True
            OnClick = mnuHXMLClick
            CaptionW = 'SQL'
          end
          object mnuHXML: TSpTBXItem
            AutoCheck = True
            OnClick = mnuHXMLClick
            CaptionW = 'XML'
          end
          object sepHighlighter: TSpTBXSeparatorItem
          end
          object mnuHNone: TSpTBXItem
            AutoCheck = True
            OnClick = mnuHNoneClick
            CaptionW = 'None'
          end
        end
        object sepView1: TSpTBXSeparatorItem
        end
        object mnuFoldAll: TSpTBXItem
          OnClick = mnuFoldAllClick
          CaptionW = 'Fold all'
        end
        object sepView2: TSpTBXSeparatorItem
        end
        object mnuShowFileTB: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowFileTBClick
          CaptionW = 'Show File-Toolbar'
        end
        object mnuShowEditTB: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowEditTBClick
          CaptionW = 'Show Edit-Toolbar'
        end
        object mnuShowCodeSnippets: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeSnippetsClick
          CaptionW = 'Show Code-Snippets'
        end
        object mnuShowCodeToolsWindow: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeToolsClick
          CaptionW = 'Show Code-Explorer and Notes'
        end
        object sepView3: TSpTBXSeparatorItem
        end
        object mnuShowCodeExplorer: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeExplorerClick
          CaptionW = 'Show Code-Explorer'
        end
        object mnuShowCodeInspector: TSpTBXItem
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeInspectorClick
          CaptionW = 'Show Code-Inspector'
        end
      end
      object mnuCompile: TSpTBXSubmenuItem
        CaptionW = 'Compile'
        object mnuDoCompile: TSpTBXItem
          ImageIndex = 22
          Images = ilImages
          ShortCut = 120
          OnClick = mnuDoCompileClick
          CaptionW = 'Compile'
        end
        object sepCompile1: TSpTBXSeparatorItem
        end
        object mnuCompileAndStartHL: TSpTBXItem
          ImageIndex = 22
          Images = ilImages
          OnClick = mnuCompileAndStartHLClick
          CaptionW = 'Compile and start Half-Life'
        end
        object mnuCompileAndUpload: TSpTBXItem
          ImageIndex = 37
          Images = ilImages
          OnClick = mnuCompileAndUploadClick
          CaptionW = 'Compile and upload'
        end
        object sepCompile2: TSpTBXSeparatorItem
        end
        object mnuRegisterPluginsIniLocal: TSpTBXItem
          OnClick = mnuRegisterPluginsIniLocalClick
          CaptionW = 'Register in plugins.ini (local)'
        end
        object mnuRegisterPluginsIniWeb: TSpTBXItem
          OnClick = mnuRegisterPluginsIniWebClick
          CaptionW = 'Register in plugins.ini (FTP)'
        end
      end
      object mnuTools: TSpTBXSubmenuItem
        CaptionW = 'Tools'
        object mnuIndenter: TSpTBXItem
          ImageIndex = 16
          Images = ilImages
          ShortCut = 16457
          OnClick = mnuIndenterClick
          CaptionW = 'Indenter'
        end
        object mnuUnindenter: TSpTBXItem
          ImageIndex = 17
          Images = ilImages
          ShortCut = 24649
          OnClick = mnuUnindenterClick
          CaptionW = 'Unindenter'
        end
        object sepTools1: TSpTBXSeparatorItem
        end
        object mnuSocketTerminal: TSpTBXItem
          ImageIndex = 40
          Images = ilImages
          OnClick = mnuSocketTerminalClick
          CaptionW = 'Socket Terminal'
        end
        object sepTools2: TSpTBXSeparatorItem
        end
        object mnuPluginsIniEditor: TSpTBXItem
          ImageIndex = 19
          Images = ilImages
          OnClick = mnuPluginsIniEditorClick
          CaptionW = 'Plugins.ini Editor'
        end
        object mnuPaster: TSpTBXItem
          ImageIndex = 10
          Images = ilImages
          OnClick = mnuPasterClick
          CaptionW = 'IRC Paster'
        end
        object mnuRestoreBackup: TSpTBXItem
          Enabled = False
          ImageIndex = 46
          Images = ilImages
          OnClick = mnuRestoreBackupClick
          CaptionW = 'Restore from backup'
        end
        object sepTools3: TSpTBXSeparatorItem
        end
        object mnuSettings: TSpTBXItem
          ImageIndex = 41
          Images = ilImages
          ShortCut = 123
          OnClick = mnuSettingsClick
          CaptionW = 'Settings'
        end
      end
      object mnuGenerators: TSpTBXSubmenuItem
        CaptionW = 'Generators'
        object mnuMenuGenerator: TSpTBXItem
          ImageIndex = 20
          Images = ilImages
          OnClick = mnuMenuGeneratorClick
          CaptionW = 'Menu Generator'
        end
        object sepGenerators1: TSpTBXSeparatorItem
        end
        object mnuHudmessage: TSpTBXItem
          OnClick = mnuHudmessageClick
          CaptionW = 'Hudmessage Generator'
        end
        object mnuMOTDGenerator: TSpTBXItem
          ImageIndex = 36
          Images = ilImages
          OnClick = mnuMOTDGeneratorClick
          CaptionW = 'MOTD Generator'
        end
        object mnuConnectionGen: TSpTBXItem
          ImageIndex = 18
          Images = ilImages
          OnClick = mnuConnectionGenClick
          CaptionW = 'Create connection'
        end
      end
      object mnuHelp: TSpTBXSubmenuItem
        CaptionW = 'Help'
        object mnuOpenHelp: TSpTBXItem
          ImageIndex = 21
          Images = ilImages
          ShortCut = 112
          OnClick = mnuOpenHelpClick
          CaptionW = 'Open help'
        end
        object sepHelp1: TSpTBXSeparatorItem
        end
        object mnuSearchForums: TSpTBXItem
          OnClick = mnuSearchForumsClick
          CaptionW = 'Search on AMX Mod X forums'
        end
        object mnuOpenScriptingForum: TSpTBXItem
          OnClick = mnuOpenScriptingForumClick
          CaptionW = 'Open AMXX Scripting Forum'
        end
        object sepHelp2: TSpTBXSeparatorItem
        end
        object mnuInfo: TSpTBXItem
          OnClick = mnuInfoClick
          CaptionW = 'About AMXX-Studio...'
        end
      end
    end
    object tbxToolbar: TTBXToolbar
      Left = 0
      Top = 23
      Caption = 'File Toolbar'
      DefaultDock = tbxTopDock
      DockPos = -10
      DockRow = 1
      TabOrder = 1
      OnVisibleChanged = tbxToolbarVisibleChanged
      object mnuTNew: TSpTBXItem
        ImageIndex = 0
        Images = ilImages
        ShortCut = 16462
        OnClick = mnuTNewClick
        CaptionW = 'New'
      end
      object mnuTOpen: TSpTBXItem
        ImageIndex = 3
        Images = ilImages
        OnClick = mnuTOpenClick
        CaptionW = 'Open'
      end
      object mnuTSave: TSpTBXItem
        ImageIndex = 1
        Images = ilImages
        OnClick = mnuTSaveClick
        CaptionW = 'Save'
      end
      object sepToolbar1: TSpTBXSeparatorItem
      end
      object mnuTSearch: TSpTBXItem
        ImageIndex = 13
        Images = ilImages
        OnClick = mnuTSearchClick
        CaptionW = 'Search'
      end
      object sepToolbar2: TSpTBXSeparatorItem
      end
      object mnuTCompile: TSpTBXItem
        ImageIndex = 22
        Images = ilImages
        OnClick = mnuTCompileClick
        CaptionW = 'Compile'
      end
    end
    object tbxCodeSnippets: TSpTBXToolbar
      Left = 0
      Top = 49
      Caption = 'Code-Snippets'
      DefaultDock = tbxTopDock
      DockPos = -3
      DockRow = 3
      TabOrder = 3
      OnVisibleChanged = tbxCodeSnippetsVisibleChanged
      ChevronVertical = True
      object mnuCodeSnippets: TSpTBXRightAlignSpacerItem
        CaptionW = 'Code-Snippets:'
      end
      object mnuPawn: TSpTBXItem
        AutoCheck = True
        Checked = True
        OnClick = OnCodeSnippetSelect
        CaptionW = 'Pawn'
      end
      object mnuCPP: TSpTBXItem
        AutoCheck = True
        OnClick = OnCodeSnippetSelect
        CaptionW = 'C++'
      end
      object mnuHTML: TSpTBXItem
        OnClick = OnCodeSnippetSelect
        CaptionW = 'HTML'
      end
      object mnuOther: TSpTBXItem
        AutoCheck = True
        OnClick = OnCodeSnippetSelect
        CaptionW = 'Other'
      end
      object sepCodeSnippets: TSpTBXSeparatorItem
      end
    end
    object tbxEdit: TSpTBXToolbar
      Left = 137
      Top = 23
      Caption = 'Edit Toolbar'
      DefaultDock = tbxTopDock
      DockPos = 136
      DockRow = 1
      TabOrder = 2
      OnVisibleChanged = tbxEditVisibleChanged
      ChevronVertical = True
      object mnuTUndo: TSpTBXItem
        ImageIndex = 23
        Images = ilImages
        OnClick = mnuTUndoClick
        CaptionW = 'Undo'
      end
      object mnuTRedo: TSpTBXItem
        ImageIndex = 24
        Images = ilImages
        OnClick = mnuTRedoClick
        CaptionW = 'Redo'
      end
      object sepTEdit1: TSpTBXSeparatorItem
      end
      object mnuTCopy: TSpTBXItem
        ImageIndex = 9
        Images = ilImages
        OnClick = mnuTCopyClick
        CaptionW = 'Copy'
      end
      object mnuTCut: TSpTBXItem
        ImageIndex = 8
        Images = ilImages
        OnClick = mnuTCutClick
        CaptionW = 'Cut'
      end
      object mnuTPaste: TSpTBXItem
        ImageIndex = 10
        Images = ilImages
        OnClick = mnuTPasteClick
        CaptionW = 'Paste'
      end
      object sepTEdit2: TSpTBXSeparatorItem
      end
      object mnuTSelectAll: TSpTBXItem
        ImageIndex = 11
        Images = ilImages
        OnClick = mnuTSelectAllClick
        CaptionW = 'Select all'
      end
    end
  end
  object tsMain: TSpTBXTabSet
    Left = 0
    Top = 72
    Width = 880
    Height = 519
    Align = alClient
    ActiveTabIndex = 0
    TabPosition = ttpBottom
    ThemeType = tttTBX
    OnActiveTabChange = tsMainActiveTabChange
    HiddenItems = <>
    object tiPAWN: TSpTBXTabItem
      Checked = True
      OnClick = tiPAWNClick
      TabPosition = ttpBottom
      ThemeType = tttTBX
      CaptionW = 'Pawn Projects'
    end
    object tiCPP: TSpTBXTabItem
      Enabled = False
      OnClick = tiCPPClick
      TabPosition = ttpBottom
      ThemeType = tttTBX
      CaptionW = 'C++ Projects'
    end
    object tiOther: TSpTBXTabItem
      OnClick = tiOtherClick
      TabPosition = ttpBottom
      ThemeType = tttTBX
      CaptionW = 'Other'
    end
    object tsDocuments: TSpTBXTabSet
      Left = 0
      Top = 0
      Width = 880
      Height = 496
      Align = alClient
      ActiveTabIndex = 0
      ThemeType = tttTBX
      OnActiveTabChange = tsDocumentsActiveTabChange
      HiddenItems = <>
      object tiDocument1: TSpTBXTabItem
        Checked = True
        OnSelect = OnTabSelect
        ThemeType = tttTBX
        CaptionW = '< 1 Untitled.sma >'
      end
      object spcLeft1: TImage
        Left = 0
        Top = 23
        Width = 3
        Height = 473
        Align = alLeft
      end
      object spcRight1: TImage
        Left = 877
        Top = 23
        Width = 3
        Height = 473
        Align = alRight
      end
      object splRight: TSplitter
        Left = 669
        Top = 23
        Height = 473
        Align = alRight
      end
      object pnlParent: TPanel
        Left = 3
        Top = 23
        Width = 666
        Height = 473
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object splOutput: TSplitter
          Left = 0
          Top = 388
          Width = 666
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Visible = False
        end
        object sciEditor: TScintilla
          Left = 0
          Top = 0
          Width = 666
          Height = 388
          Color = clWhite
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'Courier'
          Font.Style = []
          PopupMenu = ppmEditor
          Align = alClient
          OnModified = sciEditorModified
          OnDblClick = sciEditorDblClick
          OnKeyUp = sciEditorKeyUp
          OnKeyDown = sciEditorKeyDown
          OnKeyPress = sciEditorKeyPress
          OnClick = sciEditorClick
          OnMouseDown = sciEditorMouseDown
          Lines.Strings = (
            '/* Plugin generated by AMXX-Studio */'
            ''
            '#include <amxmodx>'
            '#include <amxmisc>'
            ''
            '#define PLUGIN "New Plugin"'
            '#define VERSION "1.0"'
            '#define AUTHOR "Author"'
            ''
            ''
            'public plugin_init() {'
            #9'register_plugin(PLUGIN, VERSION, AUTHOR)'
            #9
            #9'// Add your code here...'
            '}')
          EOLStyle = eolCRLF
          Indentation = [KeepIndent, TabIndents]
          IndentWidth = 0
          MarginLeft = 1
          MarginRight = 1
          Caret.ForeColor = clNone
          Caret.LineBackColor = 16770790
          Caret.LineVisible = True
          Caret.Width = 1
          Caret.Period = 1024
          DivOptions.ViewWSpace = sciWsInvisible
          DivOptions.UsePalette = False
          DivOptions.OverType = False
          DivOptions.ViewEOL = False
          DivOptions.EndAtLastLine = True
          DivOptions.ScrollBarH = True
          DivOptions.ScrollBarV = True
          ActiveHotSpot.BackColor = 16754856
          ActiveHotSpot.ForeColor = clBlue
          ActiveHotSpot.Underlined = True
          ActiveHotSpot.SingleLine = False
          Colors.SelFore = clHighlightText
          Colors.SelBack = clHighlight
          Colors.MarkerFore = clWhite
          Colors.MarkerBack = clBtnShadow
          Colors.FoldHi = clWhite
          Colors.FoldLo = clBtnFace
          Colors.BookMarkBack = clGray
          Colors.BookMarkFore = clWhite
          Gutter0.Width = 0
          Gutter0.MarginType = gutLineNumber
          Gutter1.Width = 40
          Gutter1.MarginType = gutLineNumber
          Gutter2.Width = 14
          Gutter2.MarginType = gutSymbol
          WordWrapVisualFlags = []
          WordWrapVisualFlagsLocation = []
          LayoutCache = sciCacheCaret
          HideSelect = False
          WordWrap = sciNoWrap
          EdgeMode = sciEdgeLine
          EdgeColumn = 100
          EdgeColor = clSilver
          WordChars = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
          ControlCharSymbol = #0
          BraceHilite = True
          Folding = [foldFold, foldCompact, foldComment, foldPreprocessor, foldCommentPython, foldAtElse, foldHTML, foldHTMLPreProcessor]
          FoldMarkerType = sciMarkBox
          LanguageManager.LanguageList = <
            item
              Name = 'null'
              Lexer = 'null'
              Styles = <
                item
                  FontName = 'Arial'
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clYellow
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlack
                  BackColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end>
              Keywords = <>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '/*'
              CommentBoxEnd = '*/'
              CommentBoxMiddle = '*'
              CommentBlock = '//'
              CommentAtLineStart = True
              CommentStreamStart = '/*'
              CommentStreamEnd = '*/'
              NumStyleBits = 5
            end
            item
              Name = 'XML'
              Lexer = 'xml'
              Styles = <
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clYellow
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlack
                  BackColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Default'
                  StyleNumber = 0
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 13684736
                  CharCase = CASE_MIXED
                  Name = 'Tags'
                  StyleNumber = 1
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 13684736
                  CharCase = CASE_MIXED
                  Name = 'Unknown Tags'
                  StyleNumber = 2
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12624032
                  CharCase = CASE_MIXED
                  Name = 'Attributes'
                  StyleNumber = 3
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12624032
                  CharCase = CASE_MIXED
                  Name = 'Unknown Attributes'
                  StyleNumber = 4
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  Name = 'Numbers'
                  StyleNumber = 5
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'Double quoted strings'
                  StyleNumber = 6
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'Single quoted strings'
                  StyleNumber = 7
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'Other inside tag'
                  StyleNumber = 8
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'Comment'
                  StyleNumber = 9
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  CharCase = CASE_MIXED
                  Name = 'Entities'
                  StyleNumber = 10
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'XML short tag end'
                  StyleNumber = 11
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'XML identifier start'
                  StyleNumber = 12
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'XML identifier end'
                  StyleNumber = 13
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clMaroon
                  BackColor = 15790335
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'CDATA'
                  StyleNumber = 17
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 160
                  CharCase = CASE_MIXED
                  Name = 'XML Question'
                  StyleNumber = 18
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clFuchsia
                  CharCase = CASE_MIXED
                  Name = 'Unquoted values'
                  StyleNumber = 19
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 13684736
                  CharCase = CASE_MIXED
                  Name = 'SGML tags <! ... >'
                  StyleNumber = 21
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 10526720
                  CharCase = CASE_MIXED
                  Name = 'SGML command'
                  StyleNumber = 22
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 15793935
                  CharCase = CASE_MIXED
                  Name = 'SGML 1st param'
                  StyleNumber = 23
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'SGML double string'
                  StyleNumber = 24
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'SGML single string'
                  StyleNumber = 25
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'SGML error'
                  StyleNumber = 26
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 16737843
                  CharCase = CASE_MIXED
                  Name = 'SGML special'
                  StyleNumber = 27
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  CharCase = CASE_MIXED
                  Name = 'SGML entity'
                  StyleNumber = 28
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'SGML comment'
                  StyleNumber = 29
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 6684672
                  BackColor = 14732492
                  CharCase = CASE_MIXED
                  Name = 'SGML block'
                  StyleNumber = 31
                end>
              Keywords = <
                item
                  KeywordListNumber = 0
                  Name = 'Keywords'
                end
                item
                  KeywordListNumber = 5
                  Name = 'SGML Keywords'
                  Keywords.Strings = (
                    'ELEMENT'
                    'DOCTYPE'
                    'ATTLIST'
                    'ENTITY'
                    'NOTATION')
                end>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '<!--'
              CommentBoxEnd = '-->'
              CommentBoxMiddle = ' '
              CommentBlock = '//'
              CommentAtLineStart = True
              CommentStreamStart = '<!--'
              CommentStreamEnd = '-->'
              NumStyleBits = 7
            end
            item
              Name = 'HTML'
              Lexer = 'hypertext'
              Styles = <
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clBlue
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlack
                  BackColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Text'
                  StyleNumber = 0
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clBlack
                  CharCase = CASE_MIXED
                  Name = 'Tags'
                  StyleNumber = 1
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  Name = 'Unknown Tags'
                  StyleNumber = 2
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12624032
                  CharCase = CASE_MIXED
                  Name = 'Attributes'
                  StyleNumber = 3
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Unknown Attributes'
                  StyleNumber = 4
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlue
                  CharCase = CASE_MIXED
                  Name = 'Numbers'
                  StyleNumber = 5
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 39338
                  CharCase = CASE_MIXED
                  Name = 'Double quoted strings'
                  StyleNumber = 6
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'Single quoted strings'
                  StyleNumber = 7
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Other inside tag'
                  StyleNumber = 8
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 33023
                  CharCase = CASE_MIXED
                  Name = 'Comment'
                  StyleNumber = 9
                end
                item
                  FontName = 'Times New Roman'
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 10526880
                  CharCase = CASE_MIXED
                  Name = 'Entities'
                  StyleNumber = 10
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12632064
                  CharCase = CASE_MIXED
                  Name = 'XML short tag end'
                  StyleNumber = 11
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'XML identifier start'
                  StyleNumber = 12
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 10485920
                  CharCase = CASE_MIXED
                  Name = 'XML identifier end'
                  StyleNumber = 13
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 657920
                  CharCase = CASE_MIXED
                  Name = 'SCRIPT'
                  StyleNumber = 14
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clYellow
                  CharCase = CASE_MIXED
                  Name = 'ASP <% ... %>'
                  StyleNumber = 15
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clYellow
                  CharCase = CASE_MIXED
                  Name = 'ASP <% ... %>'
                  StyleNumber = 16
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 57343
                  CharCase = CASE_MIXED
                  Name = 'CDATA'
                  StyleNumber = 17
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 5343743
                  CharCase = CASE_MIXED
                  Name = 'PHP'
                  StyleNumber = 18
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clFuchsia
                  CharCase = CASE_MIXED
                  Name = 'Unquoted values'
                  StyleNumber = 19
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'XC Comment'
                  StyleNumber = 20
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 13684736
                  CharCase = CASE_MIXED
                  Name = 'SGML tags <! ... >'
                  StyleNumber = 21
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 10526720
                  CharCase = CASE_MIXED
                  Name = 'SGML command'
                  StyleNumber = 22
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 15793935
                  CharCase = CASE_MIXED
                  Name = 'SGML 1st param'
                  StyleNumber = 23
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'SGML double string'
                  StyleNumber = 24
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'SGML single string'
                  StyleNumber = 25
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'SGML error'
                  StyleNumber = 26
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 16737843
                  CharCase = CASE_MIXED
                  Name = 'SGML special'
                  StyleNumber = 27
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'SGML entity'
                  StyleNumber = 28
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'SGML comment'
                  StyleNumber = 29
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlue
                  CharCase = CASE_MIXED
                  Name = 'SGML block'
                  StyleNumber = 31
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 32639
                  CharCase = CASE_MIXED
                  Name = 'JS Start'
                  StyleNumber = 40
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'JS Default'
                  StyleNumber = 41
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'JS Comment'
                  StyleNumber = 42
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'JS Line Comment'
                  StyleNumber = 43
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'JS Doc Comment'
                  StyleNumber = 44
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  Name = 'JS Number'
                  StyleNumber = 45
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 13421568
                  CharCase = CASE_MIXED
                  Name = 'JS Word'
                  StyleNumber = 46
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  Name = 'JS Keyword'
                  StyleNumber = 47
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'JS Double quoted string'
                  StyleNumber = 48
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'JS Single quoted string'
                  StyleNumber = 49
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'JS Symbols'
                  StyleNumber = 50
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clWhite
                  BackColor = 2105376
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'JS EOL'
                  StyleNumber = 51
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 16724672
                  CharCase = CASE_MIXED
                  Name = 'JS Regex'
                  StyleNumber = 52
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 32639
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Start'
                  StyleNumber = 55
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP JS Default'
                  StyleNumber = 56
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP JS Comment'
                  StyleNumber = 57
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Line Comment'
                  StyleNumber = 58
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP JS Doc Comment'
                  StyleNumber = 59
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Number'
                  StyleNumber = 60
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 14737632
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Word'
                  StyleNumber = 61
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Keyword'
                  StyleNumber = 62
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Double quoted string'
                  StyleNumber = 63
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Single quoted string'
                  StyleNumber = 64
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Symbols'
                  StyleNumber = 65
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clWhite
                  BackColor = 2105376
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP JS EOL'
                  StyleNumber = 66
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 16724672
                  CharCase = CASE_MIXED
                  Name = 'ASP JS Regex'
                  StyleNumber = 67
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS Default'
                  StyleNumber = 71
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS Comment'
                  StyleNumber = 72
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS Number'
                  StyleNumber = 73
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS KeyWord'
                  StyleNumber = 74
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS String'
                  StyleNumber = 75
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS Identifier'
                  StyleNumber = 76
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clWhite
                  BackColor = 2105376
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'VBS Unterminated string'
                  StyleNumber = 77
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Default'
                  StyleNumber = 81
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Comment'
                  StyleNumber = 82
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Number'
                  StyleNumber = 83
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP KeyWord'
                  StyleNumber = 84
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP String'
                  StyleNumber = 85
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Identifier'
                  StyleNumber = 86
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clWhite
                  BackColor = 2105376
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Unterminated string'
                  StyleNumber = 87
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Python Start'
                  StyleNumber = 90
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Default'
                  StyleNumber = 91
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Comment'
                  StyleNumber = 92
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Number'
                  StyleNumber = 93
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python String'
                  StyleNumber = 94
                end
                item
                  FontName = 'Courier New'
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Single quoted string'
                  StyleNumber = 95
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Keyword'
                  StyleNumber = 96
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 127
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Triple quotes'
                  StyleNumber = 97
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 127
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Triple double quotes'
                  StyleNumber = 98
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clBlue
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Class name definition'
                  StyleNumber = 99
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 8355584
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python function or method name definition'
                  StyleNumber = 100
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python function or method name definition'
                  StyleNumber = 101
                end
                item
                  FontSize = 0
                  FontStyles = []
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'Python Identifiers'
                  StyleNumber = 102
                end
                item
                  FontSize = 0
                  FontStyles = [fsItalic]
                  ForeColor = 10526720
                  CharCase = CASE_MIXED
                  Name = 'PHP Complex Variable'
                  StyleNumber = 104
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'ASP Python Start'
                  StyleNumber = 105
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Default'
                  StyleNumber = 106
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Comment'
                  StyleNumber = 107
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Number'
                  StyleNumber = 108
                end
                item
                  FontName = 'Courier New'
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python String'
                  StyleNumber = 109
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Single quoted string'
                  StyleNumber = 110
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Keyword'
                  StyleNumber = 111
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 127
                  BackColor = 13627343
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Triple quotes'
                  StyleNumber = 112
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 127
                  BackColor = 13627343
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Triple double quotes'
                  StyleNumber = 113
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clBlue
                  BackColor = 13627343
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Class name definition'
                  StyleNumber = 114
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = 8355584
                  BackColor = 15728623
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python function or method name definition'
                  StyleNumber = 115
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  BackColor = 13627343
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python function or method name definition'
                  StyleNumber = 116
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  BackColor = 13627343
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'ASP Python Identifiers'
                  StyleNumber = 117
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'PHP Default'
                  StyleNumber = 118
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'PHP Double quoted string'
                  StyleNumber = 119
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'PHP Single quoted string'
                  StyleNumber = 120
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clOlive
                  CharCase = CASE_MIXED
                  Name = 'PHP Keyword'
                  StyleNumber = 121
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  Name = 'PHP Number'
                  StyleNumber = 122
                end
                item
                  FontSize = 0
                  FontStyles = [fsItalic]
                  ForeColor = 10526720
                  CharCase = CASE_MIXED
                  Name = 'PHP Variable'
                  StyleNumber = 123
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'PHP Comment'
                  StyleNumber = 124
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'PHP One line Comment'
                  StyleNumber = 125
                end
                item
                  FontSize = 0
                  FontStyles = [fsItalic]
                  ForeColor = 10526720
                  CharCase = CASE_MIXED
                  Name = 'PHP Variable in double quoted string'
                  StyleNumber = 126
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'PHP operator'
                  StyleNumber = 127
                end>
              Keywords = <
                item
                  KeywordListNumber = 0
                  Name = 'HyperText'
                  Keywords.Strings = (
                    'a'
                    'abbr'
                    'acronym'
                    'address'
                    'applet'
                    'area'
                    'b'
                    'base'
                    'basefont'
                    'bdo'
                    'big'
                    'blockquote'
                    'body'
                    'br'
                    'button'
                    'caption'
                    'center'
                    'cite'
                    'code'
                    'col'
                    'colgroup'
                    'dd'
                    'del'
                    'dfn'
                    'dir'
                    'div'
                    'dl'
                    'dt'
                    'em'
                    'fieldset'
                    'font'
                    'form'
                    'frame'
                    'frameset'
                    'h1'
                    'h2'
                    'h3'
                    'h4'
                    'h5'
                    'h6'
                    'head'
                    'hr'
                    'html'
                    'i'
                    'iframe'
                    'img'
                    'input'
                    'ins'
                    'isindex'
                    'kbd'
                    'label'
                    'legend'
                    'li'
                    'link'
                    'map'
                    'menu'
                    'meta'
                    'noframes'
                    'noscript'
                    'object'
                    'ol'
                    'optgroup'
                    'option'
                    'p'
                    'param'
                    'pre'
                    'q'
                    's'
                    'samp'
                    'script'
                    'select'
                    'small'
                    'span'
                    'strike'
                    'strong'
                    'style'
                    'sub'
                    'sup'
                    'table'
                    'tbody'
                    'td'
                    'textarea'
                    'tfoot'
                    'th'
                    'thead'
                    'title'
                    'tr'
                    'tt'
                    'u'
                    'ul'
                    'var'
                    'xml'
                    'xmlns'
                    'abbr'
                    'accept-charset'
                    'accept'
                    'accesskey'
                    'action'
                    'align'
                    'alink'
                    'alt'
                    'archive'
                    'axis'
                    'background'
                    'bgcolor'
                    'border'
                    'cellpadding'
                    'cellspacing'
                    'char'
                    'charoff'
                    'charset'
                    'checked'
                    'cite'
                    'class'
                    'classid'
                    'clear'
                    'codebase'
                    'codetype'
                    'color'
                    'cols'
                    'colspan'
                    'compact'
                    'content'
                    'coords'
                    'data'
                    'datafld'
                    'dataformatas'
                    'datapagesize'
                    'datasrc'
                    'datetime'
                    'declare'
                    'defer'
                    'dir'
                    'disabled'
                    'enctype'
                    'event'
                    'face'
                    'for'
                    'frame'
                    'frameborder'
                    'headers'
                    'height'
                    'href'
                    'hreflang'
                    'hspace'
                    'http-equiv'
                    'id'
                    'ismap'
                    'label'
                    'lang'
                    'language'
                    'leftmargin'
                    'link'
                    'longdesc'
                    'marginwidth'
                    'marginheight'
                    'maxlength'
                    'media'
                    'method'
                    'multiple'
                    'name'
                    'nohref'
                    'noresize'
                    'noshade'
                    'nowrap'
                    'object'
                    'onblur'
                    'onchange'
                    'onclick'
                    'ondblclick'
                    'onfocus'
                    'onkeydown'
                    'onkeypress'
                    'onkeyup'
                    'onload'
                    'onmousedown'
                    'onmousemove'
                    'onmouseover'
                    'onmouseout'
                    'onmouseup'
                    'onreset'
                    'onselect'
                    'onsubmit'
                    'onunload'
                    'profile'
                    'prompt'
                    'readonly'
                    'rel'
                    'rev'
                    'rows'
                    'rowspan'
                    'rules'
                    'scheme'
                    'scope'
                    'selected'
                    'shape'
                    'size'
                    'span'
                    'src'
                    'standby'
                    'start'
                    'style'
                    'summary'
                    'tabindex'
                    'target'
                    'text'
                    'title'
                    'topmargin'
                    'type'
                    'usemap'
                    'valign'
                    'value'
                    'valuetype'
                    'version'
                    'vlink'
                    'vspace'
                    'width'
                    'text'
                    'password'
                    'checkbox'
                    'radio'
                    'submit'
                    'reset'
                    'file'
                    'hidden'
                    'image'
                    'framespacing'
                    'scrolling'
                    'allowtransparency'
                    'bordercolor')
                end
                item
                  KeywordListNumber = 1
                  Name = 'JavaScript'
                  Keywords.Strings = (
                    'abstract'
                    'boolean'
                    'break'
                    'byte'
                    'case'
                    'catch'
                    'char'
                    'class'
                    'const'
                    'continue'
                    'debugger'
                    'default'
                    'delete'
                    'do'
                    'double'
                    'else'
                    'enum'
                    'export'
                    'extends'
                    'final'
                    'finally'
                    'float'
                    'for'
                    'function'
                    'goto'
                    'if'
                    'implements'
                    'import'
                    'in'
                    'instanceof'
                    'int'
                    'interface'
                    'long'
                    'native'
                    'new'
                    'package'
                    'private'
                    'protected'
                    'public'
                    'return'
                    'short'
                    'static'
                    'super'
                    'switch'
                    'synchronized'
                    'this'
                    'throw'
                    'throws'
                    'transient'
                    'try'
                    'typeof'
                    'var'
                    'void'
                    'volatile'
                    'while'
                    'with')
                end
                item
                  KeywordListNumber = 2
                  Name = 'VBScript'
                  Keywords.Strings = (
                    'and'
                    'begin'
                    'case'
                    'call'
                    'class'
                    'continue'
                    'do'
                    'each'
                    'else'
                    'elseif'
                    'end'
                    'erase'
                    'error'
                    'event'
                    'exit'
                    'false'
                    'for'
                    'function'
                    'get'
                    'gosub'
                    'goto'
                    'if'
                    'implement'
                    'in'
                    'load'
                    'loop'
                    'lset'
                    'me'
                    'mid'
                    'new'
                    'next'
                    'not'
                    'nothing'
                    'on'
                    'or'
                    'property'
                    'raiseevent'
                    'rem'
                    'resume'
                    'return'
                    'rset'
                    'select'
                    'set'
                    'stop'
                    'sub'
                    'then'
                    'to'
                    'true'
                    'unload'
                    'until'
                    'wend'
                    'while'
                    'with'
                    'withevents'
                    'attribute'
                    'alias'
                    'as'
                    'boolean'
                    'byref'
                    'byte'
                    'byval'
                    'const'
                    'compare'
                    'currency'
                    'date'
                    'declare'
                    'dim'
                    'double'
                    'enum'
                    'explicit'
                    'friend'
                    'global'
                    'integer'
                    'let'
                    'lib'
                    'long'
                    'module'
                    'object'
                    'option'
                    'optional'
                    'preserve'
                    'private'
                    'public'
                    'redim'
                    'single'
                    'static'
                    'string'
                    'type'
                    'variant')
                end
                item
                  KeywordListNumber = 3
                  Name = 'Python'
                  Keywords.Strings = (
                    'and'
                    'assert'
                    'break'
                    'class'
                    'continue'
                    'def'
                    'del'
                    'elif'
                    'else'
                    'except'
                    'exec'
                    'finally'
                    'for'
                    'from'
                    'global'
                    'if'
                    'import'
                    'in'
                    'is'
                    'lambda'
                    'None'
                    'not'
                    'or'
                    'pass'
                    'print'
                    'raise'
                    'return'
                    'try'
                    'while'
                    'yield')
                end
                item
                  KeywordListNumber = 4
                  Name = 'PHP'
                  Keywords.Strings = (
                    'and'
                    'argv'
                    'as'
                    'argc'
                    'break'
                    'case'
                    'cfunction'
                    'class'
                    'continue'
                    'declare'
                    'default'
                    'do'
                    'die'
                    'echo'
                    'else'
                    'elseif'
                    'empty'
                    'enddeclare'
                    'endfor'
                    'endforeach'
                    'endif'
                    'endswitch'
                    'endwhile'
                    'e_all'
                    'e_parse'
                    'e_error'
                    'e_warning'
                    'eval'
                    'exit'
                    'extends'
                    'false'
                    'for'
                    'foreach'
                    'function'
                    'global'
                    'http_cookie_vars'
                    'http_get_vars'
                    'http_post_vars'
                    'http_post_files'
                    'http_env_vars'
                    'http_server_vars'
                    'if'
                    'include'
                    'include_once'
                    'list'
                    'new'
                    'not'
                    'null'
                    'old_function'
                    'or'
                    'parent'
                    'php_os'
                    'php_self'
                    'php_version'
                    'print'
                    'require'
                    'require_once'
                    'return'
                    'static'
                    'switch'
                    'stdclass'
                    'this'
                    'true'
                    'var'
                    'xor'
                    'virtual'
                    'while'
                    '__file__'
                    '__line__'
                    '__sleep'
                    '__wakeup')
                end
                item
                  KeywordListNumber = 5
                  Name = 'DTD Keywords'
                  Keywords.Strings = (
                    'ELEMENT'
                    'DOCTYPE'
                    'ATTLIST'
                    'ENTITY'
                    'NOTATION')
                end>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '<!--'
              CommentBoxEnd = '-->'
              CommentBoxMiddle = ' '
              CommentBlock = '//'
              CommentAtLineStart = True
              CommentStreamStart = '<!--'
              CommentStreamEnd = '-->'
              NumStyleBits = 7
            end
            item
              Name = 'C++'
              Lexer = 'cpp'
              Styles = <
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12255232
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end
                item
                  FontName = 'Courier New'
                  FontSize = 8
                  FontStyles = []
                  ForeColor = 12255232
                  CharCase = CASE_MIXED
                  Name = 'White space'
                  StyleNumber = 0
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Comment'
                  StyleNumber = 1
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Line Comment'
                  StyleNumber = 2
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Doc Comment'
                  StyleNumber = 3
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clNavy
                  CharCase = CASE_MIXED
                  Name = 'Number'
                  StyleNumber = 4
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 30464
                  CharCase = CASE_MIXED
                  Name = 'Keyword'
                  StyleNumber = 5
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Double quoted string'
                  StyleNumber = 6
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Single quoted string'
                  StyleNumber = 7
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Symbols/UUID'
                  StyleNumber = 8
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 33023
                  CharCase = CASE_MIXED
                  Name = 'Preprocessor'
                  StyleNumber = 9
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 30464
                  CharCase = CASE_MIXED
                  Name = 'Operators'
                  StyleNumber = 10
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clNavy
                  CharCase = CASE_MIXED
                  Name = 'Identifier'
                  StyleNumber = 11
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'EOL if string is not closed'
                  StyleNumber = 12
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'Verbatim strings for C#'
                  StyleNumber = 13
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clHotLight
                  CharCase = CASE_MIXED
                  Name = 'Regular expressions'
                  StyleNumber = 14
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Doc Comment Line'
                  StyleNumber = 15
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'User-defined keywords'
                  StyleNumber = 16
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 33023
                  CharCase = CASE_MIXED
                  Name = 'Comment keyword'
                  StyleNumber = 17
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Comment keyword error'
                  StyleNumber = 18
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGreen
                  CharCase = CASE_MIXED
                  Name = 'Global classes and typedefs'
                  StyleNumber = 19
                end>
              Keywords = <
                item
                  KeywordListNumber = 0
                  Name = 'Primary keywords and identifiers'
                  Keywords.Strings = (
                    '__asm'
                    '_asm'
                    'asm'
                    'auto'
                    '__automated'
                    'bool'
                    'break'
                    'case'
                    'catch'
                    '__cdecl'
                    '_cdecl'
                    'cdecl'
                    'char'
                    'class'
                    '__classid'
                    '__closure'
                    'const'
                    'const_cast'
                    'continue'
                    '__declspec'
                    'default'
                    'delete'
                    '__dispid'
                    'do'
                    'double'
                    'dynamic_cast'
                    'else'
                    'enum'
                    '__except'
                    'explicit'
                    '__export'
                    'export'
                    'extern'
                    'false'
                    '__fastcall'
                    '_fastcall'
                    '__finally'
                    'float'
                    'for'
                    'friend'
                    'goto'
                    'if'
                    '__import'
                    '_import'
                    '__inline'
                    'inline'
                    'int'
                    '__int16'
                    '__int32'
                    '__int64'
                    '__int8'
                    'long'
                    '__msfastcall'
                    '__msreturn'
                    'mutable'
                    'namespace'
                    'new'
                    '__pascal'
                    '_pascal'
                    'pascal'
                    'private'
                    '__property'
                    'protected'
                    'public'
                    '__published'
                    'register'
                    'reinterpret_cast'
                    'return'
                    '__rtti'
                    'short'
                    'signed'
                    'sizeof'
                    'static_cast'
                    'static'
                    '__stdcall'
                    '_stdcall'
                    'struct'
                    'switch'
                    'template'
                    'this'
                    '__thread'
                    'throw'
                    'true'
                    '__try'
                    'try'
                    'typedef'
                    'typeid'
                    'typename'
                    'union'
                    'unsigned'
                    'using'
                    'virtual'
                    'void'
                    'volatile'
                    'wchar_t'
                    'while'
                    'dllexport'
                    'dllimport'
                    'naked'
                    'noreturn'
                    'nothrow'
                    'novtable'
                    'property'
                    'selectany'
                    'thread'
                    'uuid')
                end
                item
                  KeywordListNumber = 1
                  Name = 'Secondary keywords and identifiers'
                  Keywords.Strings = (
                    'TStream'
                    'TFileStream'
                    'TMemoryStream'
                    'TBlobStream'
                    'TOleStream'
                    'TStrings'
                    'TStringList'
                    'AnsiString'
                    'String'
                    'WideString'
                    'cout'
                    'cin'
                    'cerr'
                    'endl'
                    'fstream'
                    'ostream'
                    'istream'
                    'wstring'
                    'string'
                    'deque'
                    'list'
                    'vector'
                    'set'
                    'multiset'
                    'bitset'
                    'map'
                    'multimap'
                    'stack'
                    'queue'
                    'priority_queue')
                end
                item
                  KeywordListNumber = 2
                  Name = 'Doc Comments'
                  Keywords.Strings = (
                    'a'
                    'addindex'
                    'addtogroup'
                    'anchor'
                    'arg'
                    'attention'
                    'author'
                    'b'
                    'brief'
                    'bug'
                    'c'
                    'class'
                    'code'
                    'date'
                    'def'
                    'defgroup'
                    'deprecated'
                    'dontinclude'
                    'e'
                    'em'
                    'endcode'
                    'endhtmlonly'
                    'endif'
                    'endlatexonly'
                    'endlink'
                    'endverbatim'
                    'enum'
                    'example'
                    'exception'
                    'f$'
                    'f['
                    'f]'
                    'file'
                    'fn'
                    'hideinitializer'
                    'htmlinclude'
                    'htmlonly'
                    'if'
                    'image'
                    'include'
                    'ingroup'
                    'internal'
                    'invariant'
                    'interface'
                    'latexonly'
                    'li'
                    'line'
                    'link'
                    'mainpage'
                    'name'
                    'namespace'
                    'nosubgrouping'
                    'note'
                    'overload'
                    'p'
                    'page'
                    'par'
                    'param'
                    'post'
                    'pre'
                    'ref'
                    'relates'
                    'remarks'
                    'return'
                    'retval'
                    'sa'
                    'section'
                    'see'
                    'showinitializer'
                    'since'
                    'skip'
                    'skipline'
                    'struct'
                    'subsection'
                    'test'
                    'throw'
                    'todo'
                    'typedef'
                    'union'
                    'until'
                    'var'
                    'verbatim'
                    'verbinclude'
                    'version'
                    'warning'
                    'weakgroup'
                    '$'
                    '@'
                    '<'
                    '>'
                    '\'
                    '&'
                    '#'
                    '{'
                    '}')
                end
                item
                  KeywordListNumber = 3
                  Name = 'Unused'
                end
                item
                  KeywordListNumber = 4
                  Name = 'Global classes and typedefs'
                  Keywords.Strings = (
                    'LOL')
                end>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '/*'
              CommentBoxEnd = '*/'
              CommentBoxMiddle = '*'
              CommentBlock = '//'
              CommentAtLineStart = True
              CommentStreamStart = '/*'
              CommentStreamEnd = '*/'
              NumStyleBits = 5
            end
            item
              Name = 'SQL'
              Lexer = 'mssql'
              Styles = <
                item
                  FontName = 'Arial'
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clYellow
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = [fsBold]
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clBlack
                  BackColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Default'
                  StyleNumber = 0
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'Comment'
                  StyleNumber = 1
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 9474192
                  CharCase = CASE_MIXED
                  Name = 'Line Comment'
                  StyleNumber = 2
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 224
                  CharCase = CASE_MIXED
                  Name = 'Number'
                  StyleNumber = 3
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'String'
                  StyleNumber = 4
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Operator'
                  StyleNumber = 5
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clSilver
                  CharCase = CASE_MIXED
                  Name = 'Identifier'
                  StyleNumber = 6
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Variable'
                  StyleNumber = 7
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Column Name'
                  StyleNumber = 8
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Statement'
                  StyleNumber = 9
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Data Type'
                  StyleNumber = 10
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'System Table'
                  StyleNumber = 11
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Global Variable'
                  StyleNumber = 12
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Function'
                  StyleNumber = 13
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Stored Procedure'
                  StyleNumber = 14
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Default Pref Datatype'
                  StyleNumber = 15
                end
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'Column Name 2'
                  StyleNumber = 16
                end>
              Keywords = <
                item
                  KeywordListNumber = 0
                  Name = 'Statements'
                end
                item
                  KeywordListNumber = 1
                  Name = 'Data Types'
                end
                item
                  KeywordListNumber = 2
                  Name = 'System tables'
                end
                item
                  KeywordListNumber = 3
                  Name = 'Global variables'
                end
                item
                  KeywordListNumber = 4
                  Name = 'Functions'
                end
                item
                  KeywordListNumber = 5
                  Name = 'System Stored Procedures'
                end
                item
                  KeywordListNumber = 6
                  Name = 'Operators'
                end>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '/*'
              CommentBoxEnd = '*/'
              CommentBoxMiddle = '*'
              CommentBlock = '#'
              CommentAtLineStart = True
              CommentStreamStart = '/*'
              CommentStreamEnd = '*/'
              NumStyleBits = 5
            end
            item
              Name = 'Pawn'
              Lexer = 'cpp'
              Styles = <
                item
                  FontSize = 0
                  FontStyles = []
                  CharCase = CASE_MIXED
                  Name = 'LineNumbers'
                  StyleNumber = 33
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 12255232
                  CharCase = CASE_MIXED
                  Name = 'Ok Braces'
                  StyleNumber = 34
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Bad Braces'
                  StyleNumber = 35
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Control Chars'
                  StyleNumber = 36
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clGray
                  CharCase = CASE_MIXED
                  Name = 'Indent Guide'
                  StyleNumber = 37
                end
                item
                  FontName = 'Courier New'
                  FontSize = 8
                  FontStyles = []
                  ForeColor = 12255232
                  CharCase = CASE_MIXED
                  Name = 'White space'
                  StyleNumber = 0
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Comment'
                  StyleNumber = 1
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Line Comment'
                  StyleNumber = 2
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Doc Comment'
                  StyleNumber = 3
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clNavy
                  CharCase = CASE_MIXED
                  Name = 'Number'
                  StyleNumber = 4
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 30464
                  CharCase = CASE_MIXED
                  Name = 'Keyword'
                  StyleNumber = 5
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Double quoted string'
                  StyleNumber = 6
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Single quoted string'
                  StyleNumber = 7
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'Symbols/UUID'
                  StyleNumber = 8
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 33023
                  CharCase = CASE_MIXED
                  Name = 'Preprocessor'
                  StyleNumber = 9
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 30464
                  CharCase = CASE_MIXED
                  Name = 'Operators'
                  StyleNumber = 10
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clNavy
                  CharCase = CASE_MIXED
                  Name = 'Identifier'
                  StyleNumber = 11
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  EOLFilled = True
                  Name = 'EOL if string is not closed'
                  StyleNumber = 12
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clLime
                  CharCase = CASE_MIXED
                  Name = 'Verbatim strings for C#'
                  StyleNumber = 13
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clHotLight
                  CharCase = CASE_MIXED
                  Name = 'Regular expressions'
                  StyleNumber = 14
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = 4227327
                  CharCase = CASE_MIXED
                  Name = 'Doc Comment Line'
                  StyleNumber = 15
                end
                item
                  FontSize = 0
                  FontStyles = []
                  ForeColor = clRed
                  CharCase = CASE_MIXED
                  Name = 'User-defined keywords'
                  StyleNumber = 16
                end>
              Keywords = <
                item
                  KeywordListNumber = 0
                  Name = 'Primary keywords and identifiers'
                  Keywords.Strings = (
                    'assert'
                    'char'
                    '#assert'
                    'const'
                    'break'
                    'de'
                    'ned'
                    '#de'
                    'ne'
                    'enum'
                    'case'
                    'sizeof'
                    '#else'
                    'forward'
                    'continue'
                    'tagof'
                    '#emit'
                    'native'
                    'default'
                    '#endif'
                    'new'
                    'do'
                    '#endinput'
                    'operator'
                    'else'
                    '#endscript'
                    'public'
                    'exit'
                    '#error'
                    'static'
                    'for'
                    '#'
                    'le'
                    'stock'
                    'goto'
                    '#if'
                    'if'
                    '#include'
                    'return'
                    '#line'
                    'sleep'
                    '#pragma'
                    'state'
                    '#section'
                    'switch'
                    '#tryinclude'
                    'while'
                    '#undef'
                    'Float')
                end
                item
                  KeywordListNumber = 1
                  Name = 'Secondary keywords and identifiers'
                end
                item
                  KeywordListNumber = 2
                  Name = 'Doc Comments'
                  Keywords.Strings = (
                    'a'
                    'addindex'
                    'addtogroup'
                    'anchor'
                    'arg'
                    'attention'
                    'author'
                    'b'
                    'brief'
                    'bug'
                    'c'
                    'class'
                    'code'
                    'date'
                    'def'
                    'defgroup'
                    'deprecated'
                    'dontinclude'
                    'e'
                    'em'
                    'endcode'
                    'endhtmlonly'
                    'endif'
                    'endlatexonly'
                    'endlink'
                    'endverbatim'
                    'enum'
                    'example'
                    'exception'
                    'f$'
                    'f['
                    'f]'
                    'file'
                    'fn'
                    'hideinitializer'
                    'htmlinclude'
                    'htmlonly'
                    'if'
                    'image'
                    'include'
                    'ingroup'
                    'internal'
                    'invariant'
                    'interface'
                    'latexonly'
                    'li'
                    'line'
                    'link'
                    'mainpage'
                    'name'
                    'namespace'
                    'nosubgrouping'
                    'note'
                    'overload'
                    'p'
                    'page'
                    'par'
                    'param'
                    'post'
                    'pre'
                    'ref'
                    'relates'
                    'remarks'
                    'return'
                    'retval'
                    'sa'
                    'section'
                    'see'
                    'showinitializer'
                    'since'
                    'skip'
                    'skipline'
                    'struct'
                    'subsection'
                    'test'
                    'throw'
                    'todo'
                    'typedef'
                    'union'
                    'until'
                    'var'
                    'verbatim'
                    'verbinclude'
                    'version'
                    'warning'
                    'weakgroup'
                    '$'
                    '@'
                    '<'
                    '>'
                    '\'
                    '&'
                    '#'
                    '{'
                    '}')
                end
                item
                  KeywordListNumber = 3
                  Name = 'Unused'
                end>
              AssignmentOperator = '='
              EndOfStatementOperator = ';'
              CommentBoxStart = '/*'
              CommentBoxEnd = '*/'
              CommentBoxMiddle = '*'
              CommentBlock = '//'
              CommentAtLineStart = True
              CommentStreamStart = '/*'
              CommentStreamEnd = '*/'
              NumStyleBits = 5
            end>
          LanguageManager.SelectedLanguage = 'Pawn'
          FoldDrawFlags = [sciBelowIfNotExpanded]
          KeyCommands = <
            item
              Command = 2300
              ShortCut = 40
            end
            item
              Command = 2301
              ShortCut = 8232
            end
            item
              Command = 2342
              ShortCut = 16424
            end
            item
              Command = 2426
              ShortCut = 41000
            end
            item
              Command = 2302
              ShortCut = 38
            end
            item
              Command = 2303
              ShortCut = 8230
            end
            item
              Command = 2343
              ShortCut = 16422
            end
            item
              Command = 2427
              ShortCut = 40998
            end
            item
              Command = 2415
              ShortCut = 49190
            end
            item
              Command = 2416
              ShortCut = 57382
            end
            item
              Command = 2413
              ShortCut = 49192
            end
            item
              Command = 2414
              ShortCut = 57384
            end
            item
              Command = 2304
              ShortCut = 37
            end
            item
              Command = 2305
              ShortCut = 8229
            end
            item
              Command = 2308
              ShortCut = 16421
            end
            item
              Command = 2309
              ShortCut = 24613
            end
            item
              Command = 2428
              ShortCut = 40997
            end
            item
              Command = 2306
              ShortCut = 39
            end
            item
              Command = 2307
              ShortCut = 8231
            end
            item
              Command = 2310
              ShortCut = 16423
            end
            item
              Command = 2311
              ShortCut = 24615
            end
            item
              Command = 2429
              ShortCut = 40999
            end
            item
              Command = 2390
              ShortCut = 49189
            end
            item
              Command = 2391
              ShortCut = 57381
            end
            item
              Command = 2392
              ShortCut = 49191
            end
            item
              Command = 2393
              ShortCut = 57383
            end
            item
              Command = 2331
              ShortCut = 36
            end
            item
              Command = 2332
              ShortCut = 8228
            end
            item
              Command = 2316
              ShortCut = 16420
            end
            item
              Command = 2317
              ShortCut = 24612
            end
            item
              Command = 2345
              ShortCut = 32804
            end
            item
              Command = 2431
              ShortCut = 40996
            end
            item
              Command = 2314
              ShortCut = 35
            end
            item
              Command = 2315
              ShortCut = 8227
            end
            item
              Command = 2318
              ShortCut = 16419
            end
            item
              Command = 2319
              ShortCut = 24611
            end
            item
              Command = 2347
              ShortCut = 32803
            end
            item
              Command = 2432
              ShortCut = 40995
            end
            item
              Command = 2320
              ShortCut = 33
            end
            item
              Command = 2321
              ShortCut = 8225
            end
            item
              Command = 2433
              ShortCut = 40993
            end
            item
              Command = 2322
              ShortCut = 34
            end
            item
              Command = 2323
              ShortCut = 8226
            end
            item
              Command = 2434
              ShortCut = 40994
            end
            item
              Command = 2180
              ShortCut = 46
            end
            item
              Command = 2177
              ShortCut = 8238
            end
            item
              Command = 2336
              ShortCut = 16430
            end
            item
              Command = 2396
              ShortCut = 24622
            end
            item
              Command = 2324
              ShortCut = 45
            end
            item
              Command = 2179
              ShortCut = 8237
            end
            item
              Command = 2178
              ShortCut = 16429
            end
            item
              Command = 2325
              ShortCut = 27
            end
            item
              Command = 2326
              ShortCut = 8
            end
            item
              Command = 2326
              ShortCut = 8200
            end
            item
              Command = 2335
              ShortCut = 16392
            end
            item
              Command = 2176
              ShortCut = 32776
            end
            item
              Command = 2395
              ShortCut = 24584
            end
            item
              Command = 2176
              ShortCut = 16474
            end
            item
              Command = 2011
              ShortCut = 16473
            end
            item
              Command = 2177
              ShortCut = 16472
            end
            item
              Command = 2178
              ShortCut = 16451
            end
            item
              Command = 2179
              ShortCut = 16470
            end
            item
              Command = 2013
              ShortCut = 16449
            end
            item
              Command = 2327
              ShortCut = 9
            end
            item
              Command = 2328
              ShortCut = 8201
            end
            item
              Command = 2329
              ShortCut = 13
            end
            item
              Command = 2329
              ShortCut = 8205
            end
            item
              Command = 2333
              ShortCut = 16491
            end
            item
              Command = 2334
              ShortCut = 16493
            end
            item
              Command = 2373
              ShortCut = 16495
            end
            item
              Command = 2337
              ShortCut = 16460
            end
            item
              Command = 2338
              ShortCut = 24652
            end
            item
              Command = 2455
              ShortCut = 24660
            end
            item
              Command = 2339
              ShortCut = 16468
            end
            item
              Command = 2404
              ShortCut = 16452
            end
            item
              Command = 2340
              ShortCut = 16469
            end
            item
              Command = 2341
              ShortCut = 24661
            end>
        end
        object lstOutput: TListBox
          Left = 0
          Top = 391
          Width = 666
          Height = 82
          Align = alBottom
          ItemHeight = 13
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Visible = False
          OnDblClick = lstOutputDblClick
          OnEnter = lstOutputEnter
          OnMouseDown = lstOutputMouseDown
          OnMouseMove = lstOutputMouseMove
        end
        object pnlLoading: TSpTBXPanel
          Left = 268
          Top = 278
          Width = 253
          Height = 57
          Caption = 'pnlLoading'
          Color = clNone
          ParentColor = False
          TabOrder = 2
          Visible = False
          TBXStyleBackground = True
          object pbLoading: TSpTBXProgressBar
            Left = 4
            Top = 4
            Width = 245
            Height = 18
            Caption = '0 % - Loading File...'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            CaptionGlowColor = clBtnFace
            CaptionType = pctDefault
            Smooth = True
            ThemeType = thtTBX
          end
          object cmdCancel: TSpTBXButton
            Left = 70
            Top = 30
            Width = 111
            Height = 21
            Cursor = crArrow
            Caption = 'Click here to cancel'
            TabOrder = 1
            OnClick = cmdCancelClick
            CaptionGlowColor = clBtnFace
            LinkFont.Charset = DEFAULT_CHARSET
            LinkFont.Color = clBlue
            LinkFont.Height = -11
            LinkFont.Name = 'MS Sans Serif'
            LinkFont.Style = [fsUnderline]
            ThemeType = thtTBX
          end
        end
      end
      object tcTools: TSpTBXTabControl
        Left = 672
        Top = 23
        Width = 205
        Height = 473
        Align = alRight
        ActiveTabIndex = 0
        TabPosition = ttpBottom
        ThemeType = tttTBX
        OnActiveTabChange = tcToolsActiveTabChange
        HiddenItems = <>
        object tiTools: TSpTBXTabItem
          Checked = True
          TabPosition = ttpBottom
          ThemeType = tttTBX
          CaptionW = 'Code-Tools'
        end
        object tiNotes: TSpTBXTabItem
          TabPosition = ttpBottom
          ThemeType = tttTBX
          CaptionW = 'Notes'
        end
        object tsNotes: TSpTBXTabSheet
          Left = 0
          Top = 0
          Width = 205
          Height = 450
          Caption = 'Notes'
          ImageIndex = -1
          TabItem = 'tiNotes'
          object imgRight4: TImage
            Left = 202
            Top = 26
            Width = 1
            Height = 422
            Align = alRight
          end
          object imgBottom4: TImage
            Left = 2
            Top = 448
            Width = 201
            Height = 2
            Align = alBottom
          end
          object imgTop4: TImage
            Left = 2
            Top = 24
            Width = 201
            Height = 2
            Align = alTop
          end
          object imgLeft4: TImage
            Left = 2
            Top = 26
            Width = 1
            Height = 422
            Align = alLeft
          end
          object tbxNotes: TSpTBXToolbar
            Left = 2
            Top = 2
            Width = 201
            Height = 22
            Align = alTop
            Caption = 'tbxNotes'
            Images = ilImages
            TabOrder = 0
            ChevronVertical = True
            object mnuBold: TSpTBXItem
              AutoCheck = True
              ImageIndex = 29
              OnClick = mnuBoldClick
              CaptionW = 'Bold'
            end
            object mnuItalic: TSpTBXItem
              AutoCheck = True
              ImageIndex = 27
              OnClick = mnuItalicClick
              CaptionW = 'Italic'
            end
            object mnuUnderline: TSpTBXItem
              AutoCheck = True
              ImageIndex = 28
              OnClick = mnuUnderlineClick
              CaptionW = 'Underline'
            end
            object sepNotes: TSpTBXSeparatorItem
            end
            object mnuSelectColor: TSpTBXSubmenuItem
              ImageIndex = 26
              CaptionW = 'Select color'
              object cpNotes: TTBXColorPalette
                Color = clBlack
                PaletteOptions = [tpoCustomImages]
                OnChange = cpNotesChange
              end
            end
          end
          object rtfNotes: TRichEdit
            Left = 3
            Top = 26
            Width = 199
            Height = 422
            Align = alClient
            TabOrder = 1
            OnKeyDown = rtfNotesKeyDown
            OnMouseDown = rtfNotesMouseDown
          end
        end
        object tsExplorer: TSpTBXTabSheet
          Left = 0
          Top = 0
          Width = 205
          Height = 450
          Caption = 'Code-Tools'
          ImageIndex = -1
          TabItem = 'tiTools'
          object spcBottom2: TImage
            Left = 0
            Top = 448
            Width = 205
            Height = 2
            Align = alBottom
          end
          object spcLeft2: TImage
            Left = 0
            Top = 0
            Width = 3
            Height = 448
            Align = alLeft
          end
          object spcRight2: TImage
            Left = 202
            Top = 0
            Width = 3
            Height = 448
            Align = alRight
          end
          object pnlDock: TSpTBXMultiDock
            Left = 3
            Top = 0
            Width = 199
            Height = 448
            Position = dpxClient
            object pnlCodeExplorer: TSpTBXDockablePanel
              Left = 0
              Top = 0
              Caption = 'Code-Explorer'
              DockedWidth = 195
              DockPos = 0
              TabOrder = 0
              OnVisibleChanged = pnlCodeExplorerVisibleChanged
              object trvExplorer: TTreeView
                Left = 0
                Top = 26
                Width = 195
                Height = 186
                Align = alClient
                Images = ilImages
                Indent = 19
                ReadOnly = True
                SortType = stBoth
                TabOrder = 1
                OnClick = trvExplorerClick
                OnCollapsed = trvExplorerCollapsed
                OnDblClick = trvExplorerDblClick
                OnExpanded = trvExplorerExpanded
                Items.Data = {
                  07000000220000002A0000002A000000FFFFFFFFFFFFFFFF0000000000000000
                  09436F6E7374616E7473200000002A0000002A000000FFFFFFFFFFFFFFFF0000
                  00000100000007446566696E65641E0000002A0000002A000000FFFFFFFFFFFF
                  FFFF0000000000000000054356617273210000002A0000002A000000FFFFFFFF
                  FFFFFFFF000000000000000008466F727761726473210000002A0000002A0000
                  00FFFFFFFFFFFFFFFF000000000000000008496E636C75646564200000002A00
                  00002A000000FFFFFFFFFFFFFFFF0000000003000000074D6574686F64732000
                  00002A0000002A000000FFFFFFFFFFFFFFFF0000000000000000074465666175
                  6C741F0000002A0000002A000000FFFFFFFFFFFFFFFF00000000000000000645
                  76656E74731F0000002A0000002A000000FFFFFFFFFFFFFFFF00000000000000
                  000653746F636B73200000002A0000002A000000FFFFFFFFFFFFFFFF00000000
                  00000000074E617469766573220000002A0000002A000000FFFFFFFFFFFFFFFF
                  0000000000000000095661726961626C6573}
              end
            end
            object pnlCodeInspector: TSpTBXDockablePanel
              Left = 0
              Top = 216
              Caption = 'Code-Inspector'
              DockedWidth = 195
              DockPos = 216
              TabOrder = 1
              OnVisibleChanged = pnlCodeInspectorVisibleChanged
              object jviCode: TJvInspector
                Left = 0
                Top = 26
                Width = 195
                Height = 186
                Align = alClient
                Divider = 110
                ItemHeight = 16
                Painter = JvInspectorDotNETPainter
                OnItemValueChanged = jviCodeItemValueChanged
              end
            end
          end
        end
      end
    end
  end
  object sbStatus: TSpTBXStatusBar
    Left = 0
    Top = 591
    Width = 880
    Height = 23
    object mnuFilename: TSpTBXRightAlignSpacerItem
      CustomWidth = 623
      CaptionW = 'Untitled.sma'
    end
    object sepStatus1: TSpTBXSeparatorItem
    end
    object mnuShowCodeTools: TSpTBXItem
      AutoCheck = True
      Checked = True
      OnClick = mnuShowCodeToolsClick
      CaptionW = 'Show Code-Tools'
    end
    object sepStatus2: TSpTBXSeparatorItem
    end
    object mnuModified: TSpTBXRightAlignSpacerItem
      CustomWidth = 50
    end
    object sepStatus3: TSpTBXSeparatorItem
    end
    object mnuCaret: TSpTBXRightAlignSpacerItem
      Alignment = taCenter
      CustomWidth = 80
      CaptionW = 'Ln 0 Ch 0'
    end
  end
  object TBXSwitcher: TTBXSwitcher
    Theme = 'OfficeXP'
    Left = 813
    Top = 5
  end
  object ilImages: TImageList
    Left = 722
    Top = 36
    Bitmap = {
      494C010131003600040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000E000000001001000000000000070
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016421642164216421642
      1642164216421642164216421642164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000984EDF7BDF7BDF7BDF7B
      DF7BDF7BDF77DF77DF77DF77DF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000984EFF7FDF7BDF7BDF7B
      FF7FFF7FFF7FDF77DF77DF77DF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B852FF7FBF6BBF6BBF6B
      BF6BBF6BBF6BBF6BBF6BBF6BDF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B852FF7F5E535E535E53
      5E535E535E535E535E535E53DF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D956FF7FFF7FFF7FDF7B
      DF7BFF7FDF7BDF7BDF7BDF77DF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DA56FF7FBF6BBF6BBF6B
      BF6BBF6BBF6BBF6BBF6BBF6BDF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FA5AFF7F5E535E535E53
      5E535E535E535E535E535E53DF77164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FA5AFF7FFF7FFF7FFF7F
      FF7FFF7FDF7BDF7BDF7BDF7BDF7B164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FB5AFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FDF7BDF7BDF7BDF7B164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001B5BA56AA56AA56AA56A
      A56AA56AA56AA56A04520452F856164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C5BA56AD17FD17FD17F
      D17FD17FD17FA87F984A56463642164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C5BA56AD17F3D323D32
      3D32DC1D9C11360D373ADD3A9D26183600000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003D5FA56AD17FD17FD17F
      D17FD17FD17FA87F583E1F3B593A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C5BA56AA56AA56AA56A
      A56AA56AA56A0F77583E79420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C5B3D5B3D5B3D5B3D5B
      1C5B1C5B3D5BFA5A173E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000C105C105C105C1050000000000001042104210421042104210421042
      104210421042FF7F186310421863FF7F18630000000000000000000000000000
      0000000000000000000000000000000000000000000016421642164216421642
      1642164216421642164216421642164200000000000001560156015601560156
      01560000C105A31AA41AC1050000000000001042104210421042104210421042
      1042104210421042FF7F1042FF7F1042104200002A772A772A77A56AA56A2A77
      A56A2A77A56A2A77A56AA56A00000000000000000000984E9F6B9F6B9F6B7E63
      7E635E533D4F3D4B3D4B3D4B5E4F164200000000215E697F467F677FAD7FB17F
      E56E0156C105A31AA31AC1050000000000001000100010001000000000001042
      10421042FF7FFF7FFF7F1000100010001000A56AF37FF37F2B772B772A77F37F
      2B778D7F2A778D7F2B778D7FA56A0000000000000000984EBF6B1F3B1F3B1F3B
      1F3B1F3BDD3ADD3A9D269D263D4F16420000215ED17F8B7F467F677FAD7FC105
      C105C105C105E41EC31AC105C105C105C10500000000000010001F0010000000
      00001042FF7FFF7FFF7F1000000000000000A56AF37F2B77F37F2B778D7F2A77
      2B772B772B772A778D7F2A77A56A0000000000000000B852BF739F6B9F6B9F6B
      9F6B9F6B9F6B9F6B9F6B9F6B3D4F16420000215ED17F8B7F467F677FAD7FC105
      472F262F262B0527E522E41EC31AA31AC105000000000000100010001F001000
      0000FF7FFF7FFF7FFF7F1000000000000000A56AF37FF37FF37FF37F2B77F37F
      2A778D7F2B778D7F2A778D7FA56A0000000000000000B852DF739E6B9E679E63
      7E637E635E5B5E573D4F3D4F3D4B16420000215ED17F8B7F467F677FAD7FC105
      472F472F472F262B05270527E41EC41AC10500000000000010001F0010001F00
      0000FF7FFF7FFF7FFF7F1000000000000000A56AF37FF37FF37F2B77F37F2B77
      2B772B778D7F2B772B772A772B770000000000000000D956FF7B1F3B1F3B1F3B
      1F3B1F3BDD3ADD3A9D269D263D4F16420000215EF67FF67FF87FFB7FFB7FC105
      C105C105C105472F472FC105C105C105C105000000000000100010001F001000
      0000FF7FFF03FF7FFF031000000000000000A56AF37FF37F8D7FF37F8D7FF37F
      8D7FF37F2A778D7F2B778D7FA56A0000000000000000DA56FF7F9F6B9F6B9F6B
      9F6B9F6B9F6B9F6B9F6B9F6B5E5316420000215EB67BE672826682668266A36A
      415E9373C105472F472FC10500000000000000000000000010001F0010001F00
      0000FF03FF7FFF03FF7F1000000000000000A56AF37FF37FF37FB07FF37FF37F
      F37FB07F8D7F2A772B772B772B770000000000006001FA5AFF7FDF7BDF77BF6F
      7E637E637E637E637E5F5E5B5E5716420000215E06738B7F467F677FAD7FB17F
      E672215AC105472F472FC105000000000000000000000000100010001F001000
      0000FF7FFF03FF7FFF031000000000000000A56AF37FF37FF37F8D7FF37FF37F
      2B77F37F8D7FF37F2B778D7FA56A000000000000C0056001FF7F1F3B1F3B1F3B
      1F3B1F3BDD3ADD3A9D269D267E5B16420000215ED17F8B7F467F677FAD7FB17F
      E6726166C105C105C105C10500000000000000000000000010001F0010001F00
      0000FF03FF7FFF03FF7F1000000000000000A56AF37FF37FF37FF37FF37F2B77
      F37F2B77F37FF37F2B772A772B77000000000000C005C00560019F6B9F6B9F6B
      9F6B9F6B9F6B9F6B9F6B9F6B5E5B16420000215ED17F8B7F467F677FAD7FB17F
      E672616601560000000000000000000000000000000000001000100010001000
      100010001000100010001000000000000000A56A8D7FF37F2B77F37F2B778D7F
      A56AA56AA56A2A77A56AA56A00000000410E0000010A20062006FF7FFF7FFF7F
      DF7BBF737E637E63BF6B7C63F85616420000215ED17F8B7F467F677FAD7FB17F
      E672616601560000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000002A77F37FF37F2B778D7F0000
      00000000000000000000000000000000020E000020062006FF7F1F3B1F3BDD3A
      DD3A9D26DF779E6B984A5646364216420000215EB27FB07FAD7FAF7FB27FF57F
      8C7F826601560000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000002B772B77A56AA56A2A770000
      0000000000000000A56A020E0000A62A410E000020061C5BFF7F9F6B9F6B9F6B
      9F6B9F6BFF7B3C67373ADD3A9D2618360000215EFB7FFB7FFA7FF67FF57FF57F
      F57FAE7F01560000000000000000000000000000000000000000000000000002
      0002000200020000000000000000000000000000000000000000000000000000
      00000000000000000000410E410EA62A0000000000003D5FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F5C67583E1F3B593A000000000000415EFB7FFB7FF87FF57FF57F
      D47F015A00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A41E072F020E0000000000001C5BDF7BDF77DF77DF77
      DF77DF77DF773C67583E794200000000000000000000215E215E215E215E215E
      215E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000410E410E020E020E000000001C5B3D5B3D5B3D5B3D5B
      1C5B1C5B3D5BFA5A173E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8366A56A025A0000000000000000000000000000205E205E205E205E205E205E
      205E205E205E205E205E205E205E000000000000205E205E205E205E205E205E
      205E205E205E205E205E205E000000000000560D5011100D100D100D100D100D
      100D100D100D100D100D100D100D100D100D00000000000084668466025A6362
      A56AA56A225A025AA56A856A000000000000205E2C7B205ED47F2C7B2C7B2C7B
      2C7B2C7B2C7B2C7B2C7BA76ED57F205E0000205E84662B77907F2C7B2C7B2C7B
      2C7B2C7B2C7B2C7B2C7BA76E426200000000560DEA14EA14EA14EA14EA14EA14
      EA14EA14EA14EA14EA14EA14EA14EA14100D0000000000008466A56AA56A8366
      447F497B84660B6F0C6F8466000000000000205E4D7B205ED57F4D7F4D7F4D7F
      4D7F4D7F4D7F4D7F4D7FA76EB77B205E0000205EE972A66AB27F4D7F4D7F4D7F
      4D7F4D7F4D7F4D7F4D7FC76ED97F205E0000560D000000000000000000000000
      00000000000000000000000000000000100D0000225A636209736F7B09732777
      437F477F6B7B0E6B9577927B225A00000000205E4E7F205ED57F6F7F6F7F6F7F
      6F7F6F7F6F7F6F7F6F7FC86EB77B205E0000205E4E7F205EF57F6F7F6F7F6F7F
      6F7F6F7F6F7F6F7F6F7FC86ED97F205E0000560D000000000000000000000000
      00000000000000000000000000000000100D00000B6FA56A0973927B917B6B7B
      447B437F6A7B6E7B6F7B2777A56A84660000205E6F7F205ED67F907F907F907F
      907F907F907F907F907FC86ED87B205E0000205E6F7F4262B27FB27F907F907F
      907F907F907F907F907FE972D97F42620000560D000000000000000000000000
      00000000000000000000000000000000100D0000435E846609736F7B927B6E7B
      0B6F0973257B437F437F437F447FA56A0000205E907F205ED77FB17FB17FB17F
      B17FB17FB17FB17FB17FE972D77F205E0000205E907FC86E0B77F57FB17FB17F
      B17FB17FB17FB17FB17FE972D97FD97F205E560D00000821FF7F082108212104
      00000821AD35AD35AD35AD3500000000100D0000235EA56A08776D7BF166524A
      524A524A524A0973437F437F437F63660000205EB17F205EDA7FD97FD97FD97F
      D97FD97FD97FD97FD97F776FDA7F205E0000205E917F6F7F8466FC7FD97FD97F
      D97FD97FD97FD97FD97FB27FFC7FD97F205E560D0000FF7F2104000000000000
      FF7F0821000000000000000000000000100D0000435EA56AA56A6B7B524A7C73
      D65A9552D75A524A6A7B937B957763620000205ED27F205E205E205E205E205E
      205E205E205E205E205E205E205E205E0000205ED27FD27F4262205E205E205E
      205E205E205E205E205E205E205E205E205E560D0000FF7F00000000FF7F2104
      FF7F2104000000000000000000000000100D0000225A8366A56A0877524A7C73
      D65A9552D75A524A6F7B9577F16684660000205ED37FD37FD37FD37FD37FD37F
      D37FD37FD37FD37FD37F205E000000000000205ED37FD37FD37FD37FD37FD37F
      D37FD37FD37FD37FD37F215E000000000000560D00000821FF7F082100000000
      21040000000000000000000000000000100D00000000025A84662777524A7C73
      D65A9552D75A524A6C7B0C6F025A00000000205EDA7FD47FD47FD47FD47FD47F
      D47FD47FD47FD47FD47F205E000000000000205EFC7FF47FF47FF47FF47FF47F
      F47FF47FF47FF47FF47F215E000000000000560D000000000000000000000000
      00000000000000000000000000000000100D000000000000225A235E524A7C73
      D65A9552D75A524A6362435E0000000000000000205EDA7FD47FD47FD47F205E
      205E205E205E205E205E00000000000000000000205EFC7FF47FF47FF47F205E
      205E205E205E205E205E0000000000000000560D850C850C850C850C850C850C
      850C850C850C850C850C850C850C8508100D00000000000000000000524A7C73
      B7569552D75A524A0000000000000000000000000000205E205E205E205E0000
      00000000000000000000000000000000000000000000205E205E205E205E0000
      000000000000000000000000000000000000560D1D3F1D3F1D3F1D3F1D3F1D3F
      1D3F1D3F1D3F1D3F1D3F1D3F1D3F1D3F100D00000000000000000000524AD65A
      9452534A9552524A000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000560D3C1A3C1A3C1A3C1A3C1A3C1A
      3C1A3C1A3C1A3C1A7D267D2A3A2EED61100D00000000000000000000524A9C73
      D75A94529652524A000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000541554155415541554155415
      54155415541554155415541554155415000000000000000000000000524A7C73
      7C73D75A744E524A000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000524A
      524A524A524A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000AC39784A0000000000000000
      0000000000000000000000000000000000000000990199019901990199019901
      9901990199019901990199019901990199010000000000000000000000000000
      00000000000000000000000000000000000000001046A64D574A000000000000
      0000000000000000000000000000000000004C5A2476C459794A000000000000
      00000000000000000000000000000000000000009901FF7FFF7FFF7BDF73BF6B
      9F637F5B5F575F575F575F575F575F5799010000000000000000000000000000
      00000000000000000000000000000000000000002962256E0C4E374A00000000
      000000000000000000000000000000000000C97E887E24760F4A794A00000000
      00000000000000000000000000000000000000009901FF7FFF7FFF7FFF7BDF73
      BF6B9F637F5B00630063006300635F5799010000000000000000000000000000
      0000000000000000000000000000000000000000C97EEC7E256EE465574A0000
      0000000000000000000000000000000000000000EA7EEB7E2476C459794A0000
      00000000000000000000000000000000000000009901FF7FFF7FFF7FFF7FFF7B
      DF73BF6B9F63FF7FFF7FFF7F00635F5799014366436643664366000000000000
      00000000000000000000C00500000000000000000000C97EC97E256E0C4E574A
      0000000000000000000000000000000000000000867EEA7E887E2476104A794A
      00000000000000000000000000000000000000009901FF7FFF7FFF7FFF7FFF7F
      FF7BDF73BF6B9F637F5B5F575F575F5799014366D57FCF7FAD7BC66EC66EC66E
      4366436642660000C0058B47C00500000000000000000000C97EEC7E256EE461
      000000000000000000000000000000000000000000000000EA7EEB7E066A8A35
      0000B3358001C001C005E81900000000000000009901FF7FFF7FFF7FFF7FFF7F
      FF7FFF7BDF73BF6B9F637F5B5F575F57990143667077F27FCF7FD07FD07FCF7F
      8D7F6C7BC66EC0054A3F48370627C00500000000000000000000C97EC97E256E
      00000000153E153E153E153E000000000000000000000000867EEA7E386F734E
      3542081A902E4C226216A42249220000000000009901FF7FFF7FFF7F10421042
      1042FF7F1042DF731042104210425F57990143660C73F27FCE7FCF7FAE7FAE7F
      AE7FAF7FCF7F0A73221206276212000000000000000000000000000000000000
      574A1C53FF6FFF6FFF6FFF6F3C5B153E000000000000000000000000B856994A
      9F5FDF67FF6BFF6BFF6F0212E62A010E000000009901107E007C107EFF7FFF7F
      FF7FFF7FFF7FFF7BDF73BF6B9F637F5B990143664366907BCF7FCF7FAE7FAE7F
      AE7FAE7FAE7F0A73010AA316010A000000000000000000000000000000000000
      1C53DF63FF6FFF6FFF73FF73FF7F3C5B00000000000000000000000037467D5B
      D13EDF67FF6BFF6FFF77020E893FE10D000000009901007C007C007CFF7FFF7F
      FF7FFF7FFF7FFF7FFF7BDF73BF6B9F63990143664B774366F27FCE7FAF7FAE7F
      AE7FAE7FAE7F0A73210A810E000000000000000000000000000000000000153E
      5E573D4FFF6FFF6FFF7BFF7FFF7BFF77153E0000000000000000000080010312
      AA43020EFF6B2A1E84224837CA47893F041A00009901107E007C107EFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7BDF73BF6B99014366CF7F43662E776F776F77B27F
      D07FAD7FAD7F210A810E010A000000000000000000000000000000000000153E
      1D4B1E47FF6BFF6FFF77FF7BFF77FF73153E00000000000000000000E20D8A3F
      CA472737FF6BFF6FC2092737CB478A3F4A2200009901FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7BDF7399014366F17F8C7B4366436643660B73
      C005E105210A210AE105AE7F836A00000000000000000000000000000000153E
      3D4FFE467E5BDF67FF6FFF6FFF6FFF6F153E0000000000000000020E020E0212
      893F020E020E020EFF6BF346020E4A1E584A0000990199019901990199019901
      9901990199019901990199019901990199014366F07FF07FF07FCF7FCF7F4366
      43662D77927B7077927BD47FA76E00000000000000000000000000000000153E
      9F5F1D4B3D4F9E5BFF6FFF6FFF6FFF6F153E00000000000000000000BA52E10D
      683B020E5E539E5FFF6BFF6BD342FF6B00000000000099019901990199019901
      9901990199019901990199019901990100004366F17FCF7FCF7FCF7FD07FF07F
      CE7F436643664366436665664366000000000000000000000000000000000000
      3C5BFF7F9F5F1E471D4B3D4FDF631B5700000000000000000000000000007846
      0633A4221E4BFD421E4B5E57BF63774600000000000000000000000000000000
      0000000000000000000000000000000000004366907BF27FF17FF07F4C774366
      4366000000000000000000000000000000000000000000000000000000000000
      153E1B5BFF6B7E573D4F7E571B57153E00000000000000000000000000000000
      E10DA422C2094C229032081A784A000000000000000000000000000000000000
      0000000000000000000000000000000000000000436643664366436643660000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000153E153E153E153E0000000000000000000000000000000000000000
      000000007846994E784A00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016421642164216421642
      1642164216421642164216421642164200000000000016421642164216421642
      1642164216421642164216421642164200000000000000000000000000000000
      000000000000000000000000000000000000000000000000D541164216421642
      16421642164216421642164216421642000000000000984ECD7FCD7FCD7FCD7F
      CD7FCD7FCD7FCD7FCD7FCD7F0F771642000000000000984EBF6B9E637E5F7E5B
      5E575E533D4F3D4B3D4B3D4B5E4F164200000000000000000000000000000000
      C660C660C650C65000000000000000000000000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000984ECD7F3D323D323D32
      DC1DDC1DDC1D9C119C119C110F771642000000000000984EBF6B984E984E984E
      984E984E984E984E984E984E3D4F16420000000000000000000000000000E67C
      E67CE67CC670C670C6500000000000000000000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000B8520F77A56AA56AA56A
      A56AA56AA56AA56AA56AA56AA56A1642000000000000B852BF739E6B9E677E63
      7E5F5E5B5E573D4F3D4F3D4B3D4F1642000000000000000000000000E67CE67C
      E67CE67CE67CC670C670C650000000000000000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000B852DF739E6B9E679E63
      7E637E5B5E5B5E573D4F3D4F3D4B1642000000000000B852DF739E6B9E679E63
      7E637E5B5E5B5E573D4F3D4F3D4B1642000000000000000000000000E67C8C7D
      E67CE67CE67CE67CC670C650000000000000000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000D956FF7BBF739E6B9E6B
      9E677E637E5F7E637E633D4F3D4F1642000000000000D956FF7B984E984E984E
      984E984E984E984E984E984E3D4F16420000B901EF04EF04EF04EF048C7D8C7D
      E67CE67CE67CE67CE67CC650000000000000000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000DA56FF7F3F4B3F4B1F3F
      1F3F1F3FFF36FF36FF365E535E531642000000000000DA56FF7FDF77BF739E6B
      9E679E637E637E5F5E575E535E5316420000B9015E0A9901990199018C7D737E
      8C7DE67CE67CE67CE67CC650200120012001000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000FA5AFF7FDF7BDF77BF6F
      9E6B9E679E637E637E5F5E5B5E571642000000000000FA5AFF7FDF7BDF77BF6F
      9E6B9E679E637E637E5F5E5B5E57164200000000B9015E0ABC01BC01BC018C7D
      737E8C7DE67CE67CC660E005E005E0052001000000000000D541FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F1642000000000000FA5AFF7FFF7FDF7BDF77
      BF739E6B9E677E637E5F7E5B7E5B1642000000000000FA5AFF7F984E984E984E
      984E984E984E984E984E984E7E5B1642000000000000B9015E0ABC019901EF04
      8C7DE67CE67CE67C600660066006E005200100000000067E067E067EDE7BDE7B
      DE7BDE7BDE7BDE7BDE7BDE7BDE7B1642000000000000FB5AFF7F3F4B3F4B1F3F
      1F3F1F3FFF36FF36FF367E635E5B1642000000000000FB5AFF7FFF7FFF7FDF7B
      BF73BF6F9E6B9E677E637E635E5B16420000000000000000B9015E0AEF040000
      C109083B60066006600660066006E005200100000000067EC87A067EBD77BD77
      BD77BD77BD77BD77BD77BD77BD7716420000000000001B5BFF7FFF7FFF7FFF7F
      DF7BBF73BF6FBF6FBF6B7C63F85616420000000000001B5BFF7FFF7FFF7FFF7F
      DF7BBF73BF6FBF6FBF6B7C63F856164200000000000000000000B90100000000
      C109083B60066006600660066006E0052001067E067E067EC87A067E067E067E
      9C739C739C739C73163A5646364216420000000000001C5BFF7FFF7FFF7FFF7F
      FF7FDF77DF779E6B984A5646364216420000000000001C5BFF7F984E984E984E
      984E984EDF779E6B984A56463642164200000000000000000000000000000000
      C109083B60066006600660066006E0052001067E517B767B767B767B517B067E
      7B6F7B6F7B6F1863163AFF7FFF7F16420000000000001C5B0F77A56AA56AA56A
      A56AA56AA56A0452373ADD3A9D2618360000000000001C5BFF7FFF7FFF7FFF7F
      FF7FFF7FFF7B3C67373ADD3A9D26183600000000000000000000000000000000
      C109083B60066006600660066006E0052001067E067E067E767B067E067E067E
      5A6B5A6B5A6B1863163AFF7F164200000000000000001C5BCD7F3D323D323D32
      3D32DC1D9C11360D583E1F3B593A00000000000000003D5FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F5C67583E1F3B593A000000000000000000000000000000000000
      C109083B083B083B083B083B083B083B200100000000067E767B067E39673967
      3967396739671863163A1642000000000000000000001C5BCD7FCD7FCD7FCD7F
      CD7FCD7FCD7F0F77583E7942000000000000000000001C5BDF7BDF77DF77DF77
      DF77DF77DF773C67583E79420000000000000000000000000000000000000000
      C109C109C109C109C109C109C109C109C10900000000067E067E067E163A163A
      163A163A163A163A163A0000000000000000000000003D5B3D5B3D5B3D5B3D5B
      1C5B1C5B1C5BFA5A173E0000000000000000000000001C5B3D5B3D5B3D5B3D5B
      1C5B1C5B3D5BFA5A173E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A021A021A021A021A02
      1A021A021A021A021A021A021A0200000000000000001A021A021A021A021A02
      1A021A021A021A021A021A021A02000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001A02BF6BBF6B9F679F679F63
      7F637F5F7F5F7F5B7F5B7F5B7F5B1A02000000001A02DF77BF6FBF6BBF6B9F67
      9F639F639F637F5F7F5F7F5F9F631A0200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001A02BF6FBF6B192219221922
      192219221922192219227F5B7F5B1A02000000001A02DF73BF6B9F679F679F63
      9F5F7F5F7F5F7F5F7F5F5E5B7F5F1A0200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001A02DF73BF6FBF6FBF6B9F67
      9F679F639F637F5F7F5F7F5B7F5B1A02000000001A02DF77BF6FDC463A223A22
      392639263A229C325E577F5F7F5F1A0200000000A56AA56AA56AA5144B25A56A
      A56AA56AA56AA56AA56AA56AA56AA56A00000000000000000000000000000000
      00000000000000000000000000000000000000001A02DF73DF73BF6F5E571922
      19223A2A19221D537F637F5F7F5B1A02000000001A02FF7BBF739F633B1EFB05
      9C361D53DB11DA015B267F5F9F5F1A0200000000A56AD17FD17F4B2508218A31
      6E7B6E7B6E7BA56AA56A6E7B6E7BA56A000000001042712D712D712D712D712D
      712D712D712D712D0000000000000000000000001A02DF77DF73BF6F3A2A1A2E
      BF6BBF6BBB463A2A7F5F7F637F5F1A02000000001A02FF7BDF73BF6B7C2ADB11
      FD4ABF739C32DA01DB117F5F9F631A0200000000A56AD17FD17F6E7BCA3DE855
      433571296E7BA56AF57FA56A6E7BA56A000000001042FF77DF73DF6FBE6FBE6F
      BE6FBF6F9E6B712D0000000018000000000000001A02DF77DF77BF6B3A26DC4A
      BF6FBF6B7F5F3A2A5F5B9F637F631A02000000001A02FF7FDF77BF6B7C2E3B1E
      BC3E1D533A22DA019C329F639F631A0200000000A56AF37FD17FD17FCA3DC849
      F239D8197129A56AF57FF57FA56AA56A000000001042DF739E679E677E677E63
      7E639E677D67712D00001F001F001800000000001A02FF7BDF77BF6F3A26DB46
      BF6FBF6F7F5F1A2E7F5F9F679F631A02000000001A02FF7FDF77BF6F7C2E5B1E
      39263A22DA0139267F5F9F639F671A0200000000A56AF37FF37FD17F6E7BCF3D
      FE42BE2ED81D7129A56AA56AA56AA56A000000001042DF777F5B7F5B7F5B5F5B
      5F5B7F5F7D6792311F001F001F001F00180000001A02FF7BFF7BDF733A26DC4A
      DF73DF737F631A2E7F63BF6B9F671A02000000001A02FF7FFF7BBE739C325B1E
      DC463D573B1EDB11FD4A9F679F671A0200000000A56AF37FF37FF37FD17FF539
      5F571F479D2ED81D71296E7B6E7BA56A000000001042FF7B5F535F535F535F53
      5F535F539E6FF43900001F001F001800000000001A02FF7FFF7BDF773A26FC4A
      DF77DF739F671A2E9F67BF6BBF6B1A02000000001A02FF7FFF7FDE779C365B22
      1D53DF737C2EFB05BC3ABF6BBF6B1A0200000000A56AF37FF37FF37FF37FD17F
      F5395F571F47BE2ED81971296E7BA56A000000001042FF7FBF6F9F6B9F6B9F6B
      BF6BDF739D6F180018001F001F001800000000001A02FF7FFF7FFC4A1A1A3A2A
      9F67DF77BB461A1EDB46BF6FBF6B1A02000000001A02FF7FFF7F5E577C2A5B1E
      9C329C363B1E5B223D57BF6BBF6F1A0200000000A56AF37FF37FF37FF37FF37F
      D17FF5395F571F479D2EB61D7129A56A000000001042FF7F5F535F535F535F53
      7D6F1F001F001F001F001F001F001800000000001A02FF7FFF7FFF7FFF7FFF7B
      FF7BDF77DF77DF73DF73BF6FBF6F1A02000000001A02FF7FFF7F3D571D4F1D53
      1D4FDC461D4F5E5BBF6FBF6BBF731A0200000000A56AF37FF37FF37FF37FF37F
      F37FD17FF5395F575352A67A4054004C000000001042FF7F9F6B9F6B9F6B9F6B
      36463646364636460000000000000000000000001A02FF7FFF7FFF7FFF7FFF7F
      FF7BFF7BDF77DF77DF77DF73BF6F1A02000000001A02FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FDF77DF77FF7B1A0200000000A56AA56AA56AA56AA56AA56A
      A56AA56AA56AF5396976A268A2684054004C00001042FF7FFF7FFF7FFF7FFF7F
      3646DD36383E000000000000000000000000000000001A021A021A021A021A02
      1A021A021A021A021A021A021A0200000000000000001A021A021A021A021A02
      1A021A021A021A021A021A021A02000000000000000000000000000000000000
      000000000000000060602D7EA27DA26800000000104210421042104210421042
      1042104200000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000060606060000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010111011
      1011101110111011000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001011101199019901
      9901990199019901101110110000000000000000000000000000000000000000
      0000000000000000000000000000000000000000370137013701370137013701
      370137013701370137013701370137013701000000001A021A021A021A021A02
      1A021A021A021A021A021A021A020000000000000000330D9801990199019901
      990199019901990199019801101100000000000000000000000000000000543E
      153E9331D72500000000000000000000000000003701FF7FFF7FFF7BBF6F9F67
      7F5B5F533F4B3F4B3F4B3F4B3F4B3F4B370100001A02DF77BF6FBF6BBF6BBF6B
      9F679F639F5F7F5F7F5F7F5F9F631A0200000000330D98019801980198019801
      98019801990199019901990198011011000000000000000000000000F431971D
      982198215321D0350000000000000000000000003701FF7FFF7FFF7FFF7BBF6F
      9F677F5B5F533F4B3F4B3F4B3F4B3F4B370100001A02DF73BF6BBF6B9F639F63
      9F637F5F7F5B7F5B7F5B5F5B7F5F1A02000000007705980198019801D90D3A22
      7B2E7B2E1A1E9901990199019901101100000000000000000000D725780D140D
      5111160D5715991D1019000000000000000000003701FF7F457D457D457DFF7B
      B100B100B1005F53005E005E005E3F4B370100001A02DF77BF6FBF6F1D4F1B22
      1A1ADC427F5F7F5F7F5B7F5B7F5F1A0200005409B901B901B9017B2EFF7FFF7F
      FF7FFF7F1D4F980199019901990199011011000000000000D7255709F4001501
      86012D01F500F404771D4E2100000000000000003701FF7F457D457D457DFF7F
      B100B100B1007F5B005E005E005E3F4B370100001A02FF7BBF6FBF6FBF6BBB42
      1A0E3E579F639F5F7F5F7F5B9F5F1A0200005509D909D909D90DFF7F1D4BF915
      D90DD909770598019801990199019901101100000000D725532115017901B701
      E001C401780136018E1170194E210000000000003701FF7F457D457D457DFF7F
      B100B100B1009F67005E005E005E3F4B370100001A02FF7BDF73BF6FDF6F5E57
      1A12FD46BF6B9F637F5F7F5F9F5F1A02000055091A1A1A1AF915FF7F1A1EF915
      D909B9011D4F78019801990199019901101100000000D72553217901FA013002
      47021802F701C401A105AC15D7250000000000003701FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7BBF6F9F677F5B5F533F4B370100001A02FF7FDF73DF73DF739F67
      1B22BC429F679F639F637F5F9F631A02000055099B325B2A3A22FF7FDC421A1E
      F915D909FF7F1D4B9801990199019901101100000000D7255321D801850EA612
      BA0E7F063C020602C001A809AC150000000000003701FF7F174217421742FF7F
      9B019B019B01FF7B0002000200025F53370100001A02FF7FDF77DF73DF73DF73
      DC42FB259F63BF6B9F639F639F671A0200005509DC42DC3E5B2A5E5FFF7F9E6B
      9E6BBE6FFF7FFF7F3D579901990199011011000000005321D7254F0602272633
      2F373F2F9F0A2D020002A501A5010000000000003701FF7F174217421742FF7F
      9B019B019B01FF7F0002000200027F5B370100001A02FF7FFF7BDF77DF73DF77
      1D531B225E57BF6B9F679F639F671A0200005509DC3E5E5FDC3E9C361D4F5E5F
      3D577E67FF7FFF7F3D53990199019901101100000000543ED725E61E483B944F
      D45B9A4BDA1636020602EB15543E0000000000003701FF7F174217421742FF7F
      9B019B019B01FF7F0002000200029F67370100001A02FF7FFF7BDF77DF77DF77
      9F67FB251D4FDF6FBF6B9F67BF6B1A020000000076097E637E63FC469C367B2E
      3A221A1AFF7FFD4699019901990110110000000000000000543E103B6C3FD757
      FA5F6B43C71A3E021806543E00000000000000003701FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7BBF6F370100001A02FF7FFF7FFF7BDF77DF77
      7D67FB25FB255E5BBF6FBF6BBF6B1A02000000005509DC429E6B7E67FC467B2E
      3A221A1AFD46D90DB90199019801101100000000000000000000543E15475443
      72472A33C91E7A22543E00000000000000000000370137013701370137013701
      37013701370137013701370137013701370100001A02FF7FFF7FFF7FFF7BFF7B
      5C63FD4ADC465E57DF6FBF6BDF6F1A020000000000005509FC467E679E6B3D57
      FC46BC3A7B2E3A22D9099801110D0000000000000000000000000000543E543E
      153E543E543E543E000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000001A02FF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FDF77DF77FF7B1A02000000000000000055097609FD463D53
      3D53FD469B32F915760911110000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001A021A021A021A021A02
      1A021A021A021A021A021A021A02000000000000000000000000000055097609
      760976097609330D000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000010111011
      1011101110111011000000000000000000000000000000000000B90100000000
      0000F304F304F304F304F3040000000000000000000000000000000000001C5B
      1C5B1C5BFA560000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001011101199019901
      9901990199019901101110110000000000000000000000000000B901B901B901
      B901F3047F4A9D2E1B16F304000000000000000000000000000000001C5B9E73
      596B596B9E73FA56000000000000000000000000000000000000000020018001
      2001000000000000000000000000000000000000000033099801990199019901
      9901990199019901990198011011000000000000000000000000B90100000000
      0000F304F304F304F304F30400000000000000000000000000001C5B9E73FF7F
      1A2EB829F75E9E73FA5600000000000000000000000000000000000080016112
      8001200100000000000000000000000000000000330D98019901980198019801
      9801990199019901990199019801101100000000000000000000B90100000000
      0000000000000000000000000000000000000000000000001C5B9E73FF7F1A2E
      B829B725B829F75E9E73FA560000000000000000000000000000000080016212
      6112800120010000000000000000000000000000770198019801980177013A1E
      5B2A7B2E3A22F911990199019901101100000000000000000000B90100000000
      0000F304F304F304F304F304000000000000000000001C5B9E73FF7F1A2EB829
      BF73BF739F6BB725F75E9E73FA56000000000000000000000000000080018216
      8216611280012001000000000000000000005509B805B805B805980198011D4F
      DF7BDF7BDF7BDF7B7B2E99019901990110110000000000000000B901B901B901
      B901F3047F4A9D2E1B16F30400000000000000001C5B9E73FF7F1A2EB829B829
      B829BF73B829B829B725F75E9E73FA560000000000000000000000008001C21E
      8216611261128001200100000000000000005509B909D90DD90DD90DD90DD90D
      D90DD90DD90DFC46DF7B99019901990110110000000000000000B90100000000
      0000F304F304F304F304F30400000000000000005D63FF7F1A2EF929B829B829
      B829BF73B829B829B829B725F75EFA560000000000000000000000008001C326
      A21EA21EA21E82168001400100000000000055091A1AF915F915F915F9153D57
      D90DB8057701B909DF7B99019901990110110000000000000000B90100000000
      00000000000000000000000000000000000000005D63FF7F9F679C3AF929B829
      B829BF73B829B829B8291A2E596BFA560000000000000000000000008001EA2A
      EB2AE92AE92AE92A8001400100000000000075097B325B2A3A1E3A225D5BDF7B
      F915D90DB8057B2EDF7B99019901990110110000000000000000B90100000000
      0000F304F304F304F304F30400000000000000001C5B9E73FF7F9F679C3AF929
      BF73BF73B829B8291A2EBC779E73FA5600000000000000000000000080010B2F
      0D370D33E92A8001200100000000000000005509DC42BC3E5B2A7E67DF7BDF7B
      BF739E677E67DF7BFC4699019901990110110000000000000000B901B901B901
      B901F3047F4A9D2E1B16F304000000000000000000001C5B9E73FF7F9F679C3A
      F929BC42B8291A2EFF7F9E731C5B000000000000000000000000000080010D37
      4F3F4F3F80012001000000000000000000005509DC425E5BBC3E9E67DF7BDF7B
      9E673D571D537B32980199019901990110110000000000000000B90100000000
      0000F304F304F304F304F3040000000000000000000000001C5B9E73FF7F9F67
      9C3ABF731A2EFF7F9E731C5B0000000000000000000000000000000080010C33
      514780012001000000000000000000000000000075097E637E63FC465E5FDF7B
      3A221A1AF911B9099901990199011011000000000000F304F304F304F304F304
      00000000000000000000000000000000000000000000000000001C5B9E73FF7F
      9F671A2EFF7F9E731C5B0000000000000000000000000000000000008001A71E
      80012001000000000000000000000000000000005509DC429E6B7E67FC465D5B
      3A221A1AF911D90DB805990198011011000000000000F3047F4A9D2E1B16F304
      000000000000000000000000000000000000000000000000000000001C5B9E73
      FF7FFF7F9E731C5B000000000000000000000000000000000000000020018001
      200100000000000000000000000000000000000000005509FC467E679E6B5D5B
      FC469B367B323A22B9099801310D0000000000000000F304F304F304F304F304
      0000000000000000000000000000000000000000000000000000000000001C5B
      1C5B1C5B1C5B0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000055097609FC463D53
      3D53FC467B32F915760911110000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000075097609
      7609760975093309000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C16D00000000000000000000000000000000
      0000000000000000000000000000000000000212021202120212021202120212
      0212021202120212021200000000000000000212021202120212021202120212
      0212021202120212021200000000000000000000000000000000000000000000
      00000000000000000000A069E371A069A0690000990199019901990199019901
      9901990199019901990199019901990199010212021202120212021202120212
      0212021202120212021200000000000000000212021202120212021202120212
      02120212021202120212000000000000000000000000237B237B000000000000
      0000000000000000A069457AA57EA069000000009901FF7FFF7FFF7BDF73BF6B
      9F635F575F575E575F575F575F575F5799010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000237B237B00000000
      A069A069A069A069447A857EA0690000000000009901FF7F1763176317631763
      17631763176317631763176317635F5799010000000084550000000000000000
      8001800180018001800180010000000000000000000000008355000000000000
      800180018001800180018001000000000000000000000000000000000000C16D
      C16D447A297B297B2476A06900000000000000009901FF7FFF7FA5144B25B556
      DF739F639F635F575F575E575F575F5799010000000084558455000000000000
      8001C82EA6226316421280010000000000000000000083558355000000000000
      8001C632842243162212800100000000000000000000000000000000822DC26D
      857E0376657EA57E657EA06900000000000000009901FF7F17634B2508218A31
      734E1763176317631763176317635F57990184558455486E486E845500000000
      80018001800180018001800100000000000000008355EB5EEB5E835583550000
      8001800180018001800180010000000000000000000000000000210A6316822D
      447A297BE3710376657AA06900000000000000009901FF7FFF7FFF7FCA3DE855
      43357129B5569F639F635F575F575F5799018455355B124F486E486E84550000
      000000000000000000000000000000000000835576772F63EB5EEB5E83550000
      000000000000000000000000000000000000000000000000C005420EC93EE52A
      822D0376657EC26D0372A06900000000000000009901FF7F17631763CA3DC849
      F239D8197129734E17631763D55E5F57990184558455486E0E43845500000000
      800180018001800180018001800180018001000083555873EB5E835583550000
      800180018001800180018001800180018001000000000000E105062F831A0633
      420E822DE371657AC16DA06900000000000000009901FF7FFF7FFF7FFF7FCF3D
      FE42BE2ED81D7129B5569F635F575F5799010000000084558455000000000000
      8001EA32C82EC82EC72A851E63164212C0050000000083558355000000000000
      8001C632C632A62EA62E8422641A2212A005000000000000E105062F6316210A
      E005E001822DC26DA069000000000000000000009901FF7F176317631763F539
      5F571F479D2ED81D7129734E17635F5799010000000084550000000000000000
      8001800180018001800180018001800180010000000000008355000000000000
      800180018001800180018001800180018001000000000000E105C52663160006
      E001E001E001822D0000000000000000000000009901FF7FFF7FFF7FFF7FFF7F
      F5395F571F47BE2ED8197129B5565F5799010000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000420E210A010AE001
      E001A001A0010000237B237B00000000000000009901FF7FFF7FFF7FFF7FFF7F
      FF7FF5395F571F479D2EB61D7129734E99010212021202120212021202120212
      0212021202120212021200000000000000000212021202120212021202120212
      021202120212021202120000000000000000000000006316062FC005A001A001
      A001A001000000000000237B237B000000000000990199019901990199019901
      99019901F5395F575352A67A4054004C99010212021202120212021202120212
      0212021202120212021200000000000000000212021202120212021202120212
      0212021202120212021200000000000000000000E209062F010AA00100000000
      0000000000000000000000000000000000000000000099019901990199019901
      990199019901F5396976A268A2684054004C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A001A001C001A001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000060602D7EA27DA26800000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004212A00100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000060606060000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016421642164216421642
      1642164216421642164216421642164200000000000016421642164216421642
      1642164216421642164216421642164200000000000000000000000000000000
      00000000000000000000000000000000000000001046A64D574A000000000000
      00000000000000000000000000000000000000000000984EBF6B9E637E5F7E5B
      5E575E533D4F3D4B3D4B3D4B5E4F1642000000000000984EBF6B9E637E5F7E5B
      5E575E533D4F3D4B3D4B3D4B5E4F164200000000000000000000000000000000
      00000000000000000000000000000000000000002962256E0C4E374A00000000
      00000000000000000000000000000000000000000000984EBF6B104210421042
      1042104210421042104210423D4F1642000000000000984EBF6B062BE526821A
      821A821A200620062006A0053D4F164200000000000000000821000000000000
      0000000000000000000000000000000000000000C97EEC7E256EE465574A0000
      00000000000000000000000000000000000000000000B852BF739E6B9E677E63
      7E5F5E5B5E573D4F3D4F3D4B3D4F1642000000000000B852BF739E6B9E677E63
      7E5F5E5B5E573D4F3D4F3D4B3D4F164200000000000000000000000000000000
      00000821000008210000000000000000000000000000C97EC97E256E0C4E574A
      000000000000000000000000000000000000C0050000B852DF739E6B9E679E63
      7E637E5B5E5B5E573D4F3D4F3D4B1642000000000000B852DF739E6B9E679E63
      7E637E5B5E5B5E573D4F3D4F3D4B164200000000000000000000082100000000
      000000000000000000000000000000000000000000000000C97EEC7E256EE461
      000000000000000000000000000000000000C005C005C005FF7B806580658065
      8065806580658065806580653D4F1642000000000000D956FF7B8E7E20792079
      2079207920792079207920793D4F164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C97EC97E256E
      00000000153E153E153E153E000000000000C005C005C005C005C27FC27FC27F
      C27FC27FC27FC27FC27FC27F3D4F1642000000000000DA56FF7FDF77BF739E6B
      9E679E637E637E5F5E575E535E53164200000000000000001042000008210000
      0000000008210000082100000000000000000000000000000000000000000000
      574A1C53FF6FFF6FFF6FFF6F3C5B153E00002006200620062006C27FC27FC27F
      C27FC27FC27FC27FC27FC27F7E5B1642000000000000FA5AFF7FDF7BDF77BF6F
      9E6B9E679E637E637E5F5E5B5E57164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      1C53DF63FF6FFF6FFF73FF73FF7F3C5B0000200620062006FF7F806580658065
      8065806580658065806580657E5B1642000000000000FA5AFF7F062BE526821A
      821A821A200620062006A0057E5B164200000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000153E
      5E573D4FFF6FFF6FFF7BFF7FFF7BFF77153E20060000FB5AFF7FFF7FFF7FDF7B
      BF73BF6F9E6B9E677E637E635E5B1642000000000000FB5AFF7FFF7FFF7FDF7B
      BF73BF6F9E6B9E677E637E635E5B164200000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000153E
      1D4B1E47FF6BFF6FFF77FF7BFF77FF73153E000000001B5BFF7FFF7FFF7FFF7F
      DF7BBF73BF6FBF6FBF6B7C63F85616420000000000001B5BFF7FFF7FFF7FFF7F
      DF7BBF73BF6FBF6FBF6B7C63F856164200000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000153E
      3D4FFE467E5BDF67FF6FFF6FFF6FFF6F153E000000001C5BFF7F104210421042
      10421042DF779E6B984A5646364216420000000000001C5BFF7F8E7E20792079
      20792079DF779E6B984A56463642164200000000000000000000000008210000
      000000000000000000000000000000000000000000000000000000000000153E
      9F5F1D4B3D4F9E5BFF6FFF6FFF6FFF6F153E000000001C5BFF7FFF7FFF7FFF7F
      FF7FFF7FFF7B3C67373ADD3A9D2618360000000000001C5BFF7FFF7FFF7FFF7F
      FF7FFF7FFF7B3C67373ADD3A9D26183600000000000000000000000000000821
      0000082100000000000000000000000000000000000000000000000000000000
      3C5BFF7F9F5F1E471D4B3D4FDF631B570000000000003D5FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F5C67583E1F3B593A00000000000000003D5FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7F5C67583E1F3B593A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      153E1B5BFF6B7E573D4F7E571B57153E0000000000001C5BDF7BDF77DF77DF77
      DF77DF77DF773C67583E7942000000000000000000001C5BDF7BDF77DF77DF77
      DF77DF77DF773C67583E79420000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000153E153E153E153E000000000000000000001C5B3D5B3D5B3D5B3D5B
      1C5B1C5B3D5BFB5A173E0000000000000000000000001C5B3D5B3D5B3D5B3D5B
      1C5B1C5B3D5BFA5A173E00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000F439F439
      F439F439F439F439F439F439F439712D000000000000804D804D804DF439F439
      F439F439F439F439F439F439F439913100000000183A16421642164216421642
      16421642164216421642164216421642000000000000000000000000F500B100
      0000000000001401B100000000000000000000000000000000000000F5399F67
      9F637F637F5F7F5F5F5B5F5B3E53712D00000000804D097309730973163ABE6F
      DF73BF73BE6FBE6FBE6FBE6FBE6F91310000984EFF7FFF7FFF7FFF77FF77FF77
      DF6FDF6FBF6BBF6BBF6B7F5B604C5F5316420000000000000000780114011401
      B10000001401F500F500B10000000000000000000000000000000000F5399F6B
      9C2A9C2A9C2A9C2A9C2A9C2A5F5B712D00000000804D097309730973163A9E6F
      9C2E9C2E9C2E9C2E9C2E9C2E9E6B91310000984EFF7FFF7FFF7FFF7FFF7FFF77
      FF77FF77DF6FDF6FBF6BBF6B604C7F5F16420000000000000000F50000000000
      B1000000F50000000000B10000000000000000000000000000000000163EBF73
      BF6F9F679F679F637F5F7F5F5F5B712D00000000804D097709730973173ABE6F
      9E6B7D677D677D677D637D679E6B91310000B852FF7FFF7FFF7F9B3E183AFF7F
      9B3E183AFF779B3E646464646464604C604C0000000000000000F500B1000000
      B1000000F50000007801B100000000000000F439F439F439F439F439373EDF73
      9C2A9C2A9C2A9C2A9C2A9C2A7F5F712D00000000804D2A772A770973173ABF73
      9C2E9C2E9C2E9C2E9C2E9C2E9E6B91310000B852FF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF77FF77DF6F6464BF6B164200000000000000007801F500F500
      F5005442F500F500F500B100000000000000F5399F679F637F637F5F5742DF77
      DF73BF73BF6F9F6B9F679F637F63712D00000000804D4B772A772A77383ADF77
      BE6F9E6B9E6B9E6B9E679E6BBE6F91310000DA56FF7F183AFF7FFF7F9D73F218
      91109110942D91103C63FF776464BF6B16420000000000000000000078011401
      330D9125F5001401B1000000000000000000F5399F6B9C2A9C2A9C2A5742FF7B
      9C2A9C2A9C2A9C2A9C2A9C2A9F67712D00000000804D4C7B4C7B4B775836DF77
      9C2E9C2E9C2E9C2E9C2E9C2EBF7391310000DA56FF7F9B3EFF7FFF7FF73D1321
      FF7F9D7353299110FF7FFF779B3EBF6B16420000000000000000000000000000
      D1357C6BD135000000000000000000000000163EBF73BF6F9F679F67BB42FF7F
      FF7BDF7BDF77DF73BF73BF6F9F6B712D00000000804D6D7B6D7B4C7B5836FF7B
      DF7BDF73BF73BF73BF73DF73BE6F91310000FA5AFF7FFF7FFF7FFF7F38469110
      594AFF7FFF7F9110FF7FFF7FFF77DF6F1642000000000000000000000000D135
      7C6B133E3A5FD13500000000000000000000373EDF739C2A9C2A9C2ABB42FF7F
      FF7FFF7BFF7BDF77BF6F1D535742712D00000000804D6E7F6E7B6D7B9A46FF7B
      FF7FFF7FFF7FFF7F163A163A163A163A0000FA5AFF7F183AFF7FFF7FFF7FB635
      F218911091109110FF7FFF7F183AFF771642000000000000000000000000133E
      7C6B133E3A5F133E000000000000000000005742DF77DF73BF73BF6FDC46FF7F
      FF7FFF7FFF7FFF7B163A163A163AF63900000000804D8F7F8F7F6E7B9A46FF7F
      FF7FFF7FFF7FFF7F163A9B3A9C2E00000000FA5AFF7F9B3EFF7FFF7F9D73594A
      FF7FFF7F5C6B9110FF7FFF7F9B3EFF77164200000000000000000000D1357C6B
      D1350000EF393A5FD13500000000000000005742FF7B9C2A9C2A9C2ADC46FF7F
      FF7FFF7FFF7FFF7F163ABC3A9C2A000000000000804D8F7F8F7F8F7F9A469B42
      9B429B429B429B42163AB84E804D000000001C5BFF7FFF7FFF7FFF7F9D739110
      F73D594AB6359110FF7FFF7FFF7FFF771642000000000000000000003A5FD135
      000000000000D1353A5F0000000000000000BB42FF7FFF7BDF7BDF77DC46BB42
      BB42BB42BB42BB42163ABD2E0000000000000000804D917F8F7F8F7F8F7F8F7F
      6E7F6D7B4C7B4B772A772A77804D000000001C5BFF7F183AFF7FFF7FFF7FFB5A
      5329F2181321FB5AFF7FFF7F183AFF7716420000000000000000D1357C6B133E
      000000000000133E3A5FD135000000000000BB42FF7FFF7FFF7BFF7BDF77BF6F
      1D535742712D0000000000000000000000000000804D917F917FCE39CE39CE39
      CE39CE39CE39CE394C7B4B77804D000000001C5BFF7F9B3EFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7F9B3EFF7F164200000000000000003A5FD1350000
      0000000000000000EF393A5F000000000000DC46FF7FFF7FFF7FFF7FFF7B163A
      163A163AF6390000000000000000000000000000804D917F917FCE391A5F1A5F
      1A5F1A5FF95ACE396D7B4C7B804D000000001C5BFF7FFF7FFF7F9B3E183AFF7F
      9B3E183AFF7F9B3E183AFF7FFF7FFF7F16420000000000000000D13500000000
      00000000000000000000EF39000000000000DC46FF7FFF7FFF7FFF7FFF7F163A
      BC3A9C2A000000000000000000000000000000000000804D804DCE397D63FF7F
      FF7BFF7B1A5FCE39804D804D0000000000001C5BFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7FFF7F183A0000000000000000000000000000
      000000000000000000000000000000000000DC46BB42BB42BB42BB42BB42163A
      BD2E0000000000000000000000000000000000000000000000000000CE39CE39
      CE39CE39CE3900000000000000000000000000001C5B1C5B1C5B1C5B1C5B1C5B
      FA5AFA5ADA56DA56B852B852984E984E00000000000000000000104210420000
      0000104210420000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C06CC06C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000001042F85ED75A1042
      10421042BD771042104210420000000000000000C06CC06C0000000000000000
      000000000000000000000000C06CC06C00000000000010011001EE00CD00CC00
      CD00CD00CD00CD00CD00CD00AB00000000000000000010011001EE00CD00CD00
      CD00CD00CD00CD00CD00CD00AB00000000000000000010429C73D75AD75AD75A
      4A292925104239679D7319631042104210420000C06CC06CC06C000000000000
      00000000000000000000C06CC06C000000000000550199019901760176015501
      5501550155015501550176013201AB000000000055019A017701770176015601
      5501550155015501550176013201AB000000000010427C6F5B6B955295529552
      6B2D42084208630C420832423967104200000000C06CC06CC06CC06C00000000
      0000000000000000C06CC06C00000000000000009901DC01BB01BA019A019901
      7601760176017601760199017601CD00000000009A01DC01BB01BA019A019A01
      7701770177017701770177017601CD00000010425B6B5B6B95527C6F7B6F3967
      F85ED75A9552CE394208420842081042324200000000C074C06CC06CC06C0000
      000000000000C06CC06C000000000000000000009A01DD01DC01BB01BA015C22
      7C267C265C269A0D760176015501CD0000000000BA01DD01DC01BB011D125D22
      9E2E5D225D227701770177015501CD00000010425B6B9552DE77DE77DE779D73
      9D739C737B6F3967F85E9552CE39324211420000000000000000C06CC06CC06C
      0000C06CC06CC06C000000000000000000000000BA01FD09FD05DC01BB013F4F
      FF7FFF7FFF7FFF7F7C2676015501CD0000000000BA01FD09FD059E2EFF7BFF7B
      FF7BFF7B3F4F7701770177015501CD00000010429552DE77DE77DE777C6FB556
      B656F85E3A677C6F9C735A6B19639552104200000000000000000000C06CC070
      C070C074C06C0000000000000000000000000000BA013D1A1D0EFD05DC01DC01
      FD099A0D9A0DFE46FF7F76015501CD0000000000BA013D1A1D0EFF7B1E471D0E
      1D0E9B09BA017701770177015501CD000000000010421042BE77F85E32429552
      B5569552B656B556B65639677B6F3A671042000000000000000000000000C070
      C070C07400000000000000000000000000000000BA019D2E3D16FD09FD053F4F
      DC01BB01BA019A05FF7F99015601CD0000000000BA019E2E3D16FF7B1D0EDD01
      DC01DB013F4F9A019A0177015601CD0000000000000000001042794A77463342
      1242104232429552D75A5B6B3A671042000000000000000000000000C06CC074
      C074C074C0780000000000000000000000000000BA01DE3A5D1E1D0E3F4FFF7F
      DD01DC01BB017D2AFF7F99017601EE0000000000BA01DE3A5D1AFF7BBE32FD01
      DD01DC01FF7B1F4BBA019A017701EE0000000000000000000000794E9F677F5B
      5E5B784A784A794E334210421042000000000000000000000000C078C074C074
      00000000C078C07C000000000000000000000000BA01FE3E5C227F5FFF7FFF7F
      BF6B9F639F63FF7FFE46BB019901100100000000BA01FE3E5D223F4FFF7B9F67
      9F67BF6FFF7BFF7B5F5BBA017701100100000000000000000000794E9F677F5F
      7F5B7F5B7E5B794E00000000000000000000000000000000C07CC074C07C0000
      000000000000C07CC07C00000000000000000000BA01FE427D2A7F5BFF7FFF7F
      9F633F4F3F4F9D2EDC01DC019901120100000000BA01FE427D265D1ADE3A3F4F
      3F4F7F63FF7BFF7B5F57DC019A0132010000000000000000794EBF6F9F679F63
      7F5F7F5B794E00000000000000000000000000000000C07CC07CC07C00000000
      0000000000000000C07CC07C0000000000000000BA011F4B9D325D1E3F53FF7F
      1D0E1D0EFD05FD01DD01DD01BA01330100000000BA011F4BBE325D1E5D1A3D12
      1D0E1D0EFF7B1E47DD01DD01BA0133010000000000000000794EBF73BF6F9F67
      9F637F5B794E0000000000000000000000000000C07CC07CC07C000000000000
      000000000000000000000000C07C000000000000BA013F4F1E4BFE3EBE365F57
      5C263D1A1D0EFD05FD01FD01DC01550100000000BA011F4F1F47DE3EDE369E2E
      7D265D1A1E47FD05FD01FD01DB015501000000000000794EDE77DE77BF73BF6B
      9F675E5B794E000000000000000000000000C07CC07CC07C0000000000000000
      0000000000000000000000000000000000000000BA01FE3E3F4F3F531F4BFE42
      DE3A5C263D16FD05FD05FD05DC01760100000000BA01DE3E3F4F3F4F1F4BFE42
      DE367D263D12FD05FD01FD01DD017601000000000000794E794E794EDF77BF6F
      9F67794E0000000000000000000000000000C07CC07C00000000000000000000
      00000000000000000000000000000000000000000000FD093D1A3D1A3D1A3D16
      1D12FD099A05BB01BA01BA01990100000000000000009B093D1A3D1A3D1A3D16
      1D129B099B05BA01BA01BA019A010000000000000000000000000000794E794E
      794E000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000016421642164216421642
      1642164216421642164216421642164200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000205E205E205E205E205E205E
      205E205E205E205E205E205E205E0000000000000000994EBF6F9E677E5F7E5B
      5E575E533D533D533D533D535E531642000000000000121D121D784E784E784E
      784E784E784E784ED214121D0000000000000000752D752D1C631C6338633863
      38633863752DD21800000000000000000000205E63560B77907F2C7B2C7B2C7B
      2C7B2C7B2C7B2C7B2C7BE86A205E0000000000000000994EBF6F9E677E637E5F
      7E5B5E575E533D533D533D535E53164200000000121D993198319C73B214B214
      9C739C739C733967B2141321121D00000000F739782D782D1C63F31CD6393863
      DE7BDE7B782DD21800000000000000000000205EE96EE86AD37F4E7F4E7F4D7B
      4E7F4E7F4E7F4E7F4D7BE96E707F205E000000000000B94EDF779E6B9E677E63
      7E5F7E5B5E575E533D533D535E53164200000000121D993198319C73B214B214
      9C739C739C733967B2141321121D00000000F739782D782D5846F31C33251C63
      3863DE7B782DD2180000D218000000000000205E4E7F205ED57F6F7F6F7F6F7F
      6F7F6F7F6F7F6F7F6F7FE96ED57F205E000000000000B952DF77BF6F9E6B9E67
      7E637E5F7E5B5E575E533D535E53164200000000121D993198319C73B214B214
      9C739C739C733967B2141321121D00000000F739782D782D5846584697529752
      1C633863782DD218782DD218000000000000205E6F7F425AD37FB27F907F907F
      907F907F907F907F907FE96ED67F205E000000000000B956FF7BDF77BF6F9E6B
      9E677E637E5F7E5B5E575E535E53164200000000121D993198319C739C739C73
      9C739C739C733967B2141321121D00000000F7395525762D782D782D782D782D
      782D782D782DD218782DD2180000D2180000205E907FE96E0B73D57FB17FB17F
      B17FB17FB17FB17F010EE96ED67F2C7B205E00000000BA56FF7FDF77DF77BF6F
      9E6B9E677E637E5F7E5B5E575E53164200000000121D993198319931D839D839
      B9359831B935993198319931121D00000000F7395525DF7BDF7BDF7BDF7BDF7B
      DF7BDF7B782DD218782DD218782DD2180000205E917F6E7F6356DB7FD97FD97F
      D97FD97FD97F010EE63A010EDB7FDA7F205E00000000FC56FF7FFF7BDF77BF6F
      BF6F9E6B9E677E637E5F7E5B5E57164200000000121D9731F83D794EB956B956
      B95699529952B956B9569931121D00000000F7395525DF7BDF7BDF7BDF7BDF7B
      DF7BDF7B782DD218782DD218782DD2180000205EB27FB27F425A205E205E205E
      205E205E010E2843294F073B010E205E205E00000000FC56FF7FFF7FFF7BDF77
      BF6FBF6F9E6B9E677E637E5F7E5B164200000000121D9931FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F9931121D00000000F7395525DF7BDF7BDF7BDF7BDF7B
      DF7BDF7B782DD218782DD218782DD2180000205ED37FD37FD37FD37FD37FD37F
      D37F010E284329472947294F073B010E000000000000FC56FF7FFF7FFF7FFF7B
      DF77BF6FBF6F9E6B9E679E677E5F164200000000121D9931FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F9931121D00000000F7395525DF7BDF7BDF7BDF7BDF7B
      DF7BDF7B782DD218782DD218782DD2180000205EDB7FD47FD47FD47FD47FD47F
      010E010E010E010E29472843010E010E010E000000001C57FF7FFF7FFF7FFF7F
      FF7BDF77BF6FBF6FBF6F7E63B952164200000000121D9931FF7F396739673967
      3967396739673967FF7F9931121D00000000F7395525DF7BDF7BDF7BDF7BDF7B
      DF7BDF7B782DD218782DD218782DD21800000000205EDB7FD47FD47FD47F205E
      E96EE96EE96E010E2843E63A010E00000000000000001C57FF7FFF7FFF7FFF7F
      FF7FFF7BDF779E67984A58463742164200000000121D9931FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F9931121D00000000F73955255A6BD95AD95AD95AD95A
      D95A5A6B5525D218782DD218782DD218000000000000205E205E205E205E0000
      000000000000010E073F010E000000000000000000003D57FF7FFF7FFF7FFF7F
      FF7FFF7FFF7F7E63383A7B2A7B2A383A00000000121D9931FF7F396739673967
      3967396739673967FF7F9931121D0000000000000000F7395525DF7BDF7BDF7B
      DF7BDF7BDF7BDF7B782DD218782DD21800000000000000000000000000000000
      00000000010EE63AE63A010E000000000000000000003D57FF7FFF7FFF7FFF7F
      FF7FFF7FFF7F9E6778463D535936000000000000121D9931FF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F9931121D0000000000000000F73955255A6BD95AD95A
      D95AD95AD95A5A6B5525D218782DD21800000000000000000000000000000000
      00000000010EE63A010E0000000000000000000000003D57FF7BFF7BFF7BFF7B
      DF77DF77DF779E675846784600000000000000000000121DFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F121D0000000000000000000000000000F7395525DF7B
      DF7BDF7BDF7BDF7BDF7BDF7B782DD21800000000000000000000000000000000
      010E010E010E010E00000000000000000000000000003D573D573D573D573D57
      3D573D573D57FC56173E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F73955255A6B
      D95AD95AD95AD95AD95A5A6B5525D218000000000000000000000000010E010E
      010E010E0000000000000000000000000000424D3E000000000000003E000000
      2800000040000000E00000000100010000000000000700000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C001000000000000
      C001000000000000C001000000000000C001000000000000C003000000000000
      C007000000000000C00F000000000000FF870000FFFFC001C08700008007C001
      800700000003C0010000E0070003C0010000E0070003C0010000E0070003C001
      0000E0070003C0010007E007000380010007E007000380010007E00700038001
      003FE00700068001003FFFFF83FE8001003FF81F83E48001003FF81FFFF1C003
      807FF81FFFF1C007C0FFFFFFFFF0C00FFFFFFE3F800380070000E00700010003
      0000E00700010001000080030001000100008001000100010000800100010000
      0000800100010000000080010001000000008001000700070000C00300070007
      0000E007800F800F0000F81FC3FFC3FF0000F81FFFFFFFFF0000F81FFFFFFFFF
      8001F81FFFFFFFFFFFFFFC3FFFFFFFFFFFFFB6DBFFFF9FFF8000AAAB8FFF0FFF
      8000AAAB87FF07FF8000B6DB83FF83FF80000FF7C1FF81FF80000023E1FFE107
      80000001F187E00380000003FE01F80180000003FE01F80180000007FC00F800
      80000007FC00F80080000003FC00F00080000003FC00F801C0010003FE01FC01
      FFFF00FFFE01FE03FFFF83FFFF87FF8FFFFFC001C001FFFFE001C001C001FE1F
      E001C001C001FC0FE001C001C001F807E001C001C001F807E001C001C0010007
      E001C001C0010000E001C001C0018000E001C001C001C000C001C001C001E200
      C001C001C001F6000001C001C001FE000001C001C001FE000003C003C003FE00
      C007C007C007FE00C00FC00FC00FFFFFFFFFFFFFFFFFDDBBC003C003FFFFAD5B
      80018001FFFFAD5B80018001FFFFA953800180018001DDBB800180018001801F
      800180018001801B800180018001801180018001800180008001800180018011
      80018001800180018001800180018001800180018001801F800180018000803F
      C003C003FFE1807FFFFFFFFFFFF3FFFFF81FFFFFFFFFFFFFE007FFFF8000C003
      C003FC3F800080018001F81F800080018001F00F800080010000E00780008001
      0000C003800080010000C003800080010000C003800080010000C00380008001
      0000C003800080018001E007800080018001F00F80008001C003F81FFFFF8001
      E007FFFFFFFFC003F81FFFFFFFFFFFFFFFFFFFFFFFFFF81FF707FC3FFFFFE007
      F007F81FF8FFC003F707F00FF87F8001F7FFE007F83F8001F707C003F81F0000
      F0078001F80F0000F7078001F8070000F7FF8001F8070000F7078001F80F0000
      F007C003F81F0000F707E007F83F8001C1FFF00FF87F8001C1FFF81FF8FFC003
      C1FFFC3FFFFFE007FFFFFFFFFFFFF81FFFFFFFFFFFFDFFFF000F000FFFF08000
      000F000FCFE18000FFFFFFFFE6038000DE07EE07FC078000CE07CE07F8078000
      06078207F007800003FF03FFE007800006008200E0078000CE00CE00E00F8000
      DE00EE00E01F8000FFFFFFFFE0278000000F000FC0738000000F000F87FFC000
      FFFFFFFF0FFFFFE1FFFFFFFF3FFFFFF3FFFFFFFFC001C001FFFF8FFFC001C001
      BFFF87FFC001C001CFFF83FFC001C001EE4FC1FF4001C001E717E1FF0001C001
      E68FF1870001C001E327FE010001C001F3FFFE010001C001F3FFFC004001C001
      E0FFFC00C001C001F9FFFC00C001C001F93FFC00C001C001FC7FFE01C003C003
      FFFFFE01C007C007FFFFFF87C00FC00FFFFFF801C0018001F9CFF80180010000
      F087F80180010000F6B7F80180010000F2A7000180010000F007000180010000
      F80F000180010000FE3F000180010000FC1F000180010000FC1F000180030000
      F88F000380030000F9CF000780030000F1C7003F80030000F3E7003F80030000
      F7F7007FC0070000FFFF00FFF83F8001F33FFFFCFFFFFFFFE0079FF9C003C003
      C0008FF380018001800187E7800180010000C3CF800180010000F11F80018001
      0000F83F800180018000FC7F80018001E001F83F80018001F003F19F80018001
      F01FE3CF80018001E03FC7E780018001E03F8FFB80018001C03F1FFF80018001
      C07F3FFFC003C003F8FFFFFFFFFFFFFFC001FFFFFFFF8003C001C007801F0003
      C0018003001F0001C001800300070001C001800300070001C001800300010000
      C001800300010000C001800300010000C001800300010001C001800300010000
      C001800300018003C00180030001C3C7C0018003C001FF87C0038003C001FF8F
      C007C007F001FE1FC00FFFFFF001F87F00000000000000000000000000000000
      000000000000}
  end
  object ppmOutput: TSpTBXPopupMenu
    OnInitPopup = ppmOutputInitPopup
    Left = 783
    Top = 7
    object mnuCopyMessage: TSpTBXItem
      OnClick = mnuCopyMessageClick
      CaptionW = 'Copy message'
    end
    object mnuCopyAll: TSpTBXItem
      OnClick = mnuCopyAllClick
      CaptionW = 'Copy all'
    end
    object sepOutput: TSpTBXSeparatorItem
    end
    object mnuSaveToFile: TSpTBXItem
      OnClick = mnuSaveToFileClick
      CaptionW = 'Save to file...'
    end
  end
  object sciCallTips: TSciCallTips
    EndDefinition = ')'
    Editor = sciEditor
    ParametersEnd = ')'
    ParametersSeparators = ','
    ParametersStart = '('
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    OnBeforeShow = sciCallTipsBeforeShow
    Left = 842
    Top = 36
  end
  object sciPropertyLoader: TSciPropertyLoader
    Editor = sciEditor
    Left = 812
    Top = 36
  end
  object odOpen: TOpenDialog
    Filter = 
      'All supported files|*.sma;*.inc;*.cpp;*.h;*.htm;*.html;*.sql;*.x' +
      'ml;*.txt;*.inl|SMALL scripts (*.sma;*.inc;*.inl)|*.sma;*.inc;*.i' +
      'nl|C++ files (*.cpp;*.h)|*.cpp;*.h|HTML files (*.htm;*.html)|*.h' +
      'tm;*.html|SQL databases (*.sql)|*.sql|XML files (*.xml)|*.xml|Te' +
      'xtfiles (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open...'
    Left = 752
    Top = 6
  end
  object sdSave: TSaveDialog
    Filter = 
      'All supported files|*.sma;*.inc;*.cpp;*.h;*.htm;*.html;*.sql;*.x' +
      'ml;*.txt;*.inl|SMALL scripts (*.sma;*.inc;*.inl)|*.sma;*.inc;*.i' +
      'nl|C++ files (*.cpp;*.h)|*.cpp;*.h|HTML files (*.htm;*.html)|*.h' +
      'tm;*.html|SQL databases (*.sql)|*.sql|XML files (*.xml)|*.xml|Te' +
      'xtfiles (*.txt)|*.txt|All Files (*.*)|*.*'
    Title = 'Save...'
    Left = 752
    Top = 36
  end
  object sciPrinter: TSciPrinter
    Editor = sciEditor
    Title = 'DelphiSci'
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clBlack
    HeaderFont.Height = -11
    HeaderFont.Name = 'Courier New'
    HeaderFont.Style = []
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clBlack
    FooterFont.Height = -11
    FooterFont.Name = 'Courier New'
    FooterFont.Style = []
    ColorMode = sccmNormal
    WordWrap = sciNoWrap
    Magnification = 0
    Left = 692
    Top = 6
  end
  object ppmEditor: TSpTBXPopupMenu
    Images = ilImages
    Left = 782
    Top = 36
    object mnuEditorCopy: TSpTBXItem
      ImageIndex = 9
      Images = ilImages
      OnClick = mnuEditorCopyClick
      CaptionW = 'Copy'
    end
    object mnuEditorCut: TSpTBXItem
      ImageIndex = 8
      Images = ilImages
      OnClick = mnuEditorCutClick
      CaptionW = 'Cut'
    end
    object mnuEditorPaste: TSpTBXItem
      ImageIndex = 10
      Images = ilImages
      OnClick = mnuEditorPasteClick
      CaptionW = 'Paste'
    end
    object mnuEditorDelete: TSpTBXItem
      ImageIndex = 5
      Images = ilImages
      OnClick = mnuEditorDeleteClick
      CaptionW = 'Delete'
    end
    object sepEditorMenu1: TSpTBXSeparatorItem
    end
    object mnuEditorSelectAll: TSpTBXItem
      ImageIndex = 11
      Images = ilImages
      OnClick = mnuEditorSelectAllClick
      CaptionW = 'Select all'
    end
    object sepEditorMenu2: TSpTBXSeparatorItem
    end
    object mnuEditorUndo: TSpTBXItem
      ImageIndex = 6
      Images = ilImages
      OnClick = mnuEditorUndoClick
      CaptionW = 'Undo'
    end
    object mnuEditorRedo: TSpTBXItem
      ImageIndex = 7
      Images = ilImages
      OnClick = mnuEditorRedoClick
      CaptionW = 'Redo'
    end
    object sepEditorMenu3: TSpTBXSeparatorItem
    end
    object mnuToogleBookmark: TSpTBXItem
      ShortCut = 49218
      OnClick = mnuToogleBookmarkClick
      CaptionW = 'Toogle Bookmark'
    end
    object mnuGoToBookmark: TSpTBXItem
      ShortCut = 16450
      OnClick = mnuGoToBookmarkClick
      CaptionW = 'Go to next Bookmark'
    end
  end
  object sciSearchReplace: TSciSearchReplace
    SearchForSelWord = False
    Editor = sciEditor
    Left = 692
    Top = 36
  end
  object IdFTP: TIdFTP
    MaxLineAction = maSplit
    ProxySettings.ProxyType = fpcmNone
    ProxySettings.Port = 0
    Left = 722
    Top = 6
  end
  object sciAutoComplete: TSciAutoComplete
    NumStartChars = 1
    AStrings.Strings = (
      ''
      'access'
      'add'
      'ADMIN_ADMIN'
      'ADMIN_ALL'
      'ADMIN_BAN'
      'ADMIN_CFG'
      'ADMIN_CHAT'
      'ADMIN_CVAR'
      'ADMIN_IMMUNITY'
      'ADMIN_KICK'
      'ADMIN_LEVEL_A'
      'ADMIN_LEVEL_B'
      'ADMIN_LEVEL_C'
      'ADMIN_LEVEL_D'
      'ADMIN_LEVEL_E'
      'ADMIN_LEVEL_F'
      'ADMIN_LEVEL_G'
      'ADMIN_LEVEL_H'
      'ADMIN_MAP'
      'ADMIN_MENU'
      'ADMIN_PASSWORD'
      'ADMIN_RCON'
      'ADMIN_RESERVATION'
      'ADMIN_SLAY'
      'ADMIN_USER'
      'ADMIN_VOTE'
      'ALLIES'
      'AMX_FLAG_BIGENDIAN'
      'AMX_FLAG_BROWSE'
      'AMX_FLAG_COMPACT'
      'AMX_FLAG_DEBUG'
      'AMX_FLAG_LINEOPS'
      'AMX_FLAG_NOCHECKS'
      'AMX_FLAG_RELOC'
      'AMXX_VERSION'
      'AMXX_VERSION_STR[]="1'
      'anglevector'
      'assert'
      'attach_view'
      'ATTN_IDLE'
      'ATTN_NONE'
      'ATTN_STATIC'
      'AXIS'
      'BLOCK_NOT'
      'BLOCK_ONCE'
      'BLOCK_SET'
      'bomb_defused'
      'bomb_defusing'
      'bomb_explode'
      'bomb_planted'
      'bomb_planting'
      'break'
      'build_path'
      'call_think'
      'callfunc_begin'
      'callfunc_end'
      'callfunc_push_float'
      'callfunc_push_floatrf'
      'callfunc_push_int'
      'callfunc_push_intrf'
      'callfunc_push_str'
      'CAMERA_3RDPERSON'
      'CAMERA_NONE'
      'CAMERA_TOPDOWN'
      'CAMERA_UPLEFT'
      'case'
      'CHAN_AUTO'
      'CHAN_ITEM'
      'CHAN_NETWORKVOICE_BASE'
      'CHAN_NETWORKVOICE_END'
      'CHAN_STATIC'
      'CHAN_STREAM'
      'CHAN_WEAPON'
      'change_task'
      'char'
      'clamp'
      'client_authorized'
      'client_built'
      'client_changeclass'
      'client_changeteam'
      'client_cmd'
      'client_command'
      'client_connect'
      'client_damage'
      'client_death'
      'client_disconnect'
      'client_impulse'
      'client_infochanged'
      'client_kill'
      'client_PostThink'
      'client_PreThink'
      'client_print'
      'client_putinserver'
      'client_score'
      'client_spawn'
      'cmd_access'
      'cmd_target'
      'colored_menus'
      'console_cmd'
      'console_print'
      'const'
      'contain'
      'containi'
      'CONTENTS_TRANSLUCENT'
      'continue'
      'copy'
      'copy_keyvalue'
      'copyc'
      'create_entity'
      'CreateEntity'
      'cs_get_hostage_foll'
      'cs_get_hostage_id'
      'cs_get_no_knives'
      'cs_get_user_bpammo'
      'cs_get_user_buyzone'
      'cs_get_user_deaths'
      'cs_get_user_defuse'
      'cs_get_user_driving'
      'cs_get_user_hasprim'
      'cs_get_user_model'
      'cs_get_user_money'
      'cs_get_user_nvg'
      'cs_get_user_plant'
      'cs_get_user_stationary'
      'cs_get_user_team'
      'cs_get_user_tked'
      'cs_get_user_vip'
      'cs_get_weapon_ammo'
      'cs_get_weapon_burst'
      'cs_get_weapon_id'
      'cs_get_weapon_silen'
      'cs_reset_user_model'
      'cs_set_hostage_foll'
      'cs_set_no_knives'
      'cs_set_user_bpammo'
      'cs_set_user_deaths'
      'cs_set_user_defuse'
      'cs_set_user_model'
      'cs_set_user_money'
      'cs_set_user_nvg'
      'cs_set_user_plant'
      'cs_set_user_team'
      'cs_set_user_tked'
      'cs_set_user_vip'
      'cs_set_weapon_ammo'
      'cs_set_weapon_burst'
      'cs_set_weapon_silen'
      'cstrike_running'
      'CSW_AK47'
      'CSW_AUG'
      'CSW_AWP'
      'CSW_C4'
      'CSW_DEAGLE'
      'CSW_ELITE'
      'CSW_FAMAS'
      'CSW_FIVESEVEN'
      'CSW_FLASHBANG'
      'CSW_G3SG1'
      'CSW_GALI'
      'CSW_GALIL'
      'CSW_GLOCK18'
      'CSW_HEGRENADE'
      'CSW_KNIFE'
      'CSW_M249'
      'CSW_M3'
      'CSW_M4A1'
      'CSW_MAC10'
      'CSW_MP5NAVY'
      'CSW_P228'
      'CSW_P90'
      'CSW_SCOUT'
      'CSW_SG550'
      'CSW_SG552'
      'CSW_SMOKEGRENADE'
      'CSW_TMP'
      'CSW_UMP45'
      'CSW_USP'
      'CSW_XM1014'
      'current_num_ents'
      'custom_weapon_add'
      'custom_weapon_dmg'
      'custom_weapon_shot'
      'cvar_exists'
      'date'
      'dbi_close'
      'dbi_connect'
      'dbi_error'
      'dbi_field'
      'dbi_free_result'
      'dbi_nextrow'
      'dbi_num_rows'
      'dbi_query'
      'dbi_result'
      'dbi_type'
      'default'
      'define'
      'defined'
      'delete_file'
      'DF_Blocked'
      'DF_ClientCommand'
      'DF_ClientConnect'
      'DF_ClientDisconnect'
      'DF_ClientKill'
      'DF_ClientPutInServer'
      'DF_ClientUserInfoChanged'
      'DF_CreateInstancedBaseline'
      'DF_GameInit'
      'DF_GetGameDescription'
      'DF_GetHullBounds'
      'DF_MetaFunc_CallGameEntity'
      'DF_ParmsChangeLevel'
      'DF_ParmsNewLevel'
      'DF_pfnAllowLagCompensation'
      'DF_PlayerPostThink'
      'DF_PlayerPreThink'
      'DF_PM_FindTextureType'
      'DF_RegisterEncoders'
      'DF_ServerDeactivate'
      'DF_SetAbsBox'
      'DF_Spawn'
      'DF_SpectatorConnect'
      'DF_SpectatorDisconnect'
      'DF_SpectatorThink'
      'DF_StartFrame'
      'DF_Sys_Error'
      'DF_Think'
      'DF_Touch'
      'DF_Use'
      'DispatchKeyValue'
      'DispatchSpawn'
      'dllfunc'
      'DMG_ACID'
      'DMG_ALWAYSGIB'
      'DMG_BLAST'
      'DMG_BULLET'
      'DMG_BURN'
      'DMG_CLUB'
      'DMG_CRUSH'
      'DMG_DROWN'
      'DMG_DROWNRECOVER'
      'DMG_ENERGYBEAM'
      'DMG_FALL'
      'DMG_FREEZE'
      'DMG_GENERIC'
      'DMG_MORTAR'
      'DMG_NERVEGAS'
      'DMG_NEVERGIB'
      'DMG_PARALYZE'
      'DMG_POISON'
      'DMG_RADIATION'
      'DMG_SHOCK'
      'DMG_SLASH'
      'DMG_SLOWBURN'
      'DMG_SLOWFREEZE'
      'DMG_SONIC'
      'DMG_TIMEBASED'
      'do'
      'dod_get_map_info'
      'dod_get_next_class'
      'dod_get_pl_deaths'
      'dod_get_pl_teamname'
      'dod_get_pronestate'
      'dod_get_team_score'
      'dod_get_user_ammo'
      'dod_get_user_class'
      'dod_get_user_kills'
      'dod_get_user_score'
      'dod_get_user_weapon'
      'dod_is_deployed'
      'dod_is_randomclass'
      'dod_make_deathmsg'
      'dod_set_fuse'
      'dod_set_pl_deaths'
      'dod_set_pl_teamname'
      'dod_set_stamina'
      'dod_set_user_ammo'
      'dod_set_user_class'
      'dod_set_user_kills'
      'dod_set_user_score'
      'dod_set_user_team'
      'dod_user_kill'
      'dod_wpnlog_to_id'
      'dod_wpnlog_to_name'
      'DODMAX_WEAPONS'
      'drop_to_floor'
      'EF_AllocString'
      'EF_AngleVectors'
      'EF_AnimationAutomove'
      'EF_BuildSoundMSG'
      'EF_CanSkipPlayer'
      'EF_ChangeLevel'
      'EF_ChangePitch'
      'EF_ChangeYaw'
      'EF_CheckVisibility'
      'EF_CreateEntity'
      'EF_CreateFakeClient'
      'EF_CreateNamedEntity'
      'EF_CrosshairAngle'
      'EF_DecalIndex'
      'EF_DropToFloor'
      'EF_EmitAmbientSound'
      'EF_EmitSound'
      'EF_EntIsOnFloor'
      'EF_EntitiesInPVS'
      'EF_FadeClientVolume'
      'EF_FindClientInPVS'
      'EF_FindEntityByString'
      'EF_FindEntityInSphere'
      'EF_FreeEntPrivateData'
      'EF_GetAimVector'
      'EF_GetAttachment'
      'EF_GetBonePosition'
      'EF_GetClientListening'
      'EF_GetCurrentPlayer'
      'EF_GetEntityIllum'
      'EF_GetPhysicsInfoString'
      'EF_GetPhysicsKeyValue'
      'EF_InfoKeyValue'
      'EF_INVLIGHT'
      'EF_LIGHT'
      'EF_LightStyle'
      'EF_MakeStatic'
      'EF_MakeVectors'
      'EF_MessageBegin'
      'EF_ModelFrames'
      'EF_ModelIndex'
      'EF_MoveToOrigin'
      'EF_NODRAW'
      'EF_NOINTERP'
      'EF_NumberOfEntities'
      'EF_ParticleEffect'
      'EF_PlaybackEvent'
      'EF_PointContents'
      'EF_PrecacheEvent'
      'EF_PrecacheGeneric'
      'EF_PrecacheModel'
      'EF_PrecacheSound'
      'EF_RegUserMsg'
      'EF_RemoveEntity'
      'EF_RunPlayerMove'
      'EF_SetClientKeyValue'
      'EF_SetClientListening'
      'EF_SetClientMaxspeed'
      'EF_SetGroupMask'
      'EF_SetKeyValue'
      'EF_SetModel'
      'EF_SetOrigin'
      'EF_SetPhysicsKeyValue'
      'EF_SetSize'
      'EF_SetView'
      'EF_StaticDecal'
      'EF_SzFromIndex'
      'EF_Time'
      'EF_TraceHull'
      'EF_TraceLine'
      'EF_TraceModel'
      'EF_TraceMonsterHull'
      'EF_TraceSphere'
      'EF_TraceTexture'
      'EF_TraceToss'
      'EF_VecToAngles'
      'EF_VecToYaw'
      'EF_WalkMove'
      'EF_WriteAngle'
      'EF_WriteCoord'
      'else'
      'emit_sound'
      'endif'
      'engclient_cmd'
      'engclient_print'
      'engfunc'
      'ENT_SetModel'
      'ENT_SetOrigin'
      'entity_count'
      'entity_get_byte'
      'entity_get_edict'
      'entity_get_float'
      'entity_get_int'
      'entity_get_string'
      'entity_get_vector'
      'entity_range'
      'entity_set_byte'
      'entity_set_edict'
      'entity_set_float'
      'entity_set_int'
      'entity_set_model'
      'entity_set_origin'
      'entity_set_size'
      'entity_set_string'
      'entity_set_vector'
      'Entvars_Get_Byte'
      'Entvars_Get_Edict'
      'Entvars_Get_Float'
      'Entvars_Get_Int'
      'Entvars_Get_String'
      'Entvars_Get_Vector'
      'Entvars_Set_Byte'
      'Entvars_Set_Edict'
      'Entvars_Set_Float'
      'Entvars_Set_Int'
      'Entvars_Set_String'
      'Entvars_Set_Vector'
      'enum'
      'equal'
      'equali'
      'exit'
      'fake_touch'
      'fakedamage'
      'FakeTouch'
      'fclose'
      'FCVAR_CLIENTDLL'
      'FCVAR_EXTDLL'
      'FCVAR_PRINTABLEONLY'
      'FCVAR_PROTECTED'
      'FCVAR_SPONLY'
      'FCVAR_UNLOGGED'
      'feof'
      'fflush'
      'fgetc'
      'fgetf'
      'fgeti'
      'fgetl'
      'fgets'
      'file_exists'
      'file_size'
      'filesize'
      'find_ent'
      'find_ent_by_class'
      'find_ent_by_model'
      'find_ent_by_owner'
      'find_ent_by_target'
      'find_ent_by_tname'
      'find_ent_in_sphere'
      'find_ent_sphere'
      'find_entity'
      'find_player'
      'find_plugin_bydesc'
      'find_plugin_byfile'
      'find_sphere_class'
      'FindEntity'
      'FL_ALWAYSTHINK'
      'FL_BASEVELOCITY'
      'FL_CUSTOMENTITY'
      'FL_DORMANT'
      'FL_DUCKING'
      'FL_FAKECLIENT'
      'FL_FLOAT'
      'FL_FROZEN'
      'FL_GRAPHED'
      'FL_IMMUNE_LAVA'
      'FL_IMMUNE_WATER'
      'FL_KILLME'
      'FL_MONSTERCLIP'
      'FL_ONTRAIN'
      'FL_PROXY'
      'FL_SPECTATOR'
      'FL_WORLDBRUSH'
      'FLAG_AUTHID'
      'FLAG_IP'
      'FLAG_KICK'
      'FLAG_NOPASS'
      'FLAG_TAG'
      'float'
      'floatabs'
      'floatacos'
      'floatadd'
      'floatasin'
      'floatatan'
      'floatatan2'
      'floatcmp'
      'floatcos'
      'floatdiv'
      'floatfract'
      'floatlog'
      'floatmul'
      'floatpower'
      'floatround'
      'floatsin'
      'floatsqroot'
      'floatstr'
      'floatsub'
      'floattan'
      'FMRES_HANDLED'
      'FMRES_IGNORED'
      'FMRES_OVERRIDE'
      'FMRES_SUPERCEDE'
      'FMV_CELL'
      'FMV_FLOAT'
      'fopen'
      'for'
      'force_unmodified'
      'force_use'
      'format'
      'format_args'
      'format_time'
      'forward'
      'forward_return'
      'fputc'
      'fputf'
      'fputi'
      'fputl'
      'fputs'
      'fread'
      'fscanf'
      'fseek'
      'FT_NEW'
      'FT_OLD'
      'ftell'
      'funcidx'
      'FUSE_RESET'
      'FUSE_SET'
      'FVecIVec'
      'fwrite'
      'geoip_code2'
      'geoip_code3'
      'geoip_country'
      'get_basedir'
      'get_brush_entity_origin'
      'get_build'
      'get_class'
      'get_clcmd'
      'get_clcmdsnum'
      'get_client_listen'
      'get_concmd'
      'get_concmdsnum'
      'get_configsdir'
      'get_customdir'
      'get_cvar_flags'
      'get_cvar_float'
      'get_cvar_num'
      'get_cvar_string'
      'get_datadir'
      'get_decal_index'
      'get_distance'
      'get_entity_distance'
      'get_entity_flags'
      'get_entity_origin'
      'get_entity_velocity'
      'get_entity_visibility'
      'get_filename'
      'get_flags'
      'get_gametime'
      'get_global_edict'
      'get_global_float'
      'get_global_int'
      'get_global_string'
      'get_global_vector'
      'get_grenade'
      'get_grenade_id'
      'get_grenade_index'
      'get_hostage_id'
      'get_info_keybuffer'
      'get_keyvalue'
      'get_lang'
      'get_langsnum'
      'get_localinfo'
      'get_logfile'
      'get_mapname'
      'get_mask'
      'get_max_entities'
      'get_maxplayers'
      'get_maxspeed'
      'get_modname'
      'get_module'
      'get_modulesnum'
      'get_msg_arg_float'
      'get_msg_arg_int'
      'get_msg_arg_string'
      'get_msg_args'
      'get_msg_argtype'
      'get_msg_block'
      'get_msg_origin'
      'get_owner'
      'get_pdata'
      'get_pdata_char'
      'get_pdata_float'
      'get_pdata_int'
      'get_pdata_short'
      'get_players'
      'get_playersnum'
      'get_plugin'
      'get_pluginsnum'
      'get_private_f'
      'get_private_i'
      'get_range'
      'get_res'
      'get_spawn'
      'get_speak'
      'get_special'
      'get_speed'
      'get_speedchange'
      'get_srvcmd'
      'get_srvcmdsnum'
      'get_stats'
      'get_stats2'
      'get_statsnum'
      'get_string'
      'get_systime'
      'get_time'
      'get_timeleft'
      'get_tr'
      'get_user_aiming'
      'get_user_ammo'
      'get_user_armor'
      'get_user_astats'
      'get_user_attacker'
      'get_user_authid'
      'get_user_button'
      'get_user_deaths'
      'get_user_flags'
      'get_user_frags'
      'get_user_godmode'
      'get_user_gravity'
      'get_user_health'
      'get_user_hitzones'
      'get_user_index'
      'get_user_info'
      'get_user_ip'
      'get_user_lstats'
      'get_user_maxspeed'
      'get_user_menu'
      'get_user_money'
      'get_user_msgid'
      'get_user_msgname'
      'get_user_name'
      'get_user_noclip'
      'get_user_oldbutton'
      'get_user_origin'
      'get_user_ping'
      'get_user_rstats'
      'get_user_stats'
      'get_user_stats2'
      'get_user_team'
      'get_user_time'
      'get_user_userid'
      'get_user_velocity'
      'get_user_vstats'
      'get_user_weapon'
      'get_user_weapons'
      'get_user_wlstats'
      'get_user_wrstats'
      'get_user_wstats'
      'get_usercmd'
      'get_vaultdata'
      'get_weaponname'
      'get_xvar_float'
      'get_xvar_id'
      'get_xvar_num'
      'getarg'
      'getkey_float'
      'getkey_int'
      'getkey_string'
      'GetMessageBlock'
      'give_item'
      'globals_get_edict'
      'globals_get_float'
      'globals_get_int'
      'globals_get_string'
      'globals_get_vector'
      'goto'
      'gpglobals_v'
      'gpgobals_time'
      'grenade_throw'
      'halflife_time'
      'has_weapon'
      'heapspace'
      'HIT_CHEST'
      'HIT_GENERIC'
      'HIT_HEAD'
      'HIT_LEFTARM'
      'HIT_LEFTLEG'
      'HIT_RIGHTARM'
      'HIT_RIGHTLEG'
      'HIT_STOMACH'
      'HIW_AK47'
      'HIW_AKS74U'
      'HIW_BERETTA'
      'HIW_FLASHBANG'
      'HIW_GLOCK'
      'HIW_M11'
      'HIW_M11SD'
      'HIW_M16A2'
      'HIW_M4A1'
      'HIW_MP5A4'
      'HIW_MP5SD5'
      'HIW_NATOGREN'
      'HIW_PSG1'
      'HIW_REMINGTON'
      'HIW_SPAS12'
      'HIW_TANGOGREN'
      'HIW_ZASTAVA'
      'HLTime'
      'HULL_HEAD'
      'HULL_HUMAN'
      'HULL_LARGE'
      'HULL_POINT'
      'if'
      'IN_ALT1'
      'IN_ATTACK'
      'IN_ATTACK2'
      'IN_BACK'
      'IN_CANCEL'
      'IN_DUCK'
      'IN_FORWARD'
      'IN_JUMP'
      'IN_LEFT'
      'in_list_float'
      'in_list_int'
      'in_list_string'
      'IN_MOVELEFT'
      'IN_MOVERIGHT'
      'IN_RELOAD'
      'IN_RIGHT'
      'IN_RUN'
      'IN_SCORE'
      'IN_USE'
      'include'
      'inconsistent_file'
      'is_combat'
      'is_dedicated_server'
      'is_ent_valid'
      'is_entity'
      'is_jit_enabled'
      'is_linux_server'
      'is_map_valid'
      'is_module_loaded'
      'is_plugin_loaded'
      'is_running'
      'is_user_admin'
      'is_user_alive'
      'is_user_bot'
      'is_user_connected'
      'is_user_connecting'
      'is_user_hltv'
      'is_valid_ent'
      'isalnum'
      'isalpha'
      'isdigit'
      'isspace'
      'IVecFVec'
      'jghg_find_ent_owner'
      'jghg2_set_size'
      'jghg2_think'
      'keytable_clear'
      'keytable_count'
      'keytable_delete'
      'keytable_getkey'
      'keytable_getval'
      'keytable_next'
      'keytable_reset'
      'lang_exists'
      'LANG_PLAYER'
      'LANG_SERVER'
      'list_clear'
      'list_clear_float'
      'list_clear_int'
      'list_clear_string'
      'list_delete'
      'list_delete_float'
      'list_delete_int'
      'list_delete_string'
      'list_get'
      'list_get_float'
      'list_get_int'
      'list_get_string'
      'list_getf'
      'list_next'
      'list_next_float'
      'list_next_int'
      'list_next_string'
      'list_pop'
      'list_pop_float'
      'list_pop_int'
      'list_pop_string'
      'list_push_float'
      'list_push_int'
      'list_push_string'
      'list_reset'
      'list_reset_float'
      'list_reset_int'
      'list_reset_string'
      'list_size'
      'list_size_float'
      'list_size_int'
      'list_size_string'
      'list_store_float'
      'list_store_int'
      'list_store_string'
      'log_amx'
      'log_message'
      'log_to_file'
      'make_deathmsg'
      'make_string'
      'max'
      'md5'
      'md5_file'
      'MENU_KEY_0'
      'MENU_KEY_1'
      'MENU_KEY_2'
      'MENU_KEY_3'
      'MENU_KEY_4'
      'MENU_KEY_5'
      'MENU_KEY_6'
      'MENU_KEY_7'
      'MENU_KEY_8'
      'MENU_KEY_9'
      'message_begin'
      'message_end'
      'MessageBlock'
      'min'
      'MOVETYPE_ANGLECLIP'
      'MOVETYPE_ANGLENOCLIP'
      'MOVETYPE_BOUNCEMISSILE'
      'MOVETYPE_FOLLOW'
      'msg_args'
      'msg_data'
      'msg_data_type'
      'msg_dest'
      'msg_loc'
      'msg_name'
      'MSG_ONE_UNRELIABLE'
      'MSG_PAS'
      'MSG_PAS_R'
      'MSG_PVS'
      'MSG_PVS_R'
      'msg_set_f'
      'msg_set_i'
      'msg_set_s'
      'msg_strdata'
      'msg_type'
      'mysql_close'
      'mysql_connect'
      'mysql_error'
      'mysql_getfield'
      'mysql_nextrow'
      'mysql_query'
      'native'
      'new'
      'new_float_list'
      'new_int_list'
      'new_keytable'
      'new_list'
      'new_string_list'
      'NS_CONST_INC'
      'ns_get_build'
      'ns_get_class'
      'ns_get_deaths'
      'ns_get_energy'
      'ns_get_exp'
      'ns_get_hive_trait'
      'ns_get_jpfuel'
      'ns_get_mask'
      'ns_get_maxspeed'
      'ns_get_points'
      'ns_get_res'
      'ns_get_score'
      'ns_get_spawn'
      'ns_get_speedchange'
      'ns_get_struct_owner'
      'ns_get_weap_clip'
      'ns_get_weap_dmg'
      'ns_get_weap_range'
      'ns_get_weap_reserve'
      'ns_give_item'
      'ns_has_weapon'
      'NS_INC'
      'ns_is_combat'
      'ns_popup'
      'ns_set_deaths'
      'ns_set_energy'
      'ns_set_exp'
      'ns_set_fov'
      'ns_set_hive_trait'
      'ns_set_jpfuel'
      'ns_set_mask'
      'ns_set_player_body'
      'ns_set_player_model'
      'ns_set_player_skin'
      'ns_set_points'
      'ns_set_res'
      'ns_set_score'
      'ns_set_speedchange'
      'ns_set_struct_owner'
      'ns_set_weap_clip'
      'ns_set_weap_dmg'
      'ns_set_weap_range'
      'ns_set_weap_reserve'
      'ns2amx_getammo'
      'ns2amx_getenergy'
      'ns2amx_gethives'
      'ns2amx_getjpfuel'
      'ns2amx_giveitem'
      'ns2amx_inrange'
      'ns2amx_isdigesting'
      'ns2amx_moveto'
      'ns2amx_nspopup'
      'ns2amx_setammo'
      'ns2amx_setenergy'
      'ns2amx_setjpfuel'
      'ns2amx_setres'
      'ns2amx_version'
      'num_to_str'
      'num_to_word'
      'numargs'
      'number_of_entities'
      'numtostr'
      'operator'
      'parse'
      'parse_loguser'
      'parse_time'
      'pause'
      'pev'
      'pev_f'
      'pev_i'
      'pfn_keyvalue'
      'pfn_playbackevent'
      'pfn_spawn'
      'pfn_think'
      'pfn_touch'
      'PITCH_HIGH'
      'PITCH_LOW'
      'playback_event'
      'plugin_cfg'
      'PLUGIN_CONTINUE'
      'plugin_end'
      'plugin_flags'
      'PLUGIN_HANDLED'
      'PLUGIN_HANDLED_MAIN'
      'plugin_init'
      'plugin_log'
      'plugin_modules'
      'plugin_pause'
      'plugin_precache'
      'plugin_unpause'
      'point_contents'
      'PointContents'
      'power'
      'precache_event'
      'precache_generic'
      'precache_model'
      'precache_sound'
      'public'
      'radius_damage'
      'RadiusDamage'
      'random'
      'random_float'
      'random_num'
      'read_argc'
      'read_args'
      'read_argv'
      'read_data'
      'read_datanum'
      'read_dir'
      'read_file'
      'read_flags'
      'read_logargc'
      'read_logargv'
      'read_logdata'
      'regex_free'
      'regex_match'
      'regex_substr'
      'register_changelvl'
      'register_clcmd'
      'register_clientkill'
      'register_concmd'
      'register_cvar'
      'register_dictionary'
      'register_event'
      'register_forward'
      'register_impulse'
      'register_logevent'
      'register_menu'
      'register_menucmd'
      'register_menuid'
      'register_message'
      'register_msgblock'
      'register_msgedit'
      'register_playback'
      'register_plugin'
      'register_srvcmd'
      'register_statsfwd'
      'register_think'
      'register_touch'
      'remove_cvar_flags'
      'remove_entity'
      'remove_entity_name'
      'remove_quotes'
      'remove_task'
      'remove_user_flags'
      'remove_vaultdata'
      'RemoveEntity'
      'replace'
      'require_module'
      'reset_user_wstats'
      'return'
      'rewind'
      'SEEK_CUR'
      'SEEK_END'
      'SEEK_SET'
      'server_changelevel'
      'server_cmd'
      'server_exec'
      'server_frame'
      'server_print'
      'ServerFrame'
      'set_client_listen'
      'set_cvar_flags'
      'set_cvar_float'
      'set_cvar_num'
      'set_cvar_string'
      'set_entity_flags'
      'set_entity_origin'
      'set_entity_velocity'
      'set_entity_visibility'
      'set_hudmessage'
      'set_kvhandled'
      'set_lights'
      'set_localinfo'
      'set_mask'
      'set_msg_arg_float'
      'set_msg_arg_int'
      'set_msg_arg_string'
      'set_msg_block'
      'set_pdata'
      'set_pdata_char'
      'set_pdata_float'
      'set_pdata_int'
      'set_pdata_short'
      'set_pev'
      'set_pev_f'
      'set_pev_i'
      'set_player_body'
      'set_player_model'
      'set_player_skin'
      'set_private_f'
      'set_private_i'
      'set_rendering'
      'set_size'
      'set_speak'
      'set_speedchange'
      'set_task'
      'set_tr'
      'set_user_armor'
      'set_user_deaths'
      'set_user_flags'
      'set_user_footsteps'
      'set_user_frags'
      'set_user_godmode'
      'set_user_gravity'
      'set_user_health'
      'set_user_hitzones'
      'set_user_info'
      'set_user_maxspeed'
      'set_user_money'
      'set_user_noclip'
      'set_user_origin'
      'set_user_rendering'
      'set_user_velocity'
      'set_usercmd'
      'set_vaultdata'
      'set_view'
      'set_xvar_float'
      'set_xvar_num'
      'setarg'
      'setc'
      'SetSpeak'
      'SetView'
      'show_activity'
      'show_hudmessage'
      'show_menu'
      'show_motd'
      'sizeof'
      'sleep'
      'socket_change'
      'socket_close'
      'socket_open'
      'socket_recv'
      'socket_send'
      'SOCKET_TCP'
      'SOCKET_UDP'
      'spawn'
      'SPEAK_ALL'
      'SPEAK_LISTENALL'
      'SPEAK_MUTED'
      'SPEAK_NORMAL'
      'sqroot'
      'STAMINA_RESET'
      'STAMINA_SET'
      'static'
      'stock'
      'store_float'
      'store_int'
      'store_string'
      'str_to_num'
      'strbreak'
      'string'
      'strip_user_weapons'
      'strlen'
      'strpack'
      'strtok'
      'strtolower'
      'strtonum'
      'strtoupper'
      'strunpack'
      'supercede'
      'SVC_ADDANGLE'
      'SVC_CDTRACK'
      'SVC_INTERMISSION'
      'SVC_NEWUSERMSG'
      'SVC_ROOMTYPE'
      'SVC_TEMPENTITY'
      'SVC_WEAPONANIM'
      'swapchars'
      'switch'
      'take_damage'
      'task_exists'
      'tfc_clearmodel'
      'tfc_getbammo'
      'tfc_getweaponbammo'
      'tfc_isgrenade'
      'tfc_setbammo'
      'tfc_setmodel'
      'tfc_setpddata'
      'tfc_setweaponbammo'
      'tfc_userkill'
      'TFCMAX_WEAPONS'
      'tickcount'
      'time'
      'tolower'
      'toupper'
      'trace_hull'
      'trace_line'
      'trace_normal'
      'TraceLn'
      'TraceNormal'
      'traceresult'
      'trim'
      'ts_createpwup'
      'ts_getkillingstreak'
      'ts_getusercash'
      'ts_getuseritems'
      'ts_getuserkillflags'
      'ts_getuserlastfrag'
      'ts_getuserpwup'
      'ts_getuserspace'
      'ts_getuserwpn'
      'ts_givepwup'
      'ts_giveweapon'
      'ts_setpddata'
      'ts_wpnlogtoid'
      'ts_wpnlogtoname'
      'TSA_FLASHLIGHT'
      'TSA_LASERSIGHT'
      'TSA_SCOPE'
      'TSA_SILENCER'
      'TSITEM_KUNGFU'
      'TSITEM_SUPERJUMP'
      'TSKF_DOUBLEKILL'
      'TSKF_ISSPEC'
      'TSKF_KILLEDSPEC'
      'TSKF_SLIDINGKILL'
      'TSKF_STUNTKILL'
      'TSMAX_WEAPONS'
      'TSPWUP_ARMOR'
      'TSPWUP_DFIRERATE'
      'TSPWUP_GRENADE'
      'TSPWUP_HEALTH'
      'TSPWUP_INFAMMO'
      'TSPWUP_KUNGFU'
      'TSPWUP_RANDOM'
      'TSPWUP_SLOWMO'
      'TSPWUP_SLOWPAUSE'
      'TSPWUP_SUPERJUMP'
      'ucfirst'
      'unlink'
      'unpause'
      'use'
      'user_has_weapon'
      'user_kill'
      'user_silentkill'
      'user_slap'
      'user_spawn'
      'vaultdata_exists'
      'VecDist'
      'VecLength'
      'VecToAngles'
      'vector_distance'
      'vector_length'
      'vector_to_angle'
      'velocity_by_aim'
      'VelocityByAim'
      'vexd_pfntouch'
      'ViewContents'
      'VOL_NORM'
      'write_angle'
      'write_byte'
      'write_char'
      'write_coord'
      'write_entity'
      'write_file'
      'write_long'
      'write_shortwrite_string'
      'xmod_get_maxweapons'
      'xmod_get_stats_size'
      'xmod_get_wpnlogname'
      'xmod_get_wpnname'
      'xmod_is_custom_wpn'
      'xmod_is_melee_wpn'
      'XS__LIBRELEASE'
      'XS_AMX'
      'XS_AMXX'
      'xvar_exists - BLA blubb XD')
    IgnoreCase = True
    ChooseSingle = False
    AutoHide = True
    DropRestOfWord = False
    CancelAtStart = False
    CompleteWord = False
    CompleteWordOnlyOne = True
    Editor = sciEditor
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    OnBeforeShow = sciAutoCompleteBeforeShow
    MaxWidth = 0
    MaxHeight = 5
    Left = 842
    Top = 6
  end
  object ppmDocuments: TSpTBXPopupMenu
    Left = 662
    Top = 6
    object mnuPClose: TSpTBXItem
      OnClick = mnuPCloseClick
      CaptionW = 'Close file'
    end
    object mnuPCloseAllFiles: TSpTBXItem
      OnClick = mnuPCloseAllFilesClick
      CaptionW = 'Close all files'
    end
    object sepDocuments: TSpTBXSeparatorItem
    end
    object mnuPSave: TSpTBXItem
      CaptionW = 'Save file'
    end
  end
  object JvInspectorDotNETPainter: TJvInspectorDotNETPainter
    DrawNameEndEllipsis = True
    Left = 662
    Top = 36
  end
end
