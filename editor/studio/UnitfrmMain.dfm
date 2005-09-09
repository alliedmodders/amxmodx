object frmMain: TfrmMain
  Left = 260
  Top = 305
  Width = 888
  Height = 640
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
            CaptionW = 'PAWN'
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
    Height = 518
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
      Height = 495
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
        Height = 472
        Align = alLeft
      end
      object spcRight1: TImage
        Left = 877
        Top = 23
        Width = 3
        Height = 472
        Align = alRight
      end
      object splRight: TSplitter
        Left = 701
        Top = 23
        Height = 472
        Align = alRight
      end
      object pnlParent: TPanel
        Left = 3
        Top = 23
        Width = 698
        Height = 472
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object splOutput: TSplitter
          Left = 0
          Top = 387
          Width = 698
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Visible = False
        end
        object sciEditor: TScintilla
          Left = 0
          Top = 0
          Width = 698
          Height = 387
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
          Folding = [foldFold, foldComment, foldPreprocessor, foldCommentPython, foldAtElse, foldHTML, foldHTMLPreProcessor]
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
          Top = 390
          Width = 698
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
        Left = 704
        Top = 23
        Width = 173
        Height = 472
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
          Width = 173
          Height = 449
          Caption = 'Notes'
          ImageIndex = -1
          TabItem = 'tiNotes'
          object imgRight4: TImage
            Left = 170
            Top = 26
            Width = 1
            Height = 421
            Align = alRight
          end
          object imgBottom4: TImage
            Left = 2
            Top = 447
            Width = 169
            Height = 2
            Align = alBottom
          end
          object imgTop4: TImage
            Left = 2
            Top = 24
            Width = 169
            Height = 2
            Align = alTop
          end
          object imgLeft4: TImage
            Left = 2
            Top = 26
            Width = 1
            Height = 421
            Align = alLeft
          end
          object tbxNotes: TSpTBXToolbar
            Left = 2
            Top = 2
            Width = 169
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
            Width = 167
            Height = 421
            Align = alClient
            TabOrder = 1
            OnKeyDown = rtfNotesKeyDown
            OnMouseDown = rtfNotesMouseDown
          end
        end
        object tsExplorer: TSpTBXTabSheet
          Left = 0
          Top = 0
          Width = 173
          Height = 449
          Caption = 'Code-Tools'
          ImageIndex = -1
          TabItem = 'tiTools'
          object spcBottom2: TImage
            Left = 0
            Top = 447
            Width = 173
            Height = 2
            Align = alBottom
          end
          object spcLeft2: TImage
            Left = 0
            Top = 0
            Width = 3
            Height = 447
            Align = alLeft
          end
          object spcRight2: TImage
            Left = 170
            Top = 0
            Width = 3
            Height = 447
            Align = alRight
          end
          object pnlDock: TSpTBXMultiDock
            Left = 3
            Top = 0
            Width = 167
            Height = 447
            Position = dpxClient
            object pnlCodeExplorer: TSpTBXDockablePanel
              Left = 0
              Top = 0
              Caption = 'Code-Explorer'
              DockedWidth = 163
              DockPos = 0
              TabOrder = 0
              OnVisibleChanged = pnlCodeExplorerVisibleChanged
              object trvExplorer: TTreeView
                Left = 0
                Top = 26
                Width = 163
                Height = 186
                Align = alClient
                Images = ilImages
                Indent = 19
                ReadOnly = True
                SortType = stText
                TabOrder = 1
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
              DockedWidth = 163
              DockPos = 216
              TabOrder = 1
              OnVisibleChanged = pnlCodeInspectorVisibleChanged
              object jviCode: TJvInspector
                Left = 0
                Top = 26
                Width = 163
                Height = 185
                Align = alClient
                Divider = 80
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
    Top = 590
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
      000000000000360000002800000040000000E0000000010020000000000000E0
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
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7EF00FFF7EF00FFF7
      EF00FFF7EF00FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFFF
      FF00FFF7F700FFF7F700FFF7F700FFFFFF00FFFFFF00FFFFFF00FFF7EF00FFF7
      EF00FFF7EF00FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFFF
      FF00FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEF
      D600FFEFD600FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFFF
      FF00F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6
      A500F7D6A500FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CEB5AD00FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFF7F700FFFFFF00FFF7F700FFF7F700FFF7
      F700FFF7EF00FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6B5AD00FFFF
      FF00FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEF
      D600FFEFD600FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6A500F7D6
      A500F7D6A500FFF7EF00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7F700FFF7
      F700FFF7F700FFF7F700B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7
      F700FFF7F700FFF7F700B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEC6B50029AD
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6002183
      A7002183A700C6BDAD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D6008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0041EFFF00C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D6008CF7FF00E88E6000E88E6000E88E6000E4753E00E1622300B04A1800BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD0029AD
      D6008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0041EFFF00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6007FC7E900C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000008740D0008740D000874
      0D0008740D000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400FFFFFF00C6C6
      C60084848400C6C6C600FFFFFF00C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B58484000000000000000000000000000F80AA000F80
      AA000F80AA000F80AA000F80AA000F80AA000000000008740D001DAF310021A9
      330008740D000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400848484008484840084848400FFFF
      FF0084848400FFFFFF0084848400848484000000000052CEEF0052CEEF0052CE
      EF0029ADD60029ADD60052CEEF0029ADD60052CEEF0029ADD60052CEEF0029AD
      D60029ADD6000000000000000000000000000000000000000000C6A59C00FFE7
      D600FFE7D600FFE7D600F7DEC600F7DEC600F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B58484000000000000000000088DBB0048DFFA0035D6
      F80038D9F9006FEAFC008DEEF90029B8DE000F80AA0008740D001DAF31001DAF
      310008740D000000000000000000000000008400000084000000840000008400
      00000000000000000000848484008484840084848400FFFFFF00FFFFFF00FFFF
      FF008400000084000000840000008400000029ADD6009CFFFF009CFFFF005ACE
      EF005ACEEF0052CEEF009CFFFF005ACEEF006BE7FF0052CEEF006BE7FF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000000000C6A59C00FFEF
      D600FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00EFCE9C00B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F90068E8FC0008740D0008740D0008740D0008740D0023B93B001FB4
      340008740D0008740D0008740D0008740D000000000000000000000000008400
      0000FF00000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF005ACEEF009CFF
      FF005ACEEF006BE7FF0052CEEF005ACEEF005ACEEF005ACEEF0052CEEF006BE7
      FF0052CEEF0029ADD60000000000000000000000000000000000C6ADA500FFEF
      E700FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600EFCE9C00B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F90068E8FC0008740D0038D05D0036CE5A0031C953002BC2490028BF
      430022B839001FB434001DAF310008740D000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF009CFFFF005ACEEF009CFFFF0052CEEF006BE7FF005ACEEF006BE7FF0052CE
      EF006BE7FF0029ADD60000000000000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEC600F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F90068E8FC0008740D0038D05D0038D05D0038D05D0034CC57002EC5
      4D002BC2490025BB3E0020B6370008740D000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF005ACEEF009CFFFF005ACEEF005ACEEF005ACEEF006BE7FF005ACEEF005ACE
      EF0052CEEF005ACEEF0000000000000000000000000000000000CEB5AD00FFFF
      F700FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00EFCE9C00B584840000000000088DBB00B2F9FD00B7F9FD00C2F9
      FD00DEFAFD00DEFAFD0008740D0008740D0008740D0008740D0038D05D0038D0
      5D0008740D0008740D0008740D0008740D000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000029ADD6009CFFFF009CFFFF006BE7
      FF009CFFFF006BE7FF009CFFFF006BE7FF009CFFFF0052CEEF006BE7FF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000000000D6B5AD00FFFF
      FF00FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600F7D6A500B584840000000000088DBB00B5EEF20031BFE30012A3
      CF0010A0CD0010A0CD0018A9D4000991BF0098E2E40008740D0038D05D0038D0
      5D0008740D000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF0084EFFF009CFFFF009CFFFF009CFFFF0084EFFF006BE7FF0052CEEF005ACE
      EF005ACEEF005ACEEF00000000000000000000000000005A0000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7DEC600F7DEC600F7DEC600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B584840000000000088DBB0036C5E70059E7FC0035D6
      F80038D9F9006FEAFC008DEEF90031BFE3000A88B50008740D0038D05D0038D0
      5D0008740D000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF006BE7FF009CFFFF009CFFFF005ACEEF009CFFFF006BE7FF009CFFFF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000730800005A0000FFFF
      FF00FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00F7DEB500B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F9006FEAFC008DEEF90031BFE3000E9DCA0008740D0008740D000874
      0D0008740D000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF009CFFFF009CFFFF005ACEEF009CFFFF005ACEEF009CFFFF009CFFFF005ACE
      EF0052CEEF005ACEEF000000000000000000000000000073080000730800005A
      0000FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600F7D6B500B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F9006FEAFC008DEEF90031BFE3000C9BC9000F80AA00000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000000000000000000000000000029ADD6006BE7FF009CFFFF005ACE
      EF009CFFFF005ACEEF006BE7FF0029ADD60029ADD60029ADD60052CEEF0029AD
      D60029ADD6000000000000000000089418000000000008841000008C0800008C
      0800FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700F7DEC600F7DEC600FFEF
      D600E7DEC600C6BDAD00B584840000000000088DBB008AF1FA0059E7FC0035D6
      F80038D9F9006FEAFC008DEEF90031BFE3000E9DCA000F80AA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052CEEF009CFFFF009CFF
      FF005ACEEF006BE7FF0000000000000000000000000000000000000000000000
      00000000000000000000000000001084180000000000008C0800008C0800FFFF
      FF00FFC67300FFC67300EFB57300EFB57300EFA54A00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B584840000000000088DBB0091EEF80082EEFA006FEA
      FC007BECFB0097EEF800ABFAFD0063E7FB0012A3CF000F80AA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005ACEEF005ACEEF0029AD
      D60029ADD60052CEEF00000000000000000000000000000000000000000029AD
      D600108418000000000031AD52000894180000000000008C0800E7C6B500FFFF
      FF00FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B0000000000088DBB00DEFAFD00DEFAFD00D3FA
      FD00B2F9FD00AEFAFD00AEFAFD00ABFAFD0074ECFC000F80AA00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000008400000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000089418000894180031AD5200000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE9473000000000000000000000000000991BF00DEFAFD00DEFA
      FD00C2F9FD00AEFAFD00ABFAFD00A7F7FC000C85B10000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000021AD390039C65A0010841800000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000088DBB00088D
      BB00088DBB00088DBB00088DBB00088DBB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000089418000894180010841800108418000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001EA1CD002CAAD3001585B000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00000000000000000000000000B552180084522100844218008442
      1800844218008442180084421800844218008442180084421800844218008442
      18008442180084421800844218008442180000000000000000000000000027A5
      CF0023A4CE001585B0001E98C2002CABD3002CABD300178AB5001587B1002CAB
      D30029A7D100000000000000000000000000078DBE0062CBF200078DBE00A4F6
      FD0066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CE
      F5003AAFDA00ABF2FC00078DBE0000000000078DBE0025A0CD005FC8EE0083E1
      FB0066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CD
      F4003AAED8001495C4000000000000000000B552180052392900523929005239
      2900523929005239290052392900523929005239290052392900523929005239
      29005239290052392900523929008442180000000000000000000000000027A5
      CF002CABD3002CABD3001DA0CC0020D0F8004AD7F70027A5CF005EC1DC0064C1
      DA0027A5CF00000000000000000000000000078DBE006BD2F700078DBE00A9F3
      FC006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4
      F8003AAFDA00BEECF400078DBE0000000000078DBE004CBBE30031A8D30095EC
      FB006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4
      F9003FB1DB00C8F6FB00078DBE0000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008442180000000000178CB6001E98C2004FC2
      E1007FDFF5004FC2E1003ACBEF001DD1FA003CD5F8005DD9F60074C1D600AEE2
      ED0091E3F500178AB5000000000000000000078DBE0072D6FA00078DBE00ABF2
      FC0079DCFA0079DCFA0079DCFA0079DCFA0079DCFA007ADDFB0079DCFA0079DC
      FA0045B6DF00BEECF400078DBE0000000000078DBE0072D6F900078DBE00ACF8
      FD007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADB
      FA0043B5DD00C8F6FB00078DBE0000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084421800000000005EC1DC002CABD3004FC2
      E10096E4F5008BE2F5005DD9F60027D0F7001DD1FA0056D9F70076DEF5007FDF
      F5003ACBEF002CABD30027A5CF0000000000078DBE007ADDFB00078DBE00B4F3
      FC0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4
      FB0045B6DF00C3F0F700078DBE0000000000078DBE007CDDFA001495C40095EC
      FB0092EAFB0086E3FB0083E1FB0083E1FB0086E3FB0083E1FB0083E1FB0086E3
      FB0049B8E000C8F6FB001495C40000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084421800000000001B91BC0023A4CE004FC2
      E1007FDFF50091E3F50070DCF5005EC1DC004FC2E1002DCFF4001DD1FA001DD1
      FA001DD1FA0020D0F8002CABD30000000000078DBE0081E2FB00078DBE00BEF4
      FC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EEC
      FC004CBCE400BEF4FC00078DBE0000000000078DBE0083E1FB0043B5DD0059C4
      EA00ACF8FD008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9
      FB004CBBE300C8F6FB00C8F6FB00078DBE00B55218000000000046464600FFFF
      FF0046464600464646000808080000000000464646006B6B6B006B6B6B006B6B
      6B006B6B6B00000000000000000084421800000000001A8FBA002CABD30045C7
      E9006BDBF60089BECD00979696009796960097969600979696004FC2E1001DD1
      FA001DD1FA001DD1FA001D9EC90000000000078DBE008BEAFC00078DBE00D2F7
      FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6
      FC00BADADF00D2F7FC00078DBE0000000000078DBE008CE7FB0078DAFA0025A0
      CD00E5F8FA00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6
      FB0095ECFB00E5F8FA00CFF6FA00078DBE00B552180000000000FFFFFF000808
      0800000000000000000000000000FFFFFF004646460000000000000000000000
      000000000000000000000000000084421800000000001B91BC002CABD3002CAB
      D3005DD9F60097969600E2DFE300B0B4B700ACA3A200BEB2B2009796960056D9
      F7009DE3F200AEE2ED001E98C20000000000078DBE0094F1FD00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE0000000000078DBE0096F0FC0096F0FC001495
      C400078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00B552180000000000FFFFFF000000
      000000000000FFFFFF0008080800FFFFFF000808080000000000000000000000
      00000000000000000000000000008442180000000000178CB6001EA1CD002CAB
      D30045C7E90097969600E2DFE300B0B4B700ACA3A200BEB2B200979696007FDF
      F500AEE2ED0089BECD0027A5CF0000000000078DBE009BF4FD009BF4FD009BF4
      FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4
      FD00078CBD00000000000000000000000000078DBE009CF5FD009AF4FD009AF4
      FD009CF5FD009DF6FD009AF4FD009CF5FD009AF4FD009CF5FD009AF4FD009AF4
      FD00088DBE00000000000000000000000000B55218000000000046464600FFFF
      FF00464646000000000000000000080808000000000000000000000000000000
      00000000000000000000000000008442180000000000000000001585B00021A3
      CE003ACBEF0097969600E2DFE300B0B4B700ACA3A200BEB2B2009796960067DA
      F60064C1DA001585B0000000000000000000078DBE00D2F7FC00A0F6FD00A0F6
      FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7
      FD00078CBD00000000000000000000000000078DBE00E5F8FA00A1F9FE00A1F9
      FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9
      FE00088DBE00000000000000000000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000844218000000000000000000000000001589
      B4001A8FBA0097969600E2DFE300B0B4B700ACA3A200BEB2B200979696001E98
      C2001B91BC0000000000000000000000000000000000078DBE00D2F7FC00A4F6
      FD00A4F6FD00A4F6FD00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE000000000000000000000000000000000000000000078DBE00E5F8FA00A4F9
      FE00A4F9FE00A4F9FE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE0000000000000000000000000000000000B552180029211800292118002921
      1800292118002921180029211800292118002921180029211800292118002921
      1800292118002921180029211000844218000000000000000000000000000000
      00000000000097969600E2DFE300BAAFAE00AAA1A100BEB2B200979696000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5521800EAC07800EAC07800EAC0
      7800EAC07800EAC07800EAC07800EAC07800EAC07800EAC07800EAC07800EAC0
      7800EAC07800EAC07800EAC07800844218000000000000000000000000000000
      00000000000097969600B0B4B700A6A0A00098969600A8A0A000979696000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5521800E78C3100E78C3100E78C
      3100E78C3100E78C3100E78C3100E78C3100E78C3100E78C3100E78C3100EF9C
      4A00EF9C5200D68C5A006B7BC600844218000000000000000000000000000000
      00000000000097969600E0E0E300BEB2B200A6A0A000B0A5A400979696000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A3522800A3522800A352
      2800A3522800A3522800A3522800A3522800A3522800A3522800A3522800A352
      2800A3522800A3522800A3522800000000000000000000000000000000000000
      00000000000097969600E2DFE300E2DFE300B9B3B400A39E9E00979696000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000097969600979696009796960097969600000000000000
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
      00000000000000000000000000000000000000000000636B7300C69C94000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC670100CC670100CC67
      0100CC670100CC670100CC670100CC670100CC670100CC670100CC670100CC67
      0100CC670100CC670100CC670100CC6701000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000082818A00346E9D00B991
      9200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006394B500218CEF002173B500CE9C
      9400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFAF500FFF3E600FEEBD500FEE3C300FEDCB500FED7AB00FED7AB00FED7
      AB00FED7AB00FED7AB00FED7AB00CC6701000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B88C2002E8BDF006481
      9D00BC8E96000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004AB5FF0042A5FF00218CEF007B84
      9400CE9C94000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00FFFAF500FFF3E600FEEBD500FEE3C400FEDCB50000C0C00000C0
      C00000C0C00000C0C000FED7AB00CC6701000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004EB2FD0061BDFC002E8B
      DF00247DC900B991920000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052BDFF005ABDFF00218C
      EF002173B500CE9C940000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFAF500FFF3E600FFEBD500FEE3C400FFFFFF00FFFF
      FF00FFFFFF0000C0C000FED7AB00CC6701001894CE001894CE001894CE001894
      CE00000000000000000000000000000000000000000000000000000000000000
      00000073080000000000000000000000000000000000000000004EB2FD004EB2
      FD002E8BDF0064819D00B9919200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000031A5FF0052BDFF0042A5
      FF00218CEF0084849400CE9C9400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFAF500FFF3E600FFEBD500FEE3C400FEDC
      B500FED7AB00FED7AB00FED7AB00CC6701001894CE00ADF7FF007BF7FF006BEF
      F70031B5DE0031B5DE0031B5DE001894CE001894CE001094CE00000000000073
      08005AE78C000073080000000000000000000000000000000000000000004EB2
      FD0061BDFC002E8BDF00217AC200000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000052BD
      FF005ABDFF003184D60052636B00000000009C6B6B0000630000007300000073
      0800427B310000000000000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFAF500FFF3E600FFEBD500FEE3
      C400FEDCB500FED7AB00FED7AB00CC6701001894CE0084DEEF0094FFFF007BF7
      FF0084F7FF0084F7FF007BF7FF006BE7FF0063DEF70031B5DE000073080052D6
      7B0042D66B0031C64A0000730800000000000000000000000000000000000000
      00004EB2FD004EB2FD002E8BDF000000000000000000AE837E00AE837E00AE83
      7E00AE837E0000000000000000000000000000000000000000000000000031A5
      FF0052BDFF00C6CEDE009C9C9C00AD8C84004284310084A55A0063944200109C
      290021AD42004A944200000000000000000000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00808080008080800080808000FFFFFF0080808000FFF3E6008080
      80008080800080808000FED7AB00CC6701001894CE0063C6E70094FFFF0073F7
      FF007BF7FF0073EFFF0073EFFF0073EFFF007BEFFF007BF7FF0052C6E700108C
      210031C64A00109C210000000000000000000000000000000000000000000000
      0000000000000000000000000000B9919200E3C5A200FEFEDD00FEFEDB00FEFE
      DB00FEFEDD00E1CAB200AE837E00000000000000000000000000000000000000
      000000000000C6ADAD00CEA59400FFE7BD00FFF7CE00FFFFD600FFFFD600FFFF
      DE001084210031BD5200088418000000000000000000CC6701008080FF000000
      FF008080FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFAF500FFF3
      E600FFEBD500FEE3C400FEDCB500CC6701001894CE001894CE0084E7F7007BF7
      FF007BF7FF0073EFFF0073EFFF0073EFFF0073EFFF0073EFFF0052C6E7000884
      100018AD29000884100000000000000000000000000000000000000000000000
      0000000000000000000000000000E0C5A700FDF3C700FEFEDA00FEFEDA00FEFE
      E000FEFEE500FEFEFA00E1CAB200000000000000000000000000000000000000
      000000000000BD8C8C00EFDEB5008CB57B00FFF7CE00FFFFD600FFFFDE00FFFF
      EF00108418004AE77B00087B18000000000000000000CC6701000000FF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFA
      F500FFF3E600FFEBD500FEE3C400CC6701001894CE005AD6EF001894CE0094FF
      FF0073F7FF007BEFFF0073EFFF0073EFFF0073EFFF0073EFFF0052C6E700088C
      100008A518000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00F1D7AD00EFC99900FEFEDB00FEFEDA00FEFE
      F100FEFEFA00FEFEF500FEFEE900AE837E000000000000000000000000000000
      000000000000006300001884210052EF840010841800FFFFD600528C390021A5
      420042D66B0052F78C004AE77B002184310000000000CC6701008080FF000000
      FF008080FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFAF500FFF3E600FFEBD500CC6701001894CE007BF7FF001894CE0073CE
      EF007BDEEF007BDEEF0094EFFF0084F7FF006BEFFF006BEFFF00088C100008A5
      1800088410000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00EFC79600F0C08D00FEFDD700FEFEDC00FEFE
      EE00FEFEF700FEFEEE00FEFEE000AE837E000000000000000000000000000000
      000000000000107B180052E77B0052F78C0039CE6B00FFFFD600FFFFDE001073
      100039CE6B005AF78C0052E77B005294420000000000CC670100FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFAF500FFF3E600CC6701001894CE008CFFFF0063E7F7001894
      CE001894CE001894CE005AC6E70000730800087B0800088C1000088C1000087B
      080073EFFF0018A5D60000000000000000000000000000000000000000000000
      00000000000000000000AE837E00EFC99900F0BE8D00F6DFB400FDF7CE00FEFE
      DD00FEFEDD00FEFEDB00FEFEDA00AE837E000000000000000000000000000000
      00001084180010841800108421004AE77B00108418001084180010841800FFFF
      D6009CBD8C001084180052943900C694940000000000CC670100CC670100CC67
      0100CC670100CC670100CC670100CC670100CC670100CC670100CC670100CC67
      0100CC670100CC670100CC670100CC6701001894CE0084FFFF0084FFFF0084FF
      FF007BF7FF007BF7FF001894CE001894CE006BCEEF0094E7F70084DEEF0094E7
      F700A5F7FF0039ADDE0000000000000000000000000000000000000000000000
      00000000000000000000AE837E00F8E4B900EFC79600EFC99900F7E2B600FEFE
      DA00FEFEDC00FEFEDB00FEFEDC00AE837E000000000000000000000000000000
      000000000000D6ADA500087B180042DE730010841800F7D6A500F7E7BD00FFFF
      D600FFFFD6009CB58400FFFFD600000000000000000000000000CC670100CC67
      0100CC670100CC670100CC670100CC670100CC670100CC670100CC670100CC67
      0100CC670100CC670100CC670100000000001894CE008CFFFF007BF7FF007BF7
      FF007BF7FF0084F7FF0084FFFF0073F7FF001894CE001894CE001894CE001894
      CE00299CCE001894CE0000000000000000000000000000000000000000000000
      0000000000000000000000000000E1CAB200FEFEFA00F8E4B900F0C08D00EFC3
      9200EFC99900FDF3C700DEC6AE00000000000000000000000000000000000000
      00000000000000000000C69C8C0031C6630021AD4200F7C69400EFBD8400F7C6
      9400F7D6AD00FFEFC600BD9C8C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001894CE0084E7F70094FFFF008CFF
      FF0084FFFF0063D6EF001894CE001894CE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AE837E00DFC7B100FEFDD700F3DAAF00EFC9
      9900F2D8AD00DEC5AB00AE837E00000000000000000000000000000000000000
      0000000000000000000000000000087B180021AD4200107310006394420084A5
      630042843100C69C940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001894CE001894CE001894
      CE001894CE001894CE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AE837E00AE837E00AE83
      7E00AE837E000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C69C8C00CEA59C00C69C
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000C6A59C0069F3
      FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3
      FF0069F3FF007FC7E900B5848400000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      00000000000000000000000000003131C6003131C6003131A5003131A5000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000C6A59C0069F3
      FF00E88E6000E88E6000E88E6000E4753E00E4753E00E4753E00E1622300E162
      2300E16223007FC7E900B5848400000000000000000000000000C6A59C00FFEF
      D600C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B5848400000000000000000000000000000000000000
      000000000000000000003139FF003139FF003139FF003131E7003131E7003131
      A50000000000000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000C6ADA5007FC7
      E90029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029AD
      D60029ADD60029ADD600B5848400000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000000000000000
      0000000000003139FF003139FF003139FF003139FF003139FF003131E7003131
      E7003131A500000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000000000000000
      0000000000003139FF006363FF003139FF003139FF003139FF003139FF003131
      E7003131A500000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000CEB5AD00FFFF
      F700FFEFE700F7E7D600F7E7D600F7E7CE00F7DEC600F7DEBD00F7DEC600F7DE
      C600EFCE9C00EFCE9C00B5848400000000000000000000000000CEB5AD00FFFF
      F700C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B584840000000000CE6B00007B3908007B3908007B39
      08007B3908006363FF006363FF003139FF003139FF003139FF003139FF003139
      FF003131A500000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000D6B5AD00FFFF
      FF00FFCD9100FFCD9100FFC37D00FFC37D00FFC37D00FFBA6900FFBA6900FFBA
      6900F7D6A500F7D6A500B5848400000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B584840000000000CE6B0000F7941000CE630000CE63
      0000CE6300006363FF009C9CFF006363FF003139FF003139FF003139FF003139
      FF003131A500004A0000004A0000004A0000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B5848400000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B58484000000000000000000CE6B0000F7941000E76B
      0000E76B0000E76B00006363FF009C9CFF006363FF003139FF003139FF003131
      C600007B0800007B0800007B0800004A0000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000D6BDB500FFFF
      FF00FFFFFF00FFF7F700FFF7EF00FFEFE700F7E7D600F7E7CE00F7DEC600F7DE
      BD00F7DEB500F7DEB500B5848400000000000000000000000000D6BDB500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00F7DEB500B5848400000000000000000000000000CE6B0000F794
      1000E76B0000CE6300007B3908006363FF003139FF003139FF003139FF00009C
      0800009C0800009C0800007B0800004A000000000000000000003184FF003184
      FF003184FF00F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F700F7F7F700B5848400000000000000000000000000DEBDB500FFFF
      FF00FFCD9100FFCD9100FFC37D00FFC37D00FFC37D00FFBA6900FFBA6900FFBA
      6900F7DEC600F7D6B500B5848400000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B584840000000000000000000000000000000000CE6B
      0000F79410007B390800000000000873100042C67300009C0800009C0800009C
      0800009C0800009C0800007B0800004A000000000000000000003184FF0042B5
      F7003184FF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEF
      EF00EFEFEF00EFEFEF00B5848400000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000000000000000
      0000CE6B000000000000000000000873100042C67300009C0800009C0800009C
      0800009C0800009C0800007B0800004A00003184FF003184FF003184FF0042B5
      F7003184FF003184FF003184FF00E7E7E700E7E7E700E7E7E700E7E7E700B584
      7300B5948C00B58C8400B5848400000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7EF00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000E7C6B500FFFF
      FF00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      00000000000000000000000000000873100042C67300009C0800009C0800009C
      0800009C0800009C0800007B0800004A00003184FF008CD6F700B5DEF700B5DE
      F700B5DEF7008CD6F7003184FF00DEDEDE00DEDEDE00DEDEDE00C6C6C600B584
      7300FFFFFF00FFFFFF00B5848400000000000000000000000000E7C6B5007FC7
      E90029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6002183A700BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      00000000000000000000000000000873100042C67300009C0800009C0800009C
      0800009C0800009C0800007B0800004A00003184FF003184FF003184FF00B5DE
      F7003184FF003184FF003184FF00D6D6D600D6D6D600D6D6D600C6C6C600B584
      7300FFFFFF00B584840000000000000000000000000000000000E7C6B50069F3
      FF00E88E6000E88E6000E88E6000E88E6000E4753E00E1622300B04A1800C694
      7B00FFC67300CE94730000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      00000000000000000000000000000873100042C6730042C6730042C6730042C6
      730042C6730042C6730042C67300004A000000000000000000003184FF00B5DE
      F7003184FF00CECECE00CECECE00CECECE00CECECE00CECECE00C6C6C600B584
      7300B58484000000000000000000000000000000000000000000E7C6B50069F3
      FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3FF0069F3FF007FC7E900C694
      7B00CE9C84000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000087310000873100008731000087310000873
      10000873100008731000087310000873100000000000000000003184FF003184
      FF003184FF00B5847300B5847300B5847300B5847300B5847300B5847300B584
      7300000000000000000000000000000000000000000000000000EFCEB500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500E7C6B500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D5820000D582
      0000D5820000D5820000D5820000D5820000D5820000D5820000D5820000D582
      0000D5820000D582000000000000000000000000000000000000D5810000D581
      0000D5810000D5810000D5810000D5810000D5810000D5810000D5810000D581
      0000D5810000D581000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5820000FEEAD500FDE8
      D100FCE7CF00FBE4CB00FBE1C400FBDFC000FBDEBD00FBDDBC00FBDAB500FBDA
      B500FBD8B200FBD8B200D58200000000000000000000D5810000FEF7EF00FDED
      DC00FDEAD600FDE8D100FDE7CE00FEE4C700FEE3C400FEE1C200FDDFBC00FDDE
      BB00FDDDBA00FEE1C200D5810000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5820000FEEDDA00FEEB
      D700CF834400CF834400CF834400CF834400CF834400CF834400CF834400CF83
      4400FBDAB500FBD8B200D58200000000000000000000D5810000FDF2E600FDE9
      D300FDE5CA00FDE5CA00FEE4C700FDE0BE00FDDDBA00FDDEBB00FDDFBC00FDDD
      BA00F7D7B700FDDDBA00D5810000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5820000FEF0E000FEEE
      DC00FEECD800FEEAD500FCE7CF00FBE4CB00FBE1C400FBE0C200FBDEBE00FBDE
      BD00FBDBB700FBDAB500D58200000000000000000000D5810000FDF5EC00FDED
      DC00E7B78A00D2884400D78A4000CE884A00CE884A00D3884200E1A26500F2D0
      AE00FDDDBA00FDDEBB00D5810000000000000000000029ADD60029ADD60029AD
      D6002D2D2D0058534E0029ADD60029ADD60029ADD60029ADD60029ADD60029AD
      D60029ADD60029ADD60029ADD600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5820000FEF2E500FEF0
      E000FEEEDC00F6D1AA00CF844000CF834400D4895700CF834400EEC6A000FBDF
      C000FBDEBD00FBDBB700D58200000000000000000000D5810000FEF8F100FDEF
      E000FEE4C700DB8D3F00D87B0F00E3A76D00EDC6A200DA732000D2710300DF97
      4F00FDDFBC00FDE0BE00D5810000000000000000000029ADD6008CF7FF008CF7
      FF0058534E00454442005160610073DEF70073DEF70073DEF70029ADD60029AD
      D60073DEF70073DEF70029ADD6000000000000000000808080008D5E5D008D5E
      5D008D5E5D008D5E5D008D5E5D008D5E5D008D5E5D008D5E5D008D5E5D000000
      00000000000000000000000000000000000000000000D5820000FEF5EA00FEF2
      E500FEEEDC00D4895700D6875D00FDE9D300FDE8D100DAAF8D00D4895700FBDD
      BC00FBDFC000FBDEBD00D58200000000000000000000D5810000FEFBF600FDF2
      E600FDE8D100E0995100DA732000E8BB9100FDEFE000E1A26500D2710300DA73
      2000FDDFBC00FEE1C200D5810000000000000000000029ADD6008CF7FF008CF7
      FF0073DEF70054777B00477AA90018556F008A5B520073DEF70029ADD600ADFF
      FF0029ADD60073DEF70029ADD600000000000000000080808000FFFAEB00F9F1
      E200F9F0DF00F7EEDC00F7EEDB00F7EDDA00F8EFDB00F2E5D1008E5F5E000000
      000000000000C0000000000000000000000000000000D5820000FEF6EE00FEF5
      EA00FEEBD700D2884900E1B59100FEEDDA00FEEAD500FADBB900D4895700FAD7
      B100FBE1C400FBDFC000D58200000000000000000000D5810000FEFDFA00FEF7
      EF00FDE9D300E19C5800DE8F3D00E4AE7B00EDC6A200D78A4000D2710300E1A0
      6000FEE1C200FEE4C700D5810000000000000000000029ADD6009CFFFF008CF7
      FF008CF7FF0054777B0041749600947E7500C17135008A5B520029ADD600ADFF
      FF00ADFFFF0029ADD60029ADD600000000000000000080808000F9F1E400F5E2
      CD00F5E0CA00F4DFC900F3DEC500F2DDC300F2E2CD00EBDBC9008E5F5E000000
      0000FF000000FF000000C00000000000000000000000D5820000FEF9F300FEF7
      EF00FEEEDD00D2884900DDB28E00FEEFDE00FEEDDA00FADBB900D6875D00FADB
      B900FBE4CB00FBE1C400D58200000000000000000000D5810000FEFEFD00FEF7
      EF00FCEBD900E19E5B00DE903E00CE884A00D3884200D2710300CE884A00FBDC
      BA00FEE3C400FDE5CA00D5810000000000000000000029ADD6009CFFFF009CFF
      FF008CF7FF0073DEF7007C707800F1BC8600F0A85C00C07638008A5B520029AD
      D60029ADD60029ADD60029ADD600000000000000000080808000FCF5EA00FCD8
      B500FCD8B400FCD8B300FBD7B300FBD7B200FADAB800EBDECE0092646200FF00
      0000FF000000FF000000FF000000C000000000000000D5820000FEFAF500FEF9
      F300FEF0E100D3894C00E0B49000FEF1E300FEF0E000FBDFC000D6875D00FBDF
      C000FDE8D100FBE4CB00D58200000000000000000000D5810000FEFEFD00FEFB
      F600F1E9E100E1A06000DE903E00E7B78A00EFCBA900DB8D3F00DA732000E8BB
      9100FDE7CE00FDE7CE00D5810000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF008CF7FF00AD7B7300FBD3A900F9C48D00EFA65A00C07638008A5B
      520073DEF70073DEF70029ADD600000000000000000080808000FEFBF500FDD2
      A700FDD2A700FDD2A700FDD2A700FDD2A700FDD2A700F0E5DA00A57A75000000
      0000FF000000FF000000C00000000000000000000000D5820000FEFCF800FEFB
      F700FEF3E800D3894C00E6BB9600FEF4E900FEF2E500FAE1C800D6875D00FAE1
      C800FEEAD500FDE8D100D58200000000000000000000D5810000FEFEFD00FEFD
      FA00F7F0E900E3A76D00DF924200EDC6A200FDF2E600E19C5800D87B0F00E4AB
      7300FDE8D100FDE9D300D5810000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF008CF7FF00AD7B7300FBD3A900F9C48D00F0A85C00C171
      35008A5B520073DEF70029ADD600000000000000000080808000FFFDFB00FDEA
      D800FCE7D400FBE6D300FAE6D100FDE9D300FDF4E600E8E0D900C0000000C000
      0000FF000000FF000000C00000000000000000000000D5820000FEFCF800FEFC
      F800E4B89300D1833300D4895700FBE5CE00FEF4E900DAAF8D00D0833A00DDB2
      8E00FEECD800FEEAD500D58200000000000000000000D5810000FEFEFD00FEFE
      FD00F2D0AE00E0995100DE903E00E1A26500E3A76D00DE8F3D00DF924200EFCB
      A900FDEAD600FCEBD900D5810000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF009CFFFF008CF7FF00AD7B7300FBD3A900F8C28C00EDA7
      5F00B56D3E008A5B520029ADD600000000000000000080808000FFFFFF00FDD2
      A700FDD2A700FDD2A700FDD2A700EBDFDB00FF000000FF000000FF000000FF00
      0000FF000000FF000000C00000000000000000000000D5820000FEFCF800FEFC
      F800FEFCF800FEFCF800FEFAF400FEF8F100FEF6EE00FEF5EA00FEF2E600FEF1
      E200FEEFDE00FEEDDA00D58200000000000000000000D5810000FEFEFD00FEFD
      FB00EFCBA900EBC19A00EDC6A200EBC19A00E7B78A00EBC19A00F7D7B700FDED
      DC00FDEAD600FDEFE000D5810000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF009CFFFF009CFFFF008CF7FF00AD7B7300FBD3A9009891
      A20035A8F5000316AC0000009A00000000000000000080808000FFFFFF00FCE7
      D400FCE7D400FCE7D400FCE7D400B48E8800B48E8800B48E8800B48E88000000
      00000000000000000000000000000000000000000000D5820000FEFCF800FEFC
      F800FEFCF800FEFCF800FEFCF800FEFAF500FEF8F200FEF7EF00FEF5EA00FEF3
      E800FEF1E200FEEFDE00D58200000000000000000000D5810000FEFEFD00FEFE
      FD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFEFD00FEFCF800FEF7
      EF00FDF3E900FEF9F300D5810000000000000000000029ADD60029ADD60029AD
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD600AD7B73004A9E
      ED001029D6001029D6000316AC0000009A000000000080808000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B48E8800EBB56F00C68C7800000000000000
      0000000000000000000000000000000000000000000000000000D5820000D582
      0000D5820000D5820000D5820000D5820000D5820000D5820000D5820000D582
      0000D5820000D582000000000000000000000000000000000000D5810000D581
      0000D5810000D5810000D5810000D5810000D5810000D5810000D5810000D581
      0000D5810000D581000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000018
      C6006D8AFD00106BFF001029D600000000000000000080808000808080008080
      8000808080008080800080808000808080008080800000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000018C6000018C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008543220085432200854322008543220085432200854322000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008543
      220085432200C9660100C9660100C9660100C9660100C9660100C96601008543
      2200854322000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A00000000000000000000D6820000D682
      0000D6820000D6820000D6820000D6820000D6820000D6820000D6820000D682
      0000D6820000D6820000000000000000000000000000000000009A4E1800C662
      0100C9650000C9650000C9650000C9650000C9650000C9660100C9660100C966
      0100C76301008543220000000000000000000000000000000000000000000000
      00000000000000000000A5947B00AD847B009C636300BD734A00000000000000
      00000000000000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFF700FFEFDE00FFE7CE00FFDEB500FFD6A500FFCE9400FFCE9400FFCE
      9400FFCE9400FFCE9400FFCE9400BD4A000000000000D6820000FEF7EE00FEED
      D800FEEBD400FDE8D000FDE8D000FCE3C800FDE1C300FEE0BE00FEDEBB00FDDD
      B800FDDDB800FDE1C100D682000000000000000000009A4E1800C6620100C763
      0100C6620100C6620100C6620100C6620100C7630100C9640100C9660100C966
      0100C9660100C763010085432200000000000000000000000000000000000000
      000000000000A57B6300BD633900C6634200C66342009C52420084736B000000
      00000000000000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFEFDE00FFE7CE00FFDEB500FFD6A500FFCE9400FFCE
      9400FFCE9400FFCE9400FFCE9400BD4A000000000000D6820000FEF3E500FEEA
      D200FDE8D000FCE2C600FDE1C300FDE1C100FDDCB800FDDCB700FDDBB500FBD9
      B500FAD7B300FDDDB800D68200000000000000000000BD5F0800C6620100C360
      0200C3600200CB741D00D58D4500DB9B5A00DB9B5A00D2863A00C9640100C966
      0100C9660100C966010085432200000000000000000000000000000000000000
      0000BD734A00C65A1800A54218008C522100B5421800BD522900CE6339008442
      31000000000000000000000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFF7008C2900008C2900008C290000FFD6A5000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6820000FEF5EA00FEED
      D800FEEED900ECC29C00D9814200D7823700E2B08300FEDEBB00FEDDB900FDDB
      B500FBD9B500FEDDB900D682000000000000A7541200C9680600C9680600C968
      0600DB9B5A00FBFAF900FBFAF900FBFAF900FBFAF900E9C39B00C6620100C964
      0100C9660100C9660100C966010085432200000000000000000000000000BD73
      4A00BD521000A5390000AD420000316300006B4A0000AD390000A5390800BD5A
      39007352420000000000000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFFF008C2900008C2900008C290000FFDEB5000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6820000FEF8F100FEEF
      DB00FEEDD800FEECD600DBAE8500D3801F00F1CDAA00FCE2C600FEE0BE00FDDD
      B800FDDBB500FEE0BE00D682000000000000AC571100CB701500CB701500CB74
      1D00FBFAF900E9C09600CE7B2800CB741D00CB701500BD5F0800C3600200C763
      0100C9650000C9660100C9660100854322000000000000000000BD734A009C52
      4200AD420000CE5A0000BD6B0000007B000021730000C65A0000B54A00007363
      2100845A310073524200000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFFF008C2900008C2900008C290000FFE7CE000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6820000FEFBF600FEF1
      E100FEEFDB00FEF1DF00F3D0AD00D2802700E8B98E00FDE8D000FDE1C100FEDE
      BB00FDDDB800FEE0BE00D682000000000000AC571100D0813100D0813100CE7B
      2800FBFAF900D2863A00CE7B2800CB701500C9680600E9C39B00C15F0400C662
      0100C9650000C9660100C9660100854322000000000000000000BD734A009C52
      4200CE5A0000D67B0000848C000039940000C6840000BD7B000021730000086B
      0800636B2900BD734A00000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFEFDE00FFE7
      CE00FFDEB500FFD6A500FFCE9400BD4A000000000000D6820000FEFDFA00FEF3
      E500FEF1E100FEF2E200FCE5CC00D9814200E0AF8300FDE7CF00FCE2C600FDE1
      C100FEDEBB00FDE1C300D682000000000000AC571100DEA26500D9965200D58D
      4500FBFAF900E5B58300D2863A00CE7B2800CB701500FBFAF900E9C09600C360
      0200C9650000C9660100C9660100854322000000000000000000BD734A009C52
      4200C673000029A5180031AD2100D6AD1800FF9C0800E78C0000318400000073
      0000426B1000636B2900000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFF7000084
      00000084000000840000FFD6A500BD4A000000000000D6820000FEFDFC00FEF6
      EC00FEF3E500FEF2E200FEF1E100E2B08300DB7C4F00FCE1C400FDE8D000FCE2
      C600FDE1C100FCE4CA00D682000000000000AC571100E5B58300E4B17D00D996
      5200F1D5B900FBFAF900F6E5D300F6E5D300F7EADD00FBFAF900FBFAF900EECF
      AF00C9650000C9660100C96601008543220000000000000000009C524200BD73
      4A007B94080010C64A0031CE63007BCE6B00FFCE5A00FFA510006B8C00000084
      0000296B0000296B0000000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFFF000084
      00000084000000840000FFDEB500BD4A000000000000D6820000FEFDFC00FEF8
      F100FEF6EC00FEF3E500FEF5EA00EDC5A000D9814200F3D0AD00FEECD600FCE4
      CA00FCE2C600FDE7CF00D682000000000000AC571100E4B17D00F1D5B900E4B1
      7D00E0A76D00E9C39B00F1D5B900EECFAF00F4DFC900FBFAF900FBFAF900ECCA
      A700C9650000C9660100C9660100854322000000000000000000A5947B00BD73
      4A0031BD390042D67300A5E79C00A5F7B500D6E79400D6B52900B58C00003184
      00005A7B2900A5947B00000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFFF000084
      00000084000000840000FFE7CE00BD4A000000000000D6820000FEFDFC00FEFA
      F400FEF7EF00FEF5EA00FEF6EC00FCE5CC00DB7C4F00EBC09800FEF1DF00FDE8
      D000FCE4CA00FEEAD200D68200000000000000000000B25B1000F2D9C000F2D9
      C000E7B98A00E0A76D00DB9B5A00D58D4500D0813100FBFAF900E8BC8F00CA66
      0200C9650000C96601008543220000000000000000000000000000000000A594
      7B0084C6730063DE7B00BDF7AD00D6FFBD005ADE840039B53100F78C0000C684
      0800A5947B0000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFEFDE00BD4A000000000000D6820000FEFDFC00FEFB
      F800FEFAF400FEF7EF00FEF7EF00E8DCCF00DB7C4F00DB7C4F00F6D4B100FEEE
      D900FDE8D000FEECD600D68200000000000000000000AC571100E5B58300F6E5
      D300F4DFC900E7B98A00DC9E5F00D58D4500D0813100E8BC8F00CB741D00C968
      0600C9660100C763010085432200000000000000000000000000000000000000
      0000A5947B00ADC68C00A5D6840094DE8C0052CE63004AB53900D69C4200A594
      7B000000000000000000000000000000000000000000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A000000000000D6820000FEFDFC00FEFD
      FA00FEFBF800FEFAF400FEF9F200E1D4C600EABE9500E6B68A00F3D0AD00FEF1
      DF00FEEBD400FEF0DD00D6820000000000000000000000000000AC571100E7B9
      8A00F4DFC900F6E5D300EECFAF00E7B98A00E2AD7700DC9E5F00D58D4500CB70
      1500C76301008E471E0000000000000000000000000000000000000000000000
      000000000000A5947B00A5947B00AD847B00A5947B00A5947B00A5947B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6820000FEFDFC00FEFD
      FC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFC00FEFDFA00FEF6
      EC00FEF4E800FEF9F200D682000000000000000000000000000000000000AC57
      1100B25B1000E8BC8F00ECCAA700ECCAA700E8BC8F00DEA26500CE7B2800B25B
      1000884421000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6820000D682
      0000D6820000D6820000D6820000D6820000D6820000D6820000D6820000D682
      0000D6820000D682000000000000000000000000000000000000000000000000
      000000000000AC571100B25B1000B25B1000B25B1000B25B10009A4E18000000
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
      0000000000008342230083422300834223008342230083422300834223000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      00000000000000000000E7C4B300E7C4B300E7C4B300D6B9AE00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008342
      230083422300C9650000C9650000C9650000C9650000C9650000C96500008342
      2300834223000000000000000000000000000000000000000000000000000000
      0000CC680200CC680200CC680200CD6904009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C000000000000000000000000000000000000000000000000000000
      000000000000E7C4B300F2E7E100CAD0D600CAD0D600F2E7E100D6B9AE000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009D4F1700C361
      0300C9650000C9650000C9650000C9650000C9650000C9650000C9650000C965
      0000C66301008342230000000000000000000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C000000000000000000000000000000000000000000000000000000
      0000E7C4B300F2E7E100FEFEFE00D0805D00C4695200BFB9B900F2E7E100D6B9
      AE00000000000000000000000000000000000000000000000000000000000000
      00000000000000630000089C210000630000004A000000000000000000000000
      00000000000000000000000000000000000000000000994D1900C3610300C964
      0000C6630100C6630100C6630100C6630100C9640000C9640000C9650000C965
      0000C9650000C663010083422300000000000000000000000000000000000000
      0000CC6802000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E7C4
      B300F2E7E100FEFEFE00D0805D00C46E5200BF694C00C4695200BFB9B900F2E7
      E100D6B9AE000000000000000000000000000000000000000000000000000000
      00000000000000630000109C2100089C210000630000004A0000000000000000
      00000000000000000000000000000000000000000000BE5F0600C6630100C361
      0300C3610300BE5F0600D2883E00D8945100DB9B5C00D58E4700CC782300C965
      0000C9650000C965000083422300000000000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C000000000000000000000000000000000000000000E7C4B300F2E7
      E100FEFEFE00D0805D00C46E5200F8EDE700F8EDE700F8E1D600BF694C00BFB9
      B900F2E7E100D6B9AE0000000000000000000000000000000000000000000000
      0000000000000063000010A5290010A52900089C210000630000004A00000000
      000000000000000000000000000000000000A8551200C7680800C7680800C768
      0800C7660600C3610300EAC49E00FBF7F400FBF7F400FBF7F400FBF7F400DB9B
      5C00C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC680200CC680200CC680200CC6802009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C0000000000000000000000000000000000E7C4B300F2E7E100FEFE
      FE00D0805D00C46E5200C46E5200C46E5200F8EDE700C46E5200C46E5200BF69
      4C00BFB9B900F2E7E100D6B9AE00000000000000000000000000000000000000
      0000000000000063000010B5390010A52900089C2100089C210000630000004A
      000000000000000000000000000000000000AB571100C96E1400C9711800C971
      1800C9711800C9711800C9711800C9711800C9711800CA741D00E6B98B00FBF7
      F400C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C0000000000000000000000000000000000EDD0C400FEFEFE00D080
      5D00CA7A5700C46E5200C46E5200C46E5200F8EDE700C46E5200C46E5200C46E
      5200BF694C00BFB9B900D6B9AE00000000000000000000000000000000000000
      0000000000000063000018B54A0010AD390010AD390010AD390010A529000063
      000000520000000000000000000000000000AB571100D0823300CE7D2C00CE7D
      2C00CE7D2C00CE7D2C00EDCDAC00C9711800C76A0C00BE5F0600C96E1400FBF7
      F400C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC6802000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EDD0C400FFFFFF00F8E1
      CA00E1A27400CA7A5700C46E5200C46E5200F8EDE700C46E5200C46E5200C46E
      5200D0805D00CAD0D600D6B9AE00000000000000000000000000000000000000
      0000000000000063000052BD52005ABD52004ABD52004ABD52004ABD52000063
      000000520000000000000000000000000000AE5A1200DC9E6000D8945100D288
      3E00D58E4700EFD0B100FBF7F400CE7D2C00C9711800C7680800DB9B5C00FBF7
      F400C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC6802000000000000000000000000009C3B0C009C3B0C009C3B0C009C3B
      0C009C3B0C0000000000000000000000000000000000E7C4B300F2E7E100FFFF
      FF00F8E1CA00E1A27400CA7A5700F8EDE700F8EDE700C46E5200C46E5200D080
      5D00E7EDED00F2E7E100D6B9AE00000000000000000000000000000000000000
      000000000000006300005AC65A006BC66B006BC663004ABD520000630000004A
      000000000000000000000000000000000000AB571100E5B58400E3AE7900D894
      5100F4DFC900FBF7F400FBF7F400F8EDE200F5E2CF00F4DFC900FBF7F400E6B9
      8B00C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC680200CC680200CC680200CC6802009C3B0C00F89A9400EBA15E00DA84
      2C009C3B0C000000000000000000000000000000000000000000E7C4B300F2E7
      E100FFFFFF00F8E1CA00E1A27400CA7A5700E7AE8500C46E5200D0805D00FEFE
      FE00F2E7E100E1C4B30000000000000000000000000000000000000000000000
      000000000000006300006BC66B007BD67B007BD67B0000630000004A00000000
      000000000000000000000000000000000000A8551200E5B58400F0D3B500E3AE
      7900F5E2CF00FBF7F400FBF7F400F5E2CF00EDCDAC00EBC7A200DC9E6000C663
      0100C9650000C9650000C9650000834223000000000000000000000000000000
      0000CC6802000000000000000000000000009B3A0C009C3B0C009C3A0C009B3A
      0C009C3B0C00000000000000000000000000000000000000000000000000E7C4
      B300F2E7E100FFFFFF00F8E1CA00E1A27400F8EDE700D0805D00FEFEFE00F2E7
      E100E1C4B3000000000000000000000000000000000000000000000000000000
      0000000000000063000063C663008CD68C0000630000004A0000000000000000
      00000000000000000000000000000000000000000000AE5A1200F2DAC200F2DA
      C200E7BB8E00F1D6BB00FBF7F400D58E4700D0823300CC782300C96E1400C865
      0400C9650000C9650000834223000000000000000000000000009C3B0C009C3B
      0C009C3B0C009C3B0C009C3B0C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E7C4B300F2E7E100FFFFFF00F8E1CA00D0805D00FEFEFE00F2E7E100E1C4
      B300000000000000000000000000000000000000000000000000000000000000
      0000000000000063000039AD390000630000004A000000000000000000000000
      00000000000000000000000000000000000000000000A8551200E5B58400F6E5
      D400F4DFC900E7BB8E00EFD0B100D58E4700D0823300CC782300C9711800C768
      0800C9650000C6630100834223000000000000000000000000009C3B0C00F89A
      9400EBA15E00DA842C009C3B0C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7C4B300F2E7E100FFFFFF00FEFEFE00F2E7E100E1C4B3000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AB571100E6B9
      8B00F4DFC900F6E5D400EFD0B100E6B98B00DFA66D00DC9E6000D58E4700C96E
      1400C66301008E481E00000000000000000000000000000000009B3A0C009C3B
      0C009C3A0C009B3A0C009C3B0C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E7C4B300E7C4B300E1C4B300E1C4B300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AB57
      1100B25C1100E7BB8E00ECC9A700ECC9A700E7BB8E00DC9E6000CE7D2C00B25C
      11008A4620000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AE5A1200B25C1100B25C1100B25C1100AE5A12009D4F17000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000A70D800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000076DD400197EE400076DD400076DD40000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE6300001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      200000000000000000000000000000000000000000000000000018CEF60019CB
      F70000000000000000000000000000000000000000000000000000000000076D
      D4002A94F3002BACF900076DD4000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFF700FFF7E700FFEFD600FFE7C600FFD6AD00FFD6AD00F7D6AD00FFD6
      AD00FFD6AD00FFD6AD00FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000018CE
      F60019CBF7000000000000000000076DD400076DD400076DD400076DD4002790
      F0002CA4F900076DD400000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6
      C600BDC6C600BDC6C600FFD6AD00CE63000000000000000000002066AB000000
      0000000000000000000000000000006600000066000000660000006600000066
      0000006600000000000000000000000000000000000000000000000000001B63
      AA00000000000000000000000000006600000066000000660000006600000066
      0000006600000000000000000000000000000000000000000000000000000000
      000000000000000000000A70D8000E73DB002790F0004FCBF3004FCBF300238B
      EE00076DD40000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF002D2D2D0058534E00AAAAAA00FFF7E700FFE7C600FFE7C600FFD6AD00FFD6
      AD00F7D6AD00FFD6AD00FFD6AD00CE63000000000000000000002066AB002066
      AB000000000000000000000000000066000044B65D0030AB47001E9E2F001593
      23000066000000000000000000000000000000000000000000001B63AA001B63
      AA000000000000000000000000000066000036B0650026A144001A942C00158B
      2100006600000000000000000000000000000000000000000000000000000000
      00000000000010635D001176DE002CA4F9001F86E9002E9EF8002BACF9002E9E
      F800076DD40000000000000000000000000000000000CE630000FFFFFF00BDC6
      C60058534E0045444200516061009B9B9B00BDC6C600BDC6C600BDC6C600BDC6
      C600BDC6C600BDC6C600FFD6AD00CE6300002066AB002066AB004393D8004393
      D8002066AB000000000000000000006600000066000000660000006600000066
      000000660000000000000000000000000000000000001B63AA005FBBBF005FBB
      BF001B63AA001B63AA0000000000006600000066000000660000006600000066
      0000006600000000000000000000000000000000000000000000000000000000
      00000B8813001A9A2A0010635D002790F0004FCBF300197EE4001F86E9002D98
      F600076DD40000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF0054777B00477AA90018556F008A5B5200AAAAAA00FFE7C600FFE7
      C600FFD6AD00FFD6AD00FFD6AD00CE6300002066AB00A9C8B40092C79F004393
      D8004393D8002066AB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B63AA00B6D8EA007FCAC4005FBB
      BF005FBBBF001B63AA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000674
      080012911E004EB47A002EBB520010635D001F86E9002E9EF8001176DE001C82
      E700076DD40000000000000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C60054777B0041749600947E7500C17135008A5B52009B9B9B00BDC6
      C600BDC6C600ADB5BD00FFD6AD00CE6300002066AB002066AB004393D80074C1
      86002066AB000000000000000000006600000066000000660000006600000066
      000000660000006600000066000000660000000000001B63AA00C0D6E3005FBB
      BF001B63AA001B63AA0000000000006600000066000000660000006600000066
      0000006600000066000000660000006600000000000000000000000000000A79
      0D0033C25D001FA3340037C2660012911E0011635C00197EE4002D98F6000E73
      DB00076DD40000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007C707800F1BC8600F0A85C00C07638008A5B5200AAAA
      AA00FFE7C600FFD6AD00FFD6AD00CE63000000000000000000002066AB002066
      AB000000000000000000000000000066000050B9670044B65D0044B65D0039B0
      510029A63F001E9E2F001797260007700B0000000000000000001B63AA001B63
      AA000000000000000000000000000066000036B0650036B0650032AE5C0032AE
      5C0026A14400209A3700168E2400056E08000000000000000000000000000A79
      0D0033C25D001C9D2E000B881300057F08000279030011635C001176DE00076D
      D4000000000000000000000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C600BDC6C600AD7B7300FBD3A900F9C48D00EFA65A00C07638008A5B
      52009B9B9B00BDC6C600FFD6AD00CE63000000000000000000002066AB000000
      0000000000000000000000000000006600000066000000660000006600000066
      0000006600000066000000660000006600000000000000000000000000001B63
      AA00000000000000000000000000006600000066000000660000006600000066
      0000006600000066000000660000006600000000000000000000000000000A79
      0D002AB749001C9D2E0006810B00037B0500027903000279030011635C000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00AD7B7300FBD3A900F9C48D00F0A85C00C171
      35008A5B5200AAAAAA00FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001291
      1E000E8B170009851000037B0500037B0500016E0100016C01000000000018CE
      F60019CBF70000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AD7B7300FBD3A900F8C28C00EDA7
      5F00B56D3E008A5B52009B9B9B00CE6300001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      20000000000000000000000000000000000000000000000000001C9D2E0033C2
      5D00067408000169020001690200016902000169020000000000000000000000
      000018CEF60019CBF700000000000000000000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000AD7B7300FBD3A9009891
      A20035A8F5000316AC0000009A00CE6300001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      2000000000000000000000000000000000001583200015832000158320001583
      2000158320001583200015832000158320001583200015832000158320001583
      20000000000000000000000000000000000000000000137C160033C25D000985
      1000016902000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000AD7B73004A9E
      ED001029D6001029D6000316AC0000009A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000026B0300036F0500027702000169
      0200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000018
      C6006D8AFD00106BFF001029D600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000015952200026B0300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000018C6000018C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000082818A00346E9D00B991
      9200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B88C2002E8BDF006481
      9D00BC8E96000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600848484008484840084848400848484008484840084848400848484008484
      840084848400EFCE9C00B5848400000000000000000000000000C6A59C00FFEF
      D60031C6520029BD4A0010A5310010A5310010A53100008C0800008C0800008C
      0800006B0800EFCE9C00B5848400000000000000000000000000000000004242
      4200000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004EB2FD0061BDFC002E8B
      DF00247DC900B991920000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000004242420000000000424242000000
      00000000000000000000000000000000000000000000000000004EB2FD004EB2
      FD002E8BDF0064819D00B9919200000000000000000000000000000000000000
      0000000000000000000000000000000000000073080000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000000000000000
      0000424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004EB2
      FD0061BDFC002E8BDF00217AC200000000000000000000000000000000000000
      000000000000000000000000000000000000007308000073080000730800FFFF
      F7000063CE000063CE000063CE000063CE000063CE000063CE000063CE000063
      CE000063CE00EFCE9C00B5848400000000000000000000000000CEB5AD00FFFF
      F70073A5FF00004AF700004AF700004AF700004AF700004AF700004AF700004A
      F700004AF700EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004EB2FD004EB2FD002E8BDF000000000000000000AE837E00AE837E00AE83
      7E00AE837E000000000000000000000000000073080000730800007308000073
      080010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7
      FF0010F7FF00EFCE9C00B5848400000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B5848400000000000000000000000000000000008484
      8400000000004242420000000000000000000000000042424200000000004242
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000B9919200E3C5A200FEFEDD00FEFEDB00FEFE
      DB00FEFEDD00E1CAB200AE837E0000000000008C0800008C0800008C0800008C
      080010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7
      FF0010F7FF00F7DEB500B5848400000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E0C5A700FDF3C700FEFEDA00FEFEDA00FEFE
      E000FEFEE500FEFEFA00E1CAB20000000000008C0800008C0800008C0800FFFF
      FF000063CE000063CE000063CE000063CE000063CE000063CE000063CE000063
      CE000063CE00F7DEB500B5848400000000000000000000000000D6BDB500FFFF
      FF0031C6520029BD4A0010A5310010A5310010A53100008C0800008C0800008C
      0800006B0800F7DEB500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00F1D7AD00EFC99900FEFEDB00FEFEDA00FEFE
      F100FEFEFA00FEFEF500FEFEE900AE837E00008C080000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00EFC79600F0C08D00FEFDD700FEFEDC00FEFE
      EE00FEFEF700FEFEEE00FEFEE000AE837E000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00EFC99900F0BE8D00F6DFB400FDF7CE00FEFE
      DD00FEFEDD00FEFEDB00FEFEDA00AE837E000000000000000000E7C6B500FFFF
      FF008484840084848400848484008484840084848400FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000E7C6B500FFFF
      FF0073A5FF00004AF700004AF700004AF700004AF700FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000000000004242420000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AE837E00F8E4B900EFC79600EFC99900F7E2B600FEFE
      DA00FEFEDC00FEFEDB00FEFEDC00AE837E000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      0000000000000000000042424200000000004242420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E1CAB200FEFEFA00F8E4B900F0C08D00EFC3
      9200EFC99900FDF3C700DEC6AE00000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AE837E00DFC7B100FEFDD700F3DAAF00EFC9
      9900F2D8AD00DEC5AB00AE837E00000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AE837E00AE837E00AE83
      7E00AE837E000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500DEBDB500BD84
      7B00000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5787300A5787300A5787300A5787300A5787300A5787300A578
      7300A5787300A57873008C5D5C0000000000000000000000000000669A000066
      9A0000669A00A37F7700A37F7700A37F7700A37F7700A37F7700A37F7700A37F
      7700A37F7700A37F77008F6261000000000000000000C6847300B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      000000000000AD3900008C290000000000000000000000000000A54200008C29
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A97B7500FCE3CA00FDE1C500FDDFC100FCDAB900FCDAB900F9D4
      B000F9D4B000F5CCA6008C5D5C00000000000000000000669A004BC3E4004BC3
      E4004BC3E400B3847600F7ECDE00FAF1E700F8EEE100F7ECDE00F7ECDE00F7EC
      DE00F7ECDE00F7ECDE008F62610000000000C6A59C00FFFFFF00FFFFFF00FFFF
      FF00FFFFEF00FFFFEF00FFFFEF00FFF7DE00FFF7DE00FFEFD600FFEFD600FFEF
      D600FFDEB50000189C00FFD6A500B58484000000000000000000000000000000
      0000C65A0000A5420000A54200008C29000000000000A5420000AD390000AD39
      00008C2900000000000000000000000000000000000000000000000000000000
      000000000000AD7E7500FAE6D400E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600F9D4B0008C5D5C00000000000000000000669A004AC5E6004BC3
      E4004BC3E400B3847600F5E7D800E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F5E3D1008F62610000000000C6A59C00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFEF00FFFFEF00FFFFEF00FFF7DE00FFF7DE00FFEF
      D600FFEFD60000189C00FFDEBD00B58484000000000000000000000000000000
      0000AD39000000000000000000008C29000000000000AD390000000000000000
      00008C2900000000000000000000000000000000000000000000000000000000
      000000000000B5867A00FBEEE200F9E9D900FBE4CF00FCE3CA00FDE1C500FCDD
      BD00FCDAB900F9D4B0008C5D5C00000000000000000000669A004CC7E9004AC5
      E6004BC3E400B9877500F7ECDE00F5E4D200EFDCC900EFDCC900EFDCC900EBD8
      C600EFDCC900F5E4D2008F62610000000000C6ADA500FFFFFF00FFFFFF00FFFF
      FF00DEA57B00C6847300FFFFFF00DEA57B00C6847300FFFFEF00DEA57B002118
      CE002118CE002118CE0000189C0000189C000000000000000000000000000000
      0000AD3900008C290000000000008C29000000000000AD39000000000000C65A
      00008C290000000000000000000000000000A5787300A5787300A5787300A578
      7300A5787300BA8C7D00FBF1E700E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600FCDAB9008C5D5C00000000000000000000669A0057CDED0050C9
      EA004AC5E600B9877500F9EFE400E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F5E5D5008F62610000000000C6ADA500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFFF
      EF00FFF7DE002118CE00FFEFD600B58484000000000000000000000000000000
      0000C65A0000AD390000AD390000AD390000A5948400AD390000AD390000AD39
      00008C290000000000000000000000000000A97B7500FCE3CA00FDE1C500FDDF
      C100FCDAB900BD918400FDF5ED00FBF1E700FBEEE200F9E9D900FAE6D400FCE3
      CA00FDE1C500FDDFC1008C5D5C00000000000000000000669A005DD1EF0057CD
      ED0053CBEB00C18C7200FAF2E900F7ECDE00F5E4D200F5E4D200F5E3D100F3E1
      CD00F5E4D200F5E9DB008F62610000000000D6B5AD00FFFFFF00C6847300FFFF
      FF00FFFFFF00EFE7E700943931008C2121008C212100A5635A008C212100E7CE
      C600FFFFEF002118CE00FFEFD600B58484000000000000000000000000000000
      000000000000C65A0000A54200009C4A18008C634A00AD390000A54200008C29
      000000000000000000000000000000000000AD7E7500FAE6D400E5A55600E5A5
      5600E5A55600BD918400FEF9F300E5A55600E5A55600E5A55600E5A55600E5A5
      5600E5A55600FCE3CA008C5D5C00000000000000000000669A0065D6F20061D4
      F1005BD0EE00C6906F00FAF4EE00E3A55B00E4A55900E4A55900E4A55900E4A5
      5900E4A55900F9EEE2008F62610000000000D6B5AD00FFFFFF00DEA57B00FFFF
      FF00FFFFFF00BD7B7B009C424200FFFFFF00EFE7E7009C5252008C212100FFFF
      FF00FFFFEF00DEA57B00FFEFD600B58484000000000000000000000000000000
      00000000000000000000000000008C736B00E7DED6008C736B00000000000000
      000000000000000000000000000000000000B5867A00FBEEE200F9E9D900FBE4
      CF00FCE3CA00DEAB8400FEFBF900FEF9F500FDF7F000FCF4EA00FBF1E700FBEE
      E200F9E9D900FAE5D1008C5D5C00000000000000000000669A006DDBF50069D9
      F40061D4F100C6906F00FBF8F400FBF6F200F9F0E600F9EFE400F8EEE100F9EE
      E200F9F0E600F5E9DB008F62610000000000D6BDB500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C68C8C008C212100CE949400FFFFFF00FFFFFF008C212100FFFF
      FF00FFFFFF00FFFFEF00FFF7DE00B58484000000000000000000000000000000
      000000000000000000008C736B00E7DED6009C847B00D6CEBD008C736B000000
      000000000000000000000000000000000000BA8C7D00FBF1E700E5A55600E5A5
      5600E5A55600DEAB8400FEFBF900FEFBF900FEF9F500FEF9F300FDF5ED00F9E9
      D900ECC5A200BD9184008C5D5C00000000000000000000669A0075DFF80071DD
      F60069D9F400D5A58900FBF8F400FCFAF900FCFAF900FCFAF900FCFAF900B384
      7600B3847600B3847600B384760000000000D6BDB500FFFFFF00C6847300FFFF
      FF00FFFFFF00FFFFFF00B56B6B00943931008C2121008C2121008C212100FFFF
      FF00FFFFFF00C6847300FFFFEF00B58484000000000000000000000000000000
      000000000000000000009C847B00E7DED6009C847B00D6CEBD009C847B000000
      000000000000000000000000000000000000BD918400FDF5ED00FBF1E700FBEE
      E200F9E9D900E2B18A00FEFBF900FEFBF900FEFBF900FEFBF800FEF9F300B281
      7600B2817600B2817600B07F7500000000000000000000669A007CE2F90078E1
      F90071DDF600D5A58900FCFAF800FCFAF900FCFAF900FCFAF900FCFAF900B384
      7600DDA57200E2A45B000000000000000000D6BDB500FFFFFF00DEA57B00FFFF
      FF00FFFFFF00EFE7E700CE949400FFFFFF00FFFFFF00E7D6D6008C212100FFFF
      FF00FFFFFF00DEA57B00FFFFEF00B58484000000000000000000000000000000
      0000000000008C736B00E7DED6008C736B00000000007B7B7300D6CEBD008C73
      6B0000000000000000000000000000000000BD918400FEF9F300E5A55600E5A5
      5600E5A55600E5B68E00FEFBF900FEFBF900FEFBF900FEFBF900FEFBF800B281
      7600E5AE7000E4A3530000000000000000000000000000669A007EE3F9007CE2
      F90078E1F900D5A58900D9A68200D9A68200D9A68200D9A68200D9A68200B384
      7600C6AE9A0000669A000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00EFE7E7008C212100BD7B7B00CE949400B56B6B008C212100FFFF
      FF00FFFFFF00FFFFFF00FFFFEF00B58484000000000000000000000000000000
      000000000000D6CEBD008C736B000000000000000000000000008C736B00D6CE
      BD0000000000000000000000000000000000DEAB8400FEFBF900FEF9F500FDF7
      F000FCF4EA00E5B68E00DEAB8400DEAB8400DEAB8400DEAB8400DEAB8400B281
      7600E8AB5E000000000000000000000000000000000000669A0088E5F9007EE3
      F9007EE3F9007EE3F90078E1F90075DFF8006DDBF50065D6F2005DD1EF0057CD
      ED0053CBEB0000669A000000000000000000E7C6B500FFFFFF00C6847300FFFF
      FF00FFFFFF00FFFFFF00DEBDB5009C525200943931009C424200DEBDB500FFFF
      FF00FFFFFF00C6847300FFFFEF00B58484000000000000000000000000000000
      00008C736B00E7DED6009C847B000000000000000000000000009C847B00D6CE
      BD008C736B00000000000000000000000000DEAB8400FEFBF900FEFBF900FEF9
      F500FEF9F300FDF5ED00F9E9D900ECC5A200BD9184008C5D5C00000000000000
      0000000000000000000000000000000000000000000000669A0088E5F90088E5
      F9007372720073727200737272007372720073727200737272007372720061D4
      F1005BD0EE0000669A000000000000000000E7C6B500FFFFFF00DEA57B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00DEA57B00FFFFFF00B58484000000000000000000000000000000
      0000D6CEBD008C736B0000000000000000000000000000000000000000007B7B
      7300D6CEBD00000000000000000000000000E2B18A00FEFBF900FEFBF900FEFB
      F900FEFBF800FEF9F300B2817600B2817600B2817600B07F7500000000000000
      0000000000000000000000000000000000000000000000669A0088E5F90088E5
      F90073727200D1C5BA00D1C5BA00D1C5BA00D1C5BA00C9BFB6007372720069D9
      F40061D4F10000669A000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00DEA57B00C6847300FFFFFF00DEA57B00C6847300FFFFFF00DEA57B00C684
      7300FFFFFF00FFFFFF00FFFFFF00B58484000000000000000000000000000000
      00008C736B000000000000000000000000000000000000000000000000000000
      00007B7B7300000000000000000000000000E5B68E00FEFBF900FEFBF900FEFB
      F900FEFBF900FEFBF800B2817600E5AE7000E4A3530000000000000000000000
      000000000000000000000000000000000000000000000000000000669A000066
      9A0073727200EBD8C600FAFAF900FCF9F700FCF9F700D1C5BA00737272000066
      9A0000669A00000000000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00C68473000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E5B68E00DEAB8400DEAB8400DEAB
      8400DEAB8400DEAB8400B2817600E8AB5E000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007372720073727200737272007372720073727200000000000000
      00000000000000000000000000000000000000000000E7C6B500E7C6B500E7C6
      B500E7C6B500E7C6B500E7C6B500D6BDB500D6BDB500D6B5AD00D6B5AD00C6AD
      A500C6ADA500C6A59C00C6A59C00000000000000000000000000000000000000
      0000868483008684830000000000000000008684830086848300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000632DE000632DE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008684
      8300C0BAB800B8B3B300868483008684830086848300EDEBE900868483008684
      830086848300000000000000000000000000000000000632DE000632DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000632DE000632DE00000000000000000000000000854100008541
      0000763A00006B350000673301006B3500006B3500006B3500006B3500006B35
      00006B3500005B2E030000000000000000000000000000000000854302008340
      0000733900006B35000068340000683400006834000068340000683400006834
      0000683400005D2F03000000000000000000000000000000000086848300E3E2
      E100B8B3B300B8B3B300B8B3B300505050004F4F4F0086848300CECCCC00E9E7
      E600C8C7C700868483008684830086848300000000000632DE000632DE000632
      DE00000000000000000000000000000000000000000000000000000000000000
      00000632DE000632DE00000000000000000000000000AA540000CF660000CF66
      0000B6590000B5590000AF560000AD550000AF560000AF560000AF560000AF56
      0000B4590000964A00005B2E03000000000000000000AB540000D0670000BC5D
      0000BC5D0000B5590000B0570000AE550000AE550000AE550000AE550000AE55
      0000B5590000964A00005D2F0300000000000000000086848300E0DEDE00D8D5
      D500A9A4A300A9A4A300A9A4A300595858001616160012121200181818001212
      120093888500CBCAC9008684830000000000000000000632DE000632DD000632
      DE000632DE000000000000000000000000000000000000000000000000000632
      DE000632DE0000000000000000000000000000000000CF660000E7740300DC6D
      0100D66A0000D0670000CF660000B6590000B6590000B6590000B6590000B659
      0000CF660000B45900006B3500000000000000000000D0670000E7730100DA6E
      0300D46A0100D0670000D0670000BC5D0000BC5D0000BC5D0000BC5D0000BC5D
      0000BC5D0000B5590000683400000000000086848300D8D5D500D8D5D500A9A4
      A300E0DEDE00DAD8D800CDCBCB00C5BDB900B8B3B300A9A3A100777676001212
      12001414140013131300868483009388850000000000000000000433ED000632
      DE000632DE000632DE00000000000000000000000000000000000632DE000632
      DE000000000000000000000000000000000000000000D1670100EA760500E271
      0200DC6D0100D4690000E6934100E69A4F00E69A4F00E6974900D7611900B659
      0000B6590000AF5600006B3500000000000000000000D1680100EB760400E372
      0300DA6E0300EC862100E9974600F0A35800E9974600E8934000BA5C0000BA5C
      0000BC5D0000AE550000683400000000000086848300D8D5D500A9A4A300F4F1
      ED00F6F3EF00F6F3EF00E9E7E600E9E7E600E5E4E300DAD8D800CCCBCB00C0BA
      B800A9A3A10072717100938885008E8683000000000000000000000000000000
      00000632DE000632DE000632DD00000000000632DD000632DE000632DE000000
      00000000000000000000000000000000000000000000D66A0000EA7E1400ED7A
      0800E7740300DF6F0100F8C99C00FCFAF800FCFAF800FCFAF800FCFAF800E69A
      4F00B6590000AF5600006B3500000000000000000000D46A0100EE7F1300ED7A
      0A00F0A35800FCF8F500FCF8F500FCF8F500FCF8F500F8CA9E00BC5D0000BA5C
      0000BC5D0000AE550000683400000000000086848300A9A4A300F6F3EF00F6F3
      EF00F6F3EF00E0DEDE00ADA9A900B1ACAC00C5BDB900D1CFCE00E0DEDE00E3E2
      E100D4D2D100C8C7C700AAA5A500858383000000000000000000000000000000
      0000000000000632DD000533E7000533E7000533E9000632DD00000000000000
      00000000000000000000000000000000000000000000D66A0000E98C3100EB81
      1900ED7A0800E7740300E5730300EA7E1400D7611900D7611900F4BF8C00FCFA
      F800B6590000AF5600006B3500000000000000000000D46A0100EC8D3100EE82
      1800FCF8F500F7C08B00ED841D00EE821800DA641600D5690400BC5D0000BC5D
      0000BC5D0000AE5500006834000000000000000000008684830086848300F2EF
      EC00C0BAB80093888500A9A3A100ADA9A900ACA7A700B1ACAC00ADA9A900B1AC
      AC00CAC9C900DDDBDB00D1CFCE00868483000000000000000000000000000000
      000000000000000000000632E4000632E4000433EF0000000000000000000000
      00000000000000000000000000000000000000000000D66A0000EDA45C00EA8A
      2C00EA7E1400ED7A0800F8C99C00E2710200DF6F0100D66A0000D4670A00FCFA
      F800CF660000B15700006B3500000000000000000000D46A0100F2A55B00ED8A
      2900FCF8F500ED841D00E9750300E3720300DF700300F8CA9E00D0670000D067
      0000BC5D0000B057000068340000000000000000000000000000000000008684
      8300C99B9700BF9B8B009F8D8500908684008280800093888500A9A4A300BBB6
      B500D8D5D500D1CFCE0086848300000000000000000000000000000000000000
      0000000000000632DD000433ED000533E9000433EF000434F400000000000000
      00000000000000000000000000000000000000000000D66A0000F4B27200E890
      3900EB811900F8C89900FCFAF800EA760500E5730300DF6F0100E89E5500FCFA
      F800CF660000B6590000723800000000000000000000D46A0100F6B37200EB90
      3700FCF8F500F5AA6200EC780600EB760400E7730100FCF8F500F8C59400D168
      0100D0670000B85B000073390000000000000000000000000000000000000000
      0000C99B9800FBE4C900F9DAB700F0D4B600C69C9100C69C9100C99B98009A8B
      8500868483008684830000000000000000000000000000000000000000000000
      00000434F4000433EF000533EB0000000000000000000434F4000335F8000000
      00000000000000000000000000000000000000000000D66A0000F5B97D00E693
      4100FADCBE00FCFAF800FCFAF800FBE8D500FAE1C700FAE1C700FCFAF800F4BF
      8C00D96C0000CF660000854100000000000000000000D46A0100F6B97E00E893
      4000F8CA9E00FCF8F500FBE3CB00FBE3CB00FBEBDA00FCF8F500FCF8F500F9D5
      B100D66B0200BC5D000083400000000000000000000000000000000000000000
      0000C99B9800FBE4C900F9DBB900F9DAB700F9DAB700F6D8B600C99B98000000
      0000000000000000000000000000000000000000000000000000000000000335
      F8000433EF000334F800000000000000000000000000000000000335F8000335
      F8000000000000000000000000000000000000000000D66A0000F4BC8500E89E
      5500F9D8B700FCFAF800FCFAF800FAE1C700F8C99C00F8C99C00EDA45C00E271
      0200E2710200CF660000904700000000000000000000D46A0100F7BE8700ED9C
      4C00EB903700F6B37200F8CA9E00F8CA9E00FADEC300FCF8F500FCF8F500F9D0
      A800E3720300D06700009148000000000000000000000000000000000000C99B
      9800FCECD900FCE7CF00F9E0C300F9DBB900F9DAB700C99B9800000000000000
      00000000000000000000000000000000000000000000000000000335F8000335
      F8000335F8000000000000000000000000000000000000000000000000000335
      F8000335F80000000000000000000000000000000000D66A0000F8C79600EEA7
      6100E8903900F8CCA100FCFAF800EA831D00EB811900ED7A0800EC780600EA76
      0500EA760500D66A00009E4E00000000000000000000D46A0100F8C59400F5AA
      6200E9923C00EB903700ED882500ED841D00EE821800FCF8F500F7C08B00EB76
      0400E9750300D46A01009D4D000000000000000000000000000000000000C99B
      9800FAEFE200FCECD900FBE5CC00F9E0C300F9DAB700C99B9800000000000000
      000000000000000000000000000000000000000000000335F8000335F8000335
      F800000000000000000000000000000000000000000000000000000000000000
      0000000000000335F800000000000000000000000000D66A0000F8C89900F5C2
      9000F5B97D00F1AC6800F9D3AE00E6974900E98C3100EB811900ED7A0800EC78
      0600EC780600E2710200AA5400000000000000000000D46A0100F8C79800F8C3
      8E00F7B67800F6B06C00F2A55B00ED9C4C00EB903700F7C08B00ED7A0A00EC78
      0600EC780600DF700300AB540000000000000000000000000000C99B9800F6F3
      EF00F6F3EF00FAEFE200FCEAD600FCE7CF00F3D6B700C99B9800000000000000
      0000000000000000000000000000000000000335F8000335F8000335F8000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D66A0000F5B97D00F8C9
      9C00F8CCA100F8C79600F4BC8500F4B27200E6974900EA882800ED7A0800ED7A
      0800ED7A0800E7740300B55900000000000000000000D46A0100F7B67800F8CA
      9E00F8CA9E00F8C59400F7BC8300F6B06C00ED9C4C00ED882500EE7D0E00EC78
      0600EC780600E9750300B5590000000000000000000000000000C99B9800C99B
      9800C99B9800F8F2EA00FCECD900FCE7CF00C99B980000000000000000000000
      0000000000000000000000000000000000000335F8000335F800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EA7E1400E98C
      3100E98C3100E98C3100EA8A2C00EA852200EA7E1400D4670A00D96C0000D66A
      0000D66A0000CF66000000000000000000000000000000000000DA641600EC8D
      3100EC8D3100EC8D3100ED8A2900EC862100DA641600D8670C00D66B0200D66B
      0200D66B0200D067000000000000000000000000000000000000000000000000
      000000000000C99B9800C99B9800C99B98000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B7818200B781
      8200B7818200B7818200B7818200B7818200B7818200B7818200B7818200B781
      8200B7818200B7818200B7818200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000C8A79D00F8EA
      D900F4E0C800F2DABD00F2D8B300F2D4AA00F1D3A600EFCFA200EFCFA200EFCF
      A200EFCFA200F0D1A100B781820000000000000000000000000097433F009743
      3F00C2999900C2999900C2999900C2999900C2999900C2999900C29999009230
      2F0097433F0000000000000000000000000000000000AD5A5A00AD5A5A00E7C6
      C600E7C6C600C6CEC600C6CEC600C6CEC600C6CEC600AD5A5A00943131000000
      000000000000000000000000000000000000078DBE001A9DAA005EC7EB0084E1
      FA0066CDF20066CDF20066CDF20066CDF20066CDF20066CDF20066CDF20066CD
      F20046B8D400078DBE0000000000000000000000000000000000C8A79D00F8EA
      D900F5E4CF00F3DEC500F2D9B900F2D8B300F2D4AA00F0D1A100EECEA200EECE
      A200EECEA200F0D1A100B7818200000000000000000097433F00CD666600C663
      6300E4E0E400922B2B00922B2B00E6E5E700E5E3E500E4E0E400CECACC00922B
      2B009E43410097433F000000000000000000BD7B7300C65A5A00C65A5A00E7C6
      C6009C393900B5737300C6CEC600F7F7F700F7F7F700C65A5A00943131000000
      000000000000000000000000000000000000078DBE004BBBDD0046B8D4009BF1
      FC0072D6F80072D6F8006DD2F60072D6F80072D6F80072D6F80072D6F8006DD2
      F60048B9D90080DEF900078DBE00000000000000000000000000C9A99E00FAF1
      E800F7E7D500F5E4CF00F3DEC500F2DABD00F2D8B300F2D4AA00F0D1A100EECE
      A200EECEA200F0D1A100B7818200000000000000000097433F00CD656600C162
      6200E5E3E500922B2B00922B2B00E4E0E400E6E5E700E4E0E400CECBCC00922B
      2B009E43410097433F000000000000000000BD7B7300C65A5A00C65A5A00C694
      8C009C3939009C4A4A00E7C6C600C6CEC600F7F7F700C65A5A00943131000000
      000094313100000000000000000000000000078DBE0072D6F800078DBE00ACF7
      FC007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDCFA007BDC
      FA0048B9D900ACF7FC00078DBE00000000000000000000000000CAAEA300FBF5
      EF00F8EAD900F7E7D500F5E4CF00F3DEC500F2D9B900F2D8B300F2D4AA00F0D1
      A100EECEA200F0D1A100B7818200000000000000000097433F00CD656600C162
      6200E5E3E500922B2B00922B2B00E4E0E400E6E5E700E6E5E700CECBCC00922B
      2B009E43410097433F000000000000000000BD7B7300C65A5A00C65A5A00C694
      8C00C6948C00BDA5A500BDA5A500E7C6C600C6CEC600C65A5A0094313100C65A
      5A0094313100000000000000000000000000078DBE007BDCFA001396B60099F0
      FC0092EBFB0086E3FB0086E3FB0086E3FB0086E3FB0086E3FB0086E3FB0086E3
      FB0048B9D900B1F7FC00078DBE00000000000000000000000000CEACA800FDFA
      F600FAF1E800F8EAD900F7E7D500F3E1CC00F3DEC500F2DABD00F2D8B300F2D4
      AA00F0D1A100F0D1A100B7818200000000000000000097433F00CD656600C162
      6200E6E5E700E6E5E700E4E0E400E4E0E400E4E0E400E4E0E400CECBCC00922B
      2B009E43410097433F000000000000000000BD7B7300AD524A00B55A5A00C65A
      5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A5A0094313100C65A
      5A0094313100000000009431310000000000078DBE0086E3FB0048B9D90058C3
      E700ACF7FC008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB000C84
      18004BBBDD00B6F7FD0066CDF200078DBE000000000000000000D0A9AB00FEFC
      FB00FBF5EF00FAF1E800F8EAD900F7E7D500F4E0C800F3DEC500F2D9B900F2D8
      B300F2D4AA00F1D3A600B7818200000000000000000097433F00CD656600C663
      6300C8676700C6717000C6717000C86A6A00C4636300C86C6C00CA666600C463
      6300CD65660097433F000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000078DBE008CE7FB0077DAF9001A9D
      AA00D8F7FB00CAF6FD00CAF6FD00CAF6FD00CAF6FD00CAF6FD000C84180035BC
      73000C841800D8F7FB00D6F6FB00078DBE000000000000000000E6BDAF00FEFC
      FB00FDFAF600FBF5EF00F9EDDE00F8EAD900F7E7D500F5E4CF00F3DEC500F2D9
      B900F2D8B300F2D6AF00B7818200000000000000000097433F00B8646400C47C
      7B00C89E9E00CAA8A800CAA8A800CAA8A800C9A0A000C9A0A000CAA8A800CAA8
      A800CC66660097433F000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000078DBE0095EEFC0095EEFC001396
      B600078DBE00078DBE00078DBE00078DBE00078DBE000C84180046CC80004BCC
      98003DC374000C841800078DBE00078DBE000000000000000000E6BDAF00FEFC
      FB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F8EAD900F7E7D500F3E1CC00F2DC
      C100F2D9B900F2D8B300B7818200000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000078DBE009EF4FD009FF6FD009FF6
      FD009EF4FD009FF6FD009FF6FD009EF4FD000C84180046CC800049CD890049CD
      89004BCC98003DC374000C841800000000000000000000000000E6BDAF00FEFC
      FB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F8EAD900F7E7D500F4E0
      C800F4E0C800F2D9B900B7818200000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000078DBE00D8F7FB00A2F7FD00A2F7
      FD00A2F7FD00A2F7FD00A2F7FD000C8418000C8418000C8418000C84180049CD
      890046CC80000C8418000C8418000C8418000000000000000000E7C4AD00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F9EDDE00F9EDDE00F8EA
      D900F2DCC100CCAFA600B7818200000000000000000097433F00CC666600F8F8
      F800CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00F8F8
      F800CC66660097433F000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A00943131000000000000000000078DBE00D8F7FB00A5F7
      FC00A5F7FC00A5F7FC00078DBB0048B9D90048B9D90048B9D9000C84180046CC
      800035BC73000C84180000000000000000000000000000000000E7C4AD00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FCF8F400FBF5EF00F3E1CC00C6A1
      9600C0958800BA8F8600B7818200000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F000000000000000000BD7B7300AD524A00D6D6D600CEB5
      B500CEB5B500CEB5B500CEB5B500CEB5B500D6D6D600AD524A0094313100C65A
      5A0094313100C65A5A0094313100000000000000000000000000078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000000000000C8418003FC6
      79000C8418000000000000000000000000000000000000000000EACAAC00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFBF900F3DEC500C188
      7500DF9D5600DF9D5600C58B7200000000000000000097433F00CC666600F8F8
      F800CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00CDCBCC00F8F8
      F800CC66660097433F0000000000000000000000000000000000BD7B7300AD52
      4A00FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A
      5A0094313100C65A5A0094313100000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C84180030B8720030B8
      72000C8418000000000000000000000000000000000000000000EACAAC00FEFC
      FB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00FEFCFB00F3E1CC00C39A
      8D00EECEA200CD906A0000000000000000000000000097433F00CC666600F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F800CC66660097433F0000000000000000000000000000000000BD7B7300AD52
      4A00D6D6D600CEB5B500CEB5B500CEB5B500CEB5B500CEB5B500D6D6D600AD52
      4A0094313100C65A5A0094313100000000000000000000000000000000000000
      000000000000000000000000000000000000000000000C84180030B872000C84
      1800000000000000000000000000000000000000000000000000EACAAC00FCF8
      F400FCF8F400FCF8F400FCF8F400FBF5EF00FBF5EF00FBF5EF00F3E1CC00C197
      8900C39A8D00000000000000000000000000000000000000000097433F00F8F8
      F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8F800F8F8
      F80097433F000000000000000000000000000000000000000000000000000000
      0000BD7B7300AD524A00FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7
      F700FFF7F700C65A5A0094313100000000000000000000000000000000000000
      00000000000000000000000000000C8418000C8418000C8418000C8418000000
      0000000000000000000000000000000000000000000000000000E9C8AB00EACA
      AC00EACAAC00EACAAC00EACAAC00EACAAC00EACAAC00EACAAC00E6BDAF00BD85
      7900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BD7B7300AD524A00D6D6D600CEB5B500CEB5B500CEB5B500CEB5B500CEB5
      B500D6D6D600AD524A0094313100000000000000000000000000000000000000
      0000000000000C8418000C8418000C8418000C84180000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
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
    DrawNameEndEllipsis = False
    Left = 662
    Top = 36
  end
end
