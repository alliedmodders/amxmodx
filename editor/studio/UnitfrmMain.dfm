object frmMain: TfrmMain
  Left = 184
  Top = 77
  Width = 893
  Height = 648
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
  Position = poScreenCenter
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object splRight: TSplitter
    Left = 674
    Top = 95
    Height = 501
    Align = alRight
  end
  object spcRight1: TImage
    Left = 882
    Top = 95
    Width = 3
    Height = 501
    Align = alRight
  end
  object spcLeft1: TImage
    Left = 0
    Top = 95
    Width = 3
    Height = 501
    Align = alLeft
  end
  object tbxTopDock: TSpTBXDock
    Left = 0
    Top = 0
    Width = 885
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
      object mnuFile: TSpTBXSubmenuItem
        Caption = 'File'
        object mnuNew: TSpTBXSubmenuItem
          Caption = 'New'
          ImageIndex = 32
          Images = ilImages
          object infoNewAMXX: TSpTBXRightAlignSpacerItem
            Caption = 'AMXX Scripts / Pawn'
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
          end
          object mnuEmptyPlugin: TSpTBXItem
            Caption = 'Empty Plugin'
            ImageIndex = 0
            Images = ilImages
            OnClick = mnuEmptyPluginClick
          end
          object mnuNewPlugin: TSpTBXItem
            Caption = 'Plugin'
            ImageIndex = 18
            Images = ilImages
            OnClick = mnuNewPluginClick
          end
          object mnuHeaderPAWN: TSpTBXItem
            Caption = 'Header'
            ImageIndex = 33
            Images = ilImages
            OnClick = mnuHeaderPAWNClick
          end
          object sepNew1: TSpTBXSeparatorItem
            Blank = True
          end
          object infoNewCPP: TSpTBXRightAlignSpacerItem
            Caption = 'C++'
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
          end
          object mnuNewModule: TSpTBXItem
            Caption = 'Module'
            Enabled = False
            ImageIndex = 35
            Images = ilImages
            OnClick = mnuNewModuleClick
          end
          object mnuNewUnit: TSpTBXItem
            Caption = 'Unit'
            Enabled = False
            ImageIndex = 34
            Images = ilImages
            OnClick = mnuNewUnitClick
          end
          object mnuNewHeaderCPP: TSpTBXItem
            Caption = 'Header'
            Enabled = False
            ImageIndex = 48
            Images = ilImages
            OnClick = mnuNewHeaderCPPClick
          end
          object sepNew3: TSpTBXSeparatorItem
            Blank = True
          end
          object infoNewOther: TSpTBXRightAlignSpacerItem
            Caption = 'Other'
            FontSettings.Bold = tsTrue
            FontSettings.Color = clWindowText
          end
          object mnuNewTextfile: TSpTBXItem
            Caption = 'Textfile'
            ImageIndex = 34
            Images = ilImages
            OnClick = mnuNewTextfileClick
          end
          object mnuNewHTML: TSpTBXItem
            Caption = 'HTML Page'
            ImageIndex = 25
            Images = ilImages
            OnClick = mnuNewHTMLClick
          end
          object mnuNewSQL: TSpTBXItem
            Caption = 'SQL Database'
            ImageIndex = 44
            Images = ilImages
            OnClick = mnuNewSQLClick
          end
          object mnuNewXML: TSpTBXItem
            Caption = 'XML File'
            ImageIndex = 34
            OnClick = mnuNewXMLClick
          end
        end
        object sepFile1: TSpTBXSeparatorItem
        end
        object mnuOpen: TSpTBXItem
          Caption = 'Open...'
          ImageIndex = 3
          Images = ilImages
          ShortCut = 16463
          OnClick = mnuOpenClick
        end
        object sepFile2: TSpTBXSeparatorItem
        end
        object mnuSave: TSpTBXItem
          Caption = 'Save'
          ImageIndex = 1
          Images = ilImages
          ShortCut = 16467
          OnClick = mnuSaveClick
        end
        object mnuSaveAs: TSpTBXItem
          Caption = 'Save as...'
          OnClick = mnuSaveAsClick
        end
        object mnuSaveAllFiles: TSpTBXItem
          Caption = 'Save all files'
          ImageIndex = 2
          Images = ilImages
          ShortCut = 24659
          OnClick = mnuSaveAllFilesClick
        end
        object sepFile3: TSpTBXSeparatorItem
        end
        object mnuClose: TSpTBXItem
          Caption = 'Close'
          ImageIndex = 46
          ShortCut = 16430
          OnClick = mnuCloseClick
        end
        object mnuCloseAllFiles: TSpTBXItem
          Caption = 'Close all files'
          OnClick = mnuCloseAllFilesClick
        end
        object sepFile4: TSpTBXSeparatorItem
        end
        object mnuPrint: TSpTBXItem
          Caption = 'Print'
          ImageIndex = 4
          Images = ilImages
          ShortCut = 16464
          OnClick = mnuPrintClick
        end
        object sepFile5: TSpTBXSeparatorItem
        end
        object mnuExit: TSpTBXItem
          Caption = 'Exit'
          ImageIndex = 45
          Images = ilImages
          ShortCut = 32883
          OnClick = mnuExitClick
        end
      end
      object mnuEdit: TSpTBXSubmenuItem
        Caption = 'Edit'
        object mnuUndo: TSpTBXItem
          Caption = 'Undo'
          ImageIndex = 6
          Images = ilImages
          ShortCut = 16474
          OnClick = mnuUndoClick
        end
        object mnuRedo: TSpTBXItem
          Caption = 'Redo'
          ImageIndex = 7
          Images = ilImages
          ShortCut = 24666
          OnClick = mnuRedoClick
        end
        object sepEdit1: TSpTBXSeparatorItem
        end
        object mnuCut: TSpTBXItem
          Caption = 'Cut'
          ImageIndex = 8
          Images = ilImages
          ShortCut = 16472
          OnClick = mnuCutClick
        end
        object mnuCopy: TSpTBXItem
          Caption = 'Copy'
          ImageIndex = 9
          Images = ilImages
          ShortCut = 16451
          OnClick = mnuCopyClick
        end
        object mnuPaste: TSpTBXItem
          Caption = 'Paste'
          ImageIndex = 10
          Images = ilImages
          ShortCut = 16470
          OnClick = mnuPasteClick
        end
        object sepEdit2: TSpTBXSeparatorItem
        end
        object mnuSelectAll: TSpTBXItem
          Caption = 'Select all'
          ImageIndex = 11
          Images = ilImages
          ShortCut = 16449
          OnClick = mnuSelectAllClick
        end
      end
      object mnuSearch: TSpTBXSubmenuItem
        Caption = 'Search'
        object mnuSearchDialog: TSpTBXItem
          Caption = 'Search'
          ImageIndex = 13
          Images = ilImages
          ShortCut = 16454
          OnClick = mnuSearchDialogClick
        end
        object mnuSearchAgain: TSpTBXItem
          Caption = 'Search again'
          ShortCut = 114
          OnClick = mnuSearchAgainClick
        end
        object sepSearch1: TSpTBXSeparatorItem
        end
        object mnuReplace: TSpTBXItem
          Caption = 'Replace'
          ImageIndex = 39
          Images = ilImages
          ShortCut = 16466
          OnClick = mnuReplaceClick
        end
        object sepSearch2: TSpTBXSeparatorItem
        end
        object mnuGoToLine: TSpTBXItem
          Caption = 'Go to line...'
          ImageIndex = 14
          Images = ilImages
          ShortCut = 16455
          OnClick = mnuGoToLineClick
        end
      end
      object mnuView: TSpTBXSubmenuItem
        Caption = 'View'
        object mnuChangeTheme: TSpTBXSubmenuItem
          Caption = 'Change Theme'
          object mnuThemes: TSpTBXThemeGroupItem
            OnClick = mnuThemesClick
          end
        end
        object mnuSelectHighlighter: TSpTBXSubmenuItem
          Caption = 'Set Highlighter'
          ImageIndex = 15
          Images = ilImages
          object mnuHPAWN: TSpTBXItem
            Caption = 'Pawn'
            AutoCheck = True
            Checked = True
            OnClick = mnuHXMLClick
          end
          object mnuHCPP: TSpTBXItem
            Caption = 'C++'
            AutoCheck = True
            OnClick = mnuHXMLClick
          end
          object mnuHHTML: TSpTBXItem
            Caption = 'HTML'
            AutoCheck = True
            OnClick = mnuHXMLClick
          end
          object mnuHSQL: TSpTBXItem
            Caption = 'SQL'
            AutoCheck = True
            OnClick = mnuHXMLClick
          end
          object mnuHXML: TSpTBXItem
            Caption = 'XML'
            AutoCheck = True
            OnClick = mnuHXMLClick
          end
          object sepHighlighter: TSpTBXSeparatorItem
          end
          object mnuHNone: TSpTBXItem
            Caption = 'None'
            AutoCheck = True
            OnClick = mnuHNoneClick
          end
        end
        object sepView1: TSpTBXSeparatorItem
        end
        object mnuFoldAll: TSpTBXItem
          Caption = 'Fold all'
          OnClick = mnuFoldAllClick
        end
        object sepView2: TSpTBXSeparatorItem
        end
        object mnuShowFileTB: TSpTBXItem
          Caption = 'Show File-Toolbar'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowFileTBClick
        end
        object mnuShowEditTB: TSpTBXItem
          Caption = 'Show Edit-Toolbar'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowEditTBClick
        end
        object mnuShowCodeSnippets: TSpTBXItem
          Caption = 'Show Code-Snippets'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeSnippetsClick
        end
        object mnuShowCodeToolsWindow: TSpTBXItem
          Caption = 'Show Code-Explorer and Notes'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeToolsClick
        end
        object sepView3: TSpTBXSeparatorItem
        end
        object mnuShowCodeExplorer: TSpTBXItem
          Caption = 'Show Code-Explorer'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeExplorerClick
        end
        object mnuShowCodeInspector: TSpTBXItem
          Caption = 'Show Code-Inspector'
          AutoCheck = True
          Checked = True
          OnClick = mnuShowCodeInspectorClick
        end
      end
      object mnuCompile: TSpTBXSubmenuItem
        Caption = 'Compile'
        object mnuDoCompile: TSpTBXItem
          Caption = 'Compile'
          ImageIndex = 22
          Images = ilImages
          ShortCut = 120
          OnClick = mnuDoCompileClick
        end
        object sepCompile1: TSpTBXSeparatorItem
        end
        object mnuCompileAndStartHL: TSpTBXItem
          Caption = 'Compile and start Half-Life'
          ImageIndex = 22
          Images = ilImages
          OnClick = mnuCompileAndStartHLClick
        end
        object mnuCompileAndUpload: TSpTBXItem
          Caption = 'Compile and upload'
          ImageIndex = 37
          Images = ilImages
          OnClick = mnuCompileAndUploadClick
        end
        object sepCompile2: TSpTBXSeparatorItem
        end
        object mnuRegisterPluginsIniLocal: TSpTBXItem
          Caption = 'Register in plugins.ini (local)'
          OnClick = mnuRegisterPluginsIniLocalClick
        end
        object mnuRegisterPluginsIniWeb: TSpTBXItem
          Caption = 'Register in plugins.ini (FTP)'
          OnClick = mnuRegisterPluginsIniWebClick
        end
      end
      object mnuTools: TSpTBXSubmenuItem
        Caption = 'Tools'
        object mnuIndenter: TSpTBXItem
          Caption = 'Indenter'
          ImageIndex = 16
          Images = ilImages
          ShortCut = 16457
          OnClick = mnuIndenterClick
        end
        object mnuUnindenter: TSpTBXItem
          Caption = 'Unindenter'
          ImageIndex = 17
          Images = ilImages
          ShortCut = 24649
          OnClick = mnuUnindenterClick
        end
        object sepTools1: TSpTBXSeparatorItem
        end
        object mnuSocketTerminal: TSpTBXItem
          Caption = 'Socket Terminal'
          ImageIndex = 40
          Images = ilImages
          OnClick = mnuSocketTerminalClick
        end
        object sepTools2: TSpTBXSeparatorItem
        end
        object mnuPluginsIniEditor: TSpTBXItem
          Caption = 'Plugins.ini Editor'
          ImageIndex = 19
          Images = ilImages
          OnClick = mnuPluginsIniEditorClick
        end
        object mnuPaster: TSpTBXItem
          Caption = 'IRC Paster'
          ImageIndex = 10
          Images = ilImages
          OnClick = mnuPasterClick
        end
        object mnuRestoreBackup: TSpTBXItem
          Caption = 'Restore from backup'
          Enabled = False
          ImageIndex = 46
          Images = ilImages
          OnClick = mnuRestoreBackupClick
        end
        object sepTools3: TSpTBXSeparatorItem
        end
        object mnuSettings: TSpTBXItem
          Caption = 'Settings'
          ImageIndex = 41
          Images = ilImages
          ShortCut = 123
          OnClick = mnuSettingsClick
        end
      end
      object mnuGenerators: TSpTBXSubmenuItem
        Caption = 'Generators'
        object mnuMenuGenerator: TSpTBXItem
          Caption = 'Menu Generator'
          ImageIndex = 20
          Images = ilImages
          OnClick = mnuMenuGeneratorClick
        end
        object sepGenerators1: TSpTBXSeparatorItem
        end
        object mnuHudmessage: TSpTBXItem
          Caption = 'Hudmessage Generator'
          OnClick = mnuHudmessageClick
        end
        object mnuMOTDGenerator: TSpTBXItem
          Caption = 'MOTD Generator'
          ImageIndex = 36
          Images = ilImages
          OnClick = mnuMOTDGeneratorClick
        end
        object mnuConnectionGen: TSpTBXItem
          Caption = 'Create connection'
          ImageIndex = 18
          Images = ilImages
          OnClick = mnuConnectionGenClick
        end
      end
      object mnuHelp: TSpTBXSubmenuItem
        Caption = 'Help'
        object mnuOpenHelp: TSpTBXItem
          Caption = 'Open help'
          ImageIndex = 21
          Images = ilImages
          ShortCut = 112
          OnClick = mnuOpenHelpClick
        end
        object sepHelp1: TSpTBXSeparatorItem
        end
        object mnuSearchForums: TSpTBXItem
          Caption = 'Search on AMX Mod X forums'
          OnClick = mnuSearchForumsClick
        end
        object mnuOpenScriptingForum: TSpTBXItem
          Caption = 'Open AMXX Scripting Forum'
          OnClick = mnuOpenScriptingForumClick
        end
        object sepHelp2: TSpTBXSeparatorItem
        end
        object mnuInfo: TSpTBXItem
          Caption = 'About AMXX-Studio...'
          OnClick = mnuInfoClick
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
        Caption = 'New'
        ImageIndex = 0
        Images = ilImages
        ShortCut = 16462
        OnClick = mnuTNewClick
      end
      object mnuTOpen: TSpTBXItem
        Caption = 'Open'
        ImageIndex = 3
        Images = ilImages
        OnClick = mnuTOpenClick
      end
      object mnuTSave: TSpTBXItem
        Caption = 'Save'
        ImageIndex = 1
        Images = ilImages
        OnClick = mnuTSaveClick
      end
      object sepToolbar1: TSpTBXSeparatorItem
      end
      object mnuTSearch: TSpTBXItem
        Caption = 'Search'
        ImageIndex = 13
        Images = ilImages
        OnClick = mnuTSearchClick
      end
      object sepToolbar2: TSpTBXSeparatorItem
      end
      object mnuTCompile: TSpTBXItem
        Caption = 'Compile'
        ImageIndex = 22
        Images = ilImages
        OnClick = mnuTCompileClick
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
      object mnuCodeSnippets: TSpTBXRightAlignSpacerItem
        Caption = 'Code-Snippets:'
      end
      object mnuPawn: TSpTBXItem
        Caption = 'Pawn'
        AutoCheck = True
        Checked = True
        OnClick = OnCodeSnippetSelect
      end
      object mnuCPP: TSpTBXItem
        Caption = 'C++'
        AutoCheck = True
        OnClick = OnCodeSnippetSelect
      end
      object mnuHTML: TSpTBXItem
        Caption = 'HTML'
        OnClick = OnCodeSnippetSelect
      end
      object mnuOther: TSpTBXItem
        Caption = 'Other'
        AutoCheck = True
        OnClick = OnCodeSnippetSelect
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
      object mnuTUndo: TSpTBXItem
        Caption = 'Undo'
        ImageIndex = 23
        Images = ilImages
        OnClick = mnuTUndoClick
      end
      object mnuTRedo: TSpTBXItem
        Caption = 'Redo'
        ImageIndex = 24
        Images = ilImages
        OnClick = mnuTRedoClick
      end
      object sepTEdit1: TSpTBXSeparatorItem
      end
      object mnuTCopy: TSpTBXItem
        Caption = 'Copy'
        ImageIndex = 9
        Images = ilImages
        OnClick = mnuTCopyClick
      end
      object mnuTCut: TSpTBXItem
        Caption = 'Cut'
        ImageIndex = 8
        Images = ilImages
        OnClick = mnuTCutClick
      end
      object mnuTPaste: TSpTBXItem
        Caption = 'Paste'
        ImageIndex = 10
        Images = ilImages
        OnClick = mnuTPasteClick
      end
      object sepTEdit2: TSpTBXSeparatorItem
      end
      object mnuTSelectAll: TSpTBXItem
        Caption = 'Select all'
        ImageIndex = 11
        Images = ilImages
        OnClick = mnuTSelectAllClick
      end
    end
  end
  object sbStatus: TSpTBXStatusBar
    Left = 0
    Top = 596
    Width = 885
    Height = 25
    object mnuFilename: TSpTBXRightAlignSpacerItem
      Caption = 'Untitled.sma'
      CustomWidth = 547
    end
    object sepStatus0: TSpTBXSeparatorItem
    end
    object cboCurrentIDE: TSpTBXDropDownItem
      Caption = 'Pawn'
      EditWidth = 75
      RadioItem = True
      Text = 'Pawn'
      ReadOnly = True
      DropDownList = True
      object stlIDEs: TSpTBXStringList
        MinWidth = 72
        Strings.Strings = (
          'Pawn'
          'C++'
          'Other')
        OnClick = stlIDEsClick
      end
    end
    object sepStatus1: TSpTBXSeparatorItem
    end
    object mnuShowCodeTools: TSpTBXItem
      Caption = 'Show Code-Tools'
      AutoCheck = True
      Checked = True
      OnClick = mnuShowCodeToolsClick
    end
    object sepStatus2: TSpTBXSeparatorItem
    end
    object mnuModified: TSpTBXRightAlignSpacerItem
      CustomWidth = 50
    end
    object sepStatus3: TSpTBXSeparatorItem
    end
    object mnuCaret: TSpTBXRightAlignSpacerItem
      Caption = 'Ln 0 Ch 0'
      Alignment = taCenter
      CustomWidth = 80
    end
  end
  object tbDocs: TJvTabBar
    Left = 0
    Top = 72
    Width = 885
    RightClickSelect = False
    Painter = mtpDocuments
    Tabs = <
      item
        Caption = '< 1 Untitled.sma >'
        Selected = True
      end>
    OnTabClosing = tbDocsTabClosing
    OnTabSelected = tbDocsTabSelected
    OnMouseDown = tbDocsMouseDown
  end
  object tcTools: TSpTBXTabControl
    Left = 677
    Top = 95
    Width = 205
    Height = 501
    Align = alRight
    Color = clBtnFace
    ActiveTabIndex = 0
    TabPosition = ttpBottom
    ThemeType = tttTBX
    OnActiveTabChange = tcToolsActiveTabChange
    HiddenItems = <>
    object tiTools: TSpTBXTabItem
      Caption = 'Code-Tools'
      Checked = True
      TabPosition = ttpBottom
      ThemeType = tttTBX
    end
    object tiNotes: TSpTBXTabItem
      Caption = 'Notes'
      TabPosition = ttpBottom
      ThemeType = tttTBX
    end
    object tsNotes: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 205
      Height = 478
      Caption = 'Notes'
      ImageIndex = -1
      TabItem = 'tiNotes'
      object imgRight4: TImage
        Left = 202
        Top = 26
        Width = 1
        Height = 450
        Align = alRight
      end
      object imgBottom4: TImage
        Left = 2
        Top = 476
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
        Height = 450
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
        object mnuBold: TSpTBXItem
          Caption = 'Bold'
          AutoCheck = True
          ImageIndex = 29
          OnClick = mnuBoldClick
        end
        object mnuItalic: TSpTBXItem
          Caption = 'Italic'
          AutoCheck = True
          ImageIndex = 27
          OnClick = mnuItalicClick
        end
        object mnuUnderline: TSpTBXItem
          Caption = 'Underline'
          AutoCheck = True
          ImageIndex = 28
          OnClick = mnuUnderlineClick
        end
        object sepNotes: TSpTBXSeparatorItem
        end
        object mnuSelectColor: TSpTBXSubmenuItem
          Caption = 'Select color'
          ImageIndex = 26
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
        Height = 450
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 1
        WantTabs = True
        OnKeyDown = rtfNotesKeyDown
        OnMouseDown = rtfNotesMouseDown
      end
    end
    object tsExplorer: TSpTBXTabSheet
      Left = 0
      Top = 0
      Width = 205
      Height = 478
      Caption = 'Code-Tools'
      ImageIndex = -1
      TabItem = 'tiTools'
      object spcBottom2: TImage
        Left = 2
        Top = 476
        Width = 201
        Height = 2
        Align = alBottom
      end
      object spcLeft2: TImage
        Left = 2
        Top = 2
        Width = 3
        Height = 474
        Align = alLeft
      end
      object spcRight2: TImage
        Left = 200
        Top = 2
        Width = 3
        Height = 474
        Align = alRight
      end
      object pnlDock: TSpTBXMultiDock
        Left = 5
        Top = 2
        Width = 195
        Height = 474
        Position = dpxClient
        object pnlCodeExplorer: TSpTBXDockablePanel
          Left = 0
          Top = 0
          Caption = 'Code-Explorer'
          DockedWidth = 191
          DockPos = 0
          TabOrder = 0
          OnVisibleChanged = pnlCodeExplorerVisibleChanged
          object trvExplorer: TTreeView
            Left = 0
            Top = 26
            Width = 191
            Height = 199
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
          Top = 229
          Caption = 'Code-Inspector'
          DockedWidth = 191
          DockPos = 229
          TabOrder = 1
          OnVisibleChanged = pnlCodeInspectorVisibleChanged
          object jviCode: TJvInspector
            Left = 0
            Top = 26
            Width = 191
            Height = 199
            Style = isItemPainter
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
  object pnlParent: TPanel
    Left = 3
    Top = 95
    Width = 671
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 4
    object splOutput: TSplitter
      Left = 0
      Top = 416
      Width = 671
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      Visible = False
    end
    object sciEditor: TScintilla
      Left = 0
      Top = 0
      Width = 671
      Height = 416
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
        '#define PLUGIN "New Plug-In"'
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
      CodePage = cpAnsi
      Caret.ForeColor = clNone
      Caret.LineBackColor = 16770790
      Caret.LineVisible = True
      Caret.Width = 1
      Caret.Period = 1024
      Caret.LineBackAlpha = 0
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
      Colors.WhiteSpaceFore = clDefault
      Colors.WhiteSpaceBack = clDefault
      Bookmark.BackColor = clGray
      Bookmark.ForeColor = clWhite
      Bookmark.MarkerType = sciMFullRect
      Gutter0.Width = 0
      Gutter0.MarginType = gutLineNumber
      Gutter0.Sensitive = False
      Gutter1.Width = 40
      Gutter1.MarginType = gutLineNumber
      Gutter1.Sensitive = False
      Gutter2.Width = 14
      Gutter2.MarginType = gutSymbol
      Gutter2.Sensitive = True
      Gutter3.Width = 0
      Gutter3.MarginType = gutSymbol
      Gutter3.Sensitive = False
      Gutter4.Width = 0
      Gutter4.MarginType = gutSymbol
      Gutter4.Sensitive = False
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
      Folding = [foldFold, foldCompact, foldComment, foldPreprocessor, foldCommentPython, foldAtElse, foldHTML, foldHTMLPreProcessor]
      FoldMarkers.MarkerType = sciMarkBox
      FoldMarkers.FoldOpen.BackColor = clDefault
      FoldMarkers.FoldOpen.ForeColor = clDefault
      FoldMarkers.FoldOpen.MarkerType = sciMBoxMinus
      FoldMarkers.FoldClosed.BackColor = clDefault
      FoldMarkers.FoldClosed.ForeColor = clDefault
      FoldMarkers.FoldClosed.MarkerType = sciMBoxPlus
      FoldMarkers.FoldSub.BackColor = clDefault
      FoldMarkers.FoldSub.ForeColor = clDefault
      FoldMarkers.FoldSub.MarkerType = sciMVLine
      FoldMarkers.FoldTail.BackColor = clDefault
      FoldMarkers.FoldTail.ForeColor = clDefault
      FoldMarkers.FoldTail.MarkerType = sciMLCorner
      FoldMarkers.FoldEnd.BackColor = clDefault
      FoldMarkers.FoldEnd.ForeColor = clDefault
      FoldMarkers.FoldEnd.MarkerType = sciMBoxPlusConnected
      FoldMarkers.FoldOpenMid.BackColor = clDefault
      FoldMarkers.FoldOpenMid.ForeColor = clDefault
      FoldMarkers.FoldOpenMid.MarkerType = sciMBoxMinusConnected
      FoldMarkers.FoldMidTail.BackColor = clDefault
      FoldMarkers.FoldMidTail.ForeColor = clDefault
      FoldMarkers.FoldMidTail.MarkerType = sciMTCorner
      LanguageManager.LanguageList = <
        item
          Name = 'null'
          Lexer = 'null'
          Styles = <
            item
              FontName = 'Arial'
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clYellow
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clRed
              BackColor = clDefault
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
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clYellow
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clRed
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Indent Guide'
              StyleNumber = 37
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Default'
              StyleNumber = 0
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 13684736
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Tags'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 13684736
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unknown Tags'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12624032
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Attributes'
              StyleNumber = 3
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12624032
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unknown Attributes'
              StyleNumber = 4
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Numbers'
              StyleNumber = 5
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Double quoted strings'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Single quoted strings'
              StyleNumber = 7
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 10485920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Other inside tag'
              StyleNumber = 8
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 9
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Entities'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 10485920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML short tag end'
              StyleNumber = 11
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 10485920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML identifier start'
              StyleNumber = 12
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 10485920
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML Question'
              StyleNumber = 18
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clFuchsia
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unquoted values'
              StyleNumber = 19
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 13684736
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML tags <! ... >'
              StyleNumber = 21
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 10526720
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML command'
              StyleNumber = 22
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 15793935
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML 1st param'
              StyleNumber = 23
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML double string'
              StyleNumber = 24
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML single string'
              StyleNumber = 25
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML error'
              StyleNumber = 26
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 16737843
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML special'
              StyleNumber = 27
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML entity'
              StyleNumber = 28
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clBlue
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clRed
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Indent Guide'
              StyleNumber = 37
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Text'
              StyleNumber = 0
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clBlack
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Tags'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unknown Tags'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12624032
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Attributes'
              StyleNumber = 3
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unknown Attributes'
              StyleNumber = 4
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clBlue
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Numbers'
              StyleNumber = 5
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 39338
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Double quoted strings'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Single quoted strings'
              StyleNumber = 7
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Other inside tag'
              StyleNumber = 8
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 33023
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 9
            end
            item
              FontName = 'Times New Roman'
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 10526880
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Entities'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12632064
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML short tag end'
              StyleNumber = 11
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 10485920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML identifier start'
              StyleNumber = 12
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 10485920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XML identifier end'
              StyleNumber = 13
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 657920
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SCRIPT'
              StyleNumber = 14
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clYellow
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP <% ... %>'
              StyleNumber = 15
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clYellow
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP <% ... %>'
              StyleNumber = 16
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 57343
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'CDATA'
              StyleNumber = 17
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 5343743
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP'
              StyleNumber = 18
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clFuchsia
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Unquoted values'
              StyleNumber = 19
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'XC Comment'
              StyleNumber = 20
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 13684736
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML tags <! ... >'
              StyleNumber = 21
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 10526720
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML command'
              StyleNumber = 22
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 15793935
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML 1st param'
              StyleNumber = 23
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML double string'
              StyleNumber = 24
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML single string'
              StyleNumber = 25
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML error'
              StyleNumber = 26
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 16737843
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML special'
              StyleNumber = 27
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML entity'
              StyleNumber = 28
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML comment'
              StyleNumber = 29
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clBlue
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'SGML block'
              StyleNumber = 31
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 32639
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Start'
              StyleNumber = 40
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'JS Default'
              StyleNumber = 41
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'JS Comment'
              StyleNumber = 42
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Line Comment'
              StyleNumber = 43
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'JS Doc Comment'
              StyleNumber = 44
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Number'
              StyleNumber = 45
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 13421568
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Word'
              StyleNumber = 46
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Keyword'
              StyleNumber = 47
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Double quoted string'
              StyleNumber = 48
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Single quoted string'
              StyleNumber = 49
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'JS Regex'
              StyleNumber = 52
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 32639
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Start'
              StyleNumber = 55
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP JS Default'
              StyleNumber = 56
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP JS Comment'
              StyleNumber = 57
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Line Comment'
              StyleNumber = 58
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP JS Doc Comment'
              StyleNumber = 59
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Number'
              StyleNumber = 60
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 14737632
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Word'
              StyleNumber = 61
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Keyword'
              StyleNumber = 62
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Double quoted string'
              StyleNumber = 63
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Single quoted string'
              StyleNumber = 64
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP JS Regex'
              StyleNumber = 67
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'VBS Default'
              StyleNumber = 71
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'VBS Comment'
              StyleNumber = 72
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'VBS Number'
              StyleNumber = 73
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'VBS KeyWord'
              StyleNumber = 74
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'VBS String'
              StyleNumber = 75
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Default'
              StyleNumber = 81
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Comment'
              StyleNumber = 82
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Number'
              StyleNumber = 83
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP KeyWord'
              StyleNumber = 84
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP String'
              StyleNumber = 85
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Python Start'
              StyleNumber = 90
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'Python Default'
              StyleNumber = 91
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'Python Comment'
              StyleNumber = 92
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'Python Number'
              StyleNumber = 93
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'Python Single quoted string'
              StyleNumber = 95
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = 15728623
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'Python function or method name definition'
              StyleNumber = 101
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Complex Variable'
              StyleNumber = 104
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'ASP Python Start'
              StyleNumber = 105
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Python Default'
              StyleNumber = 106
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Python Comment'
              StyleNumber = 107
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Python String'
              StyleNumber = 109
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'ASP Python Single quoted string'
              StyleNumber = 110
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
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
              ForeColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'PHP Default'
              StyleNumber = 118
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Double quoted string'
              StyleNumber = 119
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Single quoted string'
              StyleNumber = 120
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clOlive
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Keyword'
              StyleNumber = 121
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Number'
              StyleNumber = 122
            end
            item
              FontSize = 0
              FontStyles = [fsItalic]
              ForeColor = 10526720
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Variable'
              StyleNumber = 123
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Comment'
              StyleNumber = 124
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP One line Comment'
              StyleNumber = 125
            end
            item
              FontSize = 0
              FontStyles = [fsItalic]
              ForeColor = 10526720
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'PHP Variable in double quoted string'
              StyleNumber = 126
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12255232
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Bad Braces'
              StyleNumber = 35
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Control Chars'
              StyleNumber = 36
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Indent Guide'
              StyleNumber = 37
            end
            item
              FontName = 'Courier New'
              FontSize = 8
              FontStyles = []
              ForeColor = 12255232
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'White space'
              StyleNumber = 0
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Line Comment'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Doc Comment'
              StyleNumber = 3
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clNavy
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Number'
              StyleNumber = 4
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 30464
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Keyword'
              StyleNumber = 5
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Double quoted string'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Single quoted string'
              StyleNumber = 7
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Symbols/UUID'
              StyleNumber = 8
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 33023
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Preprocessor'
              StyleNumber = 9
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 30464
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Operators'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clNavy
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Identifier'
              StyleNumber = 11
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'EOL if string is not closed'
              StyleNumber = 12
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Verbatim strings for C#'
              StyleNumber = 13
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clHotLight
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Regular expressions'
              StyleNumber = 14
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Doc Comment Line'
              StyleNumber = 15
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'User-defined keywords'
              StyleNumber = 16
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 33023
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment keyword'
              StyleNumber = 17
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment keyword error'
              StyleNumber = 18
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGreen
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clYellow
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = [fsBold]
              ForeColor = clRed
              BackColor = clDefault
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
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Indent Guide'
              StyleNumber = 37
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Default'
              StyleNumber = 0
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 9474192
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Line Comment'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 224
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Number'
              StyleNumber = 3
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'String'
              StyleNumber = 4
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Operator'
              StyleNumber = 5
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clSilver
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Identifier'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Variable'
              StyleNumber = 7
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Column Name'
              StyleNumber = 8
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Statement'
              StyleNumber = 9
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Data Type'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'System Table'
              StyleNumber = 11
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Global Variable'
              StyleNumber = 12
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Function'
              StyleNumber = 13
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Stored Procedure'
              StyleNumber = 14
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Default Pref Datatype'
              StyleNumber = 15
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clDefault
              BackColor = clDefault
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
              ForeColor = clDefault
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'LineNumbers'
              StyleNumber = 33
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 12255232
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Ok Braces'
              StyleNumber = 34
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Bad Braces'
              StyleNumber = 35
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Control Chars'
              StyleNumber = 36
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clGray
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Indent Guide'
              StyleNumber = 37
            end
            item
              FontName = 'Courier New'
              FontSize = 8
              FontStyles = []
              ForeColor = 12255232
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'White space'
              StyleNumber = 0
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Comment'
              StyleNumber = 1
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Line Comment'
              StyleNumber = 2
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Doc Comment'
              StyleNumber = 3
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clNavy
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Number'
              StyleNumber = 4
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 30464
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Keyword'
              StyleNumber = 5
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Double quoted string'
              StyleNumber = 6
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Single quoted string'
              StyleNumber = 7
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Symbols/UUID'
              StyleNumber = 8
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 33023
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Preprocessor'
              StyleNumber = 9
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 30464
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Operators'
              StyleNumber = 10
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clNavy
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Identifier'
              StyleNumber = 11
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
              CharCase = CASE_MIXED
              EOLFilled = True
              Name = 'EOL if string is not closed'
              StyleNumber = 12
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clLime
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Verbatim strings for C#'
              StyleNumber = 13
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clHotLight
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Regular expressions'
              StyleNumber = 14
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = 4227327
              BackColor = clDefault
              CharCase = CASE_MIXED
              Name = 'Doc Comment Line'
              StyleNumber = 15
            end
            item
              FontSize = 0
              FontStyles = []
              ForeColor = clRed
              BackColor = clDefault
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
      Top = 419
      Width = 671
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
      Left = 206
      Top = 306
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
  object TBXSwitcher: TTBXSwitcher
    Theme = 'Default'
    Left = 813
    Top = 7
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
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6002184
      A5002184A500C6BDAD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D6008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0042EFFF00C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D6008CF7FF00EF8C6300EF8C6300EF8C6300E7733900E7632100B54A1800BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFCEBD0029AD
      D6008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0042EFFF00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B50029AD
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6007BC6EF00C694
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
      0000000000000000000000000000000000000000000008730800087308000873
      0800087308000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400FFFFFF00C6C6
      C60084848400C6C6C600FFFFFF00C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B58484000000000000000000000000000884AD000884
      AD000884AD000884AD000884AD000884AD00000000000873080018AD310021AD
      3100087308000000000000000000000000008484840084848400848484008484
      840084848400848484008484840084848400848484008484840084848400FFFF
      FF0084848400FFFFFF0084848400848484000000000052CEEF0052CEEF0052CE
      EF0029ADD60029ADD60052CEEF0029ADD60052CEEF0029ADD60052CEEF0029AD
      D60029ADD6000000000000000000000000000000000000000000C6A59C00FFE7
      D600FFE7D600FFE7D600F7DEC600F7DEC600F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B58484000000000000000000088CBD004ADEFF0031D6
      FF0039DEFF006BEFFF008CEFFF0029BDDE000884AD000873080018AD310018AD
      3100087308000000000000000000000000008400000084000000840000008400
      00000000000000000000848484008484840084848400FFFFFF00FFFFFF00FFFF
      FF008400000084000000840000008400000029ADD6009CFFFF009CFFFF005ACE
      EF005ACEEF0052CEEF009CFFFF005ACEEF006BE7FF0052CEEF006BE7FF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000000000C6A59C00FFEF
      D600FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00EFCE9C00B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF000873080008730800087308000873080021BD390018B5
      3100087308000873080008730800087308000000000000000000000000008400
      0000FF00000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF005ACEEF009CFF
      FF005ACEEF006BE7FF0052CEEF005ACEEF005ACEEF005ACEEF0052CEEF006BE7
      FF0052CEEF0029ADD60000000000000000000000000000000000C6ADA500FFEF
      E700FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600EFCE9C00B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF000873080039D65A0031CE5A0031CE520029C64A0029BD
      420021BD390018B5310018AD3100087308000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF009CFFFF005ACEEF009CFFFF0052CEEF006BE7FF005ACEEF006BE7FF0052CE
      EF006BE7FF0029ADD60000000000000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEC600F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF000873080039D65A0039D65A0039D65A0031CE520029C6
      4A0029C64A0021BD390021B53100087308000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF005ACEEF009CFFFF005ACEEF005ACEEF005ACEEF006BE7FF005ACEEF005ACE
      EF0052CEEF005ACEEF0000000000000000000000000000000000CEB5AD00FFFF
      F700FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00EFCE9C00B584840000000000088CBD00B5FFFF00B5FFFF00C6FF
      FF00DEFFFF00DEFFFF000873080008730800087308000873080039D65A0039D6
      5A00087308000873080008730800087308000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000029ADD6009CFFFF009CFFFF006BE7
      FF009CFFFF006BE7FF009CFFFF006BE7FF009CFFFF0052CEEF006BE7FF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000000000D6B5AD00FFFF
      FF00FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600F7D6A500B584840000000000088CBD00B5EFF70031BDE70010A5
      CE0010A5CE0010A5CE0018ADD6000894BD009CE7E7000873080039D65A0039D6
      5A00087308000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF0084EFFF009CFFFF009CFFFF009CFFFF0084EFFF006BE7FF0052CEEF005ACE
      EF005ACEEF005ACEEF00000000000000000000000000005A0000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7DEC600F7DEC600F7DEC600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B584840000000000088CBD0031C6E7005AE7FF0031D6
      FF0039DEFF006BEFFF008CEFFF0031BDE700088CB5000873080039D65A0039D6
      5A00087308000000000000000000000000000000000000000000000000008400
      000084000000FF0000008400000000000000FFFFFF00FFFF0000FFFFFF00FFFF
      00008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF006BE7FF009CFFFF009CFFFF005ACEEF009CFFFF006BE7FF009CFFFF005ACE
      EF006BE7FF0029ADD60000000000000000000000000000730800005A0000FFFF
      FF00FFC67300FFC67300FFC67300FFC67300FFC67300EFB57300EFB57300EFA5
      4A00EFA54A00F7DEB500B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF008CEFFF0031BDE700089CCE0008730800087308000873
      0800087308000000000000000000000000000000000000000000000000008400
      0000FF00000084000000FF00000000000000FFFF0000FFFFFF00FFFF0000FFFF
      FF008400000000000000000000000000000029ADD6009CFFFF009CFFFF009CFF
      FF009CFFFF009CFFFF005ACEEF009CFFFF005ACEEF009CFFFF009CFFFF005ACE
      EF0052CEEF005ACEEF000000000000000000000000000073080000730800005A
      0000FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFE7
      D600FFE7D600F7D6B500B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF008CEFFF0031BDE700089CCE000884AD00000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000000000000000000000000000029ADD6006BE7FF009CFFFF005ACE
      EF009CFFFF005ACEEF006BE7FF0029ADD60029ADD60029ADD60052CEEF0029AD
      D60029ADD6000000000000000000089418000000000008841000008C0800008C
      0800FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700F7DEC600F7DEC600FFEF
      D600E7DEC600C6BDAD00B584840000000000088CBD008CF7FF005AE7FF0031D6
      FF0039DEFF006BEFFF008CEFFF0031BDE700089CCE000884AD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052CEEF009CFFFF009CFF
      FF005ACEEF006BE7FF0000000000000000000000000000000000000000000000
      00000000000000000000000000001084180000000000008C0800008C0800FFFF
      FF00FFC67300FFC67300EFB57300EFB57300EFA54A00FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B584840000000000088CBD0094EFFF0084EFFF006BEF
      FF007BEFFF0094EFFF00ADFFFF0063E7FF0010A5CE000884AD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000005ACEEF005ACEEF0029AD
      D60029ADD60052CEEF00000000000000000000000000000000000000000029AD
      D600108418000000000031AD52000894180000000000008C0800E7C6B500FFFF
      FF00FFE7D600FFE7D600FFE7D600FFE7D600FFE7D600FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B0000000000088CBD00DEFFFF00DEFFFF00D6FF
      FF00B5FFFF00ADFFFF00ADFFFF00ADFFFF0073EFFF000884AD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000840000008400000084000000840000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000089418000894180031AD5200000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE9473000000000000000000000000000894BD00DEFFFF00DEFF
      FF00C6FFFF00ADFFFF00ADFFFF00A5F7FF000884B50000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000021AD390039C65A0010841800000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000088CBD00088C
      BD00088CBD00088CBD00088CBD00088CBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000089418000894180010841800108418000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000018A5CE0029ADD6001084B500000000000000
      00000000000000000000000000000000000000000000008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD00000000000000000000000000008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00000000000000000000000000B552180084522100844218008442
      1800844218008442180084421800844218008442180084421800844218008442
      18008442180084421800844218008442180000000000000000000000000021A5
      CE0021A5CE001084B500189CC60029ADD60029ADD600108CB5001084B50029AD
      D60029A5D600000000000000000000000000008CBD0063CEF700008CBD00A5F7
      FF0063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CE
      F70039ADDE00ADF7FF00008CBD0000000000008CBD0021A5CE005ACEEF0084E7
      FF0063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CE
      F70039ADDE001094C6000000000000000000B552180052392900523929005239
      2900523929005239290052392900523929005239290052392900523929005239
      29005239290052392900523929008442180000000000000000000000000021A5
      CE0029ADD60029ADD60018A5CE0021D6FF004AD6F70021A5CE005AC6DE0063C6
      DE0021A5CE00000000000000000000000000008CBD006BD6F700008CBD00ADF7
      FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6
      FF0039ADDE00BDEFF700008CBD0000000000008CBD004ABDE70031ADD60094EF
      FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6FF006BD6
      FF0039B5DE00CEF7FF00008CBD0000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008442180000000000108CB500189CC6004AC6
      E7007BDEF7004AC6E70039CEEF0018D6FF0039D6FF005ADEF70073C6D600ADE7
      EF0094E7F700108CB5000000000000000000008CBD0073D6FF00008CBD00ADF7
      FF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDE
      FF0042B5DE00BDEFF700008CBD0000000000008CBD0073D6FF00008CBD00ADFF
      FF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDE
      FF0042B5DE00CEF7FF00008CBD0000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084421800000000005AC6DE0029ADD6004AC6
      E70094E7F7008CE7F7005ADEF70021D6F70018D6FF0052DEF70073DEF7007BDE
      F70039CEEF0029ADD60021A5CE0000000000008CBD007BDEFF00008CBD00B5F7
      FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7
      FF0042B5DE00C6F7F700008CBD0000000000008CBD007BDEFF001094C60094EF
      FF0094EFFF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7
      FF004ABDE700CEF7FF001094C60000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084421800000000001894BD0021A5CE004AC6
      E7007BDEF70094E7F70073DEF7005AC6DE004AC6E70029CEF70018D6FF0018D6
      FF0018D6FF0021D6FF0029ADD60000000000008CBD0084E7FF00008CBD00BDF7
      FF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEF
      FF004ABDE700BDF7FF00008CBD0000000000008CBD0084E7FF0042B5DE005AC6
      EF00ADFFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEF
      FF004ABDE700CEF7FF00CEF7FF00008CBD00B55218000000000042424200FFFF
      FF0042424200424242000808080000000000424242006B6B6B006B6B6B006B6B
      6B006B6B6B0000000000000000008442180000000000188CBD0029ADD60042C6
      EF006BDEF7008CBDCE00949494009494940094949400949494004AC6E70018D6
      FF0018D6FF0018D6FF00189CCE0000000000008CBD008CEFFF00008CBD00D6F7
      FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7
      FF00BDDEDE00D6F7FF00008CBD0000000000008CBD008CE7FF007BDEFF0021A5
      CE00E7FFFF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7
      FF0094EFFF00E7FFFF00CEF7FF00008CBD00B552180000000000FFFFFF000808
      0800000000000000000000000000FFFFFF004242420000000000000000000000
      000000000000000000000000000084421800000000001894BD0029ADD60029AD
      D6005ADEF70094949400E7DEE700B5B5B500ADA5A500BDB5B5009494940052DE
      F7009CE7F700ADE7EF00189CC60000000000008CBD0094F7FF00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD0000000000008CBD0094F7FF0094F7FF001094
      C600008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD00B552180000000000FFFFFF000000
      000000000000FFFFFF0008080800FFFFFF000808080000000000000000000000
      00000000000000000000000000008442180000000000108CB50018A5CE0029AD
      D60042C6EF0094949400E7DEE700B5B5B500ADA5A500BDB5B500949494007BDE
      F700ADE7EF008CBDCE0021A5CE0000000000008CBD009CF7FF009CF7FF009CF7
      FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7
      FF00008CBD00000000000000000000000000008CBD009CF7FF009CF7FF009CF7
      FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7FF009CF7
      FF00088CBD00000000000000000000000000B55218000000000042424200FFFF
      FF00424242000000000000000000080808000000000000000000000000000000
      00000000000000000000000000008442180000000000000000001084B50021A5
      CE0039CEEF0094949400E7DEE700B5B5B500ADA5A500BDB5B5009494940063DE
      F70063C6DE001084B5000000000000000000008CBD00D6F7FF00A5F7FF00A5F7
      FF00A5F7FF00A5F7FF00A5F7FF00A5F7FF00A5F7FF00A5F7FF00A5F7FF00A5F7
      FF00008CBD00000000000000000000000000008CBD00E7FFFF00A5FFFF00A5FF
      FF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FF
      FF00088CBD00000000000000000000000000B552180000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084421800000000000000000000000000108C
      B500188CBD0094949400E7DEE700B5B5B500ADA5A500BDB5B50094949400189C
      C6001894BD0000000000000000000000000000000000008CBD00D6F7FF00A5F7
      FF00A5F7FF00A5F7FF00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD000000000000000000000000000000000000000000008CBD00E7FFFF00A5FF
      FF00A5FFFF00A5FFFF00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD0000000000000000000000000000000000B552180029211800292118002921
      1800292118002921180029211800292118002921180029211800292118002921
      1800292118002921180029211000844218000000000000000000000000000000
      00000000000094949400E7DEE700BDADAD00ADA5A500BDB5B500949494000000
      0000000000000000000000000000000000000000000000000000008CBD00008C
      BD00008CBD00008CBD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008CBD00008C
      BD00008CBD00008CBD0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5521800EFC67B00EFC67B00EFC6
      7B00EFC67B00EFC67B00EFC67B00EFC67B00EFC67B00EFC67B00EFC67B00EFC6
      7B00EFC67B00EFC67B00EFC67B00844218000000000000000000000000000000
      00000000000094949400B5B5B500A5A5A5009C949400ADA5A500949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5521800E78C3100E78C3100E78C
      3100E78C3100E78C3100E78C3100E78C3100E78C3100E78C3100E78C3100EF9C
      4A00EF9C5200D68C5A006B7BC600844218000000000000000000000000000000
      00000000000094949400E7E7E700BDB5B500A5A5A500B5A5A500949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A5522900A5522900A552
      2900A5522900A5522900A5522900A5522900A5522900A5522900A5522900A552
      2900A5522900A5522900A5522900000000000000000000000000000000000000
      00000000000094949400E7DEE700E7DEE700BDB5B500A59C9C00949494000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000094949400949494009494940094949400000000000000
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
      00000000000000000000000000000000000000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848C00316B9C00BD94
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000006394B500218CEF002173B500CE9C
      9400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFF700FFF7E700FFEFD600FFE7C600FFDEB500FFD6AD00FFD6AD00FFD6
      AD00FFD6AD00FFD6AD00FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A8CC600298CDE006384
      9C00BD8C94000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000004AB5FF0042A5FF00218CEF007B84
      9400CE9C94000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFF7E700FFEFD600FFE7C600FFDEB50000C6C60000C6
      C60000C6C60000C6C600FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004AB5FF0063BDFF00298C
      DE00217BCE00BD94940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000052BDFF005ABDFF00218C
      EF002173B500CE9C940000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFF7E700FFEFD600FFE7C600FFFFFF00FFFF
      FF00FFFFFF0000C6C600FFD6AD00CE6300001894CE001894CE001894CE001894
      CE00000000000000000000000000000000000000000000000000000000000000
      00000073080000000000000000000000000000000000000000004AB5FF004AB5
      FF00298CDE0063849C00BD949400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000031A5FF0052BDFF0042A5
      FF00218CEF0084849400CE9C9400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7E700FFEFD600FFE7C600FFDE
      B500FFD6AD00FFD6AD00FFD6AD00CE6300001894CE00ADF7FF007BF7FF006BEF
      F70031B5DE0031B5DE0031B5DE001894CE001894CE001094CE00000000000073
      08005AE78C000073080000000000000000000000000000000000000000004AB5
      FF0063BDFF00298CDE00217BC600000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000052BD
      FF005ABDFF003184D60052636B00000000009C6B6B0000630000007300000073
      0800427B310000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7E700FFEFD600FFE7
      C600FFDEB500FFD6AD00FFD6AD00CE6300001894CE0084DEEF0094FFFF007BF7
      FF0084F7FF0084F7FF007BF7FF006BE7FF0063DEF70031B5DE000073080052D6
      7B0042D66B0031C64A0000730800000000000000000000000000000000000000
      00004AB5FF004AB5FF00298CDE000000000000000000AD847B00AD847B00AD84
      7B00AD847B0000000000000000000000000000000000000000000000000031A5
      FF0052BDFF00C6CEDE009C9C9C00AD8C84004284310084A55A0063944200109C
      290021AD42004A944200000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00848484008484840084848400FFFFFF0084848400FFF7E7008484
      84008484840084848400FFD6AD00CE6300001894CE0063C6E70094FFFF0073F7
      FF007BF7FF0073EFFF0073EFFF0073EFFF007BEFFF007BF7FF0052C6E700108C
      210031C64A00109C210000000000000000000000000000000000000000000000
      0000000000000000000000000000BD949400E7C6A500FFFFDE00FFFFDE00FFFF
      DE00FFFFDE00E7CEB500AD847B00000000000000000000000000000000000000
      000000000000C6ADAD00CEA59400FFE7BD00FFF7CE00FFFFD600FFFFD600FFFF
      DE001084210031BD5200088418000000000000000000CE6300008484FF000000
      FF008484FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7
      E700FFEFD600FFE7C600FFDEB500CE6300001894CE001894CE0084E7F7007BF7
      FF007BF7FF0073EFFF0073EFFF0073EFFF0073EFFF0073EFFF0052C6E7000884
      100018AD29000884100000000000000000000000000000000000000000000000
      0000000000000000000000000000E7C6A500FFF7C600FFFFDE00FFFFDE00FFFF
      E700FFFFE700FFFFFF00E7CEB500000000000000000000000000000000000000
      000000000000BD8C8C00EFDEB5008CB57B00FFF7CE00FFFFD600FFFFDE00FFFF
      EF00108418004AE77B00087B18000000000000000000CE6300000000FF000000
      FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      F700FFF7E700FFEFD600FFE7C600CE6300001894CE005AD6EF001894CE0094FF
      FF0073F7FF007BEFFF0073EFFF0073EFFF0073EFFF0073EFFF0052C6E700088C
      100008A518000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00F7D6AD00EFCE9C00FFFFDE00FFFFDE00FFFF
      F700FFFFFF00FFFFF700FFFFEF00AD847B000000000000000000000000000000
      000000000000006300001884210052EF840010841800FFFFD600528C390021A5
      420042D66B0052F78C004AE77B002184310000000000CE6300008484FF000000
      FF008484FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFF700FFF7E700FFEFD600CE6300001894CE007BF7FF001894CE0073CE
      EF007BDEEF007BDEEF0094EFFF0084F7FF006BEFFF006BEFFF00088C100008A5
      1800088410000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00EFC69400F7C68C00FFFFD600FFFFDE00FFFF
      EF00FFFFF700FFFFEF00FFFFE700AD847B000000000000000000000000000000
      000000000000107B180052E77B0052F78C0039CE6B00FFFFD600FFFFDE001073
      100039CE6B005AF78C0052E77B005294420000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFF7E700CE6300001894CE008CFFFF0063E7F7001894
      CE001894CE001894CE005AC6E70000730800087B0800088C1000088C1000087B
      080073EFFF0018A5D60000000000000000000000000000000000000000000000
      00000000000000000000AD847B00EFCE9C00F7BD8C00F7DEB500FFF7CE00FFFF
      DE00FFFFDE00FFFFDE00FFFFDE00AD847B000000000000000000000000000000
      00001084180010841800108421004AE77B00108418001084180010841800FFFF
      D6009CBD8C001084180052943900C694940000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE6300001894CE0084FFFF0084FFFF0084FF
      FF007BF7FF007BF7FF001894CE001894CE006BCEEF0094E7F70084DEEF0094E7
      F700A5F7FF0039ADDE0000000000000000000000000000000000000000000000
      00000000000000000000AD847B00FFE7BD00EFC69400EFCE9C00F7E7B500FFFF
      DE00FFFFDE00FFFFDE00FFFFDE00AD847B000000000000000000000000000000
      000000000000D6ADA500087B180042DE730010841800F7D6A500F7E7BD00FFFF
      D600FFFFD6009CB58400FFFFD600000000000000000000000000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000000000001894CE008CFFFF007BF7FF007BF7
      FF007BF7FF0084F7FF0084FFFF0073F7FF001894CE001894CE001894CE001894
      CE00299CCE001894CE0000000000000000000000000000000000000000000000
      0000000000000000000000000000E7CEB500FFFFFF00FFE7BD00F7C68C00EFC6
      9400EFCE9C00FFF7C600DEC6AD00000000000000000000000000000000000000
      00000000000000000000C69C8C0031C6630021AD4200F7C69400EFBD8400F7C6
      9400F7D6AD00FFEFC600BD9C8C00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001894CE0084E7F70094FFFF008CFF
      FF0084FFFF0063D6EF001894CE001894CE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AD847B00DEC6B500FFFFD600F7DEAD00EFCE
      9C00F7DEAD00DEC6AD00AD847B00000000000000000000000000000000000000
      0000000000000000000000000000087B180021AD4200107310006394420084A5
      630042843100C69C940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001894CE001894CE001894
      CE001894CE001894CE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD847B00AD847B00AD84
      7B00AD847B000000000000000000000000000000000000000000000000000000
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
      8400B5848400B5848400B5848400000000000000000000000000C6A59C006BF7
      FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7
      FF006BF7FF007BC6EF00B5848400000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      00000000000000000000000000003131C6003131C6003131A5003131A5000000
      000000000000000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000C6A59C006BF7
      FF00EF8C6300EF8C6300EF8C6300E7733900E7733900E7733900E7632100E763
      2100E76321007BC6EF00B5848400000000000000000000000000C6A59C00FFEF
      D600C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A59C00C6A5
      9C00C6A59C00EFCE9C00B5848400000000000000000000000000000000000000
      000000000000000000003139FF003139FF003139FF003131E7003131E7003131
      A50000000000000000000000000000000000000000000000000000000000AD73
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5848400000000000000000000000000C6ADA5007BC6
      EF0029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD60029AD
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
      FF00FFCE9400FFCE9400FFC67B00FFC67B00FFC67B00FFBD6B00FFBD6B00FFBD
      6B00F7D6A500F7D6A500B5848400000000000000000000000000D6B5AD00FFFF
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
      FF00FFCE9400FFCE9400FFC67B00FFC67B00FFC67B00FFBD6B00FFBD6B00FFBD
      6B00F7DEC600F7D6B500B5848400000000000000000000000000DEBDB500FFFF
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
      7300FFFFFF00FFFFFF00B5848400000000000000000000000000E7C6B5007BC6
      EF0029ADD60029ADD60029ADD60029ADD60029ADD60029ADD6002184A500BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      00000000000000000000000000000873100042C67300009C0800009C0800009C
      0800009C0800009C0800007B0800004A00003184FF003184FF003184FF00B5DE
      F7003184FF003184FF003184FF00D6D6D600D6D6D600D6D6D600C6C6C600B584
      7300FFFFFF00B584840000000000000000000000000000000000E7C6B5006BF7
      FF00EF8C6300EF8C6300EF8C6300EF8C6300E7733900E7632100B54A1800C694
      7B00FFC67300CE94730000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      00000000000000000000000000000873100042C6730042C6730042C6730042C6
      730042C6730042C6730042C67300004A000000000000000000003184FF00B5DE
      F7003184FF00CECECE00CECECE00CECECE00CECECE00CECECE00C6C6C600B584
      7300B58484000000000000000000000000000000000000000000E7C6B5006BF7
      FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7FF006BF7FF007BC6EF00C694
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
      0000000000000000000000000000000000000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D684000000000000000000000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D684000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6840000FFEFD600FFEF
      D600FFE7CE00FFE7CE00FFE7C600FFDEC600FFDEBD00FFDEBD00FFDEB500FFDE
      B500FFDEB500FFDEB500D68400000000000000000000D6840000FFF7EF00FFEF
      DE00FFEFD600FFEFD600FFE7CE00FFE7C600FFE7C600FFE7C600FFDEBD00FFDE
      BD00FFDEBD00FFE7C600D6840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6840000FFEFDE00FFEF
      D600CE844200CE844200CE844200CE844200CE844200CE844200CE844200CE84
      4200FFDEB500FFDEB500D68400000000000000000000D6840000FFF7E700FFEF
      D600FFE7CE00FFE7CE00FFE7C600FFE7BD00FFDEBD00FFDEBD00FFDEBD00FFDE
      BD00F7D6B500FFDEBD00D6840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6840000FFF7E700FFEF
      DE00FFEFDE00FFEFD600FFE7CE00FFE7CE00FFE7C600FFE7C600FFDEBD00FFDE
      BD00FFDEB500FFDEB500D68400000000000000000000D6840000FFF7EF00FFEF
      DE00E7B58C00D68C4200D68C4200CE8C4A00CE8C4A00D68C4200E7A56300F7D6
      AD00FFDEBD00FFDEBD00D6840000000000000000000029ADD60029ADD60029AD
      D600292929005A524A0029ADD60029ADD60029ADD60029ADD60029ADD60029AD
      D60029ADD60029ADD60029ADD600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6840000FFF7E700FFF7
      E700FFEFDE00F7D6AD00CE844200CE844200D68C5200CE844200EFC6A500FFDE
      C600FFDEBD00FFDEB500D68400000000000000000000D6840000FFFFF700FFEF
      E700FFE7C600DE8C3900DE7B0800E7A56B00EFC6A500DE732100D6730000DE94
      4A00FFDEBD00FFE7BD00D6840000000000000000000029ADD6008CF7FF008CF7
      FF005A524A00424242005263630073DEF70073DEF70073DEF70029ADD60029AD
      D60073DEF70073DEF70029ADD6000000000000000000848484008C5A5A008C5A
      5A008C5A5A008C5A5A008C5A5A008C5A5A008C5A5A008C5A5A008C5A5A000000
      00000000000000000000000000000000000000000000D6840000FFF7EF00FFF7
      E700FFEFDE00D68C5200D6845A00FFEFD600FFEFD600DEAD8C00D68C5200FFDE
      BD00FFDEC600FFDEBD00D68400000000000000000000D6840000FFFFF700FFF7
      E700FFEFD600E79C5200DE732100EFBD9400FFEFE700E7A56300D6730000DE73
      2100FFDEBD00FFE7C600D6840000000000000000000029ADD6008CF7FF008CF7
      FF0073DEF70052737B00427BAD0018526B008C5A520073DEF70029ADD600ADFF
      FF0029ADD60073DEF70029ADD600000000000000000084848400FFFFEF00FFF7
      E700FFF7DE00F7EFDE00F7EFDE00F7EFDE00FFEFDE00F7E7D6008C5A5A000000
      000000000000C6000000000000000000000000000000D6840000FFF7EF00FFF7
      EF00FFEFD600D68C4A00E7B59400FFEFDE00FFEFD600FFDEBD00D68C5200FFD6
      B500FFE7C600FFDEC600D68400000000000000000000D6840000FFFFFF00FFF7
      EF00FFEFD600E79C5A00DE8C3900E7AD7B00EFC6A500D68C4200D6730000E7A5
      6300FFE7C600FFE7C600D6840000000000000000000029ADD6009CFFFF008CF7
      FF008CF7FF0052737B0042739400947B7300C67331008C5A520029ADD600ADFF
      FF00ADFFFF0029ADD60029ADD600000000000000000084848400FFF7E700F7E7
      CE00F7E7CE00F7DECE00F7DEC600F7DEC600F7E7CE00EFDECE008C5A5A000000
      0000FF000000FF000000C60000000000000000000000D6840000FFFFF700FFF7
      EF00FFEFDE00D68C4A00DEB58C00FFEFDE00FFEFDE00FFDEBD00D6845A00FFDE
      BD00FFE7CE00FFE7C600D68400000000000000000000D6840000FFFFFF00FFF7
      EF00FFEFDE00E79C5A00DE943900CE8C4A00D68C4200D6730000CE8C4A00FFDE
      BD00FFE7C600FFE7CE00D6840000000000000000000029ADD6009CFFFF009CFF
      FF008CF7FF0073DEF7007B737B00F7BD8400F7AD5A00C67339008C5A520029AD
      D60029ADD60029ADD60029ADD600000000000000000084848400FFF7EF00FFDE
      B500FFDEB500FFDEB500FFD6B500FFD6B500FFDEBD00EFDECE0094636300FF00
      0000FF000000FF000000FF000000C600000000000000D6840000FFFFF700FFFF
      F700FFF7E700D68C4A00E7B59400FFF7E700FFF7E700FFDEC600D6845A00FFDE
      C600FFEFD600FFE7CE00D68400000000000000000000D6840000FFFFFF00FFFF
      F700F7EFE700E7A56300DE943900E7B58C00EFCEAD00DE8C3900DE732100EFBD
      9400FFE7CE00FFE7CE00D6840000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF008CF7FF00AD7B7300FFD6AD00FFC68C00EFA55A00C67339008C5A
      520073DEF70073DEF70029ADD600000000000000000084848400FFFFF700FFD6
      A500FFD6A500FFD6A500FFD6A500FFD6A500FFD6A500F7E7DE00A57B73000000
      0000FF000000FF000000C60000000000000000000000D6840000FFFFFF00FFFF
      F700FFF7EF00D68C4A00E7BD9400FFF7EF00FFF7E700FFE7CE00D6845A00FFE7
      CE00FFEFD600FFEFD600D68400000000000000000000D6840000FFFFFF00FFFF
      FF00F7F7EF00E7A56B00DE944200EFC6A500FFF7E700E79C5A00DE7B0800E7AD
      7300FFEFD600FFEFD600D6840000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF008CF7FF00AD7B7300FFD6AD00FFC68C00F7AD5A00C673
      31008C5A520073DEF70029ADD600000000000000000084848400FFFFFF00FFEF
      DE00FFE7D600FFE7D600FFE7D600FFEFD600FFF7E700EFE7DE00C6000000C600
      0000FF000000FF000000C60000000000000000000000D6840000FFFFFF00FFFF
      FF00E7BD9400D6843100D68C5200FFE7CE00FFF7EF00DEAD8C00D6843900DEB5
      8C00FFEFDE00FFEFD600D68400000000000000000000D6840000FFFFFF00FFFF
      FF00F7D6AD00E79C5200DE943900E7A56300E7A56B00DE8C3900DE944200EFCE
      AD00FFEFD600FFEFDE00D6840000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF009CFFFF008CF7FF00AD7B7300FFD6AD00FFC68C00EFA5
      5A00B56B39008C5A520029ADD600000000000000000084848400FFFFFF00FFD6
      A500FFD6A500FFD6A500FFD6A500EFDEDE00FF000000FF000000FF000000FF00
      0000FF000000FF000000C60000000000000000000000D6840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00FFF7EF00FFF7E700FFF7
      E700FFEFDE00FFEFDE00D68400000000000000000000D6840000FFFFFF00FFFF
      FF00EFCEAD00EFC69C00EFC6A500EFC69C00E7B58C00EFC69C00F7D6B500FFEF
      DE00FFEFD600FFEFE700D6840000000000000000000029ADD6009CFFFF009CFF
      FF009CFFFF009CFFFF009CFFFF009CFFFF008CF7FF00AD7B7300FFD6AD009C94
      A50031ADF7000010AD0000009C00000000000000000084848400FFFFFF00FFE7
      D600FFE7D600FFE7D600FFE7D600B58C8C00B58C8C00B58C8C00B58C8C000000
      00000000000000000000000000000000000000000000D6840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00FFF7EF00FFF7
      EF00FFF7E700FFEFDE00D68400000000000000000000D6840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      EF00FFF7EF00FFFFF700D6840000000000000000000029ADD60029ADD60029AD
      D60029ADD60029ADD60029ADD60029ADD60029ADD60029ADD600AD7B73004A9C
      EF001029D6001029D6000010AD0000009C000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00B58C8C00EFB56B00C68C7B00000000000000
      0000000000000000000000000000000000000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D684000000000000000000000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D684000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000018
      C6006B8CFF00106BFF001029D600000000000000000084848400848484008484
      8400848484008484840084848400848484008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000018C6000018C60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008442210084422100844221008442210084422100844221000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008442
      210084422100CE630000CE630000CE630000CE630000CE630000CE6300008442
      2100844221000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A00000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D6840000000000000000000000000000000000009C4A1800C663
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000C66300008442210000000000000000000000000000000000000000000000
      00000000000000000000A5947B00AD847B009C636300BD734A00000000000000
      00000000000000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFF700FFEFDE00FFE7CE00FFDEB500FFD6A500FFCE9400FFCE9400FFCE
      9400FFCE9400FFCE9400FFCE9400BD4A000000000000D6840000FFF7EF00FFEF
      DE00FFEFD600FFEFD600FFEFD600FFE7CE00FFE7C600FFE7BD00FFDEBD00FFDE
      BD00FFDEBD00FFE7C600D684000000000000000000009C4A1800C6630000C663
      0000C6630000C6630000C6630000C6630000C6630000CE630000CE630000CE63
      0000CE630000C663000084422100000000000000000000000000000000000000
      000000000000A57B6300BD633900C6634200C66342009C52420084736B000000
      00000000000000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFEFDE00FFE7CE00FFDEB500FFD6A500FFCE9400FFCE
      9400FFCE9400FFCE9400FFCE9400BD4A000000000000D6840000FFF7E700FFEF
      D600FFEFD600FFE7C600FFE7C600FFE7C600FFDEBD00FFDEB500FFDEB500FFDE
      B500FFD6B500FFDEBD00D68400000000000000000000BD5A0800C6630000C663
      0000C6630000CE731800D68C4200DE9C5A00DE9C5A00D6843900CE630000CE63
      0000CE630000CE63000084422100000000000000000000000000000000000000
      0000BD734A00C65A1800A54218008C522100B5421800BD522900CE6339008442
      31000000000000000000000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFF7008C2900008C2900008C290000FFD6A5000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6840000FFF7EF00FFEF
      DE00FFEFDE00EFC69C00DE844200D6843100E7B58400FFDEBD00FFDEBD00FFDE
      B500FFDEB500FFDEBD00D684000000000000A5521000CE6B0000CE6B0000CE6B
      0000DE9C5A00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFC69C00C6630000CE63
      0000CE630000CE630000CE63000084422100000000000000000000000000BD73
      4A00BD521000A5390000AD420000316300006B4A0000AD390000A5390800BD5A
      39007352420000000000000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFFF008C2900008C2900008C290000FFDEB5000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6840000FFFFF700FFEF
      DE00FFEFDE00FFEFD600DEAD8400D6841800F7CEAD00FFE7C600FFE7BD00FFDE
      BD00FFDEB500FFE7BD00D684000000000000AD521000CE731000CE731000CE73
      1800FFFFFF00EFC69400CE7B2900CE731800CE731000BD5A0800C6630000C663
      0000CE630000CE630000CE630000844221000000000000000000BD734A009C52
      4200AD420000CE5A0000BD6B0000007B000021730000C65A0000B54A00007363
      2100845A310073524200000000000000000000000000BD4A0000FFFFFF002952
      FF002952FF002952FF00FFFFFF008C2900008C2900008C290000FFE7CE000084
      BD000084BD000084BD00FFCE9400BD4A000000000000D6840000FFFFF700FFF7
      E700FFEFDE00FFF7DE00F7D6AD00D6842100EFBD8C00FFEFD600FFE7C600FFDE
      BD00FFDEBD00FFE7BD00D684000000000000AD521000D6843100D6843100CE7B
      2900FFFFFF00D6843900CE7B2900CE731000CE6B0000EFC69C00C65A0000C663
      0000CE630000CE630000CE630000844221000000000000000000BD734A009C52
      4200CE5A0000D67B0000848C000039940000C6840000BD7B000021730000086B
      0800636B2900BD734A00000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFEFDE00FFE7
      CE00FFDEB500FFD6A500FFCE9400BD4A000000000000D6840000FFFFFF00FFF7
      E700FFF7E700FFF7E700FFE7CE00DE844200E7AD8400FFE7CE00FFE7C600FFE7
      C600FFDEBD00FFE7C600D684000000000000AD521000DEA56300DE945200D68C
      4200FFFFFF00E7B58400D6843900CE7B2900CE731000FFFFFF00EFC69400C663
      0000CE630000CE630000CE630000844221000000000000000000BD734A009C52
      4200C673000029A5180031AD2100D6AD1800FF9C0800E78C0000318400000073
      0000426B1000636B2900000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFF7000084
      00000084000000840000FFD6A500BD4A000000000000D6840000FFFFFF00FFF7
      EF00FFF7E700FFF7E700FFF7E700E7B58400DE7B4A00FFE7C600FFEFD600FFE7
      C600FFE7C600FFE7CE00D684000000000000AD521000E7B58400E7B57B00DE94
      5200F7D6BD00FFFFFF00F7E7D600F7E7D600F7EFDE00FFFFFF00FFFFFF00EFCE
      AD00CE630000CE630000CE6300008442210000000000000000009C524200BD73
      4A007B94080010C64A0031CE63007BCE6B00FFCE5A00FFA510006B8C00000084
      0000296B0000296B0000000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFFF000084
      00000084000000840000FFDEB500BD4A000000000000D6840000FFFFFF00FFFF
      F700FFF7EF00FFF7E700FFF7EF00EFC6A500DE844200F7D6AD00FFEFD600FFE7
      CE00FFE7C600FFE7CE00D684000000000000AD521000E7B57B00F7D6BD00E7B5
      7B00E7A56B00EFC69C00F7D6BD00EFCEAD00F7DECE00FFFFFF00FFFFFF00EFCE
      A500CE630000CE630000CE630000844221000000000000000000A5947B00BD73
      4A0031BD390042D67300A5E79C00A5F7B500D6E79400D6B52900B58C00003184
      00005A7B2900A5947B00000000000000000000000000BD4A0000FFFFFF00BD84
      8400BD848400BD848400FFFFFF00DE630000DE630000DE630000FFFFFF000084
      00000084000000840000FFE7CE00BD4A000000000000D6840000FFFFFF00FFFF
      F700FFF7EF00FFF7EF00FFF7EF00FFE7CE00DE7B4A00EFC69C00FFF7DE00FFEF
      D600FFE7CE00FFEFD600D68400000000000000000000B55A1000F7DEC600F7DE
      C600E7BD8C00E7A56B00DE9C5A00D68C4200D6843100FFFFFF00EFBD8C00CE63
      0000CE630000CE6300008442210000000000000000000000000000000000A594
      7B0084C6730063DE7B00BDF7AD00D6FFBD005ADE840039B53100F78C0000C684
      0800A5947B0000000000000000000000000000000000BD4A0000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFEFDE00BD4A000000000000D6840000FFFFFF00FFFF
      FF00FFFFF700FFF7EF00FFF7EF00EFDECE00DE7B4A00DE7B4A00F7D6B500FFEF
      DE00FFEFD600FFEFD600D68400000000000000000000AD521000E7B58400F7E7
      D600F7DECE00E7BD8C00DE9C5A00D68C4200D6843100EFBD8C00CE731800CE6B
      0000CE630000C663000084422100000000000000000000000000000000000000
      0000A5947B00ADC68C00A5D6840094DE8C0052CE63004AB53900D69C4200A594
      7B000000000000000000000000000000000000000000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A0000BD4A
      0000BD4A0000BD4A0000BD4A0000BD4A000000000000D6840000FFFFFF00FFFF
      FF00FFFFFF00FFFFF700FFFFF700E7D6C600EFBD9400E7B58C00F7D6AD00FFF7
      DE00FFEFD600FFF7DE00D6840000000000000000000000000000AD521000E7BD
      8C00F7DECE00F7E7D600EFCEAD00E7BD8C00E7AD7300DE9C5A00D68C4200CE73
      1000C66300008C42180000000000000000000000000000000000000000000000
      000000000000A5947B00A5947B00AD847B00A5947B00A5947B00A5947B000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      EF00FFF7EF00FFFFF700D684000000000000000000000000000000000000AD52
      1000B55A1000EFBD8C00EFCEA500EFCEA500EFBD8C00DEA56300CE7B2900B55A
      10008C4221000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6840000D684
      0000D6840000D6840000D6840000D6840000D6840000D6840000D6840000D684
      0000D6840000D684000000000000000000000000000000000000000000000000
      000000000000AD521000B55A1000B55A1000B55A1000B55A10009C4A18000000
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
      0000000000008442210084422100844221008442210084422100844221000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C3908000000000000000000000000000000000000000000000000000000
      00000000000000000000E7C6B500E7C6B500E7C6B500D6BDAD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008442
      210084422100CE630000CE630000CE630000CE630000CE630000CE6300008442
      2100844221000000000000000000000000000000000000000000000000000000
      0000CE6B0000CE6B0000CE6B0000CE6B00009C390800FF9C9400EFA55A00DE84
      29009C3908000000000000000000000000000000000000000000000000000000
      000000000000E7C6B500F7E7E700CED6D600CED6D600F7E7E700D6BDAD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009C4A1000C663
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000C66300008442210000000000000000000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C3908000000000000000000000000000000000000000000000000000000
      0000E7C6B500F7E7E700FFFFFF00D6845A00C66B5200BDBDBD00F7E7E700D6BD
      AD00000000000000000000000000000000000000000000000000000000000000
      00000000000000630000089C210000630000004A000000000000000000000000
      000000000000000000000000000000000000000000009C4A1800C6630000CE63
      0000C6630000C6630000C6630000C6630000CE630000CE630000CE630000CE63
      0000CE630000C663000084422100000000000000000000000000000000000000
      0000CE6B00000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E7C6
      B500F7E7E700FFFFFF00D6845A00C66B5200BD6B4A00C66B5200BDBDBD00F7E7
      E700D6BDAD000000000000000000000000000000000000000000000000000000
      00000000000000630000109C2100089C210000630000004A0000000000000000
      00000000000000000000000000000000000000000000BD5A0000C6630000C663
      0000C6630000BD5A0000D68C3900DE945200DE9C5A00D68C4200CE7B2100CE63
      0000CE630000CE63000084422100000000000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C3908000000000000000000000000000000000000000000E7C6B500F7E7
      E700FFFFFF00D6845A00C66B5200FFEFE700FFEFE700FFE7D600BD6B4A00BDBD
      BD00F7E7E700D6BDAD0000000000000000000000000000000000000000000000
      0000000000000063000010A5290010A52900089C210000630000004A00000000
      000000000000000000000000000000000000AD521000C66B0800C66B0800C66B
      0800C6630000C6630000EFC69C00FFF7F700FFF7F700FFF7F700FFF7F700DE9C
      5A00CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B0000CE6B0000CE6B0000CE6B00009C390800FF9C9400EFA55A00DE84
      29009C39080000000000000000000000000000000000E7C6B500F7E7E700FFFF
      FF00D6845A00C66B5200C66B5200C66B5200FFEFE700C66B5200C66B5200BD6B
      4A00BDBDBD00F7E7E700D6BDAD00000000000000000000000000000000000000
      0000000000000063000010B5390010A52900089C2100089C210000630000004A
      000000000000000000000000000000000000AD521000CE6B1000CE731800CE73
      1800CE731800CE731800CE731800CE731800CE731800CE731800E7BD8C00FFF7
      F700CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C39080000000000000000000000000000000000EFD6C600FFFFFF00D684
      5A00CE7B5200C66B5200C66B5200C66B5200FFEFE700C66B5200C66B5200C66B
      5200BD6B4A00BDBDBD00D6BDAD00000000000000000000000000000000000000
      0000000000000063000018B54A0010AD390010AD390010AD390010A529000063
      000000520000000000000000000000000000AD521000D6843100CE7B2900CE7B
      2900CE7B2900CE7B2900EFCEAD00CE731800C66B0800BD5A0000CE6B1000FFF7
      F700CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B00000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EFD6C600FFFFFF00FFE7
      CE00E7A57300CE7B5200C66B5200C66B5200FFEFE700C66B5200C66B5200C66B
      5200D6845A00CED6D600D6BDAD00000000000000000000000000000000000000
      0000000000000063000052BD52005ABD52004ABD52004ABD52004ABD52000063
      000000520000000000000000000000000000AD5A1000DE9C6300DE945200D68C
      3900D68C4200EFD6B500FFF7F700CE7B2900CE731800C66B0800DE9C5A00FFF7
      F700CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C39080000000000000000000000000000000000E7C6B500F7E7E700FFFF
      FF00FFE7CE00E7A57300CE7B5200FFEFE700FFEFE700C66B5200C66B5200D684
      5A00E7EFEF00F7E7E700D6BDAD00000000000000000000000000000000000000
      000000000000006300005AC65A006BC66B006BC663004ABD520000630000004A
      000000000000000000000000000000000000AD521000E7B58400E7AD7B00DE94
      5200F7DECE00FFF7F700FFF7F700FFEFE700F7E7CE00F7DECE00FFF7F700E7BD
      8C00CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B0000CE6B0000CE6B0000CE6B00009C390800FF9C9400EFA55A00DE84
      29009C3908000000000000000000000000000000000000000000E7C6B500F7E7
      E700FFFFFF00FFE7CE00E7A57300CE7B5200E7AD8400C66B5200D6845A00FFFF
      FF00F7E7E700E7C6B50000000000000000000000000000000000000000000000
      000000000000006300006BC66B007BD67B007BD67B0000630000004A00000000
      000000000000000000000000000000000000AD521000E7B58400F7D6B500E7AD
      7B00F7E7CE00FFF7F700FFF7F700F7E7CE00EFCEAD00EFC6A500DE9C6300C663
      0000CE630000CE630000CE630000844221000000000000000000000000000000
      0000CE6B00000000000000000000000000009C3908009C3908009C3908009C39
      08009C390800000000000000000000000000000000000000000000000000E7C6
      B500F7E7E700FFFFFF00FFE7CE00E7A57300FFEFE700D6845A00FFFFFF00F7E7
      E700E7C6B5000000000000000000000000000000000000000000000000000000
      0000000000000063000063C663008CD68C0000630000004A0000000000000000
      00000000000000000000000000000000000000000000AD5A1000F7DEC600F7DE
      C600E7BD8C00F7D6BD00FFF7F700D68C4200D6843100CE7B2100CE6B1000CE63
      0000CE630000CE630000844221000000000000000000000000009C3908009C39
      08009C3908009C3908009C390800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E7C6B500F7E7E700FFFFFF00FFE7CE00D6845A00FFFFFF00F7E7E700E7C6
      B500000000000000000000000000000000000000000000000000000000000000
      0000000000000063000039AD390000630000004A000000000000000000000000
      00000000000000000000000000000000000000000000AD521000E7B58400F7E7
      D600F7DECE00E7BD8C00EFD6B500D68C4200D6843100CE7B2100CE731800C66B
      0800CE630000C6630000844221000000000000000000000000009C390800FF9C
      9400EFA55A00DE8429009C390800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7C6B500F7E7E700FFFFFF00FFFFFF00F7E7E700E7C6B5000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000004A000000630000004A00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD521000E7BD
      8C00F7DECE00F7E7D600EFD6B500E7BD8C00DEA56B00DE9C6300D68C4200CE6B
      1000C66300008C4A1800000000000000000000000000000000009C3908009C39
      08009C3908009C3908009C390800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E7C6B500E7C6B500E7C6B500E7C6B500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000AD52
      1000B55A1000E7BD8C00EFCEA500EFCEA500E7BD8C00DE9C6300CE7B2900B55A
      10008C4221000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AD5A1000B55A1000B55A1000B55A1000AD5A10009C4A10000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000873DE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000006BD600187BE700006BD600006BD60000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE6300001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      210000000000000000000000000000000000000000000000000018CEF70018CE
      F70000000000000000000000000000000000000000000000000000000000006B
      D6002994F70029ADFF00006BD6000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFF700FFF7E700FFEFD600FFE7C600FFD6AD00FFD6AD00F7D6AD00FFD6
      AD00FFD6AD00FFD6AD00FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000018CE
      F70018CEF7000000000000000000006BD600006BD600006BD600006BD6002194
      F70029A5FF00006BD600000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6C600BDC6
      C600BDC6C600BDC6C600FFD6AD00CE63000000000000000000002163AD000000
      0000000000000000000000000000006300000063000000630000006300000063
      0000006300000000000000000000000000000000000000000000000000001863
      AD00000000000000000000000000006300000063000000630000006300000063
      0000006300000000000000000000000000000000000000000000000000000000
      000000000000000000000873DE000873DE002194F7004ACEF7004ACEF700218C
      EF00006BD60000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00292929005A524A00ADADAD00FFF7E700FFE7C600FFE7C600FFD6AD00FFD6
      AD00F7D6AD00FFD6AD00FFD6AD00CE63000000000000000000002163AD002163
      AD000000000000000000000000000063000042B55A0031AD4200189C29001094
      21000063000000000000000000000000000000000000000000001863AD001863
      AD000000000000000000000000000063000031B5630021A5420018942900108C
      2100006300000000000000000000000000000000000000000000000000000000
      00000000000010635A001073DE0029A5FF001884EF00299CFF0029ADFF00299C
      FF00006BD60000000000000000000000000000000000CE630000FFFFFF00BDC6
      C6005A524A0042424200526363009C9C9C00BDC6C600BDC6C600BDC6C600BDC6
      C600BDC6C600BDC6C600FFD6AD00CE6300002163AD002163AD004294DE004294
      DE002163AD000000000000000000006300000063000000630000006300000063
      000000630000000000000000000000000000000000001863AD005ABDBD005ABD
      BD001863AD001863AD0000000000006300000063000000630000006300000063
      0000006300000000000000000000000000000000000000000000000000000000
      0000088C1000189C290010635A002194F7004ACEF700187BE7001884EF00299C
      F700006BD60000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF0052737B00427BAD0018526B008C5A5200ADADAD00FFE7C600FFE7
      C600FFD6AD00FFD6AD00FFD6AD00CE6300002163AD00ADCEB50094C69C004294
      DE004294DE002163AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001863AD00B5DEEF007BCEC6005ABD
      BD005ABDBD001863AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000073
      0800109418004AB57B0029BD520010635A001884EF00299CFF001073DE001884
      E700006BD60000000000000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C60052737B0042739400947B7300C67331008C5A52009C9C9C00BDC6
      C600BDC6C600ADB5BD00FFD6AD00CE6300002163AD002163AD004294DE0073C6
      84002163AD000000000000000000006300000063000000630000006300000063
      000000630000006300000063000000630000000000001863AD00C6D6E7005ABD
      BD001863AD001863AD0000000000006300000063000000630000006300000063
      000000630000006300000063000000630000000000000000000000000000087B
      080031C65A0018A5310031C663001094180010635A00187BE700299CF7000873
      DE00006BD60000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007B737B00F7BD8400F7AD5A00C67339008C5A5200ADAD
      AD00FFE7C600FFD6AD00FFD6AD00CE63000000000000000000002163AD002163
      AD000000000000000000000000000063000052BD630042B55A0042B55A0039B5
      520029A53900189C2900109421000073080000000000000000001863AD001863
      AD000000000000000000000000000063000031B5630031B5630031AD5A0031AD
      5A0021A54200219C3100108C2100006B0800000000000000000000000000087B
      080031C65A00189C2900088C1000007B0800007B000010635A001073DE00006B
      D6000000000000000000000000000000000000000000CE630000FFFFFF00BDC6
      C600BDC6C600BDC6C600AD7B7300FFD6AD00FFC68C00EFA55A00C67339008C5A
      52009C9C9C00BDC6C600FFD6AD00CE63000000000000000000002163AD000000
      0000000000000000000000000000006300000063000000630000006300000063
      0000006300000063000000630000006300000000000000000000000000001863
      AD00000000000000000000000000006300000063000000630000006300000063
      000000630000006300000063000000630000000000000000000000000000087B
      080029B54A00189C290000840800007B0000007B0000007B000010635A000000
      00000000000000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00AD7B7300FFD6AD00FFC68C00F7AD5A00C673
      31008C5A5200ADADAD00FFD6AD00CE6300000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001094
      1800088C100008841000007B0000007B0000006B0000006B00000000000018CE
      F70018CEF70000000000000000000000000000000000CE630000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00AD7B7300FFD6AD00FFC68C00EFA5
      5A00B56B39008C5A52009C9C9C00CE6300001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000000000000000000000189C290031C6
      5A0000730800006B0000006B0000006B0000006B000000000000000000000000
      000018CEF70018CEF700000000000000000000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000AD7B7300FFD6AD009C94
      A50031ADF7000010AD0000009C00CE6300001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      2100000000000000000000000000000000001084210010842100108421001084
      2100108421001084210010842100108421001084210010842100108421001084
      21000000000000000000000000000000000000000000107B100031C65A000884
      1000006B00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000AD7B73004A9C
      EF001029D6001029D6000010AD0000009C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000006B0000006B000000730000006B
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000018
      C6006B8CFF00106BFF001029D600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000010942100006B0000000000000000
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
      0000000000000000000000000000000000000000000084848C00316B9C00BD94
      9400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000C6A59C00FFEF
      D600F7E7C600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE9C00EFCE9400EFCE
      9400EFCE9400F7D69C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A8CC600298CDE006384
      9C00BD8C94000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59C00FFEF
      D600848484008484840084848400848484008484840084848400848484008484
      840084848400EFCE9C00B5848400000000000000000000000000C6A59C00FFEF
      D60031C6520029BD4A0010A5310010A5310010A53100008C0800008C0800008C
      0800006B0800EFCE9C00B5848400000000000000000000000000000000004242
      4200000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004AB5FF0063BDFF00298C
      DE00217BCE00BD94940000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000C6ADA500FFEF
      E700F7E7D600F7E7CE00F7DEC600F7DEBD00F7D6B500F7D6AD00EFCE9C00EFCE
      9C00EFCE9400EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000004242420000000000424242000000
      00000000000000000000000000000000000000000000000000004AB5FF004AB5
      FF00298CDE0063849C00BD949400000000000000000000000000000000000000
      0000000000000000000000000000000000000073080000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000C6ADA500FFF7
      E700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEB500F7D6B500F7D6AD00EFCE
      9C00EFCE9C00EFCE9400B5848400000000000000000000000000000000000000
      0000424242000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004AB5
      FF0063BDFF00298CDE00217BC600000000000000000000000000000000000000
      000000000000000000000000000000000000007308000073080000730800FFFF
      F7000063CE000063CE000063CE000063CE000063CE000063CE000063CE000063
      CE000063CE00EFCE9C00B5848400000000000000000000000000CEB5AD00FFFF
      F70073A5FF00004AF700004AF700004AF700004AF700004AF700004AF700004A
      F700004AF700EFCE9C00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004AB5FF004AB5FF00298CDE000000000000000000AD847B00AD847B00AD84
      7B00AD847B000000000000000000000000000073080000730800007308000073
      080010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7
      FF0010F7FF00EFCE9C00B5848400000000000000000000000000D6B5AD00FFFF
      FF00FFF7EF00FFEFE700F7E7D600F7E7CE00F7E7C600F7DEC600F7DEBD00F7D6
      AD00F7D6A500F7D6A500B5848400000000000000000000000000000000008484
      8400000000004242420000000000000000000000000042424200000000004242
      4200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BD949400E7C6A500FFFFDE00FFFFDE00FFFF
      DE00FFFFDE00E7CEB500AD847B0000000000008C0800008C0800008C0800008C
      080010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7FF0010F7
      FF0010F7FF00F7DEB500B5848400000000000000000000000000D6BDB500FFFF
      FF00FFF7F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7E7C600F7DEC600F7DE
      BD00F7D6B500F7D6AD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E7C6A500FFF7C600FFFFDE00FFFFDE00FFFF
      E700FFFFE700FFFFFF00E7CEB50000000000008C0800008C0800008C0800FFFF
      FF000063CE000063CE000063CE000063CE000063CE000063CE000063CE000063
      CE000063CE00F7DEB500B5848400000000000000000000000000D6BDB500FFFF
      FF0031C6520029BD4A0010A5310010A5310010A53100008C0800008C0800008C
      0800006B0800F7DEB500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00F7D6AD00EFCE9C00FFFFDE00FFFFDE00FFFF
      F700FFFFFF00FFFFF700FFFFEF00AD847B00008C080000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000000000000000000000DEBDB500FFFF
      FF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEC600F7D6B500B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00EFC69400F7C68C00FFFFD600FFFFDE00FFFF
      EF00FFFFF700FFFFEF00FFFFE700AD847B000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000DEC6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFEFE700FFEFDE00FFEFDE00FFEF
      D600E7DEC600C6BDAD00B5848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00EFCE9C00F7BD8C00F7DEB500FFF7CE00FFFF
      DE00FFFFDE00FFFFDE00FFFFDE00AD847B000000000000000000E7C6B500FFFF
      FF008484840084848400848484008484840084848400FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000E7C6B500FFFF
      FF0073A5FF00004AF700004AF700004AF700004AF700FFF7EF00F7E7D600C6A5
      9400B5948C00B58C8400B5848400000000000000000000000000000000000000
      0000000000004242420000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AD847B00FFE7BD00EFC69400EFCE9C00F7E7B500FFFF
      DE00FFFFDE00FFFFDE00FFFFDE00AD847B000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000E7C6B500FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700E7CECE00BD8C
      7300EFB57300EFA54A00C6846B00000000000000000000000000000000000000
      0000000000000000000042424200000000004242420000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000E7CEB500FFFFFF00FFE7BD00F7C68C00EFC6
      9400EFCE9C00FFF7C600DEC6AD00000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000EFCEBD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E7D6CE00C694
      7B00FFC67300CE94730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000AD847B00DEC6B500FFFFD600F7DEAD00EFCE
      9C00F7DEAD00DEC6AD00AD847B00000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000E7C6B500FFF7
      F700FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00E7CECE00C694
      7B00CE9C84000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000AD847B00AD847B00AD84
      7B00AD847B000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500DEBDB500BD84
      7B00000000000000000000000000000000000000000000000000E7C6B500EFCE
      B500EFCEB500EFCEB500EFCEB500E7C6B500E7C6B500EFCEB500D6BDB500BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A57B7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B
      7300A57B7300A57B73008C5A5A0000000000000000000000000000639C000063
      9C0000639C00A57B7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B
      7300A57B7300A57B73008C6363000000000000000000C6847300B5848400B584
      8400B5848400B5848400B5848400B5848400B5848400B5848400B5848400B584
      8400B5848400B5848400B5848400000000000000000000000000000000000000
      000000000000AD3900008C290000000000000000000000000000A54200008C29
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AD7B7300FFE7CE00FFE7C600FFDEC600FFDEBD00FFDEBD00FFD6
      B500FFD6B500F7CEA5008C5A5A00000000000000000000639C004AC6E7004AC6
      E7004AC6E700B5847300F7EFDE00FFF7E700FFEFE700F7EFDE00F7EFDE00F7EF
      DE00F7EFDE00F7EFDE008C63630000000000C6A59C00FFFFFF00FFFFFF00FFFF
      FF00FFFFEF00FFFFEF00FFFFEF00FFF7DE00FFF7DE00FFEFD600FFEFD600FFEF
      D600FFDEB50000189C00FFD6A500B58484000000000000000000000000000000
      0000C65A0000A5420000A54200008C29000000000000A5420000AD390000AD39
      00008C2900000000000000000000000000000000000000000000000000000000
      000000000000AD7B7300FFE7D600E7A55200E7A55200E7A55200E7A55200E7A5
      5200E7A55200FFD6B5008C5A5A00000000000000000000639C004AC6E7004AC6
      E7004AC6E700B5847300F7E7DE00E7A55A00E7A55A00E7A55A00E7A55A00E7A5
      5A00E7A55A00F7E7D6008C63630000000000C6A59C00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFEF00FFFFEF00FFFFEF00FFF7DE00FFF7DE00FFEF
      D600FFEFD60000189C00FFDEBD00B58484000000000000000000000000000000
      0000AD39000000000000000000008C29000000000000AD390000000000000000
      00008C2900000000000000000000000000000000000000000000000000000000
      000000000000B5847B00FFEFE700FFEFDE00FFE7CE00FFE7CE00FFE7C600FFDE
      BD00FFDEBD00FFD6B5008C5A5A00000000000000000000639C004AC6EF004AC6
      E7004AC6E700BD847300F7EFDE00F7E7D600EFDECE00EFDECE00EFDECE00EFDE
      C600EFDECE00F7E7D6008C63630000000000C6ADA500FFFFFF00FFFFFF00FFFF
      FF00DEA57B00C6847300FFFFFF00DEA57B00C6847300FFFFEF00DEA57B002118
      CE002118CE002118CE0000189C0000189C000000000000000000000000000000
      0000AD3900008C290000000000008C29000000000000AD39000000000000C65A
      00008C290000000000000000000000000000A57B7300A57B7300A57B7300A57B
      7300A57B7300BD8C7B00FFF7E700E7A55200E7A55200E7A55200E7A55200E7A5
      5200E7A55200FFDEBD008C5A5A00000000000000000000639C0052CEEF0052CE
      EF004AC6E700BD847300FFEFE700E7A55A00E7A55A00E7A55A00E7A55A00E7A5
      5A00E7A55A00F7E7D6008C63630000000000C6ADA500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFFF
      EF00FFF7DE002118CE00FFEFD600B58484000000000000000000000000000000
      0000C65A0000AD390000AD390000AD390000A5948400AD390000AD390000AD39
      00008C290000000000000000000000000000AD7B7300FFE7CE00FFE7C600FFDE
      C600FFDEBD00BD948400FFF7EF00FFF7E700FFEFE700FFEFDE00FFE7D600FFE7
      CE00FFE7C600FFDEC6008C5A5A00000000000000000000639C005AD6EF0052CE
      EF0052CEEF00C68C7300FFF7EF00F7EFDE00F7E7D600F7E7D600F7E7D600F7E7
      CE00F7E7D600F7EFDE008C63630000000000D6B5AD00FFFFFF00C6847300FFFF
      FF00FFFFFF00EFE7E700943931008C2121008C212100A5635A008C212100E7CE
      C600FFFFEF002118CE00FFEFD600B58484000000000000000000000000000000
      000000000000C65A0000A54200009C4A18008C634A00AD390000A54200008C29
      000000000000000000000000000000000000AD7B7300FFE7D600E7A55200E7A5
      5200E7A55200BD948400FFFFF700E7A55200E7A55200E7A55200E7A55200E7A5
      5200E7A55200FFE7CE008C5A5A00000000000000000000639C0063D6F70063D6
      F7005AD6EF00C6946B00FFF7EF00E7A55A00E7A55A00E7A55A00E7A55A00E7A5
      5A00E7A55A00FFEFE7008C63630000000000D6B5AD00FFFFFF00DEA57B00FFFF
      FF00FFFFFF00BD7B7B009C424200FFFFFF00EFE7E7009C5252008C212100FFFF
      FF00FFFFEF00DEA57B00FFEFD600B58484000000000000000000000000000000
      00000000000000000000000000008C736B00E7DED6008C736B00000000000000
      000000000000000000000000000000000000B5847B00FFEFE700FFEFDE00FFE7
      CE00FFE7CE00DEAD8400FFFFFF00FFFFF700FFF7F700FFF7EF00FFF7E700FFEF
      E700FFEFDE00FFE7D6008C5A5A00000000000000000000639C006BDEF7006BDE
      F70063D6F700C6946B00FFFFF700FFF7F700FFF7E700FFEFE700FFEFE700FFEF
      E700FFF7E700F7EFDE008C63630000000000D6BDB500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00C68C8C008C212100CE949400FFFFFF00FFFFFF008C212100FFFF
      FF00FFFFFF00FFFFEF00FFF7DE00B58484000000000000000000000000000000
      000000000000000000008C736B00E7DED6009C847B00D6CEBD008C736B000000
      000000000000000000000000000000000000BD8C7B00FFF7E700E7A55200E7A5
      5200E7A55200DEAD8400FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00FFEF
      DE00EFC6A500BD9484008C5A5A00000000000000000000639C0073DEFF0073DE
      F7006BDEF700D6A58C00FFFFF700FFFFFF00FFFFFF00FFFFFF00FFFFFF00B584
      7300B5847300B5847300B584730000000000D6BDB500FFFFFF00C6847300FFFF
      FF00FFFFFF00FFFFFF00B56B6B00943931008C2121008C2121008C212100FFFF
      FF00FFFFFF00C6847300FFFFEF00B58484000000000000000000000000000000
      000000000000000000009C847B00E7DED6009C847B00D6CEBD009C847B000000
      000000000000000000000000000000000000BD948400FFF7EF00FFF7E700FFEF
      E700FFEFDE00E7B58C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700B584
      7300B5847300B5847300B57B7300000000000000000000639C007BE7FF007BE7
      FF0073DEF700D6A58C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B584
      7300DEA57300E7A55A000000000000000000D6BDB500FFFFFF00DEA57B00FFFF
      FF00FFFFFF00EFE7E700CE949400FFFFFF00FFFFFF00E7D6D6008C212100FFFF
      FF00FFFFFF00DEA57B00FFFFEF00B58484000000000000000000000000000000
      0000000000008C736B00E7DED6008C736B00000000007B7B7300D6CEBD008C73
      6B0000000000000000000000000000000000BD948400FFFFF700E7A55200E7A5
      5200E7A55200E7B58C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00B584
      7300E7AD7300E7A5520000000000000000000000000000639C007BE7FF007BE7
      FF007BE7FF00D6A58C00DEA58400DEA58400DEA58400DEA58400DEA58400B584
      7300C6AD9C0000639C000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00EFE7E7008C212100BD7B7B00CE949400B56B6B008C212100FFFF
      FF00FFFFFF00FFFFFF00FFFFEF00B58484000000000000000000000000000000
      000000000000D6CEBD008C736B000000000000000000000000008C736B00D6CE
      BD0000000000000000000000000000000000DEAD8400FFFFFF00FFFFF700FFF7
      F700FFF7EF00E7B58C00DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400B584
      7300EFAD5A000000000000000000000000000000000000639C008CE7FF007BE7
      FF007BE7FF007BE7FF007BE7FF0073DEFF006BDEF70063D6F7005AD6EF0052CE
      EF0052CEEF0000639C000000000000000000E7C6B500FFFFFF00C6847300FFFF
      FF00FFFFFF00FFFFFF00DEBDB5009C525200943931009C424200DEBDB500FFFF
      FF00FFFFFF00C6847300FFFFEF00B58484000000000000000000000000000000
      00008C736B00E7DED6009C847B000000000000000000000000009C847B00D6CE
      BD008C736B00000000000000000000000000DEAD8400FFFFFF00FFFFFF00FFFF
      F700FFFFF700FFF7EF00FFEFDE00EFC6A500BD9484008C5A5A00000000000000
      0000000000000000000000000000000000000000000000639C008CE7FF008CE7
      FF007373730073737300737373007373730073737300737373007373730063D6
      F7005AD6EF0000639C000000000000000000E7C6B500FFFFFF00DEA57B00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00DEA57B00FFFFFF00B58484000000000000000000000000000000
      0000D6CEBD008C736B0000000000000000000000000000000000000000007B7B
      7300D6CEBD00000000000000000000000000E7B58C00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFF700B5847300B5847300B5847300B57B7300000000000000
      0000000000000000000000000000000000000000000000639C008CE7FF008CE7
      FF0073737300D6C6BD00D6C6BD00D6C6BD00D6C6BD00CEBDB500737373006BDE
      F70063D6F70000639C000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00DEA57B00C6847300FFFFFF00DEA57B00C6847300FFFFFF00DEA57B00C684
      7300FFFFFF00FFFFFF00FFFFFF00B58484000000000000000000000000000000
      00008C736B000000000000000000000000000000000000000000000000000000
      00007B7B7300000000000000000000000000E7B58C00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B5847300E7AD7300E7A5520000000000000000000000
      000000000000000000000000000000000000000000000000000000639C000063
      9C0073737300EFDEC600FFFFFF00FFFFF700FFFFF700D6C6BD00737373000063
      9C0000639C00000000000000000000000000E7C6B500FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00C68473000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E7B58C00DEAD8400DEAD8400DEAD
      8400DEAD8400DEAD8400B5847300EFAD5A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007373730073737300737373007373730073737300000000000000
      00000000000000000000000000000000000000000000E7C6B500E7C6B500E7C6
      B500E7C6B500E7C6B500E7C6B500D6BDB500D6BDB500D6B5AD00D6B5AD00C6AD
      A500C6ADA500C6A59C00C6A59C00000000000000000000000000000000000000
      0000848484008484840000000000000000008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000031DE000031DE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400C6BDBD00BDB5B500848484008484840084848400EFEFEF00848484008484
      840084848400000000000000000000000000000000000031DE000031DE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000031DE000031DE00000000000000000000000000844200008442
      0000733900006B310000633100006B3100006B3100006B3100006B3100006B31
      00006B3100005A29000000000000000000000000000000000000844200008442
      0000733900006B3100006B3100006B3100006B3100006B3100006B3100006B31
      00006B3100005A2900000000000000000000000000000000000084848400E7E7
      E700BDB5B500BDB5B500BDB5B500525252004A4A4A0084848400CECECE00EFE7
      E700CEC6C600848484008484840084848400000000000031DE000031DE000031
      DE00000000000000000000000000000000000000000000000000000000000000
      00000031DE000031DE00000000000000000000000000AD520000CE630000CE63
      0000B55A0000B55A0000AD520000AD520000AD520000AD520000AD520000AD52
      0000B55A0000944A00005A2900000000000000000000AD520000D6630000BD5A
      0000BD5A0000B55A0000B5520000AD520000AD520000AD520000AD520000AD52
      0000B55A0000944A00005A290000000000000000000084848400E7DEDE00DED6
      D600ADA5A500ADA5A500ADA5A5005A5A5A001010100010101000181818001010
      1000948C8400CECECE008484840000000000000000000031DE000031DE000031
      DE000031DE000000000000000000000000000000000000000000000000000031
      DE000031DE0000000000000000000000000000000000CE630000E7730000DE6B
      0000D66B0000D6630000CE630000B55A0000B55A0000B55A0000B55A0000B55A
      0000CE630000B55A00006B3100000000000000000000D6630000E7730000DE6B
      0000D66B0000D6630000D6630000BD5A0000BD5A0000BD5A0000BD5A0000BD5A
      0000BD5A0000B55A00006B3100000000000084848400DED6D600DED6D600ADA5
      A500E7DEDE00DEDEDE00CECECE00C6BDBD00BDB5B500ADA5A500737373001010
      1000101010001010100084848400948C840000000000000000000031EF000031
      DE000031DE000031DE00000000000000000000000000000000000031DE000031
      DE000000000000000000000000000000000000000000D6630000EF730000E773
      0000DE6B0000D66B0000E7944200E79C4A00E79C4A00E7944A00D6631800B55A
      0000B55A0000AD5200006B3100000000000000000000D66B0000EF730000E773
      0000DE6B0000EF842100EF944200F7A55A00EF944200EF944200BD5A0000BD5A
      0000BD5A0000AD5200006B3100000000000084848400DED6D600ADA5A500F7F7
      EF00F7F7EF00F7F7EF00EFE7E700EFE7E700E7E7E700DEDEDE00CECECE00C6BD
      BD00ADA5A50073737300948C84008C8484000000000000000000000000000000
      00000031DE000031DE000031DE00000000000031DE000031DE000031DE000000
      00000000000000000000000000000000000000000000D66B0000EF7B1000EF7B
      0800E7730000DE6B0000FFCE9C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E79C
      4A00B55A0000AD5200006B3100000000000000000000D66B0000EF7B1000EF7B
      0800F7A55A00FFFFF700FFFFF700FFFFF700FFFFF700FFCE9C00BD5A0000BD5A
      0000BD5A0000AD5200006B3100000000000084848400ADA5A500F7F7EF00F7F7
      EF00F7F7EF00E7DEDE00ADADAD00B5ADAD00C6BDBD00D6CECE00E7DEDE00E7E7
      E700D6D6D600CEC6C600ADA5A500848484000000000000000000000000000000
      0000000000000031DE000031E7000031E7000031EF000031DE00000000000000
      00000000000000000000000000000000000000000000D66B0000EF8C3100EF84
      1800EF7B0800E7730000E7730000EF7B1000D6631800D6631800F7BD8C00FFFF
      FF00B55A0000AD5200006B3100000000000000000000D66B0000EF8C3100EF84
      1800FFFFF700F7C68C00EF841800EF841800DE631000D66B0000BD5A0000BD5A
      0000BD5A0000AD5200006B31000000000000000000008484840084848400F7EF
      EF00C6BDBD00948C8400ADA5A500ADADAD00ADA5A500B5ADAD00ADADAD00B5AD
      AD00CECECE00DEDEDE00D6CECE00848484000000000000000000000000000000
      000000000000000000000031E7000031E7000031EF0000000000000000000000
      00000000000000000000000000000000000000000000D66B0000EFA55A00EF8C
      2900EF7B1000EF7B0800FFCE9C00E7730000DE6B0000D66B0000D6630800FFFF
      FF00CE630000B55200006B3100000000000000000000D66B0000F7A55A00EF8C
      2900FFFFF700EF841800EF730000E7730000DE730000FFCE9C00D6630000D663
      0000BD5A0000B55200006B310000000000000000000000000000000000008484
      8400CE9C9400BD9C8C009C8C84009484840084848400948C8400ADA5A500BDB5
      B500DED6D600D6CECE0084848400000000000000000000000000000000000000
      0000000000000031DE000031EF000031EF000031EF000031F700000000000000
      00000000000000000000000000000000000000000000D66B0000F7B57300EF94
      3900EF841800FFCE9C00FFFFFF00EF730000E7730000DE6B0000EF9C5200FFFF
      FF00CE630000B55A0000733900000000000000000000D66B0000F7B57300EF94
      3100FFFFF700F7AD6300EF7B0000EF730000E7730000FFFFF700FFC69400D66B
      0000D6630000BD5A000073390000000000000000000000000000000000000000
      0000CE9C9C00FFE7CE00FFDEB500F7D6B500C69C9400C69C9400CE9C9C009C8C
      8400848484008484840000000000000000000000000000000000000000000000
      00000031F7000031EF000031EF0000000000000000000031F7000031FF000000
      00000000000000000000000000000000000000000000D66B0000F7BD7B00E794
      4200FFDEBD00FFFFFF00FFFFFF00FFEFD600FFE7C600FFE7C600FFFFFF00F7BD
      8C00DE6B0000CE630000844200000000000000000000D66B0000F7BD7B00EF94
      4200FFCE9C00FFFFF700FFE7CE00FFE7CE00FFEFDE00FFFFF700FFFFF700FFD6
      B500D66B0000BD5A000084420000000000000000000000000000000000000000
      0000CE9C9C00FFE7CE00FFDEBD00FFDEB500FFDEB500F7DEB500CE9C9C000000
      0000000000000000000000000000000000000000000000000000000000000031
      FF000031EF000031FF00000000000000000000000000000000000031FF000031
      FF000000000000000000000000000000000000000000D66B0000F7BD8400EF9C
      5200FFDEB500FFFFFF00FFFFFF00FFE7C600FFCE9C00FFCE9C00EFA55A00E773
      0000E7730000CE630000944200000000000000000000D66B0000F7BD8400EF9C
      4A00EF943100F7B57300FFCE9C00FFCE9C00FFDEC600FFFFF700FFFFF700FFD6
      AD00E7730000D6630000944A000000000000000000000000000000000000CE9C
      9C00FFEFDE00FFE7CE00FFE7C600FFDEBD00FFDEB500CE9C9C00000000000000
      00000000000000000000000000000000000000000000000000000031FF000031
      FF000031FF000000000000000000000000000000000000000000000000000031
      FF000031FF0000000000000000000000000000000000D66B0000FFC69400EFA5
      6300EF943900FFCEA500FFFFFF00EF841800EF841800EF7B0800EF7B0000EF73
      0000EF730000D66B00009C4A00000000000000000000D66B0000FFC69400F7AD
      6300EF943900EF943100EF8C2100EF841800EF841800FFFFF700F7C68C00EF73
      0000EF730000D66B00009C4A000000000000000000000000000000000000CE9C
      9C00FFEFE700FFEFDE00FFE7CE00FFE7C600FFDEB500CE9C9C00000000000000
      000000000000000000000000000000000000000000000031FF000031FF000031
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000031FF00000000000000000000000000D66B0000FFCE9C00F7C6
      9400F7BD7B00F7AD6B00FFD6AD00E7944A00EF8C3100EF841800EF7B0800EF7B
      0000EF7B0000E7730000AD5200000000000000000000D66B0000FFC69C00FFC6
      8C00F7B57B00F7B56B00F7A55A00EF9C4A00EF943100F7C68C00EF7B0800EF7B
      0000EF7B0000DE730000AD520000000000000000000000000000CE9C9C00F7F7
      EF00F7F7EF00FFEFE700FFEFD600FFE7CE00F7D6B500CE9C9C00000000000000
      0000000000000000000000000000000000000031FF000031FF000031FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D66B0000F7BD7B00FFCE
      9C00FFCEA500FFC69400F7BD8400F7B57300E7944A00EF8C2900EF7B0800EF7B
      0800EF7B0800E7730000B55A00000000000000000000D66B0000F7B57B00FFCE
      9C00FFCE9C00FFC69400F7BD8400F7B56B00EF9C4A00EF8C2100EF7B0800EF7B
      0000EF7B0000EF730000B55A0000000000000000000000000000CE9C9C00CE9C
      9C00CE9C9C00FFF7EF00FFEFDE00FFE7CE00CE9C9C0000000000000000000000
      0000000000000000000000000000000000000031FF000031FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EF7B1000EF8C
      3100EF8C3100EF8C3100EF8C2900EF842100EF7B1000D6630800DE6B0000D66B
      0000D66B0000CE63000000000000000000000000000000000000DE631000EF8C
      3100EF8C3100EF8C3100EF8C2900EF842100DE631000DE630800D66B0000D66B
      0000D66B0000D663000000000000000000000000000000000000000000000000
      000000000000CE9C9C00CE9C9C00CE9C9C000000000000000000000000000000
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
      00000000000000000000000000000000000000000000008CBD00008CBD00008C
      BD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008CBD00008C
      BD00008CBD00008CBD0000000000000000000000000000000000CEA59C00FFEF
      DE00F7E7CE00F7DEBD00F7DEB500F7D6AD00F7D6A500EFCEA500EFCEA500EFCE
      A500EFCEA500F7D6A500B5848400000000000000000000000000944239009442
      3900C69C9C00C69C9C00C69C9C00C69C9C00C69C9C00C69C9C00C69C9C009431
      29009442390000000000000000000000000000000000AD5A5A00AD5A5A00E7C6
      C600E7C6C600C6CEC600C6CEC600C6CEC600C6CEC600AD5A5A00943131000000
      000000000000000000000000000000000000008CBD00189CAD005AC6EF0084E7
      FF0063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CEF70063CE
      F70042BDD600008CBD0000000000000000000000000000000000CEA59C00FFEF
      DE00F7E7CE00F7DEC600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCEA500EFCE
      A500EFCEA500F7D6A500B5848400000000000000000094423900CE636300C663
      6300E7E7E7009429290094292900E7E7E700E7E7E700E7E7E700CECECE009429
      29009C424200944239000000000000000000BD7B7300C65A5A00C65A5A00E7C6
      C6009C393900B5737300C6CEC600F7F7F700F7F7F700C65A5A00943131000000
      000000000000000000000000000000000000008CBD004ABDDE0042BDD6009CF7
      FF0073D6FF0073D6FF006BD6F70073D6FF0073D6FF0073D6FF0073D6FF006BD6
      F7004ABDDE0084DEFF00008CBD00000000000000000000000000CEAD9C00FFF7
      EF00F7E7D600F7E7CE00F7DEC600F7DEBD00F7DEB500F7D6AD00F7D6A500EFCE
      A500EFCEA500F7D6A500B5848400000000000000000094423900CE636300C663
      6300E7E7E7009429290094292900E7E7E700E7E7E700E7E7E700CECECE009429
      29009C424200944239000000000000000000BD7B7300C65A5A00C65A5A00C694
      8C009C3939009C4A4A00E7C6C600C6CEC600F7F7F700C65A5A00943131000000
      000094313100000000000000000000000000008CBD0073D6FF00008CBD00ADF7
      FF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDEFF007BDE
      FF004ABDDE00ADF7FF00008CBD00000000000000000000000000CEADA500FFF7
      EF00FFEFDE00F7E7D600F7E7CE00F7DEC600F7DEBD00F7DEB500F7D6AD00F7D6
      A500EFCEA500F7D6A500B5848400000000000000000094423900CE636300C663
      6300E7E7E7009429290094292900E7E7E700E7E7E700E7E7E700CECECE009429
      29009C424200944239000000000000000000BD7B7300C65A5A00C65A5A00C694
      8C00C6948C00BDA5A500BDA5A500E7C6C600C6CEC600C65A5A0094313100C65A
      5A0094313100000000000000000000000000008CBD007BDEFF001094B5009CF7
      FF0094EFFF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7FF0084E7
      FF004ABDDE00B5F7FF00008CBD00000000000000000000000000CEADAD00FFFF
      F700FFF7EF00FFEFDE00F7E7D600F7E7CE00F7DEC600F7DEBD00F7DEB500F7D6
      AD00F7D6A500F7D6A500B5848400000000000000000094423900CE636300C663
      6300E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700E7E7E700CECECE009429
      29009C424200944239000000000000000000BD7B7300AD524A00B55A5A00C65A
      5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A5A00C65A5A0094313100C65A
      5A0094313100000000009431310000000000008CBD0084E7FF004ABDDE005AC6
      E700ADF7FF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF000884
      18004ABDDE00B5F7FF0063CEF700008CBD000000000000000000D6ADAD00FFFF
      FF00FFF7EF00FFF7EF00FFEFDE00F7E7D600F7E7CE00F7DEC600F7DEBD00F7DE
      B500F7D6AD00F7D6A500B5848400000000000000000094423900CE636300C663
      6300CE636300C6737300C6737300CE6B6B00C6636300CE6B6B00CE636300C663
      6300CE636300944239000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000008CBD008CE7FF0073DEFF00189C
      AD00DEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF00CEF7FF000884180031BD
      730008841800DEF7FF00D6F7FF00008CBD000000000000000000E7BDAD00FFFF
      FF00FFFFF700FFF7EF00FFEFDE00FFEFDE00F7E7D600F7E7CE00F7DEC600F7DE
      BD00F7DEB500F7D6AD00B5848400000000000000000094423900BD636300C67B
      7B00CE9C9C00CEADAD00CEADAD00CEADAD00CEA5A500CEA5A500CEADAD00CEAD
      AD00CE636300944239000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000008CBD0094EFFF0094EFFF001094
      B500008CBD00008CBD00008CBD00008CBD00008CBD000884180042CE84004ACE
      9C0039C6730008841800008CBD00008CBD000000000000000000E7BDAD00FFFF
      FF00FFFFFF00FFFFF700FFF7EF00FFEFDE00FFEFDE00F7E7D600F7E7CE00F7DE
      C600F7DEBD00F7DEB500B5848400000000000000000094423900CE636300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CE636300944239000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000008CBD009CF7FF009CF7FF009CF7
      FF009CF7FF009CF7FF009CF7FF009CF7FF000884180042CE84004ACE8C004ACE
      8C004ACE9C0039C6730008841800000000000000000000000000E7BDAD00FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFF7EF00FFEFDE00FFEFDE00F7E7D600F7E7
      CE00F7E7CE00F7DEBD00B5848400000000000000000094423900CE636300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CE636300944239000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A009431310000000000008CBD00DEF7FF00A5F7FF00A5F7
      FF00A5F7FF00A5F7FF00A5F7FF00088418000884180008841800088418004ACE
      8C0042CE84000884180008841800088418000000000000000000E7C6AD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7EF00FFEFDE00FFEFDE00FFEF
      DE00F7DEC600CEADA500B5848400000000000000000094423900CE636300FFFF
      FF00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00FFFF
      FF00CE636300944239000000000000000000BD7B7300AD524A00FFF7F700FFF7
      F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A5A0094313100C65A
      5A0094313100C65A5A00943131000000000000000000008CBD00DEF7FF00A5F7
      FF00A5F7FF00A5F7FF00008CBD004ABDDE004ABDDE004ABDDE000884180042CE
      840031BD73000884180000000000000000000000000000000000E7C6AD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7EF00F7E7CE00C6A5
      9400C6948C00BD8C8400B5848400000000000000000094423900CE636300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CE636300944239000000000000000000BD7B7300AD524A00D6D6D600CEB5
      B500CEB5B500CEB5B500CEB5B500CEB5B500D6D6D600AD524A0094313100C65A
      5A0094313100C65A5A0094313100000000000000000000000000008CBD00008C
      BD00008CBD00008CBD00000000000000000000000000000000000884180039C6
      7B00088418000000000000000000000000000000000000000000EFCEAD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7DEC600C68C
      7300DE9C5200DE9C5200C68C7300000000000000000094423900CE636300FFFF
      FF00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00CECECE00FFFF
      FF00CE6363009442390000000000000000000000000000000000BD7B7300AD52
      4A00FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700C65A
      5A0094313100C65A5A0094313100000000000000000000000000000000000000
      000000000000000000000000000000000000000000000884180031BD730031BD
      7300088418000000000000000000000000000000000000000000EFCEAD00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7E7CE00C69C
      8C00EFCEA500CE946B0000000000000000000000000094423900CE636300FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CE6363009442390000000000000000000000000000000000BD7B7300AD52
      4A00D6D6D600CEB5B500CEB5B500CEB5B500CEB5B500CEB5B500D6D6D600AD52
      4A0094313100C65A5A0094313100000000000000000000000000000000000000
      000000000000000000000000000000000000000000000884180031BD73000884
      1800000000000000000000000000000000000000000000000000EFCEAD00FFFF
      F700FFFFF700FFFFF700FFFFF700FFF7EF00FFF7EF00FFF7EF00F7E7CE00C694
      8C00C69C8C00000000000000000000000000000000000000000094423900FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00944239000000000000000000000000000000000000000000000000000000
      0000BD7B7300AD524A00FFF7F700FFF7F700FFF7F700FFF7F700FFF7F700FFF7
      F700FFF7F700C65A5A0094313100000000000000000000000000000000000000
      0000000000000000000000000000088418000884180008841800088418000000
      0000000000000000000000000000000000000000000000000000EFCEAD00EFCE
      AD00EFCEAD00EFCEAD00EFCEAD00EFCEAD00EFCEAD00EFCEAD00E7BDAD00BD84
      7B00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BD7B7300AD524A00D6D6D600CEB5B500CEB5B500CEB5B500CEB5B500CEB5
      B500D6D6D600AD524A0094313100000000000000000000000000000000000000
      0000000000000884180008841800088418000884180000000000000000000000
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
      Caption = 'Copy message'
      OnClick = mnuCopyMessageClick
    end
    object mnuCopyAll: TSpTBXItem
      Caption = 'Copy all'
      OnClick = mnuCopyAllClick
    end
    object sepOutput: TSpTBXSeparatorItem
    end
    object mnuSaveToFile: TSpTBXItem
      Caption = 'Save to file...'
      OnClick = mnuSaveToFileClick
    end
  end
  object sciCallTips: TSciCallTips
    EndDefinition = ')'
    Editor = sciEditor
    ParametersEnd = ')'
    ParametersSeparators = ','
    ParametersStart = '('
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    TabSize = 0
    UseStyle = False
    OnBeforeShow = sciCallTipsBeforeShow
    Left = 842
    Top = 36
  end
  object sciPropertyLoader: TSciPropertyLoader
    Editor = sciEditor
    StoreWhat = [stDefaults, stColors, stStyles, stOther, stLexerProperties, stExtensions]
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
      Caption = 'Copy'
      ImageIndex = 9
      Images = ilImages
      OnClick = mnuEditorCopyClick
    end
    object mnuEditorCut: TSpTBXItem
      Caption = 'Cut'
      ImageIndex = 8
      Images = ilImages
      OnClick = mnuEditorCutClick
    end
    object mnuEditorPaste: TSpTBXItem
      Caption = 'Paste'
      ImageIndex = 10
      Images = ilImages
      OnClick = mnuEditorPasteClick
    end
    object mnuEditorDelete: TSpTBXItem
      Caption = 'Delete'
      ImageIndex = 5
      Images = ilImages
      OnClick = mnuEditorDeleteClick
    end
    object sepEditorMenu1: TSpTBXSeparatorItem
    end
    object mnuEditorSelectAll: TSpTBXItem
      Caption = 'Select all'
      ImageIndex = 11
      Images = ilImages
      OnClick = mnuEditorSelectAllClick
    end
    object sepEditorMenu2: TSpTBXSeparatorItem
    end
    object mnuEditorUndo: TSpTBXItem
      Caption = 'Undo'
      ImageIndex = 6
      Images = ilImages
      OnClick = mnuEditorUndoClick
    end
    object mnuEditorRedo: TSpTBXItem
      Caption = 'Redo'
      ImageIndex = 7
      Images = ilImages
      OnClick = mnuEditorRedoClick
    end
    object sepEditorMenu3: TSpTBXSeparatorItem
    end
    object mnuToogleBookmark: TSpTBXItem
      Caption = 'Toogle Bookmark'
      ShortCut = 49218
      OnClick = mnuToogleBookmarkClick
    end
    object mnuGoToBookmark: TSpTBXItem
      Caption = 'Go to next Bookmark'
      ShortCut = 16450
      OnClick = mnuGoToBookmarkClick
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
    ReadTimeout = 0
    ProxySettings.ProxyType = fpcmNone
    ProxySettings.Port = 0
    Left = 722
    Top = 6
  end
  object sciAutoComplete: TSciAutoComplete
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
    CancelAtStart = True
    FromDocument = False
    Editor = sciEditor
    WordCharacters = '_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    OnBeforeShow = sciAutoCompleteBeforeShow
    MaxWidth = 0
    MaxHeight = 5
    Left = 842
    Top = 8
  end
  object ppmDocuments: TSpTBXPopupMenu
    Images = ilImages
    Left = 662
    Top = 6
    object mnuPClose: TSpTBXItem
      Caption = 'Close file'
      ImageIndex = 46
      Images = ilImages
      OnClick = mnuPCloseClick
    end
    object mnuPCloseAllFiles: TSpTBXItem
      Caption = 'Close all files'
      OnClick = mnuPCloseAllFilesClick
    end
    object sepDocuments: TSpTBXSeparatorItem
    end
    object mnuPSave: TSpTBXItem
      Caption = 'Save file'
      ImageIndex = 1
      Images = ilImages
    end
  end
  object JvInspectorDotNETPainter: TJvInspectorDotNETPainter
    CategoryFont.Charset = DEFAULT_CHARSET
    CategoryFont.Color = clBtnText
    CategoryFont.Height = -11
    CategoryFont.Name = 'MS Sans Serif'
    CategoryFont.Style = []
    NameFont.Charset = DEFAULT_CHARSET
    NameFont.Color = clWindowText
    NameFont.Height = -11
    NameFont.Name = 'MS Sans Serif'
    NameFont.Style = []
    ValueFont.Charset = DEFAULT_CHARSET
    ValueFont.Color = clWindowText
    ValueFont.Height = -11
    ValueFont.Name = 'MS Sans Serif'
    ValueFont.Style = []
    DrawNameEndEllipsis = True
    HideSelectFont.Charset = DEFAULT_CHARSET
    HideSelectFont.Color = clHighlightText
    HideSelectFont.Height = -11
    HideSelectFont.Name = 'MS Sans Serif'
    HideSelectFont.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clHighlightText
    SelectedFont.Height = -11
    SelectedFont.Name = 'MS Sans Serif'
    SelectedFont.Style = []
    Left = 662
    Top = 36
  end
  object mtpDocuments: TJvModernTabBarPainter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    DisabledFont.Charset = DEFAULT_CHARSET
    DisabledFont.Color = clGrayText
    DisabledFont.Height = -11
    DisabledFont.Name = 'MS Sans Serif'
    DisabledFont.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clWindowText
    SelectedFont.Height = -11
    SelectedFont.Name = 'MS Sans Serif'
    SelectedFont.Style = []
    Left = 632
    Top = 6
  end
end
