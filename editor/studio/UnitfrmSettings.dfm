object frmSettings: TfrmSettings
  Left = 379
  Top = 206
  BorderStyle = bsDialog
  Caption = 'AMXX-Studio - Settings'
  ClientHeight = 297
  ClientWidth = 504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object trvSettings: TJvSettingsTreeView
    Left = 0
    Top = 0
    Width = 153
    Height = 260
    AutoExpand = False
    ShowButtons = True
    ShowLines = True
    PageDefault = 0
    PageList = jplSettings
    Align = alLeft
    HotTrack = True
    Indent = 19
    TabOrder = 0
    OnChanging = trvSettingsChanging
    Items.Data = {
      060000001F0000000100000001000000FFFFFFFFFFFFFFFF0000000003000000
      06456469746F7224000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000000000
      0000000B486967686C6967687465721E000000FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFF010000000000000005546F6F6C7322000000FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFF02000000000000000953686F727463757473210000000100000001
      000000FFFFFFFFFFFFFFFF040000000200000008436F6D70696C65722A000000
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF040000000000000011436F6D70696C65
      722053657474696E67732C000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0500
      0000000000001348616C662D4C696665204469726563746F7279230000000100
      000001000000FFFFFFFFFFFFFFFF0A000000030000000A436F64652D546F6F6C
      7321000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0A00000000000000085365
      7474696E677326000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF030000000000
      00000D436F64652D536E6970706574732B000000FFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFF0B00000000000000124175746F636F6D706C65746520436865636B1C
      0000000100000001000000FFFFFFFFFFFFFFFF06000000020000000346545021
      000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF06000000000000000853657474
      696E67731E000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0700000000000000
      0550726F787921000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF080000000000
      000008506C75672D496E731D000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF09
      00000000000000044D697363}
    Items.Links = {
      1000000000000000000000000100000002000000040000000400000005000000
      0A0000000A000000030000000B00000006000000060000000700000008000000
      09000000}
  end
  object jplSettings: TJvPageList
    Left = 153
    Top = 0
    Width = 351
    Height = 260
    ActivePage = jspCodeSnippets
    PropagateEnable = False
    Align = alClient
    OnChange = jplSettingsChange
    OnChanging = jplSettingsChanging
    object jspHighlighter: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Highlighter'
      object lblStyles: TLabel
        Left = 6
        Top = 48
        Width = 33
        Height = 13
        Caption = 'Styles:'
      end
      object lblLanguage: TLabel
        Left = 6
        Top = 6
        Width = 51
        Height = 13
        Caption = 'Language:'
      end
      object lblProperties: TLabel
        Left = 134
        Top = 48
        Width = 53
        Height = 13
        Caption = 'Properties:'
      end
      object shpStyles: TShape
        Left = 6
        Top = 64
        Width = 121
        Height = 158
        Pen.Color = 8623776
      end
      object cboLanguage: TFlatComboBox
        Left = 6
        Top = 22
        Width = 255
        Height = 21
        Style = csDropDownList
        Color = clWindow
        ItemHeight = 13
        TabOrder = 0
        ItemIndex = -1
        OnChange = cboLanguageChange
      end
      object pnlHighlighter: TPanel
        Left = 134
        Top = 64
        Width = 213
        Height = 157
        BevelOuter = bvLowered
        TabOrder = 1
        object lblFont: TLabel
          Left = 4
          Top = 4
          Width = 26
          Height = 13
          Caption = 'Font:'
        end
        object lblFontSize: TLabel
          Left = 4
          Top = 90
          Width = 119
          Height = 13
          Caption = 'Font Size (0 for default):'
        end
        object cboFont: TmbXPFontCombo
          Left = 4
          Top = 20
          Width = 205
          Height = 22
          Style = csOwnerDrawFixed
          Color = clWhite
          ItemHeight = 16
          ItemIndex = 7
          TabOrder = 0
          Text = 'Courier'
          OnChange = cboFontChange
          Items.Strings = (
            'Arial'
            'Arial Black'
            'Arial Narrow'
            'Book Antiqua'
            'Bookman Old Style'
            'Century Gothic'
            'Comic Sans MS'
            'Courier'
            'Courier New'
            'Default'
            'Estrangelo Edessa'
            'Fixedsys'
            'Franklin Gothic Medium'
            'Garamond'
            'Gautami'
            'Georgia'
            'Haettenschweiler'
            'Impact'
            'Latha'
            'Lucida Console'
            'Lucida Sans Unicode'
            'Mangal'
            'Marlett'
            'Microsoft Sans Serif'
            'Modern'
            'Monotype Corsiva'
            'MS Outlook'
            'MS Sans Serif'
            'MS Serif'
            'MV Boli'
            'Palatino Linotype'
            'Raavi'
            'Roman'
            'Script'
            'Shruti'
            'Small Fonts'
            'Sylfaen'
            'Symbol'
            'System'
            'Tahoma'
            'Terminal'
            'Times New Roman'
            'Trebuchet MS'
            'Tunga'
            'Verdana'
            'Webdings'
            'Wingdings'
            'Wingdings 2'
            'Wingdings 3'
            'WST_Czec'
            'WST_Engl'
            'WST_Fren'
            'WST_Germ'
            'WST_Ital'
            'WST_Span'
            'WST_Swed')
          Selected = 'Courier'
        end
        object chkBold: TFlatCheckBox
          Left = 4
          Top = 66
          Width = 39
          Height = 17
          Caption = 'Bold'
          TabOrder = 1
          TabStop = True
          OnClick = chkBoldClick
        end
        object chkItalic: TFlatCheckBox
          Left = 44
          Top = 66
          Width = 41
          Height = 17
          Caption = 'Italic'
          TabOrder = 2
          TabStop = True
          OnClick = chkItalicClick
        end
        object chkUnderlined: TFlatCheckBox
          Left = 88
          Top = 66
          Width = 71
          Height = 17
          Caption = 'Underlined'
          TabOrder = 3
          TabStop = True
          OnClick = chkUnderlinedClick
        end
        object chkVisible: TFlatCheckBox
          Left = 160
          Top = 66
          Width = 47
          Height = 17
          Caption = 'Visible'
          TabOrder = 4
          TabStop = True
          OnClick = chkVisibleClick
        end
        object pnlColors: TPanel
          Left = 6
          Top = 110
          Width = 201
          Height = 41
          BevelOuter = bvLowered
          TabOrder = 5
          object imgBackground: TImage
            Left = 108
            Top = 18
            Width = 19
            Height = 19
          end
          object lblBackground: TLabel
            Left = 108
            Top = 2
            Width = 60
            Height = 13
            Caption = 'Background:'
          end
          object imgForeground: TImage
            Left = 4
            Top = 18
            Width = 19
            Height = 19
          end
          object lblForeground: TLabel
            Left = 4
            Top = 4
            Width = 60
            Height = 13
            Caption = 'Foreground:'
          end
          object cmdSelectBackground: TFlatButton
            Left = 132
            Top = 19
            Width = 61
            Height = 18
            ColorFocused = 16245198
            ColorDown = 16245198
            ColorHighLight = 8623776
            ColorShadow = 8623776
            Caption = 'Select...'
            TabOrder = 0
            OnClick = cmdSelectBackgroundClick
          end
          object cmdSelectForeground: TFlatButton
            Left = 28
            Top = 19
            Width = 61
            Height = 18
            ColorFocused = 16245198
            ColorDown = 16245198
            ColorHighLight = 8623776
            ColorShadow = 8623776
            Caption = 'Select...'
            TabOrder = 1
            OnClick = cmdSelectForegroundClick
          end
        end
        object txtFontSize: TFlatEdit
          Left = 128
          Top = 86
          Width = 77
          Height = 19
          ColorFlat = clWhite
          TabOrder = 6
          Text = '0'
          OnChange = txtFontSizeChange
        end
        object chkUseDefaultFont: TFlatCheckBox
          Left = 4
          Top = 46
          Width = 101
          Height = 17
          Caption = 'Use Default Font'
          TabOrder = 7
          TabStop = True
          OnClick = chkUseDefaultFontClick
        end
      end
      object cmdReset: TFlatButton
        Left = 266
        Top = 23
        Width = 82
        Height = 21
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'General Reset'
        TabOrder = 2
        OnClick = cmdResetClick
      end
      object lstStyles: TListBox
        Left = 7
        Top = 65
        Width = 119
        Height = 156
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 3
        OnClick = lstStylesClick
      end
    end
    object jspTools: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Tools'
      object bvlTools1: TBevel
        Left = 4
        Top = 90
        Width = 347
        Height = 3
        Shape = bsTopLine
      end
      object lblCodeFolding: TLabel
        Left = 6
        Top = 158
        Width = 67
        Height = 13
        Caption = 'Code-Folding:'
      end
      object lblCaret: TLabel
        Left = 6
        Top = 96
        Width = 31
        Height = 13
        Caption = 'Caret:'
      end
      object lblAutoIndent: TLabel
        Left = 194
        Top = 158
        Width = 63
        Height = 13
        Caption = 'Auto-Indent:'
      end
      object chkHighlightBraces: TFlatCheckBox
        Left = 6
        Top = 22
        Width = 95
        Height = 17
        Caption = 'Highlight braces'
        TabOrder = 0
        TabStop = True
      end
      object chkAutoCloseBraces: TFlatCheckBox
        Left = 176
        Top = 6
        Width = 107
        Height = 17
        Caption = 'Auto-Close braces'
        TabOrder = 1
        TabStop = True
      end
      object chkAutoCloseQuotes: TFlatCheckBox
        Left = 176
        Top = 22
        Width = 107
        Height = 17
        Caption = 'Auto-Close quotes'
        TabOrder = 2
        TabStop = True
      end
      object chkClearUndoAfterSave: TFlatCheckBox
        Left = 6
        Top = 38
        Width = 125
        Height = 17
        Caption = 'Clear Undo after save'
        TabOrder = 3
        TabStop = True
      end
      object chkWordWrap: TFlatCheckBox
        Left = 176
        Top = 38
        Width = 173
        Height = 17
        Caption = 'Word-Wrap (not recommended)'
        TabOrder = 4
        TabStop = True
      end
      object pnlCodeFolding: TPanel
        Left = 6
        Top = 174
        Width = 183
        Height = 49
        BevelOuter = bvLowered
        TabOrder = 5
        object lblCodeFoldingStyle: TLabel
          Left = 4
          Top = 4
          Width = 94
          Height = 13
          Caption = 'Code-Folding Style:'
        end
        object cboCodeFolding: TFlatComboBox
          Left = 4
          Top = 20
          Width = 171
          Height = 21
          Style = csDropDownList
          Color = clWindow
          ItemHeight = 13
          Items.Strings = (
            'Arrows'
            'Box'
            'Circle'
            'Plus-Minus'
            'Disabled')
          TabOrder = 0
          Text = 'Box'
          ItemIndex = 1
        end
      end
      object pnlCaret: TPanel
        Left = 6
        Top = 112
        Width = 343
        Height = 43
        BevelOuter = bvLowered
        TabOrder = 6
        object imgCaretFore: TImage
          Left = 4
          Top = 18
          Width = 19
          Height = 19
        end
        object lblSelectCaretFore: TLabel
          Left = 4
          Top = 4
          Width = 60
          Height = 13
          Caption = 'Foreground:'
        end
        object imgCaretBack: TImage
          Left = 96
          Top = 18
          Width = 19
          Height = 19
        end
        object lblSelectCaretBack: TLabel
          Left = 96
          Top = 4
          Width = 60
          Height = 13
          Caption = 'Background:'
        end
        object bvlCaret1: TBevel
          Left = 186
          Top = 1
          Width = 3
          Height = 41
          Shape = bsLeftLine
        end
        object lblCaretPeriod: TLabel
          Left = 192
          Top = 4
          Width = 34
          Height = 13
          Caption = 'Period:'
        end
        object cmdSelectCaretFore: TFlatButton
          Left = 28
          Top = 20
          Width = 61
          Height = 17
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = 'Select...'
          TabOrder = 0
          OnClick = cmdSelectCaretForeClick
        end
        object cmdSelectCaretBack: TFlatButton
          Left = 120
          Top = 19
          Width = 61
          Height = 18
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = 'Select...'
          TabOrder = 1
          OnClick = cmdSelectCaretBackClick
        end
        object chkShowCaret: TFlatCheckBox
          Left = 266
          Top = 20
          Width = 73
          Height = 17
          Caption = 'Show Caret'
          TabOrder = 2
          TabStop = True
        end
        object txtPeriod: TFlatEdit
          Left = 192
          Top = 19
          Width = 67
          Height = 18
          ColorFlat = clWhite
          MaxLength = 6
          TabOrder = 3
          OnChange = txtPeriodChange
        end
      end
      object chkIndentGuides: TFlatCheckBox
        Left = 6
        Top = 6
        Width = 129
        Height = 17
        Caption = 'Activate Indent Guides'
        TabOrder = 7
        TabStop = True
      end
      object pnlAutoIndent: TPanel
        Left = 194
        Top = 174
        Width = 155
        Height = 49
        BevelOuter = bvLowered
        TabOrder = 8
        object chkAutoIndent: TFlatCheckBox
          Left = 4
          Top = 4
          Width = 135
          Height = 17
          Caption = 'Enable Auto-Indentation'
          TabOrder = 0
          TabStop = True
        end
        object cmdAdvancedAutoIndent: TFlatButton
          Left = 6
          Top = 23
          Width = 143
          Height = 20
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = 'Advanced...'
          TabOrder = 1
          OnClick = cmdAdvancedAutoIndentClick
        end
      end
      object chkDontLoadFilesTwice: TFlatCheckBox
        Left = 6
        Top = 54
        Width = 157
        Height = 17
        Caption = 'Don'#39't allow to load files twice'
        TabOrder = 9
        TabStop = True
      end
      object chkMakeBaks: TFlatCheckBox
        Left = 176
        Top = 54
        Width = 151
        Height = 17
        Caption = 'Create BAK-file for each file'
        TabOrder = 10
        TabStop = True
      end
      object chkDisableAC: TFlatCheckBox
        Left = 6
        Top = 70
        Width = 127
        Height = 17
        Caption = 'Disable Auto-Complete'
        TabOrder = 11
        TabStop = True
      end
      object chkDisableCT: TFlatCheckBox
        Left = 176
        Top = 70
        Width = 97
        Height = 15
        Caption = 'Disable Call-Tips'
        TabOrder = 12
        TabStop = True
      end
    end
    object jspShortcuts: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Shortcuts'
      object shpShortcuts: TShape
        Left = 8
        Top = 8
        Width = 339
        Height = 191
        Pen.Color = 8623776
      end
      object lvShortcuts: TListView
        Left = 9
        Top = 9
        Width = 337
        Height = 189
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Function'
            Width = 160
          end
          item
            Caption = 'Shortcut'
            Width = 160
          end>
        ColumnClick = False
        FlatScrollBars = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = lvShortcutsClick
        OnSelectItem = lvShortcutsSelectItem
      end
      object cmdApply: TFlatButton
        Left = 173
        Top = 205
        Width = 84
        Height = 20
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Apply'
        Enabled = False
        TabOrder = 1
        OnClick = cmdApplyClick
      end
      object cmdResetShortcuts: TFlatButton
        Left = 261
        Top = 205
        Width = 85
        Height = 20
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Reset'
        TabOrder = 2
        OnClick = cmdResetShortcutsClick
      end
      object txtShortcut: TFlatEdit
        Left = 8
        Top = 206
        Width = 159
        Height = 19
        ColorFlat = clWhite
        TabOrder = 3
        Text = 'None'
        OnKeyPress = txtShortcutKeyPress
        OnKeyUp = txtShortcutKeyUp
      end
    end
    object jspCodeSnippets: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Code-Snippets'
      object ftcCodeSnippets: TFlatTabControl
        Left = 6
        Top = 8
        Width = 343
        Height = 221
        Tabs.Strings = (
          'Pawn'
          'C++'
          'HTML'
          'Other')
        TabOrder = 0
        OnTabChanged = ftcCodeSnippetsTabChanged
        object shpCodeSnippets: TShape
          Left = 6
          Top = 28
          Width = 121
          Height = 165
          Pen.Color = 8623776
        end
        object shpCodeSnippet: TShape
          Left = 134
          Top = 28
          Width = 202
          Height = 188
          Pen.Color = 8623776
        end
        object lstCodeSnippets: TListBox
          Left = 7
          Top = 29
          Width = 119
          Height = 163
          BorderStyle = bsNone
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstCodeSnippetsClick
        end
        object txtCodeSnippet: TMemo
          Left = 135
          Top = 29
          Width = 200
          Height = 186
          BorderStyle = bsNone
          ScrollBars = ssBoth
          TabOrder = 1
          WantTabs = True
          OnChange = txtCodeSnippetChange
          OnEnter = txtCodeSnippetEnter
          OnExit = txtCodeSnippetExit
          OnKeyUp = txtCodeSnippetKeyUp
        end
        object cmdCSAdd: TFlatButton
          Left = 6
          Top = 196
          Width = 59
          Height = 20
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = 'Add'
          TabOrder = 2
          OnClick = cmdCSAddClick
        end
        object cmdCSRemove: TFlatButton
          Left = 68
          Top = 196
          Width = 59
          Height = 20
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = 'Remove'
          Enabled = False
          TabOrder = 3
          OnClick = cmdCSRemoveClick
        end
      end
    end
    object jspCompiler: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Compiler'
      object lblPAWN: TLabel
        Left = 8
        Top = 12
        Width = 75
        Height = 13
        Caption = 'Pawn-Compiler:'
      end
      object lblCPPCompiler: TLabel
        Left = 8
        Top = 114
        Width = 71
        Height = 13
        Caption = 'C++ Compiler:'
      end
      object pnlSMALLCompiler: TPanel
        Left = 8
        Top = 28
        Width = 341
        Height = 81
        BevelOuter = bvLowered
        TabOrder = 0
        object lblPAWNCompilerPath: TLabel
          Left = 4
          Top = 4
          Width = 115
          Height = 13
          Caption = 'Compiler (amxxpc.exe):'
        end
        object lblPAWNArgs: TLabel
          Left = 4
          Top = 40
          Width = 143
          Height = 13
          Caption = 'Optional Compiler Arguments:'
        end
        object lblSPAWNOutput: TLabel
          Left = 160
          Top = 40
          Width = 123
          Height = 13
          Caption = 'Default Output Directory:'
        end
        object txtPAWNCompilerPath: TFlatEdit
          Left = 4
          Top = 18
          Width = 297
          Height = 19
          ColorFlat = clWhite
          TabOrder = 0
        end
        object cmdBrowsePAWNCompiler: TFlatButton
          Left = 306
          Top = 18
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 1
          OnClick = cmdBrowsePAWNCompilerClick
        end
        object txtPAWNOutput: TFlatEdit
          Left = 158
          Top = 54
          Width = 143
          Height = 19
          ColorFlat = clWhite
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnChange = txtPAWNOutputExit
          OnEnter = txtPAWNOutputExit
          OnExit = txtPAWNOutputExit
        end
        object txtPAWNArgs: TFlatEdit
          Left = 4
          Top = 54
          Width = 149
          Height = 19
          ColorFlat = clWhite
          TabOrder = 3
        end
        object cmdBrowseOutputPAWN: TFlatButton
          Left = 306
          Top = 54
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 4
          OnClick = cmdBrowseOutputPAWNClick
        end
      end
      object pnlCPPCompiler: TPanel
        Left = 6
        Top = 130
        Width = 341
        Height = 97
        BevelOuter = bvLowered
        TabOrder = 1
        object lblCPPCompilerPath: TLabel
          Left = 4
          Top = 4
          Width = 45
          Height = 13
          Caption = 'Compiler:'
        end
        object lblCPPHint: TLabel
          Left = 4
          Top = 80
          Width = 263
          Height = 13
          Caption = 'Hint: AMXX-Studio'#39's C++ Editor supports only libraries!'
        end
        object lblCPPCompilerArgs: TLabel
          Left = 4
          Top = 40
          Width = 143
          Height = 13
          Caption = 'Optional Compiler Arguments:'
        end
        object lblCPPOutput: TLabel
          Left = 160
          Top = 40
          Width = 143
          Height = 13
          Caption = 'Optional Compiler Arguments:'
        end
        object txtCPPCompilerPath: TFlatEdit
          Left = 4
          Top = 18
          Width = 297
          Height = 19
          ColorFlat = clWhite
          TabOrder = 0
        end
        object cmdBrowseCPPCompiler: TFlatButton
          Left = 306
          Top = 18
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 1
          OnClick = cmdBrowseCPPCompilerClick
        end
        object txtCPPCompilerArguments: TFlatEdit
          Left = 4
          Top = 54
          Width = 149
          Height = 19
          ColorFlat = clWhite
          TabOrder = 2
        end
        object txtCPPOutput: TFlatEdit
          Left = 158
          Top = 54
          Width = 143
          Height = 19
          ColorFlat = clWhite
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          OnChange = txtCPPOutputChange
          OnEnter = txtCPPOutputChange
          OnExit = txtCPPOutputChange
        end
        object cmdBrowseOutputCPP: TFlatButton
          Left = 306
          Top = 54
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 4
          OnClick = cmdBrowseOutputCPPClick
        end
      end
    end
    object jspHalfLife: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Half-Life Directory'
      object pnlHLExecutable: TPanel
        Left = 38
        Top = 70
        Width = 281
        Height = 123
        BevelOuter = bvLowered
        TabOrder = 0
        object lblHLExec: TLabel
          Left = 4
          Top = 4
          Width = 100
          Height = 13
          Caption = 'Half-Life Executable:'
        end
        object lblCustomParameters: TLabel
          Left = 6
          Top = 82
          Width = 98
          Height = 13
          Caption = 'Custom parameters:'
        end
        object lblAMXXDir: TLabel
          Left = 6
          Top = 42
          Width = 180
          Height = 13
          Caption = 'AMX Mod X directory on listen server:'
        end
        object txtHLExec: TFlatEdit
          Left = 4
          Top = 20
          Width = 235
          Height = 19
          ColorFlat = clWhite
          TabOrder = 0
        end
        object cmdBrowseHL: TFlatButton
          Left = 244
          Top = 20
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 1
          OnClick = cmdBrowseHLClick
        end
        object txtCustomParameters: TFlatEdit
          Left = 6
          Top = 98
          Width = 267
          Height = 19
          ColorFlat = clWhite
          TabOrder = 2
        end
        object txtAMXXDir: TFlatEdit
          Left = 6
          Top = 58
          Width = 233
          Height = 19
          ColorFlat = clWhite
          TabOrder = 3
        end
        object cmdBrowseAMXXDir: TFlatButton
          Left = 244
          Top = 58
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 4
          OnClick = cmdBrowseAMXXDirClick
        end
      end
    end
    object jspFTP: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Settings'
      object lblFTPData: TLabel
        Left = 8
        Top = 26
        Width = 48
        Height = 13
        Caption = 'FTP Data:'
      end
      object lblDefaultDirectory: TLabel
        Left = 8
        Top = 108
        Width = 282
        Height = 13
        Caption = 'Plesae select the AMX Mod X directory on your FTP server:'
      end
      object pnlFTPData: TPanel
        Left = 6
        Top = 42
        Width = 339
        Height = 63
        BevelOuter = bvLowered
        TabOrder = 0
        object lblHost: TLabel
          Left = 6
          Top = 6
          Width = 26
          Height = 13
          Caption = 'Host:'
        end
        object lblPassword: TLabel
          Left = 258
          Top = 6
          Width = 50
          Height = 13
          Caption = 'Password:'
        end
        object lblUsername: TLabel
          Left = 176
          Top = 6
          Width = 52
          Height = 13
          Caption = 'Username:'
        end
        object lblPort: TLabel
          Left = 130
          Top = 6
          Width = 24
          Height = 13
          Caption = 'Port:'
        end
        object chkPassive: TFlatCheckBox
          Left = 6
          Top = 42
          Width = 143
          Height = 17
          Caption = 'Use PASV (Passive) Mode'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object txtHost: TFlatEdit
          Left = 6
          Top = 20
          Width = 121
          Height = 19
          ColorFlat = clWhite
          TabOrder = 1
        end
        object txtPort: TFlatEdit
          Left = 130
          Top = 20
          Width = 41
          Height = 19
          ColorFlat = clWhite
          TabOrder = 2
          Text = '21'
          OnChange = txtPortChange
        end
        object txtUsername: TFlatEdit
          Left = 176
          Top = 20
          Width = 77
          Height = 19
          ColorFlat = clWhite
          TabOrder = 3
        end
        object txtPassword: TFlatEdit
          Left = 256
          Top = 20
          Width = 79
          Height = 19
          ColorFlat = clWhite
          PasswordChar = '*'
          TabOrder = 4
        end
        object cmdConnect: TFlatButton
          Left = 256
          Top = 42
          Width = 79
          Height = 17
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorBorder = clBtnShadow
          ColorHighLight = clBtnShadow
          ColorShadow = clBtnShadow
          Caption = 'Connect'
          TabOrder = 5
          OnClick = cmdConnectClick
        end
      end
      object pnlDefaultPath: TPanel
        Left = 8
        Top = 124
        Width = 339
        Height = 105
        BevelOuter = bvLowered
        TabOrder = 1
        object lblDefaultDir: TLabel
          Left = 4
          Top = 84
          Width = 104
          Height = 13
          Caption = 'AMX Mod X Directory:'
        end
        object pnlDirectory: TPanel
          Left = 4
          Top = 6
          Width = 331
          Height = 71
          BevelOuter = bvLowered
          TabOrder = 0
          object trvDirectories: TTreeView
            Left = 1
            Top = 1
            Width = 329
            Height = 69
            Align = alClient
            BorderStyle = bsNone
            Images = ilImages
            Indent = 19
            TabOrder = 0
            OnChange = trvDirectoriesChange
            OnCollapsing = trvDirectoriesCollapsing
            OnExpanding = trvDirectoriesExpanding
            OnExpanded = trvDirectoriesExpanded
          end
        end
        object txtDefaultDir: TFlatEdit
          Left = 112
          Top = 82
          Width = 223
          Height = 19
          ColorFlat = clWhite
          Enabled = False
          TabOrder = 1
        end
      end
    end
    object jspProxy: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Proxy'
      object pnlProxy: TPanel
        Left = 62
        Top = 48
        Width = 223
        Height = 135
        BevelOuter = bvLowered
        Caption = 'pnlProxy'
        TabOrder = 0
        object lblProxyPassword: TLabel
          Left = 116
          Top = 88
          Width = 50
          Height = 13
          Caption = 'Password:'
          Enabled = False
        end
        object lblProxyUsername: TLabel
          Left = 8
          Top = 88
          Width = 52
          Height = 13
          Caption = 'Username:'
          Enabled = False
        end
        object lblProxyPort: TLabel
          Left = 150
          Top = 48
          Width = 24
          Height = 13
          Caption = 'Port:'
          Enabled = False
        end
        object lblProxyHost: TLabel
          Left = 8
          Top = 48
          Width = 26
          Height = 13
          Caption = 'Host:'
          Enabled = False
        end
        object lblProxy: TLabel
          Left = 8
          Top = 6
          Width = 32
          Height = 13
          Caption = 'Proxy:'
        end
        object txtProxyPassword: TFlatEdit
          Left = 114
          Top = 104
          Width = 99
          Height = 19
          ColorFlat = clWhite
          Enabled = False
          PasswordChar = '*'
          TabOrder = 4
          OnChange = txtProxyHostChange
        end
        object txtProxyUsername: TFlatEdit
          Left = 8
          Top = 104
          Width = 99
          Height = 19
          ColorFlat = clWhite
          Enabled = False
          TabOrder = 3
          OnChange = txtProxyHostChange
        end
        object txtProxyHost: TFlatEdit
          Left = 8
          Top = 64
          Width = 137
          Height = 19
          ColorFlat = clWhite
          Enabled = False
          TabOrder = 1
          OnChange = txtProxyHostChange
        end
        object txtProxyPort: TFlatEdit
          Left = 148
          Top = 64
          Width = 65
          Height = 19
          ColorFlat = clWhite
          Enabled = False
          MaxLength = 5
          TabOrder = 2
          Text = '8080'
          OnChange = txtProxyPortChange
        end
        object cboProxy: TFlatComboBox
          Left = 6
          Top = 22
          Width = 207
          Height = 21
          Style = csDropDownList
          Color = clWindow
          ItemHeight = 13
          Items.Strings = (
            'None'
            'HTTP Proxy with FTP'
            'Open'
            'Site'
            'Transparent'
            'User (Pass)'
            'User (Site)')
          TabOrder = 0
          Text = 'None'
          ItemIndex = 0
          OnChange = cboProxyChange
        end
      end
    end
    object jspPlugIns: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Plug-Ins'
      object shpPlugins: TShape
        Left = 6
        Top = 6
        Width = 343
        Height = 191
        Pen.Color = 8623776
      end
      object lvPlugins: TListView
        Left = 7
        Top = 7
        Width = 341
        Height = 188
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Plugin Name'
            Width = 70
          end
          item
            Caption = 'Filename'
            Width = 90
          end
          item
            Caption = 'Description'
            Width = 105
          end
          item
            Caption = 'Status'
            Width = 60
          end>
        ColumnClick = False
        FlatScrollBars = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object cmdReload: TFlatButton
        Left = 7
        Top = 201
        Width = 80
        Height = 25
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Reload'
        TabOrder = 1
        OnClick = cmdReloadClick
      end
      object cmdLoad: TFlatButton
        Left = 93
        Top = 201
        Width = 80
        Height = 25
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Load'
        TabOrder = 2
        OnClick = cmdLoadClick
      end
      object cmdUnload: TFlatButton
        Left = 179
        Top = 201
        Width = 80
        Height = 25
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Unload'
        TabOrder = 3
        OnClick = cmdUnloadClick
      end
      object cmdRemove: TFlatButton
        Left = 266
        Top = 201
        Width = 80
        Height = 25
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Remove'
        TabOrder = 4
        OnClick = cmdRemoveClick
      end
    end
    object jspMisc: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Misc'
      object pnlDefaultNewPluginValues: TPanel
        Left = 8
        Top = 8
        Width = 337
        Height = 69
        BevelOuter = bvLowered
        TabOrder = 0
        object lblDefaultInfo: TLabel
          Left = 4
          Top = 4
          Width = 229
          Height = 13
          Caption = 'Use the following data for creating new plugins:'
        end
        object lblDefaultAuthor: TLabel
          Left = 212
          Top = 26
          Width = 37
          Height = 13
          Caption = 'Author:'
        end
        object lblDefaultVersion: TLabel
          Left = 132
          Top = 26
          Width = 39
          Height = 13
          Caption = 'Version:'
        end
        object lblDefaultName: TLabel
          Left = 6
          Top = 26
          Width = 68
          Height = 13
          Caption = 'Plug-In Name:'
        end
        object txtDefaultAuthor: TFlatEdit
          Left = 212
          Top = 42
          Width = 117
          Height = 19
          ColorFlat = clWhite
          TabOrder = 0
          Text = 'Your name'
        end
        object txtDefaultName: TFlatEdit
          Left = 6
          Top = 42
          Width = 121
          Height = 19
          ColorFlat = clWhite
          TabOrder = 1
          Text = 'New Plugin'
        end
        object txtDefaultVersion: TFlatEdit
          Left = 132
          Top = 42
          Width = 73
          Height = 19
          ColorFlat = clWhite
          TabOrder = 2
          Text = '1.0'
        end
      end
      object chkShowStatusbar: TFlatCheckBox
        Left = 8
        Top = 146
        Width = 93
        Height = 17
        Caption = 'Show Statusbar'
        TabOrder = 1
        TabStop = True
      end
      object pnlNotes: TPanel
        Left = 8
        Top = 84
        Width = 337
        Height = 57
        BevelOuter = bvLowered
        TabOrder = 2
        object lblSaveNotesTo: TLabel
          Left = 4
          Top = 4
          Width = 71
          Height = 13
          Caption = 'Save notes to:'
        end
        object optFileComment: TFlatRadioButton
          Left = 4
          Top = 20
          Width = 131
          Height = 17
          Caption = 'Its file (as comment)'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object optConfig: TFlatRadioButton
          Left = 138
          Top = 20
          Width = 193
          Height = 17
          Caption = 'AMXX-Studio configs (recommended)'
          TabOrder = 1
        end
        object optDontSave: TFlatRadioButton
          Left = 4
          Top = 36
          Width = 69
          Height = 17
          Caption = 'Don'#39't save'
          TabOrder = 2
        end
      end
    end
    object jspCTSettings: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Settings'
      object lblCodeExplorer: TLabel
        Left = 6
        Top = 6
        Width = 73
        Height = 13
        Caption = 'Code-Explorer:'
      end
      object lblCodeInspector: TLabel
        Left = 6
        Top = 94
        Width = 79
        Height = 13
        Caption = 'Code-Inspector:'
      end
      object pnlPCSpeed: TPanel
        Left = 6
        Top = 22
        Width = 341
        Height = 67
        BevelOuter = bvLowered
        TabOrder = 0
        object lblCPUSpeed: TLabel
          Left = 4
          Top = 4
          Width = 267
          Height = 13
          Caption = 'Please rate your CPU speed (needed for Auto-Update):'
        end
        object lblSlow: TLabel
          Left = 4
          Top = 48
          Width = 22
          Height = 13
          Caption = 'Slow'
        end
        object lblAverage: TLabel
          Left = 133
          Top = 48
          Width = 41
          Height = 13
          Caption = 'Average'
        end
        object lblFast: TLabel
          Left = 314
          Top = 48
          Width = 21
          Height = 13
          Caption = 'Fast'
        end
        object sldSpeed: TJvxSlider
          Left = 4
          Top = 18
          Width = 331
          Height = 31
          Increment = 2
          MinValue = 1
          MaxValue = 10
          TabOrder = 0
          Value = 5
        end
      end
      object pnlCodeInspector: TPanel
        Left = 6
        Top = 112
        Width = 341
        Height = 47
        BevelOuter = bvLowered
        TabOrder = 1
        object lblLangDir: TLabel
          Left = 4
          Top = 4
          Width = 143
          Height = 13
          Caption = 'Directory of all language files:'
        end
        object cmdBrowseLangDir: TFlatButton
          Left = 306
          Top = 20
          Width = 29
          Height = 19
          ColorFocused = 16245198
          ColorDown = 16245198
          ColorHighLight = 8623776
          ColorShadow = 8623776
          Caption = '...'
          TabOrder = 0
          OnClick = cmdBrowseLangDirClick
        end
        object txtLangDir: TFlatEdit
          Left = 4
          Top = 20
          Width = 297
          Height = 19
          ColorFlat = clWhite
          TabOrder = 1
        end
      end
      object chkAUDisable: TFlatCheckBox
        Left = 8
        Top = 166
        Width = 297
        Height = 17
        Caption = 'Disable Auto-Update if plugin has more than               lines'
        TabOrder = 2
        TabStop = True
      end
      object txtAUDisable: TFlatEdit
        Left = 238
        Top = 166
        Width = 37
        Height = 16
        ColorFlat = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -8
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        Text = '1500'
        OnExit = txtAUDisableExit
      end
      object chkAutoHideCT: TFlatCheckBox
        Left = 8
        Top = 184
        Width = 287
        Height = 17
        Caption = 'Hide calltip if function parameters have been customized'
        TabOrder = 4
        TabStop = True
      end
    end
    object jspAutocompleteCheck: TJvStandardPage
      Left = 0
      Top = 25
      Width = 351
      Height = 235
      Caption = 'Autocomplete Check'
      object shpFunctions: TShape
        Left = 6
        Top = 32
        Width = 121
        Height = 175
        Pen.Color = 8623776
      end
      object shpParams: TShape
        Left = 132
        Top = 30
        Width = 215
        Height = 177
        Pen.Color = 8623776
      end
      object lstFunctions: TListBox
        Left = 7
        Top = 33
        Width = 119
        Height = 173
        BorderStyle = bsNone
        ItemHeight = 13
        TabOrder = 1
        OnClick = lstFunctionsClick
      end
      object txtSearch: TFlatEdit
        Left = 6
        Top = 8
        Width = 121
        Height = 19
        ColorFlat = clWhite
        TabOrder = 0
        OnChange = txtSearchChange
      end
      object lvParams: TListView
        Left = 134
        Top = 31
        Width = 211
        Height = 174
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Param'
            Width = 45
          end
          item
            Caption = 'Auto-Complete items'
            Width = 400
          end>
        ColumnClick = False
        FlatScrollBars = True
        RowSelect = True
        TabOrder = 2
        ViewStyle = vsReport
        OnDblClick = lvParamsDblClick
      end
      object cmdAddParam: TFlatButton
        Left = 132
        Top = 211
        Width = 137
        Height = 18
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Add parameter information'
        TabOrder = 5
        OnClick = cmdAddParamClick
      end
      object cmdRemParam: TFlatButton
        Left = 274
        Top = 211
        Width = 73
        Height = 18
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Delete'
        TabOrder = 6
        OnClick = cmdRemParamClick
      end
      object cmdAddFunction: TFlatButton
        Left = 6
        Top = 211
        Width = 73
        Height = 18
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Add function'
        TabOrder = 3
        OnClick = cmdAddFunctionClick
      end
      object cmdRemFunction: TFlatButton
        Left = 82
        Top = 211
        Width = 45
        Height = 18
        ColorFocused = 16245198
        ColorDown = 16245198
        ColorHighLight = 8623776
        ColorShadow = 8623776
        Caption = 'Delete'
        TabOrder = 4
        OnClick = cmdRemFunctionClick
      end
    end
    object lblCurrSetting: TLabel
      Left = 0
      Top = 0
      Width = 351
      Height = 25
      Align = alTop
      Alignment = taCenter
      Caption = 'Editor - Highlighter'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 260
    Width = 504
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object bvlControls: TBevel
      Left = 0
      Top = 0
      Width = 504
      Height = 3
      Align = alTop
      Shape = bsTopLine
    end
    object lblACHint: TLabel
      Left = 2
      Top = 16
      Width = 312
      Height = 13
      Caption = 'This feature allows you to set your Autocomplete-Items manually'
      Visible = False
    end
    object cmdOK: TFlatButton
      Left = 416
      Top = 7
      Width = 85
      Height = 25
      ColorFocused = 16245198
      ColorDown = 16245198
      ColorHighLight = 8623776
      ColorShadow = 8623776
      Caption = '&OK'
      TabOrder = 0
      ModalResult = 1
    end
    object cmdCancel: TFlatButton
      Left = 326
      Top = 7
      Width = 85
      Height = 25
      ColorFocused = 16245198
      ColorDown = 16245198
      ColorHighLight = 8623776
      ColorShadow = 8623776
      Caption = '&Cancel'
      TabOrder = 1
      ModalResult = 2
    end
  end
  object odBrowse: TOpenDialog
    Filter = 'Executables (*.exe)|*.exe|All files (*.*)|*.*'
    Left = 455
    Top = 2
  end
  object ilImages: TImageList
    Left = 424
    Top = 2
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      00000000000000000000000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00000000000000000000000000078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0062CBF200078DBE00A4F6
      FD0066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CEF50066CE
      F5003AAFDA00ABF2FC00078DBE0000000000078DBE0025A0CD005FC8EE0083E1
      FB0066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CDF40066CD
      F4003AAED8001495C40000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE006BD2F700078DBE00A9F3
      FC006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4F8006FD4
      F8003AAFDA00BEECF400078DBE0000000000078DBE004CBBE30031A8D30095EC
      FB006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4F9006ED4
      F9003FB1DB00C8F6FB00078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0072D6FA00078DBE00ABF2
      FC0079DCFA0079DCFA0079DCFA0079DCFA0079DCFA007ADDFB0079DCFA0079DC
      FA0045B6DF00BEECF400078DBE0000000000078DBE0072D6F900078DBE00ACF8
      FD007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADBFA007ADB
      FA0043B5DD00C8F6FB00078DBE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE007ADDFB00078DBE00B4F3
      FC0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4FB0084E4
      FB0045B6DF00C3F0F700078DBE0000000000078DBE007CDDFA001495C40095EC
      FB0092EAFB0086E3FB0083E1FB0083E1FB0086E3FB0083E1FB0083E1FB0086E3
      FB0049B8E000C8F6FB001495C400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0081E2FB00078DBE00BEF4
      FC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EECFC008EEC
      FC004CBCE400BEF4FC00078DBE0000000000078DBE0083E1FB0043B5DD0059C4
      EA00ACF8FD008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9FB008FE9
      FB004CBBE300C8F6FB00C8F6FB00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE008BEAFC00078DBE00D2F7
      FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6FC00C8F6
      FC00BADADF00D2F7FC00078DBE0000000000078DBE008CE7FB0078DAFA0025A0
      CD00E5F8FA00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6FB00C8F6
      FB0095ECFB00E5F8FA00CFF6FA00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE0094F1FD00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE0000000000078DBE0096F0FC0096F0FC001495
      C400078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00078DBE00078DBE00078DBE00078DBE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE009BF4FD009BF4FD009BF4
      FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4FD009BF4
      FD00078CBD00000000000000000000000000078DBE009CF5FD009AF4FD009AF4
      FD009CF5FD009DF6FD009AF4FD009CF5FD009AF4FD009CF5FD009AF4FD009AF4
      FD00088DBE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000078DBE00D2F7FC00A0F6FD00A0F6
      FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7FD00A2F7FD00A0F6FD00A2F7
      FD00078CBD00000000000000000000000000078DBE00E5F8FA00A1F9FE00A1F9
      FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9FE00A1F9
      FE00088DBE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000078DBE00D2F7FC00A4F6
      FD00A4F6FD00A4F6FD00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE000000000000000000000000000000000000000000078DBE00E5F8FA00A4F9
      FE00A4F9FE00A4F9FE00078DBE00078DBE00078DBE00078DBE00078DBE00078D
      BE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000078DBE00078D
      BE00078DBE00078DBE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0080038007000000000001000300000000
      0001000100000000000100010000000000010001000000000001000000000000
      0001000000000000000100000000000000070007000000000007000700000000
      800F800F00000000C3FFC3FF00000000FFFFFFFF00000000FFFFFFFF00000000
      FFFFFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
end
