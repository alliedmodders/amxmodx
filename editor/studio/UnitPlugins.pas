unit UnitPlugins;

interface

uses SysUtils, Classes, Windows, Messages, Forms, ComCtrls;

type TCodeSnippetClick = function (pTitle, pCategory: PChar; pCode: PChar): Integer; cdecl;
     TFileAction = function (pFilename: PChar): Integer; cdecl;
     TDocChange = function (pIndex: DWord; pFilename: PChar; pHighlighter: PChar; pRestoreCaret: Boolean): Integer; cdecl;
     TProjectsChange = function (pOldIndex, pNewIndex: DWord): Integer; cdecl;
     TCreateNewFile = function (Item: PByte): Integer; cdecl;
     TDisplaySearch = function (pSearchList: PChar; pSelected: PChar): Integer; cdecl;
     TSearch = function (pExpression: PChar; pCaseSensivity, pWholeWords, pSearchFromCaret, pSelectedOnly, pRegEx, pForward: Boolean): Integer; cdecl;
     TSearchReplace = function (pExpression, pReplace, pExpList, pRepList: PChar; pCaseSensivity, pWholeWords, pSearchFromCaret, pSelectedOnly, pRegEx, pForward: Boolean): Integer; cdecl;
     TVisibleControlChange = function (pControl: DWord; pShow: Boolean): Integer; cdecl;
     TCompile = function (pCompileType: DWord; Lang, Filename: PChar): Integer; cdecl;
     TShowHelp = function (pHelpType: DWord): Integer; cdecl;
     TCustomItemClick = function (pCaption: PChar): Integer; cdecl;
     TThemeChanged = function (pTheme: PChar): Integer; cdecl;

     TModified = function (pText: PChar): Integer; cdecl;
     TKeyPress = function (var pKey: Char): Integer; cdecl;
     TEditorClick = function (pDoubleClick: Boolean): Integer; cdecl;
     TUpdateSel = function (pSelStart, pSelLength, pFirstVisibleLine: DWord): Integer; cdecl;
     TCallTipShow = function (pList: PChar): Integer; cdecl;
     TCallTipClick = function (pPosition: DWord): Integer; cdecl;
     TAutoCompleteShow = function (pList: PChar): Integer; cdecl;
     TAutoCompleteSelect = function (pText: PChar): Integer; cdecl;

     TAppMsg = function (pHwnd: HWND; pMessage: DWord; pWParam, pLParam: Integer; pTime: DWord; pPt: TPoint): Integer; cdecl;
     TUpdateCodeTools = function (pLang, pFilename, pCurrProjects: PChar): Integer; cdecl;
     TOutputEvent = function (pItemIndex: Integer): Integer; cdecl;

type TIntegerArray = array of Integer;

type TLoadInfo = record
  { Plugin Values }
  sPluginName: PChar;
  sPluginDescription: PChar;
  { Form Handles }
  hAllFilesForm: HWND;
  hAutoIndent: HWND;
  hClose: HWND;
  hConnGen: HWND;
  hGoToLine: HWND;
  hHTMLPreview: HWND;
  hHudMsgGenerator: HWND;
  hInfo: HWND;
  hMainForm: HWND;
  hMenuGenerator: HWND;
  hMOTDGen: HWND;
  hPluginsIniEditor: HWND;
  hReplace: HWND;
  hSearch: HWND;
  hSelectColor: HWND;
  hSettings: HWND;
  hSocketsTerminal: HWND;
  hSplashscreen: HWND;
  { Important Control Handles }
  hOutput: HWND;
  hCodeExplorer: HWND;
  hCodeInspector: HWND; // even if it won't be useful
  hNotes: HWND;
  { Other }
  pApplication: Pointer; // this is only useful for Delphi developers
end;

type PLoadInfo = ^TLoadInfo;
     TLoadPlugin = procedure (var LoadInfo: PLoadInfo); cdecl;
     TUnloadPlugin = procedure; cdecl;

procedure SendToMainApp(eData: String);

function LoadPlugin(ListItem: TListItem): Boolean;
procedure UnloadPlugin(ListItem: TListItem);

function Plugin_CodeSnippetClick(Title, Category: String; Code: String): Boolean;
function Plugin_FileLoad(Filename: String; Loading: Boolean): Boolean;
function Plugin_FileSave(Filename: String; Saving: Boolean): Boolean;
function Plugin_DocChange(Index: Integer; Filename, Highlighter: String; RestoreCaret, Changing: Boolean): Boolean;
function Plugin_ProjectsChange(OldIndex, NewIndex: Integer; Changing: Boolean): Boolean;
function Plugin_CreateNewFile(Item: Byte; Creating: Boolean): Boolean;
function Plugin_Search(SearchList, Selected: String; Displaying, SearchAgain: Boolean): Boolean;
function Plugin_SearchReplace(Expression, Replace, ExpList, RepList: String; CaseSensivity, WholeWords, SearchFromCaret, SelectedOnly, RegEx, Forward: Boolean): Boolean;
function Plugin_VisibleControlChange(Control: Integer; Show: Boolean): Boolean;
function Plugin_Compile(CompileType: Integer; Lang, Filename: String; Compiling: Boolean): Boolean;
function Plugin_ShowHelp(HelpType: Integer): Boolean;
function Plugin_CustomItemClick(Caption: String): Boolean;
function Plugin_ThemeChange(Theme: String): Boolean;

function Plugin_Modified(Code: PChar): Boolean;
function Plugin_KeyPress(var Key: Char): Boolean;
function Plugin_EditorClick(DoubleClick: Boolean): Boolean;
function Plugin_UpdateSel(SelStart, SelLength, FirstVisibleLine: Integer): Boolean;
function Plugin_CallTipShow(List: PChar): Boolean;
function Plugin_CallTipClick(Position: Integer): Boolean;
function Plugin_AutoCompleteShow(List: PChar): Boolean;
function Plugin_AutoCompleteSelect(Text: PChar): Boolean;

function Plugin_AppMsg(hwnd: HWND; Message: DWord; wParam, lParam: Integer; time: DWord; pt: TPoint): Boolean;
function Plugin_UpdateCodeExplorer(Lang, Filename, CurrProjects: String; Updating: Boolean): Boolean;
function Plugin_UpdateCodeInspector(Lang, Filename, CurrProjects: String; Updating: Boolean): Boolean;
function Plugin_OutputDblClick(ItemIndex: Integer): Boolean;
function Plugin_OutputPopup(ItemIndex: Integer): Boolean;

const { Return values for dlls }
      PLUGIN_CONTINUE = 0; // continue...
      PLUGIN_STOP = 1; // stop calling funcs and don't handle the command
      PLUGIN_HANDLED = 2; // don't handle the command
      { Compile values }
      COMP_DEFAULT = 0;
      COMP_STARTHL = 1;
      COMP_UPLOAD = 2;
      { Help values }
      HELP_DEFAULT = 0;
      HELP_SEARCH = 1;
      HELP_FORUMS = 2;
      HELP_ABOUT = 3;
      { Controls for visible state }
      CTRL_OUTPUT = 0; // Output list
      CTRL_CODETOOLS_MAIN = 1; // Code-Tools window
      CTRL_CODETOOLS_ITEM = 2; // Code-Tools tab
      CTRL_NOTES = 3; // Notes tab
      { Languages }
      NEW_PAWN_PLUGIN = 0;
      NEW_PAWN_EMPTYPLUGIN = 1;
      NEW_PAWN_HEADER = 2;
      NEW_CPP_MODULE = 3;
      NEW_CPP_UNIT = 4;
      NEW_CPP_HEADER = 5;
      NEW_OTHER_TEXTFILE = 6;
      NEW_OTHER_HTML = 7;
      NEW_OTHER_SQL = 8;
      NEW_OTHER_XML = 9;

implementation

uses UnitfrmSettings, UnitMainTools, UnitfrmAllFilesForm,
  UnitfrmAutoIndent, UnitfrmClose, UnitfrmConnGen, UnitfrmGoToLine,
  UnitfrmHTMLPreview, UnitfrmHudMsgGenerator, UnitfrmInfo, UnitfrmMain,
  UnitfrmMenuGenerator, UnitfrmMOTDGen, UnitfrmPluginsIniEditor,
  UnitfrmReplace, UnitfrmSearch, UnitfrmSelectColor,
  UnitfrmSocketsTerminal, UnitfrmSplashscreen, UnitLanguages;

function LoadPlugin(ListItem: TListItem): Boolean;
var eLoadInfo: TLoadInfo;
    LoadInfo: PLoadInfo;
    eHandle: Cardinal;
    eFunc, eFunc2: TLoadPlugin;
begin
  Result := False;

  with eLoadInfo do begin
    sPluginName := 'Untitled';
    sPluginDescription := 'No description';
    { Handles }
    hAllFilesForm := frmAllFilesForm.Handle;
    hAutoIndent := frmAutoIndent.Handle;
    hClose := frmClose.Handle;
    hConnGen := frmConnGen.Handle;
    hGoToLine := frmGoToLine.Handle;
    hHTMLPreview := frmHTMLPreview.Handle;
    hHudMsgGenerator := frmHudMsgGenerator.Handle;
    hInfo := frmInfo.Handle;
    hMainForm := frmMain.Handle;
    hMenuGenerator := frmMenuGenerator.Handle;
    hMOTDGen := frmMOTDGen.Handle;
    hPluginsIniEditor := frmPluginsIniEditor.Handle;
    hReplace := frmReplace.Handle;
    hSearch := frmSearch.Handle;
    hSelectColor := frmSelectColor.Handle;
    hSettings := frmSettings.Handle;
    hSocketsTerminal := frmSocketsTerminal.Handle;
    hSplashscreen := frmSplashscreen.Handle;
    { Important Control Handles }
    hOutput := frmMain.lstOutput.Handle;
    hCodeExplorer := frmMain.trvExplorer.Handle;
    hCodeInspector := frmMain.jviCode.Handle; // even if it won't be useful
    hNotes := frmMain.rtfNotes.Handle;
    { Other }
    pApplication := @Application; // this is only useful for Delphi developers
  end;

  eHandle := LoadLibrary(PChar(ExtractFilePath(ParamStr(0)) + 'plugins\' + ListItem.SubItems[0]));
  if eHandle = 0 then exit;
  @eFunc := GetProcAddress(eHandle, 'pftPluginLoad');
  @eFunc2 := GetProcAddress(eHandle, 'pftPluginUnload');
  
  if @eFunc2 <> nil then begin
    if @eFunc <> nil then begin
      ListItem.Data := Pointer(eHandle);
      ListItem.SubItems[2] := 'Loaded';
      LoadInfo := @eLoadInfo;
      eFunc(LoadInfo);
      ListItem.Caption := eLoadInfo.sPluginName;
      ListItem.SubItems[1] := eLoadInfo.sPluginDescription;
    end
    else
      MessageBox(Application.Handle, PChar('Error loading plugin:' + #13 + 'pftPluginLoad function not found.'), PChar(ExtractFileName(ExtractFilePath(ParamStr(0)) + 'plugins\' + ListItem.SubItems[0])), MB_ICONERROR);
  end
  else
    MessageBox(Application.Handle, PChar('Error loading plugin:' + #13 + 'pftPluginUnload function not found.'), PChar(ExtractFileName(ExtractFilePath(ParamStr(0)) + 'plugins\' + ListItem.SubItems[0])), MB_ICONERROR);
end;

procedure UnloadPlugin(ListItem: TListItem);
var eFunc: TUnloadPlugin;
begin
  @eFunc := GetProcAddress(Cardinal(ListItem.Data), 'pftPluginUnload');
  if @eFunc <> nil then
    eFunc;
  FreeLibrary(Cardinal(ListItem.Data));
  
  ListItem.Data := nil;
  ListItem.Caption := '-';
  ListItem.SubItems[1] := '-';
  ListItem.SubItems[2] := 'Unloaded';
end;

procedure SendToMainApp(eData: String);
var HTargetWnd: HWND;
    ACopyDataStruct: TCopyDataStruct;  
begin
  with ACopyDataStruct do  
  begin  
    dwData := 0;  
    cbData := Length(eData) + 1;
    lpData := PChar(eData);  
  end;  

  HTargetWnd := FindWindow('TfrmMain', 'AMXX-Studio');     
  if HTargetWnd <> 0 then  
    SendMessage(HTargetWnd, WM_COPYDATA, 0, LongInt(@ACopyDataStruct));
end;


function GetDLLHandles: TIntegerArray;
var i, eCount: integer;
begin
  SetLength(Result, 0);
  eCount := 0;

  if not Started then exit;

  for i := 0 to frmSettings.lvPlugins.Items.Count -1 do begin
    if frmSettings.lvPlugins.Items[i].Data <> nil then begin
      SetLength(Result, eCount +1);
      Result[eCount] := Cardinal(frmSettings.lvPlugins.Items[i].Data);
      Inc(eCount, 1);
    end;
  end;
end;

function Plugin_CodeSnippetClick(Title, Category: String; Code: String): Boolean;
var Func: TCodeSnippetClick;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftCodeSnippetClick');

    if @Func <> nil then begin
      case Func(PChar(Title), PChar(Category), PChar(Code)) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_FileLoad(Filename: String; Loading: Boolean): Boolean;
var Func: TFileAction;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Loading then
      @Func := GetProcAddress(Handles[i], 'pftLoading')
    else
      @Func := GetProcAddress(Handles[i], 'pftLoaded');

    if @Func <> nil then begin
      case Func(PChar(Filename)) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_FileSave(Filename: String; Saving: Boolean): Boolean;
var Func: TFileAction;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Saving then
      @Func := GetProcAddress(Handles[i], 'pftSaving')
    else
      @Func := GetProcAddress(Handles[i], 'pftSaved');

    if @Func <> nil then begin
      case Func(PChar(Filename)) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_DocChange(Index: Integer; Filename, Highlighter: String; RestoreCaret, Changing: Boolean): Boolean;
var Func: TDocChange;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Changing then
      @Func := GetProcAddress(Handles[i], 'pftDocChanging')
    else
      @Func := GetProcAddress(Handles[i], 'pftDocChanged');

    if @Func <> nil then begin
      case Func(Index, PChar(Filename), PChar(Highlighter), RestoreCaret) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_ProjectsChange(OldIndex, NewIndex: Integer; Changing: Boolean): Boolean;
var Func: TProjectsChange;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Changing then
      @Func := GetProcAddress(Handles[i], 'pftProjectsChanging')
    else
      @Func := GetProcAddress(Handles[i], 'pftProjectsChanged');

    if @Func <> nil then begin
      case Func(OldIndex, NewIndex) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_CreateNewFile(Item: Byte; Creating: Boolean): Boolean;
var Func: TCreateNewFile;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Creating then
      @Func := GetProcAddress(Handles[i], 'pftCreatingNewFile')
    else
      @Func := GetProcAddress(Handles[i], 'pftCreatedNewFile');

    if @Func <> nil then begin
      case Func(PByte(Item)) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_Search(SearchList, Selected: String; Displaying, SearchAgain: Boolean): Boolean;
var Func: TDisplaySearch;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Displaying then
      @Func := GetProcAddress(Handles[i], 'pftDisplayingSearch')
    else if SearchAgain then
      @Func := GetProcAddress(Handles[i], 'pftSearchAgain')
    else
      @Func := GetProcAddress(Handles[i], 'pftSearch');

    if @Func <> nil then begin
      case Func(PChar(SearchList), PChar(Selected)) of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_SearchReplace(Expression, Replace, ExpList, RepList: String; CaseSensivity, WholeWords, SearchFromCaret, SelectedOnly, RegEx, Forward: Boolean): Boolean;
var Func: TSearchReplace;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftSearchReplace');

    if @Func <> nil then begin
      case Func(PChar(Expression), PChar(Replace), PChar(ExpList), PChar(RepList), CaseSensivity, WholeWords, SearchFromCaret, SelectedOnly, RegEx, Forward)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_VisibleControlChange(Control: Integer; Show: Boolean): Boolean;
var Func: TVisibleControlChange;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftVisibleControlChange');

    if @Func <> nil then begin
      case Func(Control, Show)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_Compile(CompileType: Integer; Lang, Filename: String; Compiling: Boolean): Boolean;
var Func: TCompile;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Compiling then
      @Func := GetProcAddress(Handles[i], 'pftCompiling')
    else
      @Func := GetProcAddress(Handles[i], 'pftCompile');

    if @Func <> nil then begin
      case Func(CompileType, PChar(Lang), PChar(Filename))  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_ShowHelp(HelpType: Integer): Boolean;
var Func: TShowHelp;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftShowHelp');

    if @Func <> nil then begin
      case Func(HelpType)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_CustomItemClick(Caption: String): Boolean;
var Func: TCustomItemClick;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftCustomItemClick');

    if @Func <> nil then begin
      case Func(PChar(Caption))  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_ThemeChange(Theme: String): Boolean;
var Func: TThemeChanged;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftThemeChanged');

    if @Func <> nil then begin
      case Func(PChar(Theme))  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_Modified(Code: PChar): Boolean;
var Func: TModified;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftModified');

    if @Func <> nil then begin
      case Func(Code)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_KeyPress(var Key: Char): Boolean;
var Func: TKeyPress;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftKeyPress');

    if @Func <> nil then begin
      case Func(Key)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_EditorClick(DoubleClick: Boolean): Boolean;
var Func: TEditorClick;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if DoubleClick then
      @Func := GetProcAddress(Handles[i], 'pftDoubleClick')
    else
      @Func := GetProcAddress(Handles[i], 'pftClick');

    if @Func <> nil then begin
      case Func(DoubleClick)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_UpdateSel(SelStart, SelLength, FirstVisibleLine: Integer): Boolean;
var Func: TUpdateSel;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftUpdateSel');

    if @Func <> nil then begin
      case Func(SelStart, SelLength, FirstVisibleLine)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_CallTipShow(List: PChar): Boolean;
var Func: TCallTipShow;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftCallTipShow');

    if @Func <> nil then begin
      case Func(List)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_CallTipClick(Position: Integer): Boolean;
var Func: TCallTipClick;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftCallTipClick');

    if @Func <> nil then begin
      case Func(Position)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_AutoCompleteShow(List: PChar): Boolean;
var Func: TAutoCompleteShow;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftAutoCompleteShow');

    if @Func <> nil then begin
      case Func(List)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_AutoCompleteSelect(Text: PChar): Boolean;
var Func: TAutoCompleteSelect;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftAutoCompleteSelect');

    if @Func <> nil then begin
      case Func(Text)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_AppMsg(hwnd: HWND; Message: DWord; wParam, lParam: Integer; time: DWord; pt: TPoint): Boolean;
var Func: TAppMsg;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftMessage');

    if @Func <> nil then begin
      case Func(hwnd, Message, wParam, lParam, time, pt)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;                                 

function Plugin_UpdateCodeExplorer(Lang, Filename, CurrProjects: String; Updating: Boolean): Boolean;
var Func: TUpdateCodeTools;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Updating then
      @Func := GetProcAddress(Handles[i], 'pftUpdatingCodeExplorer')
    else
      @Func := GetProcAddress(Handles[i], 'pftUpdatedCodeExplorer');

    if @Func <> nil then begin
      case Func(PChar(Lang), PChar(Filename), PChar(CurrProjects))  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_UpdateCodeInspector(Lang, Filename, CurrProjects: String; Updating: Boolean): Boolean;
var Func: TUpdateCodeTools;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    if Updating then
      @Func := GetProcAddress(Handles[i], 'pftUpdatingCodeInspector')
    else
      @Func := GetProcAddress(Handles[i], 'pftUpdatedCodeInspector');

    if @Func <> nil then begin
      case Func(PChar(Lang), PChar(Filename), PChar(CurrProjects))  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_OutputDblClick(ItemIndex: Integer): Boolean;
var Func: TOutputEvent;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftOutputDoubleClick');

    if @Func <> nil then begin
      case Func(ItemIndex)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

function Plugin_OutputPopup(ItemIndex: Integer): Boolean;
var Func: TOutputEvent;
    i: integer;
    Handles: TIntegerArray;
begin
  Result := True;

  Handles := GetDLLHandles;
  for i := 0 to High(Handles) do begin
    @Func := GetProcAddress(Handles[i], 'pftOutputPopup');

    if @Func <> nil then begin
      case Func(ItemIndex)  of
        PLUGIN_HANDLED: Result := False;
        PLUGIN_STOP: begin
          Result := False;
          exit;
        end;
      end;
    end;
  end;
end;

end.
