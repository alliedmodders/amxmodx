unit studioapi;

interface

uses SysUtils, Windows, Messages;

type TLoadInfo = record
  { Plugin values }
  sPluginName: PChar;
  sPluginDescription: PChar;
  { Form Handles }
  hAllFilesForm: PHandle;
  hAutoIndent: PHandle;
  hClose: PHandle;
  hConnGen: PHandle;
  hGoToLine: PHandle;
  hHTMLPreview: PHandle;
  hHudMsgGenerator: PHandle;
  hInfo: PHandle;
  hIRCPaster: PHandle;
  hMainForm: PHandle;
  hMenuGenerator: PHandle;
  hMOTDGen: PHandle;
  hPluginsIniEditor: PHandle;
  hReplace: PHandle;
  hSearch: PHandle;
  hSelectColor: PHandle;
  hSettings: PHandle;
  hSocketsTerminal: PHandle;
  { Important Control Handles }
  hOutput: PHandle;
  hCodeExplorer: PHandle;
  hCodeInspector: PHandle; // even if it won't be useful
  hNotes: PHandle;
  { Other }
  pApplication: Pointer; // this is only useful for Delphi developers
end;

type PLoadInfo = ^TLoadInfo;
     TLoadPlugin = procedure (LoadInfo: PLoadInfo); cdecl;
     TUnloadPlugin = procedure; cdecl;

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

const SCM_SHOWPROGRESS = WM_USER + $100;
      SCM_HIDEPROGRESS = WM_USER + $101;
      SCM_UPDATEPROGRESS = WM_USER + $102;
      SCM_LOADCODESNIPPETS = WM_USER + $103;
      SCM_CODESNIPPETCLICK = WM_USER + $104;
      SCM_MIRC_CMD = 	WM_USER + $105;
      SCM_RELOADINI = WM_USER + $106;
      SCM_SELECTLANGUAGE = WM_USER + $107;
      SCM_LOADFILE = WM_USER + $108;
      SCM_CURRPROJECTS = WM_USER + $109;
      SCM_COMPILE = WM_USER + $110;
      SCM_COMPILE_UPLOAD = WM_USER + $111;
      SCM_COMPILE_STARTHL = WM_USER + $112;
      SCM_MENU_LOADIMAGE = WM_USER + $113;
      SCM_MENU_ADDITEM = WM_USER + $114;
      SCM_MENU_ADDSUBITEM = WM_USER + $115;
      SCM_MENU_FAKECLICK = WM_USER + $116;
      SCM_MENU_SHOWITEM = WM_USER + $117;
      SCM_MENU_HIDEITEM = WM_USER + $118;
      SCM_PLUGIN_LOAD = 	WM_USER + $119;
      SCM_PLUGIN_UNLOAD = WM_USER + $120;
      SCM_SETTINGS_CREATEPAGE = WM_USER + $121;
      SCM_CODEINSPECTOR_CLEAR = WM_USER + $122;
      SCM_CODEINSPECTOR_ADD = WM_USER + $123;
      SCM_CODEINSPECTOR_ADDCOMBO = WM_USER + $124;
      SCM_CODEINSPECTOR_SETVALUE = WM_USER + $125;
      SCM_CODEINSPECTOR_SETNAME = WM_USER + $126;
      SCM_CODEINSPECTOR_GETVALUE = WM_USER + $127;
      SCM_CODEINSPECTOR_GETNAME = WM_USER + $128;
      SCM_CODEINSPECTOR_COUNT = WM_USER + $129;
      SCM_CODEINSPECTOR_BEGINUPDATE	= WM_USER + $130;
      SCM_CODEINSPECTOR_ENDUPDATE = WM_USER + $131;
      SCM_CODEINSPECTOR_DELETE = WM_USER + $132;

      SCM_PAWN_NEWFILE = WM_USER + $133;
      SCM_PAWN_SAVEFILE = WM_USER + $134;
      SCM_PAWN_CLOSEFILE = WM_USER + $135;
      SCM_PAWN_ISUNTITLED = WM_USER + $136;
      SCM_PAWN_ACTIVATE = WM_USER + $137;
      SCM_PAWN_ACTIVATEDOC = WM_USER + $138;
      SCM_PAWN_GETNOTES = WM_USER + $139;
      SCM_PAWN_SETNOTES = WM_USER + $140;
      SCM_PAWN_GETFILENAME = WM_USER + $141;
      SCM_PAWN_SETFILENAME = WM_USER + $142;
      SCM_PAWN_GETTEXT = WM_USER + $143;
      SCM_PAWN_SETTEXT = WM_USER + $144;

      SCM_CPP_NEWFILE = WM_USER + $145;
      SCM_CPP_SAVEFILE = WM_USER + $146;
      SCM_CPP_CLOSEFILE = WM_USER + $147;
      SCM_CPP_ISUNTITLED = WM_USER + $148;
      SCM_CPP_ACTIVATE = WM_USER + $149;
      SCM_CPP_ACTIVATEDOC = WM_USER + $150;
      SCM_CPP_ACTIVATEIDE = WM_USER + $151;
      SCM_CPP_GETNOTES = WM_USER + $152;
      SCM_CPP_SETNOTES = WM_USER + $153;
      SCM_CPP_GETFILENAME = WM_USER + $154;
      SCM_CPP_SETFILENAME = WM_USER + $155;
      SCM_CPP_GETTEXT = 	WM_USER + $156;
      SCM_CPP_SETTEXT = 	WM_USER + $157;

      SCM_OTHER_NEWFILE = WM_USER + $158;
      SCM_OTHER_SAVEFILE = WM_USER + $159;
      SCM_OTHER_CLOSEFILE = WM_USER + $160;
      SCM_OTHER_ISUNTITLED = WM_USER + $161;
      SCM_OTHER_ACTIVATE = WM_USER + $162;
      SCM_OTHER_ACTIVATEDOC = WM_USER + $163;
      SCM_OTHER_GETNOTES = WM_USER + $164;
      SCM_OTHER_SETNOTES = WM_USER + $165;
      SCM_OTHER_GETFILENAME = WM_USER + $166;
      SCM_OTHER_SETFILENAME = WM_USER + $167;
      SCM_OTHER_GETTEXT = WM_USER + $168;
      SCM_OTHER_SETTEXT = WM_USER + $169;

      SCM_OUTPUT_SHOW = WM_USER + $170;
      SCM_OUTPUT_HIDE = WM_USER + $171;
      SCM_OUTPUT_ADD = 	WM_USER + $172;
      SCM_OUTPUT_CLEAR = WM_USER + $173;
      SCM_OUTPUT_DELETE = WM_USER + $174;
      SCM_OUTPUT_GETTEXT = WM_USER + $175;
      SCM_OUTPUT_GETITEM = WM_USER + $176;
      SCM_OUTPUT_INDEXOF = WM_USER + $177;
      SCM_ACTIVE_DOCUMENT = WM_USER + $178;
      SCM_ACTIVE_PROJECTS = WM_USER + $179;
      SCM_EDITOR_SETTEXT = WM_USER + $180;
      SCM_EDITOR_GETTEXT = WM_USER + $181;
      SCM_EDTIOR_SETCALLTIPS = WM_USER + $182;
      SCM_EDITOR_SHOWCALLTIP = WM_USER + $183;
      SCM_EDITOR_SETAUTOCOMPLETE = WM_USER + $184;
      SCM_EDITOR_SHOWAUTOCOMPLETE = WM_USER + $185;
      SCM_EDITOR_GETSELSTART = WM_USER + $186;
      SCM_EDITOR_GETSELLENGTH = WM_USER + $187;
      SCM_EDITOR_SETSELSTART = WM_USER + $188;
      SCM_EDITOR_SETSELLENGH = WM_USER + $189;

      SCM_REMOVE_MENUITEM = WM_USER + $190;
      SCM_REMOVE_IMAGE = WM_USER + $191;
      SCM_SETTHEME = WM_USER + $192;
      SCM_GETTHEME = WM_USER + $193;
      
function SendStudioMsg(eMessage: Integer; eData: String; eIntData: Integer): Integer;

implementation

function SendStudioMsg(eMessage: Integer; eData: String; eIntData: Integer): Integer;
var eStudioHandle: HWND;
    eCopyDataStruct: TCopyDataStruct;
begin
  with eCopyDataStruct do begin
    dwData := eIntData;  
    cbData := Length(eData) + 1;
    lpData := PChar(eData);  
  end;  

  eStudioHandle := FindWindow('TfrmMain', 'AMXX-Studio');
  if eStudioHandle <> 0 then
    Result := SendMessage(eStudioHandle, WM_COPYDATA, eMessage, LongInt(@eCopyDataStruct))
  else
    Result := 0;
end;

end.
