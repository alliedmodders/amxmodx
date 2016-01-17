[default]
style=fore:clBlack,back:clWhite,size:8,font:Courier,notbold,notitalics,notunderlined,visible,noteolfilled,changeable,nothotspot
WordWrap=0
WordChars=_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
ClearUndoAfterSave=False
EOLMode=0
LineNumbers=False
Gutter=True
CaretPeriod=1024
CaretWidth=1
CaretLineVisible=True
CaretFore=clNone
CaretBack=#E6E6FF
CaretAlpha=0
SelectForeColor=clHighlightText
SelectBackColor=clHighlight
SelectAlpha=256
MarkerForeColor=clWhite
MarkerBackColor=clBtnShadow
FoldMarginHighlightColor=clWhite
FoldMarginColor=clBtnFace
WhitespaceForeColor=clDefault
WhitespaceBackColor=clDefault
ActiveHotspotForeColor=clBlue
ActiveHotspotBackColor=#A8A8FF
ActiveHotspotUnderlined=True
ActiveHotspotSingleLine=False
FoldMarkerType=1
MarkerBookmark=markertype:sciMFullRect,Alpha:256,fore:clWhite,back:clGray
EdgeColumn=100
EdgeMode=1
EdgeColor=clSilver
CodeFolding=True
BraceHighlight=True

[extension]


[null]
lexer=null

style.33=name:LineNumbers,font:Arial
style.34=name:Ok Braces,fore:clYellow,bold
style.35=name:Bad Braces,fore:clRed,bold
style.36=name:Control Chars,back:clSilver
style.37=name:Indent Guide,fore:clGray

CommentBoxStart=/*
CommentBoxEnd=*/
CommentBoxMiddle=*
CommentBlock=//
CommentStreamStart=/*
CommentStreamEnd=*/
AssignmentOperator==
EndOfStatementOperator=;
[XML]
lexer=xml
NumStyleBits=7
keywords.0=name:Keywords::

keywords.5=name:SGML Keywords::ELEMENT DOCTYPE ATTLIST ENTITY NOTATION

style.33=name:LineNumbers
style.34=name:Ok Braces,fore:clYellow,bold
style.35=name:Bad Braces,fore:clRed,bold
style.36=name:Control Chars,back:clSilver
style.37=name:Indent Guide,fore:clGray
style.0=name:Default
style.1=name:Tags,fore:#00D0D0
style.2=name:Unknown Tags,fore:#00D0D0
style.3=name:Attributes,fore:#A0A0C0
style.4=name:Unknown Attributes,fore:#A0A0C0
style.5=name:Numbers,fore:#E00000
style.6=name:Double quoted strings,fore:clLime
style.7=name:Single quoted strings,fore:clLime
style.8=name:Other inside tag,fore:#A000A0
style.9=name:Comment,fore:#909090
style.10=name:Entities,bold
style.11=name:XML short tag end,fore:#A000A0
style.12=name:XML identifier start,fore:#A000A0,bold
style.13=name:XML identifier end,fore:#A000A0,bold
style.17=name:CDATA,fore:clMaroon,back:#FFF0F0,eolfilled
style.18=name:XML Question,fore:#A00000
style.19=name:Unquoted values,fore:clFuchsia
style.21=name:SGML tags <! ... >,fore:#00D0D0
style.22=name:SGML command,fore:#00A0A0,bold
style.23=name:SGML 1st param,fore:#0FFFF0
style.24=name:SGML double string,fore:clLime
style.25=name:SGML single string,fore:clLime
style.26=name:SGML error,fore:clRed
style.27=name:SGML special,fore:#3366FF
style.28=name:SGML entity,bold
style.29=name:SGML comment,fore:#909090
style.31=name:SGML block,fore:#000066,back:#CCCCE0

CommentBoxStart=<!--
CommentBoxEnd=-->
CommentBoxMiddle=
CommentBlock=//
CommentStreamStart=<!--
CommentStreamEnd=-->
AssignmentOperator==
EndOfStatementOperator=;
[HTML]
lexer=hypertext
NumStyleBits=7
keywords.0=name:HyperText::a abbr acronym address applet area b base basefont bdo big blockquote body br button caption center\
cite code col colgroup dd del dfn dir div dl dt em fieldset font form frame frameset\
h1 h2 h3 h4 h5 h6 head hr html i iframe img input ins isindex kbd label\
legend li link map menu meta noframes noscript object ol optgroup option p param pre q s\
samp script select small span strike strong style sub sup table tbody td textarea tfoot th thead\
title tr tt u ul var xml xmlns abbr accept-charset accept accesskey action align alink alt archive\
axis background bgcolor border cellpadding cellspacing char charoff charset checked cite class classid clear codebase codetype color\
cols colspan compact content coords data datafld dataformatas datapagesize datasrc datetime declare defer dir disabled enctype event\
face for frame frameborder headers height href hreflang hspace http-equiv id ismap label lang language leftmargin link\
longdesc marginwidth marginheight maxlength media method multiple name nohref noresize noshade nowrap object onblur onchange onclick ondblclick\
onfocus onkeydown onkeypress onkeyup onload onmousedown onmousemove onmouseover onmouseout onmouseup onreset onselect onsubmit onunload profile prompt readonly\
rel rev rows rowspan rules scheme scope selected shape size span src standby start style summary tabindex\
target text title topmargin type usemap valign value valuetype version vlink vspace width text password checkbox radio\
submit reset file hidden image framespacing scrolling allowtransparency bordercolor

keywords.1=name:JavaScript::abstract boolean break byte case catch char class const continue debugger default delete do double else enum\
export extends final finally float for function goto if implements import in instanceof int interface long native\
new package private protected public return short static super switch synchronized this throw throws transient try typeof\
var void volatile while with

keywords.2=name:VBScript::and begin case call class continue do each else elseif end erase error event exit false for\
function get gosub goto if implement in load loop lset me mid new next not nothing on\
or property raiseevent rem resume return rset select set stop sub then to true unload until wend\
while with withevents attribute alias as boolean byref byte byval const compare currency date declare dim double\
enum explicit friend global integer let lib long module object option optional preserve private public redim single\
static string type variant

keywords.3=name:Python::and assert break class continue def del elif else except exec finally for from global if import\
in is lambda None not or pass print raise return try while yield

keywords.4=name:PHP::and argv as argc break case cfunction class continue declare default do die echo else elseif empty\
enddeclare endfor endforeach endif endswitch endwhile e_all e_parse e_error e_warning eval exit extends false for foreach function\
global http_cookie_vars http_get_vars http_post_vars http_post_files http_env_vars http_server_vars if include include_once list new not null old_function or parent\
php_os php_self php_version print require require_once return static switch stdclass this true var xor virtual while __file__\
__line__ __sleep __wakeup

keywords.5=name:DTD Keywords::ELEMENT DOCTYPE ATTLIST ENTITY NOTATION

style.33=name:LineNumbers
style.34=name:Ok Braces,fore:clBlue,bold
style.35=name:Bad Braces,fore:clRed,bold
style.36=name:Control Chars,back:clSilver
style.37=name:Indent Guide,fore:clGray
style.0=name:Text
style.1=name:Tags,bold
style.2=name:Unknown Tags,fore:clOlive
style.3=name:Attributes,fore:#A0A0C0
style.4=name:Unknown Attributes,fore:clRed
style.5=name:Numbers,fore:clBlue
style.6=name:Double quoted strings,fore:#AA9900
style.7=name:Single quoted strings,fore:clLime
style.8=name:Other inside tag
style.9=name:Comment,fore:#FF8000
style.10=name:Entities,fore:#A0A0A0,font:Times New Roman,bold
style.11=name:XML short tag end,fore:#00C0C0
style.12=name:XML identifier start,fore:#A000A0
style.13=name:XML identifier end,fore:#A000A0
style.14=name:SCRIPT,fore:#000A0A
style.15=name:ASP <% ... %>,fore:clYellow
style.16=name:ASP <% ... %>,fore:clYellow
style.17=name:CDATA,fore:#FFDF00
style.18=name:PHP,fore:#FF8951
style.19=name:Unquoted values,fore:clFuchsia
style.20=name:XC Comment
style.21=name:SGML tags <! ... >,fore:#00D0D0
style.22=name:SGML command,fore:#00A0A0,bold
style.23=name:SGML 1st param,fore:#0FFFF0
style.24=name:SGML double string,fore:clLime
style.25=name:SGML single string,fore:clLime
style.26=name:SGML error,fore:clRed
style.27=name:SGML special,fore:#3366FF
style.28=name:SGML entity
style.29=name:SGML comment,fore:#909090
style.31=name:SGML block,fore:clBlue
style.40=name:JS Start,fore:#7F7F00
style.41=name:JS Default,bold,eolfilled
style.42=name:JS Comment,fore:#909090,eolfilled
style.43=name:JS Line Comment,fore:#909090
style.44=name:JS Doc Comment,fore:#909090,bold,eolfilled
style.45=name:JS Number,fore:#E00000
style.46=name:JS Word,fore:#00CCCC
style.47=name:JS Keyword,fore:clOlive,bold
style.48=name:JS Double quoted string,fore:clLime
style.49=name:JS Single quoted string,fore:clLime
style.50=name:JS Symbols
style.51=name:JS EOL,fore:clWhite,back:#202020,eolfilled
style.52=name:JS Regex,fore:#C032FF
style.55=name:ASP JS Start,fore:#7F7F00
style.56=name:ASP JS Default,bold,eolfilled
style.57=name:ASP JS Comment,fore:#909090,eolfilled
style.58=name:ASP JS Line Comment,fore:#909090
style.59=name:ASP JS Doc Comment,fore:#909090,bold,eolfilled
style.60=name:ASP JS Number,fore:#E00000
style.61=name:ASP JS Word,fore:#E0E0E0
style.62=name:ASP JS Keyword,fore:clOlive,bold
style.63=name:ASP JS Double quoted string,fore:clLime
style.64=name:ASP JS Single quoted string,fore:clLime
style.65=name:ASP JS Symbols
style.66=name:ASP JS EOL,fore:clWhite,back:#202020,eolfilled
style.67=name:ASP JS Regex,fore:#C032FF
style.71=name:VBS Default,eolfilled
style.72=name:VBS Comment,fore:#909090,eolfilled
style.73=name:VBS Number,fore:#E00000,eolfilled
style.74=name:VBS KeyWord,fore:clOlive,bold,eolfilled
style.75=name:VBS String,fore:clLime,eolfilled
style.76=name:VBS Identifier,fore:clSilver,eolfilled
style.77=name:VBS Unterminated string,fore:clWhite,back:#202020,eolfilled
style.81=name:ASP Default,eolfilled
style.82=name:ASP Comment,fore:#909090,eolfilled
style.83=name:ASP Number,fore:#E00000,eolfilled
style.84=name:ASP KeyWord,fore:clOlive,bold,eolfilled
style.85=name:ASP String,fore:clLime,eolfilled
style.86=name:ASP Identifier,fore:clSilver,eolfilled
style.87=name:ASP Unterminated string,fore:clWhite,back:#202020,eolfilled
style.90=name:Python Start,fore:clGray
style.91=name:Python Default,fore:clGray,eolfilled
style.92=name:Python Comment,fore:#909090,eolfilled
style.93=name:Python Number,fore:#E00000,eolfilled
style.94=name:Python String,fore:clLime,eolfilled
style.95=name:Python Single quoted string,fore:clLime,font:Courier New,eolfilled
style.96=name:Python Keyword,fore:clOlive,bold,eolfilled
style.97=name:Python Triple quotes,fore:#7F0000,back:#EFFFEF,eolfilled
style.98=name:Python Triple double quotes,fore:#7F0000,back:#EFFFEF,eolfilled
style.99=name:Python Class name definition,fore:clBlue,back:#EFFFEF,bold,eolfilled
style.100=name:Python function or method name definition,fore:#007F7F,back:#EFFFEF,bold,eolfilled
style.101=name:Python function or method name definition,back:#EFFFEF,bold,eolfilled
style.102=name:Python Identifiers,back:#EFFFEF,eolfilled
style.104=name:PHP Complex Variable,fore:#00A0A0,italics
style.105=name:ASP Python Start,fore:clGray
style.106=name:ASP Python Default,fore:clGray,eolfilled
style.107=name:ASP Python Comment,fore:#909090,eolfilled
style.108=name:ASP Python Number,fore:#E00000,eolfilled
style.109=name:ASP Python String,fore:clLime,font:Courier New,eolfilled
style.110=name:ASP Python Single quoted string,fore:clLime,eolfilled
style.111=name:ASP Python Keyword,fore:clOlive,bold,eolfilled
style.112=name:ASP Python Triple quotes,fore:#7F0000,back:#CFEFCF,eolfilled
style.113=name:ASP Python Triple double quotes,fore:#7F0000,back:#CFEFCF,eolfilled
style.114=name:ASP Python Class name definition,fore:clBlue,back:#CFEFCF,bold,eolfilled
style.115=name:ASP Python function or method name definition,fore:#007F7F,back:#EFFFEF,bold,eolfilled
style.116=name:ASP Python function or method name definition,back:#CFEFCF,bold,eolfilled
style.117=name:ASP Python Identifiers,fore:clSilver,back:#CFEFCF,eolfilled
style.118=name:PHP Default,eolfilled
style.119=name:PHP Double quoted string,fore:clLime
style.120=name:PHP Single quoted string,fore:clLime
style.121=name:PHP Keyword,fore:clOlive,bold
style.122=name:PHP Number,fore:#E00000
style.123=name:PHP Variable,fore:#00A0A0,italics
style.124=name:PHP Comment,fore:#909090
style.125=name:PHP One line Comment,fore:#909090
style.126=name:PHP Variable in double quoted string,fore:#00A0A0,italics
style.127=name:PHP operator,fore:clSilver

CommentBoxStart=<!--
CommentBoxEnd=-->
CommentBoxMiddle=
CommentBlock=//
CommentStreamStart=<!--
CommentStreamEnd=-->
AssignmentOperator==
EndOfStatementOperator=;
[C++]
lexer=cpp
keywords.0=name:Primary keywords and identifiers::__asm _asm asm auto __automated bool break case catch __cdecl _cdecl cdecl char class __classid __closure const\
const_cast continue __declspec default delete __dispid do double dynamic_cast else enum __except explicit __export export extern false\
__fastcall _fastcall __finally float for friend goto if __import _import __inline inline int __int16 __int32 __int64 __int8\
long __msfastcall __msreturn mutable namespace new __pascal _pascal pascal private __property protected public __published register reinterpret_cast return\
__rtti short signed sizeof static_cast static __stdcall _stdcall struct switch template this __thread throw true __try try\
typedef typeid typename union unsigned using virtual void volatile wchar_t while dllexport dllimport naked noreturn nothrow novtable\
property selectany thread uuid

keywords.1=name:Secondary keywords and identifiers::TStream TFileStream TMemoryStream TBlobStream TOleStream TStrings TStringList AnsiString String WideString cout cin cerr endl fstream ostream istream\
wstring string deque list vector set multiset bitset map multimap stack queue priority_queue

keywords.2=name:Doc Comments::a addindex addtogroup anchor arg attention author b brief bug c class code date def defgroup deprecated\
dontinclude e em endcode endhtmlonly endif endlatexonly endlink endverbatim enum example exception f$ f[ f] file fn\
hideinitializer htmlinclude htmlonly if image include ingroup internal invariant interface latexonly li line link mainpage name namespace\
nosubgrouping note overload p page par param post pre ref relates remarks return retval sa section see\
showinitializer since skip skipline struct subsection test throw todo typedef union until var verbatim verbinclude version warning\
weakgroup $ @ < > \ & # { }

keywords.3=name:Unused::

keywords.4=name:Global classes and typedefs::LOL

style.33=name:LineNumbers
style.34=name:Ok Braces,fore:#0000BB
style.35=name:Bad Braces,fore:clRed
style.36=name:Control Chars,fore:clGray
style.37=name:Indent Guide,fore:clGray
style.0=name:White space,fore:#0000BB,font:Courier New
style.1=name:Comment,fore:#FF8040
style.2=name:Line Comment,fore:#FF8040
style.3=name:Doc Comment,fore:#FF8040
style.4=name:Number,fore:clNavy
style.5=name:Keyword,fore:#007700
style.6=name:Double quoted string,fore:clRed
style.7=name:Single quoted string,fore:clRed
style.8=name:Symbols/UUID,fore:clRed
style.9=name:Preprocessor,fore:#FF8000
style.10=name:Operators,fore:#007700
style.11=name:Identifier,fore:clNavy
style.12=name:EOL if string is not closed,fore:clRed,eolfilled
style.13=name:Verbatim strings for C#,fore:clLime
style.14=name:Regular expressions,fore:clHotLight
style.15=name:Doc Comment Line,fore:#FF8040
style.16=name:User-defined keywords,fore:clRed
style.17=name:Comment keyword,fore:#FF8000
style.18=name:Comment keyword error,fore:clRed
style.19=name:Global classes and typedefs,fore:clGreen

CommentBoxStart=/*
CommentBoxEnd=*/
CommentBoxMiddle=*
CommentBlock=//
CommentStreamStart=/*
CommentStreamEnd=*/
AssignmentOperator==
EndOfStatementOperator=;
[SQL]
lexer=mssql
keywords.0=name:Statements::

keywords.1=name:Data Types::

keywords.2=name:System tables::

keywords.3=name:Global variables::

keywords.4=name:Functions::

keywords.5=name:System Stored Procedures::

keywords.6=name:Operators::

style.33=name:LineNumbers,font:Arial
style.34=name:Ok Braces,fore:clYellow,bold
style.35=name:Bad Braces,fore:clRed,bold
style.36=name:Control Chars,back:clSilver
style.37=name:Indent Guide,fore:clGray
style.0=name:Default,fore:clSilver
style.1=name:Comment,fore:#909090
style.2=name:Line Comment,fore:#909090
style.3=name:Number,fore:#E00000
style.4=name:String,fore:clLime
style.5=name:Operator,fore:clSilver
style.6=name:Identifier,fore:clSilver
style.7=name:Variable
style.8=name:Column Name
style.9=name:Statement
style.10=name:Data Type
style.11=name:System Table
style.12=name:Global Variable
style.13=name:Function
style.14=name:Stored Procedure
style.15=name:Default Pref Datatype
style.16=name:Column Name 2

CommentBoxStart=/*
CommentBoxEnd=*/
CommentBoxMiddle=*
CommentBlock=#
CommentStreamStart=/*
CommentStreamEnd=*/
AssignmentOperator==
EndOfStatementOperator=;
[Pawn]
lexer=cpp
keywords.0=name:Primary keywords and identifiers::assert char #assert const break de ned #de ne enum case sizeof #else forward continue tagof #emit\
native default #endif new do #endinput operator else #endscript public exit #error static for # le stock\
goto #if if #include return #line sleep #pragma state #section switch #tryinclude while #undef Float

keywords.1=name:Secondary keywords and identifiers:: heapspace funcidx numargs getarg setarg strlen tolower toupper swapchars random min max clamp power sqroot time\
date tickcount abs float floatstr floatmul floatdiv floatadd floatsub floatfract floatround floatcmp floatsqroot floatpower floatlog floatsin floatcos\
floattan floatsinh floatcosh floattanh floatabs floatatan floatacos floatasin floatatan2 contain containi replace add format formatex vformat vdformat\
format_args num_to_str str_to_num float_to_str str_to_float equal equali copy copyc setc parse strtok strbreak trim strtolower strtoupper ucfirst\
isdigit isalpha isspace isalnum strcat strfind strcmp is_str_num split remove_filepath replace_all read_dir read_file write_file delete_file file_exists rename_file\
dir_exists file_size fopen fclose fread fread_blocks fread_raw fwrite fwrite_blocks fwrite_raw feof fgets fputs fprintf fseek ftell fgetc\
fputc fungetc filesize rmdir unlink open_dir next_file close_dir get_vaultdata set_vaultdata remove_vaultdata vaultdata_exists get_langsnum get_lang register_dictionary lang_exists CreateLangKey\
GetLangTransKey AddTranslation message_begin message_end write_byte write_char write_short write_long write_entity write_angle write_coord write_string emessage_begin emessage_end ewrite_byte ewrite_char ewrite_short\
ewrite_long ewrite_entity ewrite_angle ewrite_coord ewrite_string set_msg_block get_msg_block register_message get_msg_args get_msg_argtype get_msg_arg_int get_msg_arg_float get_msg_arg_string set_msg_arg_int set_msg_arg_float set_msg_arg_string get_msg_origin\
get_distance get_distance_f velocity_by_aim vector_to_angle angle_vector vector_length vector_distance IVecFVec FVecIVec SortIntegers SortFloats SortStrings SortCustom1D SortCustom2D plugin_init plugin_pause plugin_unpause\
server_changelevel plugin_cfg plugin_end plugin_log plugin_precache client_infochanged client_connect client_authorized client_disconnect client_command client_putinserver register_plugin precache_model precache_sound precache_generic set_user_info get_user_info\
set_localinfo get_localinfo show_motd client_print engclient_print console_print console_cmd register_event register_logevent set_hudmessage show_hudmessage show_menu read_data read_datanum read_logdata read_logargc read_logargv\
parse_loguser server_print is_map_valid is_user_bot is_user_hltv is_user_connected is_user_connecting is_user_alive is_dedicated_server is_linux_server is_jit_enabled get_amxx_verstring get_user_attacker get_user_aiming get_user_frags get_user_armor get_user_deaths\
get_user_health get_user_index get_user_ip user_has_weapon get_user_weapon get_user_ammo num_to_word get_user_team get_user_time get_user_ping get_user_origin get_user_weapons get_weaponname get_user_name get_user_authid get_user_userid user_slap\
user_kill log_amx log_message log_to_file get_playersnum get_players read_argv read_args read_argc read_flags get_flags find_player remove_quotes client_cmd engclient_cmd server_cmd set_cvar_string\
cvar_exists remove_cvar_flags set_cvar_flags get_cvar_flags set_cvar_float get_cvar_float get_cvar_num set_cvar_num get_cvar_string get_mapname get_timeleft get_gametime get_maxplayers get_modname get_time format_time get_systime\
parse_time set_task remove_task change_task task_exists set_user_flags get_user_flags remove_user_flags register_clcmd register_concmd register_srvcmd get_clcmd get_clcmdsnum get_srvcmd get_srvcmdsnum get_concmd get_concmd_plid\
get_concmdsnum get_plugins_cvarsnum get_plugins_cvar register_menuid register_menucmd get_user_menu server_exec emit_sound register_cvar random_float random_num get_user_msgid get_user_msgname xvar_exists get_xvar_id get_xvar_num get_xvar_float\
set_xvar_num set_xvar_float is_module_loaded get_module get_modulesnum is_plugin_loaded get_plugin get_pluginsnum pause unpause callfunc_begin callfunc_begin_i get_func_id callfunc_push_int callfunc_push_float callfunc_push_intrf callfunc_push_floatrf\
callfunc_push_str callfunc_push_array callfunc_end inconsistent_file force_unmodified md5 md5_file plugin_flags plugin_modules require_module is_amd64_server mkdir find_plugin_byfile plugin_natives register_native register_library log_error\
param_convert get_string set_string get_param get_param_f get_param_byref get_float_byref set_param_byref set_float_byref get_array get_array_f set_array set_array_f menu_create menu_makecallback menu_additem menu_pages\
menu_items menu_display menu_find_id menu_item_getinfo menu_item_setname menu_item_setcmd menu_item_setcall menu_destroy player_menu_info menu_addblank menu_setprop menu_cancel query_client_cvar set_error_filter dbg_trace_begin dbg_trace_next dbg_trace_info\
dbg_fmt_error set_native_filter set_module_filter abort module_exists LibraryExists next_hudchannel CreateHudSyncObj ShowSyncHudMsg ClearSyncHud int3 set_fail_state get_var_addr get_addr_val set_addr_val CreateMultiForward CreateOneForward\
PrepareArray ExecuteForward DestroyForward get_cvar_pointer get_pcvar_flags set_pcvar_flags get_pcvar_num set_pcvar_num get_pcvar_float set_pcvar_float get_pcvar_string arrayset get_weaponid dod_make_deathmsg user_silentkill make_deathmsg is_user_admin\
cmd_access access cmd_target show_activity colored_menus cstrike_running is_running get_basedir get_configsdir get_datadir register_menu get_customdir AddMenuItem AddClientMenuItem AddMenuItem_call constraint_offset

keywords.2=name:Doc Comments::a addindex addtogroup anchor arg attention author b brief bug c class code date def defgroup deprecated\
dontinclude e em endcode endhtmlonly endif endlatexonly endlink endverbatim enum example exception f$ f[ f] file fn\
hideinitializer htmlinclude htmlonly if image include ingroup internal invariant interface latexonly li line link mainpage name namespace\
nosubgrouping note overload p page par param post pre ref relates remarks return retval sa section see\
showinitializer since skip skipline struct subsection test throw todo typedef union until var verbatim verbinclude version warning\
weakgroup $ @ < > \ & # { }

keywords.3=name:Unused::

style.33=name:LineNumbers
style.34=name:Ok Braces,fore:#0000BB
style.35=name:Bad Braces,fore:clRed
style.36=name:Control Chars,fore:clGray
style.37=name:Indent Guide,fore:clGray
style.0=name:White space,fore:#0000BB,font:Courier New
style.1=name:Comment,fore:#FF8040
style.2=name:Line Comment,fore:#FF8040
style.3=name:Doc Comment,fore:#FF8040
style.4=name:Number,fore:clNavy
style.5=name:Keyword,fore:#007700
style.6=name:Double quoted string,fore:clRed
style.7=name:Single quoted string,fore:clRed
style.8=name:Symbols/UUID,fore:clRed
style.9=name:Preprocessor,fore:#FF8000
style.10=name:Operators,fore:#007700
style.11=name:Identifier,fore:clNavy
style.12=name:EOL if string is not closed,fore:clRed,eolfilled
style.13=name:Verbatim strings for C#,fore:clLime
style.14=name:Regular expressions,fore:clHotLight
style.15=name:Doc Comment Line,fore:#FF8040
style.16=name:User-defined keywords,fore:clRed

CommentBoxStart=/*
CommentBoxEnd=*/
CommentBoxMiddle=*
CommentBlock=//
CommentStreamStart=/*
CommentStreamEnd=*/
AssignmentOperator==
EndOfStatementOperator=;
[other]
BookMarkBackColor=clGray
BookMarkForeColor=clWhite
BookMarkPixmapFile=
Unicode=False
