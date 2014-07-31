var SMfunctions=new Array()
var SMfiles=new Array()
var SMfiledata=new Array()
SMfiles[0] = "amxconst"
SMfiles[1] = "amxmisc"
SMfiles[2] = "amxmodx"
SMfiles[3] = "amxmodx_version"
SMfiles[4] = "cellarray"
SMfiles[5] = "celltrie"
SMfiles[6] = "core"
SMfiles[7] = "csstats"
SMfiles[8] = "cstrike"
SMfiles[9] = "csx"
SMfiles[10] = "datapack"
SMfiles[11] = "dbi"
SMfiles[12] = "dodconst"
SMfiles[13] = "dodfun"
SMfiles[14] = "dodstats"
SMfiles[15] = "dodx"
SMfiles[16] = "engine"
SMfiles[17] = "engine_const"
SMfiles[18] = "engine_stocks"
SMfiles[19] = "esf"
SMfiles[20] = "esf_const"
SMfiles[21] = "fakemeta"
SMfiles[22] = "fakemeta_const"
SMfiles[23] = "fakemeta_stocks"
SMfiles[24] = "fakemeta_util"
SMfiles[25] = "file"
SMfiles[26] = "float"
SMfiles[27] = "fun"
SMfiles[28] = "geoip"
SMfiles[29] = "hamsandwich"
SMfiles[30] = "ham_const"
SMfiles[31] = "hlsdk_const"
SMfiles[32] = "lang"
SMfiles[33] = "messages"
SMfiles[34] = "message_const"
SMfiles[35] = "message_stocks"
SMfiles[36] = "newmenus"
SMfiles[37] = "ns"
SMfiles[38] = "ns_const"
SMfiles[39] = "nvault"
SMfiles[40] = "regex"
SMfiles[41] = "sockets"
SMfiles[42] = "sorting"
SMfiles[43] = "sqlx"
SMfiles[44] = "string"
SMfiles[45] = "tfcconst"
SMfiles[46] = "tfcstats"
SMfiles[47] = "tfcx"
SMfiles[48] = "time"
SMfiles[49] = "tsconst"
SMfiles[50] = "tsfun"
SMfiles[51] = "tsstats"
SMfiles[52] = "tsx"
SMfiles[53] = "vault"
SMfiles[54] = "vector"
SMfiles[55] = "xs"
SMfunctions[0] = Array ("show_activity","Standard method to show activity to clients connected to the server.
This depends on the amx_show_activity cvar.  See documentation for more details.");
SMfunctions[1] = Array ("show_activity_id","Standard method to show activity to one single client.
This is useful for messages that get pieced together by many language keys.
This depends on the amx_show_activity cvar.  See documentation for more details.");
SMfunctions[2] = Array ("show_activity_key","Standard method to show activity to one single client with normal language keys.
These keys need to be in the format of standard AMXX keys:
  eg: ADMIN_KICK_1 = ADMIN: kick %s
      ADMIN_KICK_2 = ADMIN %s: kick %s
This depends on the amx_show_activity cvar.  See documentation for more details.");
SMfunctions[3] = Array ("AddMenuItem","MENU_TEXT: Text that will be shown for this item in menu
MENU_CMD: Command that should be executed to start menu
MENU_ACCESS: Access required for menu
MENU_PLUGIN: The exact case-insensitive name of plugin holding the menu command");
SMfunctions[4] = Array ("AddClientMenuItem","");
SMfunctions[5] = Array ("has_flag","false if they have none");
SMfunctions[6] = Array ("has_all_flags","false otherwise");
SMfunctions[7] = Array ("client_print_color","Sends colored message to player.  Set index to 0 to send text globally.
This works only under Counter-Strike 1.6 and Counter-Strike: Condition Zero.
The available colors identifiers are :
         green       ^4   ; use location color from this point forward
 red/blue/grey       ^3   ; use team color from this point forward
 red/blue/grey       ^2   ; use team color up to the end of the player name. This only works at the start of the string, and precludes using the other control characters.
         normal      ^1   ; use normal color from this point forward
The team color is defined either with a sender\'s index, or a specific team color using print_team_* constants (print_team_blue, print_team_red, print_team_grey).
A message must start with a default color.
An example would be: client_print_color(id, print_team_red, \\&quot;^4This is green ^3this is red, ^1this is your default chat text color\\&quot;);
Another with index : client_print_color(id, idOther, \\&quot;^4This is green ^3this idOther\'s team color, ^1this is your default chat text color\\&quot;);
In multilingual file : KEY = ^1This is normal color, ^4this is green, ^1normal again ^3and now team color.");
SMfunctions[8] = Array ("register_logevent","Examples for conditions:
\\&quot;0=World triggered\\&quot; \\&quot;1=Game_Commencing\\&quot;
\\&quot;1=say\\&quot;
\\&quot;3=Terrorists_Win\\&quot;
\\&quot;1=entered the game\\&quot;
\\&quot;0=Server cvar\\&quot;");
SMfunctions[9] = Array ("set_hudmessage","Sets format for hudmessage.
Note - as of AMX Mod X 1.61, setting the channel to -1
 will automatically choose the next available HUD channel for a player.
Note - if you plan to make a permanent message, don\'t forget to specify a channel (1-4)
 to avoid flickering effect due to auto-channeling.");
SMfunctions[10] = Array ("set_dhudmessage","Sets the Director HUD parameters for drawing text.
These parameters are stored globally, although nothing other than this function and set_hudmessage modify them.
You must call this function before drawing text. If you are drawing text to multiple clients, you can set the
parameters once, since they won\'t be modified. However, as soon as you pass control back to other plugins,
you must reset the parameters next time you draw.");
SMfunctions[11] = Array ("show_dhudmessage","Shows a Director HUD message to a client. Usually such message has a bigger letter size.
Ideally used to show static/persistent message.
Unlike classic HUD message which is channel-based, Director HUD message is stack-based. Don\'t expect same behavior.
You can have up to 8 messages displatying at once. If you try more, the first will be overwritten, then second, etc.
Each message is limited to 128 characters. This will be automatically truncated by the native.
Due to its way for working, there is no native to clear a specific message.");
SMfunctions[12] = Array ("get_user_attacker","case returns index of attacking player. On second and third
parameter you may get info about weapon and body hit place.
As of 1.75, get_user_attacker can return a non-player index if the player was attacked by a non-player entity.");
SMfunctions[13] = Array ("get_user_weapons"," also theirs indexes.
Note that num is incremental - if you pass 0, you get
 32 weapons towards the total.  Afterwards, num will
 will contain the number of weapons retrieved.
However, subsequent calls to get_user_weapons() will
 return the next batch of weapons, in case the mod
 supports more than 32 weapons.
This means to call get_user_weapons() on the same
 inputs twice, you must reset num to 0 to get the
 original output again.");
SMfunctions[14] = Array ("engclient_cmd","This is an emulation of a client command (commands aren\'t send to client!).
It allows to execute some commands on players and bots.
Function is excellent for forcing to do an action related to a game (not settings!).
The command must stand alone but in arguments you can use spaces.");
SMfunctions[15] = Array ("amxclient_cmd","This is an emulation of a client command (commands aren\'t send to client!).
It allows to execute some commands on players and bots.
Function is excellent for forcing to do an action related to a game (not settings!).
The command must stand alone but in arguments you can use spaces.");
SMfunctions[16] = Array ("register_clcmd","Set FlagManager to 1 to make FlagManager always include this command
Set FlagManager to 0 to make FlagManager never include this command
Returns the command ID.");
SMfunctions[17] = Array ("register_concmd","Set FlagManager to 1 to make FlagManager always include this command
Set FlagManager to 0 to make FlagManager never include this command
Returns the command ID.");
SMfunctions[18] = Array ("register_srvcmd","Returns the command ID.");
SMfunctions[19] = Array ("register_cvar","Returns the cvar pointer for get/set_pcvar functions.");
SMfunctions[20] = Array ("is_plugin_loaded","Checks whether a plugin is loaded by the given registered name (such as \\&quot;Admin Base\\&quot;), or, optionally
the given filename (\\&quot;admin.amxx\\&quot;).");
SMfunctions[21] = Array ("get_plugin","Function returns -1 if plugin doesn\'t exist with given index.
Note: the [...] portion should not be used, and is only for backward compatibility.
Use index of -1 to use the calling plugin\'s ID.");
SMfunctions[22] = Array ("pause","In most cases param1 is name of function and
param2 name of plugin (all depends on flags).
Flags:
\\&quot;a\\&quot; - pause whole plugin.
\\&quot;c\\&quot; - look outside the plugin (by given plugin name).
\\&quot;d\\&quot; - set \\&quot;stopped\\&quot; status when pausing whole plugin.
      In this status plugin is unpauseable.
Example: pause(\\&quot;ac\\&quot;,\\&quot;myplugin.amxx\\&quot;)
Note: There used to be the b and e flags as well,
which have been deprecated and are no longer used.");
SMfunctions[23] = Array ("get_func_id","");
SMfunctions[24] = Array ("callfunc_push_int","Note that none of these values are const.
Anything pushed by intrf, floatrf, array, or str
 can be modified by the called function.");
SMfunctions[25] = Array ("callfunc_push_str","Note that this will defy the \'const\' specifier for push_str(),
 which is only kept for special backwards compatibility.");
SMfunctions[26] = Array ("plugin_flags","If hdr is 1, it will return the pcode flags rather than state flags.
Use a plid of -1 to get the flags for the calling plugin.");
SMfunctions[27] = Array ("plugin_modules","");
SMfunctions[28] = Array ("register_native","the handler will be called with two parameters: the calling plugin id, and the
number of parameters.
If you set style=1, the method of parameter passing is a tad more efficient.
Instead of \\&quot;id, numParams\\&quot;, you label the native exactly as how the parameters
 should, in theory, be sent.  Then for each byreference parameter, you call
 param_convert(num).  This is theoretically more efficient but quite hacky.
 The method was discovered by dJeyL, props to him!");
SMfunctions[29] = Array ("register_library","in your include file:
 #pragma reqlib &lt;name&gt;
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib &lt;name&gt;
 #endif");
SMfunctions[30] = Array ("log_error","Acts as if the calling plugin had the error.");
SMfunctions[31] = Array ("set_error_filter","Allows you to trap error messages that occur in your plugin.
You can use this to override the debug messages that occur when your plugin
 causes some sort of runtime error.  Your handler will be called in this style:
public error_filter(error_code, bool:debugging, message[])
   error_code is the AMX_ERR code.  debugging is whether or not the plugin is in debug mode.
   message[] is any message that was sent along with the error.
Return PLUGIN_CONTINUE to let the error pass through the filter.
Return PLUGIN_HANDLED to block the error from displaying.");
SMfunctions[32] = Array ("dbg_trace_begin","Gets a trace handle for the item at the top of the traced call stack.
Returns 0 if no debugging information is available.");
SMfunctions[33] = Array ("dbg_trace_next","Gets the next item in a traced call stack.  Returns 0 if no more traces exist.");
SMfunctions[34] = Array ("dbg_trace_info","Gets the call stack info for a trace.");
SMfunctions[35] = Array ("dbg_fmt_error","Gets the formatted error string, which looks like \\&quot;Run time error X: (description)\\&quot;");
SMfunctions[36] = Array ("set_native_filter","Sets a native filter.  This must be first set in plugin_natives(), but future calls will
 simply set a new filter.
This filter will allow your plugin to load even if its modules aren\'t loaded.  For example,
 if Fun isn\'t loaded and you use set_user_frags, your plugin will still load.  However, if you
 attempt to call this native, your filter will intercept it with these parameters:
public function native_filter(const native[], index, trap)
 native - name of native
 index - index of native
 trap - 0 if native couldn\'t be found, 1 if native use was attempted
If you return PLUGIN_HANDLED, no error is thrown.  If you return PLUGIN_CONTINUE,
 your plugin will have a run-time-error.  To print your own error, or change the default,
 you can return PLUGIN_HANDLED or return PLUGIN_CONTINUE and use set_error_filter.
If you return PLUGIN_CONTINUE when trap is 0, the plugin will ABORT AND FAIL TO LOAD!
When trap is 0, it is unsafe to use natives that modify the server or use other plugins.");
SMfunctions[37] = Array ("set_module_filter","This function sets a module/library filter.  It will let you intercept the automatic requirement
 of a module and return PLUGIN_CONTINUE to fail load or PLUGIN_HANDLED to imply that load
 can continue even without the module.
This is the most unforgiving of the filter functions.  You can ONLY call it during plugin_natives,
 and any error that occurs is not filtered -- instead your plugin will fail to load as if you
 returned PLUGIN_CONTINUE.
Your handler will be called with this prototype:
public module_filter(const library[], LibType:type);
 library - library or class name of the module that is required
 libtype - The type of requirement being checked (library/module or class).
set_module_filter() returns 0 on success (unlike most natives).");
SMfunctions[38] = Array ("abort","Aborts execution of the current callback.  Your script will throw a run time error.
You can also specify an optional message.
You should NOT call this function inside:
 - Error or module filters (native filters are safe if trap is 1)
 - plugin_natives()
Note that the plugin\'s filename is prepending to your message:
 [myplugin.amxx] MESSAGE");
SMfunctions[39] = Array ("module_exists","Checks if a specific module is loaded.  This is the exact same method AMX Mod X
 uses to see if a module is required by a plugin.  For example:
 module_exists(\\&quot;cstrike\\&quot;)
 module_exists(\\&quot;dbi\\&quot;)");
SMfunctions[40] = Array ("LibraryExists","Checks if a library/class is loaded.  This is the newer version of module_exists.");
SMfunctions[41] = Array ("next_hudchannel","Returns the next valid hudchannel for a user, from 1-4.");
SMfunctions[42] = Array ("CreateHudSyncObj","Creates a HUD Synchronization Object.  Create one of these
 for each section of the screen that contains overlapping HUD messages.
For example, if you use both sides of the screen to display three messages
 that can potentially overlap, each side counts as a synchronizable area.
You can then use ShowSyncHudMsg() to correctly synchronize displaying the
 HUD message with any other messages potentially in its class.  Note that this
 does not yet do anything like reserve screen area, its sole purpose is to be
 able to wipe an old message on an auto-channel and ensure that it will not
 clear a message from another plugin.
The parameters are kept blank for future use.");
SMfunctions[43] = Array ("ShowSyncHudMsg","Displays a synchronized HUD message.  This will check that your
 HUD object has its previous display on the screen cleared before
 it proceeds to write another.  It will only do this in the case
 of that channel not having been cleared already.
Target can be 0 for all players or 1-get_maxplayers().
You must use set_hudmessage, although the channel parameter is
 entirely ignored.");
SMfunctions[44] = Array ("ClearSyncHud","Clears the display on a HudSync Object.  This is essentially the same
 thing as: ShowSyncHudMsg(x, y, \\&quot;\\&quot;), except doing that would send
 out two messages and use up another channel.  This re-uses the last
 channel and clears it at the same time.
Note: for this you do not have to use set_hudmessage().
Note: target can be 0 for all players.");
SMfunctions[45] = Array ("CreateMultiForward","Creates a multi-plugin forward.
Stop type must be one of the ET_ values in amxconst.inc
results will be &gt; 0 for success");
SMfunctions[46] = Array ("CreateOneForward","Creates a forward for one plugin.
Results will be &gt; 0 for success.
id should be an id such as returned by find_plugin_byfile.
Unlike get_plugin(), negative numbers will not work.");
SMfunctions[47] = Array ("PrepareArray","prepares an array.  use this and pass the result into
ExecuteForward() instead of the array itself.");
SMfunctions[48] = Array ("ExecuteForward","executes a forward.  returns result in ret.
returns 1 for success, 0 for failure.");
SMfunctions[49] = Array ("DestroyForward","Destroys/deallocates any type of forward");
SMfunctions[50] = Array ("get_cvar_pointer","Get a cvar pointer.  Returns 0 if not found.");
SMfunctions[51] = Array ("arrayset","Sets a whole array to a certain value.");
SMfunctions[52] = Array ("get_weaponid","Returns the weapon id, otherwise 0 when no id found.
The weapon name is case sensitive, and has the weapon_* form.");
SMfunctions[53] = Array ("admins_push","Adds an admin to the dynamic admin storage
for lookup at a later time");
SMfunctions[54] = Array ("admins_num","Gets the number of admins in the dynamic admin
storage list");
SMfunctions[55] = Array ("admins_lookup","Gets information about a dynamically stored admin
Use the enum AdminProp
Returns an integer value: AdminProp_Access, AdminProp_Flags
Sets the buffer string: AdminProp_Auth, AdminProp_Password");
SMfunctions[56] = Array ("admins_flush","Clears the list of dynamically stored admins");
SMfunctions[57] = Array ("has_map_ent_class","Searches whether a map contains at least one entity with the provided class name.");
SMfunctions[58] = Array ("ArrayCreate","Creates a handle to a dynamically sized array.
It is very important that the cellsize you provide matches up with the buffer sizes
that you pass with subsequent Array{Get,Set,Push} calls.");
SMfunctions[59] = Array ("ArrayClone","Clones an array, returning a new handle with the same size and data.
You must close it.");
SMfunctions[60] = Array ("ArrayClear","Clears all entries from the array.");
SMfunctions[61] = Array ("ArraySize","Returns the number of elements in the array.");
SMfunctions[62] = Array ("ArrayResize","Resizes an array.  If the size is smaller than the current size,
the array is truncated.");
SMfunctions[63] = Array ("ArrayGetArray","Returns data within an array.
Make sure the output buffer matches the size the array was created with!");
SMfunctions[64] = Array ("ArrayGetCell","Returns a single cell of data from an array.
Use this only with arrays that were created with a cellsize of 1!");
SMfunctions[65] = Array ("ArrayGetString","Returns a string value from an array.");
SMfunctions[66] = Array ("ArraySetArray","Sets an item\'s data with that of a local buffer.
The buffer size must match what the cellsize that the array was created with!
The item must already exist, use ArrayPushArray to create a new item within the array.");
SMfunctions[67] = Array ("ArraySetCell","Sets an array\'s single cell value.  Use this only on array that were created with a cellsize of 1!
The item must already exist, use ArrayPushCell to create a new item within the array.");
SMfunctions[68] = Array ("ArraySetString","Sets a string value from an array.
The stored string will be truncated if it is longer than the cellsize the array was created with!
The item must already exist, use ArrayPushString to create a new item within the array.");
SMfunctions[69] = Array ("ArrayPushArray","Creates a new item at the end of the array and sets its data with that of a local buffer.
The buffer size must match what the cellsize that the array was created with!");
SMfunctions[70] = Array ("ArrayPushCell","Creates a new item and sets the array\'s single cell value.
Use this only on array that were created with a cellsize of 1!");
SMfunctions[71] = Array ("ArrayPushString","Creates a new element in the array and sets its value to the input buffer.
The stored string will be truncated if it is longer than the cellsize the array was created with!");
SMfunctions[72] = Array ("ArrayInsertArrayAfter","Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
The buffer size must match what the cellsize that the array was created with!");
SMfunctions[73] = Array ("ArrayInsertCellAfter","Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
Use this only on an array that was created with a cellsize of 1!");
SMfunctions[74] = Array ("ArrayInsertStringAfter","Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
The stored string will be truncated if it is longer than the cellsize the array was created with!");
SMfunctions[75] = Array ("ArrayInsertArrayBefore","Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
The buffer size must match what the cellsize that the array was created with!");
SMfunctions[76] = Array ("ArrayInsertCellBefore","Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
Use this only on an array that was created with a cellsize of 1!");
SMfunctions[77] = Array ("ArrayInsertStringBefore","Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
The stored string will be truncated if it is longer than the cellsize the array was created with!");
SMfunctions[78] = Array ("ArraySwap","Swaps the position of two items.");
SMfunctions[79] = Array ("ArrayDeleteItem","Deletes an item from the array.  All items beyond it get shifted down 1 space.");
SMfunctions[80] = Array ("ArrayFindString","Returns the index for the first occurance of the provided string. If the string
cannot be located, -1 will be returned.");
SMfunctions[81] = Array ("ArrayFindValue","Returns the index for the first occurance of the provided value. If the value
cannot be located, -1 will be returned.");
SMfunctions[82] = Array ("ArrayGetStringHandle","Creates a handle that is passable to a format compliant routine for printing as a string (with the %a format option).
It is suggested to pass the function directly as a parameter to the format routine.
The array contents must be a null-terminated string!
An example usage:  client_print(id, print_chat, \\&quot;%a\\&quot;, ArrayGetStringHandle(MessageArray, i));");
SMfunctions[83] = Array ("ArrayDestroy","Destroys the array, and resets the handle to 0 to prevent accidental usage after it is destroyed.");
SMfunctions[84] = Array ("ArraySort","Similar to sorting.inc\'s CustomSort.
The sorting algorithm then uses your comparison function to sort the data.
The function is called in the following manner:
public MySortFunc(Array:array, item1, item2, const data[], data_size)
 array			- Array handle in its current un-sorted state.
 item1, item2	- Current item pair being compared
 data[]			- Extra data array you passed to the sort func.
 data_size		- Size of extra data you passed to the sort func.
Your function should return:
 -1 if item1 should go before item2
  0 if item1 and item2 are equal
  1 if item1 should go after item2
Note that the parameters after item2 are all optional and you do not need to specify them.
Note that unlike the sorting.inc versions, the array passed to the callback is not in mid-sorted state.");
SMfunctions[85] = Array ("ArraySortEx","A faster version of ArraySort.
The sorting algorithm then uses your comparison function to sort the data.
The advantage of this native is that the Array elements being compared are
directly passed into your function, instead of the item indexes that are passed by ArraySort.
This removes the need for calling ArrayGet[Cell|String|Array] every time before being
able to compare the elements.
For Arrays with a cellsize of 1 (used for storing integers and floats),
the function is called in the following manner:
public MySortFunc(Array:array, elem1, elem2, const data[], data_size)
 array			- Array handle in its current un-sorted state.
 elem1, elem2	- Current element pair being compared
 data[]			- Extra data array you passed to the sort func.
 data_size		- Size of extra data you passed to the sort func.
For Arrays with a cellsize larger than 1 (used for storing arrays and strings),
the function is called in the following manner:
public MySortFunc(Array:array, elem1[], elem2[], const data[], data_size)
 array				- Array handle in its current un-sorted state.
 elem1[], elem2[]	- Current element pair being compared
 data[]				- Extra data array you passed to the sort func.
 data_size			- Size of extra data you passed to the sort func.
In both cases your function should return:
 -1 if elem1 should go before elem2
  0 if elem1 and elem2 are equal
  1 if elem1 should go after elem2
Note that the parameters after elem2 are all optional and you do not need to specify them.
Note that unlike the sorting.inc versions, the array passed to the callback is not in mid-sorted state.");
SMfunctions[86] = Array ("TrieCreate","Creates a hash map. A hash map is a container that can map strings (called
\\&quot;keys\\&quot;) to arbitrary values (cells, arrays, or strings). Keys in a hash map
are unique. That is, there is at most one entry in the map for a given key.
Insertion, deletion, and lookup in a hash map are all considered to be fast
operations, amortized to O(1), or constant time.
The word \\&quot;Trie\\&quot; in this API is historical. As of AMX Mod X 1.8.3, tries have
been internally replaced with hash tables, which have O(1) insertion time
instead of O(n).");
SMfunctions[87] = Array ("TrieClear","Clears all entries from a Map.");
SMfunctions[88] = Array ("TrieSetCell","Sets a value in a hash map, either inserting a new entry or replacing an old one.");
SMfunctions[89] = Array ("TrieSetString","Sets a string value in a Map, either inserting a new entry or replacing an old one.");
SMfunctions[90] = Array ("TrieSetArray","Sets an array value in a Map, either inserting a new entry or replacing an old one.");
SMfunctions[91] = Array ("TrieGetCell","Retrieves a value in a Map.");
SMfunctions[92] = Array ("TrieGetString","Retrieves a string in a Map.");
SMfunctions[93] = Array ("TrieGetArray","Retrieves an array in a Map.");
SMfunctions[94] = Array ("TrieDeleteKey","Removes a key entry from a Map.");
SMfunctions[95] = Array ("TrieKeyExists","Checks a key entry existence from a Map.");
SMfunctions[96] = Array ("TrieDestroy","Destroys a Map.");
SMfunctions[97] = Array ("TrieGetSize","Retrieves the number of elements in a map.");
SMfunctions[98] = Array ("TrieSnapshotCreate","Creates a snapshot of all keys in the map. If the map is changed after this
call, the changes are not reflected in the snapshot. Keys are not sorted.");
SMfunctions[99] = Array ("TrieSnapshotLength","Returns the number of keys in a map snapshot. Note that this may be
different from the size of the map, since the map can change after the
snapshot of its keys was taken.");
SMfunctions[100] = Array ("TrieSnapshotKeyBufferSize","Returns the buffer size required to store a given key. That is, it returns
the length of the key plus one.");
SMfunctions[101] = Array ("TrieSnapshotGetKey","Retrieves the key string of a given key in a map snapshot.");
SMfunctions[102] = Array ("TrieSnapshotDestroy","Destroys a Map snapshot");
SMfunctions[103] = Array ("get_user_stats2","new stats:
0 - total defusions
1 - bomb defused
2 - bomb plants
3 - bomb explosions");
SMfunctions[104] = Array ("cs_get_user_deaths","");
SMfunctions[105] = Array ("cs_set_user_deaths","");
SMfunctions[106] = Array ("cs_get_hostage_foll","Note: this native does not work on Condition Zero, which has a different hostage AI than CS.");
SMfunctions[107] = Array ("cs_set_hostage_foll","Note: this native does not work on Condition Zero, which has a different hostage AI than CS.");
SMfunctions[108] = Array ("cs_get_hostage_id","");
SMfunctions[109] = Array ("cs_get_user_bpammo","Look in amxconst.inc for weapon types: CSW_*.
Weapons on the same line uses the same ammo type:
awm
scout, ak, g3
para
famas, m4a1, aug, sg550, galil, sg552
m3, xm
usp, ump, mac
fiveseven, p90
deagle
p228
glock, mp5, tmp, elites
flash
he
smoke");
SMfunctions[110] = Array ("cs_set_user_bpammo","");
SMfunctions[111] = Array ("cs_get_user_defuse","");
SMfunctions[112] = Array ("cs_set_user_defuse","You can specify a different colour for the defuse kit icon showing on hud. Default is the normal green.
You can specify an icon. Default is \\&quot;defuser\\&quot;. Set flash to 1 if you want the icon to flash red.");
SMfunctions[113] = Array ("cs_get_user_buyzone","");
SMfunctions[114] = Array ("cs_get_user_hasprim","");
SMfunctions[115] = Array ("cs_get_user_model","");
SMfunctions[116] = Array ("cs_set_user_model","");
SMfunctions[117] = Array ("cs_reset_user_model","");
SMfunctions[118] = Array ("cs_get_user_money","");
SMfunctions[119] = Array ("cs_set_user_money","");
SMfunctions[120] = Array ("cs_get_user_nvg","");
SMfunctions[121] = Array ("cs_set_user_nvg","");
SMfunctions[122] = Array ("cs_get_user_plant","");
SMfunctions[123] = Array ("cs_set_user_plant","You should use this if you give a player a weapon_c4, or he won\'t be able to plant it
without dropping it and picking it up again (only possible for terrorists).
If showbombicon is 1, the green C4 icon will be shown on user hud (if plant \\&quot;skill\\&quot; was enabled).");
SMfunctions[124] = Array ("cs_get_user_vip","");
SMfunctions[125] = Array ("cs_get_user_tked","");
SMfunctions[126] = Array ("cs_set_user_tked","tk = 1: player has TKed
tk = 0: player hasn\'t TKed
Set subtract to how many frags to subtract. Set subtract to negative value to add frags.");
SMfunctions[127] = Array ("cs_get_user_driving","0: no driving
1: driving, but standing still
2-4: driving, different positive speeds
5: driving, negative speed (backing)
Note: these values were tested quickly, they may differ.");
SMfunctions[128] = Array ("cs_get_user_shield","");
SMfunctions[129] = Array ("cs_get_user_stationary","");
SMfunctions[130] = Array ("cs_set_user_armor","Appropriate message to update client\'s HUD will be sent if armortype is kevlar or vesthelm.");
SMfunctions[131] = Array ("cs_get_weapon_burst","");
SMfunctions[132] = Array ("cs_set_weapon_burst","Only GLOCK and FAMAS can enter/leave burst mode.");
SMfunctions[133] = Array ("cs_get_weapon_silen","");
SMfunctions[134] = Array ("cs_set_weapon_silen","");
SMfunctions[135] = Array ("cs_get_weapon_ammo","");
SMfunctions[136] = Array ("cs_set_weapon_ammo","");
SMfunctions[137] = Array ("cs_get_weapon_id","");
SMfunctions[138] = Array ("cs_get_no_knives","");
SMfunctions[139] = Array ("cs_set_no_knives","No knives mode means that player will not be given a knife when spawning.
You can still give knives (ie through fun\'s give_item).");
SMfunctions[140] = Array ("cs_user_spawn","");
SMfunctions[141] = Array ("cs_get_armoury_type","");
SMfunctions[142] = Array ("cs_set_armoury_type","The second argument, type, should be a CSW_* constant. Not all weapons are supported by Counter-strike.
Supported weapons/items: CSW_MP5NAVY, CSW_TMP, CSW_P90, CSW_MAC10, CSW_AK47, CSW_SG552, CSW_M4A1, CSW_AUG, CSW_SCOUT
CSW_G3SG1, CSW_AWP, CSW_M3, CSW_XM1014, CSW_M249, CSW_FLASHBANG, CSW_HEGRENADE, CSW_VEST, CSW_VESTHELM, CSW_SMOKEGRENADE");
SMfunctions[143] = Array ("cs_get_user_mapzones","NOTE: If user can\'t plant (cs_get_user_plant(index) is 0) then cs_get_user_mapzones(index) &amp; CS_MAPZONE_BOMBTARGET will return 0 too.");
SMfunctions[144] = Array ("cs_set_user_zoom","The 2nd param has to be one of the above zoom types in the enum. Mode can only be 0 or 1.
If mode=0 (blocking mode), the user will be forced to use the zoom type set by the native, and wont be able to change it (even by changing weapon)
until the native resets the zoom with CS_RESET_ZOOM.
If mode=1 the user will be able to restore back to a normal view by changing weapon.");
SMfunctions[145] = Array ("cs_get_user_zoom","");
SMfunctions[146] = Array ("cs_get_user_submodel","If this is 1, then the user has a backpack or defuser on their model (depending on team)");
SMfunctions[147] = Array ("cs_set_user_submodel","If this is 1, then the user has a backpack or defuser on their model (depending on team)
0 removes it.");
SMfunctions[148] = Array ("cs_get_user_lastactivity","checks to see who has been afk too long.");
SMfunctions[149] = Array ("cs_get_user_hostagekills","");
SMfunctions[150] = Array ("cs_get_hostage_lastuse","");
SMfunctions[151] = Array ("cs_get_hostage_nextuse","");
SMfunctions[152] = Array ("cs_get_c4_explode_time","");
SMfunctions[153] = Array ("cs_get_c4_defusing","");
SMfunctions[154] = Array ("CS_InternalCommand","Called when CS internally fires a command to a player.  It does this for a few
functions, most notably rebuy/autobuy functionality.  This is also used to pass
commands to CZ bots internally.");
SMfunctions[155] = Array ("CS_OnBuyAttempt","Called when a player attempts to purchase an item.
This is ususally called right away on buy commands issued by a player.");
SMfunctions[156] = Array ("CS_OnBuy","Called when a player purchases an item.
This usually called right before a player gets the purchased item.");
SMfunctions[157] = Array ("get_map_objectives","Gets current map objectives.");
SMfunctions[158] = Array ("CreateDataPack","Creates a new data pack.");
SMfunctions[159] = Array ("WritePackCell","Packs a normal cell into a data pack.");
SMfunctions[160] = Array ("WritePackFloat","Packs a float into a data pack.");
SMfunctions[161] = Array ("WritePackString","Packs a string into a data pack.");
SMfunctions[162] = Array ("ReadPackCell","Reads a cell from a data pack.");
SMfunctions[163] = Array ("ReadPackFloat","Reads a float from a data pack.");
SMfunctions[164] = Array ("ReadPackString","Reads a string from a data pack.");
SMfunctions[165] = Array ("ResetPack","Resets the position in a data pack.");
SMfunctions[166] = Array ("GetPackPosition","Returns the read or write position in a data pack.");
SMfunctions[167] = Array ("SetPackPosition","Sets the read/write position in a data pack.");
SMfunctions[168] = Array ("IsPackReadable","Returns whether or not a specified number of bytes from the data pack
 position to the end can be read.");
SMfunctions[169] = Array ("DestroyDataPack","Disposes of a data pack.");
SMfunctions[170] = Array ("dbi_connect","If it does fail, the error will be mirrored in dbi_error()
The return value will otherwise be a resource handle, not an
OK code or cell pointer.");
SMfunctions[171] = Array ("dbi_query","If it fails, it will return a number BELOW ZERO (0)
If zero, it succeeded with NO RETURN RESULT.
If greater than zero, make sure to call dbi_free_result() on it!
 The return is a handle to the result set");
SMfunctions[172] = Array ("dbi_query2","reference the number of rows affected in the query. If the
query fails rows will be equal to -1.");
SMfunctions[173] = Array ("dbi_nextrow","Advances result pointer by one row.");
SMfunctions[174] = Array ("dbi_field","Although internally fields always start from 0,
This function takes fieldnum starting from 1.
No extra params: returns int
One extra param: returns Float: byref
Two extra param: Stores string with length");
SMfunctions[175] = Array ("dbi_result","One extra param: returns Float: byref
Two extra param: Stores string with length");
SMfunctions[176] = Array ("dbi_num_rows","");
SMfunctions[177] = Array ("dbi_free_result","");
SMfunctions[178] = Array ("dbi_close","mark the handle as free, so this particular handle may
be re-used in the future to save time.");
SMfunctions[179] = Array ("dbi_error","this is a direct error return from the database handle/API.
For MSSQL, it returns the last error message found from a
thrown exception.");
SMfunctions[180] = Array ("dbi_type","\\&quot;mysql\\&quot;, \\&quot;pgsql\\&quot;, \\&quot;mssql\\&quot;, \\&quot;sqlite\\&quot;");
SMfunctions[181] = Array ("dbi_num_fields","Unlike dbi_nextrow, you must pass a valid result handle.");
SMfunctions[182] = Array ("dbi_field_name","Requires a valid result handle, and columns are numbered 1 to n.");
SMfunctions[183] = Array ("sqlite_table_exists","");
SMfunctions[184] = Array ("dod_set_pl_deaths","Note if you opt to refresh the scoreboard, it
will make the player appear as \\&quot;DEAD\\&quot; in the scoreboard.");
SMfunctions[185] = Array ("pfn_touch","ptr - touched entity
ptd - toucher entity");
SMfunctions[186] = Array ("find_sphere_class","certain entity specified in aroundent. All matching ents are stored in entlist. Specify max amount of entities to find in maxents.
If aroundent is 0 its origin is not used, but origin in 6th parameter. Ie, do not specify 6th parameter (origin) if you specified an entity
in aroundent.");
SMfunctions[187] = Array ("is_in_viewcone","Set use3d to 1 to do the calculation in 3D. Otherwise it will be in 2D.");
SMfunctions[188] = Array ("set_ent_rendering","Sets rendering of an entity, including player entities.
This is basically the same as set_rendering() stock.");
SMfunctions[189] = Array ("entity_intersects","Checks whether two entities intersect by comparing
their absolute minimum and maximum coordinates.");
SMfunctions[190] = Array ("pev","Returns entvar data from an entity.  Use the pev_* enum (in fakemeta_const.inc) to specify which data you want retrieved.");
SMfunctions[191] = Array ("set_pev","Sets entvar data for an entity.  Use the pev_* enum from fakemeta_const.inc for reference.");
SMfunctions[192] = Array ("set_pev_string","Use this native to set a pev field to a string that is already allocated (via a function such
as EngFunc_AllocString).");
SMfunctions[193] = Array ("pev_valid","Checks the validity of an entity.");
SMfunctions[194] = Array ("pev_serial","Returns the serial number for each entity.  The serial number is a unique identity
generated when an entity is created.");
SMfunctions[195] = Array ("global_get","When returning data from glb_pStringBase (the global string table), you may give a pointer into that table
in order to get different strings.
Example:
new model[128]
new ptr = pev(id, pev_viewmodel)
global_get(glb_pStringBase, ptr, model, 127)");
SMfunctions[196] = Array ("get_pdata_int","Returns a integer from an entity\'s private data.
_linuxdiff value is what to add to the _Offset for linux servers.
_macdiff value is what to add to the _Offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[197] = Array ("set_pdata_int","Sets an integer to an entity\'s private data.
_linuxdiff value is what to add to the _Offset for linux servers.
_macdiff value is what to add to the _Offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[198] = Array ("get_pdata_float","Returns a float from an entity\'s private data.
_linuxdiff value is what to add to the _Offset for linux servers.
_macdiff value is what to add to the _Offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[199] = Array ("set_pdata_float","Sets a float to an entity\'s private data.
_linuxdiff value is what to add to the _Offset for linux servers.
_macdiff value is what to add to the _Offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[200] = Array ("get_pdata_ent","Tries to retrieve an edict pointer from an entity\'s private data.
This function is byte-addressable.  Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_ent searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[201] = Array ("set_pdata_ent","Sets an edict pointer to an entity\'s private data.
This function is byte-addressable.  Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_ent searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[202] = Array ("get_pdata_bool","Returns a boolean from an entity\'s private data.
This function is byte-addressable. Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_bool searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[203] = Array ("set_pdata_bool","Sets a boolean to an entity\'s private data.
This function is byte-addressable. Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_bool searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[204] = Array ("get_pdata_byte","Returns a byte value from an entity\'s private data.
This function is byte-addressable. Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_byte searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[205] = Array ("set_pdata_byte","Sets a byte value to an entity\'s private data.
This function is byte-addressable. Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_byte searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[206] = Array ("get_pdata_short","Returns a short value from an entity\'s private data.
This function is byte-addressable. Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_short searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[207] = Array ("set_pdata_short","Sets a short value to an entity\'s private data.
This function is byte-addressable.  Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_short searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[208] = Array ("get_pdata_vector","Returns a vector from an entity\'s private data.
This function is byte-addressable. Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_vector searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[209] = Array ("set_pdata_vector","Sets a vector to an entity\'s private data.
This function is byte-addressable.  Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_vector searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[210] = Array ("get_pdata_ehandle","Tries to retrieve an edict (entity encapsulation) pointer from an entity\'s private data.
This function is byte-addressable.  Unlike get_pdata_int() which searches in byte increments of 4,
get_pdata_ehandle searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _offset.");
SMfunctions[211] = Array ("set_pdata_ehandle","Sets an edict (entity encapsulation) pointer to an entity\'s private data.
This function is byte-addressable.  Unlike set_pdata_int() which searches in byte increments of 4,
set_pdata_ehandle searches in increments of 1.
_linuxdiff value is what to add to the _offset for linux servers.
_macdiff value is what to add to the _offset for os x servers.
A log error is thrown on invalid _index and _Offset.");
SMfunctions[212] = Array ("register_forward","Returns an id you can pass to unregister_forward");
SMfunctions[213] = Array ("unregister_forward","The registerId must be from register_forward, and
post/forwardtype must match what you registered the forward as.");
SMfunctions[214] = Array ("get_orig_retval","This is only valid in forwards that were registered as post.
get_orig_retval() - no params, retrieves integer return value
get_orig_retval(&amp;Float:value) - retrieves float return value by reference
get_orig_retval(value[], len) - retrives string return value");
SMfunctions[215] = Array ("create_tr2","Creates a traceresult handle.  This value should never be altered.
The handle can be used in get/set_tr2 and various traceresult engine functions.
NOTE: You must call free_tr2() on every handle made with create_tr2().");
SMfunctions[216] = Array ("free_tr2","Frees a traceresult handle created with free_tr2().  Do not call
this more than once per handle, or on handles not created through
create_tr2().");
SMfunctions[217] = Array ("lookup_sequence","Looks up the sequence for the entity.");
SMfunctions[218] = Array ("set_controller","Sets a bone controller with the specified value.");
SMfunctions[219] = Array ("GetModelCollisionBox","Gets size of the entity models collision box.");
SMfunctions[220] = Array ("SetModelCollisionBox","Sets entity size to the models collision box.");
SMfunctions[221] = Array ("GetModelBoundingBox","Gets size of a model bounding box.");
SMfunctions[222] = Array ("SetModelBoundingBox","Sets size to a model bounding box.");
SMfunctions[223] = Array ("fm_set_user_noclip","");
SMfunctions[224] = Array ("rename_file","if relative true, rename_file will act like other natives which
use the moddir as a base directory.  otherwise, the current directory is
undefined (but assumed to be hlds).");
SMfunctions[225] = Array ("LoadFileForMe","Loads a file using the LoadFileForMe engine function.
The data is truncated if there is not enough space.  No null-terminator
is applied; the data is the raw contents of the file.");
SMfunctions[226] = Array ("fflush","Flushes a buffered output stream.");
SMfunctions[227] = Array ("GetFileTime","Returns a file timestamp as a unix timestamp.");
SMfunctions[228] = Array ("geoip_code2_ex","Lookup the two character country code for a given IP address.
e.g: \\&quot;US\\&quot;, \\&quot;CA\\&quot;, etc.");
SMfunctions[229] = Array ("geoip_code3_ex","Lookup the three character country code for a given IP address.
e.g: \\&quot;USA\\&quot;, \\&quot;cAN\\&quot;, etc.");
SMfunctions[230] = Array ("geoip_code2","");
SMfunctions[231] = Array ("geoip_code3","");
SMfunctions[232] = Array ("geoip_country","Lookup the full country name for the given IP address.  Sets the buffer to \\&quot;error\\&quot; on
an unsuccessful lookup.");
SMfunctions[233] = Array ("RegisterHam","Hooks the virtual table for the specified entity class.
An example would be: RegisterHam(Ham_TakeDamage, \\&quot;player\\&quot;, \\&quot;player_hurt\\&quot;);
Look at the Ham enum for parameter lists.");
SMfunctions[234] = Array ("RegisterHamPlayer","Hooks the virtual table for the player class.
An example would be: RegisterHam(Ham_TakeDamage, \\&quot;player_hurt\\&quot;);
Look at the Ham enum for parameter lists.");
SMfunctions[235] = Array ("RegisterHamFromEntity","Hooks the virtual table for the specified entity\'s class.
An example would be: RegisterHam(Ham_TakeDamage, id, \\&quot;player_hurt\\&quot;);
Look at the Ham enum for parameter lists.
Note: This will cause hooks for the entire internal class that the entity is
      not exclusively for the provided entity.");
SMfunctions[236] = Array ("DisableHamForward","Stops a ham forward from triggering.
Use the return value from RegisterHam as the parameter here!");
SMfunctions[237] = Array ("EnableHamForward","Starts a ham forward back up.
Use the return value from RegisterHam as the parameter here!");
SMfunctions[238] = Array ("ExecuteHam","Executes the virtual function on the entity.
Look at the Ham enum for parameter lists.");
SMfunctions[239] = Array ("ExecuteHamB","Executes the virtual function on the entity, this will trigger all hooks on that function.
Be very careful about recursion!
Look at the Ham enum for parameter lists.");
SMfunctions[240] = Array ("GetHamReturnStatus","Gets the return status of the current hook.
This is useful to determine what return natives to use.");
SMfunctions[241] = Array ("GetHamReturnInteger","Gets the return value of a hook for hooks that return integers or booleans.");
SMfunctions[242] = Array ("GetHamReturnFloat","Gets the return value of a hook for hooks that return float.");
SMfunctions[243] = Array ("GetHamReturnVector","Gets the return value of a hook for hooks that return Vectors.");
SMfunctions[244] = Array ("GetHamReturnEntity","Gets the return value of a hook for hooks that return entities.");
SMfunctions[245] = Array ("GetHamReturnString","Gets the return value of a hook for hooks that return strings.");
SMfunctions[246] = Array ("GetOrigHamReturnInteger","Gets the original return value of a hook for hooks that return integers or booleans.");
SMfunctions[247] = Array ("GetOrigHamReturnFloat","Gets the original return value of a hook for hooks that return floats.");
SMfunctions[248] = Array ("GetOrigHamReturnVector","Gets the original return value of a hook for hooks that return Vectors.");
SMfunctions[249] = Array ("GetOrigHamReturnEntity","Gets the original return value of a hook for hooks that return entities.");
SMfunctions[250] = Array ("GetOrigHamReturnString","Gets the original return value of a hook for hooks that return strings.");
SMfunctions[251] = Array ("SetHamReturnInteger","Sets the return value of a hook that returns an integer or boolean.
This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.");
SMfunctions[252] = Array ("SetHamReturnFloat","Sets the return value of a hook that returns a float.
This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.");
SMfunctions[253] = Array ("SetHamReturnVector","Sets the return value of a hook that returns a Vector.
This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.");
SMfunctions[254] = Array ("SetHamReturnEntity","Sets the return value of a hook that returns an entity.  Set to -1 for null.
This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.");
SMfunctions[255] = Array ("SetHamReturnString","Sets the return value of a hook that returns a string.
This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.");
SMfunctions[256] = Array ("SetHamParamInteger","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are integers.");
SMfunctions[257] = Array ("SetHamParamFloat","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are floats.");
SMfunctions[258] = Array ("SetHamParamVector","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are Vectors.");
SMfunctions[259] = Array ("SetHamParamEntity","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are entities.");
SMfunctions[260] = Array ("SetHamParamString","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are strings.");
SMfunctions[261] = Array ("SetHamParamTraceResult","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are trace result handles.");
SMfunctions[262] = Array ("SetHamParamItemInfo","Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
Use this on parameters that are trace result handles.");
SMfunctions[263] = Array ("GetHamItemInfo","Gets a parameter on the fly of the current hook.
Use this on parameters that are iteminfo result handles.");
SMfunctions[264] = Array ("SetHamItemInfo","Sets a parameter on the fly of the current hook.
Use this on parameters that are iteminfo result handles.");
SMfunctions[265] = Array ("CreateHamItemInfo","Creates an ItemInfo handle.  This value should never be altered.
The handle can be used in Get/SetHamItemInfo.
NOTE: You must call FreeHamItemInfo() on every handle made with CreateHamItemInfo().");
SMfunctions[266] = Array ("FreeHamItemInfo","Frees an ItemIndo handle created with CreateHamItemInfo().  Do not call
this more than once per handle, or on handles not created through
CreateHamItemInfo().");
SMfunctions[267] = Array ("IsHamValid","Returns whether or not the function for the specified Ham is valid.
Things that would make it invalid would be bounds (an older module version
 may not have all of the functions), and the function not being found in
 the mod\'s hamdata.ini file.");
SMfunctions[268] = Array ("get_pdata_cbase","This is used to compliment fakemeta\'s {get,set}_pdata_{int,float,string}.
This requires the mod to have the pev and base fields set in hamdata.ini.
Note this dereferences memory! Improper use of this will crash the server.
This will return an index of the corresponding cbase field in private data.
Returns -1 on a null entry.");
SMfunctions[269] = Array ("set_pdata_cbase","This is used to compliment fakemeta\'s {get,set}_pdata_{int,float,string}.
This requires the mod to have the pev and base fields set in hamdata.ini.
This will set the corresponding cbase field in private data with the index.
Pass -1 to null the entry.");
SMfunctions[270] = Array ("get_pdata_cbase_safe","This is similar to the get_pdata_cbase, however it does not dereference memory.
This is many times slower than get_pdata_cbase, and this should only be used
for testing and finding of offsets, not actual release quality plugins.
This will return an index of the corresponding cbase field in private data.
Returns -1 on a null entry. -2 on an invalid entry.");
SMfunctions[271] = Array ("CreateLangKey","Adds or finds a translation key.");
SMfunctions[272] = Array ("GetLangTransKey","Finds a translation key id without adding on failure.
Returns -1 on not found.");
SMfunctions[273] = Array ("AddTranslation","Adds a translation.");
SMfunctions[274] = Array ("LookupLangKey","Looks up the translation of the key for the given type
This does NOT format the output text.
eg: If the key includes %s, the outputted text will also contain %s.
NOTE: LANG_PLAYER is invalid in this, use a player index
      or LANG_SERVER");
SMfunctions[275] = Array ("emessage_begin"," are also sent to all other plugins and Metamod plugins.
This means that if you send one of these messages, other plugins will
 be notified, which was previously impossible.
BE CAREFUL! Using these incorrectly, or not for their intended purpose,
 could cause infinite recursion or something just as bad.
NOTE! These natives are experimental.");
SMfunctions[276] = Array ("client_printex","Sends a predefined text message to player.
Predefined texts are default game messages which will be translated
to player\'s game language, e.g. #Game_join_ct.");
SMfunctions[277] = Array ("menu_create"," Creates a new menu object. The handler function should be prototyped as: public &lt;function&gt;(id, menu, item)  id     - Client the menu is being acted upon.  menu   - Menu resource identifier.  item   - Item the client selected.  If less than 0, the menu was           cancelled and the item is a status code.  menu_display 		 should never be called immediately if the item is a status 		 code, for re-entrancy reasons. The handler function should always return PLUGIN_HANDLED to block any old menu handlers from potentially feeding on the menu, unless that is the desired functionality.");
SMfunctions[278] = Array ("menu_makecallback","Creates a menu item callback handler.
The handler function should be prototyped as:
public &lt;function&gt;(id, menu, item)
 id      - Client index being displayed to.
 menu    - Menu resource identifier.
 item    - Item being drawn.
&lt;return&gt; - ITEM_IGNORE to use the default functionality.  ITEM_ENABLED to
           explicitly enable or ITEM_DISABLED to explicitly disable.");
SMfunctions[279] = Array ("menu_additem","Adds an menu to a menu.");
SMfunctions[280] = Array ("menu_pages","Returns the number of pages in a menu.");
SMfunctions[281] = Array ("menu_items","Returns the number of items in a menu.");
SMfunctions[282] = Array ("menu_display","Displays a menu to one client.  This should never be called from a handler
when the item is less than 0 (i.e. calling this from a cancelled menu will
result in an error).
Starting with 1.8.3 this allows to specify a menu timeout similar to the
show_menu native. If the menu exists on the client past the timeout *any*
further action will send the MENU_TIMEOUT status code to the menu handler.
That includes actions which would otherwise send MENU_EXIT, such as the
client selecting an item or disconnecting and calling menu_cancel or
menu_destroy on a live menu.");
SMfunctions[283] = Array ("menu_find_id","Given a page on a menu and a keypress on that page, returns the item id selected.
If the item is less than 0, a special option was chosen (such as MENU_EXIT).");
SMfunctions[284] = Array ("menu_item_getinfo","Retrieves info about a menu item.");
SMfunctions[285] = Array ("menu_item_setname","Sets an item\'s display text.");
SMfunctions[286] = Array ("menu_item_setcmd","Sets an item\'s info string.");
SMfunctions[287] = Array ("menu_item_setcall","Sets an item\'s callback.");
SMfunctions[288] = Array ("menu_destroy","Destroys a menu.  Player menus will be cancelled (although may still linger
on the HUD), and future attempts to access the menu resource will result in
an error.
This must be called if you create menus dynamically, otherwise you will
leak memory.  For normal dynamic menus, you will destroy the menu in the
handler function (remembering to handle the case of a menu being cancelled,
it must still be destroyed).");
SMfunctions[289] = Array ("player_menu_info","Returns information about a menu (if any) the client is currently viewing.
If newmenu is valid, then the menu will refer to the menuid associated with
the title.  If newmenu is not valid, and the menu is valid, then the player
is viewing a menu displayed with show_menu().
Both may be invalid if the player is not viewing a menu.");
SMfunctions[290] = Array ("menu_addblank","Adds a blank line to a menu.
When using slot=1 this might break your menu. To achieve this functionality
menu_addblank2 should be used.");
SMfunctions[291] = Array ("menu_addtext","Adds a text line to a menu.  Only available in amxmodx 1.8.1 and above.
When using slot=1 this might break your menu. To achieve this functionality
menu_addtext2 should be used.");
SMfunctions[292] = Array ("menu_addblank2","Adds a blank line to a menu, always shifting the numbering down.
This will add a special item to create a blank line. It will affect the menu
item count and pagination. These items can be modified later but will ignore
access and item callback results.
Only available in 1.8.3 and above.");
SMfunctions[293] = Array ("menu_addtext2","Adds a text line to a menu, always shifting the numbering down.
This will add a special item to create a blank line. It will affect the menu
item count and pagination. These items can be modified later but will ignore
access and item callback results.
Only available in 1.8.3 and above.");
SMfunctions[294] = Array ("menu_setprop","Sets a menu property.");
SMfunctions[295] = Array ("menu_cancel","Cancels a player\'s menu, effectively forcing the player to select MENU_EXIT.
The menu will still exist on their screen but any results are invalidated,
and the callback is invoked.");
SMfunctions[296] = Array ("client_changeclass","Called whenever the client\'s class is changed.");
SMfunctions[297] = Array ("client_built","Called whenever the client builds a structure.");
SMfunctions[298] = Array ("ns_is_combat","Tell whether or not the map is combat.");
SMfunctions[299] = Array ("ns_get_gameplay","Returns the gameplay type for the currently active map.
Refer to ns_const.inc\'s NSGameplay enum for details.");
SMfunctions[300] = Array ("ns_get_user_team","Exact syntax as get_user_team, but should be more accurate.");
SMfunctions[301] = Array ("ns_popup","Send an NS-style popup message.");
SMfunctions[302] = Array ("ns_set_player_model","Sets a player model.  Omit the second parameter to return to default");
SMfunctions[303] = Array ("ns_set_player_skin","Sets a player skin.  Omit the second parameter to return to default");
SMfunctions[304] = Array ("ns_set_player_body","Sets a player body.  Omit the second parameter to return to default");
SMfunctions[305] = Array ("ns_set_speedchange","Set this to modify the player\'s speed by a certain amount.");
SMfunctions[306] = Array ("ns_get_speedchange","Returns a client\'s current speed modifier.");
SMfunctions[307] = Array ("ns_get_maxspeed","Returns a client\'s maxspeed before the speed change modifier is factored in.");
SMfunctions[308] = Array ("ns_get_build","");
SMfunctions[309] = Array ("ns_get_spawn","");
SMfunctions[310] = Array ("ns_get_jpfuel","Gets the player\'s jetpack fuel reserve.");
SMfunctions[311] = Array ("ns_set_jpfuel","Sets the player\'s jetpack fuel reserve.");
SMfunctions[312] = Array ("ns_add_jpfuel","Adds to the player\'s jetpack fuel reserve.");
SMfunctions[313] = Array ("ns_get_energy","Gets the player\'s energy percentage.");
SMfunctions[314] = Array ("ns_set_energy","Sets the player\'s energy percentage.");
SMfunctions[315] = Array ("ns_add_energy","Adds to the player\'s energy percentage.");
SMfunctions[316] = Array ("ns_get_res","Returns a player\'s resources.");
SMfunctions[317] = Array ("ns_set_res","Sets a player\'s resources.");
SMfunctions[318] = Array ("ns_add_res","Adds an amount of resources to the player.");
SMfunctions[319] = Array ("ns_get_teamres","Returns the team\'s resources.");
SMfunctions[320] = Array ("ns_set_teamres","Sets the team\'s resources in the resource pool.");
SMfunctions[321] = Array ("ns_add_teamres","Adds to the team\'s resources in the resource pool.");
SMfunctions[322] = Array ("ns_get_exp","Returns the player\'s experience.");
SMfunctions[323] = Array ("ns_set_exp","Sets the player\'s experience.");
SMfunctions[324] = Array ("ns_add_exp","Adds to the player\'s experience.");
SMfunctions[325] = Array ("ns_get_points","Gets the player\'s points spent count in combat.");
SMfunctions[326] = Array ("ns_set_points","Sets the player\'s points spent count in combat.");
SMfunctions[327] = Array ("ns_add_points","Adds to the player\'s points spent count in combat.");
SMfunctions[328] = Array ("ns_get_weap_dmg","Gets the damage for this weapon.");
SMfunctions[329] = Array ("ns_set_weap_dmg","Sets the damage for this weapon.");
SMfunctions[330] = Array ("ns_get_weap_range","Gets the maximum range for this weapon.");
SMfunctions[331] = Array ("ns_set_weap_range","Sets the maximum range for this weapon.");
SMfunctions[332] = Array ("ns_get_weap_clip","Gets the weapon\'s clip ammo.");
SMfunctions[333] = Array ("ns_set_weap_clip","Sets the weapon\'s ammo in the clip.");
SMfunctions[334] = Array ("ns_get_weap_reserve","Gets the player\'s weapon reserve (backpack ammo) for the specified
type of weapon.");
SMfunctions[335] = Array ("ns_set_weap_reserve","Sets the player\'s weapon reserve (backpack ammo) for the specified
type of weapon.");
SMfunctions[336] = Array ("ns_get_score","Gets the player\'s score.");
SMfunctions[337] = Array ("ns_set_score","Sets the player\'s score.");
SMfunctions[338] = Array ("ns_add_score","Returns the new score on success");
SMfunctions[339] = Array ("ns_add_deaths","Returns the new death count on success");
SMfunctions[340] = Array ("ns_give_item","Give the player an item.");
SMfunctions[341] = Array ("ns_get_hive_ability","Returns 1 if a player has the hive ability number.
If ability is 0, it will return the number of active hives.");
SMfunctions[342] = Array ("client_changeteam","Triggered whenever a client\'s pev-&gt;team changes.");
SMfunctions[343] = Array ("client_spawn","Triggered whenever a client\'s pev-&gt;deadflag changes from &gt;0 to 0.");
SMfunctions[344] = Array ("ns_takedamage","Calls NS\'s private damage routine on the victim entity.");
SMfunctions[345] = Array ("ns_unstick_player","Attempts to unstick a player.");
SMfunctions[346] = Array ("ns_round_in_progress","Whether or not there is a game in progress.");
SMfunctions[347] = Array ("round_start","Called at the approximate time that a round is started.");
SMfunctions[348] = Array ("round_end","Called immediately when a round ends");
SMfunctions[349] = Array ("ns_get_locationname","(z origin is ignored; can\'t have location over location)
-
Note that as of NS 3.2 beta 2, on the following maps
the returned string should be passed through ns_lookup_title
to be human readable:
  ns_bast, ns_hera, ns_nothing, ns_tanith,
  ns_nancy, ns_caged, ns_eclipse, ns_veil
Passing the 5th parameter as non zero will auto look up
the title if it exists.");
SMfunctions[350] = Array ("ns_lookup_title","Returns -1 if the key is not found
Otherwise it returns the length of the output");
SMfunctions[351] = Array ("ns_build_structure","Removes the ghost state from marine structures.
Do not use this on hives! It wont work.");
SMfunctions[352] = Array ("ns_recycle","Passing an index other than a marine structure will
have undefined results!
-
Note: This calls a private NS function!
      Be careful when using this!");
SMfunctions[353] = Array ("ns_finish_weldable","Passing an index other than a weldable
will have undefined results!
-
NS renames func_weldable to avhweldable
at map load.
-
Note: This calls a private NS function!
      Be careful when using this!");
SMfunctions[354] = Array ("ns_get_weld_time","func_weldable shut.
Note: NS renames \\&quot;func_weldable\\&quot;s to \\&quot;avhweldable\\&quot;s
at run time!");
SMfunctions[355] = Array ("ns_set_weld_time","func_weldable shut.");
SMfunctions[356] = Array ("ns_add_weld_time","Returns the new required time on success.
Note this native clamps the low value to 0.");
SMfunctions[357] = Array ("ns_get_weld_done","has been welded.");
SMfunctions[358] = Array ("ns_set_weld_done","has been welded.");
SMfunctions[359] = Array ("ns_add_weld_done","has been welded.  Returns the new value.
Note this native clamps the low value to 0.0");
SMfunctions[360] = Array ("ns_remove_upgrade","Removes an upgrade from the player\'s bought and active upgrade lists.
This will not refund the points spent on the upgrade, nor will it
immediately strip the upgrade if the player is alive.  Rather, it will
make it so the player no longer receives the upgrade on spawn.");
SMfunctions[361] = Array ("ns_create_ps","-
Note! this is not a particle system you can pass to
ns_fire_ps()!");
SMfunctions[362] = Array ("ns_set_ps_name","-
This is used for things like ns_get_ps_id()
and through calling another particle system
through the \\&quot;ps_to_gen\\&quot; field");
SMfunctions[363] = Array ("ns_set_ps_sprite","-
You do NOT have to precache the sprite, BUT
the sprite must obviously be on the client to
display.");
SMfunctions[364] = Array ("ns_spawn_ps","A usable particle system handle is returned.");
SMfunctions[365] = Array ("ns_fire_ps","Flags are the FEV_* defines from hlsdk_const.inc
Only use handles returned by ns_spawn_ps or ns_get_ps_id here!");
SMfunctions[366] = Array ("ns_get_ps_id","Returns a usable particle system handle.");
SMfunctions[367] = Array ("ns_set_ps_genrate","particle system.  Look through the fgd and NSPSEdit
for details!");
SMfunctions[368] = Array ("nvault_open","Returns a vault id, INVALID_HANDLE otherwise (-1)");
SMfunctions[369] = Array ("nvault_get","setting a byref float or setting a string + maxlength");
SMfunctions[370] = Array ("nvault_lookup","Returns 0 if the entry is not found");
SMfunctions[371] = Array ("nvault_prune","This will not erase values set with pset");
SMfunctions[372] = Array ("nvault_touch","If stamp is -1 (default), it will use the current time.
Like the unix command \\&quot;touch,\\&quot; it will create an empty key
 if the value does not exist.");
SMfunctions[373] = Array ("regex_compile","Precompile a regular expression.");
SMfunctions[374] = Array ("regex_match_c","Matches a string against a pre-compiled regular expression pattern.");
SMfunctions[375] = Array ("regex_match","Matches a string against a regular expression pattern.");
SMfunctions[376] = Array ("regex_substr","Returns a matched substring from a regex handle.");
SMfunctions[377] = Array ("regex_free","Frees the memory associated with a regex result, and sets the handle to 0.");
SMfunctions[378] = Array ("regex_compile_ex","Precompile a regular expression.");
SMfunctions[379] = Array ("regex_match_all_c","Matches a string against a pre-compiled regular expression pattern, matching all
occurrences of the pattern inside the string. This is similar to using the \\&quot;g\\&quot; flag
in perl regex.");
SMfunctions[380] = Array ("regex_match_all","Matches a string against a regular expression pattern, matching all occurrences of the
pattern inside the string. This is similar to using the \\&quot;g\\&quot; flag in perl regex.");
SMfunctions[381] = Array ("regex_match_simple","Matches a string against a regular expression pattern.");
SMfunctions[382] = Array ("regex_replace","Perform a regular expression search and replace.
An optional parameter, flags, allows you to specify options on how the replacement is performed.
Supported format specifiers for replace parameter:
  $number  : Substitutes the substring matched by group number.
             n must be an integer value designating a valid backreference, greater than 0, and of two digits at most.
  ${name}  : Substitutes the substring matched by the named group name (a maximum of 32 characters).
  $&amp;       : Substitutes a copy of the whole match.
  $`       : Substitutes all the text of the input string before the match.
  $\'       : Substitutes all the text of the input string after the match.
  $+       : Substitutes the last group that was captured.
  $_       : Substitutes the entire input string.
  $$       : Substitutes a literal \\&quot;$\\&quot;.
As note, the character \\ can be also used with format specifier, this is same hehavior as $.");
SMfunctions[383] = Array ("socket_open","returns a socket (positive) or negative or zero on error.
States of error:
0 - no error
1 - error while creating socket
2 - couldn\'t resolve hostname
3 - couldn\'t connect to given hostname:port");
SMfunctions[384] = Array ("SortIntegers","Basic sorting functions below.");
SMfunctions[385] = Array ("SortCustom1D","Sorts a custom 1D array.  You must pass in a comparison function.
The sorting algorithm then uses your comparison function to sort the data.
The function is called in the following manner:
public MySortFunc(elem1, elem2, const array[], const data[], data_size)
 elem1, elem2	- Current element pair being compared
 array[]			- Array in its current mid-sorted state.
 data[]			- Extra data array you passed to the sort func.
 data_size		- Size of extra data you passed to the sort func.
Your function should return:
 -1 if elem1 should go before elem2
  0 if elem1 and elem2 are equal
  1 if elem1 should go after elem2
Note that the parameters after elem2 are all optional and you do not need to specify them.");
SMfunctions[386] = Array ("SortCustom2D","Sorts a custom 2D array.
The sorting algorithm then uses your comparison function to sort the data.
The function is called in the following manner:
public MySortFunc(const elem1[], const elem2[], const array[], data[], data_size)
 elem1[], elem2[] - Current element array pairs being compared
 array[][]		 - Array in its currently being sorted state.
 data[]			 - Extra data array you passed to the sort func.
 data_size		 - Size of extra data you passed to the sort func.
Your function should return:
 -1 if elem1[] should go before elem2[]
  0 if elem1[] and elem2 are equal[]
  1 if elem1[] should go after elem2[]
Note that the parameters after elem2[] are all optional and you do not need to specify them.");
SMfunctions[387] = Array ("SortADTArray","Sort an ADT Array. Specify the type as Integer, Float, or String.");
SMfunctions[388] = Array ("SQL_MakeDbTuple","Creates a connection information tuple.
This tuple must be passed into connection routines.
Freeing the tuple is not necessary, but is a good idea if you
 create many of them.  You can cache these handles globally.
!!NOTE!! I have seen most people think that this connects to the DB.
  Nowhere does it say this, and in fact it does not.  It only caches
  the connection information, the host/user/pass/etc.
The optional timeout parameter specifies how long connections should wait before
giving up.  If 0, the default (which is undefined) is used.");
SMfunctions[389] = Array ("SQL_FreeHandle","Frees an SQL handle.
The handle can be to anything (tuple, connection, query, results, etc).
If you free a database connection, it closes the connection as well.");
SMfunctions[390] = Array ("SQL_Connect","Opens a database connection.
Returns an SQL handle, which must be freed.
Returns Empty_Handle on failure.");
SMfunctions[391] = Array ("SQL_SetCharset","Sets the character set of the current connection.
Like SET NAMES .. in mysql, but stays after connection problems.
If a connection tuple is supplied, this should be called before SQL_Connect or SQL_ThreadQuery.
Also note the change will remain until you call this function with another value.
Example: \\&quot;utf8\\&quot;, \\&quot;latin1\\&quot;");
SMfunctions[392] = Array ("SQL_PrepareQuery","Prepares a query.
The query must always be freed.
This does not actually do the query!");
SMfunctions[393] = Array ("SQL_QuoteString","Back-quotes characters in a string for database querying.
Note: The buffer\'s maximum size should be 2*strlen(string) to catch
all scenarios.");
SMfunctions[394] = Array ("SQL_QuoteStringFmt","Back-quotes characters in a string for database querying.
Note: The buffer\'s maximum size should be 2*strlen(string) to catch
all scenarios.");
SMfunctions[395] = Array ("SQL_ThreadQuery","Prepares and executes a threaded query.
This will not interrupt gameplay in the event of a poor/lossed
 connection, however, the interface is more complicated and
 asynchronous.  Furthermore, a new connection/disconnection is
 made for each query to simplify driver support.
The handler should look like:");
SMfunctions[396] = Array ("SQL_Execute","Executes a query.
Returns 1 if the query succeeded.
Returns 0 if the query failed.
NOTE: You can call this multiple times as long as its parent
 connection is kept open.  Each time the result set will be freed
 from the previous call.");
SMfunctions[397] = Array ("SQL_QueryError","Gets information about a failed query error.
Returns the errorcode.");
SMfunctions[398] = Array ("SQL_MoreResults","Returns 1 if there are more results to be read,
 0 otherwise.");
SMfunctions[399] = Array ("SQL_IsNull","Tells whether a specific column in the current row
 is NULL or not.");
SMfunctions[400] = Array ("SQL_ReadResult","Retrieves the current result.
A successful query starts at the first result,
 so you should not call SQL_NextRow() first.
Passing no extra params - return int
Passing one extra param - return float in 1st extra arg
Passing two extra params - return string in 1st arg, max length in 2nd
Example:
 new num = SQL_ReadResult(query, 0)
 new Float:num2
 new str[32]
 SQL_ReadResult(query, 1, num2)
 SQL_ReadResult(query, 2, str, 31)");
SMfunctions[401] = Array ("SQL_NextRow","Advances to the next result (return value should be ignored).");
SMfunctions[402] = Array ("SQL_AffectedRows","Returns the number of affected rows.");
SMfunctions[403] = Array ("SQL_NumResults","Returns the number of rows total.");
SMfunctions[404] = Array ("SQL_NumColumns","Returns the number of columns total.");
SMfunctions[405] = Array ("SQL_FieldNumToName","Returns the name of a column.
Errors on a bad field number.");
SMfunctions[406] = Array ("SQL_FieldNameToNum","Returns the number of a named column, or -1 if not found.");
SMfunctions[407] = Array ("SQL_Rewind","Rewinds a result set to the first row.");
SMfunctions[408] = Array ("SQL_GetInsertId","Returns the insert id of the last INSERT query.
Returns 0 otherwise.");
SMfunctions[409] = Array ("SQL_GetAffinity","Returns which driver this plugin is currently bound to.");
SMfunctions[410] = Array ("SQL_SetAffinity","Sets driver affinity.  You can use this to force a particular
 driver implementation.  This will automatically change all SQL
 natives in your plugin to be \\&quot;bound\\&quot; to the module in question.
If no such module is found, it will return 0.  This isn\'t necessarily bad -
 the user might have typed the wrong driver.  Unless your plugin is built
 to handle different driver types at once, you should let this error pass.
Note, that using this while you have open handles to another database
 type will cause problems.  I.e., you cannot open a handle, switch
 affinity, then close the handle with a different driver.
Switching affinity is an O(n*m) operation, where n is the number of
 SQL natives and m is the number of used natives in total.
Intuitive programmers will note that this causes problems for threaded queries.
 You will have to either force your script to work under one affinity, or to
 pack the affinity type into the query data, check it against the current, then
 set the new affinity if necessary.  Then, restore the old for safety.");
SMfunctions[411] = Array ("SQL_GetQueryString","Returns the original query string that a query handle used.");
SMfunctions[412] = Array ("SQL_NextResultSet","For queries which return multiple result sets, this advances to the next
result set if one is available.  Otherwise, the current result set is
destroyed and will no longer be accessible.
This function will always return false on SQLite, and when using threaded
queries in MySQL.  Nonetheless, it has the same effect of removing the last
result set.");
SMfunctions[413] = Array ("sqlite_TableExists","This function can be used to find out if a table in a Sqlite database exists.
(updated for newer API)");
SMfunctions[414] = Array ("SQL_SimpleQuery","Use this for executing a query where you don\'t care about the result.
Returns 0 on failure, 1 on success");
SMfunctions[415] = Array ("SQL_SimpleQueryFmt","Use this for executing a query where you don\'t care about the result.
Returns 0 on failure, 1 on success");
SMfunctions[416] = Array ("SQL_QueryAndIgnore","Use this for executing a query and not caring about the error.
Returns -1 on error, &gt;=0 on success (with number of affected rows)");
SMfunctions[417] = Array ("strlen","Calculates the length of a string.");
SMfunctions[418] = Array ("contain","Tests whether a string is found inside another string.");
SMfunctions[419] = Array ("containi","Tests whether a string is found inside another string with case ignoring.");
SMfunctions[420] = Array ("replace","Given a string, replaces the first occurrence of a search string with a
replacement string.");
SMfunctions[421] = Array ("replace_string","Given a string, replaces all occurrences of a search string with a
replacement string.");
SMfunctions[422] = Array ("replace_stringex","Given a string, replaces the first occurrence of a search string with a
replacement string.");
SMfunctions[423] = Array ("add","Concatenates one string onto another.");
SMfunctions[424] = Array ("format","Formats a string according to the AMX Mod X format rules (see documentation).");
SMfunctions[425] = Array ("formatex","Formats a string according to the AMX Mod X format rules (see documentation).");
SMfunctions[426] = Array ("vformat","Formats a string according to the AMX Mod X format rules (see documentation).");
SMfunctions[427] = Array ("vdformat","Formats a string according to the AMX Mod X format rules (see documentation).");
SMfunctions[428] = Array ("format_args","Gets parameters from function as formated string.");
SMfunctions[429] = Array ("num_to_str","Converts an integer to a string.");
SMfunctions[430] = Array ("str_to_num","Converts a string to an integer.");
SMfunctions[431] = Array ("strtol","Parses the \'string\' interpreting its content as an integral number of the specified \'base\',
which is returned as integer value. The function also sets the value of \'endPos\' to point
to the position of the first character after the number.
This is the same as C++ strtol function with a difference on second param.
The function first discards as many whitespace characters as necessary until the first
non-whitespace character is found. Then, starting from this character, takes as many
characters as possible that are valid following a syntax that depends on the \'base\' parameter,
and interprets them as a numerical value. Finally, a position of the first character following
the integer representation in \'string\' is stored in \'endPos\'.
If the value of \'base\' is zero, the syntax expected is similar to that of integer constants,
which is formed by a succession of :
   An optional sign character (+ or -)
   An optional prefix indicating octal or hexadecimal base (\\&quot;0\\&quot; or \\&quot;0x\\&quot;/\\&quot;0X\\&quot; respectively)
   A sequence of decimal digits (if no base prefix was specified) or either octal or hexadecimal digits if a specific prefix is present
If the \'base\' value is between 2 and 36, the format expected for the integral number is a succession
of any of the valid digits and/or letters needed to represent integers of the specified radix
(starting from \'0\' and up to \'z\'/\'Z\' for radix 36). The sequence may optionally be preceded by
a sign (either + or -) and, if base is 16, an optional \\&quot;0x\\&quot; or \\&quot;0X\\&quot; prefix.
If the first sequence of non-whitespace characters in \'string\' is not a valid integral number
as defined above, or if no such sequence exists because either \'string\' is empty or it contains
only whitespace characters, no conversion is performed.");
SMfunctions[432] = Array ("strtof","Parses the \'string\' interpreting its content as an floating point number and returns its value as a float.
The function also sets the value of \'endPos\' to point to the position of the first character after the number.
This is the same as C++ strtod function with a difference on second param.
The function first discards as many whitespace characters as necessary until the first
non-whitespace character is found. Then, starting from this character, takes as many
characters as possible that are valid and interprets them as a numerical value.
Finally, a position of the first character following the float representation in \'string\'
is stored in \'endPos\'.
If the first sequence of non-whitespace characters in \'string\' is not a valid float number
as defined above, or if no such sequence exists because either \'string\' is empty or it contains
only whitespace characters, no conversion is performed.");
SMfunctions[433] = Array ("float_to_str","Converts a floating point number to a string.");
SMfunctions[434] = Array ("str_to_float","Converts a string to a floating point number.");
SMfunctions[435] = Array ("equal","Returns whether two strings are equal.");
SMfunctions[436] = Array ("equali","Returns whether two strings are equal with case ignoring.");
SMfunctions[437] = Array ("copy","Copies one string to another string.");
SMfunctions[438] = Array ("copyc","Copies one string to another string until ch is found.");
SMfunctions[439] = Array ("setc","Sets string with given character.");
SMfunctions[440] = Array ("parse","Gets parameters from text.");
SMfunctions[441] = Array ("strtok","Breaks a string in two by token.");
SMfunctions[442] = Array ("strtok2","Breaks a string in two by token.");
SMfunctions[443] = Array ("trim","Removes whitespace characters from the beginning and end of a string.");
SMfunctions[444] = Array ("strtolower","Converts all chars in string to lower case.");
SMfunctions[445] = Array ("strtoupper","Converts all chars in string to upper case.");
SMfunctions[446] = Array ("ucfirst","Make a string\'s first character uppercase.");
SMfunctions[447] = Array ("isdigit","Returns whether a character is numeric.");
SMfunctions[448] = Array ("isalpha","Returns whether a character is an ASCII alphabet character.");
SMfunctions[449] = Array ("isspace","Returns whether a character is whitespace.");
SMfunctions[450] = Array ("isalnum","Returns whether a character is numeric or an ASCII alphabet character.");
SMfunctions[451] = Array ("is_char_mb","Returns if a character is multi-byte or not.");
SMfunctions[452] = Array ("is_char_upper","Returns whether an alphabetic character is uppercase.");
SMfunctions[453] = Array ("is_char_lower","Returns whether an alphabetic character is lowercase.");
SMfunctions[454] = Array ("is_str_num","Returns whether a given string contains only digits.
This returns false for zero-length strings.");
SMfunctions[455] = Array ("get_char_bytes","Returns the number of bytes a character is using.  This is
for multi-byte characters (UTF-8).  For normal ASCII characters,
this will return 1.");
SMfunctions[456] = Array ("char_to_upper","Returns an uppercase character to a lowercase character.");
SMfunctions[457] = Array ("char_to_lower","Returns a lowercase character to an uppercase character.");
SMfunctions[458] = Array ("strcat","Concatenates one string onto another.");
SMfunctions[459] = Array ("strfind","Tests whether a string is found inside another string.");
SMfunctions[460] = Array ("strcmp","Compares two strings lexographically.");
SMfunctions[461] = Array ("strncmp","Compares two strings parts lexographically.");
SMfunctions[462] = Array ("argparse","Parses an argument string to find the first argument. You can use this to
replace strbreak().");
SMfunctions[463] = Array ("argbreak","Emulates strbreak() using argparse().");
SMfunctions[464] = Array ("split_string","Returns text in a string up until a certain character sequence is reached.");
SMfunctions[465] = Array ("split","It is basically strbreak but you have a delimiter that is more than one character in length. By Suicid3.");
SMfunctions[466] = Array ("remove_filepath","Removes a path from szFilePath leaving the name of the file in szFile for a pMax length.");
SMfunctions[467] = Array ("replace_all","Replaces a contained string iteratively.");
SMfunctions[468] = Array ("explode_string","Breaks a string into pieces and stores each piece into an array of buffers.");
SMfunctions[469] = Array ("implode_strings","Joins an array of strings into one string, with a \\&quot;join\\&quot; string inserted in
between each given string.  This function complements ExplodeString.");
SMfunctions[470] = Array ("tfc_setpddata","");
SMfunctions[471] = Array ("tfc_get_user_goalitem","Team is by reference parameter that will be set to owning team(s) of the goal item.
Use the TFC_GOALITEM_* constants to determine the owning team.");
SMfunctions[472] = Array ("tfc_is_team_ally","Note: Team must be 1-&gt;4
      Team 0 will always return 0
      Any other team will result in an error");
SMfunctions[473] = Array ("Melee_Attack","damage and time length may be altered with natives.
Return PLUGIN_HANDLED to stop attack.
UNAVAILABLE IN 1.70");
SMfunctions[474] = Array ("client_powerup","Returns value of powerup. Use TSPWUP_*\'s
to find exactly which one it is.
UNAVAILABLE IN 1.70");
SMfunctions[475] = Array ("register_statsfwd","DEPRECATED");
SMfunctions[476] = Array ("IVecFVec","This is not a for loop because that\'s slower");
SMfiledata[1] = Array (0,1,2,3,4,5,6);
SMfiledata[2] = Array (7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57);
SMfiledata[4] = Array (58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85);
SMfiledata[5] = Array (86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102);
SMfiledata[7] = Array (103);
SMfiledata[8] = Array (104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156);
SMfiledata[9] = Array (157);
SMfiledata[10] = Array (158,159,160,161,162,163,164,165,166,167,168,169);
SMfiledata[11] = Array (170,171,172,173,174,175,176,177,178,179,180,181,182,183);
SMfiledata[13] = Array (184);
SMfiledata[16] = Array (185,186,187,188,189);
SMfiledata[21] = Array (190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222);
SMfiledata[24] = Array (223);
SMfiledata[25] = Array (224,225,226,227);
SMfiledata[28] = Array (228,229,230,231,232);
SMfiledata[29] = Array (233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,265,266,267,268,269,270);
SMfiledata[32] = Array (271,272,273,274);
SMfiledata[33] = Array (275);
SMfiledata[35] = Array (276);
SMfiledata[36] = Array (277,278,279,280,281,282,283,284,285,286,287,288,289,290,291,292,293,294,295);
SMfiledata[37] = Array (296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,337,338,339,340,341,342,343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,367);
SMfiledata[39] = Array (368,369,370,371,372);
SMfiledata[40] = Array (373,374,375,376,377,378,379,380,381,382);
SMfiledata[41] = Array (383);
SMfiledata[42] = Array (384,385,386,387);
SMfiledata[43] = Array (388,389,390,391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,409,410,411,412,413,414,415,416);
SMfiledata[44] = Array (417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,464,465,466,467,468,469);
SMfiledata[47] = Array (470,471,472);
SMfiledata[50] = Array (473,474);
SMfiledata[52] = Array (475);
SMfiledata[54] = Array (476);
