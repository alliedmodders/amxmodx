/* AMX Mod X functions
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _amxmodx_included
  #endinput
#endif
#define _amxmodx_included

#include <core>
#include <float>
#include <amxconst>
#include <string>
#include <file>
#include <vault>

/* Function is called just after server activation.
* Good place for configuration loading, commands and cvars registration. */
forward plugin_init();

/* Function is called when all plugin_init from plugins
* were called, so all commmands and cvars should be already registered. */
forward plugin_cfg();

/* Function called before plugin unloading (server deactivation) */
forward plugin_end();

/* Called on log message. */
forward plugin_log();

/* Use here model_precache() and sound_precache() functions. */
forward plugin_precache();

/* Whenever player info is changed, this function is called. */
forward client_infochanged(id);

/* Called on client connection. */
forward client_connect(id);

/* Called when client gets valid STEAM id (usually
* between client_connect() and client_putinserver()). */
forward client_authorized(id);

/* Called when client is disconnecting from server. */
forward client_disconnect(id);

/* Called when client is sending command. */
forward client_command(id);

/* Called when client is entering to a game. */
forward client_putinserver(id);

/* Sets informations about plugin. */
native register_plugin(const plugin_name[],const version[],const author[]);

/* Precache model. Can be used only in plugin_precache() function.*/
native precache_model(const name[]);

/* Precache sound. Can be used only in plugin_precache() function.*/
native precache_sound(const name[]);

/* Sets info for player. */
native set_user_info(index,const info[],const value[]);

/* Gets info from player. */
native get_user_info(index,const info[],output[],len);

/* Sets info for server. */
native set_localinfo(const info[],const value[]);

/* Gets info from server. */
native get_localinfo(const info[],output[],len);

/* Shows text in MOTD window. When there is no header, the MOTD title 
* will be the name of server. If message is filename, then a contents
* of this file will be displayed as MOTD. */
native show_motd(player,const message[],const header[]="");

/* Sends message to player. Set index to 0 to send text globaly. */
native client_print(index,type,const message[],{Float,_}:...);

/* Sends message to player by engine. Set index to 0 to send text globaly. */
native engclient_print(player,type,const message[],{Float,_}:...);

/* Sends message to console. */
native console_print(id,const message[],{Float,_}:...);

/* Sends command to console. */
native console_cmd(id,const cmd[],{Float,_}:...);

/* Registers event on which a given function will be called
* Flags:
* "a" - global event.
* "b" - specified.
* "c" - send only once when repeated to other players.
* "d" - call if is send to dead player.
* "e" - to alive.
* Examples for conditions:
* "2=c4" - 2nd parameter of message must be sting "c4".
* "3>10" - 3rd parameter must be greater then 10.
* "3!4" - 3rd must be different from 4.
* "2&Buy" - 2nd parameter of message must contain "Buy" substring.
* "2!Buy" - 2nd parameter of message can't contain "Buy" substring. */
native register_event(const event[],const function[],const flags[],cond[]="", ... );

/* Registers log event on which the given function will be called
* Examples for conditions:
* "0=World triggered" "1=Game_Commencing"
* "1=say"
* "3=Terrorists_Win"
* "1=entered the game"
* "0=Server cvar"
*/
native register_logevent(const function[], argsnum,  ... );

/* Sets format for hudmessage. */
native set_hudmessage(red=200, green=100, blue=0, Float:x=-1.0, Float:y=0.35, effects=0, Float:fxtime=6.0, Float:holdtime=12.0, Float:fadeintime=0.1, Float:fadeouttime=0.2,channel=4);

/* Displays HUD message to given player. */
native show_hudmessage(index,const message[],{Float,_}:...);

/* Displays menu. Keys have bit values (key 1 is (1<<0), key 5 is (1<<4) etc.). */
native show_menu(index,keys,const menu[], time = -1);

/* Gets value from client messages.
* When you are asking for string the array and length is needed (read_data(2,name,len)).
* Integer is returned by function (new me = read_data(3)).
* Float is set in second parameter (read_data(3,value)). */
native read_data(value, {Float,_}:... );

/* Returns number of values in client message. */
native read_datanum();

/* Gets log message. Can be called only in plugin_log() forward function. */
native read_logdata(output[],len);

/* Returns number of log arguments. 
* Can be called only in plugin_log() forward function. */
native read_logargc();

/* Gets log argument indexed from 0.
* Can be called only in plugin_log() forward function. */
native read_logargv(id,output[],len);

/* Parse log data about user ( "Butcher<5><BOT><TERRORIST>" etc. ). */
native parse_loguser(const text[], name[], nlen, &userid = -2, authid[] = "", alen = 0, team[]="", tlen=0);

/* Prints message to server console.
* You may use text formating (f.e. server_print("%-32s %.2f!","hello",7.345)) */
native server_print(const message[], {Float,_}:...);

/* Returns 1 or 0. */
native is_map_valid(const mapname[]);

/* Returns 1 or 0. */
native is_user_bot(index);

/* Returns 1 or 0. */
native is_user_hltv(index);

/* Returns 1 or 0. */
native is_user_connected(index);

/* Returns 1 or 0. */
native is_user_connecting(index);

/* Returns 1 or 0. */
native is_user_alive(index);

/* Returns 1 or 0. */
native is_dedicated_server();

/* Returns 1 or 0. */
native is_linux_server();

/* If player is not attacked function returns 0, in other
* case returns index of attacking player. On second and third
* parameter you may get info about weapon and body hit place. */
native get_user_attacker(index,...);

/* If player doesn't hit at anything function returns 0.0,
* in other case the distance between hit point and player is returned.
* If player is aiming at another player then the id and part of body are set. */
native Float:get_user_aiming(index,&id,&body,dist=9999);

/* Returns player frags. */
native get_user_frags(index);

/* Returns player armor. */
native get_user_armor(index);

/* Returns player deaths. */
native get_user_deaths(index);

/* Sets player deaths. */
native set_user_deaths(index, newdeaths);

/* Sets player frags. */
native set_user_frags(index, frags);

/* Returns player health. */
native get_user_health(index);

/* Returns index. */
native get_user_index(const name[]);

/* Returns ip. */
native get_user_ip(index,ip[],len, without_port = 0);

/* Returns id of currently carried weapon. Gets also 
* ammount of ammo in clip and backpack. */
native get_user_weapon(index,&clip,&ammo);

/* Gets ammo and clip from current weapon. */
native get_user_ammo(index,weapon,&clip,&ammo);

/* Converts numbers from range 0 - 999 to words. */
native num_to_word(num,output[],len);

/* Returns team id. When length is greater then 0
* then a name of team is set. */
native get_user_team(index, team[]="", len = 0);

/* Returns player playing time in seconds. 
* If flag is set then result is without connection time. */
native get_user_time(index, flag = 0);

/* Gets ping and loss at current time. */
native get_user_ping(index, &ping, &loss);

/* Gets origin from player.
* Modes:
* 0 - current position.
* 1 - position from eyes (weapon aiming).
* 2 - end position from player position.
* 3 - end position from eyes (hit point for weapon). 
* 4 - position of last bullet hit (only CS). */
native get_user_origin(index, origin[3], mode = 0);

/* Returns all carried weapons as bit sum. Gets
* also theirs indexes. */
native get_user_weapons(index,weapons[32],&num);

/* Returns weapon name. */
native get_weaponname(id,weapon[],len);

/* Returns player name. */
native get_user_name(index,name[],len);

/* Gets player authid. */
native get_user_authid(index, authid[] ,len);

/* Returns player userid. */
native get_user_userid(index);

/* Slaps player with given power. */
native user_slap(index,power,rnddir=1);

/* Kills player. When flag is set to 1 then death won't decrase frags. */
native user_kill(index,flag=0);

/* Logs something into the current amx logfile
* Parameters:
*  string[] - format string
*  ...      - optional parameters
* Return value:
*  always 0 */
native log_amx(const string[], {Float,_}:...);

/* Sends message to standard HL logs. */
native log_message(const message[],{Float,_}:...);

/* Sends log message to specified file. */
native log_to_file(const file[],const message[],{Float,_}:...);

/* Returns number of players put in server. 
* If flag is set then also connecting are counted. */
native get_playersnum(flag=0);

/* Sets indexes of players.
* Flags:
* "a" - don't collect dead players.
* "b" - don't collect alive players.
* "c" - skip bots.
* "d" - skip real players.
* "e" - match with team.
* "f" - match with part of name.
* "g" - ignore case sensitivity.
* Example: Get all alive CTs: get_players(players,num,"ae","CT") */
native get_players(players[32], &num ,const flags[]="", const team[]="");

/* Gets argument from command. */
native read_argv(id,output[],len);

/* Gets line of all arguments. */
native read_args(output[],len);

/* Returns number of arguments (+ one as command). */
native read_argc();

/* Converts string to sum of bits.
* Example: "abcd" is a sum of 1, 2, 4 and 8. */
native read_flags(const flags[]);

/* Converts sum of bits to string.
* Example: 3 will return "ab". */
native get_flags(flags,output[],len);

/* Find player.
* Flags:
* "a" - with given name.
* "b" - with given part of name.
* "c" - with given authid.
* "d" - with given ip.
* "e" - with given team name.
* "f" - don't look in dead players.
* "g" - don't look in alive players.
* "h" - skip bots.
* "i" - skip real players.
* "j" - return index of last found player. 
* "k" - with given userid.
* "l" - ignore case sensitivity. */
native find_player(const flags[], ... );

/* Removes quotes from sentence. */
native remove_quotes(text[]);

/* Executes command on player. */
native client_cmd(index,const command[],{Float,_}:...);

/* This is an emulation of a client command (commands aren't send to client!).
* It allows to execute some commands on players and bots.
* Function is excellent for forcing to do an action related to a game (not settings!).
* The command must stand alone but in arguments you can use spaces. */
native engclient_cmd(index,const command[],arg1[]="",arg2[]="");

/* Executes command on a server console. */
native server_cmd(const command[],{Float,_}:...);

/* Sets a cvar to given value. */
native set_cvar_string(const cvar[],const value[]);

/* If a cvar exists returns 1, in other case 0 */
native cvar_exists(const cvar[]);

/* Removes a cvar flags (not allowed for amx_version,
* fun_version and sv_cheats cvars). */
native remove_cvar_flags(const cvar[],flags = -1);

/* Sets a cvar flags (not allowed for amx_version,
* fun_version and sv_cheats cvars). */
native set_cvar_flags(const cvar[],flags);

/* Returns a cvar flags. */
native get_cvar_flags(const cvar[]);

/* Sets a cvar to given float. */
native set_cvar_float(const cvar[],Float:value);

/* Gets a cvar float. */
native Float:get_cvar_float(const cvarname[]);

/* Gets a cvar integer value. */
native get_cvar_num(const cvarname[]);

/* Sets a cvar with integer value. */
native set_cvar_num(const cvarname[],value);

/* Reads a cvar value. */
native get_cvar_string(const cvarname[],output[],iLen);

/* Returns a name of currently played map. */
native get_mapname(name[],len);

/* Returns time remaining on map in seconds. */
native get_timeleft();

/* Returns a game time. */
native Float:get_gametime();

/* Returns maxplayers setting. */
native get_maxplayers();

/* Returns a name of currently played mod. */
native get_modname(name[],len);

/* Returns time in given format. The most popular is: "%m/%d/%Y - %H:%M:%S". */
native get_time(const format[],output[],len);

/* Returns time in given format. The most popular is: "%m/%d/%Y - %H:%M:%S".
* Last parameter sets time to format. */
native format_time(output[],len, const format[],time = -1);

/* Returns system time in seconds elapsed since 00:00:00 on January 1, 1970. 
* Offset is given in seconds.*/
native get_systime(offset = 0);

/* Returns time in input and additionaly fills missing information
* with current time and date. If time is different than -1 then parsed
* time is added to given time.
* Example:
* parset_time( "10:32:54 04/02/2003", "%H:%M:%S %m:%d:%Y" )
* For more information see strptime(...) function from C libraries. */
native parse_time(const input[],const format[], time = -1);

/* Calls function on specified time.
* Flags:
* "a" - repeat.
* "b" - loop task.
* "c" - do task on time after a map timeleft.
* "d" - do task on time before a map timelimit. */
native set_task(Float:time,const function[],id = 0,parameter[]="",len = 0,flags[]="", repeat = 0);

/* Removes all tasks with given id. If outside var is 
* set then a task can be removed also when
* was set in another plugin. */
native remove_task(id = 0, outside = 0);

/* Returns 1 if task under given id exists. */
native task_exists(id = 0, outside = 0);

/* Sets flags for player. Set flags to -1 if you want to clear all flags.
* You can use different settings by changing the id, which is from range 0 - 31. */
native set_user_flags(index,flags=-1,id=0);

/* Gets flags from player. Set index to 0 if you want to read flags from server. */
native get_user_flags(index,id=0);

/* Removes flags for player. */
native remove_user_flags(index,flags=-1,id=0);

/* Registers function which will be called from client console. */
native register_clcmd(const client_cmd[],const function[],flags=-1, info[]="");

/* Registers function which will be called from any console. */
native register_concmd(const cmd[],const function[],flags=-1, info[]="");

/* Registers function which will be called from server console. */
native register_srvcmd(const server_cmd[],const function[],flags=-1, info[]="");

/* These functinos are used to generate client messages.
* You may generate menu, smoke, shockwaves, thunderlights,
* intermission and many many others messages.
* See HL SDK for more examples. */
native message_begin( dest, msg_type, origin[3]={0,0,0},player=0);
native message_end();
native write_byte( x );
native write_char( x );
native write_short( x );
native write_long( x );
native write_entity( x );
native write_angle( x );
native write_coord( x );
native write_string( x[] );

/* Gets info about client command. */
native get_clcmd(index, command[], len1, &flags, info[], len2, flag);

/* Returns number of registered client commands. */
native get_clcmdsnum(flag);

/* Gets info about server command. */
native get_srvcmd(index,server_cmd[],len1,&flags, info[],len2, flag);

/* Returns number of registered server commands. */
native get_srvcmdsnum(flag);

/* Gets info about console command. If id is set to 0,
then function returns only server cmds, if positive then
returns only client cmds. in other case returns all console commands. */
native get_concmd(index,cmd[],len1,&flags, info[],len2, flag, id = -1);

/* Returns number of registered console commands. */
native get_concmdsnum(flag,id = -1);

/* Gets unique id of menu. Outside set to 1 allows
* to catch menus outside a plugin where register_menuid is called. */
native register_menuid(const menu[], outside=0 );

/* Calls function when player uses specified menu and proper keys. */
native register_menucmd(menuid,keys, const function[] );

/* Gets what menu the player is watching and what keys for menu he have.
* When there is no menu the index is 0. If the id is negative then the menu
* is VGUI in other case the id is from register_menuid() function. */
native get_user_menu(index,&id,&keys);

/* Forces server to execute sent server command at current time.
* Very useful for map changes, setting cvars and other activities. */
native server_exec();

/* Emits sound. Sample must be precached. */
native emit_sound(index, channel, sample[], Float:vol, Float:att,flags, pitch);

/* Returns distance between two vectors. */
native get_distance(origin1[3],origin2[3]);

/* Registers new cvar for HL engine. */
native register_cvar(const name[],const string[],flags = 0,Float:fvalue = 0.0);

/* Generates random floating point number from a to b. */
native Float:random_float(Float:a,Float:b);

/* Generates random integer from a to b. */
native random_num(a,b);

/* Returns id of client message.
* Example: get_user_msgid("TextMsg"). */
native get_user_msgid(const name[]);

/* Checks if public variable with given name exists in loaded plugins. */
native xvar_exists( const name[] );

/* Returns an unique id for public variable specified by name. If such
* variable doesn't exist then returned value is -1. */
native get_xvar_id( const name[] );

/* Returns an integer value of a public variable. Id is a value
* returned by get_xvar_id(...) native. */
native get_xvar_num( id );

/* Returns a float value of a public variable. Id is a value
* returned by get_xvar_id(...) native. */
native Float:get_xvar_float( id );

/* Sets a value of a public variable. Id is a value
* returned by get_xvar_id(...) native. */
native set_xvar_num( id, value = 0 );

/* Sets a float value of a public variable. Id is a value
* returned by get_xvar_id(...) native. */
native set_xvar_float( id, Float:value = 0.0 );

/* Checks whether a module is loaded. If it is not, the return value is 0, otherwise
* the return value is non-zero. The function is case insensitive. */
native is_module_loaded(const name[]);

/* Gets info about a module.
* Parameters:
*  id         - the id of the module
*  name[]     - The name of the module will be stored here
*  nameLen    - maximal length of the name
*  author[]   - the author will be stored here
*  authorLen  - maximal length of the author
*  version[]  - the version of the module will be stored here
*  versionLen - maximal length of the version
*  status - the status of the module will be stored here
* Return value:
*  id         - success
*  -1         - module not found */
native get_module(id, name[], nameLen, author[], authorLen, version[], versionLen, &status);

/* Returns number of currently registered modules */
native get_modulesnum();

/* Checks whether a plugin is loaded. If it is not, the return value is 0, otherwise
* the return value is non-zero. The function is case insensitive. */
native is_plugin_loaded(const name[]);

/* Gets info about plugin by given index.
* Function returns -1 if plugin doesn't exist with given index. */
native get_plugin(index,filename[],len1,name[],len2,version[],len3,author[],len4,status[],len5);

/* Returns number of all loaded plugins. */
native get_pluginsnum();

/* Pauses function or plugin so it won't be executed.
* In most cases param1 is name of function and
* param2 name of plugin (all depends on flags).
* Flags:
* "a" - pause whole plugin.
* "b" - pause function.
* "c" - look outside the plugin (by given plugin name).
* "d" - set "stopped" status when pausing whole plugin.
* "e" - set "locked" status when pausing whole plugin.
*       In this status plugin is unpauseable.
* Example: pause("ac","myplugin.amx") 
*          pause("bc","myfunc","myplugin.amx") */
native pause(flag[], const param1[]="",const param2[]="");

/* Unpauses function or plugin.
* Flags:
* "a" - unpause whole plugin.
* "b" - unpause function.
* "c" - look outside the plugin (by given plugin name). */
native unpause(flag[], const param1[]="",const param2[]="");

/* Call a function in this / an another plugin by name.
* Parameters:
*  plugin - plugin name; if "", the caller plugin is used.
*           If specified, it has to be the exact name (for example stats.amx)
*  func   - function name
* Return value:
*   1     - Success
*   0     - Runtime error
*  -1     - Plugin not found
*  -2     - Function not found */
native callfunc_begin(const plugin[]="", const func[]);

/* Push a parameter (integer, string, float) */
native callfunc_push_int(value);
native callfunc_push_str(value[]);
native callfunc_push_float(Float: value);
native callfunc_push_intrf(&value);
native callfunc_push_floatrf(& Float: value);

/* Make the actual call.
* Return value of the function called. */
native callfunc_end();

/* Called on inconsistent file. You can put any text
* into reason to change an original message. */
forward inconsistent_file(id,const filename[], reason[64] );

/* Forces the client and server to be running with the same
* version of the specified file ( e.g., a player model ). */
native force_unmodified(force_type, mins[3] , maxs[3], const filename[]);
