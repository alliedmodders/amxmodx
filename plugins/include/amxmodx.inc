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
#include <lang>
#include <messages>
#include <vector>
#include <sorting>
#include <cellarray>
#include <cellstack>
#include <celltrie>
#include <datapack>
#include <newmenus>

/**
 * Called just after server activation.
 *
 * @note Good place to initialize most of the plugin, such as registering
 *       cvars, commands or forwards, creating data structures for later use or
 *       generating and loading other required configuration.
 *
 * @noreturn
 */
forward plugin_init();

/**
 * Called just before the plugin is paused from execution.
 *
 * @noreturn
 */
forward plugin_pause();


/**
 * Called just after the plugin is unpaused.
 *
 * @noreturn
 */
forward plugin_unpause();

/**
 * Called when the mod tries to change the map.
 *
 * @note This is *only* called if the mod itself handles the map change. The
 *       server command "changelevel" which is also used by plugins will not
 *       trigger this forward. Unfortunately this means that in practice this
 *       forward is very unreliable, and will not be called in many situations.
 *
 * @param map   Map that the mod tries to change to
 *
 * @return      PLUGIN_CONTINUE to let the mod change the map
 *              PLUGIN_HANDLED or higher to prevent the map change
 */
forward server_changelevel(map[]);

/**
 * Called when all plugins went through plugin_init
 *
 * @note When this forward is called most plugins should have registered their
 *       cvars and commands already.
 *
 * @noreturn
 */
forward plugin_cfg();

/**
 * Called just before server deactivation and subsequent
 * unloading of the plugin.
 *
 * @note The plugin is required to manually free Handles it has acquired, such
 *       as those from dynamic data structures. Failing to do that will result
 *       in the plugin and AMXX leaking memory.
 *
 * @noreturn
 */
forward plugin_end();

/**
 * Called when a message is about to be logged.
 *
 * @note Message data and information can be retrieved using the read_log* set
 *       of functions.
 *
 * @return      PLUGIN_CONTINUE to let the log message through
 *              PLUGIN_HANDLED or higher to stop the log message
 */
forward plugin_log();

/**
 * This forward allows you to add models, sounds and generic files to the
 * precache tables using the precache_* set of functions.
 *
 * @note Adding files to the precaching tables will trigger the client to
 *       download them to its local filesystem.
 * @note There is a hard upper limit of entries in the precaching tables for
 *       every game, this limit is 512 in most cases. The entries will be filled
 *       and indexed incrementally. Going over this limit will crash the server.
 *
 * @noreturn
 */
forward plugin_precache();

/**
 * Called when a clients info has changed
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_infochanged(id);

/**
 * Called when a client is connecting.
 *
 * @note This forward is called too early to do anything that directly affects
 *       the client.
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_connect(id);

/**
 * Called when the client gets a valid SteamID.
 *
 * @note This may occur before or after client_putinserver has been called.
 * @note This is called for bots, and the SteamID will be "BOT"
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_authorized(id);

/**
 * Called when a client is disconnecting from the server.
 *
 * @note By this point it is already too late to do anything that directly
 *       affects the client.
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_disconnect(id);

/**
 * Called when a client attempts to execute a command.
 *
 * @note The command and its arguments can be read using the read_arg* set of
 *       functions.
 *
 * @param id    Client index
 *
 * @return      PLUGIN_CONTINUE to let the client execute the command
 *              PLUGIN_HANDLED or higher to stop the command
 */
forward client_command(id);

/**
 * Called when a client is entering the game.
 *
 * @note It is not defined whether the client already has a SteamID when this
 *       forward is called. client_authorized may occur either before or after
 *       this.
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_putinserver(id);

/**
 * Sets informations about the calling plugin.
 *
 * @param plugin_name   Name of the plugin
 * @param version       Version of the plugin
 * @param author        Author of the plugin
 *
 * @return              Plugin id of the calling plugin
 */
native register_plugin(const plugin_name[], const version[], const author[]);

/**
 * Precaches a model file.
 *
 * @note Can only be used inside of the plugin_precache() forward.
 *
 * @param name  Path to the model file
 *
 * @return      Cache id of the model, even if the file was already precached
 * @error       If called outside of the plugin_precache() forward, an error is
 *              thrown.
 */
native precache_model(const name[]);

/**
 * Precaches a sound file.
 *
 * @note Can only be used inside of the plugin_precache() forward.
 * @note The filepath is always relative to the "sound" folder, and the file has
 *       to be a wav file. Precaching a file with this will add it to the engine
 *       sound table, making it available for usage in emit_sound for example.
 * @note Precaching other filetypes (such as mp3 music), optionally in different
 *       locations, has to be done with precache_generic.
 *
 *
 * @param name  Path to the sound file
 *
 * @return      Cache id of the sound, even if the file was already precached
 * @error       If called outside of the plugin_precache() forward, an error is
 *              thrown.
 */
native precache_sound(const name[]);

/**
 * Precaches a generic file.
 *
 * @note Can only be used inside of the plugin_precache() forward.
 * @note Precaching sounds with this will not add them to the engine sound table
 *
 * @param szFile    Path to the file
 *
 * @return          Cache id of the file, even if the file was already precached
 * @error           If called outside of the plugin_precache() forward, an error
 *                  is thrown.
 */
native precache_generic(const szFile[]);

/**
 * Sets info on the client.
 *
 * @param index     Client index
 * @param info      Info key
 * @param value     New value
 *
 * @noreturn
 * @error           If the index is not within the range of 1 to MaxClients or
 *                  the client is not connected an error will be thrown.
 * @error           If called outside of the plugin_precache() forward, an error
 *                  is thrown.
 */
native set_user_info(index,const info[], const value[]);

/**
 * Gets info from the client.
 *
 * @param index     Client index
 * @param info      Info key
 * @param output    Buffer to copy value to
 * @param len       Maximum size of the buffer
 *
 * @return          Number of cells written to buffer
 * @error           If the index is not within the range of 1 to MaxClients or
 *                  the client is not connected an error will be thrown.
 */
native get_user_info(index, const info[], output[], len);

/**
 * Sets info on the server.
 *
 * @param info      Info key
 * @param value     New value
 *
 * @noreturn
 */
native set_localinfo(const info[], const value[]);

/**
 * Gets info from the server.
 *
 * @param info      Info key
 * @param output    Buffer to copy value to
 * @param len       Maximum size of the buffer
 *
 * @return          Number of cells written to buffer
 */
native get_localinfo(const info[], output[], len);

/**
 * Shows text or a file in MOTD window.
 *
 * @param player    Client index, use 0 to display to all players
 * @param message   Message to display inside the MOTD window. If this is a
 *                  filename the contents of this file will be displayed.
 * @param header    Text for the MOTD header. If this is empty the servers
 *                  hostname will be displayed instead.
 *
 * @noreturn
 */
native show_motd(player, const message[], const header[]="");

/**
 * Sends a message to the client.
 *
 * @param index     Client index, use 0 to display to all players
 * @param type      Message type, see print_* destination constants in amxconst
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 *                  If 0 is specified as the index then 0 will be returned if
 *                  nothing has been sent. The number of printed characters will
 *                  otherwise refer to the message that is sent last, to the
 *                  client with the highest index.
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native client_print(index, type, const message[], any:...);

/**
 * Sends colored chat messages to clients.
 *
 * @note This only works in Counter-Strike 1.6 and Condition Zero.
 * @note The colors can be modified inside of the format string using special
 *       characters. These characters can be included using the escape character
 *          green           x04   ; use location color from this point forward
 *          red/blue/grey   x03   ; use team color from this point forward
 *          red/blue/grey   x02   ; use team color to the end of the player name
 *                                ; This only works at the start of the string,
 *                                ; and precludes using other control characters
 *          default         x01   ; use default color from this point forward
 * @note The team color is defined by the sender's index or a specific team
 *       color using the print_team_* constants in amxconst
 * @note Usage examples:
 *       client_print_color(id, print_team_red, "^4Green ^3Red ^1Default")
 *       client_print_color(id, id2, "^4Green ^3id2's team color, ^1Default")
 * @note Including colors in ML can be done using the same escaping method:
 *       EXAMPLE_ML_KEY = ^4Green ^3Team color ^1Default.
 *
 * @param index     Client index, use 0 to display to all players
 * @param sender    Client index used as the sender, defining the team color
 *                  used in the message. Use print_team_* constants to force
 *                  a specific color.
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 *                  If 0 is specified as the index then 0 will be returned if
 *                  nothing has been sent. The number of printed characters will
 *                  otherwise refer to the message that is sent last, to the
 *                  client with the highest index.
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native client_print_color(index, sender, const message[], any:...);

/**
 * Sends a message to the client via the engine.
 *
 * @param player    Client index, use 0 to display to all players
 * @param type      Message type, see print_* destination constants in amxconst
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 *                  If 0 is specified as the index then 0 will be returned if
 *                  nothing has been sent. The number of printed characters will
 *                  otherwise refer to the message that is sent last, to the
 *                  client with the highest index.
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native engclient_print(player, type, const message[], any:...);

/**
 * Sends a message to the console of a client or the server.
 *
 * @param index     Client index, or 0 to print to the server console
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native console_print(id, const message[], any:...);

/**
 * Executes a command from the specified client or the server console.
 *
 * @param id    Client index, or 0 to print to the server console
 * @param cmd   Formatting rules
 * @param ...   Variable number of formatting parameters
 *
 * @return      The length of the formatted command.
 */
native console_cmd(id, const cmd[], any:...);

/**
 * Registers a function to be called on a given game event.
 *
 * @note Examples for event conditions:
 *       "2=c4" - Second parameter of message must be the string "c4"
 *       "3>10" - Third parameter of message must be greater than 10
 *       "3!4" - Third parameter of message must not be equal to 4
 *       "2&Buy" - Second parameter of message must contain "Buy" substring
 *       "2!Buy" - Second parameter of message must not equal "Buy"
 * @note Due to a long-standing bug that would break compatibility with older
 *       plugins, the client id should be checked for alive/dead state if you
 *       use flags "d" or "e".
 *
 * @param event     Name of event that should be hooked
 * @param function  Name of callback function
 * @param flags     Flags used for filtering events, the valid flags are:
 *                  "a" - Global event (sent to every client)
 *                  "b" - Event sent to single client
 *                  "c" - Call only once when repeated to multiple clients
 *                  "d" - Call only if sent to dead player
 *                  "e" - Call only if sent to alive player
 *                  "f" - Call only if sent to human player ("b" flag required)
 *                  "g" - Call only if sent to bot ("b" flag required)
 * @param cond      Condition string used for filtering events, built as:
 *                  "<argument number><comparison operator><value>"
 *                  Argument number is the argument position to be filtered
 *                  The comparison operator may be:
 *                    - "=" for equality comparison (all argument types)
 *                    - "!" for inequality comparison (all argument types)
 *                    - "&" for bitwise and (int argument) or substring
 *                      comparison (string argument)
 *                    - "<" for less than comparison (int/float arguments)
 *                    - ">" for greater than comparison (int/float arguments)
 *                  The argument is compared to the specified value accordingly
 * @param ...       Any number of additional conditions
 *
 * @return          1 on successfully registering event
 *                  0 on failure
 * @error           Invalid event name or invalid callback function
 */
native register_event(const event[], const function[], const flags[], const cond[]="", ...);

/**
 * Registers a function to be called on a given log event.
 *
 * @note Examples for log conditions:
 *       "0=World triggered" "1=Game_Commencing"
 *       "1=say"
 *       "3=Terrorists_Win"
 *       "1=entered the game"
 *       "0=Server cvar"
 *
 * @param function  Name of callback function
 * @param argsnum   Number of arguments of the log event
 * @param ...       Any number of conditions used for filtering events
 *                  A condition string is built as:
 *                  "<argument number><comparison operator><string>"
 *                  Argument number is the argument position to be filtered
 *                  The comparison operator may be:
 *                    - "=" for equality comparison
 *                    - "&" for substring comparison
 *                  The argument is compared to the specified string accordingly
 *
 * @return          1 on successfully registering event, 0 on failure
 * @error           Invalid callback function
 */
native register_logevent(const function[], argsnum,  ...);

/**
 * Sets display parameters for hudmessages.
 *
 * @note As of AMXX 1.61, setting the channel to -1 will automatically choose
 *       the next available HUD channel for the client.
 * @note There are four different HUD channels available on the client (1-4).
 *       Sending a hudmessage to a channel will overwrite any existing messages
 *       already displaying on that channel.
 * @note If you plan to create a permanent message don't forget to specify a
 *       specific channel to avoid possible flickering due to auto-channeling
 * @note For the hudmessage coordinates x and y, -1.0 will center the message
 *       on the respective axis.
 * @note These parameters stay until the next call to set_hudmessage overwrites
 *       them. Multiple calls to show_hudmessage will therefore re-use the same
 *       parameters. The parameters are not stored per-plugin, so other plugins
 *       can overwrite them.
 *
 * @param red           Red component of hudmessage color
 * @param green         Green component of hudmessage color
 * @param blue          Blue component of hudmessage color
 * @param x             Location of the message on the x axis in percent
 * @param y             Location of the message on the y axis in percent
 * @param effects       Display effect
 * @param fxtime        Duration of the effect
 * @param holdtime      Time the message stays on screen
 * @param fadeintime    Time it takes the message to fully appear (fade-in)
 * @param fadeouttime   Time it takes the message to fully disappear (fade-out)
 * @param channel       Channel to use on the client
 *
 * @noreturn
 */
native set_hudmessage(red=200, green=100, blue=0, Float:x=-1.0, Float:y=0.35, effects=0, Float:fxtime=6.0, Float:holdtime=12.0, Float:fadeintime=0.1, Float:fadeouttime=0.2,channel=-1);

/**
 * Displays a message on the client HUD.
 *
 * @note Use set_hudmessage to define how the message should look on screen.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 *                  If 0 is specified as the index then 0 will be returned if
 *                  nothing has been sent. The number of printed characters will
 *                  otherwise refer to the message that is sent last, to the
 *                  client with the highest index.
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native show_hudmessage(index, const message[], any:...);

/**
 * Sets display parameters for director hudmessages.
 *
 * @note For the hudmessage coordinates x and y, -1.0 will center the message
 *       on the respective axis.
 * @note These parameters stay until the next call to set_dhudmessage overwrites
 *       them. Multiple calls to show_dhudmessage will therefore re-use the same
 *       parameters. The parameters are not stored per-plugin, so other plugins
 *       can overwrite them.
 *
 * @param red           Red component of hudmessage color
 * @param green         Green component of hudmessage color
 * @param blue          Blue component of hudmessage color
 * @param x             Location of the message on the x axis in percent
 * @param y             Location of the message on the y axis in percent
 * @param effects       Display effect
 * @param fxtime        Duration of the effect
 * @param holdtime      Time the message stays on screen
 * @param fadeintime    Time it takes the message to fully appear (fade-in)
 * @param fadeouttime   Time it takes the message to fully disappear (fade-out)
 *
 * @noreturn
 */
native set_dhudmessage(red=200, green=100, blue=0, Float:x=-1.0, Float:y=0.35, effects=0, Float:fxtime=6.0, Float:holdtime=12.0, Float:fadeintime=0.1, Float:fadeouttime=0.2);

/**
 * Displays a director message on the client HUD.
 *
 * @note Use set_dhudmessage to define how the message should look on screen.
 * @note Unlike the classic HUD message which is channel-based, director
 *       messages are stack-based. You can have up to 8 messages displaying at
 *       once, if you add more they will be overwtitten in the order they were
 *       sent. There is no way to clear a specific message.
 * @note The message has a maximum length of 128 characters which this function
 *       will automatically enforce.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 *                  If 0 is specified as the index then 0 will be returned if
 *                  nothing has been sent. The number of printed characters will
 *                  otherwise refer to the message that is sent last, to the
 *                  client with the highest index.
 * @error           If a single client is specified and the index is not
 *                  within the range of 1 to MaxClients an error will be thrown
 */
native show_dhudmessage(index, const message[], any:...);

/* Displays menu. Keys have bit values (key 1 is (1<<0), key 5 is (1<<4) etc.). */
/**
 * Displays a menu to the client.
 *
 * @note Keys is a bitflag value that represents which keys the user can press
 *       on the menu. If you want to display disabled menu options or skip
 *       certain number slots you should exclude that key from the bitflag.
 *       amxconst.inc provides MENU_KEY_* constants for convenience.
 * @note If a menu timeout is specified it does not automatically overwrite
 *       the menu on the client's screen. But if a client acts upon a timeouted
 *       displayed menu that action will not be sent to the plugin.
 * @note The title parameter is not displayed to the client and is only used for
 *       identifying menus internally and assigning them to their callbacks.
 *       The title corresponds to the menu name that you register with
 *       register_menuid().
 *
 * @param index     Client to display menu to, use 0 to display to all clients
 * @param keys      Enabled keys
 * @param menu      Menu body
 * @param time      Menu timeout in seconds, -1 to disable
 * @param title     Name of the menu for internal tracking purposes
 */
native show_menu(index,keys,const menu[], time = -1, const title[] = "");

/**
 * Retrieves values from a client message.
 *
 * @note For use within callbacks registered with register_event().
 * @note Usage examples:
 *       value = read_data(1);
 *       read_data(2, floatvalue);
 *       written = read_data(3, buffer, buffersize);
 *
 *
 * @param value     Argument number to retrieve value from
 * @param ...       If 0 additional parameters are provided, the function
 *                  will return the argument value directly.
 *                  If 1 additional parameter is provided, the function will
 *                  store a float value in that second parameter.
 *                  If two additional parameters are provided, the function
 *                  will copy a string to the buffer provided in the second
 *                  parameter, using the third as the maximum buffer size.
 *
 * @return          If zero additional parameters are provided, the function
 *                  will return an integer value.
 *                  If one additional parameter is provided, the function will
 *                  return the float value, converted (truncated) to an integer.
 *                  If two additional parameters are provided, the function
 *                  will return the number of cells written to the buffer.
 */
native read_data(value, any:... );

/**
 * Returns the number of values in the client message.
 *
 * @note For use within callbacks registered with register_event().
 *
 * @return      Number of values in client message
 */
native read_datanum();

/**
 * Returns the message id of the client message.
 *
 * @note For use within callbacks registered with register_event().
 *
 * @return      Message id of the client message
 */
native read_datatype();

/**
 * Retrieves current log message.
 *
 * @note Can only be used inside of the plugin_log() forward.
 *
 * @param output    Buffer to copy log message to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 * @error           If called outside of the plugin_log() forward, an error is
 *                  thrown.
 */
native read_logdata(output[],len);

/**
 * Returns number of log message arguments.
 *
 * @note Can only be used inside of the plugin_log() forward.
 *
 * @return          Number of arguments in the log message
 * @error           If called outside of the plugin_log() forward, an error is
 *                  thrown.
 */
native read_logargc();

/**
 * Retrieves argument of log message.
 *
 * @note Can only be used inside of the plugin_log() forward.
 *
 * @param id        Argument index, starting from 0
 * @param output    Buffer to copy log argument to
 * @param len       Maximum buffer size
 *
 * @return          Number of arguments in the log message
 * @error           If called outside of the plugin_log() forward, an error is
 *                  thrown.
 */
native read_logargv(id, output[], len);

/**
 * Parse log data about client.
 *
 * @note When client actions are logged they appear in the the format
 *       "Name<#userid><SteamID><teamname>", this native extracts the individual
 *       pieces of information.
 *
 * @param text      String to process
 * @param name      Buffer to copy client name to
 * @param nlen      Maximum name buffer size
 * @param userid    Variable to store userid in
 * @param authid    Buffer to copy client authid to
 * @param alen      Maximum auth buffer size
 * @param team      Buffer to copy client team to
 * @param tlen      Maximum team buffer size
 *
 * @noreturn
 * @error           If the provided string is not valid client log data, an
 *                  error will be thrown.
 */
native parse_loguser(const text[], name[], nlen, &userid = -2, authid[] = "", alen = 0, team[]="", tlen=0);

/**
 * Sends a message to the console of the server.
 *
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 */
native server_print(const message[], any:...);

/**
 * Returns if the given mapname is deemed valid by the engine.
 *
 * @param mapname   Name of the map
 *
 * @return          1 if the map name is valid, 0 otherwise
 */
native is_map_valid(const mapname[]);

/**
 * Returns if the client is a bot.
 *
 * @param index     Client index
 *
 * @return          1 if client is a bot, 0 otherwise
 */
native is_user_bot(index);

/**
 * Returns if the client is a HLTV proxy.
 *
 * @param index     Client index
 *
 * @return          1 if client is a HLTV proxy, 0 otherwise
 */
native is_user_hltv(index);

/**
 * Returns if the client is connected.
 *
 * @note This does not throw an error if the provided index is out of the
 *       1 to MaxClients range. That means you can safely use this native
 *       without manually verifying an index to be a valid player index.
 *
 * @param index     Client index
 *
 * @return          1 if client is connected, 0 otherwise
 */
native is_user_connected(index);

/**
 * Returns if the client is connecting.
 *
 * @param index     Client index
 *
 * @return          1 if client is connecting, 0 otherwise
 */
native is_user_connecting(index);

/**
 * Returns if the client is alive.
 *
 * @note This will never return true if a player is not connected. If you need
 *       to know whether a player is alive, an additional call to
 *       is_user_connected() is unnecessary.
 *
 * @param index     Client index
 *
 * @return          1 if client is alive, 0 otherwise
 */
native is_user_alive(index);

/**
 * Returns if the server is a dedicated server.
 *
 * @param index     Client index
 *
 * @return          1 if server is a dedicated server, 0 otherwise
 */
native is_dedicated_server();

/**
 * Returns if the server is running on Linux.
 *
 * @param index     Client index
 *
 * @return          1 if server is running on Linux, 0 otherwise
 */
native is_linux_server();

/**
 * Returns if the AMXX installation has the JIT enabled.
 *
 * @param index     Client index
 *
 * @return          1 if JIT is enabled, 0 otherwise
 */
native is_jit_enabled();

/**
 * Retrieves the version string of the AMXX installation.
 *
 * @param buffer    Buffer to copy version to
 * @param length    Maximum buffer size
 *
 * @return          Number of cells written to the buffer
 */
native get_amxx_verstring(buffer[], length);

/**
 * Returns the last known attacker of a player.
 *
 * @note As of AMXX 1.75 this can return a non-player entity index if the player
 *       was attacked by a non-player entity.
 *
 * @param index     Client index
 * @param ...       Optionally a second byref parameter will be filled with the
 *                  attacker weapon, and a third byref parameter will be filled
 *                  with the hit place on the body.
 *
 * @return          Attacker client index, a non-player entity or 0 if no
 *                  attacker was found
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients an error will be thrown.
 */

native get_user_attacker(index, ...);

/**
 * Traces the client's current aim vector to see if it hits something.
 *
 * @note If the trace does not hit a player id and body will be set to 0.
 * @note If the trace hits nothing within the specified distance 0.0 is returned
 *
 * @param index     Client index to trace aim from
 * @param id        Variable to store hit player index (if applicable)
 * @param body      Variable to store hit player body part (if applicable)
 * @param dist      Maximum distance of the trace
 *
 * @return          Distance between the trace start and end point
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients an error will be thrown.
 */
native Float:get_user_aiming(index, &id, &body, dist=9999);

/**
 * Returns the player's frags.
 *
 * @note While this is mod-independent the mod may track frag count differently
 *       so it can only be retrieved using another native or other methods.
 * @note This will actually return the player's overall score, which may or may
 *       not be equal to their scored frags depending on the mod.
 *
 * @param index     Client index
 *
 * @return          Frags/Score of the player. Also returns 0 if the client is
 *                  not connected or the index is not within the range of
 *                  1 to MaxClients
 */
native get_user_frags(index);

/**
 * Returns the player's armor value.
 *
 * @note While this is mod-independent the mod may track armor data differently
 *       so it can only be retrieved using another native or other methods.
 *
 * @param index     Client index
 *
 * @return          Amount of armor the player has. Also returns 0 if the client
 *                  is not connected or the index is not within the range of
 *                  1 to MaxClients
 */
native get_user_armor(index);

/**
 * Returns the player's death count.
 *
 * @note While this is mod-independent the mod may track death count differently
 *       so it can only be retrieved using another native or other methods.
 *
 * @param index     Client index
 *
 * @return          Amount of deaths the player has. Also returns 0 if the
 *                  client is not connected or the index is not within the range
 *                  of 1 to MaxClients
 */
native get_user_deaths(index);

/**
 * Returns the player's health points.
 *
 * @note While this is mod-independent the mod may track health points
 *       differently so it can only be retrieved using another native or other
 *       methods.
 *
 * @param index     Client index
 *
 * @return          Amount of health points the player has. Also returns 0 if
 *                  the client is not connected or the index is not within the
 *                  range of 1 to MaxClients
 */
native get_user_health(index);

/**
 * Retrieve a client's index by name.
 *
 * @param name  Name to search for
 *
 * @return      Client index on success, 0 otherwise
 */
native get_user_index(const name[]);

/**
 * Retrieves the IP of a client or the server.
 *
 * @param index         Client index, use 0 to retrieve the server IP
 * @param ip            Buffer to copy IP to
 * @param len           Maximum buffer size
 * @param without_port  Remove the port from the IP if nonzero
 *
 * @return              Number of cells written to the buffer
 */
native get_user_ip(index, ip[], len, without_port = 0);

/* Returns if the player has the weapon or not in their pev->weapons field.
   set "setweapon" to 0 to turn the bit off, set to 1 to turn it on. */
native user_has_weapon(index,weapon,setweapon=-1);

/* Returns id of currently carried weapon. Gets also
* ammount of ammo in clip and backpack. */
native get_user_weapon(index,&clip=0,&ammo=0);

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
 *  also theirs indexes.
 * Note that num is incremental - if you pass 0, you get
 *  32 weapons towards the total.  Afterwards, num will
 *  will contain the number of weapons retrieved.
 * However, subsequent calls to get_user_weapons() will
 *  return the next batch of weapons, in case the mod
 *  supports more than 32 weapons.
 * This means to call get_user_weapons() on the same
 *  inputs twice, you must reset num to 0 to get the
 *  original output again.
 */
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
native log_amx(const string[], any:...);

/* Sends message to standard HL logs. */
native log_message(const message[],any:...);

/* Sends log message to specified file. */
native log_to_file(const file[],const message[],any:...);

/**
 * Returns the number of players on the server.
 *
 * @param flag      Count clients still in the connecting process if nonzero
 *
 * @return          Number of players on the server
 */
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
* "h" - skip HLTV.
* Example: Get all alive CTs: get_players(players,num,"ae","CT") */
native get_players(players[32], &num ,const flags[]="", const team[]="");

/* Gets argument from command. */
native read_argv(id,output[],len);

/* Gets line of all arguments. */
native read_args(output[],len);

/* Returns number of arguments (+ one as command). */
native read_argc();

/**
 * Converts a flag string to a bitflag value.
 *
 * @note Example: The string "abcd" represents the sum of 1, 2, 4 and 8 - or
 *       (1<<0)|(1<<1)|(1<<2)|(1<<3). The function will return 15.
 *
 * @param flags     Flag string to convert
 *
 * @return          Bitflag value
 */
native read_flags(const flags[]);

/**
 * Converts a bitflag value to a flag string.
 *
 * @note Example: The value 3 will yield the string "ab"
 *
 * @param flags     Bitflag value to convert
 * @param output    Buffer to copy flag string to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
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
native client_cmd(index,const command[],any:...);

/**
 * This is an emulation of a client command (commands aren't send to client!).
 * It allows to execute some commands on players and bots.
 * Function is excellent for forcing to do an action related to a game (not settings!).
 * The command must stand alone but in arguments you can use spaces.
 *
 * @param index			Index of the client, use 0 to send to all clients.
 * @param command		The client command to execute on.
 * @param arg1			Optionnal. The command arguments.
 * @param arg2			Optionnal. The command arguments.
 * @noreturn
 */
native engclient_cmd(index,const command[],const arg1[]="",const arg2[]="");

/**
 * This is an emulation of a client command (commands aren't send to client!).
 * It allows to execute some commands on players and bots.
 * Function is excellent for forcing to do an action related to a game (not settings!).
 * The command must stand alone but in arguments you can use spaces.
 *
 * @note This is similar to engclient_cmd with the difference all plugins hooking
 *       the command will be notified as well.
 *
 * @param index			Index of the client, use 0 to send to all clients.
 * @param command		The client command to execute on.
 * @param arg1			Optionnal. The command arguments.
 * @param arg2			Optionnal. The command arguments.
 * @noreturn
 */
native amxclient_cmd(index, const command[], const arg1[] = "", const arg2[] = "");

/* Executes command on a server console. */
native server_cmd(const command[],any:...);

/**
 * Sets a cvar to a given string value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_* set of functions.
 *
 * @param cvar      cvar name to set
 * @param value     value to set cvar to
 *
 * @noreturn
 */
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

/**
 * Sets a cvar to a given float value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_* set of functions.
 *
 * @param cvar      cvar name to set
 * @param value     value to set cvar to
 *
 * @noreturn
 */
native set_cvar_float(const cvar[], Float:value);

/**
 * Gets a floating value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_* set of functions.
 *
 * @param cvarname  cvar name to get value from
 *
 * @return          Cvar value, converted to float
 */
native Float:get_cvar_float(const cvarname[]);

/**
 * Gets an integer value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_* set of functions.
 *
 * @param cvarname  cvar name to get value from
 *
 * @return          Cvar value, converted to int
 */
native get_cvar_num(const cvarname[]);

/**
 * Sets a cvar to a given integer value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_* set of functions.
 *
 * @param cvarname  cvar name to set
 * @param value     value to set cvar to
 *
 * @noreturn
 */
native set_cvar_num(const cvarname[],value);

/**
 * Gets a string value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engines cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_* set of functions.
 *
 * @param cvar      cvar name to get value from
 * @param output    Buffer to copy cvar value to
 * @param iLen      Maximum size of the buffer
 *
 * @return          Number of cells written to buffer.
 */
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
native set_task(Float:time,const function[],id = 0,const parameter[]="",len = 0,const flags[]="", repeat = 0);

/* Removes all tasks with given id. If outside var is
* set then a task can be removed also when
* was set in another plugin. */
native remove_task(id = 0, outside = 0);

/* Changes the time of a task */
native change_task(id = 0, Float:newTime=1.0, outside = 0);

/* Returns 1 if task under given id exists. */
native task_exists(id = 0, outside = 0);

/* Sets the users flags with the assignment by bitwise OR operator. */
native set_user_flags(index,flags=-1,id=0);

/* Gets flags from player. Set index to 0 if you want to read flags from server. */
native get_user_flags(index,id=0);

/* Removes flags for player. */
native remove_user_flags(index,flags=-1,id=0);

/* Registers function which will be called from client console.
 * Set FlagManager to 1 to make FlagManager always include this command
 * Set FlagManager to 0 to make FlagManager never include this command
 * Returns the command ID.
 */
native register_clcmd(const client_cmd[],const function[],flags=-1, const info[]="", FlagManager=-1);

/* Registers function which will be called from any console.
 * Set FlagManager to 1 to make FlagManager always include this command
 * Set FlagManager to 0 to make FlagManager never include this command
 * Returns the command ID.
 */
native register_concmd(const cmd[],const function[],flags=-1, const info[]="", FlagManager=-1);

/* Registers function which will be called from server console.
 * Returns the command ID.
 */
native register_srvcmd(const server_cmd[],const function[],flags=-1, const info[]="");

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

/* Gets the parent plugin id of a console command. */
native get_concmd_plid(cid, flag_mask, id_type);

/* Returns number of registered console commands. */
native get_concmdsnum(flag,id = -1);

/* Returns the number of plugin-registered cvars. */
native get_plugins_cvarsnum();

/* Returns information about a plugin-registered cvar. */
native get_plugins_cvar(num, name[], namelen, &flags=0, &plugin_id=0, &pcvar_handle=0);

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
native emit_sound(index, channel, const sample[], Float:vol, Float:att,flags, pitch);

/* Registers new cvar for HL engine.
 * Returns the cvar pointer for get/set_pcvar functions.
 */
native register_cvar(const name[],const string[],flags = 0,Float:fvalue = 0.0);

/* Generates random floating point number from a to b. */
native Float:random_float(Float:a,Float:b);

/* Generates random integer from a to b. */
native random_num(a,b);

/* Returns id of client message.
* Example: get_user_msgid("TextMsg"). */
native get_user_msgid(const name[]);

/* Gets name of client message index. Return value is number of
* characters copied into name. Returns 0 on invalid msgid. */
native get_user_msgname(msgid, name[], len);

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

/* Checks whether a module is loaded. If it is not, the return value is -1, otherwise
* the return value is the module id. The function is case insensitive. */
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

/**
 * Checks whether a plugin is loaded by the given registered name (such as "Admin Base"), or, optionally
 * the given filename ("admin.amxx").
 *
 * @param name			Either the plugin name to lookup, or the plugin filename to lookup.
 * @param usefilename	Set to true if you want to search for the plugin by the filename, false to search
 *						by the plugin's registered name.
 *
 * @return				Plugin ID of the matching plugin on a successful search, -1 on a failed search.
 *
 * @note				Prior to 1.8, this function would only search for plugins registered names, not
 *						the filename.
 *
 * @note				The plugin registered name search is a case insensitive search, however, the plugin
 *						filename search is case sensitive.
 */
native is_plugin_loaded(const name[], bool:usefilename=false);

/* Gets info about plugin by given index.
 * Function returns -1 if plugin doesn't exist with given index.
 * Note: the [...] portion should not be used, and is only for backward compatibility.
 * Use index of -1 to use the calling plugin's ID.
 */
native get_plugin(index,filename[]="",len1=0,name[]="",len2=0,version[]="",len3=0,author[]="",len4=0,status[]="",len5=0,...);

/* Returns number of all loaded plugins. */
native get_pluginsnum();

/* Pauses function or plugin so it won't be executed.
* In most cases param1 is name of function and
* param2 name of plugin (all depends on flags).
* Flags:
* "a" - pause whole plugin.
* "c" - look outside the plugin (by given plugin name).
* "d" - set "stopped" status when pausing whole plugin.
*       In this status plugin is unpauseable.
* Example: pause("ac","myplugin.amxx")
*
* Note: There used to be the b and e flags as well,
* which have been deprecated and are no longer used.
*/
native pause(const flag[], const param1[]="",const param2[]="");

/* Unpauses function or plugin.
* Flags:
* "a" - unpause whole plugin.
* "c" - look outside the plugin (by given plugin name). */
native unpause(const flag[], const param1[]="",const param2[]="");

/**
 * Initiates a function call to this or another plugin by function name.
 *
 * @note This only sets up the function call and covers the pre-requisites.
 *       Push parameters using the callfunc_push_* set of functions. The call
 *       will be executed only upon using callfunc_end()
 *
 * @param func      Function name
 * @param plugin    Plugin filename. If empty the calling plugin is targeted.
 *                  The filename has to be the full exact name (e.g. stats.amxx)
 *
 * @return           1 on success
 *                   0 on runtime error
 *                  -1 if plugin was not found
 *                  -2 if function was not found
 * @error           If called while another callfunc has not yet been finished,
 *                  an error is thrown.
 */
native callfunc_begin(const func[], const plugin[]="");

/**
 * Initiates a function call to this or another plugin by function id.
 *
 * @note This only sets up the function call and covers the pre-requisites.
 *       Push parameters using the callfunc_push_* set of functions. The call
 *       will be executed only upon using callfunc_end()
 * @note The function id can be retrieved by get_func_id()
 *
 * @param func      Function id
 * @param plugin    Plugin filename. If empty the calling plugin is targeted.
 *                  The filename has to be the full exact name (e.g. stats.amxx)
 *
 * @return           1 on success
 *                  -1 if plugin was not found
 *                  -2 if function is not executable
 * @error           If called while another callfunc has not yet been finished,
 *                  or the specified function is invalid, an error is thrown.
 */
native callfunc_begin_i(func, plugin = -1);

/**
 * Retrieve a functions id for use with callfunc_begin_i()
 *
 * @param funcName  Function name
 * @param pluginId  Plugin id. If -1 the calling plugin is targeted. The plugin
 *                  id can be retrieved using find_plugin_byfile()
 *
 * @return          >0 Function id on success
 *                  -1 if plugin or function was not found
 */
native get_func_id(const funcName[], pluginId = -1);

/**
 * Pushes an int value onto the current call.
 *
 * @param value     Int value to push
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_int(value);

/**
 * Pushes a float value onto the current call.
 *
 * @param value     Float value to push
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_float(Float: value);

/**
 * Pushes an int value reference onto the current call.
 *
 * @note Changes made to this value by the called function will be reflected
 *       in the calling plugin.
 *
 * @param value     Int value to push
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_intrf(&value);

/**
 * Pushes a float value reference onto the current call.
 *
 * @note Changes made to this value by the called function will be reflected
 *       in the calling plugin.
 *
 * @param value     Float value to push
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_floatrf(&Float:value);

/**
 * Pushes a string onto the current call.
 *
 * @note This will defy the "const" specifier if copyback is true, which is
 *       only kept for special backwards compatibility.
 *
 * @param VALUE     String to push
 * @param copyback  If true any changes made in the called function will be
 *                  copied back to the calling plugin.
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_str(const VALUE[], bool:copyback=true);

/**
 * Pushes an array onto the current call.
 *
 * @note This will defy the "const" specifier if copyback is true, which is
 *       only kept for special backwards compatibility.
 *
 * @param VALUE         Array to push
 * @param array_size    Size of the array
 * @param copyback      If true any changes made in the called function will be
 *                      copied back to the calling plugin.
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_array(const VALUE[], array_size, bool:copyback=true);

/**
 * Completes the call to a function.
 *
 * @return       1 on success
 *              -1 if the plugin was not found
 *              -2 if the function was not found
 * @error       If called without initiating a callfunc an error is thrown.
 */
native callfunc_end();

/* Called on inconsistent file. You can put any text
* into reason to change an original message. */
forward inconsistent_file(id,const filename[], reason[64] );

/* Forces the client and server to be running with the same
* version of the specified file ( e.g., a player model ). */
native force_unmodified(force_type, const mins[3] , const maxs[3], const filename[]);

/**
 * Calculates the MD5 keysum of a string.
 *
 * @param szString      String to calculate keysum of
 * @param md5buffer     Buffer to copy the MD5 hash to
 *
 * @return              Number of cells written to the buffer (always 32)
 */
native md5(const szString[], md5buffer[34]);

/**
 * Calculates the MD5 keysum of a file.
 *
 * @param file          Path to file to calculate keysum of
 * @param md5buffer     Buffer to copy the MD5 hash to
 *
 * @return              Number of cells written to the buffer (always 32)
 * @error               If the file can not be opened, and error is thrown.
 */
native md5_file(const file[], md5buffer[34]);

/* Returns the internal flags set on the plugin's state
 * If hdr is 1, it will return the pcode flags rather than state flags.
 *
 * Use a plid of -1 to get the flags for the calling plugin.
 */
native plugin_flags(hdr=0, plid=-1);

/**
 * @deprecated
 * Do not use!
 */
forward plugin_modules();

native require_module(const module[]);

native is_amd64_server();

/* Returns plugin id searched by file/name.  Returns INVALID_PLUGIN_ID on failure. */
native find_plugin_byfile(const filename[], ignoreCase=1);

/* This is called before plugin_init and allows you to register natives. */
forward plugin_natives();

/* Registers a NATIVE.  When a plugin uses your native (you should distribute a .inc),
 * the handler will be called with two parameters: the calling plugin id, and the
 * number of parameters.
 * If you set style=1, the method of parameter passing is a tad more efficient.
 * Instead of "id, numParams", you label the native exactly as how the parameters
 *  should, in theory, be sent.  Then for each byreference parameter, you call
 *  param_convert(num).  This is theoretically more efficient but quite hacky.
 *  The method was discovered by dJeyL, props to him!
 */
native register_native(const name[], const handler[], style=0);

/* Registers a library.  To mark a library as required, place the following
 * in your include file:
 *  #pragma reqlib <name>
 *  #if !defined AMXMODX_NOAUTOLOAD
 *   #pragma loadlib <name>
 *  #endif
 */
native register_library(const library[]);

/* Logs an error in your native, and breaks into the debugger.
 * Acts as if the calling plugin had the error.
 */
native log_error(error, const fmt[], any:...);

// More Dynamic Native System Stuff
// Each of these natives affects one of the parameters sent to your native.
// Parameters go from 1 to n, just like in modules, and it is important to
//  remember two things: The parameters are actually coming from another plugin
//  (and just like modules, you must use these special natives).
//  two: you CANNOT call your native from inside your native.  This is very bad.

//This function should only be called if you registered with style=1
//You only need to use it on by-reference parameters.
native param_convert(num);

// Gets a string from the calling plugin
native get_string(param, dest[], maxlen);

// Sets a string in the calling plugin
native set_string(param, dest[], maxlen);

// Gets a normal int or float parameter
native get_param(param);
native Float:get_param_f(param);

// Gets/Sets a float or int parameter by reference
native get_param_byref(param);
native Float:get_float_byref(param);
native set_param_byref(param, value);
native set_float_byref(param, Float:value);

// Copies an array either from the calling plugin to you
// Or copies an array from you to the calling plugin
native get_array(param, dest[], size);
native get_array_f(param, Float:dest[], size);
native set_array(param, const source[], size);
native set_array_f(param, const Float:source[], size);

// Dispatches a client cvar query
//  id: Player id
//  cvar: cvar name
//  resultFunc: public handler function
//  paramLen + params: optional array parameter
// resultFunc looks like:
//  public callbackCvarValue(id, const cvar[], const value[])
// or if you use the optional parameter:
//  public callbackCvarValue(id, const cvar[], const value[], const param[])
native query_client_cvar(id, const cvar[], const resultFunc[], paramlen=0, const params[] = "");


/**
 * Allows you to trap error messages that occur in your plugin.
 * You can use this to override the debug messages that occur when your plugin
 *  causes some sort of runtime error.  Your handler will be called in this style:
 *
 * public error_filter(error_code, bool:debugging, message[])
 *    error_code is the AMX_ERR code.  debugging is whether or not the plugin is in debug mode.
 *    message[] is any message that was sent along with the error.
 * Return PLUGIN_CONTINUE to let the error pass through the filter.
 * Return PLUGIN_HANDLED to block the error from displaying.
 */
native set_error_filter(const handler[]);

/**
 * Gets a trace handle for the item at the top of the traced call stack.
 * Returns 0 if no debugging information is available.
 */
native dbg_trace_begin();

/**
 * Gets the next item in a traced call stack.  Returns 0 if no more traces exist.
 */
native dbg_trace_next(trace);

/**
 * Gets the call stack info for a trace.
 */
native dbg_trace_info(trace, &line, function[], maxLength1, file[], maxLength2);

/**
 * Gets the formatted error string, which looks like "Run time error X: (description)"
 */
native dbg_fmt_error(buffer[], maxLength);

/**
 * The following two natives are useful for creating cross-mod plugins
 *  where instead of #define flags to compile separate versions, you can
 *  filter out the natives and modules depending on the current mod.
 * Examples of this usage are in plmenu.sma, which filters out the cstrike module.
 */

/**
 * Sets a native filter.  This must be first set in plugin_natives(), but future calls will
 *  simply set a new filter.
 * This filter will allow your plugin to load even if its modules aren't loaded.  For example,
 *  if Fun isn't loaded and you use set_user_frags, your plugin will still load.  However, if you
 *  attempt to call this native, your filter will intercept it with these parameters:
 *
 * public function native_filter(const native[], index, trap)
 *  native - name of native
 *  index - index of native
 *  trap - 0 if native couldn't be found, 1 if native use was attempted
 *
 * If you return PLUGIN_HANDLED, no error is thrown.  If you return PLUGIN_CONTINUE,
 *  your plugin will have a run-time-error.  To print your own error, or change the default,
 *  you can return PLUGIN_HANDLED or return PLUGIN_CONTINUE and use set_error_filter.
 * If you return PLUGIN_CONTINUE when trap is 0, the plugin will ABORT AND FAIL TO LOAD!
 * When trap is 0, it is unsafe to use natives that modify the server or use other plugins.
 */
native set_native_filter(const handler[]);

/**
 * This function sets a module/library filter.  It will let you intercept the automatic requirement
 *  of a module and return PLUGIN_CONTINUE to fail load or PLUGIN_HANDLED to imply that load
 *  can continue even without the module.
 *
 * This is the most unforgiving of the filter functions.  You can ONLY call it during plugin_natives,
 *  and any error that occurs is not filtered -- instead your plugin will fail to load as if you
 *  returned PLUGIN_CONTINUE.
 *
 * Your handler will be called with this prototype:
 *
 * public module_filter(const library[], LibType:type);
 *  library - library or class name of the module that is required
 *  libtype - The type of requirement being checked (library/module or class).
 *
 *
 * set_module_filter() returns 0 on success (unlike most natives).
 */
native set_module_filter(const handler[]);

/**
 * Aborts execution of the current callback.  Your script will throw a run time error.
 * You can also specify an optional message.
 * You should NOT call this function inside:
 *  - Error or module filters (native filters are safe if trap is 1)
 *  - plugin_natives()
 * Note that the plugin's filename is prepending to your message:
 *  [myplugin.amxx] MESSAGE
 */
native abort(error, const fmt[]="", any:...);

/**
 * Checks if a specific module is loaded.  This is the exact same method AMX Mod X
 *  uses to see if a module is required by a plugin.  For example:
 *  module_exists("cstrike")
 *  module_exists("dbi")
 */
native module_exists(const logtag[]);

/**
 * Checks if a library/class is loaded.  This is the newer version of module_exists.
 */
native LibraryExists(const library[], LibType:type);

/**
 * Returns the next valid hudchannel for a user, from 1-4.
 */
native next_hudchannel(player);

/**
 * Creates a HUD Synchronization Object.  Create one of these
 *  for each section of the screen that contains overlapping HUD messages.
 * For example, if you use both sides of the screen to display three messages
 *  that can potentially overlap, each side counts as a synchronizable area.
 * You can then use ShowSyncHudMsg() to correctly synchronize displaying the
 *  HUD message with any other messages potentially in its class.  Note that this
 *  does not yet do anything like reserve screen area, its sole purpose is to be
 *  able to wipe an old message on an auto-channel and ensure that it will not
 *  clear a message from another plugin.
 * The parameters are kept blank for future use.
 */
native CreateHudSyncObj(num=0, ...);

/**
 * Displays a synchronized HUD message.  This will check that your
 *  HUD object has its previous display on the screen cleared before
 *  it proceeds to write another.  It will only do this in the case
 *  of that channel not having been cleared already.
 * Target can be 0 for all players or 1-get_maxplayers().
 * You must use set_hudmessage, although the channel parameter is
 *  entirely ignored.
 */
native ShowSyncHudMsg(target, syncObj, const fmt[], any:...);

/**
 * Clears the display on a HudSync Object.  This is essentially the same
 *  thing as: ShowSyncHudMsg(x, y, ""), except doing that would send
 *  out two messages and use up another channel.  This re-uses the last
 *  channel and clears it at the same time.
 * Note: for this you do not have to use set_hudmessage().
 * Note: target can be 0 for all players.
 */
native ClearSyncHud(target, syncObj);

//no
native int3();

//Sets your plugin to a failed/error state.
//If you use this, your plugin will cease operating.
//This is a good idea to fatally, but gracefully, handle errors.
//You can set a failed error message.
native set_fail_state(const fmt[], any:...);

//Returns the reference address of the variable passed in.
//This address is local to the plugin, and not a full CPU address
//pass the variable as the first parameter
native get_var_addr(any:...);

//Returns the value of an address.  This dereferences something returned by
// get_var_addr().  Attempting to pass in a value beyond stack or heap limits
// will result in AMX_ERR_MEMACCESS.
native get_addr_val(addr);

//Sets the value of an address.  same as above, essentially
native set_addr_val(addr, val);


/**
 * Creates a multi-plugin forward.
 * Stop type must be one of the ET_ values in amxconst.inc
 * results will be > 0 for success
 */
native CreateMultiForward(const name[], stop_type, ...);

/**
 * Creates a forward for one plugin.
 * Results will be > 0 for success.
 * id should be an id such as returned by find_plugin_byfile.
 * Unlike get_plugin(), negative numbers will not work.
 */
native CreateOneForward(plugin_id, const name[], ...);

/**
 * prepares an array.  use this and pass the result into
 * ExecuteForward() instead of the array itself.
 */
native PrepareArray(const array[], size, copyback=0);

/**
 * executes a forward.  returns result in ret.
 * returns 1 for success, 0 for failure.
 */
native ExecuteForward(forward_handle, &ret, any:...);

/**
 * Destroys/deallocates any type of forward
 */
native DestroyForward(forward_handle);


/* CVAR Pointer natives.  Use these for
 *  more optimized CVAR usage.
 * register_cvar() returns a pointer you can use.
 */

/**
 * Get a cvar pointer.  Returns 0 if not found.
 */
native get_cvar_pointer(const cvar[]);

native get_pcvar_flags(pcvar);
native set_pcvar_flags(pcvar, flags);
native get_pcvar_num(pcvar);
native set_pcvar_num(pcvar, num);
native Float:get_pcvar_float(pcvar);
native set_pcvar_float(pcvar, Float:num);
native get_pcvar_string(pcvar, string[], maxlen);
native set_pcvar_string(pcvar, const string[]);

/**
 * Sets a whole array to a certain value.
 */
native arrayset(array[], value, size);

/**
 * Returns the weapon id, otherwise 0 when no id found.
 * The weapon name is case sensitive, and has the weapon_* form.
 */
native get_weaponid(const name[]);

/**
 * Adds an admin to the dynamic admin storage
 * for lookup at a later time
 */
native admins_push(const AuthData[], const Password[], Access, Flags);

/**
 * Gets the number of admins in the dynamic admin
 * storage list
 */
native admins_num();

/**
 * Gets information about a dynamically stored admin
 * Use the enum AdminProp
 * Returns an integer value: AdminProp_Access, AdminProp_Flags
 * Sets the buffer string: AdminProp_Auth, AdminProp_Password
 */
native admins_lookup(num, AdminProp:Property, Buffer[]="", BufferSize=0);

/**
 * Clears the list of dynamically stored admins
 */
native admins_flush();

/**
 * Searches whether a map contains at least one entity with the provided class name.
 *
 * @param classname		The entity classname to check.
 *
 * @return				Returns true if an entity is found, otherwise false.
 */
native bool:has_map_ent_class(const classname[]);


// Keep this always at the bottom of this file
#include <message_stocks>
