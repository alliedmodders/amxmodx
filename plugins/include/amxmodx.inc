// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

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
#include <textparse_smc>
#include <textparse_ini>
#include <cvars>
#include <gameconfig>

/**
 * Called just after server activation.
 *
 * @note Good place to initialize most of the plugin, such as registering
 *       cvars, commands or forwards, creating data structures for later use, or
 *       generating and loading other required configurations.
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
 *       server command "changelevel", which is used by many plugins, will not
 *       trigger this forward. Unfortunately, this means that in practice this
 *       forward can be unreliable and will not be called in many situations.
 * @note AMXX 1.8.3 has added the engine_changelevel() function, which will utilize
 *       the correct engine function to change the map, and therefore trigger
 *       this forward.
 *
 * @param map   Map that the mod tries to change to
 *
 * @return      PLUGIN_CONTINUE to let the mod change the map
 *              PLUGIN_HANDLED or higher to prevent the map change
 */
forward server_changelevel(map[]);

/**
 * Called when all plugins went through plugin_init()
 *
 * @note When this forward is called, most plugins should have registered their
 *       cvars and commands already.
 *
 * @noreturn
 */
forward plugin_cfg();

/**
 * Called just before server deactivation and subsequent unloading of the
 * plugin.
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
 * This forward allows plugins to add models, sounds and generic files to the
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
 * Called when a clients info has changed.
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
 * Called when a client is connecting.
 *
 * @note This forward is called too early to do anything that directly affects
 *       the client.
 *
 * @param id        Client index
 * @param name      Client name
 * @param ip        Client ip address with port
 * @param reason    A reason that will be displayed when player gets rejected (can be overwritten)
 *
 * @return          PLUGIN_CONTINUE to let a client join to the server
 *                  PLUGIN_HANDLED or higher to prevent a client to join
 */
forward client_connectex(id, const name[], const ip[], reason[128]);

/**
 * Called when the client gets a valid SteamID.
 *
 * @note This may occur before or after client_putinserver has been called.
 * @note This is called for bots, and the SteamID will be "BOT".
 *
 * @param id		Client index
 * @param authid	Client auth
 *
 * @noreturn
 */
forward client_authorized(id, const authid[]);

/**
 * @deprecated This function does not catch all cases.
 */
#pragma deprecated Use client_disconnected() instead.
forward client_disconnect(id);

/**
 * Called when a client is disconnected from the server.
 *
 * @note This will be called in some additional cases that client_disconnect doesn't cover, 
 *       most notably when a client aborts the connection process. It is guaranteed to pair 
 *       with the client_connect() forward.
 * @note When this fires the player entity is still valid (e.g. is_user_connected(id) will 
 *       return true), but no networked commands will reach the client.
 *
 * @param id         Client index
 * @param drop       If true, the game has explicitly dropped the client
 * @param message    If drop is true, a writable buffer containing the disconnect info message
 * @param maxlen     Maximum size of buffer
 *
 * @noreturn
 */
forward client_disconnected(id, bool:drop, message[], maxlen);

/**
 * Called when a client entity has been removed from the server.
 *
 * @note This fires after the client_disconnected() forward, when the player entity has been
 *       removed (e.g. is_user_connected(id) will return false).
 *
 * @param id         Client index
 * @param drop       If true, the game has explicitly dropped the client
 * @param message    If drop is true, contains the disconnect info message
 *
 * @noreturn
 */
forward client_remove(id, bool:drop, const message[]);


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
 * @param url           URL of the plugin
 * @param description   Description of the plugin
 *
 * @return              Plugin id of the calling plugin
 */
native register_plugin(const plugin_name[], const version[], const author[], const url[] = "", const description[] = "");

/**
 * Precaches a model file.
 *
 * @note Can only be used inside of the plugin_precache() forward.
 *
 * @param name  Path to the model file
 *
 * @return      Unique cache id of the model
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
 *       sound table, making it available for usage in emit_sound() for example.
 * @note Precaching other filetypes (such as mp3 music), optionally in different
 *       locations, has to be done with precache_generic()
 *
 * @param name  Path to the sound file
 *
 * @return      Unique cache id of the sound
 * @error       If called outside of the plugin_precache() forward, an error is
 *              thrown.
 */
native precache_sound(const name[]);

/**
 * Precaches a generic file.
 *
 * @note Can only be used inside of the plugin_precache() forward.
 * @note Precaching sounds with this will not add them to the engine sound table.
 *
 * @param szFile    Path to the file
 *
 * @return          Unique cache id of the file
 * @error           If called outside of the plugin_precache() forward, an error
 *                  is thrown.
 */
native precache_generic(const szFile[]);

/**
 * Precaches an event file.
 *
 * @note The event type should always be 1.
 * @note Contrary to the other precache_* natives, this can be used outside of
 *       the plugin_precache() forward, e.g. in plugin_init() or plugin_cfg().
 *       A bug in some clients makes this necessary, as plugin_precache() is
 *       called before the mod has precached its own, default event files. This
 *       can cause the event table to be misaligned on the client, leading to
 *       visual and audio bugs that are hard to diagnose.
 *
 * @param type  Event type
 * @param Name  Formatting rules, path to the event file
 * @param ...   Variable number of formatting parameters
 *
 * @return      Unique cache id of the event
 */
native precache_event(type, const Name[], any:...);

/**
 * Changes the map.
 *
 * @note  This calls the pfnChangelLevel engine function.
 * @note  This has the same behavior as using the "changelevel" server command,
 *        but will also trigger the server_changelevel() forward in AMXX
 *        plugins. It will also notify any Metamod plugins that are hooking
 *        the pfnChangeLevel function.
 *
 * @param map   Map name to change to
 *
 * @noreturn
 */
native engine_changelevel(const map[]);

/**
 * Sets info on the client.
 *
 * @param index     Client index
 * @param info      Info key
 * @param value     New value
 *
 * @noreturn
 * @error           If the index is not within the range of 1 to MaxClients or
 *                  the client is not connected, an error will be thrown.
 */
native set_user_info(index, const info[], const value[]);

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
 *                  the client is not connected, an error will be thrown.
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
 * @param player    Client index, use 0 to display to all clients
 * @param message   Message to display inside the MOTD window, if this is a
 *                  filename the contents of this file will be displayed
 * @param header    Text for the MOTD header, if empty the servers hostname will
 *                  be displayed instead
 *
 * @noreturn
 */
native show_motd(player, const message[], const header[] = "");

/**
 * Sends a message to the client.
 *
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param type      Message type, see print_* destination constants in
 *                  amxconst.inc
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
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
 *          red/blue/grey   x02   ; use team color to the end of the client name
 *                                ; This only works at the start of the string,
 *                                ; and precludes using other control characters
 *          default         x01   ; use default color from this point forward
 * @note The team color is defined by the sender's index. Alternatively, a
 *       specific team color can be enforced using the print_team_* constants in
 *       amxconst.inc
 * @note Usage examples:
 *       client_print_color(id, print_team_red, "^4Green ^3Red ^1Default")
 *       client_print_color(id, id2, "^4Green ^3id2's team color, ^1Default")
 * @note Including colors in ML can be done using the same escaping method:
 *       EXAMPLE_ML_KEY = ^4Green ^3Team color ^1Default
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified, or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param sender    Client index used as the message sender
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native client_print_color(index, sender, const message[], any:...);

/**
 * Sends a message to the client via the engine.
 *
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified, or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param player    Client index, use 0 to display to all clients
 * @param type      Message type, see engprint_* destination constants in
 *                  amxconst.inc
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
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
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native console_print(id, const message[], any:...);

/**
 * Executes a command from the specified client or the server console.
 *
 * @param id    Client index, or 0 to execute from the server console
 * @param cmd   Formatting rules
 * @param ...   Variable number of formatting parameters
 *
 * @return      Length of the formatted command
 */
native console_cmd(id, const cmd[], any:...);

/**
 * Registers a function to be called on a given game event.
 *
 * @note Please consider using register_event_ex() instead which allows you to
 *       use named constants for flags instead of letters.
 * @note Examples for event conditions:
 *       "2=c4" - Second parameter of message must be the string "c4"
 *       "3>10" - Third parameter of message must be greater than 10
 *       "3!4" - Third parameter of message must not be equal to 4
 *       "2&Buy" - Second parameter of message must contain "Buy" substring
 *       "2!Buy" - Second parameter of message must not equal "Buy"
 * @note Due to a long-standing bug that would break compatibility with older
 *       plugins, the client id should be checked for alive/dead state if using
 *       flags "d" or "e".
 * @note If multiple conditions are specified for a single parameter, only one
 *       of them has to hold true for the event function to be called.
 *
 * @param event     Name of event that should be hooked
 * @param function  Name of callback function
 * @param flags     Flags used for filtering events, the valid flags are:
 *                  "a" - Global event (sent to every client)
 *                  "b" - Event sent to single client
 *                  "c" - Call only once when repeated to multiple clients
 *                  "d" - Call only if sent to dead client
 *                  "e" - Call only if sent to alive client
 *                  "f" - Call only if sent to human client ("b" flag required)
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
 * @return          Event handle
 * @error           If an invalid event name or callback function is provided,
 *                  an error will be thrown.
 */
native register_event(const event[], const function[], const flags[], const cond[] = "", ...);

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
 *       plugins, the client id should be checked for alive/dead state if using
 *       flags "d" or "e".
 * @note If multiple conditions are specified for a single parameter, only one
 *       of them has to hold true for the event function to be called.
 *
 * @param event     Name of event that should be hooked
 * @param function  Name of callback function
 * @param flags     Flags used for filtering events (enum RegisterEventFlags); the valid flags are:
 *                    RegisterEvent_Global - Global event (sent to every client)
 *                    RegisterEvent_Single - Event sent to single client
 *                    RegisterEvent_OnceForMultiple - Call only once when repeated to multiple clients
 *                    RegisterEvent_OnlyDead - Call only if sent to dead client
 *                    RegisterEvent_OnlyAlive - Call only if sent to alive client
 *                    RegisterEvent_OnlyHuman - Call only if sent to human client (RegisterEvent_Single required)
 *                    RegisterEvent_OnlyBots - Call only if sent to bot (RegisterEvent_Single required)
 * @param cond      Condition string used for filtering events, built as:
 *                  "<argument number><comparison operator><value>"
 *                  Argument number is the argument position to be filtered
 *                  The comparison operator may be:
 *                    "=" for equality comparison (all argument types)
 *                    "!" for inequality comparison (all argument types)
 *                    "&" for bitwise and (int argument) or substring
 *                        comparison (string argument)
 *                    "<" for less than comparison (int/float arguments)
 *                    ">" for greater than comparison (int/float arguments)
 *                  The argument is compared to the specified value accordingly
 * @param ...       Any number of additional conditions
 *
 * @return          Event handle
 * @error           If an invalid event name or callback function is provided,
 *                  an error will be thrown.
 */
native register_event_ex(const event[], const function[], RegisterEventFlags:flags, const cond[] = "", ...);

/**
 * Enables a function hook of a game event which has been previously registered with register_event_ex().
 *
 * @param handle    Value returned from register_event() or register_event_ex()
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native enable_event(handle);

/**
 * Disables a function hook of a game event which has been previously registered with register_event_ex().
 *
 * @param handle    Value returned from register_event() or register_event_ex()
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native disable_event(handle);

/**
 * Registers a function to be called on a given log event.
 *
 * @note Examples for log conditions:
 *       "0 = World triggered" "1 = Game_Commencing"
 *       "1 = say"
 *       "3 = Terrorists_Win"
 *       "1 = entered the game"
 *       "0 = Server cvar"
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
 * @return          Log event handle
 * @error           If an invalid callback function is provided, an error will
 *                  be thrown.
 */
native register_logevent(const function[], argsnum, ...);

/**
 * Enables a function hook of a game log event which has been previously registered with register_logevent().
 *
 * @param handle    Value returned from register_logevent()
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native enable_logevent(handle);

/**
 * Disables a function hook of a game log event which has been previously registered with register_logevent().
 *
 * @param handle    Value returned from register_logevent()
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native disable_logevent(handle);

/**
 * Sets display parameters for hudmessages.
 *
 * @note As of AMXX 1.61, setting the channel to -1 will automatically choose
 *       the next available HUD channel for the client.
 * @note There are four different HUD channels available on the client (1-4).
 *       Sending a hudmessage to a channel will overwrite any existing messages
 *       already displaying on that channel.
 * @note If you plan to create a permanent message, don't forget to specify a
 *       specific channel to avoid possible flickering due to auto-channeling.
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
 * @param alpha1        Alpha component of hudmessage color
 * @param color2[4]     Color 2 array {r,g,b,a}
 * 
 * @noreturn
 */
native set_hudmessage(red = 200, green = 100, blue = 0, Float:x = -1.0, Float:y = 0.35, effects = 0, Float:fxtime = 6.0, Float:holdtime = 12.0, Float:fadeintime = 0.1, Float:fadeouttime = 0.2, channel = -1, alpha1 = 0, color2[4] = {255, 255, 250, 0});

/**
 * Displays a message on the client HUD.
 *
 * @note Use set_hudmessage to define how the message should look on screen.
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified, or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
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
native set_dhudmessage(red = 200, green = 100, blue = 0, Float:x = -1.0, Float:y = 0.35, effects = 0, Float:fxtime = 6.0, Float:holdtime = 12.0, Float:fadeintime = 0.1, Float:fadeouttime = 0.2);

/**
 * Displays a director message on the client HUD.
 *
 * @note Use set_dhudmessage to define how the message should look on screen.
 * @note Unlike the classic HUD message, which is channel-based, director
 *       messages are stack-based. You can have up to 8 messages displaying at
 *       once. If more are added, they will be overwritten in the order they were
 *       sent. There is no way to clear a specific message.
 * @note The message has a maximum length of 128 characters which this function
 *       will automatically enforce.
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified, or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param index     Client index, use 0 to display to all clients
 * @param message   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native show_dhudmessage(index, const message[], any:...);

/**
 * Displays a menu to the client.
 *
 * @note Keys is a bitflag value that represents which keys the user can press
 *       on the menu. If you want to display disabled menu options, or skip
 *       certain number slots, you should exclude that key from the bitflag.
 *       amxconst.inc provides MENU_KEY_* constants for convenience.
 * @note The title parameter is not displayed to the client and is only used for
 *       identifying menus internally and assigning them to their callbacks.
 *       The title corresponds to the menu name that you register with
 *       register_menuid()
 *
 * @param index     Client to display menu to, use 0 to display to all clients
 * @param keys      Enabled keys
 * @param menu      Menu body
 * @param time      Menu timeout in seconds, -1 to disable
 * @param title     Name of the menu for internal tracking purposes
 *
 * @return          1 on success, 0 if menu could not be displayed (client not
 *                  connected)
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native show_menu(index, keys, const menu[], time = -1, const title[] = "");

/**
 * Retrieves values from a client message.
 *
 * @note For use within callbacks registered with register_event_ex()
 * @note Usage examples:
 *       value = read_data(1);
 *       read_data(2, floatvalue);
 *       written = read_data(3, buffer, buffersize);
 *
 * @param value     Argument number to retrieve value from
 * @param ...       Changes the native's behavior depending on how many
 *                  additional parameters are provided:
 *                     0 - Return the argument integer value directly
 *                     1 - Store the argument float value in the variable passed
 *                         as the second parameter
 *                     2 - Copy the argument string value to the buffer provided
 *                         in the second parameter, using the third as the
 *                         maximum buffer size
 *
 * @return          Changes depending on how many additional parameters are
 *                  provided:
 *                     0 - Returns the argument integer value
 *                     1 - Returns the argument float value, converted
 *                         (truncated) to an integer
 *                     2 - Returns the number of cells written to the buffer
 */
native read_data(value, any:...);

/**
 * Returns the number of values in the client message.
 *
 * @note For use within callbacks registered with register_event_ex()
 *
 * @return      Number of values in client message
 */
native read_datanum();

/**
 * Returns the message id of the client message.
 *
 * @note For use within callbacks registered with register_event_ex()
 *
 * @return      Message id of the client message
 */
native read_datatype();

/**
 * Retrieves current log message.
 *
 * @note Should only be used inside of the plugin_log() forward.
 *
 * @param output    Buffer to copy log message to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native read_logdata(output[], len);

/**
 * Returns number of log message arguments.
 *
 * @note Should only be used inside of the plugin_log() forward.
 *
 * @return          Number of arguments in the log message
 */
native read_logargc();

/**
 * Retrieves argument of log message.
 *
 * @note Should only be used inside of the plugin_log() forward.
 *
 * @param id        Argument index, starting from 0
 * @param output    Buffer to copy log argument to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native read_logargv(id, output[], len);

/**
 * Parse log data about client.
 *
 * @note When client actions are logged, they appear in the the format
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
native parse_loguser(const text[], name[], nlen, &userid =-2, authid[] = "", alen = 0, team[] = "", tlen = 0);

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
 * Returns if the client is authorized.
 *
 * @note This does not throw an error if the provided index is out of the
 *       1 to MaxClients range. That means you can safely use this native
 *       without manually verifying that the index is a valid client index.
 *
 * @param index     Client index
 *
 * @return          1 if client is authorized, 0 otherwise
 */
native is_user_authorized(index);

/**
 * Returns if the client is connected.
 *
 * @note This does not throw an error if the provided index is out of the
 *       1 to MaxClients range. That means you can safely use this native
 *       without manually verifying that the index is a valid client index.
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
 * @note This will never return true if a client is not connected. If you need
 *       to know whether a client is alive, an additional call to
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
 * @return          1 if server is a dedicated server, 0 otherwise
 */
native is_dedicated_server();

/**
 * Returns if the server is running on Linux.
 *
 * @return          1 if server is running on Linux, 0 otherwise
 */
native is_linux_server();

/**
 * Returns if the AMXX installation has the JIT enabled.
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
 * Returns the last known attacker of a client.
 *
 * @note As of AMXX 1.75 this can return a non-client entity index if the client
 *       was attacked by a non-client entity.
 *
 * @param index     Client index
 * @param ...       If provided, the attacker weapon will be stored in an
 *                  optional second parameter, and the body hit place will be
 *                  stored in an optional third parameter
 *
 * @return          Attacker client index, a non-client entity or 0 if no
 *                  attacker was found
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native get_user_attacker(index, ...);

/**
 * Traces the client's current aim vector to see if it hits something.
 *
 * @note If the trace does not hit a client, id and body will be set to 0.
 * @note If the trace hits nothing within the specified distance, 0 is returned.
 * @note For a list of possible body hitplaces see the HIT_* constants in amxconst.inc.
 *
 * @param index     Client index to trace aim from
 * @param id        Variable to store hit client index (if applicable)
 * @param body      Variable to store hit client body part (if applicable)
 * @param dist      Maximum distance of the trace
 *
 * @return          Distance between the trace start and end point
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native Float:get_user_aiming(index, &id, &body = HIT_GENERIC, dist = 9999);

/**
 * Returns the client's frags.
 *
 * @note While this is mod-independent, the mod may track frag count differently,
 *       so it can only be retrieved using another native or other methods.
 * @note This will actually return the client's overall score, which may or may
 *       not be equal to their scored frags depending on the mod.
 *
 * @param index     Client index
 *
 * @return          Frags/Score of the client. Also returns 0 if the client is
 *                  not connected or the index is not within the range of
 *                  1 to MaxClients
 */
native get_user_frags(index);

/**
 * Returns the client's armor value.
 *
 * @note While this is mod-independent, the mod may track armor data differently,
 *       so it can only be retrieved using another native or other methods.
 *
 * @param index     Client index
 *
 * @return          Amount of armor the client has. Also returns 0 if the client
 *                  is not connected or the index is not within the range of
 *                  1 to MaxClients
 */
native get_user_armor(index);

/**
 * Returns the client's death count.
 *
 * @note While this is mod-independent, the mod may track death count differently,
 *       so it can only be retrieved using another native or other methods.
 *
 * @param index     Client index
 *
 * @return          Amount of deaths the client has. Also returns 0 if the
 *                  client is not connected or the index is not within the range
 *                  of 1 to MaxClients
 */
native get_user_deaths(index);

/**
 * Returns the client's health points.
 *
 * @note While this is mod-independent, the mod may track health points
 *       differently, so it can only be retrieved using another native or other
 *       methods.
 *
 * @param index     Client index
 *
 * @return          Amount of health points the client has. Also returns 0 if
 *                  the client is not connected or the index is not within the
 *                  range of 1 to MaxClients
 */
native get_user_health(index);

/**
 * Retrieves a client's index by name.
 *
 * @param name  Name to search for
 *
 * @return      Client index on success, 0 otherwise
 */
native get_user_index(const name[]);

/**
 * Retrieves the IP of a client or the server.
 *
 * @param index             Client index, use 0 to retrieve the server IP
 * @param ip                Buffer to copy IP to
 * @param len               Maximum buffer size
 * @param without_port      Remove the port from the IP if nonzero
 *
 * @return                  Number of cells written to the buffer
 */
native get_user_ip(index, ip[], len, without_port = 0);

/**
 * Returns if the client has the specified weapon in their inventory.
 *
 * @param index         Client index
 * @param weapon        Weapon index
 * @param setweapon     If zero the weapon bit will be removed from the client's
 *                      inventory, if 1 it will be set
 *
 * @return              1 if the weapon is present, 0 if it is not
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, an error will be thrown.
 */
native user_has_weapon(index, weapon, setweapon = -1);

/**
 * Returns weapon index of the currently carried weapon. Also allows retrieval
 * of ammo in the clip and backpack.
 *
 * @param index     Client index
 * @param clip      Optional variable to store clip ammo to
 * @param ammo      Optional variable to store backpack ammo to
 *
 * @return          Weapon index on success or 0 if the client is not connected
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native get_user_weapon(index, &clip = 0, &ammo = 0);

/**
 * Retrieves ammo in the clip and backpack of the specified weapon.
 *
 * @param index     Client index
 * @param weapon    Weapon index
 * @param clip      Variable to store clip ammo to
 * @param ammo      Variable to store backpack ammo to
 *
 * @return          1 on success or 0 if the client is not connected
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients or the weapon index is invalid, an error will
 *                  be thrown.
 */
native get_user_ammo(index, weapon, &clip, &ammo);

/**
 * Converts an integer to a text string.
 *
 * @note The conversion algorithm is limited to a certain range of numbers, but
 *       is guaranteed to work correctly for all numbers from 0 to 999. Outside
 *       of that range, the conversion will result in an incorrect string, but
 *       will not fail.
 * @note The conversion is to english text, there is no way to change this.
 *
 * @param num       Integer to convert
 * @param output    Buffer to copy string to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native num_to_word(num, output[], len);

/**
 * Returns the team id of the client, and optionally retrieves the name of
 * the team.
 *
 * @param index     Client index
 * @param team      Buffer to copy team name to
 * @param len       Maximum size of buffer
 *
 * @return          Team index on success, -1 if client index is invalid or
 *                  the client is not connected
 */
native get_user_team(index, team[] = "", len = 0);

/**
 * Returns client's playing time in seconds.
 *
 * @param index     Client index
 * @param flag      If nonzero, the result will not include the time it took
 *                  the client to connect.
 *
 * @return          Connection time in seconds, 0 if client index is invalid or
 *                  client is not connected
 */
native get_user_time(index, flag = 0);

/**
 * Retrieves the ping and loss of a client.
 *
 * @param index     Client index
 * @param ping      Variable to store ping in
 * @param loss      Variable to store loss in
 *
 * @return          1 on success, 0 if client index is invalid or the client
 *                  is not connected
 */
native get_user_ping(index, &ping, &loss);

/**
 * Retrieves an origin related to the client.
 *
 * @note For a list of possible modes see the Origin_* constants in amxconst.inc.
 *
 * @param index     Client index
 * @param origin    Array to store origin in
 * @param mode      What type of origin to retrieve:
 *                    Origin_Client - current position
 *                    Origin_Eyes - position of eyes (and weapon)
 *                    Origin_AimEndClient - aim end position from client position
 *                    Origin_AimEndEyes - aim end position from eyes (hit point for weapon)
 *                    Origin_CS_LastBullet - position of last bullet hit (only for Counter-Strike)
 *
 * @return          1 on success, 0 if client is not connected
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native get_user_origin(index, origin[3], mode = 0);

/**
 * Retrieves all weapons in the client inventory, stores them in an array, and
 * returns the inventory as a bitflag sum.
 *
 * @note Make sure that num has an initial value of 0 or the native will not
 *       work correctly.
 *
 * @param index     Client index
 * @param weapons   Array to store weapon indexes in
 * @param num       Variable to store number of weapons in the inventory to
 *
 * @return          Bitflag sum of weapon indexes, 0 if client is not connected
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native get_user_weapons(index, weapons[32], &num);

/**
 * Retrieves the full name of a weapon.
 *
 * @param id        Weapon index
 * @param weapon    Buffer to copy name to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native get_weaponname(id, weapon[], len);

/**
 * Retrieves the name of a client or the server.
 *
 * @param index     Client index, or 0 to retrieve the server hostname
 * @param name      Buffer to copy name to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native get_user_name(index, name[], len);

/**
 * Retrieves the SteamID of a client.
 *
 * @note The SteamID is only available once the client_authorized() forward has
 *       been called for the client.
 *
 * @param index     Client index
 * @param authid    Buffer to copy auth to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native get_user_authid(index, authid[], len);

/**
 * Returns the userid of a client.
 *
 * @param index     Client index
 *
 * @return          Client userid, 0 if the userid is not available or the
 *                  client index is invalid
 */
native get_user_userid(index);

/**
 * Slaps the client with specified power. Killing the client if applicable.
 *
 * @note This removes "power" amount of health from the client, performing
 *       a kill if they have no health left after the slap.
 * @note The function will apply a velocity to the client that is independent
 *       of the slap power. The slap direction can be influenced by the third
 *       parameter.
 *
 * @param index     Client index
 * @param power     Power of the slap
 * @param rnddir    If set to zero the player will be slapped along it's aim
 *                  vector, otherwise the direction will be randomized
 *
 * @return          1 if user is alive and slap succeeded, 0 otherwise
 */
native user_slap(index, power, rnddir = 1);

/**
 * Kills a client.
 *
 * @param index     Client index
 * @param flag      If nonzero, the death will not affect the client's score
 *
 * @return          1 on success, 0 if client index is invalid or the client
 *                  is not connected
 */
native user_kill(index, flag = 0);

/**
 * Logs a message to the current AMXX log file.
 *
 * @note The message will automatically be tagged with the plugin's name and the
 *       log will include a timestamp with the message.
 *
 * @param string    Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 */
native log_amx(const string[], any:...);

/**
 * Logs a message to the current server log file.
 *
 * @note The log will include a timestamp with the message.
 *
 * @param string    Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 */
native log_message(const message[], any:...);

/**
 * Logs a message hookable by plugins to the current server log file.
 *
 * @note The log will include a timestamp with the message.
 * @note The message can be hooked using "register_logevent".
 *
 * @param string    Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 */
native elog_message(const message[], any:...);

/**
 * Logs a message to the specified file
 *
 * @note The log will include a timestamp with the message.
 *
 * @param string    Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 */
native log_to_file(const file[], const message[], any:...);

/**
 * Returns the number of clients on the server.
 *
 * @param flag      Count clients still in the connecting process if nonzero
 *
 * @return          Number of clients on the server
 */
native get_playersnum(flag = 0);

/**
 * Stores a filtered list of client indexes to an array.
 *
 * @note Please consider using get_players_ex() instead which allows you to
 *       use named constants for flags instead of letters.
 * @note Example retrieving all alive CTs: get_players(players, num "ae", "CT")
 *
 * @param players   Array to store indexes to
 * @param num       Variable to store number of indexes to
 * @param flags     Optional list of filtering flags:
 *                    "a" - do not include dead clients
 *                    "b" - do not include alive clients
 *                    "c" - do not include bots
 *                    "d" - do not include human clients
 *                    "e" - match with team
 *                    "f" - match with part of name
 *                    "g" - match case insensitive
 *                    "h" - do not include HLTV proxies
 *                    "i" - include connecting clients
 * @param team      String to match against if the "e" or "f" flag is specified
 *
 * @noreturn
 */
native get_players(players[MAX_PLAYERS], &num, const flags[] = "", const team[] = "");

/**
 * Retrieves argument of client command as string.
 *
 * @note Should only be used inside of the client_command() forward.
 *
 * @param id        Argument index starting from 1, 0 returns the command itself
 * @param output    Buffer to copy command argument to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native read_argv(id, output[], len);

/**
 * Retrieves argument of client command as integer value.
 *
 * @note Should only be used inside of the client_command() forward.
 *
 * @param id        Argument index starting from 1
 *
 * @return          Integer value
 */
native read_argv_int(id);

/**
 * Retrieves argument of client command as float value.
 *
 * @note Should only be used inside of the client_command() forward.
 *
 * @param id        Argument index starting from 1
 *
 * @return          Float value
 */
native Float:read_argv_float(id);

/**
 * Retrieves full client command string.
 *
 * @note Should only be used inside of the client_command() forward.
 *
 * @param output    Buffer to copy command line to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native read_args(output[], len);

/**
 * Returns number of client command arguments.
 *
 * @note Should only be used inside of the client_command() forward.
 * @note This count includes the command itself. I.e. in a command with 4
 *       arguments, this will return 5.
 *
 * @return          Number of arguments in the command
 */
native read_argc();

/**
 * Converts a flag string to a bitflag value.
 *
 * @note Example: The string "abcd" represents the sum of 1, 2, 4, and 8 - or
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
native get_flags(flags, output[], len);

/**
 * Find a player given a filter.
 *
 * @note Please consider using find_player_ex() instead which allows you to
 *       use named constants for flags instead of letters.
 * @note If matching by userid, do not also specify the "a", "b" or "c" flags,
 *       or the function may not return a correct result.
 *
 * @param flags     List of filtering flags:
 *                    "a" - match with name
 *                    "b" - match with name substring
 *                    "c" - match with authid
 *                    "d" - match with ip
 *                    "e" - match with team name
 *                    "f" - do not include dead clients
 *                    "g" - do not include alive clients
 *                    "h" - do not include bots
 *                    "i" - do not include human clients
 *                    "j" - return last matched client instead of the first
 *                    "k" - match with userid
 *                    "l" - match case insensitively
 *                    "m" - include connecting clients
 * @param ...       String to match against (integer if "k" flag is specified)
 *
 * @return          Client index, or 0 if no client was found
 */
native find_player(const flags[], ...);

/**
 * Find a player given a filter.
 *
 * @note If matching by userid, do not also specify FindPlayer_MatchName, FindPlayer_MatchNameSubstring
 *       or FindPlayer_MatchAuthId, or the function may not return a correct result.
 *
 * @param flags     Filtering flags (enum FindPlayerFlags); valid flags are:
 *                    FindPlayer_MatchName - match with name
 *                    FindPlayer_MatchNameSubstring - match with name substring
 *                    FindPlayer_MatchAuthId - match with authid
 *                    FindPlayer_MatchIP - match with ip
 *                    FindPlayer_MatchTeam - match with team name
 *                    FindPlayer_ExcludeDead - do not include dead clients
 *                    FindPlayer_ExcludeAlive - do not include alive clients
 *                    FindPlayer_ExcludeBots - do not include bots
 *                    FindPlayer_ExcludeHuman - do not include human clients
 *                    FindPlayer_LastMatched - return last matched client instead of the first
 *                    FindPlayer_MatchUserId - match with userid
 *                    FindPlayer_CaseInsensitive - match case insensitively
 *                    FindPlayer_IncludeConnecting - include connecting clients
 * @param ...       String to match against (integer if FindPlayer_MatchUserId is specified)
 *
 * @return          Client index, or 0 if no client was found
 */
native find_player_ex(FindPlayerFlags:flags, ...);

/**
 * Removes double-quotes from the beginning and end of a string.
 *
 * @note If the string only has a double-quote at either the start *or* the end,
 *       and not both, the function will do nothing.
 * @note The function does not perform any trimming per-se. But if a
 *       double-quote is found at the beginning of the string, it will remove
 *       one ^r (carriage return) character at the end of the string if present,
 *       even if no matching double-quote is found. This is for convenience.
 *
 * @param text      String to remove double-quotes from
 *
 * @return          1 if matching double-quotes have been removed, 0 otherwise
 */
native remove_quotes(text[]);

/**
 * Executes a command on the client.
 *
 * @note Executing malicious commands on the client ("slowhacking") is frowned
 *       upon.
 * @note Valve has introduced a command filter to Counter-Strike 1.6. It is not
 *       possible to execute many commands if the client has opted in to this.
 *
 * @param index     Client index, use 0 to execute on all clients
 * @param command   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Length of formatted command string
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native client_cmd(index, const command[], any:...);

/**
 * Execute a command from the client without actually sending it to the client's
 * DLL.
 *
 * @note This emulates a client command on the server side, and is an excellent
 *       tool to force a client to do certain actions related to the game.
 * @note The command has to stand alone in the command parameter, only add
 *       arguments using the designated parameters.
 * @note Commands emulated using this function will not trigger plugin command
 *       hooks. For an alternative that does, see amxclient_cmd()
 *
 * @param index         Client index, use 0 to execute from all clients
 * @param command       Client command to execute on
 * @param arg1          Optional command arguments
 * @param arg2          Optional command arguments
 *
 * @noreturn
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native engclient_cmd(index, const command[], const arg1[] = "", const arg2[] = "");

/**
 * Execute a command from the client without actually sending it to the client's
 * DLL. This triggers plugin command hooks.
 *
 * @note This emulates a client command on the server side, and is an excellent
 *       tool to force a client to do certain actions related to the game.
 * @note The command has to stand alone in the command parameter, only add
 *       arguments using the designated parameters.
 * @note Commands emulated using this function will trigger other plugin's
 *       command hooks. For an alternative that doesn't, see engclient_cmd()
 *
 * @param index         Client index, use 0 to execute from all clients
 * @param command       Client command to execute on
 * @param arg1          Optional command arguments
 * @param arg2          Optional command arguments
 *
 * @noreturn
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native amxclient_cmd(index, const command[], const arg1[] = "", const arg2[] = "");

/**
 * Queues a command to be executed from the server console.
 *
 * @note Warning: This is a potential source of command injection. Do not feed
 *       client-controlled input (including client names) to this function
 *       without sanitizing it first.
 * @note The queued commands will be executed by the engine on the next frame.
 *       If you require them to be executed immediately, see server_exec()
 *
 * @param command   Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 */
native server_cmd(const command[], any:...);

/**
 * Retrieves the name of the currently played map.
 *
 * @param name      Buffer to copy map name to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 */
native get_mapname(name[], len);

/**
 * Returns time remaining on map.
 *
 * @return  Time left on map, in seconds
 */
native get_timeleft();

/**
 * Returns the game time based on the game tick.
 *
 * @note This time is counted up from map start. If the engine is not processing
 *       this function will return the same value between calls, which makes it
 *       unusable for profiling purposes.
 *
 * @return      Game time, in seconds
 */
native Float:get_gametime();

/**
 * Returns the maxplayers setting of the current server, that is how many
 * clients it supports.
 *
 * @note As of AMXX 1.8.3, this value is also exposed through a dynamic constant
 *       via the MaxClients variable, declared in amxconst.inc
 *
 * @return      Maxplayers setting
 */
native get_maxplayers();

/**
 * Retrieves the name of the currently played mod.
 *
 * @note This retrieves the short name of the mod. Example: for Counter-Strike,
 *       it will copy "cstrike" to the buffer.
 *
 * @param name      Buffer to copy mod name to
 * @param len       Maximum size of the buffer
 *
 * @return          Number of cells written to buffer
 */
native get_modname(name[], len);

/**
 * Retrieves the current time using the specified format string.
 *
 * @note Uses the strftime C function. For a list of valid format parameters,
 *       see: http://cplusplus.com/reference/clibrary/ctime/strftime.html
 *       A common example for a format string would be: "%m/%d/%Y - %H:%M:%S"
 *
 * @param format    Format string
 * @param output    Buffer to copy formatted time string to
 * @param len       Maximum size of buffer
 *
 * @return          Number of cells written to buffer
 */
native get_time(const format[], output[], len);

/**
 * Retrieves the provided time using the specified format string.
 *
 * @note Uses the strftime C function. For a list of valid format parameters,
 *       see: http://cplusplus.com/reference/clibrary/ctime/strftime.html
 *       A common example for a format string would be: "%m/%d/%Y - %H:%M:%S"
 *
 * @param output    Buffer to copy formatted time string to
 * @param len       Maximum size of buffer
 * @param format    Format string
 * @param time      Unix timestamp, use -1 to use the current time
 *
 * @return          Number of cells written to buffer
 * @error           If the conversion process fails, an error will be thrown.
 */
native format_time(output[], len, const format[], time = -1);

/**
 * Returns the system time as a unix timestamp (number of seconds since unix
 * epoch).
 *
 * @param offset    Optional offset value in seconds
 *
 * @return          Unix time stamp
 */
native get_systime(offset = 0);

/**
 * Converts time strings to unix time stamp.
 *
 * @note Uses the strptime C function. For a list of valid format parameters,
 *       see: http://www.cplusplus.com/reference/ctime/strftime/
 *       An example for a input/format combination would be:
 *       Input: "10:32:54 04/02/2013"  Format: "%H:%M:%S %m:%d:%Y"
 * @note Information missing from the input will be filled with the current
 *       time and date.
 *
 * @param input     Time string to convert
 * @param format    Formatting information for conversion
 * @param time      If different from -1, the converted time will be added to
 *                  this time stamp
 *
 * @return          Unix time stamp
 * @error           If the conversion process fails, an error will be thrown.
 */
native parse_time(const input[], const format[], time = -1);

/**
 * Calls a function after a specified time has elapsed.
 *
 * @note Please consider using set_task_ex() instead which allows you to
 *       use named constants for flags instead of letters.
 *
 * @note The function is called in the following manner if data is passed:
 *   data[]          - Data passed
 *   id              - Task index
 *
 * @note The function is called in the following manner if no data is passed:
 *   id              - Task index
 *
 * @param time          Time interval to assign
 * @param function      Function to execute
 * @param id            Task id to assign
 * @param parameter     Data to pass through to callback
 * @param len           Size of data
 * @param flags         Optional set of flags:
 *                        "a" - repeat timer a set amount of times
 *                        "b" - loop indefinitely until timer is stopped
 *                        "c" - time interval is treated as absolute time after
 *                              map start
 *                        "d" - time interval is treated as absolute time before
 *                              map change
 * @param repeat        If the "a" flag is set, the task will be repeated this
 *                      many times
 *
 * @noreturn
 * @error               If an invalid callback function is provided, an error is
 *                      thrown.
 */
native set_task(Float:time, const function[], id = 0, const any:parameter[] = "", len = 0, const flags[] = "", repeat = 0);

/**
 * Removes all tasks with the specified id.
 *
 * @param id        Task id to search for
 * @param outside   Will remove tasks set by other plugins if nonzero
 *
 * @return          Number of removed tasks
 */
native remove_task(id = 0, outside = 0);

/**
 * Modifies the time interval of all tasks with the specified id.
 *
 * @param id        Task id to search for
 * @param newTime   New time interval to set
 * @param outside   Will affect tasks set by other plugins if nonzero
 *
 * @return          Number of affected tasks
 */
native change_task(id = 0, Float:newTime = 1.0, outside = 0);

/**
 * Returns if a task with the specified id exists.
 *
 * @param id        Task id to search for
 * @param outside   Search for tasks set by other plugins if nonzero
 *
 * @return          1 if a task was found, 0 otherwise
 */
native task_exists(id = 0, outside = 0);

/**
 * Sets the specified admin flags to a client.
 *
 * @note For a list of possible flags, see the ADMIN_* constants in amxconst.inc
 * @note This function just adds the flags using a bitwise-or operation. After it
 *       has run, the flags may not exactly equal the specified bitflag sum.
 * @note AMXX stores multiple sets of flags internally, but only flag set
 *       0 is actively used. You should not change the value of the third
 *       parameter from the default.
 *
 * @param index     Client index, 0 to set flags of server
 * @param flags     Admin flags
 * @param id        Flag set id, ranging from 0 to 31
 *
 * @noreturn
 * @error           If the index is not within the range of 0 to MaxClients, an
 *                  error will be thrown.
 */
native set_user_flags(index, flags = -1, id = 0);

/**
 * Returns the client's admin flags as a bitflag sum.
 *
 * @note For a list of possible flags, see the ADMIN_* constants in amxconst.inc
 * @note AMXX stores multiple sets of flags internally, but only flag set
 *       0 is actively used. You should not change the value of the second
 *       parameter from the default.
 *
 * @param index     Client index, 0 to get flags of server
 * @param id        Flag set id, ranging from 0 to 31
 *
 * @return          Bitflag sum of client's admin flags
 * @error           If the index is not within the range of 0 to MaxClients, an
 *                  error will be thrown.
 */
native get_user_flags(index, id = 0);

/**
 * Removes the specified admin flags from a client.
 *
 * @note For a list of possible flags, see the ADMIN_* constants in amxconst.inc
 * @note This function just removes the flags using a bitwise-and operation.
 * @note AMXX stores multiple sets of flags internally, but only flag set
 *       0 is actively used. You should not change the value of the third
 *       parameter from the default.
 *
 * @param index     Client index, 0 to set flags of server
 * @param flags     Admin flags
 * @param id        Flag set id, ranging from 0 to 31
 *
 * @noreturn
 * @error           If the index is not within the range of 0 to MaxClients, an
 *                  error will be thrown.
 */
native remove_user_flags(index, flags = -1, id = 0);

/**
 * Registers a callback to be called when the client executes a command from the
 * console.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 * @note Opting in to FlagManager enables the admin privileges to be overwritten
 *       by the end user via the cmdaccess.ini config file.
 * @note Automatic detection for FlagManager will only include a command if it
 *       has required privileges (flags is not -1) and it is not a command
 *       starting with "say".
 *
 * @param client_cmd    Command to register
 * @param function      Callback function
 * @param flags         Admin privilege flags required
 * @param info          Command description
 * @param FlagManager   0 opts out of flag manager, 1 opts in, -1 selects
 *                      automatically
 * @param info_ml       If true, the parameter "info" will be looked up as multilingual key
 *
 * @return              Command id, 0 on failure
 * @error               If an invalid callback function is specified, an error
 *                      will be thrown.
 */
native register_clcmd(const client_cmd[], const function[], flags = -1, const info[] = "", FlagManager = -1, bool:info_ml = false);

/**
 * Registers a callback to be called when the client or server executes a
 * command from the console.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 * @note Opting in to FlagManager enables the admin privileges to be overwritten
 *       by the end user via the cmdaccess.ini config file.
 * @note Automatic detection for FlagManager will only include a command if it
 *       has required privileges (flags is not -1) and it is not a command
 *       starting with "say".
 *
 * @param client_cmd    Command to register
 * @param function      Callback function
 * @param flags         Admin privilege flags required
 * @param info          Command description
 * @param FlagManager   0 opts out of flag manager, 1 opts in, -1 selects
 *                      automatically
 * @param info_ml       If true, the parameter "info" will be looked up as multilingual key
 *
 * @return              Command id, 0 on failure
 * @error               If an invalid callback function is specified, an error
 *                      will be thrown.
 */
native register_concmd(const cmd[], const function[], flags = -1, const info[] = "", FlagManager = -1, bool:info_ml = false);

/**
 * Registers a callback to be called when the server executes a command from the
 * console.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param client_cmd    Command to register
 * @param function      Callback function
 * @param flags         Admin privilege flags required
 * @param info          Command description
 * @param info_ml       If true, the parameter "info" will be looked up as multilingual key
 *
 * @return              Command id, 0 on failure
 * @error               If an invalid callback function is specified, an error
 *                      will be thrown.
 */
native register_srvcmd(const server_cmd[], const function[], flags = -1, const info[] = "", bool:info_ml = false);

/**
 * Retrieves information about a client command.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param index     Command index
 * @param command   Buffer to copy command name to
 * @param len1      Maximum name buffer size
 * @param flags     Variable to store privilege flags to
 * @param info      Buffer to copy command description to
 * @param len2      Maximum description buffer size
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 * @param info_ml   Variable to store whether the parameter "info" is a multilingual key
 *
 * @return          1 on success, 0 if command was not found
 */
native get_clcmd(index, command[], len1, &flags, info[], len2, flag, &bool:info_ml = false);

/**
 * Returns number of registered client commands.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 *
 * @return          Number of registered client commands
 */
native get_clcmdsnum(flag);

/**
 * Retrieves information about a server command.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param index     Command index
 * @param command   Buffer to copy command name to
 * @param len1      Maximum name buffer size
 * @param flags     Variable to store privilege flags to
 * @param info      Buffer to copy command description to
 * @param len2      Maximum description buffer size
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 * @param info_ml   Variable to store whether the parameter "info" is a multilingual key
 *
 * @return          1 on success, 0 if command was not found
 */
native get_srvcmd(index, server_cmd[], len1, &flags, info[], len2, flag, &bool:info_ml = false);

/**
 * Returns number of registered server commands.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 *
 * @return          Number of registered server commands
 */
native get_srvcmdsnum(flag);

/**
 * Retrieves information about a console command.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param index     Command index
 * @param command   Buffer to copy command name to
 * @param len1      Maximum name buffer size
 * @param flags     Variable to store privilege flags to
 * @param info      Buffer to copy command description to
 * @param len2      Maximum description buffer size
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 * @param id        If set to 0 only server commands will be considered,
 *                  positive will only consider client commands, otherwise
 *                  all console commands will be considered
 * @param info_ml   Variable to store whether the parameter "info" is a multilingual key
 *
 * @return          1 on success, 0 if command was not found
 */
native get_concmd(index, cmd[], len1, &flags, info[], len2, flag, id = -1, &bool:info_ml = false);

/**
 * Returns the parent plugin id of a console command.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param cid           Command index
 * @param flag_mask     Only considers commands that can be accessed with
 *                      the specified privilege flags.
 * @param id_type       If set to 0 only server commands will be considered,
 *                      positive will only consider client commands, otherwise
 *                      all console commands will be considered.
 *
 * @return              Plugin id
 */
native get_concmd_plid(cid, flag_mask, id_type);

/**
 * Returns number of registered console commands.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 *
 * @param flag      Only considers commands that can be accessed with
 *                  the specified privilege flags
 * @param id        If set to 0 only server commands will be considered,
 *                  positive will only consider client commands, otherwise
 *                  all console commands will be considered
 *
 * @return          Number of registered console commands
 */
native get_concmdsnum(flag, id = -1);

/**
 * Returns unique menu id of a menu.
 *
 * @param menu      Menu name
 * @param outside   Catch menus outside the calling plugin
 *
 * @return          Menu id
 */
native register_menuid(const menu[], outside = 0);

/**
 * Registers a callback function to a menu id and keys.
 *
 * @param menuid        Menu id
 * @param keys          Key flags
 * @param function      Callback function
 *
 * @noreturn
 * @error               If an invalid callback function is specified, an error
 *                      will be thrown.
 */
native register_menucmd(menuid, keys, const function[]);

/**
 * Returns if the client is watching a menu.
 *
 * @note If there is no menu, the id is 0. If the id is negative, then the client
 *       views a VGUI menu. Otherwise, the id is an id acquired from the
 *       register_menuid() function.
 *
 * @param index     Client index
 * @param id        Variable to store menu id to
 * @param keys      Variable to store menu keys to
 *
 * @return          1 if client views a menu, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, an error will be thrown.
 */
native get_user_menu(index, &id, &keys);

/**
 * Forces the server to execute the command queue immediately.
 *
 * @note Commands can be added to the queue using server_cmd()
 *
 * @noreturn
 */
native server_exec();

/**
 * Emits a sound from an entity from the engine.
 *
 * @note The sample must be precached using precache_sound() so it is available
 *       in the engine's sound table.
 * @note For a list of available channels, see CHAN_* constants in amxconst.inc,
 *       sounds emitted from the same channel will override each other.
 * @note There are helpful reference constants in amxconst.inc for sound volume
 *       (VOL_*), attenuation (ATTN_*), flags (SND_*), and pitch (PITCH_*).
 *
 * @param index     Entity index, use 0 to emit from all clients
 * @param channel   Channel to emit from
 * @param sample    Sound file to emit
 * @param vol       Volume in percent
 * @param att       Sound attenuation
 * @param flags     Emit flags
 * @param pitch     Sound pitch
 *
 * @noreturn
 */
native emit_sound(index, channel, const sample[], Float:vol, Float:att, flags, pitch);

/**
 * Returns a random floating point value generated by the engine.
 *
 * @param a     Minimum value (inclusive)
 * @param b     Maximum value (inclusive)
 *
 * @return      Generated random value
 */
native Float:random_float(Float:a, Float:b);

/**
 * Returns a random integer value generated by the engine.
 *
 * @param a     Minimum value (inclusive)
 * @param b     Maximum value (inclusive)
 *
 * @return      Generated random value
 */
native random_num(a, b);

/**
 * Returns unique id of a client message.
 *
 * @note Example usage: get_user_msgid("TextMsg")
 * @note The message id is unique as long as the server is running, but might
 *       change between updates. They should not be hardcoded into plugins.
 * @note On first server start, this function will return 0 if used inside
 *       plugin_precache(). Consider hooking RegUserMsg in order to retrieve
 *       the correct message id.
 *
 * @param name      Client message name
 *
 * @return          Message id, 0 if message was not found
 */
native get_user_msgid(const name[]);

/**
 * Retrieves the client message name from a message id.
 *
 * @param msgid     Client message id
 * @param name      Buffer to copy message name to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer, 0 on invalid message id
 */
native get_user_msgname(msgid, name[], len);

/**
 * Returns a unique id for a public variable.
 *
 * @note Variables declared with the "public" specifier are accessible by-name
 *       from outside of the declaring plugin.
 * @note If multiple plugins declare the same public variable, this native will
 *       still return a unique id.
 *
 * @param name      Variable name
 *
 * @return          Xvar id on success, -1 on failure
 */
native get_xvar_id(const name[]);

/**
 * Returns if a public variable exists in any loaded plugin.
 *
 * @param name      Variable name
 *
 * @return          1 if public cvar exists, 0 otherwise
 */
native xvar_exists(const name[]);

/**
 * Returns the integer value of a public variable.
 *
 * @note If multiple plugins declare the same public variable, they are not
 *       automatically synchronized. The xvar system accesses only one of all
 *       public variables directly. Xvars have to be read through the natives or
 *       the value will be incorrect.
 *
 * @param id    Xvar id, an xvar id can be retrieved using get_xvar_id()
 *
 * @return      Xvar integer value
 */
native get_xvar_num(id);

/**
 * Returns the float value of a public variable.
 *
 * @note If multiple plugins declare the same public variable, they are not
 *       automatically synchronized. The xvar system accesses only one of all
 *       public variables directly. Xvars have to be read through the natives or
 *       the value will be incorrect.
 *
 * @param id    Xvar id, an xvar id can be retrieved using get_xvar_id()
 *
 * @return      Xvar float value
 */
native Float:get_xvar_float(id);

/**
 * Sets the integer value of a public variable.
 *
 * @note If multiple plugins declare the same public variable, they are not
 *       automatically synchronized. The xvar system accesses only one of all
 *       public variables directly. Xvars have to be set through the natives or
 *       the xvar will not be updated.
 *
 * @param id        Xvar id, an xvar id can be retrieved using get_xvar_id()
 * @param value     Value to set
 *
 * @noreturn
 * @error           If an invalid xvar id is specified, an error will be thrown.
 */
native set_xvar_num(id, value = 0);

/**
 * Sets the float value of a public variable.
 *
 * @note If multiple plugins declare the same public variable, they are not
 *       automatically synchronized. The xvar system accesses only one of all
 *       public variables directly. Xvars have to be set through the natives or
 *       the xvar will not be updated.
 *
 * @param id        Xvar id, an xvar id can be retrieved using get_xvar_id()
 * @param value     Value to set
 *
 * @noreturn
 * @error           If an invalid xvar id is specified, an error will be thrown.
 */
native set_xvar_float(id, Float:value = 0.0);

/**
 * Returns if a module is loaded.
 *
 * @param name      Module name
 *
 * @return          Module id of the matching module, -1 otherwise
 */
native is_module_loaded(const name[]);

/**
 * Retrieves info about a module by module index.
 *
 * @note For a list of possible status flags, see module_* constants in
 *       amxconst.inc
 *
 * @param id            Module id
 * @param name          Buffer to copy module name to
 * @param nameLen       Maximum name buffer size
 * @param author        Buffer to copy module author to
 * @param authorLen     Maximum author buffer size
 * @param version       Buffer to copy module version to
 * @param versionLen    Maximum version buffer size
 * @param status        Variable to store module status to
 *
 * @return              Module id on success, -1 on invalid module
 */
native get_module(id, name[], nameLen, author[], authorLen, version[], versionLen, &status);

/**
 * Returns the number of currently registered modules.
 *
 * @return  Number of modules
 */
native get_modulesnum();

/**
 * Returns if a plugin is loaded by registered name or filename.
 *
 * @note An example for a registered name would be "Admin Base", while a possible
 *       filename would be "admin.amxx".
 * @note Prior to AMXX 1.80, this function would only search for plugins
 *       registered names, not the filename.
 * @note The plugin name matching is case insensitive, while the filename
 *       matching is case sensitive.
 *
 * @param name          Plugin name or filename
 * @param usefilename   If true searches for plugin filename, false searches for
 *                      plugin name
 *
 * @return              Plugin id of the matching plugin, -1 otherwise
 */
native is_plugin_loaded(const name[], bool:usefilename = false);

/**
 * Retrieves info about a plugin by plugin index.
 *
 * @param index     Plugin index, -1 to target calling plugin
 * @param filename  Buffer to copy plugin filename to
 * @param len1      Maximum filename buffer size
 * @param name      Buffer to copy plugin name to
 * @param len2      Maximum name buffer size
 * @param version   Buffer to copy plugin version to
 * @param len3      Maximum version buffer size
 * @param author    Buffer to copy plugin author to
 * @param len4      Maximum author buffer size
 * @param status    Buffer to copy plugin status flags to
 * @param len5      Maximum status buffer size
 * @param url       Buffer to copy plugin url to
 * @param len6      Maximum url buffer size
 * @param desc      Buffer to copy plugin description to
 * @param len7      Maximum description buffer size
 *
 * @return          Plugin index on success, -1 if there is no plugin with given
 *                  index
 */
native get_plugin(index, filename[] = "", len1 = 0, name[] = "", len2 = 0, version[] = "", len3 = 0, author[] = "", len4 = 0, status[] = "", len5 = 0, url[] = "", len6 = 0, desc[] = "", len7 = 0);

/**
 * Returns the number of loaded AMXX plugins.
 *
 * @return  Number of loaded plugins
 */
native get_pluginsnum();

/**
 * Pauses a plugin so it will not be executed until it is unpaused.
 *
 * @note This used to be able to pause specific functions, but this functionality
 *       (along with the flags "b" and "e") has been deprecated.
 * @note If used without flag "c" this will pause the calling plugin.
 *
 * @param flag      Pause flags
 *                    "a" - pause plugin
 *                    "c" - search for other plugins using param1
 *                    "d" - stop plugin, making it unavailable to unpause
 * @param param1    Plugin filename
 * @param param2    Unused and ignored
 *
 * @return          1 on success, 0 otherwise
 * @error           If it is attempted to use the deprecated functionality,
 *                  an error is thrown.
 */
native pause(const flag[], const param1[] = "", const param2[] = "");

/**
 * Unpauses a plugin so it will resume execution if it was previously paused.
 *
 * @note This used to be able to unpause specific functions, but this
 *       functionality (along with the flags "b" and "e") has been deprecated.
 * @note Without specifying flag "c" this function will do nothing, as a plugin
 *       is incapable of unpausing itself. This is a relict of the deprecated
 *       functionality.
 *
 * @param flag      Pause flags
 *                    "a" - pause plugin
 *                    "c" - search for other plugins using param1
 * @param param1    Plugin filename
 * @param param2    Unused and ignored
 *
 * @return          1 on success, 0 otherwise
 * @error           If it is attempted to use the deprecated functionality,
 *                  an error is thrown.
 */
native unpause(const flag[], const param1[] = "", const param2[] = "");

/**
 * Initiates a function call to this or another plugin by function name.
 *
 * @note This only sets up the function call and covers the pre-requisites.
 *       Push parameters using the callfunc_push_* set of functions. The call
 *       will be executed only upon using callfunc_end()
 *
 * @param func      Function name
 * @param plugin    Plugin filename, if empty the calling plugin is targeted
 *                  The filename has to be the full exact name (e.g. stats.amxx)
 *
 * @return           1 on success
 *                   0 on runtime error
 *                  -1 if plugin was not found
 *                  -2 if function was not found
 * @error           If called while another callfunc has not yet been finished,
 *                  an error is thrown.
 */
native callfunc_begin(const func[], const plugin[] = "");

/**
 * Initiates a function call to this or another plugin by function id.
 *
 * @note This only sets up the function call and covers the pre-requisites.
 *       Push parameters using the callfunc_push_* set of functions. The call
 *       will be executed only upon using callfunc_end()
 * @note The function id can be retrieved by get_func_id()
 *
 * @param func      Function id
 * @param plugin    Plugin filename, if empty the calling plugin is targeted
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
 * Retrieves a functions id for use with callfunc_begin_i()
 *
 * @param funcName  Function name
 * @param pluginId  Plugin id, if -1 the calling plugin is targeted
 *                  The plugin id can be retrieved using find_plugin_byfile()
 *
 * @return          >=0 Function id on success
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
 * @param copyback  If true, any changes made in the called function will be
 *                  copied back to the calling plugin
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_str(const VALUE[], bool:copyback = true);

/**
 * Pushes an array onto the current call.
 *
 * @note This will defy the "const" specifier if copyback is true, which is
 *       only kept for special backwards compatibility.
 *
 * @param VALUE         Array to push
 * @param array_size    Size of the array
 * @param copyback      If true, any changes made in the called function will be
 *                      copied back to the calling plugin
 *
 * @noreturn
 * @error           If called without initiating a callfunc, or the maximum
 *                  amount of parameters is reached, an error is thrown.
 */
native callfunc_push_array(const VALUE[], array_size, bool:copyback = true);

/**
 * Completes the call to a function.
 *
 * @return       1 on success
 *              -1 if the plugin was not found
 *              -2 if the function was not found
 * @error       If called without initiating a callfunc, an error is thrown.
 */
native callfunc_end();

/**
 * Called when an inconsistent file is encountered by the engine.
 *
 * @param id        Client index
 * @param filename  Detected file
 * @param reason    Buffer storing the disconnect reason (can be overwritten)
 *
 * @return          PLUGIN_CONTINUE to let the engine kick the client
 *                  PLUGIN_HANDLED to block the inconsistency kick
 */
forward inconsistent_file(id, const filename[], reason[64]);

/**
 * Forces the clients and server to be running with the same version of a
 * specified file.
 *
 * @note For a list of possible enforcement types, see the force_* constants
 *       in amxconst.inc
 *
 * @param force_type    Enforcement type
 * @param mins          Bounding box mins vector
 * @param maxs          Bounding box maxs vector
 * @param filename      Filename
 *
 * @return              1 on success, 0 otherwise
 */
native force_unmodified(force_type, const mins[3], const maxs[3], const filename[]);

/**
 * Calculates the MD5 keysum of a string.
 *
 * @param szString      String to calculate keysum of
 * @param md5buffer     Buffer to copy the MD5 hash to
 *
 * @return              Number of cells written to the buffer (always 32)
 */
#pragma deprecated Use hash_string() function. Also, see Hash_* constants.
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
#pragma deprecated Use hash_file() function. Also, see Hash_* constants.
native md5_file(const file[], md5buffer[34]);

/**
 * Generate a hash value (message digest)
 *
 * @param string        String to be hashed.
 * @param type          Type of selected hashing algorithm. See Hash_* constants in amxconst.inc file.
 * @param output        Output string to store hash in.
 * @param outputSize    The maximum size of the output string to store hash in.
 *
 * @return              Number of written bytes.
 */
native hash_string(const string[], const HashType:type, output[], const outputSize);

/**
 * Generate a hash value using the contents of a given file
 *
 * @param fileName      Path of file to be hashed.
 * @param type          Type of selected hashing algorithm. See Hash_* constants in amxconst.inc file.
 * @param output        Output string to store hash in.
 * @param outputSize    The maximum size of the output string to store hash in.
 *
 * @return              Number of written bytes.
 * @error               If the file couldn't be opened, an error is thrown.
 */
native hash_file(const fileName[], const HashType:type, output[], const outputSize);

/**
 * Returns the internal flags set on the plugin's state.
 *
 * @param hdr       If nonzero, the function will return the pcode rather than
 *                  state flags
 * @param plid      Plugin id, -1 to target calling plugin
 *
 * @return          Plugin flags
 */
native plugin_flags(hdr = 0, plid = -1);

/**
 * Allows plugins to declare module dependencies using require_module()
 *
 * @deprecated Module dependency has been automatically handled by the compiler
 *             since AMXX 1.50, released in 2005. This forward is no longer
 *             called.
 *
 * @noreturn
 */
#pragma deprecated Module dependency is now automatically handled by the compiler. This forward is no longer called.
forward plugin_modules();

/**
 * Adds a module dependency.
 *
 * @deprecated Module dependency has been automatically handled by the compiler
 *             since AMXX 1.50, released in 2005. This native has no effect.
 *
 * @noreturn
 */
#pragma deprecated Module dependency is now automatically handled by the compiler. This native has no effect.
native require_module(const module[]);

/**
 * Returns if the server is 64 bit.
 *
 * @deprecated As a result of Valve dropping support for 64bit binaries, AMXX is
 *             also not shipping 64bit builds anymore. This native is basically
 *             guaranteed to return 0.
 *
 * @return  1 if the server is 64 bit, 0 otherwise
 */
#pragma deprecated AMXX is not shipping 64bits builds anymore. This native is basically guaranteed to return 0.
native is_amd64_server();

/**
 * Returns plugin id by filename.
 *
 * @param filename      Filename to match
 * @param ignoreCase    If nonzero matches case insensitively, case sensitively
 *                      otherwise
 *
 * @return              Plugin id, -1 (INVALID_PLUGIN_ID) on failure
 */
native find_plugin_byfile(const filename[], ignoreCase = 1);

/**
 * Called before plugin_init(), allows the plugin to register natives.
 *
 * @noreturn
 */
forward plugin_natives();

/**
 * Registers a native.
 *
 * @note Style 0 natives call the handler in the following manner:
 *
 * public native_handler(plugin_id, argc)
 *
 *   plugin_id    - plugin calling the native
 *   argc         - number of parameters
 *
 * @note Style 1 natives are deprecated. Plugins should not use them, they might
 *       break.
 * @note Style 1 natives work a little different. Instead of passing plugin id
 *       and number of parameters, the handler should be prototyped just like the
 *       native would be called. For each by-reference parameter, the plugin
 *       then has to use param_convert() to properly use them.
 * @note A native should *never* recurse. Bad things will happen.
 *
 * @param name      Native name
 * @param handler   Callback function
 * @param style     Native style
 *
 * @noreturn
 * @error           If an invalid callback is specified, an error is thrown.
 */
native register_native(const name[], const handler[], style = 0);

/**
 * Registers the plugin as a library.
 *
 * @note To mark a library as required, place the following in the include
 *       file:
 *       #pragma reqlib <name>
 *       #if !defined AMXMODX_NOAUTOLOAD
 *          #pragma loadlib <name>
 *       #endif
 *
 * @noreturn
 */
native register_library(const library[]);

/**
 * Logs an error in the native and breaks into the AMXX debugger.
 *
 * @note This acts as if the calling plugin - the plugin that is calling the
 *       native, not the plugin calling this function - triggered the error,
 *       just like when AMXX natives error.
 *
 * @param error     Error number
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @noreturn
 * @error           The function is guaranteed to throw an error, but will make
 *                  it appear as if the plugin calling the native triggered it.
 */
native log_error(error, const fmt[], any:...);

/**
 * Converts a parameter to work as a by-reference parameter.
 *
 * @deprecated Style 1 natives are deprecated and should be converted to
 *             style 0. This should not be used.
 *
 * @note This only needs to be called if the native was registered with style 1.
 * @note Remember that arrays (and strings) are always by-reference and need to
 *       be converted.
 *
 * @param num       Argument to convert, starting from 1
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 0, an error will be thrown.
 */
native param_convert(num);

/**
 * Retrieves a string from the plugin calling the native.
 *
 * @param param     Argument to retrieve, starting from 1
 * @param dest      Buffer to copy string to
 * @param maxlen    Maximum size of buffer
 *
 * @return          Number of cells copied to buffer
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native get_string(param, dest[], maxlen);

/**
 * Copies a string to the plugin calling the native.
 *
 * @param param     Argument to set, starting from 1
 * @param dest      Buffer to copy string from
 * @param maxlen    Maximum size of buffer
 *
 * @return          Number of cells copied from buffer
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native set_string(param, dest[], maxlen);

/**
 * Returns the integer value of a parameter from the plugin calling the native.
 *
 * @param param     Argument to retrieve, starting from 1
 *
 * @return          Integer value
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native get_param(param);

/**
 * Returns the float value of a parameter from the plugin calling the native.
 *
 * @param param     Argument to retrieve, starting from 1
 *
 * @return          Float value
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native Float:get_param_f(param);

/**
 * Returns the integer value of a by-reference parameter from the plugin calling
 * the native.
 *
 * @param param     Argument to retrieve, starting from 1
 *
 * @return          Integer value
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native get_param_byref(param);

/**
 * Returns the float value of a by-reference parameter from the plugin calling
 * the native.
 *
 * @param param     Argument to retrieve, starting from 1
 *
 * @return          Float value
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native Float:get_float_byref(param);

/**
 * Sets the integer value of a by-reference parameter to the plugin calling the
 * native.
 *
 * @param param     Argument to set, starting from 1
 * @param value     Value to set parameter to
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native set_param_byref(param, value);

/**
 * Sets the float value of a by-reference parameter to the plugin calling the
 * native.
 *
 * @param param     Argument to set, starting from 1
 * @param value     Value to set parameter to
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native set_float_byref(param, Float:value);

/**
 * Retrieves an array from the plugin calling the native.
 *
 * @param param     Argument to retrieve, starting from 1
 * @param dest      Buffer to copy array to
 * @param maxlen    Size of buffer
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native get_array(param, dest[], size);

/**
 * Retrieves a float array from the plugin calling the native.
 *
 * @param param     Argument to retrieve, starting from 1
 * @param dest      Buffer to copy array to
 * @param maxlen    Size of buffer
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native get_array_f(param, Float:dest[], size);

/**
 * Copies an array to the plugin calling the native.
 *
 * @param param     Argument to set, starting from 1
 * @param source    Buffer to copy array from
 * @param maxlen    Size of buffer
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native set_array(param, const source[], size);

/**
 * Copies a float array to the plugin calling the native.
 *
 * @param param     Argument to set, starting from 1
 * @param source    Buffer to copy array from
 * @param maxlen    Size of buffer
 *
 * @noreturn
 * @error           If used outside of a native callback, or the native was
 *                  created with style 1, an error will be thrown.
 */
native set_array_f(param, const Float:source[], size);

/**
 * Allows to trap error messages that occur in a plugin.
 *
 * @note This can be used to override the debug messages that occur when the
 *       plugin causes some kind of runtime error.
 * @note The handler will be called in the following manner:
 *
 * public error_filter(error_code, bool:debugging, message[])
 *
 *   error_code      - AMX_ERR_* code.
 *   debugging       - True if the plugin is in debug mode, false otherwise
 *   message[]       - Message sent along with the error
 *
 * @note The handler should return PLUGIN_CONTINUE to let the error through the
 *       filter, or PLUGIN_HANDLED to block the error from displaying.
 *
 * @param handler       Function name to call
 *
 * @noreturn
 * @error               If an invalid callback function is provided, an error
 *                      is thrown.
 */
native set_error_filter(const handler[]);

/**
 * Returns a trace handle for the item at the top of the traced call stack.
 *
 * @note Intended for use inside an error handler set with set_error_filter()
 *
 * @return      Trace handle, 0 if no debugging information is available
 */
native dbg_trace_begin();

/**
 * Returns the next item in a traced call stack.
 *
 * @param trace     Trace handle
 *
 * @return          New trace handle, 0 if no more traces exist
 */
native dbg_trace_next(trace);

/**
 * Retrieves the call stack info for a trace.
 *
 * @param trace         Trace handle
 * @param line          Variable to set line at which plugin failed to
 * @param function      Buffer to copy function to
 * @param maxLength1    Maximum function buffer size
 * @param file          Buffer to copy filename to
 * @param maxLength2    Maximum filename buffer size
 *
 * @return              1 on success, 0 if no trace data is available
 */
native dbg_trace_info(trace, &line, function[], maxLength1, file[], maxLength2);

/**
 * Retrieves the formatted error string from a trace.
 *
 * @note The string format is generally: "Run time error <errno>: <description>"
 *
 * @param buffer        Buffer to copy error message to
 * @param maxLength     Maximum buffer size
 *
 * @return              1 on success, 0 if no trace data is available
 */
native dbg_fmt_error(buffer[], maxLength);

/**
 * Sets a native filter, letting the plugin intercept and handle an
 * automatic native requirement.
 *
 * @note This has to be used inside the plugin_native() forward, otherwise it
 *       has no effect.
 * @note This is useful for creating plugins that can dynamically decide which
 *       modules or features to use at runtime, often necessary for cross-mod
 *       plugins. It allows to deploy a single version of the plugin instead
 *       of compiling multiple versions for each use-case.
 * @note The handler will be called in the following manner:
 *
 * public native_filter(const native[], index, trap)
 *
 *   native      - Native name
 *   index       - Native index
 *   trap        - 0 if native couldn't be found, 1 if native use was attempted
 *
 * @note The handler should return PLUGIN_CONTINUE to let the error through the
 *       filter (which will throw a run-time error), or return PLUGIN_HANDLED
 *       to continue operation.
 * @note Returning PLUGIN_CONTINUE if trap is 0 will result in the plugin
 *       failing to load!
 *
 * @param handler   Function name to call
 *
 * @return          1 if handler is set successfully, 0 otherwise (called
 *                  outside of plugin_native() forward)
 * @error           If an invalid callback function is provided, an error is
 *                  thrown.
 */
native set_native_filter(const handler[]);

/**
 * Sets a module/library filter, letting the plugin intercept and handle an
 * automatic module requirement.
 *
 * @note This has to be used inside the plugin_native() forward, otherwise it
 *       has no effect.
 * @note This is useful for creating plugins that can dynamically decide which
 *       modules or features to use at runtime, often necessary for cross-mod
 *       plugins. It allows to deploy a single version of the plugin instead
 *       of compiling multiple versions for each use-case.
 * @note For a list of possible libtypes see the LibType enum in amxconst.inc
 * @note The handler will be called in the following manner:
 *
 * public module_filter(const library[], LibType:type)
 *
 *   library     - Shortname of library or class that is required
 *   libtrype    - Type of requirement being checked (library/module or class)
 *
 * @note The handler should return PLUGIN_CONTINUE to let the error through the
 *       filter (which will result in the plugin failing to load), or
 *       PLUGIN_HANDLED to imply that load can continue without the module.
 * @note Errors occuring inside the handler will not be filtered and cause the
 *       plugin to fail load as if the handler returned PLUGIN_CONTINUE.
 *
 * @return      0 on success, -1 if filtering is not available, -2 if handler
 *              could not be found.
 */
native set_module_filter(const handler[]);

/**
 * Aborts execution of the current callback by throwing an error.
 *
 * @note Warning: This function should not be used inside error filters, module
 *       filters (native filters are safe if trap equals 1) or the
 *       plugin_natives() forward.
 * @note The message will automatically be tagged with the plugin's name and the
 *       log will include a timestamp with the message.
 * @note For a list of possible error codes, see AMX_* constants in amxconst.inc
 *
 * @param error     Error code
 * @param fmt       Formatting rules
 * @param ...       Variable list of formatting parameters
 *
 * @noreturn
 * @error           The function is guaranteed to throw an error, using the
 *                  specified custom log message.
 */
native abort(error, const fmt[] = "", any:...);

/**
 * Returns if a specific module is loaded.
 *
 * @note This uses the same method AMXX uses internally to see if a module is
 *       required by a plugin.
 * @note Example usage: module_exists("cstrike")
 *
 * @param logtag    Module shortname
 *
 * @return          1 if module is loaded, 0 otherwise
 */
native module_exists(const logtag[]);

/**
 * Returns if a specific library or class is loaded.
 *
 * @note This is the newer version of module_exists(), enabling users to
 *       distinguish between libraries and classes, while module_exists() always
 *       checks for both types.
 * @note For a list of possible types, see the LibType enum in amxconst.inc
 *
 * @param library   Library/Class shortname
 * @param type      Type to search for
 *
 * @return          1 if module is loaded, 0 otherwise
 */
native LibraryExists(const library[], LibType:type);

/**
 * Returns the next valid hudchannel for the client.
 *
 * @note This function uses the same method set_hudmessage() uses to determine
 *       the next channel if it is set to auto-select.
 *
 * @param player    Client index
 *
 * @return          Valid hudchannel (1-4)
 * @error           If the index is not within the range of 1 to MaxClients or
 *                  the client is not connected, an error will be thrown.
 */
native next_hudchannel(player);

/**
 * Creates a HUD synchronization object.
 *
 * @note Create one of these for each section of the screen that contains
 *       overlapping HUD messages. For example, if using both sides of the
 *       screen to display three messages that could potentially overlap,
 *       each side is considered a synchronizable area. You can then use
 *       ShowSyncHudMsg() to correctly synchronize displaying the HUD message
 *       with any other messages potentially in its class.
 * @note This does not do anything like reserving screen area. Its sole
 *       purpose is to be able to wipe an old message on an auto-channel and
 *       ensure that it will not clear a message from another plugin.
 *
 * @param num   Unused and ignored
 * @param ...   Unused and ignored
 *
 * @return      HUD sync object handle
 */
native CreateHudSyncObj(num = 0, ...);

/**
 * Displays a synchronized HUD message.
 *
 * @note This will check that the HUD object has its previous display on the
 *       screen cleared before it proceeds to write another message. It will
 *       only do this in the case of that channel not having been cleared
 *       already.
 * @note This uses the display parameters set with set_hudmessage(), ignoring
 *       the selected channel in favor of its own synchronization.
 * @note This functions return value behaves differently depending on what is
 *       used as the client index: If 0 is specified, then the function will
 *       return 0 if nothing has been sent (no client connected). If either a
 *       single client is specified, or there is at least one client connected,
 *       the number of printed characters will refer to the message that is sent
 *       last, to the client with the highest index.
 *
 * @param target    Client index, use 0 to display to all clients
 * @param syncObj   HUD sync object handle
 * @param fmt       Formatting rules
 * @param ...       Variable number of formatting parameters
 *
 * @return          Number of printed characters
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native ShowSyncHudMsg(target, syncObj, const fmt[], any:...);

/**
 * Clears the display on a HUD sync object.
 *
 * @note This sends an empty message to the previously occupied HUD channel.
 *       It is not quite the same as manually sending an empty message to the
 *       sync object as that would send out two separate messages, one for
 *       clearing the occupied channel and another using a new channel, which
 *       will subsequently not mark the sync object as cleared.
 *
 * @param target    Client index, use 0 to display to all clients
 * @param syncObj   HUD sync object handle
 *
 * @noreturn
 * @error           If a single client is specified and the index is not within
 *                  the range of 1 to MaxClients, an error will be thrown.
 */
native ClearSyncHud(target, syncObj);

/**
 * Triggers the software interrupt 3, used for breaking into an attached
 * debugger.
 *
 * @note Warning: This is a debugging function that is not intended for general
 *       plugin use. Using this function will either halt the server and break
 *       into the attached debugger, or outright crash the server if no
 *       debugger is attached.
 *
 * @noreturn
 */
native int3();

/**
 * Sets the calling plugin to a failed state.
 *
 * @note Calling this will cause the calling plugin to completely cease
 *       operation. It is not possible to recover.
 * @note This should be used to gracefully handle fatal errors. The log message
 *       will appear in the AMXX error log.
 *
 * @param fmt   Formatting rules
 * @param ...   Variable number of formatting parameters
 *
 * @noreturn
 * @error       The function is guaranteed to throw a fatal error, ceasing
 *              further operation of the plugin.
 */
native set_fail_state(const fmt[], any:...);

/**
 * Returns the reference address of the variable passed in.
 *
 * @note Addresses are local to the plugin and do not represent a full CPU
 *       address.
 *
 * @param ...   Variable to retrieve address from
 *
 * @return      Variable address
 */
native get_var_addr(any:...);

/**
 * Returns the value of an address.
 *
 * @note Addresses can be acquired using get_var_addr()
 *
 * @param addr      Variable address
 *
 * @return          Value at address
 * @error           If the plugin attempts to access an address outside of the
 *                  stack or heap limits of the plugin, an error will be thrown.
 */
native get_addr_val(addr);

/**
 * Sets the value of an address.
 *
 * @note Addresses can be acquired using get_var_addr()
 *
 * @param addr      Variable address
 * @param val       Value to set
 *
 * @noreturn
 * @error           If the plugin attempts to access an address outside of the
 *                  stack or heap limits of the plugin, an error will be thrown.
 */
native set_addr_val(addr, val);

/**
 * Creates a global forward that will be called in all plugins.
 *
 * @note For a list of valid stop types, see the ET_* constants in amxconst.inc
 * @note For a list of valid parameter types, see the FP_* constants in
 *       amxconst.inc
 *
 * @param name          Function name to call
 * @param stop_type     Treatment of the plugin return values
 * @param ...           List of parameter types
 *
 * @return              Forward handle, -1 on failure
 */
native CreateMultiForward(const name[], stop_type, ...);

/**
 * Creates a private forward that will be called in a single plugin.
 *
 * @note Unlike other natives expecting a plugin id, specifying -1 will not
 *       select the calling plugin, and instead throw an error.
 *
 * @param plugin_id     Plugin to call forward in. The plugin id can be
 *                      retrieved using find_plugin_byfile()
 * @param name          Function name to call
 * @param ...           List of parameter types
 *
 * @return              Forward handle, -1 on failure
 * @error               If an invalid plugin id is specified, an error will be
 *                      thrown.
 */
native CreateOneForward(plugin_id, const name[], ...);

/**
 * Prepares an array for use in a forward. Pass the result ExecuteForward()
 * instead of the array itself.
 *
 * @param array         Array to prepare
 * @param size          Size of array
 * @param copyback      If nonzero, modifications made by the called plugin(s)
 *                      will be copied back to the caller
 *
 * @return              Special handle for use in ExecuteForward()
 */
native PrepareArray(const array[], size, copyback = 0);

/**
 * Executes a forward.
 *
 * @note Passing arrays requires them to be prepared using PrepareArray()
 *
 * @param forward_handle    Forward handle
 * @param ret               Optional variable to store return value in
 * @param ...               Variable number of parameters to pass through
 *
 * @return                  1 on success, 0 if forward can't be executed
 * @error                   If the number of parameters mismatch from the number
 *                          of parameters that the forward was declared with,
 *                          an error is thrown.
 */
native ExecuteForward(forward_handle, &ret = 0, any:...);

/**
 * Destroys and deallocates a forward.
 *
 * @note Does not distinguish between private and global forwards.
 *
 * @param forward_handle    Forward handle
 *
 * @noreturn
 */
native DestroyForward(forward_handle);

/**
 * Sets all elements of array to a specified value.
 *
 * @param array     Array to modify
 * @param value     Value to set each element to
 * @param size      Size of array
 *
 * @noreturn
 */
native arrayset(any:array[], any:value, size);

/**
 * Returns the weapon id associated with a weapon name.
 *
 * @note The weapon name is case sensitive and has the weapon_* form.
 *
 * @param name      Weapon name
 *
 * @return          Weapon id, or 0 if no id was found
 */
native get_weaponid(const name[]);

/**
 * Adds an admin to the dynamic admin storage for lookup at a later time.
 *
 * @note For a list of possible access flags, see the ADMIN_* constants in
 *       amxconst.inc
 * @note For a list of possible auth flags, see the FLAG_* constants in
 *       amxconst.inc
 *
 * @param AuthData      Auth information to set (can be name, IP or SteamID)
 * @param Password      Password to set
 * @param Access        Admin access flags
 * @param Flags         Auth behavior flags
 *
 * @noreturn
 */
native admins_push(const AuthData[], const Password[], Access, Flags);

/**
 * Returns the number of admins in the dynamic admin storage.
 *
 * @return  Number of admins
 */
native admins_num();

/**
 * Retrieves information about a dynamically stored admin.
 *
 * @note For a list of possible props, see the AdminProp enum in amxconst.inc
 *
 * @param num           Admin storage index
 * @param Property      Admin property to retrieve
 * @param Buffer        Buffer to copy property information to, if AdminProp_Auth
 *                      or AdminProp_Password is specified
 * @param BufferSize    Maximum buffer size
 *
 * @return              Property value if AdminProp_Access or AdminProp_Flags
 *                      is requested, 0 otherwise
 * @error               If an invalid storage index is specified, an error will
 *                      be thrown.
 */
native admins_lookup(num, AdminProp:Property, Buffer[] = "", BufferSize = 0);

/**
 * Clears the list of dynamically stored admins.
 *
 * @noreturn
 */
native admins_flush();

/**
 * Returns if a map contains at least one entity with the provided class name.
 *
 * @param classname     Entity classname to match
 *
 * @return              True if an entity is found, false otherwise
 */
native bool:has_map_ent_class(const classname[]);


/**
 * Called when the map has loaded, and all configs are done executing.
 * This includes servercfgfile (server.cfg), amxx.cfg, plugin's config, and
 * per-map config.
 *
 * @note This is best place to initialize plugin functions which are based on cvar data.  
 * @note This will always be called once and only once per map.  It will be 
 *       called few seconds after plugin_cfg().
 *
 * @noreturn
 */
forward OnConfigsExecuted();

/**
 * Called when the map has loaded, right after plugin_cfg() but any time 
 * before OnConfigsExecuted.  It's called after amxx.cfg and  all 
 * AutoExecConfig() exec commands have been added to the server command buffer.
 *
 * @note This will always be called once and only once per map.
 *
 * @noreturn
 */
forward OnAutoConfigsBuffered();

/**
 * Specifies that the given config file should be executed after plugin load.
 *
 * @note OnConfigsExecuted() will not be called until the config file has executed,
 *       but it will be called if the execution fails.
 * @note The name parameter should not contain dots, otherwise file will not be executed.
 *
 * @param autoCreate    If true, and the config file does not exist, such a config
 *                      file will be automatically created and populated with
 *                      information from the plugin's registered cvars.
 * @param name          Name of the config file, excluding the .cfg extension.
 *                      If empty, <plugin.filename.cfg> is assumed.
 * @param folder        Folder under plugins/ to use.
 *
 * @noreturn
 */
native AutoExecConfig(bool:autoCreate = true, const name[] = "", const folder[] = "");

/**
 * Creates a single use hook for the next frame.
 *
 * @param callback      Function to be executed on the next frame.
 * @param data          Optional data to be passed to the callback function.
 *
 * @note                Callback function prototype:
 *                          public function(data)
 *
 * @noreturn
 */
native RequestFrame(const callback[], any:data = 0);

// Always keep this at the bottom of this file
#include <message_stocks>
