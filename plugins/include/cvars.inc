// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _cvars_included
	#endinput
#endif
#define _cvars_included

/**
 * CVAR flags for create_cvar() and register_cvar().
 */
#define FCVAR_NONE              0   // No special behavior
#define FCVAR_ARCHIVE           1   // Cvar will be saved to vars.rc Set to cause it to be saved to vars.rc
#define FCVAR_USERINFO          2   // Cvar changes the client's info string
#define FCVAR_SERVER            4   // Clients get notified when cvar value is changed
#define FCVAR_EXTDLL            8   // Defined by an external DLL
#define FCVAR_CLIENTDLL         16  // Defined by the client DLL
#define FCVAR_PROTECTED         32  // Cvar value is masked from outside access, should be used for sensitive cvars like passwords
#define FCVAR_SPONLY            64  // Cvar can't be changed by clients connected to a multiplayer server
#define FCVAR_PRINTABLEONLY     128 // The cvar string value can not contain unprintable characters
#define FCVAR_UNLOGGED          256 // If the cvar is FCVAR_SERVER, don't log changes to a file/the console
#define FCVAR_NOEXTRAWHITEPACE  512 // Automatically strips trailing/leading white space from the string value

/**
 * Cvar bound constants used with [get|set]_pcvar_bounds().
 */
enum CvarBounds
{
	CvarBound_Upper = 0,
	CvarBound_Lower
};

/**
 * Creates a new cvar for the engine.
 *
 * @note This has the same effect as register_cvar() but provides more options.
 * @note For a list of possible cvar flags see FCVAR_* constants above.
 * @note If an already existing cvar is registered it will not be duplicated.
 *       The default value is only set when the cvar is registered for the very
 *       first time since the server was started. Cvar bounds are overwritten
 *       by the create_cvar() call just as if they were re-set using
 *       set_pcvar_bounds().
 * @note The returned cvar pointer should be used with the get_pcvar_* and
 *       set_pcvar_* set of functions.
 *
 * @param name          Cvar name
 * @param string        Default cvar value
 * @param flags         Optional bitsum of flags specifying cvar behavior
 * @param description   Optional description of the cvar
 * @param has_min       Optional boolean that specifies if the cvar has a
 *                      minimum value
 * @param min_val       Minimum floating point value
 * @param has_max       Optional boolean that specifies if the cvar has a
 *                      maximum value
 * @param max_val       Maximum floating point value
 *
 * @return              Unique cvar pointer
 * @error               If invalid bounds are provided (min_val > max_val or
 *                      vice versa), an error will be thrown.
 */
native create_cvar(const name[], const string[], flags = FCVAR_NONE, const description[] = "", bool:has_min = false, Float:min_val = 0.0, bool:has_max = false, Float:max_val = 0.0);

/**
 * Registers a new cvar for the engine.
 *
 * @note Deprecated. Consider to use create_cvar for more options.
 * @note For a list of possible cvar flags see FCVAR_* constants in cvars.inc
 * @note If an already existing cvar is registered it will not be duplicated.
 *       The default value is only set when the cvar is registered for the very
 *       first time since the server was started.
 * @note The returned cvar pointer should be used with the get_pcvar_* and
 *       set_pcvar_* set of functions.
 *
 * @param name      Cvar name
 * @param string    Default cvar value
 * @param flags     Optional bitsum of flags specifying cvar behavior
 * @param fvalue    Unused
 *
 * @return          Unique cvar pointer
 */
native register_cvar(const name[], const string[], flags = FCVAR_NONE, Float:fvalue = 0.0);

/**
 * Returns if a cvar is registered on the server.
 *
 * @param cvar      Cvar name to check
 *
 * @return          1 if the cvar exists, 0 otherwise
 */
native cvar_exists(const cvar[]);

/**
 * Returns the cvar pointer of the specified cvar.
 *
 * @note A pointer is also returned by register_cvar() and create_cvar().
 *       Plugins can (and should) retrieve and use pointers for already existing
 *       mod cvars.
 *
 * @param cvar      Cvar name to find
 *
 * @return          Cvar pointer on success, 0 if cvar was not found
 */
native get_cvar_pointer(const cvar[]);

/**
 * Creates a hook for when a cvar's value is changed.
 *
 * @note Changing the cvar value from within this forward can lead to infinite
 *       recursion and should be avoided.
 * @note The callback will be called in the following manner:
 *
 * public cvar_change_callback(pcvar, const old_value[], const new_value[])
 *
 *   pcvar         - Pointer to cvar that was changed
 *   old_value     - Buffer containing the previous value of the cvar
 *   new_value     - Buffer containing the new value of the cvar
 *
 * The return value is ignored
 *
 * @param pcvar     Pointer to cvar
 * @param callback  Name of callback function
 *
 * @return          Callback handle that can be used with
 *                  [disable|enable]_cvar_hook
 * @error           If an invalid cvar pointer or callback function is provided,
 *                  an error will be thrown.
 */
native cvarhook:hook_cvar_change(pcvar, const callback[]);

/**
 * Disables a cvar hook, stopping it from being called.
 *
 * @note Use the handle returned by hook_cvar_change as the parameter here.
 *
 * @param handle    Forward to disable
 * @error           If an invalid hook handle is provided, an error will be
 *                  thrown.
 */
native disable_cvar_hook(cvarhook:handle);

/**
 * Enables a cvar hook, restoring it to being called.
 *
 * @note Use the handle returned by hook_cvar_change as the parameter here.
 *
 * @param handle    Forward to enable
 * @error           If an invalid hook handle is provided, an error will be
 *                  thrown.
 */
native enable_cvar_hook(cvarhook:handle);

/**
 * Returns flags of a cvar. The cvar is accessed by name.
 *
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent get_pcvar_flags() function should be used
 *       instead.
 *
 * @param cvar  Cvar name to retrieve flags from
 *
 * @return      Flag value
 */
native get_cvar_flags(const cvar[]);

/**
 * Sets specified flags to a cvar. The cvar is accessed by name.
 *
 * @note Not permitted for the "amx_version", "amxmodx_version", "fun_version"
 *       and "sv_cheats" cvars.
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 * @note This function just adds the flags using a bitwise-or operation. After
 *       it has run the flags may not exactly equal the specified bitflag sum.
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent set_pcvar_flags() function should be used
 *       instead.
 *
 * @param cvar      Cvar name to remove flags from
 * @param flags     Bitflag sum of flags to set
 *
 * @return          1 on success, 0 if cvar does not exist or is not permitted
 */
native set_cvar_flags(const cvar[], flags);

/**
 * Removes specified flags from a cvar. The cvar is accessed by name.
 *
 * @note Not permitted for the "amx_version", "amxmodx_version", "fun_version"
 *       and "sv_cheats" cvars.
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 * @note This function removes the flags using a bitwise-and operation.
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the set_pcvar_flags() function should be used instead.
 *
 * @param cvar      Cvar name to remove flags from
 * @param flags     Bitflag sum of flags to remove
 *
 * @return          1 on success, 0 if cvar does not exist or is not permitted
 */
native remove_cvar_flags(const cvar[], flags=-1);

/**
 * Gets a string value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent get_pcvar_string() function should be used
 *       instead.
 *
 * @param cvar      Cvar name to retrieve value from
 * @param output    Buffer to copy cvar value to
 * @param iLen      Maximum size of the buffer
 *
 * @return          Number of cells written to buffer.
 */
native get_cvar_string(const cvarname[], output[], iLen);

/**
 * Sets a cvar to a given string value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent set_pcvar_string() function should be used
 *       instead.
 *
 * @param cvar      Cvar name to set value of
 * @param value     Value to set cvar to
 *
 * @noreturn
 */
native set_cvar_string(const cvar[], const value[]);

/**
 * Returns a floating value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent get_pcvar_float() function should be used
 *       instead.
 *
 * @param cvarname  Cvar name to retrieve value from
 *
 * @return          Cvar value, converted to float
 */
native Float:get_cvar_float(const cvarname[]);

/**
 * Sets a cvar to a given float value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent set_pcvar_float() function should be used
 *       instead.
 *
 * @param cvar      Cvar name to set value of
 * @param value     Value to set cvar to
 *
 * @noreturn
 */
native set_cvar_float(const cvar[], Float:value);

/**
 * Returns an integer value from a cvar. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent get_pcvar_num() function should be used
 *       instead.
 *
 * @param cvarname  Cvar name to retrieve value from
 *
 * @return          Cvar value, converted to int
 */
native get_cvar_num(const cvarname[]);

/**
 * Sets a cvar to a given integer value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name is slower than direct pointer access, which is
 *       why the otherwise equivalent set_pcvar_num() function should be used
 *       instead.
 *
 * @param cvar      Cvar name to set value of
 * @param value     Value to set cvar to
 *
 * @noreturn
 */
native set_cvar_num(const cvarname[], value);

/**
 * Returns flags of a cvar via direct pointer access.
 *
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 *
 * @param pcvar     Pointer to cvar to retrieve flags from
 *
 * @return          1 on success, 0 if cvar pointer is invalid
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native get_pcvar_flags(pcvar);

/**
 * Sets specified flags to a cvar via direct pointer access.
 *
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 * @note This function directly sets the provided bitflag, unlike set_cvar_flags
 *       which adds them using a bitwise OR.
 *
 * @param pcvar     Pointer to cvar to set flags of
 * @param flags     Bitflag sum of flags to set
 *
 * @return          1 on success, 0 if cvar does not exist or is not permitted
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native set_pcvar_flags(pcvar, flags);

/**
 * Returns an integer value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to int
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native get_pcvar_num(pcvar);

/**
 * Returns an boolean value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to bool
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native bool:get_pcvar_bool(pcvar);

/**
 * Sets an integer value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native set_pcvar_num(pcvar, num);

/**
 * Sets a boolean value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native set_pcvar_bool(pcvar, bool:num);

/**
 * Returns a float value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to float
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native Float:get_pcvar_float(pcvar);

/**
 * Sets a float value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native set_pcvar_float(pcvar, Float:num);

/**
 * Returns a string value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 * @param string    Buffer to copy cvar value to
 * @param maxlen    Maximum size of the buffer
 *
 * @return          Number of cells written to buffer.
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native get_pcvar_string(pcvar, string[], maxlen);

/**
 * Sets a string value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 * @param string    Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is provided, an error will be
 *                  thrown.
 */
native set_pcvar_string(pcvar, const string[]);

/**
 * Retrieves the specified value boundary of a cvar.
 *
 * @param pcvar     Pointer to cvar
 * @param type      Type of boundary to retrieve
 * @param value     Variable to store the specified boundary to
 *
 * @return          True if the cvar has a boundary set, false otherwise
 * @error           If an invalid cvar pointer or boundary type is provided,
 *                  an error will be thrown.
 */
native bool:get_pcvar_bounds(pcvar, CvarBounds:type, &Float:value);

/**
 * Sets the specified boundary of a cvar.
 *
 * @param pcvar     Pointer to cvar
 * @param type      Type of boundary to set
 * @param set       If true the cvar boundary will be set, otherwise it will be
 *                  removed (value is ignored)
 * @param value     Floating point value to use as the boundary
 *
 * @noreturn
 * @error           If an invalid cvar pointer or boundary type is provided, an
 *                  error will be thrown.
 */
native set_pcvar_bounds(pcvar, CvarBounds:type, bool:set, Float:value = 0.0);

/**
 * Binds a cvar's integer value to a global variable. The variable will then
 * always contain the current cvar value as it is automatically kept up to date.
 *
 * @note The variable *has* to be a global or a static variable. Local variables
 *       created within functions can not be used for technical reasons.
 * @note Variables can not be bound to multiple cvars.
 *
 * @param pcvar     Pointer to cvar
 * @param var       Global variable to keep updated
 *
 * @noreturn
 * @error           If an invalid cvar pointer or variable is provided, an error
 *                  will be thrown.
 */
native bind_pcvar_num(pcvar, &any:var);

/**
 * Binds a cvar's float value to a global variable. The variable will then
 * always contain the current cvar value as it is automatically kept up to date.
 *
 * @note The variable *has* to be a global or a static variable. Local variables
 *       created within functions can not be used for technical reasons.
 * @note Variables can not be bound to multiple cvars.
 *
 * @param pcvar     Pointer to cvar
 * @param var       Global variable to keep updated
 *
 * @noreturn
 * @error           If an invalid cvar pointer or variable is provided, an error
 *                  will be thrown.
 */
native bind_pcvar_float(pcvar, &Float:var);

/**
 * Binds a cvar's string value to a global array. The array will then
 * always contain the current cvar value as it is automatically kept up to date.
 *
 * @note The array *has* to be a global or a static array. Local arrays
 *       created within functions can not be used for technical reasons.
 * @note Arrays can not be bound to multiple cvars.
 *
 * @param pcvar     Pointer to cvar
 * @param var       Global array to keep updated
 * @param varlen    Maximum length of string array
 *
 * @noreturn
 * @error           If an invalid cvar pointer or variable is provided, an error
 *                  will be thrown.
 */
native bind_pcvar_string(pcvar, any:var[], varlen);

/**
 * Returns the number of plugin-registered cvars.
 *
 * @return  Number of registered cvars
 */
native get_plugins_cvarsnum();

/**
 * Retrieves information about a plugin-registered cvar via iterative access.
 *
 * @note The returned cvar pointer should be used with the get_pcvar_* and
 *       set_pcvar_* set of functions.
 * @note The cvar index does not equal the cvar pointer. It is the internal
 *       AMXX id of a cvar, incremented for each registered cvar.
 *
 * @param num           Index to retrieve
 * @param name          Buffer to copy cvar name to
 * @param namelen       Maximum buffer size
 * @param flags         Variable to store cvar flags to
 * @param plugin_id     Variable to store id of the registering plugin to
 * @param pcvar_handle  Variable to store cvar pointer to
 * @param description   Variable to store cvar description to
 * @param desc_len      Maximum length of string buffer
 *
 * @return              1 on success, 0 if index is invalid
 */
native get_plugins_cvar(num, name[], namelen, &flags = 0, &plugin_id = 0, &pcvar_handle = 0, description[] = "", desc_len = 0);

/**
 * Dispatches a client cvar query, allowing the plugin to query for its value on
 * the client.
 *
 * @note The callback will be called in the following manner:
 *
 * public cvar_query_callback(id, const cvar[], const value[], const param[])
 *
 *   id      - Client index
 *   cvar    - Cvar queried
 *   value   - Cvar value on the client
 *   param   - Optional extra data
 *
 * @param id            Client index
 * @param cvar          Cvar to query
 * @param resultFunc    Callback function
 * @param paramlen      Size of extra data
 * @param params        Extra data to pass through to callback
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, the client is not connected, the callback
 *                      function is invalid or the querying process encounters
 *                      a problem, an error will be thrown.
 */
native query_client_cvar(id, const cvar[], const resultFunc[], paramlen = 0, const params[] = "");
