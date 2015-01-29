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
 * CVAR flags for create_cvar()
 */
#define FCVAR_NONE              0   /* No flags */
#define FCVAR_ARCHIVE           1   /* Set to cause it to be saved to vars.rc */
#define FCVAR_USERINFO          2   /* Changes the client's info string */
#define FCVAR_SERVER            4   /* Notifies players when changed */
#define FCVAR_EXTDLL            8   /* Defined by external DLL */
#define FCVAR_CLIENTDLL         16  /* Defined by the client dll */
#define FCVAR_PROTECTED         32  /* It's a server cvar, but we don't send the data since it's a password, etc.  Sends 1 if it's not bland/zero, 0 otherwise as value */
#define FCVAR_SPONLY            64  /* This cvar cannot be changed by clients connected to a multiplayer server. */
#define FCVAR_PRINTABLEONLY     128 /* This cvar's string cannot contain unprintable characters ( e.g., used for player name etc ). */
#define FCVAR_UNLOGGED          256 /* If this is a FCVAR_SERVER, don't log changes to the log file / console if we are creating a log */
#define FCVAR_NOEXTRAWHITEPACE  512 /* Strip trailing/leading white space from this cvar */

/**
 * Creates a new cvar for the engine.
 *
 * @note This has the same effect as register_cvar but provides more options.
 * @note For a list of possible cvar flags see FCVAR_* constants above.
 * @note If an already existing cvar is registered it will not be duplicated.
 * @note The returned cvar pointer should be used with the get_pcvar_* and
 *       set_pcvar_* set of functions.
 *
 * @param name        Cvar name
 * @param string      Default cvar value
 * @param flags       Optional bitsum of flags specifying cvar behavior
 * @param description Optional description of the cvar
 * @param has_min     Optional boolean that specifies if the cvar has a minimum value
 * @param min_val     Minimum floating point value
 * @param has_max     Optional boolean that specifies if the cvar has a maximum value
 * @param max_val     Maximum floating point value
 *
 * @return          Unique cvar pointer
 */
native create_cvar(const name[], const string[], flags = FCVAR_NONE, const description[] = "", bool:has_min = false, Float:min_val = 0.0, bool:has_max = false, Float:max_val = 0.0);

/**
 * Registers a new cvar for the engine.
 *
 * @note Deprecated. Consider to use create_cvar for more options.
 * @note For a list of possible cvar flags see FCVAR_* constants in amxconst.inc
 * @note If an already existing cvar is registered it will not be duplicated.
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
native register_cvar(const name[], const string[], flags = FCVAR_NONE, Float:fvalue=0.0);

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
 * @note A pointer is also returned by register_cvar(). Plugins can (and should)
 *       retrieve and use pointers for already existing mod cvars.
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
 * @note Callback will be called in the following manner:
 *
 *       public cvar_change_callback(pcvar, const old_value[], const new_value[])
 *
 *       pcvar         - Pointer to cvar that was changed
 *       old_value     - String containing the value of the cvar before it was changed
 *       new_value     - String containing the new value of the cvar
 *
 *       The return value is ignored
 *
 * @param pcvar         Pointer to cvar
 * @param callback      Name of callback function
 *
 * @return              Callback handle that can be used with [disable|enable]_cvar_hook
 * @error               Invalid cvar pointer or invalid callback function
 */
native cvarhook:hook_cvar_change(pcvar, const callback[]);

/**
 * Stops a cvar hook forward from triggering.
 *
 * @note  Use the handle returned by hook_cvar_change as the parameter here.
 *
 * @param handle        Forward to stop
 * @error               Invalid hook handle
 */
native disable_cvar_hook(cvarhook:handle);

/**
 * Starts a cvar hook forward back up.
 *
 * @note  Use the handle returned by hook_cvar_change as the parameter here.
 *
 * @param handle        Forward to back up
 * @error               Invalid hook handle
 */
native enable_cvar_hook(cvarhook:handle);

/**
 * Returns flags of a cvar. The cvar is accessed by name.
 *
 * @note For a list of possible flags see the FCVAR_* constants in amxconst.inc
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_flags function.
 *
 * @param cvar      Cvar name to retrieve flags from
 *
 * @return          1 on success, 0 if cvar does not exist or is not permitted
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
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_flags function.
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
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_flags function.
 *
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
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_string function.
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
 * @note Accessing a cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_string function.
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
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_float function.
 *
 * @param cvarname  Cvar name to retrieve value from
 *
 * @return          Cvar value, converted to float
 */
native Float:get_cvar_float(const cvarname[]);

/**
 * Sets a cvar to a given float value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_float function.
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
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the get_pcvar_num function.
 *
 * @param cvarname  Cvar name to retrieve value from
 *
 * @return          Cvar value, converted to int
 */
native get_cvar_num(const cvarname[]);

/**
 * Sets a cvar to a given integer value. The cvar is accessed by name.
 *
 * @note Accessing a Cvar by name requires this function to walk through the
 *       engine's cvar list every time, which can result in a considerable waste
 *       of processing time, especially if many cvars have been registered. For
 *       a vastly superior alternative look at the set_pcvar_num function.
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
 * @error           If an invalid cvar pointer is specified, an error is thrown.
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
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native set_pcvar_flags(pcvar, flags);

/**
 * Returns an integer value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to int
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native get_pcvar_num(pcvar);

/**
 * Returns an boolean value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to bool
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native bool:get_pcvar_bool(pcvar);

/**
 * Sets an integer value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native set_pcvar_num(pcvar, num);

/**
 * Sets a boolean value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native set_pcvar_bool(pcvar, bool:num);

/**
 * Returns a float value from a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 *
 * @return          Cvar value, converted to float
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native Float:get_pcvar_float(pcvar);

/**
 * Sets a float value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to set value of
 * @param num       Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is specified, an error is thrown.
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
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native get_pcvar_string(pcvar, string[], maxlen);

/**
 * Sets a string value to a cvar via direct pointer access.
 *
 * @param pcvar     Pointer to cvar to retrieve value from
 * @param string    Value to set cvar to
 *
 * @noreturn
 * @error           If an invalid cvar pointer is specified, an error is thrown.
 */
native set_pcvar_string(pcvar, const string[]);

/**
 * Cvar bound values used with get/set_pcvar_bounds()
 */
enum CvarBounds
{
    CvarBound_Upper = 0,
    CvarBound_Lower
};

/**
 * Retrieves the specified value bounds of a cvar.
 *
 * @param pcvar     Pointer to cvar
 * @param type      Type of bound to retrieve, CvarBound_Lower or CvarBound_Upper
 * @param value     Variable to store the specified bound to
 *
 * @return          True if the cvar has the specified bound set, false otherwise.
 * @error           If an invalid cvar pointer or CvarBounds value, an error is thrown.
 */
native bool:get_pcvar_bounds(pcvar, CvarBounds:type, &Float:value);

/**
 * Sets the specified bounds of a cvar.
 *
 * @param pcvar     Pointer to cvar
 * @param type      Type of bound to set, CvarBound_Lower or CvarBound_Upper
 * @param set       If set to true, cvar will use specified bound. If false, bound will be removed
 * @param value     Floating point value to use as the specified bound
 *
 * @error           If an invalid cvar pointer or CvarBounds value, an error is thrown.
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
 * @error           Invalid cvar pointer, invalid provided variable or cvar/variable already binded.
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
 * @error           Invalid cvar pointer, invalid provided variable or cvar/variable already binded.
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
 * @error           Invalid cvar pointer, invalid provided variable or cvar/variable already binded.
 */
native bind_pcvar_string(pcvar, any:var[], varlen);

/**
 * Returns the number of plugin-registered cvars.
 *
 * @return  Number of registered cvars
 */
native get_plugins_cvarsnum();

/**
 * Retrieves information about a plugin-registered cvar.
 *
 * @note The returned cvar pointer should be used with the get_pcvar_* and
 *       set_pcvar_* set of functions.
 *
 * @param num           Cvar index, this does not equal the cvar pointer, it is
 *                      the internal index, incremented for each registered cvar
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
native get_plugins_cvar(num, name[], namelen, &flags=0, &plugin_id=0, &pcvar_handle=0, description[]="", desc_len=0);

/**
 * Dispatches a client cvar query, allowing the plugin to query for its value on
 * the client.
 *
 * @note The callback will be called in the following manner:
 *
 * public cvar_query_callback(id, const cvar[], const value[], const param[])
 *
 *  id      - Client index
 *  cvar    - Cvar queried
 *  value   - Cvar value on the client
 *  param   - Extra data [optional]
 *
 * @param id            Client index
 * @param cvar          Cvar to query
 * @param resultFunc    Callback function
 * @param paramlen      Size of extra data
 * @param params        Extra data to pass through to callback
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients or the client is not connected, an error
 *                      will be thrown.
 *                      If the callback function is invalid, cvar querying is
 *                      unavailable or the querying process runs out of memory,
 *                      an error will be thrown.
 */
native query_client_cvar(id, const cvar[], const resultFunc[], paramlen=0, const params[]="");
