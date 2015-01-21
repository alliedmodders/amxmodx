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
 * Creates a hook for when a console variable's value is changed.
 *
 * @note Callback will be called in the following manner:
 *
 *       public cvar_change_callback(pcvar, const old_value[], const new_value[])
 *
 *       pcvar         - Pointer to cvar that was changed
 *       old_value     - String containing the value of the cvar before it was changed
 *       new_value     - String containing the new value of the cvar
 *
 * @param pcvar         Pointer to cvar
 * @param callback      Name of callback function
 * @error               Invalid pointer or invalid callback function
 */
native cvarhook:hook_cvar_change(pcvar, const callback[]);

/**
 * Stops a cvar hook forward from triggering.
 *
 * @note  Use the return value from hook_cvar_change as the parameter here.
 *
 * @param handle        Forward to stop
 * @error               Invalid hook handle
 */
native disable_cvar_hook(cvarhook:handle);

/**
 * Starts a cvar hook forward back up.
 *
 * @note  Use the return value from hook_cvar_change as the parameter here.
 *
 * @param handle        Forward to back up
 * @error               Invalid hook handle
 */
native enable_cvar_hook(cvarhook:handle);


