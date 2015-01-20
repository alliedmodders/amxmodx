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
 * Called when a console variable's value is changed.
 * 
 * @param cvarHandle    Handle to the cvar that was changed
 * @param oldValue      String containing the value of the cvar before it was changed
 * @param newValue      String containing the new value of the cvar
 * @param cvarName      String containing the name of the cvar
 *
 * public OnCvarChanged(cvarHandle, const oldValue[], const newValue[], const cvarName[]);
 */
