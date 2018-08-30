// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Functions
//

#if defined _csstats_const_included
	#endinput
#endif
#define _csstats_const_included

/**
 * Constants for objective based statistics
 */
enum
{
	STATSX_TOTAL_DEFUSIONS = 0,
	STATSX_BOMBS_DEFUSED,
	STATSX_BOMBS_PLANTED,
	STATSX_BOMB_EXPLOSIONS,
	STATSX_MAX_OBJECTIVE
}
