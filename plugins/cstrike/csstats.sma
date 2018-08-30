// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Rank Calculation
//

/* File location: $moddir/addons/amxmodx/data/csstats.amxx */

#include <amxmodx>

/* Function calculates position in rank.
*
* Stats:
* 0 - kills
* 1 - deaths
* 2 - headshots
* 3 - teamkills
* 4 - shots
* 5 - hits
* 6 - damage
* 7 - defusions
* 8 - defused
* 9 - plants
* 10 - explosions
*
* Returning cellmin as value in get_score function
* makes that rank won't be saved. */

public get_score(stats[11], body[MAX_BODYHITS])
	return stats[STATSX_KILLS] - stats[STATSX_DEATHS] - stats[STATSX_TEAMKILLS]	// kills - deaths - teamkills
