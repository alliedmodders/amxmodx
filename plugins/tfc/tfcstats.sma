// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 SidLuke.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TFC Stats Rank Calculation
//

/* Function calculates position in rank.
 *
 * Stats:
 * 0 - kills
 * 1 - deaths
 * 2 - headshots
 * 3 - teamkilling
 * 4 - shots
 * 5 - hits
 * 6 - damage
 *
 * File location: $moddir/addons/amxmodx/data/tfcstats.amxx
 */

#include <amxmodx>

public get_score(stats[STATSX_MAX_STATS],body[MAX_BODYHITS])
{
	return stats[STATSX_KILLS] - stats[STATSX_DEATHS] /* kills - deaths */
}
