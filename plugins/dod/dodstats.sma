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
// DoD Stats Rank Calculation
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
 * 7 - score
 *
 * File location: $moddir/addons/amxmodx/data/dodstats.amxx
 */

#include <amxmodx>
#include <dodx>

#define KILLS	stats[DODX_KILLS]
#define DEATHS	stats[DODX_DEATHS]
#define TK		stats[DODX_TEAMKILLS]
#define SCORE	stats[DODX_POINTS]

public get_score(stats[DODX_MAX_STATS],body[MAX_BODYHITS]){
	if (!DEATHS)
		DEATHS = 1

	return (KILLS-DEATHS-TK)*KILLS/DEATHS 
}
