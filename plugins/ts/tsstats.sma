/* Get Score for TS STATS.
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 *
 * Function calculates position in rank.
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
 * File location: $moddir/addons/amxmodx/data/tsstats.amxx
 */

#include <amxmodx>

public get_score(stats[8],body[8])
{
	return stats[0] - stats[1] - stats[3] /* kills - deaths - TKs */
}