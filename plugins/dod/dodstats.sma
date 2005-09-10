/* Get Score for DoD STATS.
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
 * 7 - score
 *
 * File location: $moddir/addons/amxmodx/data/dodstats.amxx
 */

#include <amxmodx>

#define KILLS	stats[0]
#define DEATHS	stats[1]
#define TK		stats[3]
#define SCORE	stats[7]

public get_score(stats[9],body[8]){
	if (!DEATHS)
		DEATHS = 1

	return (KILLS-DEATHS-TK)*KILLS/DEATHS 
}