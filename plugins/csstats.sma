/* Get Score for CS STATS.
 *
 * (c) 2003, OLO
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
 * Returning cellmin as value in get_score function
 * makes that rank won't be saved.
 *
 * File location: $moddir/addons/amx
 */

#include <amxmod>

public get_score( stats[8] , body[8] )
	return stats[0] - stats[1] // kills - deaths