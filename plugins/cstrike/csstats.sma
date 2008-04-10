/* AMX Mod X
*   Rank Calculation
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is part of AMX Mod X.
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

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

public get_score(stats[11], body[8])
	return stats[0] - stats[1] - stats[3]	// kills - deaths - teamkills
