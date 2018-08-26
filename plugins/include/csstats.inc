// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _csstats_included
	#endinput
#endif
#define _csstats_included

#include <csstats_const>

/**
 * Retrieves the client's current weapon statistics.
 *
 * @note For a list of default CS weapon ids see the CSW_* constants in
 *       amxconst.inc, this function also works on custom weapons.
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *
 * @param index     Client index
 * @param wpnindex  Weapon id, or 0 to retrieve total statistics across all
 *                  weapons
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 *
 * @return          1 on success, 0 if no statistics are available for the weapon
 *                  id
 * @error           If an invalid client index or weapon id is provided, an
 *                  error will be thrown.
 */
native get_user_wstats(index, wpnindex, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS]);

/**
 * Retrieves the client's weapon statistics from the current round.
 *
 * @note For a list of default CS weapon ids see the CSW_* constants in
 *       amxconst.inc, this function also works on custom weapons.
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *
 * @param index     Client index
 * @param wpnindex  Weapon id, or 0 to retrieve total statistics across all
 *                  weapons
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 *
 * @return          1 on success, 0 if no statistics are available for the
 *                  weapon id
 * @error           If an invalid client index or weapon id is provided, an
 *                  error will be thrown.
 */
native get_user_wrstats(index, wpnindex, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS]);

/**
 * Retrieves the client's weapon statistics from the permanent storage on the
 * server.
 *
 * @note The permanent storage is updated on every respawn or client disconnect.
 * @note Player rank is determined by the customizable "get_score" function in
 *       "data/csstats.amxx". By default it uses the difference of kills to
 *       deaths/teamkills.
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *          7 - Rank
 *
 * @param index     Client index
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 *
 * @return          Players rank > 0 on success, or 0 if player is not ranked
 *                  and no statistics are available
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native get_user_stats(index, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS]);

/**
 * Retrieves the client's statistics from the current round.
 *
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *
 * @param index     Client index
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 *
 * @return          1 on success, 0 if no statistics are available
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native get_user_rstats(index, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS]);

/**
 * Retrieves the client's statistics inflicted upon another client from the
 * current round.
 *
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *
 * @param index     Client index
 * @param victim    Victim client index, or 0 to retrieve the statistics against
 *                  all victims
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 * @param wpnname   Optional buffer to copy last used weapon name to
 * @param len       Maximum buffer size
 *
 * @return          1 on success, 0 if no statistics are available against the
 *                  specified victim
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native get_user_vstats(index, victim, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS], wpnname[] = "", len = 0);

/**
 * Retrieves the client's statistics received from another client from the
 * current round.
 *
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *
 * @param index     Client index
 * @param wpnindex  Attacker client index, or 0 to retrieve the statistics from
 *                  all attackers
 * @param stats     Buffer to copy statistics to
 * @param bodyhits  Buffer to copy body hits to
 * @param wpnname   Optional buffer to copy last used weapon name to
 * @param len       Maximum buffer size
 *
 * @return          1 on success, 0 if no statistics are available against the
 *                  specified attacker
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native get_user_astats(index, wpnindex, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS], wpnname[] = "", len = 0);

/**
 * Resets the current round weapon, attacker and victim statistics.
 *
 * @param index     Client index
 *
 * @noreturn
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native reset_user_wstats(index);

/**
 * Retrieves statistics from the permanent storage on the server via iterative,
 * incremental access.
 *
 * @note The permanent storage is updated on every respawn or client disconnect.
 * @note Player rank is determined by the customizable "get_score" function in
 *       "data/csstats.amxx". By default it uses the difference of kills to
 *       deaths/teamkills.
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - Kills
 *          1 - Deaths
 *          2 - Headshots
 *          3 - Teamkills
 *          4 - Shots
 *          5 - Hits
 *          6 - Damage
 *          7 - Rank
 *
 * @param index         Rank index
 * @param stats         Buffer to copy statistics to
 * @param bodyhits      Buffer to copy body hits to
 * @param name          Buffer to copy client name to
 * @param len           Maximum name buffer size
 * @param authid        Buffer to copy client auth id to
 * @param authidlen     Maximum authid buffer size
 *
 * @return              Next rank index (> 0 and > index), or 0 if no more
 *                      statistics exist
 */
native get_stats(index, stats[STATSX_MAX_STATS], bodyhits[MAX_BODYHITS], name[], len, authid[] = "", authidlen = 0);

/**
 * Returns the number of all entries in the permanent statistics storage.
 *
 * @return  Number of entries in statistics storage
 */
native get_statsnum();

/**
 * Retrieves the client's objective statistics from the permanent storage.
 *
 * @note The permanent storage is updated on every respawn or client disconnect.
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - total defusions
 *          1 - bomb defused
 *          2 - bomb plants
 *          3 - bomb explosions
 *
 * @param index     Client index
 * @param stats     Buffer to copy statistics to
 *
 * @return          Players rank > 0 on success, or 0 if player is not ranked
 *                  and no statistics are available
 * @error           If an invalid client index is provided, an error will be
 *                  thrown.
 */
native get_user_stats2(index, stats[STATSX_MAX_OBJECTIVE]);

/**
 * Retrieves objective statistics from the permanent storage on the server via
 * iterative, incremental access.
 *
 * @note The permanent storage is updated on every respawn or client disconnect.
 * @note For a list of possible stat constants see the STATSX_* constants in
 *       amxconst.inc
 * @note The fields in the statistics are:
 *          0 - total defusions
 *          1 - bomb defused
 *          2 - bomb plants
 *          3 - bomb explosions
 *
 * @param index         Client index
 * @param stats         Buffer to copy statistics to
 * @param authid        Buffer to copy client auth id to
 * @param authidlen     Maximum authid buffer size
 *
 * @return              Next rank index (> 0 and > index), or 0 if no more
 *                      statistics exist
 */
native get_stats2(index, stats[STATSX_MAX_OBJECTIVE], authid[] = "", authidlen = 0);
