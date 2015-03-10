// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// CSX Functions
//

#if defined _csx_included
	#endinput
#endif
#define _csx_included

#include <csstats>

#pragma reqclass xstats
#if !defined AMXMODX_NOAUTOLOAD
	#pragma defclasslib xstats csx
#endif

/**
 * Map objective flags returned by get_map_objectives().
 */
enum MapObjective
{
	MapObjective_Bomb    = (1<<0),
	MapObjective_Hostage = (1<<1),
	MapObjective_Vip     = (1<<2),
	MapObjective_Escape  = (1<<3),
};

/**
 * Called after a client attacks another client.
 *
 * @note For a list of possible weapon ids see the CSW_* constants in
 *       amxconst.inc
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 *
 * @param attacker  Attacker client index
 * @param victim    Victim client index
 * @param damage    Damage dealt to victim
 * @param wpnindex  Weapon id
 * @param hitplace  Body hitplace
 * @param ta        If nonzero the attack was a team attack
 *
 * @noreturn
 */
forward client_damage(attacker, victim, damage, wpnindex, hitplace, TA);

/**
 * Called after a client death.
 *
 * @note For a list of possible weapon ids see the CSW_* constants in
 *       amxconst.inc
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 *
 * @param attacker  Attacker client index
 * @param victim    Victim client index
 * @param wpnindex  Weapon id
 * @param hitplace  Body hitplace
 * @param tk        If nonzero the death was a teamkill
 *
 * @noreturn
 */
forward client_death(killer, victim, wpnindex, hitplace, TK);

/**
 * Called after a grenade was thrown.
 *
 * @note Weapon id is one of CSW_HEGRENADE, CSW_SMOKEGRENADE or CSW_FLASHBANG.
 *
 * @param index     Client index
 * @param greindex  Grenade entity index
 * @param wId       Weapon id
 *
 * @noreturn
 */
forward grenade_throw(index, greindex, wId);

/**
 * Called after a bomb plant attempt has started.
 *
 * @param planter   Planter client index
 *
 * @noreturn
 */
forward bomb_planting(planter);

/**
 * Called after a bomb plant has finished.
 *
 * @param planter   Planter client index
 *
 * @noreturn
 */
forward bomb_planted(planter);

/**
 * Called when the bomb exploded.
 *
 * @param planter   Planter client index
 * @param defuser   Defuser client index, if applicable
 *
 * @noreturn
 */
forward bomb_explode(planter, defuser);

/**
 * Called after a bomb defuse attempt has started.
 *
 * @param defuser   Defuser client index
 *
 * @noreturn
 */
forward bomb_defusing(defuser);

/**
 * Called after a bomb defuse has finished.
 *
 * @param defuser   Defuser client index
 *
 * @noreturn
 */
forward bomb_defused(defuser);

/**
 * @section Shared natives
 */

/**
 * Adds a custom weapon to the stats system.
 *
 * @note The weapon name should be the full display name of the gun such as
 *       "Desert Eagle" while the logname should be "weapon_deagle".
 *
 * @param wpnname   Full weapon name
 * @param melee     If nonzero the weapon will be considered a melee weapon
 * @param logname   Weapon short name
 *
 * @return          Cusom weapon id (>0) on success, 0 if no more custom weapons
 *                  can be added
 */
native custom_weapon_add(const wpnname[], melee = 0, const logname[] = "");

/**
 * Triggers a damage event on a custom weapon, adding it to the internal stats.
 *
 * @note This will also call the client_damage() and client_kill() forwards if
 *       applicable.
 * @note For a list of possible body hitplaces see the HIT_* constants in
 *       amxconst.inc
 *
 * @param weapon    Custom weapon id
 * @param att       Attacker client index
 * @param vic       Victim client index
 * @param damage    Damage dealt
 * @param hitplace  Optional body hitplace
 *
 * @noreturn
 * @error           If the weapon id is not a custom weapon, an invalid client
 *                  index, damage value or hitplace is provided, an error will
 *                  be thrown.
 */
native custom_weapon_dmg(weapon, att, vic, damage, hitplace = 0);

/**
 * Adds a shot event on a custom weapon to the internal stats.
 *
 * @param weapon    Custom weapon id
 * @param index     Client index
 *
 * @noreturn
 * @error           If the weapon id is not a custom weapon or an invalid client
 *                  index is provided, an error will be thrown.
 */
native custom_weapon_shot(weapon, index);

/**
 * Returns if the weapon is considered a melee weapon.
 *
 * @note For a list of default CS weapon ids see the CSW_* constants in
 *       amxconst.inc, this function also works on custom weapons.
 * @note For the default CS weapons this obviously returns true only for
 *       CSW_KNIFE.
 *
 * @param wpnindex  Weapon id
 *
 * @return          1 if weapon is a melee weapon, 0
 * @error           If an invalid weapon id is provided an error will be thrown.
 */
native xmod_is_melee_wpn(wpnindex);

/**
 * Retrieves the full weapon name of a weapon id.
 *
 * @note For a list of default CS weapon ids see the CSW_* constants in
 *       amxconst.inc, this function also works on custom weapons.
 * @note For the default CS weapons this obviously returns true only for
 *       CSW_KNIFE.
 *
 * @param wpnindex  Weapon id
 * @param name      Buffer to copy weapon name to
 * @param len       Maximmum buffer size
 *
 * @return          Number of cells written to buffer
 * @error           If an invalid weapon id is provided an error will be thrown.
 */
native xmod_get_wpnname(wpnindex, name[], len);

/**
 * Retrieves the weapon log name of a weapon id.
 *
 * @note For a list of default CS weapon ids see the CSW_* constants in
 *       amxconst.inc, this function also works on custom weapons.
 * @note For the default CS weapons this obviously returns true only for
 *       CSW_KNIFE.
 *
 * @param wpnindex  Weapon id
 * @param name      Buffer to copy weapon log name to
 * @param len       Maximmum buffer size
 *
 * @return          Number of cells written to buffer
 * @error           If an invalid weapon id is provided an error will be thrown.
 */
native xmod_get_wpnlogname(wpnindex, name[], len);

/**
 * Returns the maximum amount of weapons that the stats system supports.
 *
 * @return      Maximum number of weapons supported
 */
native xmod_get_maxweapons();

/**
 * Returns the number of stats tracked by the stats system.
 *
 * @return      Number of stats tracked
 */
native xmod_get_stats_size();

/**
 * @endsection Shared natives
 */

/**
 * Returns the current map's objectives as a bitflag value.
 *
 * @note For a list of possible map objective flags see the MapObjective enum.
 *
 * @return      Bitflag value of map objectives
 */
native MapObjective:get_map_objectives();
