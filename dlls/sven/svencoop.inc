/* Sven Co-op functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _svencoop_included
	#endinput
#endif
#define _svencoop_included

#include <svencoop_const>

/* Gets a monster's or player's frags */
native Float:sc_get_frags(index);

/* Sets a monster's or player's frags */
native sc_set_frags(index, Float:value);

/* Gets a monster's displayname

   Returns 1 if custom displayname, 0 if displayname unavailable, -1 if default displayname
*/
native sc_get_displayname(index, displayname[], len);

/* Sets a monster's displayname */
native sc_set_displayname(index, displayname[], {Float,Sql,Result,_}:...);

/* Checks if a monster is a player ally or not */
native sc_is_player_ally(index);

/* Gets if a player has longjump */
native sc_get_user_longjump(index);

/* Sets if a player has longjump */
native sc_set_user_longjump(index, value);

/* Gets a player's deaths */
native sc_get_user_deaths(index);

/* Sets a player's deaths */
native sc_set_user_deaths(index, value);

/* Gets the amount of ammo in dropped ammo weaponbox */
native sc_get_wbox_ammo(index);

/* Sets the amount of ammo in dropped ammo weaponbox */
native sc_set_wbox_ammo(index, value);

/* Gets a weapon's type (in the form of SCW_* constants)

   "index" = weapon_* entity
*/
native sc_get_weapon_id(index);

/* Gets the amount of ammo in weapon's clip

   Usage 1:
		new weapon = find_ent_by_owner(-1,"weapon_9mmAR",id);
		sc_get_weapon_ammo(weapon);
   Usage 2:
		sc_get_weapon_ammo(id,SCW_9MMAR);
*/
native sc_get_weapon_ammo(index1, index2=0);

/* Sets the amount of ammo in weapon's clip

   Usage 1:
		new weapon = find_ent_by_owner(-1,"weapon_9mmAR",id);
		sc_set_weapon_ammo(weapon,50);
   Usage 2:
		sc_set_weapon_ammo(id,50,SCW_9MMAR);
*/
native sc_set_weapon_ammo(index1, newammo, index2=0);

/* Gets the amount of ammo in players's backpack for a specific weapon

   "weapon" = SCW_* constant
*/
native sc_get_user_bpammo(index, weapon);

/* Sets the amount of ammo in players's backpack for a specific weapon

   "weapon" = SCW_* constant
*/
native sc_set_user_bpammo(index, weapon, value);

/* Called whenever a player respawns */
forward sc_client_spawn(index);

/* Called whenever a player or monster throws a grenade

   "isplayer" is 1 if the thrower is a player, 0 if a monster
*/
forward sc_grenade_throw(index, greindex, wId, isplayer);

/* Called whenever a player ATTEMPTS to heal another player or monster
   (so, they could be healing for 0.0 health, or trying to heal an enemy)

   "isplayer" is 1 if the healed entity is a player, 0 if a monster
   "isally" is 1 if the healed entity is an ally, 0 if an enemy
*/
forward sc_client_heal(healer, healed, Float:amount, isplayer, isally);
