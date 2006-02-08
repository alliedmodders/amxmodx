/* Sven Coop Natives
*
* This file is provided as is (no warranties).
*/

#if defined _svencoop_included
  #endinput
#endif
#define _svencoop_included

#include <svencoop_const>
#include <svencoop_stocks>

#pragma library svenfun

// Gets/sets the amount of frags a monster or player has.
native Float:sc_get_frags(id);
native sc_set_frags(id,Float:frags);

// Gets/sets the deaths a player has.
native sc_get_user_deaths(id);
native sc_set_user_deaths(id,deaths);

// returns 1 if entity (non-player) is an ally.
native sc_is_player_ally(id);

// Gets the amount of ammo an ent has.
// Use the SVEN_WEP_* defines for type
native sc_get_weapon_ammo(id, type)

// Sets the amount of ammo an ent has.
// Use the SVEN_WEP_* defines for type
native sc_set_weapon_ammo(id, amount, type)	