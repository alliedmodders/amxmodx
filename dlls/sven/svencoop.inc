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

//Stocks for 9mm ammo (mp5, uzi(akimbo))
stock sc_get_bpammo_9mm(id) { return sc_get_weapon_ammo(id,SVEN_WEP_9MM; }			
stock sc_set_bpammo_9mm(id, amount) { return sc_set_weapon_ammo(id, amount, SVEN_WEP_9MM; }

//Stocks for RPG ammo (RPG launcher)
stock sc_get_bpammo_rpg(id) { return sc_get_weapon_ammo(id,SVEN_WEP_RPG; }			
stock sc_set_bpammo_rpg(id, amount) { return sc_set_weapon_ammo(id, amount, SVEN_WEP_RPG; }

//Stocks for radioactive ammo (gauss, egon)
stock sc_get_bpammo_radio(id) { return sc_get_weapon_ammo(id,SVEN_WEP_RADIO; }			
stock sc_set_bpammo_radio(id, amount) { return sc_set_weapon_ammo(id, amount, SVEN_WEP_RADIO; }

//Stocks for snark ammo
stock sc_get_bpammo_snark(id) { return sc_get_weapon_ammo(id,SVEN_WEP_SNARK; }			
stock sc_set_bpammo_snark(id, amount) { return sc_set_weapon_ammo(id, amount, SVEN_WEP_SNARK; }

//Stocks for shotgun ammo
stock sc_get_bpammo_shot(id) { return sc_get_weapon_ammo(id,SVEN_WEP_SHOTGUN; }			
stock sc_set_bpammo_shot(id, amount) { return sc_set_weapon_ammo(id, amount, SVEN_WEP_SHOTGUN; }	