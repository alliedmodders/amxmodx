// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TFCX Functions
//

#if defined _tfcx_included
	#endinput
#endif
#define _tfcx_included

#include <tfcconst>
#include <tfcstats>

#pragma reqclass xstats
#if !defined AMXMODX_NOAUTOLOAD
	#pragma defclasslib xstats tfcx
#endif

/************* Shared Natives Start ********************************/

/* Forward types */
enum {
  XMF_DAMAGE = 0,
  XMF_DEATH,
};

/* Use this function to register forwards */
native register_statsfwd( ftype );

/* Function is called after player to player attacks ,
* if players were damaged by teammate TA is set to 1 */
forward client_damage(attacker,victim,damage,wpnindex,hitplace,TA);

/* Function is called after player death ,
* if player was killed by teammate TK is set to 1 */
forward client_death(killer,victim,wpnindex,hitplace,TK);

/* Custom Weapon Support */
/* function will return index of new weapon */
native custom_weapon_add( const wpnname[],melee = 0,const logname[]="" ); 
/* Function will pass damage done by this custom weapon to stats module and other plugins */
native custom_weapon_dmg( weapon, att, vic, damage, hitplace=0 ); 
/* Function will pass info about custom weapon shot to stats module */
native custom_weapon_shot( weapon,index ); // weapon id , player id

/* function will return 1 if true */
native xmod_is_melee_wpn(wpnindex);

/* Returns weapon name. */
native xmod_get_wpnname(wpnindex,name[],len);

/* Returns weapon logname. */
native xmod_get_wpnlogname(wpnindex,name[],len);

/* Returns weapons array size */
native xmod_get_maxweapons();

/* Returns stats array size ex. 8 in TS , 9 in DoD */
native xmod_get_stats_size();

/* Returns 1 if true */
native xmod_is_custom_wpn(wpnindex);

/************* Shared Natives End ********************************/

stock tfc_isgrenade( weapon ){
  switch( weapon )
  {
    case TFC_WPN_CALTROP,
         TFC_WPN_CONCUSSIONGRENADE,
         TFC_WPN_NORMALGRENADE,
         TFC_WPN_NAILGRENADE,
         TFC_WPN_MIRVGRENADE,
         TFC_WPN_NAPALMGRENADE,
         TFC_WPN_GASGRENADE,
         TFC_WPN_EMPGRENADE:
      return 1;
    default: return 0;
  }
  return 0;
}

native tfc_userkill( index );

/* Use this function to set private data offsets if needed 
Default offsets:
	timer: 932
	sentrygun: 83
from AssKicR
	shells: 53
	bullets: 55
	cells: 57
	rockets: 59
	nade1: 14
	nade2: 15
*/
native tfc_setpddata(timer,sentrygun,shells,bullets,cells,rockets,nade1,nade2);

/*********************************************************************/

native 	tfc_setmodel(index, const Model[], const Skin[]);
native 	tfc_clearmodel(index);

/* Get amount of ammo in backpack on a user for a specific weapon */
/* Ammo Types in tfcconst.inc */
native 	tfc_getbammo(index, ammo);

/* Set amount of ammo in backpack on a user for a specific weapon */
native 	tfc_setbammo(index, ammo, value);

/* Returns amount of ammo in weapon's clip (backpack) */
/* Weapons list in tfcconst.inc */
native tfc_getweaponbammo(index, weapon);

/* Sets amount of ammo in weapon's clip (backpack) */
native tfc_setweaponbammo(index, weapon, value);

/* Returns amount of ammo in weapon's clip */
/* Index must be weapon's entity index */
native tfc_getweaponammo(index);

/* Sets amount of ammo in weapon's clip */
/* Index must be weapon's entity index */
native tfc_setweaponammo(index, value);

/* Returns 1 if user is carrying a goal item such as a flag or a keycard, else 0.
 * Team is by reference parameter that will be set to owning team(s) of the goal item.
 * Use the TFC_GOALITEM_* constants to determine the owning team.
 */
native tfc_get_user_goalitem(index, &team);

/* Returns 1 if the player is a spy and is currently feigning death */
native tfc_is_user_feigning(index);

/* Returns 1 if the two teams are allies, 0 otherwise 
 * Note: Team must be 1->4
 *       Team 0 will always return 0
 *       Any other team will result in an error
 */
native tfc_is_team_ally(TeamA,TeamB);
