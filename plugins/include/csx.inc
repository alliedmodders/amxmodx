/* CSX functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _csx_included
  #endinput
#endif
#define _csx_included

#include <csstats>

#if AMXX_VERSION_NUM >= 175
 #pragma reqclass xstats
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma defclasslib xstats csx
 #endif
#else
 #pragma library csx
#endif

/*
 * Forwards
 */

/* Function is called after player to player attacks ,
* if players were damaged by teammate TA is set to 1 */
forward client_damage(attacker,victim,damage,wpnindex,hitplace,TA);

/* Function is called after player death ,
* if player was killed by teammate TK is set to 1 */
forward client_death(killer,victim,wpnindex,hitplace,TK);

forward grenade_throw( index,greindex,wId );

forward bomb_planting(planter);
forward bomb_planted(planter);
forward bomb_explode(planter,defuser);
forward bomb_defusing(defuser);
forward bomb_defused(defuser);

/************* Shared Natives Start ********************************/

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

/* Returns stats array size */
native xmod_get_stats_size();

/************* Shared Natives End ********************************/
