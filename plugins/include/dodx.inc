/* DoDX functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _dodx_included
  #endinput
#endif
#define _dodx_included

#include <dodconst>
#include <dodstats>

#if AMXX_VERSION_NUM >= 175
 #pragma reqclass xstats
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma defclasslib xstats dodx
 #endif
#else
 #pragma library dodx
#endif

/************* Shared Natives Start ********************************/

/* Forward types */
enum {
  XMF_DAMAGE = 0,
  XMF_DEATH,
  XMF_SCORE,
}

/* Use this function to register forwards */
native register_statsfwd(ftype);

/* Function is called after player to player attacks ,
* if players were damaged by teammate TA is set to 1 */
forward client_damage(attacker, victim, damage, wpnindex, hitplace, TA);

/* Function is called after player death ,
* if player was killed by teammate TK is set to 1 */
forward client_death(killer, victim, wpnindex, hitplace, TK);

/* Function is called if player scored  */
forward client_score(id, score, total);

/* This Forward is called when a player changes team */
forward dod_client_changeteam(id, team, oldteam);

/* This Forward is called if a player changes class, but just after spawn */
forward dod_client_changeclass(id, class, oldclass);

/* This Forward is called when a player spawns */
forward dod_client_spawn(id);

/* Sets the model for a player */
native dod_set_model(id, model[]);

/* Sets the model for a player */
native dod_set_body_number(id, bodynumber);

/* Un-Sets the model for a player */
native dod_clear_model(id);

/* Custom Weapon Support */
/* function will return index of new weapon */
native custom_weapon_add( wpnname[], melee = 0, logname[]="" ); 

/* Function will pass damage done by this custom weapon to stats module and other plugins */
native custom_weapon_dmg( weapon, att, vic, damage, hitplace=0 ); 

/* Function will pass info about custom weapon shot to stats module */
native custom_weapon_shot( weapon, index ); // weapon id , player id

/* function will return 1 if true */
native xmod_is_melee_wpn(wpnindex);

/* Returns weapon name. */
native xmod_get_wpnname(wpnindex, name[], len);

/* Returns weapon logname. */
native xmod_get_wpnlogname(wpnindex, name[], len);

/* Returns weapons array size */
native xmod_get_maxweapons();

/* Returns stats array size ex. 8 in TS , 9 in DoD */
native xmod_get_stats_size();

/* Returns 1 if true */
native xmod_is_custom_wpn(wpnindex);

/************* Shared Natives End ********************************/

/* weapon logname to weapon name convertion */
native dod_wpnlog_to_name(logname[],name[],len);

/* weapon logname to weapon index convertion */
native dod_wpnlog_to_id(logname[]);

native dod_get_map_info( info );

/* Returns id of currently carried weapon. Gets also 
* ammount of ammo in clip and backpack. */
native dod_get_user_weapon(index,&clip,&ammo);

/* Returns team score */
native dod_get_team_score(teamId);

/* Returns player class id */
native dod_get_user_class(index);

/* Returns player score */
native dod_get_user_score(index);

/* values are: 0-no prone, 1-prone, 2-prone + w_deploy */
native dod_get_pronestate(index);

/* It is not as safe as original but player deaths will not be increased */
native dod_user_kill(index);
