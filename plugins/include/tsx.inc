/* TSXMod functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _tsx_included
  #endinput
#endif
#define _tsx_included

#include <tsconst>
#include <tsstats>

#pragma library tsx

/************* Shared Natives Start ********************************/

/* Forward types */
enum {
  XMF_DAMAGE = 0,
  XMF_DEATH,
}

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
native custom_weapon_add( wpnname[],melee = 0,logname[]="" ); 
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

/* weapon logname to weapon name convertion */
native ts_wpnlogtoname(logname[],name[],len);

/* weapon logname to weapon index convertion */
native ts_wpnlogtoid(logname[]);

native ts_getuserwpn( index,&ammo,&clip,&mode,&extra );
native ts_getusercash( index );
native ts_getuserspace( index );
native ts_getuserpwup( index,&Value );
native ts_getuseritems( index );
native ts_getuserkillflags(killer);
native ts_getkillingstreak( index );
native ts_getuserlastfrag( index );

native ts_giveweapon( index,weapon,clips,extra );

/* Function will create pwup entity and return its index (pwupent) */
native ts_createpwup( pwup );

native ts_givepwup( index,pwupent );

native ts_setpddata( knifeoffset );