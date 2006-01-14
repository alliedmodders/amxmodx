/* TSXMod functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _tsx_included
  #endinput
#endif
#define _tsx_included

#include <tsstats>
#include <tsfun>

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


/************* Shared Natives End ********************************/