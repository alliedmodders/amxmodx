// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DODX Functions
//

#if defined _dodx_included
	#endinput
#endif
#define _dodx_included

#include <dodconst>
#include <dodstats>

#pragma reqclass xstats
#if !defined AMXMODX_NOAUTOLOAD
	#pragma defclasslib xstats dodx
#endif

/************* Shared Natives Start ********************************/

/* Forward types */
enum {
  XMF_DAMAGE = 0,
  XMF_DEATH,
  XMF_SCORE,
};

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

/* This will be called whenever a player scopes or unscopes
value = 1 scope up
value = 0 scope down */
forward dod_client_scope(id, value);

/* This will be called whenever a player drops a weapon 
weapon is weapon dropped or picked up 
value = 1 picked up
value = 0 dropped */
forward dod_client_weaponpickup(id, weapon, value);

/* Called whenever the the player goes to or comes from prone position 
value = 1 going down
value = 0 getting up */
forward dod_client_prone(id, value);

/* This will be called whenever a player switches a weapon */
forward dod_client_weaponswitch(id, wpnew, wpnold);

/* Forward for when a grenade explodes and its location */
forward dod_grenade_explosion(id, Float:pos[3], wpnid);

/* Forward for when a rocket explodes and its location */
forward dod_rocket_explosion(id, Float:pos[3], wpnid);

/* Forward for when a player picks up a object */
forward dod_client_objectpickup(id, objid, Float:pos[3], value);

/* Forward for when a users stamina decreases */
forward dod_client_stamina(id, stamina);

/* We want to get just the weapon of whichever type that the player is on him 
Use DODWT_* in dodconst.inc for type */
native dod_weapon_type(id, type);

/* This native will change the position of a weapon within the users slots and its ammo ammount */
native dod_set_weaponlist(id, wpnID, slot, dropslot, totalrds);

/* Sets the model for a player */
native dod_set_model(id, const model[]);

/* Sets the model for a player */
native dod_set_body_number(id, bodynumber);

/* Un-Sets the model for a player */
native dod_clear_model(id);

/* Custom Weapon Support */
/* function will return index of new weapon */
native custom_weapon_add( const wpnname[], melee = 0, const logname[]="" ); 

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
native dod_wpnlog_to_name(const logname[],name[],len);

/* weapon logname to weapon index convertion */
native dod_wpnlog_to_id(const logname[]);

native dod_get_map_info( info );

/* Returns id of currently carried weapon. Gets also 
* ammount of ammo in clip and backpack. */
native dod_get_user_weapon(index,&clip=0,&ammo=0);

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
