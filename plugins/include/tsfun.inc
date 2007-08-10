/* TSFUN
 *
 * (c) 2005-2006, AMX Mod X Dev Team
 * This file is provided as is (no warranties).
 */

#if defined _tsxfun_included
  #endinput
#endif
#define _tsxfun_included

#include <tsx>
#include <tsconst>

/************* Shared Natives Start ********************************/

/* Forward types */
enum {
  XMF_DAMAGE = 0,
  XMF_DEATH,
};

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib tsfun
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib tsfun
 #endif
#else
 #pragma library tsfun
#endif

/************* Shared Natives End ********************************/

/* Function is called just before a kung foo attack is done,
 * damage and time length may be altered with natives.
 * Return PLUGIN_HANDLED to stop attack.
 * UNAVAILABLE IN 1.70
 */
forward Melee_Attack(id,Float:time,Float:damage,UNAVAILABLE);


// Returns when someone stunts, after they do it.
//UNAVAILABLE IN 1.70
forward client_stunt(id,stunttype,UNAVAILABLE);

/* Function is called when powerups are ran,
 * Returns value of powerup. Use TSPWUP_*'s
 * to find exactly which one it is. 
 * UNAVAILABLE IN 1.70
 */
forward client_powerup(id,powerup,UNAVAILABLE);

/* weapon logname to weapon name convertion */
native ts_wpnlogtoname(const logname[],name[],len);

/* weapon logname to weapon index convertion */
native ts_wpnlogtoid(const logname[]);

//UNAVAILABLE IN 1.70
//native Float:ts_getusertime( index );		//!
//native ts_setusertime( index, Float:time );	//!

native ts_getusercash( index );
native ts_setusercash( index, money );

native ts_getuserslots( index );
native ts_setuserslots( index, slots );

native ts_getuserstate( index );
native ts_getuserwpn( index,&clip=0,&ammo=0,&mode=0,&extra=0 );
native ts_getuserspace( index );

native ts_getuserkillflags(killer);
native ts_getkillingstreak( index );
native ts_getuserlastfrag( index );

native ts_giveweapon( index,weapon,clips,extra );

native ts_getuserpwup( index );

stock ts_has_slowmo(id) return (ts_getuserpwup(id) &TSPWUP_SLOWMO);
stock ts_has_infammo(id) return (ts_getuserpwup(id) &TSPWUP_INFAMMO);
stock ts_has_slowpause(id) return (ts_getuserpwup(id) &TSPWUP_SLOWPAUSE);
stock ts_has_dfirerate(id) return (ts_getuserpwup(id) &TSPWUP_DFIRERATE);
stock ts_has_grenade(id) return (ts_getuserpwup(id) &TSPWUP_GRENADE);
stock ts_has_health(id) return (ts_getuserpwup(id) &TSPWUP_HEALTH);
stock ts_has_armor(id) return (ts_getuserpwup(id) &TSPWUP_ARMOR);

/* Function will create pwup entity and return its index (pwupent) */
native ts_createpwup( pwup );

native ts_givepwup( index,pwupent );

native ts_setpddata( knifeoffset );

// Alters a fu attack. Use with fu forward
// UNAVAILABLE IN 1.70
// native ts_set_fuattack(id,Float:time,Float:damage);	//!

// Changes board message
native ts_set_message(id,message);

// Gets the message board message
native ts_get_message(id);

stock ts_is_normal(id)
{
	new msg = ts_get_message(id);
	if( (msg > 11) || (msg > 6 && msg < 10) ) return 1;
	return 0;
}
stock ts_is_waiting(id) return (ts_get_message(id) == TSMSG_WAITING);
stock ts_is_dead(id) return (ts_get_message(id) == TSMSG_DEAD);
stock ts_is_killer(id) return (ts_get_message(id) == TSMSG_KILLER);
stock ts_is_demolition(id) return (ts_get_message(id) == TSMSG_DEMOLITION);
stock ts_is_specialist(id) return (ts_get_message(id) == TSMSG_SPECIALIST);
stock ts_is_unstoppable(id) return (ts_get_message(id) == TSMSG_UNSTOPPABLE);
stock ts_is_theone(id) return (ts_get_message(id) == TSMSG_THEONE);

// Return one on true, 0 on false
// UNAVAILABLE IN 1.70
native ts_has_superjump(id);					//!
native ts_has_fupowerup(id);					//!
native ts_is_in_slowmo(id);

// Get and set consecutive frags
//UNAVAILABLE IN 1.70
//native ts_get_cons_frags(id);
//native ts_set_cons_frags(id,num);

// Set to see cool bullet trails. Only id will see them.
native ts_set_bullettrail(id,yesorno);

// Sets fake versions of slow mo and slow pause. Use ts_set_speed for more options.
native ts_set_fakeslowmo(id,Float:time);
native ts_set_fakeslowpause(id,Float:time);

/* Sets speed artificially. 1.0 is default, Go into fractions and decimals for slower
* and put in higher numbers for higher speeds. Aura is how far things around you are effected
* Time is the time until it wears off. 0.0 for speed will freeze people. Do not use negatives. */

native ts_set_speed(id,Float:speed,Float:auradist,Float:time);

/* Sets physics speed artificially. Things like sparks and sounds will be effected.
* Any negative number will render all physics paused. */
native ts_set_physics_speed(id,Float:speed);

// Returns 0 if no powerup is running. Returns the powerup type otherwise.
native ts_is_running_powerup(id);

// Highly experimental command which overrides powerup types.
// Use if a powerup is already running, or if a powerup is not running.
// Safe to use in powerup forward.
native ts_force_run_powerup(id,PWUP_TYPE);
