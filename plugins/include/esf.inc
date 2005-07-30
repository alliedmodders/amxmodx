/**
 * (C)2004-2005 AMX Mod X Development Team
 *   based on the stocks and information provided by LynX
 *   organized and released by BAILOPAN
 * This file is provided as is (no warranties).
 */
 
//Original author notice:
/* [ ESF stocks ] - made by LynX
   Credits:
   Jinto - for his first ESF stocks which he gave to me, and from which I learned
           how to create stocks. Also, he learned me how to modify ESF
           Without him, this wouldn't be possible.
*/

#if defined _esf_included
  #endinput
#endif
#define _esf_included

#include <esf_const>

#pragma library esfmod

/**************************
 * Main functions *********
 **************************/

/* Returns a player's character class. */
native esf_get_class(player);

/* Sets a player's class (experimental. */
native esf_set_class(player);

/* Returns a player's ki.*/
native Float:esf_get_ki(player);

/* Sets a player's ki (min=0, max=1000). */
native esf_set_ki(player, Float:ki);

/* Gets a player's swooping speed.  SWOOP, GRASP! */
native Float:esf_get_swoopspeed(player);

/* Sets a player's swooping speed. */
native esf_set_swoopspeed(player, Float:speed);

/* Sets a player's health. */
native esf_set_health(player, health);

/* Creates a chargebar (0-100). */
native esf_set_chargebar(player, charge);

/* Returns power level. */
native esf_get_powerlevel(player);

/* Sets power level. */
native esf_set_powerlevel(player, power);

/* Returns speed. */
native esf_get_speed(player);

/* Sets speed. */
native esf_set_speed(player, speed);

/**************************
 * Avatar functions *******
 **************************/

native esf_set_avatar_model(id, const szModel[]);

native esf_has_avatar(id);

native esf_remove_avatar(id);

native esf_create_avatar(id);

/**************************
 * Graphics functions *****
 **************************/
 
/* Starts power-up sequence. */
native esf_start_powerup(player, iRed, iGreen, iBlue);

/* Stops power-up. */
native esf_stop_powerup(player);

/* Creates a generic ESF explosion. */
native esf_explosion(origin[3], radius, type = Explosion_White);

/* use character's TransformFX 1 message */
native esf_transformfx_1(id);

/* use characters TransformFX 2 message */
native esf_transformfx_2(id);

/* stop character's transform */
native esf_transformfx_off(id);


/**************************
 * Animation based stuff **
 **************************/

/* Checks if a player is swooping in a certain direction.
   0 = any direction
 */
native esf_is_swooping(player, dir = 0);

/* Checks if player is in prepunch, or if player is being prepunched. */
native esf_is_prepunch(player);

/* Checks whether player is in stance to do a combo/punches. */
native esf_in_stance(player);

/* Checks if a player is stunned. */
native esf_is_stunned(player);

/* Checks if a player is grappling/in an adv. melee struggle. */
native esf_is_grappling(player);

/* Checks if a player is crushed against a wall. */
native esf_is_crushed(player);

/* Checks if a player is in a combo (1 or 2).
   If the attacked is 1, checks if being attacked by this combo.
 */
native esf_in_combo(player, combo, attacked = 0);

/* Checks if a player is advanced meleeing. */
native esf_is_advmeleeing(player);

/* Checks if a player is being advanced meleed. */
native esf_is_advmeleed(player);

/* Checks if a player is in advanced melee. */
native esf_in_advmelee(player);

/* Checks if a player is blocking. */
native esf_is_blocking(player);

/* Checks if a player has kicked someone with basic melee. */
native esf_is_kicking(player);

/* Checks if a player has been kicked in basic melee. */
native esf_is_kicked(player);

/* Checks if a player is recovering from an attack. */
native esf_is_recovered(player, recovery);

/* Checks if a player is flying. */
native esf_is_flying(player);

/* Checks if a player is tumbling. */
native esf_is_tumbling(player);

/* Checks if a player is lying on the ground. */
native esf_is_lying(player);

/* Checks if a player has just died. */
native esf_has_died(player);

/* Checks if a player has a dragonball. */
native esf_has_dragonball(player);

/* Checks if a player just threw a dragonball. */
native esf_threw_dragonball(player);

/* Checks if a player is shooting. */
//1 = kiblast, 2 = generic beam
native esf_is_shooting(player, mode);

/* Checks if a player is transforming. */
native esf_transforming(player);

/* Checks if a player's transformation is done. */
native esf_transform_done(player);

/* Checks if a player has been thrown to the ground. */
native esf_groundthrown(player);

/* Gets the state of a player's attack.  
   You must specify the attack, and it will return 0, 
     Charging, Controlling, Shooting, or Shot (see #defines)
 */
native esf_get_shotstate(player, attack);

/* Checks if a player has been swung. */
native esf_is_swung(player);

/* Checks if a player has grabbed a wall. */
native esf_grabbedwall(player);

/* Checks if a player has jumped off a wall. */
native esf_walljumped(player);

/* Checks if a player is in water. */
native esf_in_water(player);

/* Checks if a player is powering up. */
native esf_is_powering(player);

/* Checks if a player is idling. */
native esf_is_idle(player);

/* Checks if a player is beam jumping. */
native esf_is_beamjumping(player);

