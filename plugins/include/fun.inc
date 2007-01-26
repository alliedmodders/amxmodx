/* Fun functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _fun_included
  #endinput
#endif
#define _fun_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib fun
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib fun
 #endif
#else
 #pragma library fun
#endif

/* Returns 1 if receiver hears sender via voice communication. */
native get_client_listen(receiver, sender);

/* Sets who can listen who. Function returns 0
* if for some reasons this setting can't be done. */
native set_client_listen(receiver, sender, listen);

/* Sets player godmode. If you want to disable godmode set only first parameter. */
native set_user_godmode(index, godmode = 0);

/* Returns 1 if godmode is set. */
native get_user_godmode(index);

/* Sets player armor. */
native set_user_armor(index, armor);

/* Sets player health. */
native set_user_health(index, health);

/* Move player to origin. */
native set_user_origin(index, const origin[3]);

/* Sets player rendering mode. */
native set_user_rendering(index, fx = kRenderFxNone, r = 255, g = 255, b = 255, render = kRenderNormal, amount = 16);

/* Gives item to player, name of item can start
 * with weapon_, ammo_ and item_. This event
 * is announced with proper message to all players. */
native give_item(index, const item[]);

/* Sets hit zones for player.
 * Parts of body are as bits:
 * 1   - generic
 * 2   - head
 * 4   - chest
 * 8   - stomach
 * 16  - left arm
 * 32  - right arm
 * 64  - left leg
 * 128 - right leg */
native set_user_hitzones(index = 0, target = 0, body = 255);

/* Get user hitzones. */
native get_user_hitzones(index, target);

/* Sets users max. speed. */
native set_user_maxspeed(index, Float:speed = -1.0);

/* Returns users max. speed. */
native Float:get_user_maxspeed(index);

/* Sets users gravity. */
native set_user_gravity(index, Float:gravity = 1.0);

/* Returns users gravity. */
native Float:get_user_gravity(index);

/* Spawns entity. */
native spawn(index);

/* Sets player noclip. If you want to disable noclip set only first parameter. */
native set_user_noclip(index, noclip = 0);

/* Returns 1 if noclip is set. */
native get_user_noclip(index);

/* Returns 1 if player has silent footsteps, 0 if footsteps are set to normal */
native get_user_footsteps(index);

/* Gives player silent footsteps.
* if set = 0 it will return footsteps to normal */
native set_user_footsteps(id, set = 1);

/* Strips all weapons from user. */
native strip_user_weapons(index);

/* Sets player frags. */
native set_user_frags(index, frags);
