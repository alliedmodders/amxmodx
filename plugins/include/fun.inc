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
native set_user_origin(index, origin[3]);

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
 * 128 - right leg 
 * Set index to a player's index and leave target at 0 to define what bodyparts this player can hit when he is firing.
 * Set index to 0 and target to a player's index to define what bodyparts on player other players can hit when they are firing.
 * Set both index and target to 0 to define globally what bodyparts people can hit and what bodyparts can be hit when firing. */
native set_user_hitzones(index = 0, target = 0, body = 255);

/* Get user hitzones.
 * To get what bodyparts a player can hit when firing, set the player's index to index and target to 0.
 * To get what bodyparts other players can hit when firing at player, set index to 0 and target to player's index. */
native get_user_hitzones(index, target);

/* Sets users max. speed. */
native set_user_maxspeed(index, Float:speed = -1.0);

/* Returns users max. speed. */
native Float:get_user_maxspeed(index);

/* Sets users gravity. */
native set_user_gravity(index, Float:gravity = 1.0);

/* Returns users gravity. */
native get_user_gravity(index);

/* Spawns entity. */
native spawn(index);

/* Sets player noclip. If you want to disable noclip set only first parameter. */
native set_user_noclip(index, noclip = 0);

/* Returns 1 if noclip is set. */
native get_user_noclip(index);

/* Gives player silent footsteps.
* if set = 0 it will return footsteps to normal */
native set_user_footsteps(id, set = 1);
