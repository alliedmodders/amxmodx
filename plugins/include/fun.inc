// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fun Functions
//

#if defined _fun_included
	#endinput
#endif
#define _fun_included

#pragma reqlib fun
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib fun
#endif


/**
 * Parts of body for hits, for use with set_user_hitzones().
 */
const HITZONE_GENERIC  = (1 << HIT_GENERIC);   // 1
const HITZONE_HEAD     = (1 << HIT_HEAD);      // 2
const HITZONE_CHEST    = (1 << HIT_CHEST);     // 4
const HITZONE_STOMACH  = (1 << HIT_STOMACH);   // 8
const HITZONE_LEFTARM  = (1 << HIT_LEFTARM);   // 16
const HITZONE_RIGHTARM = (1 << HIT_RIGHTARM);  // 32
const HITZONE_LEFTLEG  = (1 << HIT_LEFTLEG);   // 64
const HITZONE_RIGHTLEG = (1 << HIT_RIGHTLEG);  // 128
const HITZONES_DEFAULT = HITZONE_GENERIC | HITZONE_HEAD     | HITZONE_CHEST   | HITZONE_STOMACH | 
                         HITZONE_LEFTARM | HITZONE_RIGHTARM | HITZONE_LEFTLEG | HITZONE_RIGHTLEG; // 255

/**
 * Tells whether receiver hears sender via voice communication.
 *
 * @param receiver      Receiver
 * @param sender        Sender 
 *
 * @return              1 if receiver hears the sender, 0 otherwise.
 * @error               If receiver or sender are not connected or not
 *                      within the range of 1 to MaxClients
 */
native get_client_listen(receiver, sender);

/**
 * Sets who can listen who.
 *
 * @param receiver      Receiver
 * @param sender        Sender
 * @param listen        1 if receiver should be able to hear sender, 0 if not
 *
 * @return              0 if the setting can't be done for some reason
 * @error               If receiver or sender are not connected or not
 *                      within the range of 1 to MaxClients.
 */
native set_client_listen(receiver, sender, listen);

/**
 * Sets player's godmode.
 *
 * @param index         Client index
 * @param godmode       1 to enable godmode, 0 to disable
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_godmode(index, godmode = 0);

/**
 * Tells whether a player has godmode on.
 *
 * @param index         Client index
 *
 * @return              1 if player has godmode on, 0 if not
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native get_user_godmode(index);

/**
 * Sets player's armor amount.
 *
 * @param index         Client index
 * @param armor         The armor amount to set
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_armor(index, armor);

/**
 * Sets player's health amount.
 *
 * @param index         Client index
 * @param health        The health amount to set
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_health(index, health);

/**
 * Moves a player to the given origin.
 *
 * @param index         Client index
 * @param origin        Origin to move a player to
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_origin(index, const origin[3]);

/**
 * Sets player's rendering mode.
 *
 * @note A really useful render modes reference:
 *		 https://sites.google.com/site/svenmanor/rendermodes
 *
 * @param index         Client index
 * @param fx            Rendering effects. One of kRenderFx* constants
 * @param r             The amount of red color (0 to 255)
 * @param g             The amount of green color (0 to 255)
 * @param b             The amount of blue color (0 to 255)
 * @param render        Render mode. One of kRender* constants
 * @param amount        Render amount (0 to 255)
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_rendering(index, fx = kRenderFxNone, r = 0, g = 0, b = 0, render = kRenderNormal, amount = 0);

/**
 * Gets player's rendering mode.
 *
 * @note A really useful render modes reference:
 *		 https://sites.google.com/site/svenmanor/rendermodes
 *
 * @param index         Client index
 * @param fx            Variable to store the rendering effect
 * @param r             Variable to store the amount of red color
 * @param g             Variable to store the amount of green color
 * @param b             Variable to store the amount of blue color
 * @param render        Variable to store the render mode
 * @param amount        Variable to store the render amount
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native get_user_rendering(index, &fx = kRenderFxNone, &r = 0, &g = 0, &b = 0, &render = kRenderNormal, &amount = 0);

/**
 * Gives an item to a player.
 *
 * @param index         Client index
 * @param item          Classname of the item to give. Should start with either
 *                      "weapon_", "ammo_", "item_" or "tf_weapon_"
 *
 * @return              Item entity index. If an invalid item name is
 *                      given or the item failed to create, it will return 0.
 *                      If the item was removed, it will return -1
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients or item creation fails.
 */
native give_item(index, const item[]);

/**
 * Sets (adds, removes) hit zones for a player.
 *
 * @note This actually sets rules of how any player can hit any other.
 *       Example: set_user_hitzones(id, target, 2) - makes @id able to
 *       hit @target only in the head.
 *
 * @param index         Client index
 * @param target        The target player
 * @param body          A bitsum of the body parts that can/can't be shot. See HITZONE* constants.
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_hitzones(index = 0, target = 0, body = HITZONES_DEFAULT);

/**
 * Gets the set of hit zone "rules" between @index and @target players.
 *
 * @note For the body part bitsum, see HITZONE* constants.
 *
 * @param index         Client index
 * @param target        The target player
 *
 * @return              The bitsum of @target's body parts @index is able to hit
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native get_user_hitzones(index, target);

/**
 * Sets player's maximum movement speed.
 *
 * @param index         Client index
 * @param speed         The maximum speed player will be able to run at
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_maxspeed(index, Float:speed = -1.0);

/**
 * Gets player's maximum movement speed.
 *
 * @param index         Client index
 *
 * @return              Player's maximum movement speed
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native Float:get_user_maxspeed(index);

/**
 * Sets player's gravity.
 *
 * @param index         Client index
 * @param gravity       Gravity value to set, 1.0 being normal gravity (800)
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_gravity(index, Float:gravity = 1.0);

/**
 * Gets player's gravity.
 *
 * @param index         Client index
 *
 * @return              Player's gravity value, 1.0 being normal gravity (800)
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native Float:get_user_gravity(index);

/**
 * Spawns an entity.
 *
 * @param index         Entity index
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native spawn(index);

/**
 * Enables or disables player's noclip.
 *
 * @param index         Client index
 * @param noclip        1 to enable noclip, 0 to disable
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_noclip(index, noclip = 0);

/**
 * Gets whether a player has noclip enabled or not.
 *
 * @param index         Client index
 *
 * @return              1 if noclip is enabled, 0 if disabled
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native get_user_noclip(index);

/**
 * Tells whether a player has silent footsteps enabled.
 *
 * @param index         Client index
 *
 * @return              1 if silent footsteps are enabled, 0 if not
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native get_user_footsteps(index);

/**
 * Enables or disables player's silent footsteps.
 *
 * @param index        Client index
 * @param set          1 if player should have silent footsteps, 0 otherwise
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_footsteps(id, set = 1);

/**
 * Strips all weapons from a player, including their knife.
 *
 * @param index         Client index
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native strip_user_weapons(index);

/**
 * Sets player's frags amount.
 *
 * @param index         Client index
 * @param frags         The amount of frags to set
 *
 * @noreturn
 * @error               If player is not connected or not within the range
 *                      of 1 to MaxClients.
 */
native set_user_frags(index, frags);
