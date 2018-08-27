// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Temporary Entity Functions
//

#if defined _msgnatives_included
	#endinput
#endif

#define _msgnatives_included

/**
 * Temporarily draws HUD numerical ammo amount and corresponding ammo
 * HUD icon in the middle of the right side of the screen.
 *
 * @note Draw time depends on the hud_drawhistory_time client cvar value.
 *
 * @param id            Client index or 0 for all clients
 * @param ammoid        Ammunition id
 * @param amount        Ammunition amount
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native draw_ammo_pickup_icon(id, ammoid, amount, bool:reliable = true)

/**
 * Temporarily draws the corresponding weapon HUD icon in the middle of the
 * right side of the screen.
 *
 * @note Draw time depends on the hud_drawhistory_time client cvar value.
 *
 * @param id            Client index or 0 for all clients
 * @param weaponid      Weapon id
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native draw_weapon_pickup_icon(id, weaponid, bool:reliable = true)

/**
 * Draws a specified status HUD icon.
 *
 * @note This native is available only in the following games:
 *         Counter-Strike
 *         Counter-Strike: Condition Zero
 *         Half-Life: Opposing Force
 *         Team Fortress Classic
 * @note A list of all icons and screenshots of them can be found here:
 *       http://doktor.haze.free.fr/counter-strike/sprites_screens/index.php
 *
 * @param id            Client index or 0 for all clients
 * @param sprite        Sprite name (valid names are listed in sprites/hud.txt)
 * @param status        Valid status values:
 *                        StatusIcon_Hide         - hides the status icon
 *                        StatusIcon_Show         - shows the status icon
 *                        StatusIcon_Flash        - flashes the status icon
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native draw_status_icon(id, sprite[] = "", StatusIconFlags:status = StatusIcon_Hide, r = 0, g = 0, b = 0, bool:reliable = true)

/**
 * Hides specific elements from the HUD.
 *
 * @note The symbol + means that an additional crosshair will be drawn.
 * This crosshair looks not like the regular one, but like the one that
 * is drawn in spectator mode. You can remove this crosshair by setting
 * the "noadd" argument to "true".
 *
 * @param id            Client index or 0 for all clients
 * @param elements      HUD elements to hide. The names are self-explanatory:
 *                        HideElement_Cross_Ammo_WPNList
 *                        HideElement_Flashlight (+)
 *                        HideElement_All
 *                        HideElement_Radar_Health_Armor (+)
 *                        HideElement_Timer (+)
 *                        HideElement_Money (+)
 *                        HideElement_Crosshair
 * @param noadd         If set to false and the chosen element names have
 *                      a "+" sign, an additional crosshair will be drawn.
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native hide_hud_elements(id, HideElemenentFlags:elements, bool:noadd = true, bool:reliable = true)

/**
 * Displays the speed bar used for controlling a train.
 *
 * @note This native isn't available in Day of Defeat.
 *
 * @param id            Client index or 0 for all clients
 * @param speed         Train speed:
 *                        TrainControls_None
 *                        TrainControls_Neutral
 *                        TrainControls_Slow
 *                        TrainControls_Medium
 *                        TrainControls_Maximum
 *                        TrainControls_Reverse
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native draw_train_controls(id, TrainControlFlags:speed = TrainControls_None, bool:reliable = true)

/**
 * Sends the geiger signal that notifies the player of nearby radiation level.
 *
 * @note This native isn't available in Day of Defeat.
 *
 * @param id            Client index or 0 for all clients
 * @param distance      Signal distance
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native send_geiger_signal(id, distance, bool:reliable = true)

/**
 * Fades the client's screen.
 *
 * @param id            Client index or 0 for all clients
 * @param duration      How long (in seconds) the fade is going to stay
 *                      on the screen (0 - 16)
 * @param fadetime      How many seconds is the fade going to fade in (0 - 16)
 * @param flags         Screen fade flags:
 *                        ScreenFade_FadeIn       - default
 *                        ScreenFade_FadeOut      - fade out (not in)
 *                        ScreenFade_Modulate     - modulate (don't blend)
 *                        ScreenFade_StayOut      - ignores the duration and stays faded out until a new ScreenFade messages is received
 * @param r             Red color amount (0 - 255)
 * @param g             Green color amount (0 - 255)
 * @param b             Blue color amount (0 - 255)
 * @param a             Color alpha (brightness) (0 - 255)
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */

native fade_user_screen(id, Float:duration = 1.0, Float:fadetime = 0.5, ScreenFadeFlags:flags = ScreenFade_FadeIn, r = 0, g = 0, b = 255, a = 75, bool:reliable = true)

/**
 * Shakes the client's screen.
 *
 * @param id            Client index or 0 for all clients
 * @param amplitude     Shake amplitude (0 - 16)
 * @param duration      Shake duration (in seconds) (0 - 16)
 * @param frequency     Delay between each shake (0 - 16)
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native shake_user_screen(id, Float:amplitude = 3.0, Float:duration = 3.0, Float:frequency = 1.0, bool:reliable = true)

/**
 * Changes the client's field of view (FOV).
 *
 * @note Setting the "fov" argument below 45 will also draw a sniper scope.
 *
 * @param id            Client index or 0 for all clients
 * @param fov           Field of view degrees (0 - 255)
 * @param reliable      If true, the message will be sent via the reliable
 *                      channel, otherwise it will use the unreliable one
 *
 * @noreturn
 * @error               If "id" is non-zero and the client isn't
 *                      connected, an error will be thrown.
 */
native set_user_fov(id, fov = 0, bool:reliable = true)
