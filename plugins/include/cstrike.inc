// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Functions
//

#if defined _cstrike_included
	#endinput
#endif
#define _cstrike_included

#pragma reqlib cstrike
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib cstrike
#endif

#include <cstrike_const>

/**
 * Returns client's deaths.
 *
 * @param index     Client index
 *
 * @return          Client deaths
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_deaths(index);

/**
 * Sets client's deaths.
 *
 * @param index         Client index
 * @param newdeaths     New value to set
 * @param scoreboard    If true the scoreboard will be updated to reflect the new value.
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_deaths(index, newdeaths, bool:scoreboard = true);

/**
 * Returns index of the entity that a hostage is following.
 *
 * @note Hostages can theoretically follow any entity in the game, so the
 *       returned entity index is not necessarily a client index.
 *
 * @param index     Hostage entity index
 *
 * @return          Entity index if hostage is following something, 0 otherwise
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native cs_get_hostage_foll(index);

/**
 * Sets hostage to follow an entity.
 *
 * @note Hostages can theoretically follow any entity in the game, so the
 *       followedindex does not have to be a client index.
 *
 * @param index             Hostage entity index
 * @param followedindex     New entity to follow
 *
 * @noreturn
 * @error                   If the provided entity index is not a hostage, an
 *                          error will be thrown.
 */
native cs_set_hostage_foll(index, followedindex = 0);

/**
 * Returns unique id of a hostage.
 *
 * @param index     Hostage entity index
 *
 * @return          Unique hostage id
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native cs_get_hostage_id(index);

/**
 * Returns amount of ammo in the client's backpack for a specific weapon.
 *
 * @note For a list of possible weapon ids see the CSW_* constants in
 *       amxconst.inc
 * @note Some weapons share ammo types and therefore ammo backpack pools. List
 *       of ammo types:
 *       ammo_338magnum  - awp
 *       ammo_762nato    - scout, ak47, g3sg1
 *       ammo_556natobox - m249
 *       ammo_556nato    - famas, m4a1, aug, sg550, galil, sg552
 *       ammo_buckshot   - m3, xm1014
 *       ammo_45acp      - usp, ump45, mac10
 *       ammo_57mm       - fiveseven, p90
 *       ammo_50ae       - deagle
 *       ammo_357sig     - p228
 *       ammo_9mm        - glock, mp5, tmp, elites
 *       /               - hegrenade
 *       /               - flashbang
 *       /               - smokegrenade
 *
 * @param index     Client index
 * @param weapon    Weapon id
 *
 * @return          Amount of ammo in backpack
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, the client is not connected, or an invalid
 *                  weapon id is provided, an error will be thrown.
 */
native cs_get_user_bpammo(index, weapon);

/**
 * Sets amount of ammo in the client's backpack for a specific weapon.
 *
 * @note For a list of possible weapon ids see the CSW_* constants in
 *       amxconst.inc
 * @note Some weapons share ammo types and therefore ammo backpack pools. List
 *       of ammo types:
 *       ammo_338magnum  - awp
 *       ammo_762nato    - scout, ak47, g3sg1
 *       ammo_556natobox - m249
 *       ammo_556nato    - famas, m4a1, aug, sg550, galil, sg552
 *       ammo_buckshot   - m3, xm1014
 *       ammo_45acp      - usp, ump45, mac10
 *       ammo_57mm       - fiveseven, p90
 *       ammo_50ae       - deagle
 *       ammo_357sig     - p228
 *       ammo_9mm        - glock, mp5, tmp, elites
 *       /               - hegrenade
 *       /               - flashbang
 *       /               - smokegrenade
 *
 * @param index     Client index
 * @param weapon    Weapon id
 * @param amount    New backpack ammo amount to set
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, the client is not connected, or an invalid
 *                  weapon id is provided, an error will be thrown.
 */
native cs_set_user_bpammo(index, weapon, amount);

/**
 * Returns if the client has a defuse kit.
 *
 * @param index     Client index
 *
 * @return          1 if the client has a defuse kit, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, the client is not connected, or an invalid
 *                  weapon id is provided, an error will be thrown.
 */
native cs_get_user_defuse(index);

/**
 * Sets the client's defusekit status and allows to set a custom HUD icon and
 * color.
 *
 * @param index         Client index
 * @param defusekit     If nonzero the client will have a defusekit, otherwise
 *                      it will be removed
 * @param r             Red component of icon color
 * @param g             Green component of icon color
 * @param b             Blue component of icon color
 * @param icon          HUD sprite to use as icon
 * @param flash         If nonzero the icon will flash red
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_defuse(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0);

/**
 * Returns if the client is inside a buyzone.
 *
 * @param index     Client index
 *
 * @return          1 if the client is inside a buyzone, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_buyzone(index);

/**
 * Returns if the client has a primary weapon or a shield in the inventory.
 *
 * @param index     Client index
 *
 * @return          1 if the client has a primary weapon or shield in the
 *                  inventory, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_hasprim(index);

/**
 * Retrieves the client's player model.
 *
 * @param index     Client index
 * @param model     Buffer to copy model to
 * @param len       Maximum buffer size
 *
 * @return          Number of cells written to buffer
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_model(index, model[], len);

/**
 * Sets the client's player model.
 *
 * @note This is not a one-time set. The CStrike module will remember the
 *       selected model and try to prevent attempts at changing the player
 *       model, or immediately re-apply it if necessary.
 * @note Updating modelindex is useful for custom models which don't have 
 *       the same structure as the default ones (hitbox, etc..). Model must 
 *       be precached before.
 *
 * @param index           Client index
 * @param model           Model name
 * @param update_index    If true, the modelindex is updated as well
 *
 * @noreturn
 * @error                 If the client index is not within the range of 1 to
 *                        MaxClients, the client is not connected, the provided
 *                        model is empty, or if modeindex is updated and the 
 *                        provided model is not precached, an error will be thrown.
 */
native cs_set_user_model(index, const model[], bool:update_index = false);

/**
 * Resets the client's model.
 *
 * @note This lifts the model-lock set by a previous cs_set_user_model() call.
 *
 * @param index     Client index
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_reset_user_model(index);

/**
 * Returns the client's amount of money.
 *
 * @param index     Client index
 *
 * @return          Amount of money
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_money(index);

/**
 * Sets the client's amount of money.
 *
 * @param index     Client index
 * @param money     New amount to set
 * @param flash     If nonzero the HUD will flash the difference between new
 *                  and old amount in red or green
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_set_user_money(index, money, flash = 1);

/**
 * Returns if the client's has night vision goggles.
 *
 * @param index     Client index
 *
 * @return          1 if user has NVG, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_nvg(index);

/**
 * Sets the client's night vision goggles.
 *
 * @param index         Client index
 * @param nvgoogles     If nonzero the NVG will be added to the client's
 *                      inventory, otherwise they will be removed from it
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_nvg(index, nvgoggles = 1);

/**
 * Returns if the client has the ability to plant the bomb.
 *
 * @note Only with this set can the client plant the bomb within the usual bomb
 *       target areas. If this is not set the user can not plant the bomb, even
 *       when he has one in the inventory.
 *
 * @param index     Client index
 *
 * @return          1 if the client is able to plant the bomb, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_plant(index);

/**
 * Sets the client's ability to plant the bomb and displays or hides the bomb
 * HUD icon.
 *
 * @note Only with this set can the client plant the bomb within the usual bomb
 *       target areas. If this is not set the user can not plant the bomb, even
 *       when he has one in the inventory. This is only correctly set when the
 *       client touches a bomb and picks it up "manually" (only possible for
 *       Terrorists), so this should be used if the bomb is added to the
 *       inventory through other means.
 *
 * @param index         Client index
 * @param plant         If nonzero the client will be able to plant the bomb,
 *                      otherwise he will be unable to
 * @param showbombicon  If nonzero the green C4 icon will be displayed on the
 *                      client's hud, otherwise it will be hidden
 *
 * @return              1 if the client is able to plant the bomb, 0 otherwise
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_plant(index, plant = 1, showbombicon = 1);

/**
 * Sets the client's team without killing the player, and sets the client model.
 *
 * @note For a list of valid team ids see the CsTeams enum, and for a list of
 *       valid internal model ids see the CsInternalModel enum.
 *
 * @param index         Client index
 * @param team          Team id
 * @param model         Internal model id, if CS_DONTCHANGE the game will choose the model
 *                      or if CS_NORESET the game will not update it.
 * @param send_teaminfo If true, a TeamInfo message will be sent
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_set_user_team(index, any:team, any:model = CS_DONTCHANGE, bool:send_teaminfo = true);

/**
 * Returns the client's team and optionally the model id.
 *
 * @note For a list of valid team ids see the CsTeams enum, and for a list of
 *       valid internal model ids see the CsInternalModel enum.
 *
 * @param index     Client index
 * @param model     Optional variable to store model id in
 *
 * @return          Team id
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native CsTeams:cs_get_user_team(index, &any:model = CS_DONTCHANGE);

/**
 * Returns if the client is a VIP.
 *
 * @param index     Client index
 *
 * @return          1 if the client is a VIP, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_vip(index);

/**
 * Sets the client's VIP status and displayed model and scoreboard flag.
 *
 * @note This is mostly useful for removing VIP status so the client can change
 *       teams and/or buy items properly. It does not alter gameplay, the player
 *       that is selected as VIP at the start of a round will retain the
 *       internal VIP status and remain the primary objective for the game mode.
 *
 * @param index         Client index
 * @param vip           If nonzero the client will be made a VIP, otherwise the
 *                      VIP status will be removed
 * @param model         If nonzero the client's model will be changed to the VIP
 *                      model, otherwise a random CT model will be selected
 * @param scoreboard    If nonzero the scoreboard will be updated to reflect the
 *                      new VIP status
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_vip(index, vip = 1, model = 1, scoreboard = 1);

/**
 * Returns if the client has committed a team kill in the current round.
 *
 * @note If this is set to 1 the client will be punished at the start of the
 *       next round depending on the value of the mp_tkpunish cvar. The team
 *       kill status is then reset.
 *
 * @param index     Client index
 *
 * @return          1 if the client has committed a team kill, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_tked(index);

/**
 * Sets the client's team kill status, indicating whether the client has
 * committed a team kill in the current round.
 *
 * @note If this is set to 1 the client will be punished at the start of the
 *       next round depending on the value of the mp_tkpunish cvar. The team
 *       kill status is then reset.
 *
 * @param index     Client index
 * @param tk        Team kill status
 * @param subtract  Amount of frags to subtract, negative values add frags
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_set_user_tked(index, tk = 1, subtract = 1);

/**
 * Returns if the client is currently driving a vehicle and if so, indicates
 * the speed.
 *
 * @param index     Client index
 *
 * @return          0 if the client is not driving, 1 if driving a vehicle but
 *                  not moving, 2 to 4 if driving positive speeds, 5 if
 *                  driving at a negative speed (backing), see TRAIN_* constants
 *                  in hlsdk_const.inc
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_driving(index);

/**
 * Returns if the client has a shield in the inventory.
 *
 * @param index     Client index
 *
 * @return          1 if the client has a shield, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_shield(index);

/**
 * Returns if the client is using a stationary gun.
 *
 * @param index     Client index
 *
 * @return          1 if the client uses a stationary gun, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_stationary(index);

/**
 * Returns the client's armor value and retrieves the type of armor.
 *
 * @note For a list of possible armor types see the CsArmorType enum.
 *
 * @param index         Client index
 * @param armortype     Variable to store armor type in
 *
 * @return              Amount of armor, 0 if client has no armor
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_get_user_armor(index, &CsArmorType:armortype = CS_ARMOR_NONE);

/**
 * Sets the client's armor value the type of armor.
 *
 * @note For a list of possible armor types see the CsArmorType enum.
 * @note Sends the appropriate message to update the client's HUD.
 *
 * @param index         Client index
 * @param armorvalue    Amount of armor to set
 * @param armortype     CS armor type
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native cs_set_user_armor(index, armorvalue, CsArmorType:armortype);

/**
 * Returns if the weapon is in burst mode.
 *
 * @note Only the Glock and Famas can return 1 as they are the only guns in the
 *       game that have a burst fire mode.
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will return incorrect values for non-weapon entities.
 *
 * @param index     Weapon entity index
 *
 * @return          1 if the weapon is in burst mode, 0 otherwise
 * @error           If an invalid entity index or a client index is provided,
 *                  an error will be thrown.
 */
native cs_get_weapon_burst(index);

/**
 * Sets the weapon's burst mode.
 *
 * @note Only the Glock and Famas can be set to burst fire mode as they are the
 *       only guns in the game that provide such a mode.
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will result in undefined behavior if used on non-weapon
 *       entities.
 *
 * @param index         Weapon entity index
 * @param burstmode     If nonzero the weapon will be put into burstmode,
 *                      otherwise the burst mode will be removed
 *
 * @return              1 if burst mode set successfully, 0 if entity is not
 *                      an applicable weapon
 * @error               If an invalid entity index or a client index is
 *                      provided, an error will be thrown.
 */
native cs_set_weapon_burst(index, burstmode = 1);

/**
 * Returns if the weapon is in silenced mode.
 *
 * @note Only the USP and M4A1 can return 1 as they are the only guns in the
 *       game that have a silenced fire mode.
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will return incorrect values for non-weapon entities.
 *
 * @param index     Weapon entity index
 *
 * @return          1 if the weapon is in silenced mode, 0 otherwise
 * @error           If an invalid entity index or a client index is provided,
 *                  an error will be thrown.
 */
native cs_get_weapon_silen(index);

/**
 * Sets the weapon's silenced mode.
 *
 * @note Only the USP and M4A1 can be set to silenced fire mode as they are the
 *       only guns in the game that provide such a mode.
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will result in undefined behavior if used on non-weapon
 *       entities.
 *
 * @param index             Weapon entity index
 * @param silence           If nonzero the weapon will be put into silenced
 *                          mode, otherwise the silenced mode will be removed
 * @param draw_animation    If 1 and the weapon is currently held by a
 *                          client, the appropriate weapon animation will be
 *                          played
 *                          If 2, same as 1 but follows game behavior by playing
 *                          the associated player's model sequence and disallowing
 *                          firing while animation is playing.
 *
 * @return                  1 if silenced mode set successfully, 0 if entity is
 *                          not an applicable weapon
 * @error                   If an invalid entity index or a client index is
 *                          provided, an error will be thrown.
 */
native cs_set_weapon_silen(index, silence = 1, draw_animation = 1);

/**
 * Returns the amount of ammo in weapon's magazine.
 *
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will return incorrect values for non-weapon entities.
 *
 * @param index     Weapon entity index
 *
 * @return          Amount of ammo in magazine
 * @error           If an invalid entity index or a client index is provided,
 *                  an error will be thrown.
 */
native cs_get_weapon_ammo(index);

/**
 * Sets the amount of ammo in weapon's clip.
 *
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will result in undefined behavior if used on non-weapon
 *       entities.
 *
 * @param index     Weapon entity index
 * @param newammo   New ammo amount
 *
 * @noreturn
 * @error           If an invalid entity index or a client index is provided,
 *                  an error will be thrown.
 */
native cs_set_weapon_ammo(index, newammo);

/**
 * Returns the weapon id of an entity.
 *
 * @note For a list of possible weapon ids see the CSW_* constants in
 *       amxconst.inc
 * @note This native does not verify that the provided entity is a weapon
 *       entity. It will return incorrect values for non-weapon entities.
 *
 * @param index     Weapon entity index
 *
 * @return          Weapon id
 * @error           If an invalid entity index or a client index is provided,
 *                  an error will be thrown.
 */
native cs_get_weapon_id(index);

/**
 * Returns if "no knives" mode is enabled.
 *
 * @note "No knives" mode means that the CStrike module will prevent the game
 *       from creating (and thus attaching) "weapon_knife" entities. This means
 *       that clients will spawn without knives, but knives can still be put
 *       into the client inventories directly.
 *
 * @return  1 if "no knives" mode is enabled, 0 otherwise
 */
native cs_get_no_knives();

/**
 * Enables or disables the "no knives" mode.
 *
 * @note "No knives" mode means that the CStrike module will prevent the game
 *       from creating (and thus attaching) "weapon_knife" entities. This means
 *       that clients will spawn without knives, but knives can still be put
 *       into the client inventories directly.
 *
 * @param noknives  If nonzero enable "no knives" mode, disable otherwise
 *
 * @noreturn
 */
native cs_set_no_knives(noknives = 0);

/**
 * Sets a dead client up for spawning.
 *
 * @note This sets the client deadflag and triggers a client think, effectively
 *       making the game respawn the client. Should only be used on dead
 *       clients.
 *
 * @param player    Client index
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_user_spawn(player);

/**
 * Returns the armoury entity's weapon id.
 *
 * @note Not all weapon ids are supported by Counter-Strike, an armoury entity
 *       can not be a pistol, a knife or a bomb for exmaple. The full list is:
 *          CSW_SCOUT, CSW_HEGRENADE, CSW_XM1014, CSW_MAC10, CSW_AUG,
 *          CSW_SMOKEGRENADE, CSW_AWP, CSW_MP5NAVY, CSW_M249, CSW_M3, CSW_M4A1,
 *          CSW_TMP, CSW_G3SG1, CSW_VEST, CSW_VESTHELM, CSW_FLASHBANG,
 *          CSW_SG552, CSW_AK47, CSW_P90
 *
 * @param index     Armoury entity index
 * @param count     Optional variable to store in the number of times that an item can be retrieved  
 *                  from the same entity before being hidden
 *
 * @return          Weapon id
 * @error           If a non-armoury entity is provided, an error will be
 *                  thrown.
 */
native cs_get_armoury_type(index, &count = 1);

/**
 * Sets the amoury entity type.
 *
 * @note Not all weapon ids are supported by Counter-Strike, an armoury entity
 *       can not be a pistol, a knife or a bomb for exmaple. The full list is:
 *          CSW_SCOUT, CSW_HEGRENADE, CSW_XM1014, CSW_MAC10, CSW_AUG,
 *          CSW_SMOKEGRENADE, CSW_AWP, CSW_MP5NAVY, CSW_M249, CSW_M3, CSW_M4A1,
 *          CSW_TMP, CSW_G3SG1, CSW_VEST, CSW_VESTHELM, CSW_FLASHBANG,
 *          CSW_SG552, CSW_AK47, CSW_P90
 * @note This does not update the entity model.
 * @note On restart, entity is always unhidden and the count is restored (this can not be below 1).
 *
 * @param index     Armoury entity index
 * @param type      Weapon id
 * @param count     Number of times that an item can be retrieved from 
 *                  the same entity before being hidden
 *                  If zero, the entity is hidden
 *                  If below zero, nothing is set
 * @noreturn
 * @error           If a non-armoury entity is provided, an error will be
 *                  thrown.
 */
native cs_set_armoury_type(index, type, count = -1);

/**
 * Returns the weapon entity index that was packed into a weaponbox.
 *
 * @param weaponboxIndex  Weaponbox entity index
 *
 * @return                Weapon entity index on success or 0 if no weapon can be found
 * @error                 If a non-weaponbox entity is provided or the entity is invalid, an error will be
 *                        thrown.
 */
native cs_get_weaponbox_item(weaponboxIndex);

/**
 * Returns the map zones the client is inside of as a bitflag value.
 *
 * @note If the user does not have the ability to plant (cs_get_user_plant()
 *       returns 0) then the bitflag will not contain CS_MAPZONE_BOMBTARGET.
 * @nore For a list of possible zone flags see the CS_MAPZONE_* constants.
 *
 * @param index     Client index
 *
 * @return          Bitflag value of map zones
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_mapzones(index);

/**
 * Sets a zoom type on the client.
 *
 * @note Zoom types are not tied to their intended weapons, so any zoom type can
 *       be combined with any weapon.
 * @note For a list of possible zoom types see the zoom type enum above
 *       (CS_*_ZOOM constants).
 *
 * @param index     Client index
 * @param type      Zoom type
 * @param mode      If zero (blocking) the client will be forced to use the zoom
 *                  type set and won't be able to change it until it is reset
 *                  with CS_RESET_ZOOM, otherwise the user can restore back to
 *                  normal as usual
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, the client is not connected, or an invalid zoom
 *                  type is provided, an error will be thrown.
 */
native cs_set_user_zoom(index, type, mode);

/**
 * Returns if the client is zooming.
 *
 * @note For a list of possible zoom types see the zoom type enum above
 *       (CS_*_ZOOM constants).
 *
 * @param index     Client index
 *
 * @return          Zoom type if the user is zoomed in, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_zoom(index);

/**
 * Returns if a submodel is set on the client.
 *
 * @note In Counter-Strike the submodel setting determines whether the user has
 *       a bomb backpack (if a Terrorist) or a defuse kit (if a CT) on their
 *       model.
 *
 * @param index     Client index
 *
 * @return          1 if submodel is set, 0 otherwise
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native cs_get_user_submodel(index);

/**
 * Sets the submodel on a client.
 *
 * @note In Counter-Strike the submodel setting determines whether the user has
 *       a bomb backpack (if a Terrorist) or a defuse kit (if a CT) on their
 *       model.
 *
 * @param index     Client index
 * @param value     If nonzero the submodel is set, otherwise it is removed
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 */
native cs_set_user_submodel(index, value);

/**
 * Returns the client's last activity time.
 *
 * @note This is the time that the internal Counter-Strike afk kicker uses to
 *       see who has been inactive too long.
 *
 * @param index     Client index
 *
 * @return          Last activity time
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 */
native Float:cs_get_user_lastactivity(index);

/**
 * Sets the client's last activity time.
 *
 * @note This is the time that the internal Counter-Strike afk kicker uses to
 *       see who has been inactive too long.
 *
 * @param index     Client index
 * @param value     New last activity time
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 */
native cs_set_user_lastactivity(index, Float:value);

/**
 * Returns the amount of hostages that the client has killed.
 *
 * @note This is the value that the internal Counter-Strike hostage punisher
 *       uses to determine if a client should be kicked, depending on the
 *       value of the mp_hostagepenalty value.
 *
 * @param index     Client index
 *
 * @return          Amount of hostages killed
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 */
native cs_get_user_hostagekills(index);

/**
 * Sets the amount of hostages that the client has killed.
 *
 * @note This is the value that the internal Counter-Strike hostage punisher
 *       uses to determine if a client should be kicked, depending on the
 *       value of the mp_hostagepenalty value. The punisher only checks this
 *       value when a hostage is killed, so setting this will not cause the
 *       client to be kicked until they actually kill a hostage.
 *
 * @param index     Client index
 * @param value     New amount of hostages killed
 *
 * @noreturn
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 */
native cs_set_user_hostagekills(index, value);

/**
 * Returns the last time a hostage was used.
 *
 * @param index     Hostage entity
 *
 * @return          Last use time
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native Float:cs_get_hostage_lastuse(index);

/**
 * Sets the last time a hostage was used.
 *
 * @param index     Hostage entity
 * @param value     New last use time
 *
 * @noreturn
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native cs_set_hostage_lastuse(index, Float:value);

/**
 * Returns the next time a hostage can be used.
 *
 * @param index     Hostage entity
 *
 * @return          Next use time
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native Float:cs_get_hostage_nextuse(index);

/**
 * Sets the next time a hostage can be used.
 *
 * @param index     Hostage entity
 * @param value     New next use time
 *
 * @noreturn
 * @error           If the provided entity index is not a hostage, an error will
 *                  be thrown.
 */
native cs_set_hostage_nextuse(index, Float:value);

/**
 * Returns the game time at which the bomb will explode.
 *
 * @param index     C4 entity
 *
 * @return          Explosion time
 * @error           If the provided entity index is not a bomb, an error will be
 *                  thrown.
 */
native Float:cs_get_c4_explode_time(index);

/**
 * Sets the game time at which the bomb will explode.
 *
 * @param index     C4 entity
 * @param value     New explosion time
 *
 * @noreturn
 * @error           If the provided entity index is not a bomb, an error will be
 *                  thrown.
 */
native cs_set_c4_explode_time(index, Float:value);

/**
 * Returns if the bomb is being defused.
 *
 * @param c4index   C4 entity
 *
 * @return          1 if the bomb is being defused, 0 otherwise
 * @error           If the provided entity index is not a bomb, an error will be
 *                  thrown.
 */
native bool:cs_get_c4_defusing(c4index);

/**
 * Sets if the bomb is being defused.
 *
 * @param c4index   C4 entity
 * @param defusing  True if the bomb should be defused, false otherwise
 *
 * @noreturn
 * @error           If the provided entity index is not a bomb, an error will be
 *                  thrown.
 */
native cs_set_c4_defusing(c4index, bool:defusing);

/**
 * Creates an entity using Counter-Strike's custom CreateNamedEntity wrapper.
 *
 * @note Unlike other mods CS keeps track of entities using a custom hashtable.
 *       This function adds entities to this hashtable, providing benefits over
 *       the default CreateNamedEntity (used by create_entity() for example):
 *       - Storing entities in a hashtable allows CS to improve classname lookup
 *         performance compared to functions like FindEntityByString (used by
 *         find_ent_by_class() for example) that usually have to loop
 *         through all entities incrementally.
 *       - As CS exclusively uses the hashtable for classname lookup, entities
 *         created using the default engine functions will not be found by the
 *         game. For example "weaponbox" entities are supposed to be
 *         automatically cleaned up on round restart but are not considered if
 *         they have not been added to the hashtable.
 * @note The faster hashtable lookup can be utilized with cs_find_ent_by_class()
 * @note When creating an entity the classname has to be valid in the mod, as
 *       the engine needs to link the entity to an existing class internally.
 *       The classname string that is stored in the entvar struct
 *       (EV_SZ_classname) is separate from this association and can later be
 *       freely changed to serve other purposes.
 *
 * @param classname     Entity class name
 *
 * @return              Index of the created entity (> 0), 0 otherwise
 */
native cs_create_entity(const classname[]);

/**
 * Finds an entity in the world using Counter-Strike's custom FindEntityByString
 * wrapper.
 *
 * @note Unlike other mods CS keeps track of entities using a custom hashtable.
 *       This function utilizes the hasthable and allows for considerably faster
 *       classname lookup compared to the default FindEntityByString (used by
 *       find_ent_by_class() for example).
 * @note This exclusively considers entities in the hashtable, created by the
 *       game itself, using cs_create_entity(), or added via cs_set_ent_class().
 *
 * @param start_index   Entity index to start searching from. -1 to start from
 *                      the first entity
 * @param classname     Classname to search for
 *
 * @return              Entity index > 0 if found, 0 otherwise
 */
native cs_find_ent_by_class(start_index, const classname[]);

/**
 * Finds an entity in the world using Counter-Strike's custom FindEntityByString
 * wrapper, matching by owner.
 *
 * @note Unlike other mods CS keeps track of entities using a custom hashtable.
 *       This function utilizes the hasthable and allows for considerably faster
 *       classname lookup compared to the default FindEntityByString (used by
 *       find_ent_by_owner() for example).
 * @note This exclusively considers entities in the hashtable, created by the
 *       game itself, using cs_create_entity(), or added via cs_set_ent_class().
 *
 * @param start_index   Entity index to start searching from. -1 to start from
 *                      the first entity
 * @param classname     Classname to search for
 * @param owner         Entity index to search for entity's owner
 *
 * @return              Entity index > 0 if found, 0 otherwise
 */
native cs_find_ent_by_owner(start_index, const classname[], owner);

/**
 * Sets a custom classname of an entity.
 *
 * @note Unlike other mods CS keeps track of entities using a custom hashtable.
 *       This function adds or updates the classname in the hasthable as well.
 *       This is useful for use with cs_find_ent_by_class() and cs_find_ent_by_owner().
 *
 * @param index         Entity index
 * @param classname     Classname to update for
 *
 * @noreturn
 */
native cs_set_ent_class(index, const classname[]);

/**
 * Returns the item id associated with an item name and its aliases.
 *
 * @note The item name is case sensitive an can be with or without 
 *       weapon_ and item_ prefixes. This can be a command alias as well.
 *       Values examples: ak47, weapon_ak47, kevlar, item_kevlar, vest, bullpup, ...
 *
 * @param name          Alias or classname
 * @param classid       If item is a weapon, variable to store the associated 
 *                      weapon class id in (CS_WEAPONCLASS_* constants)
 *
 * @return              Item id (CSI_* constants)
 */
native any:cs_get_item_id(const name[], &CsWeaponClassType:classid = CS_WEAPONCLASS_NONE);

/**
 * Returns the alias name associated with an item index.
 *
 * @param itemid          Item id (CSI_* constants)
 * @param name            Buffer to store alias name to
 * @param name_maxlen     Maximum buffer size
 * @param altname         Optional buffer to store if available alternative alias name to
 * @param altname_maxlen  Maximum buffer size
 *
 * @return                True if alias is found, false otherwise
 */
native bool:cs_get_item_alias(itemid, name[], name_maxlen, altname[] = "", altname_maxlen = 0);

/**
 * Returns an item name associated with a command alias.
 *
 * @note The alias is case sensitive.
 * @note If not an alias to a weapon, buffer will be set with the original alias.
 *
 * @param alias         Alias name
 * @param itemname      Buffer to store item name to
 * @param maxlength     Maximum buffer size
 *
 * @return              True if alias is translated, false otherwise
 */
native bool:cs_get_translated_item_alias(const alias[], itemname[], maxlength);

/**
 * Returns some information about a weapon.
 *
 * @param weapon_id     Weapon id, see CSW_* constants
 * @param type          Info type, see CS_WEAPONINFO_* constants
 *
 * @return              Weapon information value
 * @error               If weapon_id and type are out of bound, an error will be thrown.
 */
native any:cs_get_weapon_info(weapon_id, CsWeaponInfo:type);

/**
 * Returns active weapon entity.
 *
 * @param playerIndex   Player index
 *
 * @return              Weapon entity index on success or 0 if there is no active weapon
 * @error               If the client index is not within the range of 1 to
 *                      maxClients, or the client is not connected, an error will be
 *                      thrown.
 */
native cs_get_user_weapon_entity(playerIndex);

/**
 * Returns weapon index of the active weapon.
 *
 * @note More reliable than get_user_weapon.
 *
 * @param playerIndex   Player index
 * @param clip          Optional variable to store clip ammo to
 * @param ammo          Optional variable to store backpack ammo to
 *
 * @return              Weapon index on success or 0 if there is no active weapon
 * @error               If the client index is not within the range of 1 to
 *                      maxClients, or the client is not connected, an error will be
 *                      thrown.
 */
native cs_get_user_weapon(playerIndex, &clip = 0, &ammo = 0);

/**
 * Returns a weapon class id associated with a weapon id.
 *
 * @param weapon_id     Weapon id (CSI_* constants)
 *
 * @return              Weapon class id (CS_WEAPONCLASS_* constants)
 */
stock CsWeaponClassType:cs_get_weapon_class(weapon_id)
{
	new CsWeaponClassType:type = CS_WEAPONCLASS_NONE;

	if (cs_is_valid_itemid(weapon_id, .weapon_only = true) || weapon_id == CSI_SHIELD)
	{
		switch (weapon_id)
		{
			case CSI_SHIELDGUN, CSI_SHIELD: 
			{
				type = CS_WEAPONCLASS_PISTOL;
			}
			case CSI_KNIFE: 
			{
				type = CS_WEAPONCLASS_KNIFE;
			}
			default:
			{
				new const bits = (1 << weapon_id);

				if(bits & CSI_ALL_PISTOLS)
				{
					type = CS_WEAPONCLASS_PISTOL;
				}
				else if(bits & CSI_ALL_GRENADES)
				{
					type = CS_WEAPONCLASS_GRENADE;
				}
				else if(bits & CSI_ALL_SMGS)
				{
					type = CS_WEAPONCLASS_SUBMACHINEGUN;
				}
				else if(bits & CSI_ALL_SHOTGUNS)
				{
					type = CS_WEAPONCLASS_SHOTGUN;
				}
				else if(bits & CSI_ALL_MACHINEGUNS)
				{
					type = CS_WEAPONCLASS_MACHINEGUN;
				}
				else if(bits & CSI_ALL_RIFLES)
				{
					type = CS_WEAPONCLASS_RIFLE;
				}
				else if(bits & CSI_ALL_SNIPERRIFLES)
				{
					type = CS_WEAPONCLASS_SNIPERRIFLE;
				}
			}
		}
	}

	return type;
}

/**
 * Checks whether an item id is not out of bounds.
 *
 * @param id           Item id (CSI_* constants) 
 * @param weapon_only  If true, only the real weapon ids will be checked,
 *                     including shield as well 
 *
 * @return             True if item id is valid, false otherwise
 */
stock bool:cs_is_valid_itemid(id, bool:weapon_only = false)
{
	if (id <= CSI_NONE)
	{
		return false;
	}

	if (id > CSI_LAST_WEAPON && id != CSI_SHIELDGUN && weapon_only)
	{
		return false;
	}

	if (id >= CSI_MAX_COUNT)
	{
		return false;
	}

	return true;
}

/**
 * Called when CS internally fires a command to a player.
 *
 * @note This is most notably used by the rebuy/autobuy functionality,
 *       Condition Zero also uses this to pass commands to bots internally.
 *
 * @param id    Client index
 * @param cmd   Command string
 *
 * @return      PLUGIN_CONTINUE to let the command continue
 *              PLUGIN_HANDLED to block the command
 */
forward CS_InternalCommand(id, const cmd[]);

/**
 * Called when a client attempts to purchase an item.
 *
 * @note This is called immediately when the client issues a buy command. The
 *       game has not yet checked if the client can actually buy the weapon.
 * @note For a list of possible item ids see the CSI_* constants.
 *
 * @param index     Client index
 * @param item      Item id
 *
 * @return          PLUGIN_CONTINUE to let the buy attempt continue
 *                  PLUGIN_HANDLED to block the buy attempt
 */
forward CS_OnBuyAttempt(index, item);

/**
 * Called when a client purchases an item.
 *
 * @note This is called right before the user receives the item and before the
 *       money is deducted from their cash reserves.
 * @note For a list of possible item ids see the CSI_* constants.
 *
 * @param index     Client index
 * @param item      Item id
 *
 * @return          PLUGIN_CONTINUE to let the buy continue
 *                  PLUGIN_HANDLED to block the buy
 */
forward CS_OnBuy(index, item);
